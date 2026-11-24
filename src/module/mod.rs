//! Modules: finding the files a program imports, and putting them together.
//!
//! A file is a module, and the directory tree is the module tree: from a
//! program's root (the directory of the file being run), `geo/shapes.aja` is
//! the module `geo::shapes`. [`load`] reads the file being run, then every
//! module it imports, and theirs, rejecting import cycles. [`resolve`] then
//! gives every definition a name that says which module it is in
//! (`geo::shapes::area`), rewrites every use of a name to that name, and
//! checks that what one module uses from another is `pub`. The result is one
//! [`Program`] that the type checker and the backends handle like a program
//! in a single file.
//!
//! Definitions in the file being run keep their names (`main`), since no
//! other module can import it: any import of it would be a cycle.
//!
//! Paths that start with `std` are the standard library's, which is built
//! into the binary (see [`stdlib`]), so a program can't have a module of its
//! own called `std`.

use std::collections::HashMap;

use crate::{
    ast::{Id, Program, Source, Span},
    check,
    diagnostic::{Diagnostic, Files},
    lexer::tokenize_source,
    parse::Parser,
    stdlib,
    vm::{Compiled, Compiler},
};

mod resolve;
#[cfg(test)]
mod tests;

pub use resolve::resolve;

/// One file of a program.
#[derive(Debug)]
pub struct Module {
    /// Its path from the root, `["geo", "shapes"]`.
    pub path: Vec<Id>,
    pub source: Source,
    pub program: Program,
}

impl Module {
    /// Its name as written in code, `geo::shapes`.
    pub fn name(&self) -> String {
        path_name(&self.path)
    }

    /// Whether it is part of the standard library.
    pub fn is_std(&self) -> bool {
        self.path.len() > 1 && self.path[0] == "std"
    }

    /// Whether it is the file being run.
    pub fn is_main(&self) -> bool {
        self.source == Source::MAIN
    }
}

fn path_name(path: &[Id]) -> String {
    let segments: Vec<&str> = path.iter().map(|s| s.as_str()).collect();
    segments.join("::")
}

/// The file a module's source is in, relative to the root:
/// `geo/shapes.aja` for `geo::shapes`.
pub fn file_name(path: &[Id]) -> String {
    let segments: Vec<&str> = path.iter().map(|s| s.as_str()).collect();
    format!("{}.aja", segments.join("/"))
}

/// Load `main` (a file in `root`) and the modules it imports, resolve their
/// names, type-check them, and compile them, returning their files and the
/// compiled program, or every error found. Names are type-checked even when
/// some failed to resolve, so all of those errors are reported at once.
pub fn build(root: &dyn Root, main: &str) -> (Files, Result<Compiled, Vec<Diagnostic>>) {
    let (loaded, errors) = load(root, main);
    if !errors.is_empty() {
        return (loaded.files, Err(errors));
    }
    let library: Vec<_> = loaded
        .modules
        .iter()
        .filter(|m| m.is_std())
        .map(|m| m.source)
        .collect();
    let (program, mut errors) = resolve(loaded.modules);
    let types = check::check_modules(&program, library.iter().copied().collect());
    if let Err(type_errors) = &types {
        errors.extend(type_errors.iter().map(|e| e.to_diagnostic()));
    }
    if !errors.is_empty() {
        errors.sort_by_key(Diagnostic::sort_key);
        return (loaded.files, Err(errors));
    }
    let types = types.expect("there are no type errors");
    let compiled = Compiler::new()
        .program(&program, &types)
        .map(|mut c| {
            c.library = library;
            c
        })
        .map_err(|e| vec![e]);
    (loaded.files, compiled)
}

/// Where a module's text comes from: the file system, or (in tests) a map.
pub trait Root {
    /// How a file is labelled in messages.
    fn display(&self, file: &str) -> String;
    /// The text of `file` (relative to the root), or `None` if there is no
    /// such file. An error reading a file that exists is an `Err`.
    fn read(&self, file: &str) -> Result<Option<String>, String>;
}

/// A directory on disk.
pub struct Dir(pub std::path::PathBuf);

impl Root for Dir {
    fn display(&self, file: &str) -> String {
        self.0.join(file).display().to_string()
    }

    fn read(&self, file: &str) -> Result<Option<String>, String> {
        match std::fs::read_to_string(self.0.join(file)) {
            Ok(text) => Ok(Some(text)),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
            Err(e) => Err(e.to_string()),
        }
    }
}

/// Files held in memory, by path from the root.
impl Root for HashMap<String, String> {
    fn display(&self, file: &str) -> String {
        file.to_string()
    }

    fn read(&self, file: &str) -> Result<Option<String>, String> {
        Ok(self.get(file).cloned())
    }
}

/// A program's modules, the file being run first, and the files they are in.
pub struct Loaded {
    pub modules: Vec<Module>,
    pub files: Files,
}

/// Read and parse `main` (a file in `root`) and every module it imports,
/// directly or not. Diagnostics are for every file that couldn't be read or
/// parsed, and every import cycle; `files` is returned either way so they
/// can be shown.
pub fn load(root: &dyn Root, main: &str) -> (Loaded, Vec<Diagnostic>) {
    let main_path: Vec<Id> = vec![main.strip_suffix(".aja").unwrap_or(main).into()];
    let mut loader = Loader {
        root,
        loaded: Loaded {
            modules: Vec::new(),
            files: Files::default(),
        },
        ids: HashMap::new(),
        stack: Vec::new(),
        errors: Vec::new(),
    };
    match root.read(main) {
        Ok(Some(text)) => {
            loader.add(main_path, root.display(main), text);
        }
        Ok(None) => loader.errors.push(Diagnostic::new(format!(
            "could not read '{}': no such file",
            root.display(main)
        ))),
        Err(e) => loader.errors.push(Diagnostic::new(format!(
            "could not read '{}': {e}",
            root.display(main)
        ))),
    }
    (loader.loaded, loader.errors)
}

struct Loader<'r> {
    root: &'r dyn Root,
    loaded: Loaded,
    /// Each module seen so far, by path, and its index in `loaded.modules`
    /// (`None` if its file couldn't be read or parsed).
    ids: HashMap<Vec<Id>, Option<usize>>,
    /// The modules being loaded, outermost first, each with the import of
    /// the next one; a cycle is an import of one of them.
    stack: Vec<(Vec<Id>, Option<Span>)>,
    errors: Vec<Diagnostic>,
}

impl Loader<'_> {
    /// Parse a module from `text`, then load its imports.
    fn add(&mut self, path: Vec<Id>, display: String, text: String) -> Option<usize> {
        let source = self.loaded.files.add(display, text);
        let text = self.loaded.files.text(source).to_string();
        let parsed = tokenize_source(&text, source)
            .map_err(|e| e.to_diagnostic())
            .and_then(|tokens| {
                Parser::new(tokens.into_iter())
                    .parse()
                    .map_err(|e| e.to_diagnostic())
            });
        let program = match parsed {
            Ok(program) => program,
            Err(e) => {
                self.errors.push(e);
                self.ids.insert(path, None);
                return None;
            }
        };

        let index = self.loaded.modules.len();
        self.ids.insert(path.clone(), Some(index));
        let mut program = program;
        for import in &mut program.imports {
            self.import_item_by_path(import);
        }
        let imports: Vec<(Vec<Id>, Span)> = program
            .imports
            .iter()
            .map(|i| {
                (
                    i.path.iter().map(|(s, _)| s.clone()).collect(),
                    i.path_span(),
                )
            })
            .collect();
        self.loaded.modules.push(Module {
            path: path.clone(),
            source,
            program,
        });

        self.stack.push((path, None));
        for (import, span) in imports {
            self.stack.last_mut().unwrap().1 = Some(span);
            self.import(import, span);
        }
        self.stack.pop();
        Some(index)
    }

    /// Make `import a::b::item;` mean `import a::b::{item};` when there is
    /// no module `a::b::item` but there is a module `a::b`. A module of that
    /// name wins, so adding an item never changes what an import means.
    fn import_item_by_path(&self, import: &mut crate::ast::Import) {
        if import.items.is_some() || import.path.len() < 2 {
            return;
        }
        let path: Vec<Id> = import.path.iter().map(|(s, _)| s.clone()).collect();
        let parent = &path[..path.len() - 1];
        if self.module_exists(&path) || !self.module_exists(parent) {
            return;
        }
        let item = import
            .path
            .pop()
            .expect("the path has two segments or more");
        import.items = Some(vec![item]);
    }

    /// Whether there is a module at `path`, without loading it. One that
    /// can't be read counts, so that importing it reports why.
    fn module_exists(&self, path: &[Id]) -> bool {
        if self.ids.contains_key(path) || self.stack.iter().any(|(p, _)| p == path) {
            return true;
        }
        if path[0] == "std" {
            return path.len() > 1 && stdlib::source(&path_name(&path[1..])).is_some();
        }
        !matches!(self.root.read(&file_name(path)), Ok(None))
    }

    /// Load the module `path`, imported at `span`, unless it is already.
    fn import(&mut self, path: Vec<Id>, span: Span) {
        if let Some(start) = self.stack.iter().position(|(p, _)| *p == path) {
            self.cycle(start, span);
            return;
        }
        if self.ids.contains_key(&path) {
            return;
        }
        if path[0] == "std" {
            return self.import_std(path, span);
        }
        let file = file_name(&path);
        match self.root.read(&file) {
            Ok(Some(text)) => {
                self.add(path, self.root.display(&file), text);
            }
            Ok(None) => {
                let name = path_name(&path);
                let help = format!(
                    "a module is a file, named by its path from the program's root; \
                     '{name}' would be in '{}'",
                    self.root.display(&file)
                );
                self.errors.push(
                    Diagnostic::new(format!("no module named '{name}'"))
                        .with_span(span.start, span.end)
                        .with_help(Some(help)),
                );
                self.ids.insert(path, None);
            }
            Err(e) => {
                self.errors.push(
                    Diagnostic::new(format!(
                        "could not read '{}': {e}",
                        self.root.display(&file)
                    ))
                    .with_span(span.start, span.end),
                );
                self.ids.insert(path, None);
            }
        }
    }

    /// Load the standard library module `path` (`std::io`), which is built
    /// into the binary. It imports only other standard library modules, so
    /// it can't be in a cycle with the program's.
    fn import_std(&mut self, path: Vec<Id>, span: Span) {
        let name = path_name(&path[1..]);
        match stdlib::source(&name) {
            Some(text) => {
                self.add(path, stdlib::display(&name), text.to_string());
            }
            None => {
                let mut modules: Vec<String> = stdlib::MODULES
                    .iter()
                    .filter(|(m, _)| *m != stdlib::NATIVE)
                    .map(|(m, _)| format!("std::{m}"))
                    .collect();
                modules.sort();
                self.errors.push(
                    Diagnostic::new(format!("no module named '{}'", path_name(&path)))
                        .with_span(span.start, span.end)
                        .with_help(Some(format!(
                            "the standard library's modules are: {}",
                            modules.join(", ")
                        ))),
                );
                self.ids.insert(path, None);
            }
        }
    }

    /// Report the cycle of imports from `stack[start]` around to the import
    /// at `span`, which is of `stack[start]` again.
    fn cycle(&mut self, start: usize, span: Span) {
        let mut names: Vec<String> = self.stack[start..]
            .iter()
            .map(|(p, _)| format!("'{}'", path_name(p)))
            .collect();
        names.push(names[0].clone());
        let notes = self.stack[start..]
            .windows(2)
            .map(|w| {
                let at = w[0]
                    .1
                    .expect("each module on the stack but the last is importing");
                format!(
                    "'{}' imports '{}' at {}:{}:{}",
                    path_name(&w[0].0),
                    path_name(&w[1].0),
                    self.loaded.files.path(at.start.source),
                    at.start.line,
                    at.start.col
                )
            })
            .collect();
        let message = if names.len() == 2 {
            format!("module {} imports itself", names[0])
        } else {
            format!("import cycle: {}", names.join(" -> "))
        };
        self.errors.push(
            Diagnostic::new(message)
                .with_span(span.start, span.end)
                .with_notes(notes)
                .with_help(Some(
                    "modules can't import each other in a cycle; move what they share into a module \
                     that both import"
                        .to_string(),
                )),
        );
    }
}
