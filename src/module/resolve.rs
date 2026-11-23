//! Name resolution across modules.
//!
//! Every definition gets a name that says which module it is in: `area` in
//! `geo/shapes.aja` becomes `geo::shapes::area`, while the file being run
//! keeps its names. Every use of a name is rewritten to the definition it
//! refers to, which is, in order: a local variable or type parameter; a
//! definition in the same file; a name the file imports; and otherwise
//! something the checker knows (a built-in, the prelude, or nothing, which
//! it reports). `shapes::area` refers to `area` in the module imported as
//! `shapes`, which must be `pub`.
//!
//! What's left for the type checker is privacy within types (methods and
//! fields), which needs types to decide.

use std::collections::{HashMap, HashSet};

use super::{file_name, Module};
use crate::{
    ast::{
        types::Type, Definition, Expr, Expression, Id, Pattern, PatternKind, Program, Span,
        Statement, TypeParam,
    },
    builtins::BUILTINS,
    diagnostic::{closest_match, Diagnostic},
    prelude, stdlib,
};

/// A function, struct, or enum defined at the top level of a module.
#[derive(Debug, Clone)]
struct Item {
    /// The name it has once resolved, `geo::shapes::area`.
    global: Id,
    public: bool,
}

/// The top-level definitions of one module, by the name written in it.
#[derive(Debug, Default)]
struct Items {
    /// Functions.
    values: HashMap<Id, Item>,
    /// Structs and enums.
    types: HashMap<Id, Item>,
}

/// Resolve the names in `modules` (the file being run first), and put their
/// definitions together into one program, with the errors found. The
/// program can be type-checked even when there are errors: a name that
/// couldn't be resolved becomes an [`Expression::Error`] (or a
/// [`Type::Error`] or [`PatternKind::Error`]), which the checker accepts
/// without reporting it again, and a private item is resolved anyway, so
/// the code using it is still checked.
pub fn resolve(modules: Vec<Module>) -> (Program, Vec<Diagnostic>) {
    let index: HashMap<Vec<Id>, usize> = modules
        .iter()
        .enumerate()
        .map(|(i, m)| (m.path.clone(), i))
        .collect();
    let items: Vec<Items> = modules.iter().map(collect_items).collect();
    let mut errors = Vec::new();
    for m in &modules {
        if !m.is_main() {
            reserved_names(m, &mut errors);
        }
    }

    let mut definitions = Vec::new();
    for (i, mut m) in modules.into_iter().enumerate() {
        let mut r = Resolver::new(i, &index, &items, &mut errors);
        r.imports(&m);
        for def in &mut m.program.definitions {
            r.definition(def);
        }
        definitions.append(&mut m.program.definitions);
    }

    errors.sort_by_key(Diagnostic::sort_key);
    (Program::new(definitions), errors)
}

/// The name a definition named `name` in `module` gets once resolved.
fn global_name(module: &Module, name: &Id) -> Id {
    if module.is_main() {
        name.clone()
    } else {
        format!("{}::{name}", module.name()).into()
    }
}

fn collect_items(module: &Module) -> Items {
    let mut items = Items::default();
    for def in &module.program.definitions {
        let (map, name, public) = match def {
            Definition::Function { name, public, .. } => (&mut items.values, name, *public),
            Definition::Struct { name, public, .. } | Definition::Enum { name, public, .. } => {
                (&mut items.types, name, *public)
            }
            Definition::Impl { .. } => continue,
        };
        // A second definition of a name keeps the first; the checker
        // reports it, since both get the same global name.
        map.entry(name.clone()).or_insert(Item {
            global: global_name(module, name),
            public,
        });
    }
    items
}

/// Report definitions in an imported module that take a name the checker
/// reserves: a built-in type or function, or a prelude type. (The checker
/// reports these itself in the file being run, whose names it sees as
/// written.)
fn reserved_names(module: &Module, errors: &mut Vec<Diagnostic>) {
    for def in &module.program.definitions {
        let (name, span, is_type) = match def {
            Definition::Function {
                name, name_span, ..
            } => (name, *name_span, false),
            Definition::Struct {
                name, name_span, ..
            }
            | Definition::Enum {
                name, name_span, ..
            } => (name, *name_span, true),
            Definition::Impl { .. } => continue,
        };
        let message = if is_type && Type::try_from(name.to_string()).is_ok() {
            format!("'{name}' is a built-in type, so it can't be redefined")
        } else if is_type && prelude::is_prelude_type(name) {
            format!("'{name}' is defined in the prelude, so it can't be redefined")
        } else if !is_type && BUILTINS.iter().any(|b| b.name == name.as_str()) {
            format!("function '{name}' conflicts with the built-in function of the same name")
        } else {
            continue;
        };
        errors.push(error(message, span, "choose a different name"));
    }
}

fn error(message: impl Into<String>, span: Span, help: impl Into<String>) -> Diagnostic {
    Diagnostic::new(message)
        .with_span(span.start, span.end)
        .with_help(Some(help.into()))
}

/// Resolves the names in one module.
struct Resolver<'a> {
    /// This module's index.
    this: usize,
    /// Each module's path, by index.
    paths: Vec<Vec<Id>>,
    index: &'a HashMap<Vec<Id>, usize>,
    items: &'a [Items],
    /// The modules imported whole, by the name they're used through.
    aliases: HashMap<Id, usize>,
    /// The functions this module can name without a path: its own, and those
    /// it imports, by written name, with their global names.
    values: HashMap<Id, Id>,
    /// Likewise for structs and enums.
    types: HashMap<Id, Id>,
    /// Names whose import failed (and has been reported), so their uses
    /// aren't reported again.
    poisoned: HashSet<Id>,
    /// The local variables in scope, innermost scope last.
    locals: Vec<HashSet<Id>>,
    /// The type parameters in scope.
    type_params: Vec<Id>,
    errors: &'a mut Vec<Diagnostic>,
}

impl<'a> Resolver<'a> {
    fn new(
        this: usize,
        index: &'a HashMap<Vec<Id>, usize>,
        items: &'a [Items],
        errors: &'a mut Vec<Diagnostic>,
    ) -> Self {
        let mut paths = vec![Vec::new(); index.len()];
        for (path, &i) in index {
            paths[i] = path.clone();
        }
        let own = &items[this];
        Resolver {
            this,
            paths,
            index,
            items,
            aliases: HashMap::new(),
            values: own
                .values
                .iter()
                .map(|(n, i)| (n.clone(), i.global.clone()))
                .collect(),
            types: own
                .types
                .iter()
                .map(|(n, i)| (n.clone(), i.global.clone()))
                .collect(),
            poisoned: HashSet::new(),
            locals: Vec::new(),
            type_params: Vec::new(),
            errors,
        }
    }

    fn error(&mut self, message: impl Into<String>, span: Span, help: impl Into<String>) {
        self.errors.push(error(message, span, help));
    }

    /// Whether this module can use what is private to `module`: it is this
    /// one, or both are in the standard library, which is written as one
    /// package.
    fn sees_private(&self, module: usize) -> bool {
        let is_std = |path: &[Id]| path.len() > 1 && path[0] == "std";
        module == self.this || (is_std(&self.paths[self.this]) && is_std(&self.paths[module]))
    }

    fn module_name(&self, module: usize) -> String {
        super::path_name(&self.paths[module])
    }

    // ---- Imports ---------------------------------------------------------------

    /// Bring the names `module` imports into scope.
    fn imports(&mut self, module: &Module) {
        for import in &module.program.imports {
            let path: Vec<Id> = import.path.iter().map(|(s, _)| s.clone()).collect();
            // A module that couldn't be loaded has been reported.
            let Some(&target) = self.index.get(&path) else {
                continue;
            };
            if path.len() == 2 && path[0] == "std" && path[1] == stdlib::NATIVE && !module.is_std()
            {
                self.error(
                    format!(
                        "module '{}' is internal to the standard library",
                        super::path_name(&path)
                    ),
                    import.path_span(),
                    "use the standard library modules built on it, such as `std::io`",
                );
                match &import.items {
                    None => {
                        self.poisoned.insert(path[1].clone());
                    }
                    Some(items) => self.poisoned.extend(items.iter().map(|(n, _)| n.clone())),
                }
                continue;
            }
            match &import.items {
                None => {
                    let (alias, span) = import.path.last().expect("an import names a module");
                    self.import_module(alias, *span, target);
                }
                Some(items) => {
                    for (name, span) in items {
                        self.import_item(name, *span, target);
                    }
                }
            }
        }
    }

    /// `import a::b;`, which makes `b::item` available.
    fn import_module(&mut self, alias: &Id, span: Span, target: usize) {
        if let Some(&other) = self.aliases.get(alias) {
            let message = if other == target {
                format!(
                    "module '{}' is imported more than once",
                    self.module_name(target)
                )
            } else {
                format!(
                    "two modules are imported as '{alias}': '{}' and '{}'",
                    self.module_name(other),
                    self.module_name(target)
                )
            };
            let help = if other == target {
                "remove this import".to_string()
            } else {
                format!(
                    "import what you need from one of them by name instead: `import {}::{{...}};`",
                    self.module_name(target)
                )
            };
            return self.error(message, span, help);
        }
        if self.values.contains_key(alias) || self.types.contains_key(alias) {
            return self.error(
                format!("'{alias}' is both an imported module and a name in this file"),
                span,
                "rename the definition, or import what you need from the module by name",
            );
        }
        self.aliases.insert(alias.clone(), target);
    }

    /// `import a::{item};`, which makes `item` available by itself.
    fn import_item(&mut self, name: &Id, span: Span, target: usize) {
        let items = &self.items[target];
        let found: Vec<(bool, &Item)> = [
            items.values.get(name).map(|i| (true, i)),
            items.types.get(name).map(|i| (false, i)),
        ]
        .into_iter()
        .flatten()
        .collect();
        if found.is_empty() {
            if !self.values.contains_key(name) && !self.types.contains_key(name) {
                self.poisoned.insert(name.clone());
            }
            return self.no_item(target, name, span);
        }
        let mut reported = false;
        for (is_value, item) in found {
            // Reported, but imported anyway, so its uses are checked.
            if !item.public && !reported && !self.sees_private(target) {
                self.private(target, name, span);
                reported = true;
            }
            let scope = if is_value {
                &mut self.values
            } else {
                &mut self.types
            };
            match scope.get(name) {
                Some(existing) if *existing == item.global => {
                    let message = format!("'{name}' is imported more than once");
                    self.error(message, span, "remove one of the imports");
                }
                Some(_) => {
                    let message = format!(
                        "'{name}' is imported from '{}', but this file already has a '{name}'",
                        self.module_name(target)
                    );
                    self.error(
                        message,
                        span,
                        format!(
                            "import the module instead and write `{}::{name}`",
                            self.paths[target].last().unwrap()
                        ),
                    );
                }
                None => {
                    scope.insert(name.clone(), item.global.clone());
                }
            }
        }
    }

    fn no_item(&mut self, module: usize, name: &Id, span: Span) {
        let items = &self.items[module];
        let public: Vec<&str> = items
            .values
            .iter()
            .chain(&items.types)
            .filter(|(_, i)| i.public)
            .map(|(n, _)| n.as_str())
            .collect();
        let help = match closest_match(name, public.iter().copied()) {
            Some(s) => format!("an item with a similar name exists: '{s}'"),
            None if public.is_empty() => "it has no 'pub' items".to_string(),
            None => {
                let mut public = public;
                public.sort();
                format!("its 'pub' items are: {}", public.join(", "))
            }
        };
        let message = format!(
            "module '{}' has no item named '{name}'",
            self.module_name(module)
        );
        self.error(message, span, help);
    }

    fn private(&mut self, module: usize, name: &Id, span: Span) {
        let message = format!(
            "'{name}' is private to module '{}'",
            self.module_name(module)
        );
        let help = format!(
            "mark it 'pub' in '{}' to use it from other modules",
            file_name(&self.paths[module])
        );
        self.error(message, span, help);
    }

    /// The item `name` of the module imported as `alias`, as its global name,
    /// if it exists (reporting it otherwise, and reporting it if it isn't
    /// `pub`). `types` says to look only at structs and enums.
    fn module_item(&mut self, module: usize, name: &Id, types: bool, span: Span) -> Option<Id> {
        let items = &self.items[module];
        let item = if types {
            items.types.get(name)
        } else {
            items.values.get(name).or_else(|| items.types.get(name))
        };
        match item {
            None => {
                self.no_item(module, name, span);
                None
            }
            Some(item) if !item.public && !self.sees_private(module) => {
                self.private(module, name, span);
                Some(item.global.clone())
            }
            Some(item) => Some(item.global.clone()),
        }
    }

    // ---- Names ------------------------------------------------------------------

    /// The global name of the value `name`, written by itself, or `None` if
    /// it is the name of a failed import.
    fn value(&self, name: &Id) -> Option<Id> {
        if self.locals.iter().any(|scope| scope.contains(name)) {
            return Some(name.clone());
        }
        if self.poisoned.contains(name) {
            return None;
        }
        Some(
            self.values
                .get(name)
                .cloned()
                .unwrap_or_else(|| name.clone()),
        )
    }

    /// The global name of the struct or enum written `name` (`Shape`, or
    /// `shapes::Shape`) at `span`, or `None` if it can't have one, having
    /// reported why.
    fn type_name(&mut self, name: &Id, span: Span) -> Option<Id> {
        let segments: Vec<&str> = name.split("::").collect();
        match segments[..] {
            [_] if self.type_params.contains(name) => Some(name.clone()),
            [_] if self.poisoned.contains(name) => None,
            [_] => Some(
                self.types
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| name.clone()),
            ),
            [module, _] if self.poisoned.contains(module) => None,
            [module, item] => match self.aliases.get(module).copied() {
                Some(m) => self.module_item(m, &item.into(), true, span),
                // `Shape::Circle` where a type is expected: the checker
                // reports it.
                None if self.types.contains_key(module) || prelude::is_prelude_type(module) => {
                    Some(name.clone())
                }
                None => {
                    self.not_imported(module, span);
                    None
                }
            },
            _ => {
                self.too_long(&segments, span);
                None
            }
        }
    }

    fn not_imported(&mut self, module: &str, span: Span) {
        self.error(
            format!("'{module}' is not a module imported in this file"),
            span,
            format!("import it at the top of the file: `import {module};`"),
        );
    }

    /// Report a path through more than one module, `a::b::C`.
    fn too_long(&mut self, segments: &[&str], span: Span) {
        let module = segments[..segments.len() - 1].join("::");
        let last = segments[segments.len() - 2];
        self.error(
            format!(
                "'{}' names an item through more than one module",
                segments.join("::")
            ),
            span,
            format!(
                "import the module, `import {module};`, and write `{last}::{}`",
                segments[segments.len() - 1]
            ),
        );
    }

    fn ty(&mut self, t: &mut Type) {
        match t {
            Type::Named(name, args, span) => match self.type_name(name, *span) {
                Some(global) => {
                    *name = global;
                    for a in args {
                        self.ty(a);
                    }
                }
                None => *t = Type::Error,
            },
            Type::Fn {
                param_types,
                ret_type,
            } => {
                for p in param_types {
                    self.ty(p);
                }
                self.ty(ret_type);
            }
            Type::Int | Type::Float | Type::Bool | Type::String | Type::Void | Type::Error => {}
        }
    }

    fn with_type_params<R>(&mut self, params: &[TypeParam], f: impl FnOnce(&mut Self) -> R) -> R {
        let n = self.type_params.len();
        self.type_params
            .extend(params.iter().map(|p| p.name.clone()));
        let r = f(self);
        self.type_params.truncate(n);
        r
    }

    fn bind(&mut self, name: &Id) {
        self.locals
            .last_mut()
            .expect("a local is bound inside a scope")
            .insert(name.clone());
    }

    fn scoped<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.locals.push(HashSet::new());
        let r = f(self);
        self.locals.pop();
        r
    }

    // ---- Definitions -------------------------------------------------------------

    /// Report a `native` definition outside the standard library.
    fn native_here(&mut self, native: bool, span: Span) {
        let path = &self.paths[self.this];
        if native && !(path.len() > 1 && path[0] == "std") {
            self.error(
                "only the standard library can declare native functions and types",
                span,
                "write the function in Aja, with a body",
            );
        }
    }

    fn definition(&mut self, def: &mut Definition) {
        match def {
            Definition::Function {
                native, name_span, ..
            }
            | Definition::Struct {
                native, name_span, ..
            } => self.native_here(*native, *name_span),
            Definition::Impl { methods, .. } => {
                for m in methods.iter() {
                    self.native_here(m.native, m.name_span);
                }
            }
            Definition::Enum { .. } => {}
        }
        match def {
            Definition::Function {
                name,
                type_params,
                params,
                body,
                rtype,
                ..
            } => {
                *name = self.values[&*name].clone();
                self.with_type_params(type_params, |r| {
                    for p in params.iter_mut() {
                        if let Some(t) = &mut p.t {
                            r.ty(t);
                        }
                    }
                    if let Some(t) = rtype {
                        r.ty(t);
                    }
                    r.scoped(|r| {
                        for p in params.iter() {
                            r.bind(&p.name);
                        }
                        r.stmt(body);
                    });
                });
            }
            Definition::Struct {
                name,
                type_params,
                fields,
                ..
            } => {
                *name = self.types[&*name].clone();
                self.with_type_params(type_params, |r| {
                    for f in fields {
                        r.ty(&mut f.t);
                    }
                });
            }
            Definition::Enum {
                name,
                type_params,
                variants,
                ..
            } => {
                *name = self.types[&*name].clone();
                self.with_type_params(type_params, |r| {
                    for v in variants {
                        for t in &mut v.fields {
                            r.ty(t);
                        }
                    }
                });
            }
            Definition::Impl {
                type_params,
                target,
                methods,
            } => self.with_type_params(type_params, |r| {
                let written = target.to_string();
                r.ty(target);
                r.impl_here(target, &written);
                for m in methods {
                    r.with_type_params(&m.type_params, |r| {
                        for p in m.params.iter_mut() {
                            if let Some(t) = &mut p.t {
                                r.ty(t);
                            }
                        }
                        if let Some(t) = &mut m.rtype {
                            r.ty(t);
                        }
                        r.scoped(|r| {
                            for p in &m.params {
                                r.bind(&p.name);
                            }
                            r.stmt(&mut m.body);
                        });
                    });
                }
            }),
        }
    }

    /// Report an impl for a type that another module defines.
    fn impl_here(&mut self, target: &Type, written: &str) {
        let Type::Named(name, _, span) = target else {
            return;
        };
        let own = self.items[self.this]
            .types
            .values()
            .any(|i| i.global == *name);
        let elsewhere =
            self.items.iter().enumerate().find(|(i, items)| {
                *i != self.this && items.types.values().any(|t| t.global == *name)
            });
        if let (false, Some((module, _))) = (own, elsewhere) {
            let message = format!(
                "methods for '{written}' can only be defined in module '{}', where it is defined",
                self.module_name(module)
            );
            self.errors.push(
                error(message, *span, "write a function that takes it instead").with_notes(vec![
                    "a type's methods are all defined next to it, so there is one place to look for them"
                        .to_string(),
                ]),
            );
        }
    }

    // ---- Code ---------------------------------------------------------------------

    fn stmt(&mut self, stmt: &mut Statement) {
        match stmt {
            Statement::Block(stmts, tail) => self.scoped(|r| {
                for s in stmts {
                    r.stmt(s);
                }
                if let Some(e) = tail {
                    r.expr(e);
                }
            }),
            Statement::Expr(e) => self.expr(e),
            Statement::Let { var, value, .. } => {
                if let Some(t) = &mut var.t {
                    self.ty(t);
                }
                // A closure can refer to itself by the name it is bound to.
                if value.is_closure() {
                    self.bind(&var.name);
                    self.expr(value);
                } else {
                    self.expr(value);
                    self.bind(&var.name);
                }
            }
            Statement::If(cond, body) | Statement::While(cond, body) => {
                self.expr(cond);
                self.stmt(body);
            }
            Statement::Return(e) => {
                if let Some(e) = e {
                    self.expr(e);
                }
            }
            Statement::For { var, iter, body } => {
                self.expr(iter);
                self.scoped(|r| {
                    r.bind(&var.name);
                    r.stmt(body);
                });
            }
            Statement::Break(_) | Statement::Continue(_) => {}
        }
    }

    fn expr(&mut self, e: &mut Expr) {
        let span = e.span;
        match &mut e.kind {
            Expression::Int(_)
            | Expression::Float(_)
            | Expression::String(_)
            | Expression::True
            | Expression::False => {}
            Expression::Unary(_, x) | Expression::Grouping(x) | Expression::Try(x) => self.expr(x),
            Expression::BinOp(a, _, b) => {
                self.expr(a);
                self.expr(b);
            }
            Expression::If(cond, then, otherwise) => {
                self.expr(cond);
                self.stmt(then);
                self.stmt(otherwise);
            }
            Expression::Var(name) => match self.value(name) {
                Some(global) => *name = global,
                None => e.kind = Expression::Error(Vec::new()),
            },
            Expression::Assign(name, value) => {
                self.expr(value);
                match self.value(name) {
                    Some(global) => *name = global,
                    None => {
                        let value =
                            std::mem::replace(&mut **value, Expr::new(Expression::True, span));
                        e.kind = Expression::Error(vec![value]);
                    }
                }
            }
            Expression::Error(es) => {
                for x in es {
                    self.expr(x);
                }
            }
            Expression::Call(callee, args) => {
                self.expr(callee);
                for a in args {
                    self.expr(a);
                }
            }
            Expression::Closure(params, rtype, body) => {
                for p in params.iter_mut() {
                    if let Some(t) = &mut p.t {
                        self.ty(t);
                    }
                }
                if let Some(t) = rtype {
                    self.ty(t);
                }
                self.scoped(|r| {
                    for p in params.iter() {
                        r.bind(&p.name);
                    }
                    r.stmt(body);
                });
            }
            Expression::Field(obj, _, _) => self.expr(obj),
            Expression::List(items) => {
                for x in items {
                    self.expr(x);
                }
            }
            Expression::Index(list, index) => {
                self.expr(list);
                self.expr(index);
            }
            Expression::Path(prefix, last) => {
                if let Some(kind) = self.path(prefix, last, span) {
                    e.kind = kind;
                }
            }
            Expression::StructLit {
                name,
                name_span,
                fields,
                base,
            } => {
                for f in fields.iter_mut() {
                    self.expr(&mut f.value);
                }
                if let Some(b) = base {
                    self.expr(b);
                }
                match self.type_name(name, *name_span) {
                    Some(global) => *name = global,
                    None => {
                        let mut values: Vec<Expr> = std::mem::take(fields)
                            .into_iter()
                            .map(|f| f.value)
                            .collect();
                        values.extend(base.take().map(|b| *b));
                        e.kind = Expression::Error(values);
                    }
                }
            }
            Expression::Match(scrutinee, arms) => {
                self.expr(scrutinee);
                for arm in arms {
                    self.scoped(|r| {
                        r.pattern(&mut arm.pattern);
                        if let Some(g) = &mut arm.guard {
                            r.expr(g);
                        }
                        r.stmt(&mut arm.body);
                    });
                }
            }
        }
    }

    /// `prefix::last` in an expression: a variant or associated function of
    /// a type (`Shape::Circle`, `shapes::Shape::Circle`), or an item of a
    /// module (`shapes::area`). Returns what it becomes, if that changes:
    /// an [`Expression::Error`] if it can't be resolved.
    fn path(&mut self, prefix: &Id, last: &Id, span: Span) -> Option<Expression> {
        let error = || Some(Expression::Error(Vec::new()));
        let segments: Vec<&str> = prefix.split("::").collect();
        match segments[..] {
            [_] if self.type_params.contains(prefix) => None,
            [_] if self.types.contains_key(prefix) => {
                Some(Expression::Path(self.types[prefix].clone(), last.clone()))
            }
            [_] if self.poisoned.contains(prefix) => error(),
            [module] => {
                let m = *self.aliases.get(module)?;
                match self.module_item(m, last, false, span) {
                    Some(global) => Some(Expression::Var(global)),
                    None => error(),
                }
            }
            [_, _] => match self.type_name(prefix, span) {
                Some(t) => Some(Expression::Path(t, last.clone())),
                None => error(),
            },
            _ => {
                let mut all = segments.clone();
                all.push(last);
                self.too_long(&all[..all.len() - 1], span);
                error()
            }
        }
    }

    fn pattern(&mut self, p: &mut Pattern) {
        let span = p.span;
        match &mut p.kind {
            PatternKind::Wildcard
            | PatternKind::Int(_)
            | PatternKind::Str(_)
            | PatternKind::Bool(_) => {}
            PatternKind::Binding(var) => self.bind(&var.name),
            PatternKind::Variant {
                enum_name,
                variant,
                fields,
                ..
            } => {
                for f in fields.iter_mut() {
                    self.pattern(f);
                }
                let global = if self.aliases.contains_key(enum_name.as_str())
                    && !self.types.contains_key(enum_name.as_str())
                {
                    self.error(
                        format!("'{enum_name}::{variant}' is not a variant"),
                        span,
                        format!(
                            "a variant is written with its enum's name: `{enum_name}::Enum::{variant}`"
                        ),
                    );
                    None
                } else {
                    self.type_name(enum_name, span)
                };
                match global {
                    Some(global) => *enum_name = global,
                    None => p.kind = PatternKind::Error(std::mem::take(fields)),
                }
            }
            PatternKind::Struct {
                name,
                name_span,
                fields,
                ..
            } => {
                for (_, _, f) in fields.iter_mut() {
                    self.pattern(f);
                }
                match self.type_name(name, *name_span) {
                    Some(global) => *name = global,
                    None => {
                        let subs = std::mem::take(fields)
                            .into_iter()
                            .map(|(_, _, f)| f)
                            .collect();
                        p.kind = PatternKind::Error(subs);
                    }
                }
            }
            PatternKind::Error(subs) => {
                for f in subs {
                    self.pattern(f);
                }
            }
        }
    }
}
