//! Modules: what the front end loads and checks (see
//! [`aja_frontend::module`]), compiled for the VM.

use crate::{
    ast::{test_of, Definition},
    diagnostic::{Diagnostic, Files},
    vm::{Compiled, Compiler},
};

pub use aja_frontend::module::*;

#[cfg(test)]
mod tests;

/// Load `main` (a file in `root`) and the modules it imports, check them,
/// and compile them to bytecode, returning their files and the compiled
/// program, or every error found.
///
/// Its tests are checked too, but left out, since nothing runs them.
pub fn build(root: &dyn Root, main: &str) -> (Files, Result<Compiled, Vec<Diagnostic>>) {
    compile(root, main, false)
}

/// Like [`build`], but for running the program's tests, which are kept,
/// with or without a `main`. Only those in the program's own modules are:
/// the standard library's tests are its own.
pub fn build_tests(root: &dyn Root, main: &str) -> (Files, Result<Compiled, Vec<Diagnostic>>) {
    compile(root, main, true)
}

fn compile(root: &dyn Root, main: &str, tests: bool) -> (Files, Result<Compiled, Vec<Diagnostic>>) {
    let (files, checked) = check(root, main);
    let compiled = checked.and_then(|mut checked| {
        let library = checked.library;
        checked.program.definitions.retain(|def| match def {
            Definition::Function {
                name, name_span, ..
            } if test_of(name).is_some() => tests && !library.contains(&name_span.start.source),
            _ => true,
        });
        let mut compiler = Compiler::new();
        let compiled = if tests {
            compiler.tests(&checked.program, &checked.types)
        } else {
            compiler.program(&checked.program, &checked.types)
        };
        compiled
            .map(|mut compiled| {
                compiled.library = library;
                compiled
            })
            .map_err(|e| vec![e])
    });
    (files, compiled)
}
