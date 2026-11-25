//! Modules: what the front end loads and checks (see
//! [`aja_frontend::module`]), compiled for the VM.

use crate::{
    diagnostic::{Diagnostic, Files},
    vm::{Compiled, Compiler},
};

pub use aja_frontend::module::*;

#[cfg(test)]
mod tests;

/// Load `main` (a file in `root`) and the modules it imports, check them,
/// and compile them to bytecode, returning their files and the compiled
/// program, or every error found.
pub fn build(root: &dyn Root, main: &str) -> (Files, Result<Compiled, Vec<Diagnostic>>) {
    let (files, checked) = check(root, main);
    let compiled = checked.and_then(|checked| {
        Compiler::new()
            .program(&checked.program, &checked.types)
            .map(|mut compiled| {
                compiled.library = checked.library;
                compiled
            })
            .map_err(|e| vec![e])
    });
    (files, compiled)
}
