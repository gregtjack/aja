//! Aja's backends, as a library that the `aja` tool is built on: the
//! bytecode compiler and VM ([`vm`]) and ahead-of-time compilation
//! ([`aot`]). Reading and checking programs is the `aja-frontend` crate,
//! and what a running program needs is `aja-runtime`, which a program
//! compiled ahead of time is linked with.

pub mod aot;
pub mod module;
pub mod vm;

pub use aja_frontend::{ast, builtins, check, lexer, parse, prelude, stdlib, token};
pub use aja_runtime::diagnostic;

#[cfg(test)]
mod tests {
    /// A backend puts each built-in in the global slot of its place here,
    /// which the runtime knows by name alone.
    #[test]
    fn the_runtime_has_the_same_names_in_the_same_order() {
        let names: Vec<&str> = crate::builtins::BUILTINS.iter().map(|b| b.name).collect();
        assert_eq!(names, aja_runtime::builtins::NAMES);
    }
}
