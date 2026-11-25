//! The front end of Aja: everything between a program's source and a
//! type-checked syntax tree. [`module`] finds a program's files, [`lexer`]
//! and [`parse`] read them, and [`check`] checks them, with the [`prelude`]
//! and the standard library ([`stdlib`]) built in.
//!
//! It runs nothing and generates no code, so tools that only read programs
//! (a formatter, a language server) need nothing else; the backends are in
//! the `aja` crate.

pub use aja_diagnostic as diagnostic;

pub mod ast;
pub mod builtins;
pub mod check;
pub mod lexer;
pub mod module;
pub mod parse;
pub mod prelude;
pub mod stdlib;
pub mod token;
