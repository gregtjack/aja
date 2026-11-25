//! The Aja compiler and runtime, as a library: the `aja` tool is built on
//! it, and a program compiled ahead of time links against it for its
//! runtime (see [`aot`]).

pub mod aot;
pub mod ast;
pub mod builtins;
pub mod check;
pub mod diagnostic;
pub mod lexer;
pub mod module;
pub mod parse;
pub mod prelude;
pub mod stdlib;
pub mod token;
pub mod vm;
