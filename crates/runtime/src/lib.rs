//! What a running Aja program needs, whichever backend runs it: values,
//! the garbage-collected heap, lists and maps, the native functions, the
//! scheduler, and how errors are reported.
//!
//! The VM (in the `aja` crate) is built on it, and a program compiled
//! ahead of time is linked with it: [`rt`] is what such a program runs on.
//! It holds none of the compiler, so a native program doesn't either.

/// Where things are in a program's source, and what names are.
pub mod ast {
    pub use aja_diagnostic::source::{Id, Location, Source, Span};
}
pub mod builtins;
pub mod bytecode;
pub use aja_diagnostic as diagnostic;
mod error;
pub mod hamt;
pub mod heap;
pub mod intrinsics;
pub mod layout;
pub mod list;
pub mod meta;
pub mod native;
pub mod net;
pub mod rt;
pub mod scheduler;
pub mod sorted;
pub mod value;

pub use error::{internal_error, Note, RuntimeError, TraceEntry, DEFAULT_STACK_LIMIT};
