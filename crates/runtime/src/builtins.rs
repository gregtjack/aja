//! The built-in functions' names, in the order of their global slots.
//! The front end knows their signatures (`builtins.rs` in `aja-frontend`;
//! a test in the `aja` crate checks that the two lists agree); a backend only needs to know
//! which function goes in which slot.

pub const NAMES: &[&str] = &["print", "println", "clock", "sleep", "assert"];
