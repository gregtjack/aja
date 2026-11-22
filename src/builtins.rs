//! Built-in functions: what they are called and what they accept, independent
//! of any execution backend.
//!
//! The type checker reads these signatures. Each backend implements every
//! built-in listed here (the VM's are in `vm/native.rs`), and
//! fails at startup, and in its tests, if one is missing.

use crate::ast::types::Type;

/// What a built-in accepts.
#[derive(Debug)]
pub enum Params {
    /// Exactly these parameters, as (name, type).
    Fixed(&'static [(&'static str, Type)]),
    /// Any number of arguments of any type. A variadic built-in can only be
    /// called directly, not passed around as a value.
    Variadic,
}

/// The signature of a built-in function.
#[derive(Debug)]
pub struct Builtin {
    pub name: &'static str,
    pub description: &'static str,
    pub params: Params,
    pub returns: Type,
}

pub static BUILTINS: &[Builtin] = &[
    Builtin {
        name: "print",
        description: "Prints its arguments separated by spaces",
        params: Params::Variadic,
        returns: Type::Void,
    },
    Builtin {
        name: "println",
        description: "Prints its arguments separated by spaces, then a newline",
        params: Params::Variadic,
        returns: Type::Void,
    },
    Builtin {
        name: "clock",
        description: "Returns the current time in milliseconds",
        params: Params::Fixed(&[]),
        returns: Type::Int,
    },
    Builtin {
        name: "sleep",
        description: "Sleeps for the given number of milliseconds",
        params: Params::Fixed(&[("ms", Type::Int)]),
        returns: Type::Void,
    },
    Builtin {
        name: "assert",
        description: "Stops the program if the condition is false",
        params: Params::Fixed(&[("condition", Type::Bool)]),
        returns: Type::Void,
    },
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn names_are_unique() {
        for (i, b) in BUILTINS.iter().enumerate() {
            assert!(
                BUILTINS[i + 1..].iter().all(|other| other.name != b.name),
                "built-in '{}' is defined twice",
                b.name
            );
        }
    }
}
