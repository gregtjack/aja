//! The prelude: types every program can use without importing them, written
//! in Aja (`prelude.aja`) and compiled into the `aja` binary.
//!
//! The type checker and each backend add the prelude's definitions before a
//! program's own. Its enums' variants can be written by themselves (`Some(1)`
//! for `Option::Some(1)`); the parser expands them, so later passes only see
//! qualified variants.

use crate::{
    ast::{Definition, Source},
    lexer::tokenize_source,
    parse::Parser,
};

/// The prelude's source.
pub const SOURCE: &str = include_str!("prelude.aja");

/// The prelude's variants that can be written without their enum's name, and
/// the enum each belongs to.
pub const VARIANTS: &[(&str, &str)] = &[
    ("Some", "Option"),
    ("None", "Option"),
    ("Ok", "Result"),
    ("Err", "Result"),
];

/// The enum a prelude variant written by itself belongs to, e.g. `Option`
/// for `Some`.
pub fn variant_enum(name: &str) -> Option<&'static str> {
    VARIANTS.iter().find(|(v, _)| *v == name).map(|(_, e)| *e)
}

/// How a variant is written: by itself for the prelude's (`Some`), and with
/// its enum's name otherwise (`Shape::Circle`).
pub fn variant_display(enum_name: &str, variant: &str) -> String {
    match variant_enum(variant) {
        Some(e) if e == enum_name => variant.to_string(),
        _ => format!("{enum_name}::{variant}"),
    }
}

/// The prelude's definitions.
pub fn definitions() -> Vec<Definition> {
    let tokens = tokenize_source(SOURCE, Source::Prelude).expect("the prelude lexes");
    Parser::new(tokens.into_iter())
        .parse()
        .expect("the prelude parses")
        .definitions
}

/// The types the prelude defines.
pub const TYPES: &[&str] = &["Option", "Result", "Iter", "List"];

/// Whether `name` is a type the prelude defines.
pub fn is_prelude_type(name: &str) -> bool {
    TYPES.contains(&name)
}
