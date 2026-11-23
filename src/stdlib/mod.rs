//! The standard library, written in Aja and built into the binary, so it
//! works with nothing to install.
//!
//! Its modules are imported like any other, by paths that start with
//! `std`: `import std::io;` is `stdlib/io.aja`. They are written on the
//! intrinsics in `std::native` (`native.aja`), which only the standard
//! library can import.

/// Each module's path after `std::`, and its source.
pub const MODULES: &[(&str, &str)] = &[
    ("io", include_str!("io.aja")),
    (NATIVE, include_str!("native.aja")),
];

/// The path of the module of intrinsics, after `std::`.
pub const NATIVE: &str = "native";

/// The source of the module `std::<name>`.
pub fn source(name: &str) -> Option<&'static str> {
    MODULES.iter().find(|(n, _)| *n == name).map(|(_, s)| *s)
}

/// How a module's file is shown in messages: `<std>/io.aja`.
pub fn display(name: &str) -> String {
    format!("<std>/{}.aja", name.replace("::", "/"))
}
