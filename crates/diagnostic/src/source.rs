//! Where things are in a program's source, and what names are: the parts
//! of the syntax tree that a running program still needs, for its error
//! messages.

use ecow::EcoString;

/// A name in a program.
pub type Id = EcoString;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub line: u32,
    pub col: u32,
    pub pos: u32,
    /// Which source the location is in: one of the program's files, or the
    /// prelude.
    pub source: Source,
}

/// A source of code, so that a location in one file isn't mistaken for one
/// in another. `File(0)` is the file being run (or the REPL); the files of
/// the modules it imports come after it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Source {
    File(u16),
    Prelude,
}

impl Source {
    /// The file being run.
    pub const MAIN: Source = Source::File(0);
}

impl Default for Source {
    fn default() -> Self {
        Source::MAIN
    }
}

impl Location {
    pub fn new(pos: u32, line: u32, col: u32) -> Self {
        Location {
            line,
            col,
            pos,
            source: Source::MAIN,
        }
    }
}

/// A region of source code; `end` is exclusive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Location,
    pub end: Location,
}

impl Span {
    pub fn new(start: Location, end: Location) -> Self {
        Span { start, end }
    }

    /// The span from the start of `self` to the end of `other`.
    pub fn to(self, other: Span) -> Span {
        Span::new(self.start, other.end)
    }
}
