//! What a native program's runtime needs to know about the program besides
//! its code: its string literals, struct and enum layouts, native
//! functions, and, for error messages, its functions' names, the places
//! in the source that can fail, and the source itself.
//!
//! The compiler writes it into the object file as bytes ([`Meta::encode`]),
//! and the runtime reads it back when the program starts
//! ([`Meta::decode`]). Both ends are this code, in one version of `aja`,
//! so the format has no version and can change freely.

use crate::{
    ast::{Location, Source, Span},
    bytecode::Capture,
};

#[derive(Debug, Default, PartialEq)]
pub struct Meta {
    /// The program's source files, as (path, text), in [`Source`] order.
    pub files: Vec<(String, String)>,
    /// The string literals, with their numbers.
    pub literals: Vec<(u32, String)>,
    /// The native functions, with their global slots.
    pub natives: Vec<(u32, String)>,
    pub layouts: Vec<LayoutMeta>,
    /// The functions, in the order of the table of their code.
    pub functions: Vec<FnMeta>,
    /// How many global slots there are.
    pub globals: u32,
    /// The global slot of `main`.
    pub main: u32,
    /// Where `main` is named, if it returns a `Result`.
    pub main_result: Option<Span>,
    /// The source files that are the standard library's, by number.
    pub library: Vec<u32>,
    /// The prelude's source.
    pub prelude: String,
}

/// A [`crate::bytecode::Layout`].
#[derive(Debug, PartialEq)]
pub struct LayoutMeta {
    pub name: String,
    pub tag: u32,
    pub fields: Option<Vec<String>>,
}

#[derive(Debug, PartialEq)]
pub struct FnMeta {
    pub name: Option<String>,
    pub signature: String,
    pub arity: u8,
    pub num_regs: u16,
    /// The global slot a top-level function belongs in; `None` for a
    /// closure.
    pub slot: Option<u32>,
    pub captures: Vec<Capture>,
    /// The instructions that can fail or that call, with where each is in
    /// the source.
    pub sites: Vec<(u32, Span)>,
    /// The names of the functions that call instructions call, where the
    /// callee is a name.
    pub call_names: Vec<(u32, String)>,
}

impl Meta {
    pub fn encode(&self) -> Vec<u8> {
        let mut w = Writer(Vec::new());
        w.list(&self.files, |w, (path, text)| {
            w.str(path);
            w.str(text);
        });
        w.list(&self.literals, |w, (n, s)| {
            w.u32(*n);
            w.str(s);
        });
        w.list(&self.natives, |w, (slot, name)| {
            w.u32(*slot);
            w.str(name);
        });
        w.list(&self.layouts, |w, l| {
            w.str(&l.name);
            w.u32(l.tag);
            w.option(&l.fields, |w, fields| w.list(fields, |w, f| w.str(f)));
        });
        w.list(&self.functions, |w, f| {
            w.option(&f.name, |w, n| w.str(n));
            w.str(&f.signature);
            w.u32(f.arity as u32);
            w.u32(f.num_regs as u32);
            w.option(&f.slot, |w, s| w.u32(*s));
            w.list(&f.captures, |w, c| match c {
                Capture::Local(r) => {
                    w.u32(0);
                    w.u32(*r as u32);
                }
                Capture::Upvalue(i) => {
                    w.u32(1);
                    w.u32(*i as u32);
                }
                Capture::Itself => w.u32(2),
            });
            w.list(&f.sites, |w, (ip, span)| {
                w.u32(*ip);
                w.span(span);
            });
            w.list(&f.call_names, |w, (ip, name)| {
                w.u32(*ip);
                w.str(name);
            });
        });
        w.u32(self.globals);
        w.u32(self.main);
        w.option(&self.main_result, |w, s| w.span(s));
        w.list(&self.library, |w, id| w.u32(*id));
        w.str(&self.prelude);
        w.0
    }

    /// Read back what [`Meta::encode`] wrote. The bytes come from the
    /// program's own binary, so anything unexpected in them is a bug in
    /// `aja`, and panics.
    pub fn decode(bytes: &[u8]) -> Meta {
        let mut r = Reader(bytes);
        let meta = Meta {
            files: r.list(|r| (r.str(), r.str())),
            literals: r.list(|r| (r.u32(), r.str())),
            natives: r.list(|r| (r.u32(), r.str())),
            layouts: r.list(|r| LayoutMeta {
                name: r.str(),
                tag: r.u32(),
                fields: r.option(|r| r.list(|r| r.str())),
            }),
            functions: r.list(|r| FnMeta {
                name: r.option(|r| r.str()),
                signature: r.str(),
                arity: r.u32() as u8,
                num_regs: r.u32() as u16,
                slot: r.option(|r| r.u32()),
                captures: r.list(|r| match r.u32() {
                    0 => Capture::Local(r.u32() as u16),
                    1 => Capture::Upvalue(r.u32() as u16),
                    2 => Capture::Itself,
                    n => panic!("internal error: unknown capture kind {n}"),
                }),
                sites: r.list(|r| (r.u32(), r.span())),
                call_names: r.list(|r| (r.u32(), r.str())),
            }),
            globals: r.u32(),
            main: r.u32(),
            main_result: r.option(|r| r.span()),
            library: r.list(|r| r.u32()),
            prelude: r.str(),
        };
        assert!(
            r.0.is_empty(),
            "internal error: bytes left after the metadata"
        );
        meta
    }
}

struct Writer(Vec<u8>);

impl Writer {
    fn u32(&mut self, n: u32) {
        self.0.extend(n.to_le_bytes());
    }

    fn str(&mut self, s: &str) {
        self.u32(u32::try_from(s.len()).expect("a string in the metadata is under 4 GB"));
        self.0.extend(s.as_bytes());
    }

    fn list<T>(&mut self, items: &[T], mut each: impl FnMut(&mut Self, &T)) {
        self.u32(u32::try_from(items.len()).expect("a list in the metadata is under 2^32 long"));
        for item in items {
            each(self, item);
        }
    }

    fn option<T>(&mut self, value: &Option<T>, some: impl FnOnce(&mut Self, &T)) {
        match value {
            Some(v) => {
                self.u32(1);
                some(self, v);
            }
            None => self.u32(0),
        }
    }

    fn location(&mut self, l: &Location) {
        self.u32(l.line);
        self.u32(l.col);
        self.u32(l.pos);
        self.u32(match l.source {
            Source::File(id) => id as u32,
            Source::Prelude => u32::MAX,
        });
    }

    fn span(&mut self, s: &Span) {
        self.location(&s.start);
        self.location(&s.end);
    }
}

struct Reader<'a>(&'a [u8]);

impl Reader<'_> {
    fn u32(&mut self) -> u32 {
        let (n, rest) = self.0.split_at(4);
        self.0 = rest;
        u32::from_le_bytes(n.try_into().unwrap())
    }

    fn str(&mut self) -> String {
        let len = self.u32() as usize;
        let (s, rest) = self.0.split_at(len);
        self.0 = rest;
        String::from_utf8(s.to_vec()).expect("a string in the metadata is UTF-8")
    }

    fn list<T>(&mut self, mut each: impl FnMut(&mut Self) -> T) -> Vec<T> {
        let len = self.u32();
        (0..len).map(|_| each(self)).collect()
    }

    fn option<T>(&mut self, some: impl FnOnce(&mut Self) -> T) -> Option<T> {
        (self.u32() == 1).then(|| some(self))
    }

    fn location(&mut self) -> Location {
        let (line, col, pos) = (self.u32(), self.u32(), self.u32());
        let source = match self.u32() {
            u32::MAX => Source::Prelude,
            id => Source::File(id as u16),
        };
        Location {
            line,
            col,
            pos,
            source,
        }
    }

    fn span(&mut self) -> Span {
        Span::new(self.location(), self.location())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decoding_gives_back_what_was_encoded() {
        let at = |pos, source| Location {
            line: 3,
            col: 7,
            pos,
            source,
        };
        let span = Span::new(at(40, Source::File(2)), at(44, Source::Prelude));
        let meta = Meta {
            files: vec![("main.aja".into(), "fn main() {}\n".into())],
            literals: vec![(0, "héllo".into()), (3, String::new())],
            natives: vec![(9, "std::native::status".into())],
            layouts: vec![
                LayoutMeta {
                    name: "Point".into(),
                    tag: 0,
                    fields: Some(vec!["x".into(), "y".into()]),
                },
                LayoutMeta {
                    name: "Option::None".into(),
                    tag: 1,
                    fields: None,
                },
            ],
            functions: vec![FnMeta {
                name: Some("main".into()),
                signature: "() -> void".into(),
                arity: 2,
                num_regs: 300,
                slot: Some(12),
                captures: vec![Capture::Local(4), Capture::Upvalue(1), Capture::Itself],
                sites: vec![(5, span)],
                call_names: vec![(5, "f".into())],
            }],
            globals: 20,
            main: 12,
            main_result: Some(span),
            library: vec![1, 2],
            prelude: "enum Option<T> {}".into(),
        };
        assert_eq!(Meta::decode(&meta.encode()), meta);
    }
}
