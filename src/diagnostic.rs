//! Human-readable error reports, rendered in a rustc-like style:
//!
//! ```text
//! error: expected '}', found end of file
//!  --> main.aja:3:8
//!   |
//! 3 |   x + 2
//!   |        ^
//!   = help: ...
//! ```

use std::io::IsTerminal;

use crate::ast::{Location, Source};

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    /// Start and end of the offending source (end is exclusive).
    pub span: Option<(Location, Location)>,
    pub help: Option<String>,
    /// Extra context, such as which function calls led to the error.
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn new(message: impl Into<String>) -> Self {
        Diagnostic {
            message: message.into(),
            span: None,
            help: None,
            notes: Vec::new(),
        }
    }

    pub fn with_span(mut self, start: Location, end: Location) -> Self {
        self.span = Some((start, end));
        self
    }

    pub fn with_help(mut self, help: Option<String>) -> Self {
        self.help = help;
        self
    }

    pub fn with_notes(mut self, notes: Vec<String>) -> Self {
        self.notes = notes;
        self
    }

    /// Render the diagnostic against `source`, labelling it with `path`. A
    /// location in the prelude is shown in the prelude instead.
    pub fn render(&self, source: &str, path: &str, color: bool) -> String {
        let (source, path) = match self.span {
            Some((start, _)) if start.source == Source::Prelude => (crate::prelude::SOURCE, "<prelude>"),
            _ => (source, path),
        };
        let style = Style::new(color);
        let mut out = format!(
            "{}error{}{}: {}{}\n",
            style.red, style.reset, style.bold, self.message, style.reset
        );

        let mut pad = String::from(" ");
        if let Some((start, end)) = self.span {
            let line_no = start.line.max(1) as usize;
            let line_text = source.lines().nth(line_no - 1).unwrap_or("");
            pad = " ".repeat(line_no.to_string().len());

            out += &format!(
                "{pad}{}-->{} {path}:{}:{}\n",
                style.blue, style.reset, line_no, start.col
            );
            out += &format!("{pad} {}|{}\n", style.blue, style.reset);
            out += &format!("{}{line_no} |{} {line_text}\n", style.blue, style.reset);

            // Underline the span; keep tabs so the caret lines up.
            let col = (start.col.max(1) - 1) as usize;
            let line_len = line_text.chars().count();
            let width = if end.line == start.line && end.col > start.col {
                (end.col - start.col) as usize
            } else {
                // A span that continues onto later lines: underline to end of line.
                line_len.saturating_sub(col)
            };
            let width = width.min(line_len.saturating_sub(col)).max(1);
            let prefix: String = line_text
                .chars()
                .chain(std::iter::repeat(' '))
                .take(col)
                .map(|c| if c == '\t' { '\t' } else { ' ' })
                .collect();
            out += &format!(
                "{pad} {}|{} {prefix}{}{}{}\n",
                style.blue,
                style.reset,
                style.red,
                "^".repeat(width),
                style.reset
            );
        }

        for note in &self.notes {
            out += &format!(
                "{pad} {}={} {}note{}: {note}\n",
                style.blue, style.reset, style.bold, style.reset
            );
        }

        if let Some(help) = &self.help {
            out += &format!(
                "{pad} {}={} {}help{}: {help}\n",
                style.blue, style.reset, style.bold, style.reset
            );
        }

        out
    }
}

/// Whether to colorize output written to stderr.
pub fn use_color() -> bool {
    std::env::var_os("NO_COLOR").is_none() && std::io::stderr().is_terminal()
}

struct Style {
    red: &'static str,
    blue: &'static str,
    bold: &'static str,
    reset: &'static str,
}

impl Style {
    fn new(color: bool) -> Self {
        if color {
            Style {
                red: "\x1b[1;31m",
                blue: "\x1b[1;34m",
                bold: "\x1b[1m",
                reset: "\x1b[0m",
            }
        } else {
            Style {
                red: "",
                blue: "",
                bold: "",
                reset: "",
            }
        }
    }
}

/// Levenshtein distance, used for "did you mean" suggestions.
pub fn edit_distance(a: &str, b: &str) -> usize {
    let b: Vec<char> = b.chars().collect();
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    for (i, ca) in a.chars().enumerate() {
        let mut cur = vec![i + 1];
        for (j, cb) in b.iter().enumerate() {
            let cost = if ca == *cb { 0 } else { 1 };
            cur.push((prev[j] + cost).min(prev[j + 1] + 1).min(cur[j] + 1));
        }
        prev = cur;
    }
    prev[b.len()]
}

/// The closest candidate to `name`, if any is close enough to be a likely typo.
pub fn closest_match<'a>(name: &str, candidates: impl IntoIterator<Item = &'a str>) -> Option<&'a str> {
    let max = (name.chars().count() / 3).max(1);
    candidates
        .into_iter()
        .filter(|c| *c != name)
        .map(|c| (edit_distance(name, c), c))
        .filter(|(d, _)| *d <= max)
        .min_by_key(|(d, _)| *d)
        .map(|(_, c)| c)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn loc(line: u32, col: u32) -> Location {
        Location::new(0, line, col)
    }

    #[test]
    fn renders_snippet_with_caret() {
        let d = Diagnostic::new("expected ';', found 'x'")
            .with_span(loc(2, 3), loc(2, 4))
            .with_help(Some("add a ';'".into()));
        let out = d.render("fn main() {\n  x\n}", "t.aja", false);
        assert_eq!(
            out,
            "error: expected ';', found 'x'\n --> t.aja:2:3\n  |\n2 |   x\n  |   ^\n  = help: add a ';'\n"
        );
    }

    #[test]
    fn suggests_close_names() {
        assert_eq!(closest_match("cout", ["count", "print"]), Some("count"));
        assert_eq!(closest_match("zzz", ["count", "print"]), None);
    }
}
