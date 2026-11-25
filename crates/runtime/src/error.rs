//! The errors that stop a running program.

use crate::{
    ast::{Source, Span},
    diagnostic::{Diagnostic, Files},
    value::Value,
};

/// How much memory the call stack (frames and their registers) may use
/// before the VM reports a stack overflow: 256 MiB.
pub const DEFAULT_STACK_LIMIT: usize = 256 << 20;

/// An error that stops the program.
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
    /// The expression that failed.
    pub span: Option<Span>,
    /// The calls in progress, innermost first, with runs of identical calls
    /// (deep recursion) collapsed into one entry.
    pub trace: Vec<TraceEntry>,
    /// Notes after the trace: where the failing task was spawned, or, for
    /// a deadlock, what each task is waiting for.
    pub notes: Vec<Note>,
}

/// A note that points at places in the program: each `{}` in `text` is
/// replaced by where the next of `places` is, as in "line 3, column 5".
#[derive(Debug, Clone)]
pub struct Note {
    pub text: String,
    pub places: Vec<Span>,
}

/// One or more identical calls in an error's trace.
#[derive(Debug, Clone)]
pub struct TraceEntry {
    /// e.g. "function 'f'" or "an anonymous function".
    pub function: String,
    /// The call expression.
    pub site: Span,
    /// How many times in a row this call is on the stack.
    pub count: usize,
}

impl RuntimeError {
    /// The error as a diagnostic, for a program in one file.
    pub fn to_diagnostic(&self) -> Diagnostic {
        self.to_diagnostic_in(&Files::default())
    }

    /// The error as a diagnostic, naming which of `files` each call in its
    /// trace is in, when it isn't the file being run.
    pub fn to_diagnostic_in(&self, files: &Files) -> Diagnostic {
        const MAX_NOTES: usize = 8;
        let mut notes: Vec<String> = self
            .trace
            .iter()
            .take(MAX_NOTES)
            .map(|e| {
                let repeat = if e.count > 1 {
                    format!(" ({} times)", e.count)
                } else {
                    String::new()
                };
                let place = match e.site.start.source {
                    Source::MAIN => String::new(),
                    Source::Prelude => " in the prelude".to_string(),
                    file => format!(" in {}", files.path(file)),
                };
                format!(
                    "in {}, called{place} at line {}, column {}{repeat}",
                    e.function, e.site.start.line, e.site.start.col
                )
            })
            .collect();
        if self.trace.len() > MAX_NOTES {
            notes.push(format!(
                "... and {} more calls",
                self.trace.len() - MAX_NOTES
            ));
        }
        for note in &self.notes {
            let mut text = String::new();
            let mut places = note.places.iter();
            let mut parts = note.text.split("{}");
            text.push_str(parts.next().unwrap_or(""));
            for part in parts {
                if let Some(span) = places.next() {
                    let place = match span.start.source {
                        Source::MAIN => String::new(),
                        Source::Prelude => " in the prelude".to_string(),
                        file => format!(" in {}", files.path(file)),
                    };
                    text.push_str(&format!(
                        "line {}, column {}{place}",
                        span.start.line, span.start.col
                    ));
                }
                text.push_str(part);
            }
            notes.push(text);
        }

        let d = Diagnostic::new(self.message.clone()).with_notes(notes);
        match self.span {
            Some(span) => d.with_span(span.start, span.end),
            None => d,
        }
    }
}

/// A register held the wrong kind of value. The type checker rules this
/// out, so it is a bug in the compiler or VM.
#[cold]
pub fn internal_error(expected: &str, found: &Value) -> ! {
    panic!("internal error: expected {expected} in a register, found {found:?}")
}
