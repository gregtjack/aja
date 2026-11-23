//! The VM's implementations of the built-in functions declared in
//! `crate::builtins`.

use std::{
    cell::RefCell,
    fs,
    io::{self, BufRead, Read, Write},
    thread,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use super::{heap::Heap, value::Value};

/// Runs a built-in. The type checker has already checked the arguments
/// against the built-in's signature. The heap is there to read objects the
/// arguments refer to, and to put a string a built-in returns on. An error
/// message stops the program.
///
/// Allocating directly on the heap never collects, so the arguments stay
/// valid while a native function runs, and it can allocate what it returns
/// in several parts (a list of strings).
pub type NativeFn = fn(&mut Heap, &[Value]) -> Result<Value, String>;

// How the last intrinsic that can fail went, for `std::native::status`. The
// codes match `std::io::ErrorKind` in `stdlib/io.aja`.
const OK: i64 = 0;
const END_OF_INPUT: i64 = 1;
const NOT_FOUND: i64 = 2;
const PERMISSION_DENIED: i64 = 3;
const INVALID_DATA: i64 = 4;
const BROKEN_PIPE: i64 = 5;
const ALREADY_EXISTS: i64 = 6;
const OTHER: i64 = 7;
const NOT_A_DIRECTORY: i64 = 8;
const IS_A_DIRECTORY: i64 = 9;
const DIRECTORY_NOT_EMPTY: i64 = 10;

thread_local! {
    /// The status of the last intrinsic that can fail, and its message.
    static STATUS: RefCell<(i64, String)> = const { RefCell::new((OK, String::new())) };
}

fn set_status(code: i64, message: String) {
    STATUS.with(|s| *s.borrow_mut() = (code, message));
}

/// Record how an I/O operation went, giving its value if it succeeded.
fn record<T: Default>(result: io::Result<T>) -> T {
    record_as(result, |e| e)
}

/// Like [`record`], with the message of an error made by `message` from
/// the error's own description.
fn record_as<T: Default>(result: io::Result<T>, message: impl FnOnce(String) -> String) -> T {
    match result {
        Ok(v) => {
            set_status(OK, String::new());
            v
        }
        Err(e) => {
            let code = match e.kind() {
                io::ErrorKind::NotFound => NOT_FOUND,
                io::ErrorKind::PermissionDenied => PERMISSION_DENIED,
                io::ErrorKind::InvalidData => INVALID_DATA,
                io::ErrorKind::BrokenPipe => BROKEN_PIPE,
                io::ErrorKind::AlreadyExists => ALREADY_EXISTS,
                io::ErrorKind::NotADirectory => NOT_A_DIRECTORY,
                io::ErrorKind::IsADirectory => IS_A_DIRECTORY,
                io::ErrorKind::DirectoryNotEmpty => DIRECTORY_NOT_EMPTY,
                _ => OTHER,
            };
            set_status(code, message(describe(&e)));
            T::default()
        }
    }
}

/// An error's description, as Aja's messages are written: "no such file or
/// directory", not "No such file or directory (os error 2)".
fn describe(e: &io::Error) -> String {
    let text = e.to_string();
    let text = match text.rfind(" (os error ") {
        Some(i) if text.ends_with(')') => &text[..i],
        _ => &text,
    };
    let mut chars = text.chars();
    match chars.next() {
        // Lowercase a capital only when the next letter isn't one too
        // (so not "UTF-8").
        Some(c) if c.is_uppercase() && !chars.clone().next().is_some_and(char::is_uppercase) => {
            c.to_lowercase().chain(chars).collect()
        }
        _ => text.to_string(),
    }
}

/// Record how a file system operation on `path` went, with a message that
/// says what was being done: "could not read 'notes.txt': ...".
fn record_fs<T: Default>(result: io::Result<T>, doing: &str, path: &str) -> T {
    record_as(result, |e| format!("could not {doing} '{path}': {e}"))
}

// ---- The file system (`std::fs`) ---------------------------------------------

/// What `std::native::path_kind` says a path is.
const NO_PATH: i64 = 0;
const FILE: i64 = 1;
const DIRECTORY: i64 = 2;
const OTHER_KIND: i64 = 3;

fn path_kind(path: &str) -> i64 {
    match fs::metadata(path) {
        Err(_) => NO_PATH,
        Ok(m) if m.is_file() => FILE,
        Ok(m) if m.is_dir() => DIRECTORY,
        Ok(_) => OTHER_KIND,
    }
}

fn write_file(path: &str, text: &str, append: bool) -> io::Result<()> {
    fs::OpenOptions::new()
        .create(true)
        .write(true)
        .append(append)
        .truncate(!append)
        .open(path)?
        .write_all(text.as_bytes())
}

/// The names of the entries in directory `path`, sorted, so a program
/// sees them in the same order on every system.
fn read_dir(path: &str) -> io::Result<Vec<String>> {
    let mut names = Vec::new();
    for entry in fs::read_dir(path)? {
        let name = entry?.file_name();
        match name.into_string() {
            Ok(name) => names.push(name),
            Err(name) => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("the name {name:?} isn't valid UTF-8"),
                ))
            }
        }
    }
    names.sort();
    Ok(names)
}

/// The lines of `text`, without their line endings (`\n` or `\r\n`).
fn lines(text: &str) -> Vec<String> {
    text.lines().map(str::to_string).collect()
}

/// A list of strings for a native function to return.
fn str_list(heap: &mut Heap, items: Vec<String>) -> Value {
    let list: super::list::List = items.into_iter().map(|s| str_value(heap, s)).collect();
    Value::List(heap.alloc_list(list))
}

/// The arguments of a file system intrinsic that takes one path.
fn path_arg<'h>(heap: &'h Heap, name: &str, args: &[Value]) -> Result<&'h str, String> {
    match args {
        [Value::Str(p)] => Ok(heap.str(*p)),
        _ => Err(bad_args(name, args)),
    }
}

/// The arguments of a file system intrinsic that takes two paths.
fn two_paths<'h>(heap: &'h Heap, name: &str, args: &[Value]) -> Result<(&'h str, &'h str), String> {
    match args {
        [Value::Str(a), Value::Str(b)] => Ok((heap.str(*a), heap.str(*b))),
        _ => Err(bad_args(name, args)),
    }
}

fn bad_fd(fd: i64, what: &str) -> io::Error {
    io::Error::other(format!("file descriptor {fd} can't be {what}"))
}

/// Read a line from standard input, without its line ending. Standard
/// output is flushed first, so a prompt written without a newline shows.
fn read_line(fd: i64) -> io::Result<Option<String>> {
    if fd != 0 {
        return Err(bad_fd(fd, "read"));
    }
    io::stdout().flush()?;
    let mut line = String::new();
    if io::stdin().lock().read_line(&mut line)? == 0 {
        return Ok(None);
    }
    if line.ends_with('\n') {
        line.pop();
        if line.ends_with('\r') {
            line.pop();
        }
    }
    Ok(Some(line))
}

fn read_all(fd: i64) -> io::Result<String> {
    if fd != 0 {
        return Err(bad_fd(fd, "read"));
    }
    io::stdout().flush()?;
    let mut text = String::new();
    io::stdin().lock().read_to_string(&mut text)?;
    Ok(text)
}

fn write(fd: i64, text: &str) -> io::Result<()> {
    match fd {
        1 => io::stdout().write_all(text.as_bytes()),
        2 => io::stderr().write_all(text.as_bytes()),
        _ => Err(bad_fd(fd, "written to")),
    }
}

fn flush(fd: i64) -> io::Result<()> {
    match fd {
        1 => io::stdout().flush(),
        2 => io::stderr().flush(),
        _ => Err(bad_fd(fd, "written to")),
    }
}

/// A string for a native function to return.
fn str_value(heap: &mut Heap, s: String) -> Value {
    Value::Str(heap.alloc_str(s.into_boxed_str()))
}

/// The functions the VM implements, by name: the built-ins, and the
/// functions the prelude and standard library declare `native`, by the
/// name they are compiled under (`List::len`, `std::native::write`).
static NATIVES: &[(&str, NativeFn)] = &[
    ("print", |heap, args| {
        print!("{}", join(heap, args));
        Ok(Value::Void)
    }),
    ("println", |heap, args| {
        println!("{}", join(heap, args));
        Ok(Value::Void)
    }),
    ("clock", |_, _| {
        let ms = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis();
        Ok(Value::Int(ms as i64))
    }),
    ("sleep", |_, args| {
        let [Value::Int(ms)] = args else {
            return Err(bad_args("sleep", args));
        };
        thread::sleep(Duration::from_millis((*ms).max(0) as u64));
        Ok(Value::Void)
    }),
    ("assert", |_, args| match args {
        [Value::Bool(true)] => Ok(Value::Void),
        [Value::Bool(false)] => Err("assertion failed".to_string()),
        _ => Err(bad_args("assert", args)),
    }),
    ("std::native::status", |_, _| {
        Ok(Value::Int(STATUS.with(|s| s.borrow().0)))
    }),
    ("std::native::error_message", |heap, _| {
        let message = STATUS.with(|s| s.borrow().1.clone());
        Ok(str_value(heap, message))
    }),
    ("std::native::read_line", |heap, args| {
        let [Value::Int(fd)] = args else {
            return Err(bad_args("read_line", args));
        };
        let line = match read_line(*fd) {
            Ok(None) => {
                set_status(END_OF_INPUT, "end of input".to_string());
                String::new()
            }
            result => record(result).unwrap_or_default(),
        };
        Ok(str_value(heap, line))
    }),
    ("std::native::read_all", |heap, args| {
        let [Value::Int(fd)] = args else {
            return Err(bad_args("read_all", args));
        };
        let text = record(read_all(*fd));
        Ok(str_value(heap, text))
    }),
    ("std::native::write", |heap, args| {
        let [Value::Int(fd), Value::Str(text)] = args else {
            return Err(bad_args("write", args));
        };
        record(write(*fd, heap.str(*text)));
        Ok(Value::Void)
    }),
    ("std::native::flush", |_, args| {
        let [Value::Int(fd)] = args else {
            return Err(bad_args("flush", args));
        };
        record(flush(*fd));
        Ok(Value::Void)
    }),
    // ---- The file system (`std::fs`) --------------------------------------
    ("std::native::path_kind", |heap, args| {
        let path = path_arg(heap, "path_kind", args)?;
        Ok(Value::Int(path_kind(path)))
    }),
    ("std::native::file_size", |heap, args| {
        let path = path_arg(heap, "file_size", args)?;
        let size = record_fs(
            fs::metadata(path).map(|m| m.len()),
            "read the size of",
            path,
        );
        Ok(Value::Int(size as i64))
    }),
    ("std::native::read_file", |heap, args| {
        let path = path_arg(heap, "read_file", args)?;
        let text = record_fs(fs::read_to_string(path), "read", path);
        Ok(str_value(heap, text))
    }),
    ("std::native::read_file_lines", |heap, args| {
        let path = path_arg(heap, "read_file_lines", args)?;
        let text = record_fs(fs::read_to_string(path), "read", path);
        Ok(str_list(heap, lines(&text)))
    }),
    ("std::native::write_file", |heap, args| {
        let [Value::Str(path), Value::Str(text), Value::Bool(append)] = args else {
            return Err(bad_args("write_file", args));
        };
        let path = heap.str(*path);
        record_fs(write_file(path, heap.str(*text), *append), "write to", path);
        Ok(Value::Void)
    }),
    ("std::native::read_dir", |heap, args| {
        let path = path_arg(heap, "read_dir", args)?;
        let names = record_fs(read_dir(path), "read the directory", path);
        Ok(str_list(heap, names))
    }),
    ("std::native::create_dir", |heap, args| {
        let [Value::Str(path), Value::Bool(all)] = args else {
            return Err(bad_args("create_dir", args));
        };
        let path = heap.str(*path);
        let result = if *all {
            fs::create_dir_all(path)
        } else {
            fs::create_dir(path)
        };
        record_fs(result, "create the directory", path);
        Ok(Value::Void)
    }),
    ("std::native::remove_file", |heap, args| {
        let path = path_arg(heap, "remove_file", args)?;
        record_fs(fs::remove_file(path), "remove", path);
        Ok(Value::Void)
    }),
    ("std::native::remove_dir", |heap, args| {
        let [Value::Str(path), Value::Bool(all)] = args else {
            return Err(bad_args("remove_dir", args));
        };
        let path = heap.str(*path);
        let result = if *all {
            fs::remove_dir_all(path)
        } else {
            fs::remove_dir(path)
        };
        record_fs(result, "remove the directory", path);
        Ok(Value::Void)
    }),
    ("std::native::rename", |heap, args| {
        let (from, to) = two_paths(heap, "rename", args)?;
        record_fs(fs::rename(from, to), &format!("rename '{from}' to"), to);
        Ok(Value::Void)
    }),
    ("std::native::copy_file", |heap, args| {
        let (from, to) = two_paths(heap, "copy_file", args)?;
        record_fs(fs::copy(from, to), &format!("copy '{from}' to"), to);
        Ok(Value::Void)
    }),
    // ---- Lists (`List` in the prelude) ------------------------------------
    ("List::len", |heap, args| {
        let [Value::List(l)] = args else {
            return Err(bad_args("List::len", args));
        };
        Ok(Value::Int(heap.list(*l).len() as i64))
    }),
    ("List::push", |heap, args| {
        let [Value::List(l), value] = args else {
            return Err(bad_args("List::push", args));
        };
        let mut list = heap.list(*l).clone();
        list.push(value.clone());
        Ok(Value::List(heap.alloc_list(list)))
    }),
    ("List::set", |heap, args| {
        let [Value::List(l), Value::Int(i), value] = args else {
            return Err(bad_args("List::set", args));
        };
        let mut list = heap.list(*l).clone();
        let i = in_range(*i, list.len())?;
        list.set(i, value.clone());
        Ok(Value::List(heap.alloc_list(list)))
    }),
    ("List::drop_last", |heap, args| {
        let [Value::List(l)] = args else {
            return Err(bad_args("List::drop_last", args));
        };
        let mut list = heap.list(*l).clone();
        list.pop();
        Ok(Value::List(heap.alloc_list(list)))
    }),
    ("List::concat", |heap, args| {
        let [Value::List(a), Value::List(b)] = args else {
            return Err(bad_args("List::concat", args));
        };
        let mut list = heap.list(*a).clone();
        let more: Vec<Value> = heap.list(*b).iter().cloned().collect();
        for v in more {
            list.push(v);
        }
        Ok(Value::List(heap.alloc_list(list)))
    }),
    ("List::slice", |heap, args| {
        let [Value::List(l), Value::Int(start), Value::Int(end)] = args else {
            return Err(bad_args("List::slice", args));
        };
        let list = heap.list(*l);
        let len = list.len();
        if *start < 0 || *end < *start || *end > len as i64 {
            return Err(format!(
                "slice {start}..{end} is out of range for a list of length {len}"
            ));
        }
        let part: super::list::List = list
            .iter()
            .skip(*start as usize)
            .take((*end - *start) as usize)
            .cloned()
            .collect();
        Ok(Value::List(heap.alloc_list(part)))
    }),
];

/// The implementation of the function called `name`, and its name.
pub fn implementation(name: &str) -> Option<(&'static str, NativeFn)> {
    NATIVES.iter().find(|(n, _)| *n == name).copied()
}

fn join(heap: &Heap, args: &[Value]) -> String {
    args.iter()
        .map(|a| heap.display(a))
        .collect::<Vec<_>>()
        .join(" ")
}

/// `i` as an index into a list of `len` values, or the error if it is out
/// of range.
pub fn in_range(i: i64, len: usize) -> Result<usize, String> {
    if i >= 0 && (i as usize) < len {
        Ok(i as usize)
    } else {
        Err(format!(
            "index {i} is out of range for a list of length {len}"
        ))
    }
}

/// Arguments the type checker should have rejected.
fn bad_args(name: &str, args: &[Value]) -> String {
    let types: Vec<String> = args.iter().map(|a| format!("{a:?}")).collect();
    format!(
        "{name}() was called with unexpected arguments ({})",
        types.join(", ")
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builtins::BUILTINS;

    #[test]
    fn every_builtin_is_implemented() {
        for b in BUILTINS {
            assert!(
                implementation(b.name).is_some(),
                "built-in '{}' has no implementation",
                b.name
            );
        }
    }
}
