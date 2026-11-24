//! The VM's implementations of the built-in functions declared in
//! `crate::builtins`, except `sleep` and `std::native::sleep_nanos`, which
//! the scheduler implements (see `scheduler::Intrinsic`).

use std::{
    cell::RefCell,
    fs,
    io::{self, BufRead, Read, Write},
    sync::OnceLock,
    time::{Instant, SystemTime, UNIX_EPOCH},
};

use super::{
    hamt::Hamt,
    heap::{Gc, Heap},
    sorted::Tree,
    value::Value,
};

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
pub(super) const OK: i64 = 0;
pub(super) const END_OF_INPUT: i64 = 1;
const NOT_FOUND: i64 = 2;
const PERMISSION_DENIED: i64 = 3;
const INVALID_DATA: i64 = 4;
const BROKEN_PIPE: i64 = 5;
const ALREADY_EXISTS: i64 = 6;
const OTHER: i64 = 7;
const NOT_A_DIRECTORY: i64 = 8;
const IS_A_DIRECTORY: i64 = 9;
const DIRECTORY_NOT_EMPTY: i64 = 10;
const CONNECTION_REFUSED: i64 = 11;
const CONNECTION_RESET: i64 = 12;
const ADDR_IN_USE: i64 = 13;
const ADDR_NOT_AVAILABLE: i64 = 14;
pub(super) const NOT_CONNECTED: i64 = 15;

thread_local! {
    /// The status of the last intrinsic that can fail, and its message.
    static STATUS: RefCell<(i64, String)> = const { RefCell::new((OK, String::new())) };
}

pub(super) fn set_status(code: i64, message: String) {
    STATUS.with(|s| *s.borrow_mut() = (code, message));
}

/// The status, for a task that is being switched out, which takes it with
/// it. Each task has its own: another task's intrinsics mustn't change what
/// `status` says straight after a call.
pub(super) fn take_status() -> (i64, String) {
    STATUS.with(|s| std::mem::replace(&mut *s.borrow_mut(), (OK, String::new())))
}

/// Put back the status of a task that is being switched in.
pub(super) fn restore_status(status: (i64, String)) {
    STATUS.with(|s| *s.borrow_mut() = status);
}

/// Record how an I/O operation went, giving its value if it succeeded.
fn record<T: Default>(result: io::Result<T>) -> T {
    record_as(result, |e| e)
}

/// Like [`record`], with the message of an error made by `message` from
/// the error's own description.
pub(super) fn record_as<T: Default>(
    result: io::Result<T>,
    message: impl FnOnce(String) -> String,
) -> T {
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
                io::ErrorKind::ConnectionRefused => CONNECTION_REFUSED,
                io::ErrorKind::ConnectionReset | io::ErrorKind::ConnectionAborted => {
                    CONNECTION_RESET
                }
                io::ErrorKind::AddrInUse => ADDR_IN_USE,
                io::ErrorKind::AddrNotAvailable => ADDR_NOT_AVAILABLE,
                io::ErrorKind::NotConnected => NOT_CONNECTED,
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

/// The longest string a native function will make, in bytes, so a
/// mistake like `"x".repeat(n)` with a huge `n` stops the program instead
/// of exhausting memory.
const MAX_STR_BYTES: usize = 1 << 32;

/// The string argument of a string method that takes no others.
fn str_arg<'h>(heap: &'h Heap, name: &str, args: &[Value]) -> Result<&'h str, String> {
    match args {
        [Value::Str(s)] => Ok(heap.str(*s)),
        _ => Err(bad_args(&format!("str::{name}"), args)),
    }
}

/// The arguments of a string method that takes one other string.
fn two_strs<'h>(heap: &'h Heap, name: &str, args: &[Value]) -> Result<(&'h str, &'h str), String> {
    match args {
        [Value::Str(a), Value::Str(b)] => Ok((heap.str(*a), heap.str(*b))),
        _ => Err(bad_args(&format!("str::{name}"), args)),
    }
}

/// The characters of `s` from position `start` up to `end`, counting
/// characters, or the error if that isn't a range within it.
fn char_slice(s: &str, start: i64, end: i64) -> Result<&str, String> {
    let out_of_range = || {
        format!(
            "slice {start}..{end} is out of range for a string of length {}",
            s.chars().count()
        )
    };
    if start < 0 || end < start {
        return Err(out_of_range());
    }
    // The byte offset of each character position, and of the end.
    let mut offsets = s
        .char_indices()
        .map(|(i, _)| i)
        .chain(std::iter::once(s.len()));
    let from = offsets.nth(start as usize).ok_or_else(out_of_range)?;
    let to = if end == start {
        from
    } else {
        offsets
            .nth((end - start - 1) as usize)
            .ok_or_else(out_of_range)?
    };
    Ok(&s[from..to])
}

/// `base` to the power `exp`, for `int::pow`.
fn pow(base: i64, exp: i64) -> Result<i64, String> {
    if exp < 0 {
        return Err(format!(
            "'pow' needs an exponent of 0 or more, but it was {exp}"
        ));
    }
    let overflow = || "integer overflow in 'pow'".to_string();
    match u32::try_from(exp) {
        Ok(exp) => base.checked_pow(exp).ok_or_else(overflow),
        // Only these bases have powers this high that fit.
        Err(_) => match base {
            0 | 1 => Ok(base),
            -1 => Ok(if exp % 2 == 0 { 1 } else { -1 }),
            _ => Err(overflow()),
        },
    }
}

/// A float method that maps a float to a float.
fn float_op(name: &str, args: &[Value], f: fn(f64) -> f64) -> Result<Value, String> {
    match args {
        [Value::Float(x)] => Ok(Value::Float(f(*x))),
        _ => Err(bad_args(&format!("float::{name}"), args)),
    }
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

/// The message of the error that stops a program whose standard output was
/// closed by its reader (as `aja f.aja | head` does once `head` has its
/// lines). It is never shown: the program stops quietly instead.
pub const OUTPUT_CLOSED: &str = "standard output was closed";

#[cfg(test)]
thread_local! {
    /// While `Some`, what `print` and `println` write goes here instead of
    /// to standard output, for tests to read.
    pub(super) static CAPTURED: RefCell<Option<String>> = const { RefCell::new(None) };
}

/// Write what `print` or `println` prints. Failing stops the program, since
/// `print` doesn't return a `Result`; `std::io` is for handling it.
fn print_out(text: String) -> Result<(), String> {
    #[cfg(test)]
    if CAPTURED.with(|c| {
        c.borrow_mut()
            .as_mut()
            .map(|out| out.push_str(&text))
            .is_some()
    }) {
        return Ok(());
    }
    match io::stdout().write_all(text.as_bytes()) {
        Ok(()) => Ok(()),
        Err(e) if e.kind() == io::ErrorKind::BrokenPipe => Err(OUTPUT_CLOSED.to_string()),
        Err(e) => Err(format!("could not write to standard output: {e}")),
    }
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
pub(super) fn str_value(heap: &mut Heap, s: String) -> Value {
    Value::Str(heap.alloc_str(s.into_boxed_str()))
}

/// The functions the VM implements, by name: the built-ins, and the
/// functions the prelude and standard library declare `native`, by the
/// name they are compiled under (`List::len`, `std::native::write`).
static NATIVES: &[(&str, NativeFn)] = &[
    ("print", |heap, args| {
        print_out(join(heap, args))?;
        Ok(Value::Void)
    }),
    ("println", |heap, args| {
        print_out(join(heap, args) + "\n")?;
        Ok(Value::Void)
    }),
    ("clock", |_, _| {
        let ms = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis();
        Ok(Value::Int(ms as i64))
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
    // ---- Channels (`std::channel`) -------------------------------------------
    // A channel's receiving end is the same channel as its sending end.
    ("std::channel::receiver", |_, args| Ok(args[0].clone())),
    // ---- Time (`std::time`) ------------------------------------------------
    ("std::native::monotonic_nanos", |_, _| {
        // Counted from the first call, so an `int` holds it for centuries.
        static START: OnceLock<Instant> = OnceLock::new();
        let start = *START.get_or_init(Instant::now);
        Ok(Value::Int(start.elapsed().as_nanos() as i64))
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
    // ---- Strings (`impl str` in the prelude) ------------------------------
    ("str::len", |heap, args| {
        let s = str_arg(heap, "len", args)?;
        Ok(Value::Int(s.chars().count() as i64))
    }),
    ("str::byte_len", |heap, args| {
        let s = str_arg(heap, "byte_len", args)?;
        Ok(Value::Int(s.len() as i64))
    }),
    ("str::contains", |heap, args| {
        let (s, part) = two_strs(heap, "contains", args)?;
        Ok(Value::Bool(s.contains(part)))
    }),
    ("str::starts_with", |heap, args| {
        let (s, part) = two_strs(heap, "starts_with", args)?;
        Ok(Value::Bool(s.starts_with(part)))
    }),
    ("str::ends_with", |heap, args| {
        let (s, part) = two_strs(heap, "ends_with", args)?;
        Ok(Value::Bool(s.ends_with(part)))
    }),
    ("str::index_of", |heap, args| {
        let (s, part) = two_strs(heap, "index_of", args)?;
        let i = s
            .find(part)
            .map_or(-1, |byte| s[..byte].chars().count() as i64);
        Ok(Value::Int(i))
    }),
    ("str::slice", |heap, args| {
        let [Value::Str(s), Value::Int(start), Value::Int(end)] = args else {
            return Err(bad_args("str::slice", args));
        };
        let s = heap.str(*s);
        let part = char_slice(s, *start, *end)?.to_string();
        Ok(str_value(heap, part))
    }),
    ("str::chars", |heap, args| {
        let s = str_arg(heap, "chars", args)?;
        let chars = s.chars().map(String::from).collect();
        Ok(str_list(heap, chars))
    }),
    ("str::split", |heap, args| {
        let (s, sep) = two_strs(heap, "split", args)?;
        let parts = if sep.is_empty() {
            s.chars().map(String::from).collect()
        } else {
            s.split(sep).map(str::to_string).collect()
        };
        Ok(str_list(heap, parts))
    }),
    ("str::lines", |heap, args| {
        let s = str_arg(heap, "lines", args)?;
        let lines = lines(s);
        Ok(str_list(heap, lines))
    }),
    ("str::words", |heap, args| {
        let s = str_arg(heap, "words", args)?;
        let words = s.split_whitespace().map(str::to_string).collect();
        Ok(str_list(heap, words))
    }),
    ("str::trim", |heap, args| {
        let s = str_arg(heap, "trim", args)?.trim().to_string();
        Ok(str_value(heap, s))
    }),
    ("str::trim_start", |heap, args| {
        let s = str_arg(heap, "trim_start", args)?.trim_start().to_string();
        Ok(str_value(heap, s))
    }),
    ("str::trim_end", |heap, args| {
        let s = str_arg(heap, "trim_end", args)?.trim_end().to_string();
        Ok(str_value(heap, s))
    }),
    ("str::to_upper", |heap, args| {
        let s = str_arg(heap, "to_upper", args)?.to_uppercase();
        Ok(str_value(heap, s))
    }),
    ("str::to_lower", |heap, args| {
        let s = str_arg(heap, "to_lower", args)?.to_lowercase();
        Ok(str_value(heap, s))
    }),
    ("str::replace", |heap, args| {
        let [Value::Str(s), Value::Str(from), Value::Str(to)] = args else {
            return Err(bad_args("str::replace", args));
        };
        let (s, from, to) = (heap.str(*s), heap.str(*from), heap.str(*to));
        let out = if from.is_empty() {
            s.to_string()
        } else {
            s.replace(from, to)
        };
        Ok(str_value(heap, out))
    }),
    ("str::repeat", |heap, args| {
        let [Value::Str(s), Value::Int(count)] = args else {
            return Err(bad_args("str::repeat", args));
        };
        let s = heap.str(*s);
        let count = (*count).max(0) as usize;
        if s.len().checked_mul(count).is_none_or(|n| n > MAX_STR_BYTES) {
            return Err(format!(
                "'repeat' would make a string of {count} copies of {} bytes, which is too long",
                s.len()
            ));
        }
        let out = s.repeat(count);
        Ok(str_value(heap, out))
    }),
    ("str::join", |heap, args| {
        let [Value::Str(sep), Value::List(parts)] = args else {
            return Err(bad_args("str::join", args));
        };
        let mut out = String::new();
        for (i, part) in heap.list(*parts).iter().enumerate() {
            if i > 0 {
                out.push_str(heap.str(*sep));
            }
            let Value::Str(part) = part else {
                return Err(bad_args("str::join", args));
            };
            out.push_str(heap.str(*part));
        }
        Ok(str_value(heap, out))
    }),
    ("str::is_int", |heap, args| {
        let s = str_arg(heap, "is_int", args)?;
        Ok(Value::Bool(s.parse::<i64>().is_ok()))
    }),
    ("str::to_int_unchecked", |heap, args| {
        let s = str_arg(heap, "to_int_unchecked", args)?;
        s.parse::<i64>()
            .map(Value::Int)
            .map_err(|_| format!("{s:?} isn't an int"))
    }),
    ("str::is_float", |heap, args| {
        let s = str_arg(heap, "is_float", args)?;
        Ok(Value::Bool(s.parse::<f64>().is_ok()))
    }),
    ("str::to_float_unchecked", |heap, args| {
        let s = str_arg(heap, "to_float_unchecked", args)?;
        s.parse::<f64>()
            .map(Value::Float)
            .map_err(|_| format!("{s:?} isn't a float"))
    }),
    // ---- Numbers (`impl int` and `impl float` in the prelude) -------------
    ("int::to_str", |heap, args| {
        let [Value::Int(n)] = args else {
            return Err(bad_args("int::to_str", args));
        };
        Ok(str_value(heap, n.to_string()))
    }),
    ("int::to_float", |_, args| {
        let [Value::Int(n)] = args else {
            return Err(bad_args("int::to_float", args));
        };
        Ok(Value::Float(*n as f64))
    }),
    ("int::pow", |_, args| {
        let [Value::Int(base), Value::Int(exp)] = args else {
            return Err(bad_args("int::pow", args));
        };
        pow(*base, *exp).map(Value::Int)
    }),
    ("float::to_str", |heap, args| {
        let [Value::Float(x)] = args else {
            return Err(bad_args("float::to_str", args));
        };
        Ok(str_value(heap, format!("{x:?}")))
    }),
    ("float::to_int", |_, args| {
        let [Value::Float(x)] = args else {
            return Err(bad_args("float::to_int", args));
        };
        // Every float in this range truncates to an int; 2^63 itself doesn't.
        if x.is_nan() || *x < -9_223_372_036_854_775_808.0 || *x >= 9_223_372_036_854_775_808.0 {
            return Err(format!("{x:?} can't be converted to an int"));
        }
        Ok(Value::Int(*x as i64))
    }),
    ("float::floor", |_, args| {
        float_op("floor", args, f64::floor)
    }),
    ("float::ceil", |_, args| float_op("ceil", args, f64::ceil)),
    ("float::round", |_, args| {
        float_op("round", args, f64::round)
    }),
    ("float::sqrt", |_, args| float_op("sqrt", args, f64::sqrt)),
    ("float::is_nan", |_, args| {
        let [Value::Float(x)] = args else {
            return Err(bad_args("float::is_nan", args));
        };
        Ok(Value::Bool(x.is_nan()))
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
    ("List::sort", |heap, args| {
        let [Value::List(l)] = args else {
            return Err(bad_args("List::sort", args));
        };
        // The values stay reachable from `l`, in the caller's registers,
        // while the new list is allocated.
        let mut values: Vec<Value> = heap.list(*l).iter().cloned().collect();
        values.sort_by(|a, b| heap.compare(a, b)); // stable
        let sorted: super::list::List = values.into_iter().collect();
        Ok(Value::List(heap.alloc_list(sorted)))
    }),
    // ---- Maps and sets (`std::collections`) --------------------------------
    //
    // Keys are hashed and compared with `Heap::hash` and `Heap::equal`, so
    // equal keys are one key whatever their history.
    ("std::collections::Map::new", |heap, _| {
        Ok(Value::Map(heap.alloc_map(Hamt::default())))
    }),
    ("std::collections::Map::len", |heap, args| {
        let [Value::Map(m)] = args else {
            return Err(bad_args("Map::len", args));
        };
        Ok(Value::Int(heap.hamt(*m).len() as i64))
    }),
    ("std::collections::Map::contains_key", |heap, args| {
        let [Value::Map(m), key] = args else {
            return Err(bad_args("Map::contains_key", args));
        };
        Ok(Value::Bool(lookup(heap, *m, key).is_some()))
    }),
    ("std::collections::Map::value", |heap, args| {
        let [Value::Map(m), key] = args else {
            return Err(bad_args("Map::value", args));
        };
        lookup(heap, *m, key).ok_or_else(|| format!("no key {} in the map", heap.repr(key)))
    }),
    ("std::collections::Map::insert", |heap, args| {
        let [Value::Map(m), key, value] = args else {
            return Err(bad_args("Map::insert", args));
        };
        let map = with_key(heap, *m, key, Some(value));
        Ok(Value::Map(heap.alloc_map(map)))
    }),
    ("std::collections::Map::remove", |heap, args| {
        let [Value::Map(m), key] = args else {
            return Err(bad_args("Map::remove", args));
        };
        let map = with_key(heap, *m, key, None);
        Ok(Value::Map(heap.alloc_map(map)))
    }),
    ("std::collections::Map::keys", |heap, args| {
        let [Value::Map(m)] = args else {
            return Err(bad_args("Map::keys", args));
        };
        let mut keys = super::list::List::default();
        heap.hamt(*m).for_each(|k, _| keys.push(k.clone()));
        Ok(Value::List(heap.alloc_list(keys)))
    }),
    ("std::collections::Map::values", |heap, args| {
        let [Value::Map(m)] = args else {
            return Err(bad_args("Map::values", args));
        };
        let mut values = super::list::List::default();
        heap.hamt(*m).for_each(|_, v| values.push(v.clone()));
        Ok(Value::List(heap.alloc_list(values)))
    }),
    ("std::collections::Set::new", |heap, _| {
        Ok(Value::Set(heap.alloc_set(Hamt::default())))
    }),
    ("std::collections::Set::len", |heap, args| {
        let [Value::Set(s)] = args else {
            return Err(bad_args("Set::len", args));
        };
        Ok(Value::Int(heap.hamt(*s).len() as i64))
    }),
    ("std::collections::Set::contains", |heap, args| {
        let [Value::Set(s), value] = args else {
            return Err(bad_args("Set::contains", args));
        };
        Ok(Value::Bool(lookup(heap, *s, value).is_some()))
    }),
    ("std::collections::Set::insert", |heap, args| {
        let [Value::Set(s), value] = args else {
            return Err(bad_args("Set::insert", args));
        };
        let set = with_key(heap, *s, value, Some(&Value::Void));
        Ok(Value::Set(heap.alloc_set(set)))
    }),
    ("std::collections::Set::remove", |heap, args| {
        let [Value::Set(s), value] = args else {
            return Err(bad_args("Set::remove", args));
        };
        let set = with_key(heap, *s, value, None);
        Ok(Value::Set(heap.alloc_set(set)))
    }),
    ("std::collections::Set::to_list", |heap, args| {
        let [Value::Set(s)] = args else {
            return Err(bad_args("Set::to_list", args));
        };
        let mut values = super::list::List::default();
        heap.hamt(*s).for_each(|k, _| values.push(k.clone()));
        Ok(Value::List(heap.alloc_list(values)))
    }),
    // Sorted maps and sets keep their keys in the order `Heap::compare`
    // gives. A sorted set is a sorted map whose values are `void`, and its
    // natives are the map's under another name.
    ("std::collections::SortedMap::new", |heap, _| {
        Ok(Value::SortedMap(heap.alloc_sorted_map(Tree::default())))
    }),
    ("std::collections::SortedMap::len", |heap, args| {
        sorted_len(heap, args)
    }),
    ("std::collections::SortedMap::contains_key", |heap, args| {
        let (t, key) = sorted_key(heap, args)?;
        Ok(Value::Bool(sorted_lookup(heap, t, key).is_some()))
    }),
    ("std::collections::SortedMap::value", |heap, args| {
        let (t, key) = sorted_key(heap, args)?;
        sorted_lookup(heap, t, key)
            .ok_or_else(|| format!("no key {} in the sorted map", heap.repr(key)))
    }),
    ("std::collections::SortedMap::insert", |heap, args| {
        let [Value::SortedMap(t), key, value] = args else {
            return Err(bad_args("SortedMap::insert", args));
        };
        let tree = sorted_with_key(heap, *t, key, Some(value));
        Ok(Value::SortedMap(heap.alloc_sorted_map(tree)))
    }),
    ("std::collections::SortedMap::remove", |heap, args| {
        let (t, key) = sorted_key(heap, args)?;
        let tree = sorted_with_key(heap, t, key, None);
        Ok(Value::SortedMap(heap.alloc_sorted_map(tree)))
    }),
    ("std::collections::SortedMap::keys", |heap, args| {
        sorted_part(heap, args, true)
    }),
    ("std::collections::SortedMap::values", |heap, args| {
        sorted_part(heap, args, false)
    }),
    ("std::collections::SortedMap::first_key", |heap, args| {
        sorted_end(heap, args, true)
    }),
    ("std::collections::SortedMap::last_key", |heap, args| {
        sorted_end(heap, args, false)
    }),
    ("std::collections::SortedMap::range_keys", |heap, args| {
        sorted_range(heap, args, true)
    }),
    ("std::collections::SortedMap::range_values", |heap, args| {
        sorted_range(heap, args, false)
    }),
    ("std::collections::SortedSet::new", |heap, _| {
        Ok(Value::SortedSet(heap.alloc_sorted_set(Tree::default())))
    }),
    ("std::collections::SortedSet::len", |heap, args| {
        sorted_len(heap, args)
    }),
    ("std::collections::SortedSet::contains", |heap, args| {
        let (t, key) = sorted_key(heap, args)?;
        Ok(Value::Bool(sorted_lookup(heap, t, key).is_some()))
    }),
    ("std::collections::SortedSet::insert", |heap, args| {
        let [Value::SortedSet(t), value] = args else {
            return Err(bad_args("SortedSet::insert", args));
        };
        let tree = sorted_with_key(heap, *t, value, Some(&Value::Void));
        Ok(Value::SortedSet(heap.alloc_sorted_set(tree)))
    }),
    ("std::collections::SortedSet::remove", |heap, args| {
        let (t, value) = sorted_key(heap, args)?;
        let tree = sorted_with_key(heap, t, value, None);
        Ok(Value::SortedSet(heap.alloc_sorted_set(tree)))
    }),
    ("std::collections::SortedSet::to_list", |heap, args| {
        sorted_part(heap, args, true)
    }),
    ("std::collections::SortedSet::first_value", |heap, args| {
        sorted_end(heap, args, true)
    }),
    ("std::collections::SortedSet::last_value", |heap, args| {
        sorted_end(heap, args, false)
    }),
    ("std::collections::SortedSet::range", |heap, args| {
        sorted_range(heap, args, true)
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
/// The value for `key` in the map or set `m`.
fn lookup(heap: &Heap, m: Gc, key: &Value) -> Option<Value> {
    heap.hamt(m)
        .get(heap.hash(key), key, &|a, b| heap.equal(a, b))
        .cloned()
}

/// The map or set `m` with `key` given `value`, or removed if `value` is
/// `None`. The keys and values stay reachable from `m` and the arguments, in
/// the caller's registers, while the result is allocated.
fn with_key(heap: &Heap, m: Gc, key: &Value, value: Option<&Value>) -> Hamt {
    let mut map = heap.hamt(m).clone();
    let hash = heap.hash(key);
    let eq = |a: &Value, b: &Value| heap.equal(a, b);
    match value {
        Some(v) => map.insert(hash, key.clone(), v.clone(), &eq),
        None => {
            map.remove(hash, key, &eq);
        }
    }
    map
}

/// The tree that the first of `args`, a sorted map or set, holds.
fn sorted_tree(args: &[Value]) -> Option<Gc> {
    match args.first() {
        Some(Value::SortedMap(t) | Value::SortedSet(t)) => Some(*t),
        _ => None,
    }
}

fn sorted_len(heap: &mut Heap, args: &[Value]) -> Result<Value, String> {
    let t = sorted_tree(args).ok_or_else(|| bad_args("len", args))?;
    Ok(Value::Int(heap.tree(t).len() as i64))
}

/// A sorted map or set, and a key, from `args`.
fn sorted_key<'a>(_: &Heap, args: &'a [Value]) -> Result<(Gc, &'a Value), String> {
    match (sorted_tree(args), args) {
        (Some(t), [_, key]) => Ok((t, key)),
        _ => Err(bad_args("a sorted map or set's method", args)),
    }
}

fn sorted_lookup(heap: &Heap, t: Gc, key: &Value) -> Option<Value> {
    heap.tree(t).get(key, &|a, b| heap.compare(a, b)).cloned()
}

/// The sorted map or set `t` with `key` given `value`, or removed if `value`
/// is `None` (see `with_key`).
fn sorted_with_key(heap: &Heap, t: Gc, key: &Value, value: Option<&Value>) -> Tree {
    let mut tree = heap.tree(t).clone();
    let cmp = |a: &Value, b: &Value| heap.compare(a, b);
    match value {
        Some(v) => tree.insert(key.clone(), v.clone(), &cmp),
        None => {
            tree.remove(key, &cmp);
        }
    }
    tree
}

/// The keys (or values) of a sorted map or set, in order.
fn sorted_part(heap: &mut Heap, args: &[Value], keys: bool) -> Result<Value, String> {
    let t = sorted_tree(args).ok_or_else(|| bad_args("keys", args))?;
    let mut out = super::list::List::default();
    heap.tree(t)
        .for_each(|k, v| out.push(if keys { k } else { v }.clone()));
    Ok(Value::List(heap.alloc_list(out)))
}

/// The smallest (or largest) key of a sorted map or set, which isn't empty.
fn sorted_end(heap: &mut Heap, args: &[Value], first: bool) -> Result<Value, String> {
    let t = sorted_tree(args).ok_or_else(|| bad_args("first", args))?;
    let tree = heap.tree(t);
    let end = if first { tree.first() } else { tree.last() };
    end.map(|(k, _)| k.clone())
        .ok_or_else(|| "the sorted map or set is empty".to_string())
}

/// The keys (or values) of a sorted map or set whose keys are at least
/// `args[1]` and less than `args[2]`, in order.
fn sorted_range(heap: &mut Heap, args: &[Value], keys: bool) -> Result<Value, String> {
    let (Some(t), [_, lo, hi]) = (sorted_tree(args), args) else {
        return Err(bad_args("range", args));
    };
    let mut out = super::list::List::default();
    heap.tree(t).for_range(
        Some(lo),
        Some(hi),
        &|a, b| heap.compare(a, b),
        &mut |k, v| out.push(if keys { k } else { v }.clone()),
    );
    Ok(Value::List(heap.alloc_list(out)))
}

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
                implementation(b.name).is_some()
                    || super::super::scheduler::Intrinsic::named(b.name).is_some(),
                "built-in '{}' has no implementation",
                b.name
            );
        }
    }
}
