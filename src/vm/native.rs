//! The VM's implementations of the built-in functions declared in
//! `crate::builtins`.

use std::{
    cell::RefCell,
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
/// A native function may allocate only what it returns, as the last thing
/// it does: allocating directly on the heap never collects, so the
/// arguments stay valid while it runs.
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

thread_local! {
    /// The status of the last intrinsic that can fail, and its message.
    static STATUS: RefCell<(i64, String)> = const { RefCell::new((OK, String::new())) };
}

fn set_status(code: i64, message: String) {
    STATUS.with(|s| *s.borrow_mut() = (code, message));
}

/// Record how an I/O operation went, giving its value if it succeeded.
fn record<T: Default>(result: io::Result<T>) -> T {
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
                _ => OTHER,
            };
            set_status(code, e.to_string());
            T::default()
        }
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
