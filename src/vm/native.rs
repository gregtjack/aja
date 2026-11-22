//! The VM's implementations of the built-in functions declared in
//! `crate::builtins`.

use std::{
    thread,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use super::{heap::Heap, value::Value};

/// Runs a built-in. The type checker has already checked the arguments
/// against the built-in's signature. The heap is there to read objects the
/// arguments refer to. An error message stops the program.
pub type NativeFn = fn(&Heap, &[Value]) -> Result<Value, String>;

/// The implementation of the built-in called `name`.
pub fn implementation(name: &str) -> Option<NativeFn> {
    let f: NativeFn = match name {
        "print" => |heap, args| {
            print!("{}", join(heap, args));
            Ok(Value::Void)
        },
        "println" => |heap, args| {
            println!("{}", join(heap, args));
            Ok(Value::Void)
        },
        "clock" => |_, _| {
            let ms = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis();
            Ok(Value::Int(ms as i32))
        },
        "sleep" => |_, args| {
            let [Value::Int(ms)] = args else { return Err(bad_args("sleep", args)) };
            thread::sleep(Duration::from_millis((*ms).max(0) as u64));
            Ok(Value::Void)
        },
        "assert" => |_, args| match args {
            [Value::Bool(true)] => Ok(Value::Void),
            [Value::Bool(false)] => Err("assertion failed".to_string()),
            _ => Err(bad_args("assert", args)),
        },
        _ => return None,
    };
    Some(f)
}

fn join(heap: &Heap, args: &[Value]) -> String {
    args.iter().map(|a| heap.display(a)).collect::<Vec<_>>().join(" ")
}

/// Arguments the type checker should have rejected.
fn bad_args(name: &str, args: &[Value]) -> String {
    let types: Vec<String> = args.iter().map(|a| format!("{a:?}")).collect();
    format!("{name}() was called with unexpected arguments ({})", types.join(", "))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builtins::BUILTINS;

    #[test]
    fn every_builtin_is_implemented() {
        for b in BUILTINS {
            assert!(implementation(b.name).is_some(), "built-in '{}' has no implementation", b.name);
        }
    }
}
