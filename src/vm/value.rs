use std::{fmt, rc::Rc};

use crate::builtins::Builtin;

use super::{heap::Gc, native::NativeFn};

/// A value in a register, global, or cell.
///
/// Values carry a tag so that the VM stays memory-safe and so `print` can
/// format them, but the VM never uses the tag to decide what an instruction
/// does: the instruction's type says that.
#[derive(Clone)]
pub enum Value {
    Void,
    Int(i32),
    Float(f32),
    Bool(bool),
    Str(Rc<str>),
    /// A closure on the heap.
    Fn(Gc),
    /// A built-in: its signature and the VM's implementation.
    Native(&'static Builtin, NativeFn),
    /// A local variable captured by a closure, as a cell on the heap. Cells
    /// only appear in the registers of the function that declared the
    /// variable; programs never see them as values.
    Cell(Gc),
}

impl Value {
    /// The heap object this value refers to, if any.
    pub fn handle(&self) -> Option<Gc> {
        match self {
            Value::Fn(gc) | Value::Cell(gc) => Some(*gc),
            _ => None,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Void => write!(f, "Void"),
            Value::Int(i) => write!(f, "Int({i})"),
            Value::Float(n) => write!(f, "Float({n:?})"),
            Value::Bool(b) => write!(f, "Bool({b})"),
            Value::Str(s) => write!(f, "Str({:?})", &**s),
            Value::Fn(gc) => write!(f, "Fn({gc})"),
            Value::Native(b, _) => write!(f, "Native({})", b.name),
            Value::Cell(gc) => write!(f, "Cell({gc})"),
        }
    }
}
