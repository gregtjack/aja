use std::fmt;

use super::{heap::Gc, native::NativeFn};

/// A value in a register, global, or cell.
///
/// Values carry a tag so that the VM stays memory-safe and so `print` can
/// format them, but the VM never uses the tag to decide what an instruction
/// does: the instruction's type says that.
#[derive(Clone)]
pub enum Value {
    Void,
    Int(i64),
    Float(f64),
    Bool(bool),
    /// A string on the heap.
    Str(Gc),
    /// A closure on the heap.
    Fn(Gc),
    /// A struct or enum value on the heap.
    Data(Gc),
    /// A list on the heap.
    List(Gc),
    /// A function implemented by the VM: a built-in, or one declared
    /// `native` in the prelude or standard library. Its name, and its
    /// implementation.
    Native(&'static str, NativeFn),
    /// A local variable captured by a closure, as a cell on the heap. Cells
    /// only appear in the registers of the function that declared the
    /// variable; programs never see them as values.
    Cell(Gc),
}

impl Value {
    /// The heap object this value refers to, if any.
    pub fn handle(&self) -> Option<Gc> {
        match self {
            Value::Str(gc)
            | Value::Fn(gc)
            | Value::Cell(gc)
            | Value::Data(gc)
            | Value::List(gc) => Some(*gc),
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
            Value::Str(gc) => write!(f, "Str({gc})"),
            Value::Fn(gc) => write!(f, "Fn({gc})"),
            Value::Data(gc) => write!(f, "Data({gc})"),
            Value::List(gc) => write!(f, "List({gc})"),
            Value::Native(name, _) => write!(f, "Native({name})"),
            Value::Cell(gc) => write!(f, "Cell({gc})"),
        }
    }
}
