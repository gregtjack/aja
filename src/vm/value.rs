use std::fmt;

use super::{heap::Gc, native::NativeFn, scheduler::Intrinsic};

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
    /// A `std::collections::Map` on the heap.
    Map(Gc),
    /// A `std::collections::Set` on the heap.
    Set(Gc),
    /// A `std::collections::SortedMap` on the heap.
    SortedMap(Gc),
    /// A `std::collections::SortedSet` on the heap.
    SortedSet(Gc),
    /// A function implemented by the VM: a built-in, or one declared
    /// `native` in the prelude or standard library. Its name, and its
    /// implementation.
    Native(&'static str, NativeFn),
    /// A function the VM's runtime implements itself, because it starts,
    /// waits for, or finishes tasks.
    Runtime(Intrinsic),
    /// A `std::task::Scope` on the heap.
    Scope(Gc),
    /// A `std::task::Task` on the heap.
    Task(Gc),
    /// A `std::channel::Sender` or `Receiver`: the same channel on the
    /// heap, used from either end.
    Channel(Gc),
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
            | Value::List(gc)
            | Value::Map(gc)
            | Value::Set(gc)
            | Value::SortedMap(gc)
            | Value::SortedSet(gc)
            | Value::Scope(gc)
            | Value::Task(gc)
            | Value::Channel(gc) => Some(*gc),
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
            Value::Map(gc) => write!(f, "Map({gc})"),
            Value::Set(gc) => write!(f, "Set({gc})"),
            Value::SortedMap(gc) => write!(f, "SortedMap({gc})"),
            Value::SortedSet(gc) => write!(f, "SortedSet({gc})"),
            Value::Native(name, _) => write!(f, "Native({name})"),
            Value::Runtime(i) => write!(f, "Runtime({})", i.name()),
            Value::Scope(gc) => write!(f, "Scope({gc})"),
            Value::Task(gc) => write!(f, "Task({gc})"),
            Value::Channel(gc) => write!(f, "Channel({gc})"),
            Value::Cell(gc) => write!(f, "Cell({gc})"),
        }
    }
}
