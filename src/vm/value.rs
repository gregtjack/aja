use std::{cell::RefCell, fmt, rc::Rc};

use crate::builtins::Builtin;

use super::{bytecode::Proto, native::NativeFn};

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
    Fn(Rc<Closure>),
    /// A built-in: its signature and the VM's implementation.
    Native(&'static Builtin, NativeFn),
    /// A local variable captured by a closure. Cells only appear in the
    /// registers of the function that declared the variable; programs never
    /// see them as values.
    Cell(Rc<RefCell<Value>>),
}

/// A function value: compiled code plus the variables it captured.
pub struct Closure {
    pub proto: Rc<Proto>,
    pub upvalues: Box<[Rc<RefCell<Value>>]>,
}

impl Value {
    /// How the REPL shows a value: like `print`, but with strings quoted.
    pub fn repr(&self) -> String {
        match self {
            Value::Str(s) => format!("{:?}", &**s),
            other => other.to_string(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Void => write!(f, "()"),
            Value::Int(i) => write!(f, "{i}"),
            Value::Float(n) => write!(f, "{n:?}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Str(s) => write!(f, "{s}"),
            Value::Fn(c) => write!(f, "<fn {}>", c.proto.signature),
            Value::Native(builtin, _) => write!(f, "<native fn: {}>", builtin.name),
            Value::Cell(c) => write!(f, "{}", c.borrow()),
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
            Value::Fn(_) | Value::Native(..) => write!(f, "Fn({self})"),
            Value::Cell(c) => write!(f, "Cell({:?})", c.borrow()),
        }
    }
}
