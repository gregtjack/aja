use std::{
    fmt::Display,
    sync::{Arc, Mutex},
};

use crate::ast::{types::Type, Statement, Variable};

use super::{env::RuntimeEnv, native::NativeFn};
use crate::builtins::Builtin;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Fn(Function),
    /// A built-in: its signature and the interpreter's implementation.
    NativeFn(&'static Builtin, NativeFn),
    Void,
}

impl Value {
    /// The type's name as shown in error messages, e.g. `int` or `fn`.
    pub fn type_name(&self) -> String {
        match self {
            Value::Fn(_) | Value::NativeFn(..) => "fn".to_string(),
            other => other.to_type().to_string(),
        }
    }

    pub fn to_type(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::Bool(_) => Type::Bool,
            Value::String(_) => Type::String,
            // Function values are only checked to be functions at runtime;
            // their full types are checked statically.
            Value::Fn(_) | Value::NativeFn(..) => Type::Fn {
                param_types: vec![],
                ret_type: Box::new(Type::Void),
            },
            Value::Void => Type::Void,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<Variable>,
    pub body: Statement,
    pub ret_type: Option<Type>,
    pub env: Arc<Mutex<RuntimeEnv>>,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{i:?}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Fn(c) => match c {
                Function {
                    params, ret_type, ..
                } => {
                    let vars: Vec<String> = params.iter().map(|v| v.to_string()).collect();
                    match ret_type {
                        Some(t) => write!(f, "<fn ({}) -> {}>", vars.join(", "), t),
                        None => write!(f, "<fn ({})>", vars.join(", ")),
                    }
                }
            },
            Value::NativeFn(builtin, _) => write!(f, "<native fn: {}>", builtin.name),
            Value::Void => write!(f, "()"),
            Value::Float(n) => write!(f, "{n:?}"),
        }
    }
}
