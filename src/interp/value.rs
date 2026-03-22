use std::{
    fmt::Display,
    sync::{Arc, Mutex},
};

use crate::ast::{types::Type, Id, Statement, Variable};

use super::{env::RuntimeEnv, Builtin};

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Fn(Function),
    NativeFn(Builtin),
    Void,
}

impl Value {
    pub fn to_type(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::Bool(_) => Type::Bool,
            Value::String(_) => Type::String,
            Value::Fn(c) => match c {
                Function {
                    params, ret_type, ..
                } => Type::Fn {
                    param_types: params.iter().map(|p| p.t.clone()).collect(),
                    ret_type: Box::new(ret_type.clone()),
                },
            },
            Value::NativeFn(builtin) => builtin.info().return_type,
            Value::Void => Type::Void,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<Variable>,
    pub body: Statement,
    pub ret_type: Type,
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
                    let vars: Vec<String> = params.iter().map(|v| v.t.to_string()).collect();
                    write!(f, "<fn ({}) -> {}>", vars.join(", "), ret_type)
                }
            },
            Value::NativeFn(builtin) => write!(f, "<native fn: {}>", builtin.info().name),
            Value::Void => write!(f, "()"),
            Value::Float(n) => write!(f, "{n:?}"),
        }
    }
}
