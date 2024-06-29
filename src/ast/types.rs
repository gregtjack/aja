use std::fmt::Display;

use color_eyre::eyre::bail;

use crate::interp::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Any,
    Int,
    Float,
    Bool,
    String,
    Fn { arity: usize, rtype: Box<Type> },
    Vec(Box<Type>),
    Void,
}

impl Default for Type {
    fn default() -> Self {
        Self::Any
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Any => write!(f, "any"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "str"),
            Type::Fn { arity, rtype } => write!(f, "Fn<{}>:{}", rtype, arity),
            Type::Vec(t) => write!(f, "vec[{}]", t),
            Type::Void => write!(f, "void"),
        }
    }
}

impl TryFrom<String> for Type {
    type Error = color_eyre::eyre::Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "int" => Ok(Self::Int),
            "float" => Ok(Self::Float),
            "bool" => Ok(Self::Bool),
            "str" => Ok(Self::String),
            "void" => Ok(Self::Void),
            "any" => Ok(Self::Any),
            _ => bail!("'{value}' is not a valid type"),
        }
    }
}

impl From<Value> for Type {
    fn from(value: Value) -> Self {
        match value {
            Value::Int(_) => Self::Int,
            Value::Bool(_) => Self::Bool,
            Value::String(_) => Self::String,
            Value::Function(ps, _, t) => Self::Fn {
                arity: ps.len(),
                rtype: Box::new(t),
            },
            Value::Void => Self::Void,
        }
    }
}
