use std::fmt::Display;

use color_eyre::eyre::bail;

use super::{Id, Span};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Fn {
        param_types: Vec<Type>,
        ret_type: Box<Type>,
    },
    Void,
    /// A struct or enum, or a type parameter, by name, with its type
    /// arguments (`Option<int>`) and where it is written.
    Named(Id, Vec<Type>, Span),
    /// A type whose error has already been reported (by the module
    /// resolver). It fits anywhere.
    Error,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "str"),
            Type::Fn {
                param_types,
                ret_type,
            } => {
                let params: Vec<String> = param_types.iter().map(|t| t.to_string()).collect();
                write!(f, "({}) -> {}", params.join(", "), ret_type)
            }
            Type::Void => write!(f, "void"),
            Type::Error => write!(f, "_"),
            Type::Named(name, args, _) if args.is_empty() => write!(f, "{name}"),
            Type::Named(name, args, _) => {
                let args: Vec<String> = args.iter().map(|t| t.to_string()).collect();
                write!(f, "{name}<{}>", args.join(", "))
            }
        }
    }
}

impl TryFrom<String> for Type {
    type Error = color_eyre::eyre::Error;

    fn try_from(value: String) -> Result<Self, color_eyre::eyre::Error> {
        match value.as_str() {
            "int" => Ok(Self::Int),
            "float" => Ok(Self::Float),
            "bool" => Ok(Self::Bool),
            "str" => Ok(Self::String),
            "void" => Ok(Self::Void),
            _ => bail!("'{value}' is not a valid type"),
        }
    }
}
