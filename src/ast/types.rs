use std::fmt::Display;

use color_eyre::eyre::bail;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Any,
    Int,
    Float,
    Bool,
    String,
    Fn {
        param_types: Vec<Type>,
        ret_type: Box<Type>,
    },
    List(Box<Type>),
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
            Type::Fn {
                param_types,
                ret_type,
            } => {
                write!(
                    f,
                    "fn<{}> -> {}",
                    param_types
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<String>>()
                        .join(", "),
                    ret_type
                )
            }
            Type::List(t) => write!(f, "List<{}>", t),
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
