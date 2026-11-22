use std::{thread::sleep, time::Duration};

use crate::{
    ast::types::Type,
    interp::{value::Value, RuntimeError},
};

#[derive(Debug, Clone)]
pub struct BuiltinInfo {
    pub name: &'static str,
    pub description: &'static str,
    pub param_types: Vec<Type>,
    pub return_type: Type,
    /// Takes any number of arguments of any type (`param_types` is ignored).
    pub variadic: bool,
}

#[derive(Debug, Clone)]
pub enum Builtin {
    Print,
    PrintLn,
    Clock,
    Sleep,
    Assert,
}

impl Builtin {
    pub fn info(&self) -> BuiltinInfo {
        match self {
            Builtin::Print => BuiltinInfo {
                name: "print",
                description: "Prints values to stdout",
                param_types: vec![],
                return_type: Type::Void,
                variadic: true,
            },
            Builtin::PrintLn => BuiltinInfo {
                name: "println",
                description: "Prints values to stdout, appending a new line at the end.",
                param_types: vec![],
                return_type: Type::Void,
                variadic: true,
            },
            Builtin::Clock => BuiltinInfo {
                name: "clock",
                description: "Returns current time in milliseconds",
                param_types: vec![],
                return_type: Type::Int,
                variadic: false,
            },
            Builtin::Sleep => BuiltinInfo {
                name: "sleep",
                description: "Sleeps for specified milliseconds",
                param_types: vec![Type::Int],
                return_type: Type::Void,
                variadic: false,
            },
            Builtin::Assert => BuiltinInfo {
                name: "assert",
                description: "Stops execution if condition is false",
                param_types: vec![Type::Bool],
                return_type: Type::Void,
                variadic: false,
            },
        }
    }

    pub fn all() -> Vec<Builtin> {
        vec![
            Builtin::Print,
            Builtin::PrintLn,
            Builtin::Clock,
            Builtin::Sleep,
            Builtin::Assert,
        ]
    }

    pub fn execute(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        match self {
            Builtin::Print => {
                print!(
                    "{}",
                    args.into_iter()
                        .map(|a| a.to_string())
                        .collect::<Vec<String>>()
                        .join(" ")
                );
                Ok(Value::Void)
            }
            Builtin::PrintLn => {
                println!(
                    "{}",
                    args.into_iter()
                        .map(|a| a.to_string())
                        .collect::<Vec<String>>()
                        .join(" ")
                );
                Ok(Value::Void)
            }
            Builtin::Clock => {
                use std::time::{SystemTime, UNIX_EPOCH};
                let now = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_millis() as i32;
                Ok(Value::Int(now))
            }
            Builtin::Sleep => {
                if let Some(Value::Int(i)) = args.first() {
                    sleep(Duration::from_millis(*i as u64));
                    Ok(Value::Void)
                } else {
                    Err(RuntimeError::type_error(format!(
                        "sleep() expects an int (milliseconds), found {}",
                        args.first()
                            .map_or("nothing".to_string(), |a| a.type_name())
                    )))
                }
            }
            Builtin::Assert => {
                if args.is_empty() {
                    return Err(RuntimeError::adhoc(
                        "assert() takes 1 argument but 0 were given".to_string(),
                    ));
                }

                if let Value::Bool(condition) = &args[0] {
                    if !condition {
                        return Err(RuntimeError::adhoc("assertion failed".to_string()));
                    }
                    Ok(Value::Void)
                } else {
                    Err(RuntimeError::type_error(format!(
                        "assert() expects a bool, found {}",
                        args[0].type_name()
                    )))
                }
            }
        }
    }
}
