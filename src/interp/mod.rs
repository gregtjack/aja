use std::{cell::RefCell, collections::HashMap, error::Error, fmt::Display, rc::Rc};

use crate::ast::{
    types::Type, Definition, Expression, Id, Literal, Op1, Op2, Program, Statement, Variable,
};
use color_eyre::Result;
use ecow::EcoString;
use env::REnvironment;

mod env;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Bool(bool),
    String(String),
    Callable(Callable),
    Void,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{i}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Callable(c) => match c {
                Callable::Function(ps, _, rt) => {
                    let fvs: Vec<String> = ps.iter().map(|v| v.t.to_string()).collect();
                    write!(f, "fn({}) -> {}", fvs.join(", "), rt)
                }
                Callable::Builtin(_) => write!(f, "<native fn>"),
            },
            Value::Void => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Callable {
    Function(Vec<Variable>, Statement, Type),
    Builtin(Id),
}

#[derive(Debug, Clone)]
pub enum RuntimeError {
    Adhoc(String),
    UndefinedVariable { var: EcoString },
    TypeError { msg: String },
    Return(Value),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::UndefinedVariable { var } => write!(f, "Undefined variable: {var}"),
            RuntimeError::TypeError { msg } => write!(f, "TypeError: {msg}"),
            RuntimeError::Return(_) => write!(f, "return statement"),
            RuntimeError::Adhoc(msg) => {
                write!(f, "{msg}")
            }
        }
    }
}

impl Error for RuntimeError {}

type Answer = Result<Value, RuntimeError>;

pub struct Interpreter {
    pub program: Program,
    pub env: Rc<RefCell<REnvironment>>,
}

impl Interpreter {
    pub fn new(prog: Program) -> Self {
        Interpreter {
            program: prog,
            env: Rc::new(RefCell::new(REnvironment::new())),
        }
    }

    pub fn run(mut self) -> Answer {
        self.defines()?;
        let bs = self.env.borrow().values.clone();
        for binding in &bs {
            if binding.0 == "main" {
                match &binding.1 {
                    Value::Callable(c) => match c {
                        Callable::Function(_, s, t) => {
                            let rt = t.clone();
                            let res = match self.interp_stmt(s.clone()) {
                                Ok(v) => Ok(v),
                                Err(err) => match err {
                                    RuntimeError::Return(v) => Ok(v),
                                    _ => Err(err),
                                },
                            }?;
                            Self::type_check(&res, rt)?;
                            return Ok(res);
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
        }

        return Err(RuntimeError::Adhoc(
            "no entrypoint to program. Write a main function.".to_string(),
        ));
    }

    fn defines(&mut self) -> Result<(), RuntimeError> {
        self.define_builtin("println".into())?;

        for def in &self.program.ds {
            match def {
                Definition::Function {
                    name,
                    params,
                    body,
                    rtype,
                } => {
                    self.env.borrow_mut().define(
                        name.clone(),
                        Value::Callable(Callable::Function(
                            params.into_iter().map(|a| a.clone()).collect(),
                            body.clone(),
                            rtype.clone(),
                        )),
                    )?;
                }
                _ => unimplemented!(),
            }
        }

        Ok(())
    }

    fn define_builtin(&mut self, name: Id) -> Result<(), RuntimeError> {
        self.env
            .borrow_mut()
            .define(name.clone(), Value::Callable(Callable::Builtin(name)))
    }

    fn interp_stmt(&mut self, stmt: Statement) -> Answer {
        match stmt {
            Statement::Block(stmts, tail) => {
                let prev = self.env.clone();
                let mut tail_val: Value = Value::Void;
                self.env = Rc::new(RefCell::new(REnvironment::from_enclosing(prev.clone())));
                for stmt in stmts {
                    self.interp_stmt(stmt.clone())?;
                }
                if let Some(tail) = tail {
                    tail_val = self.interp_expr(tail)?;
                }
                self.env = prev;
                Ok(tail_val)
            }
            Statement::Expr(e) => self.interp_expr(e),
            Statement::Let { var, value } => self.interp_let(var, value),
            Statement::If(e1, s1) => self.interp_if(e1, *s1, None),
            Statement::While(e, s) => self.interp_while(e, *s),
            Statement::Return(maybe_e) => self.interp_return(maybe_e),
        }
    }

    fn interp_expr(&mut self, expr: Expression) -> Answer {
        let ans = match expr {
            Expression::Literal(p) => self.interp_literal(p),
            Expression::Unary(op, e1) => self.interp_unary(op, *e1),
            Expression::BinOp(e1, op, e2) => self.interp_binop(*e1, op, *e2),
            Expression::Grouping(e) => self.interp_expr(*e),
            Expression::If(e1, e2, e3) => self.interp_if(*e1, *e2, Some(*e3)),
            Expression::Var(id) => self.env.borrow().get(id),
            Expression::Call(callee, args) => self.interp_call(*callee, args),
            Expression::Assign(lhs, rhs) => {
                let value = self.interp_expr(*rhs)?;
                self.env.borrow_mut().assign(lhs, value)
            }
            Expression::Empty => Ok(Value::Void),
        };
        ans
    }

    fn interp_call(&mut self, callee: Expression, args: Vec<Expression>) -> Answer {
        let callee = self.interp_expr(callee)?;

        let args: Vec<Value> = args
            .iter()
            .map(|e| self.interp_expr(e.clone()).unwrap())
            .collect();

        match callee {
            Value::Callable(c) => match c {
                Callable::Function(params, body, rtype) => {
                    assert_eq!(args.len(), params.len(), "Number of arguments passed to function does not equal the number of parameters.");
                    params.iter().zip(args.iter()).for_each(|(var, value)| {
                        Self::type_check(value, var.t.clone()).expect("Typecheck failed");
                        let _ = self
                            .env
                            .borrow_mut()
                            .define(var.name.clone(), value.clone());
                    });

                    let ret = match self.interp_stmt(body) {
                        Ok(v) => Ok(v),
                        Err(err) => match err {
                            RuntimeError::Return(v) => Ok(v),
                            _ => Err(err),
                        },
                    }?;
                    Self::type_check(&ret, rtype)?;
                    Ok(ret)
                }
                Callable::Builtin(name) => run_builtin(name, args),
            },
            _ => {
                return Err(RuntimeError::TypeError {
                    msg: "Can't call non-callable".to_string(),
                })
            }
        }
    }

    fn interp_let(&mut self, var: Variable, e: Expression) -> Answer {
        let r = self.interp_expr(e)?;
        Self::type_check(&r, var.t)?;
        self.env.borrow_mut().define(var.name, r)?;
        Ok(Value::Void)
    }

    fn interp_return(&mut self, e: Option<Expression>) -> Answer {
        if let Some(expr) = e {
            let value = self.interp_expr(expr)?;
            Err(RuntimeError::Return(value))
        } else {
            Err(RuntimeError::Return(Value::Void))
        }
    }

    fn interp_if(&mut self, e1: Expression, s1: Statement, s2: Option<Statement>) -> Answer {
        let cond = self.interp_expr(e1)?;
        match cond {
            Value::Bool(true) => self.interp_stmt(s1),
            Value::Bool(false) => {
                if let Some(s) = s2 {
                    self.interp_stmt(s)
                } else {
                    Ok(Value::Void)
                }
            }
            _ => {
                return Err(RuntimeError::TypeError {
                    msg: "Expected a boolean expression in if condition".to_string(),
                })
            }
        }
    }

    fn interp_while(&mut self, e: Expression, s: Statement) -> Answer {
        while match self.interp_expr(e.clone())? {
            Value::Bool(true) => true,
            Value::Bool(false) => false,
            _ => {
                return Err(RuntimeError::TypeError {
                    msg: "Expected a boolean expression in while condition".to_string(),
                })
            }
        } {
            self.interp_stmt(s.clone())?;
        }
        Ok(Value::Void)
    }

    fn interp_binop(&mut self, e1: Expression, op: Op2, e2: Expression) -> Answer {
        let r1 = self.interp_expr(e1)?;
        let r2 = self.interp_expr(e2)?;

        match op {
            Op2::Addition => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Int(i1 + i2)),
                _ => Err(RuntimeError::TypeError {
                    msg: "cannot add non-integers".to_string(),
                }),
            },
            Op2::Subtraction => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Int(i1 - i2)),
                _ => Err(RuntimeError::TypeError {
                    msg: "cannot subtract non-integers".to_string(),
                }),
            },
            Op2::Multiplication => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Int(i1 * i2)),
                _ => Err(RuntimeError::TypeError {
                    msg: "cannot multiply non-integers".to_string(),
                }),
            },
            Op2::Division => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Int(i1 / i2)),
                _ => Err(RuntimeError::TypeError {
                    msg: "cannot divide non-integers".to_string(),
                }),
            },
            // Equality
            Op2::Equal => match (r1, r2) {
                (Value::Bool(b1), Value::Bool(b2)) => Ok(Value::Bool(b1 == b2)),
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 == i2)),
                (Value::String(s1), Value::String(s2)) => Ok(Value::Bool(s1.eq(&s2))),
                _ => Err(RuntimeError::TypeError {
                    msg: format!("equality operator expects operands of the same (supported) type"),
                }),
            },
            Op2::NotEqual => match (r1, r2) {
                (Value::Bool(b1), Value::Bool(b2)) => Ok(Value::Bool(b1 != b2)),
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 != i2)),
                (Value::String(s1), Value::String(s2)) => Ok(Value::Bool(s1.ne(&s2))),
                _ => Err(RuntimeError::TypeError {
                    msg: format!(
                        "inequality operator expects operands of the same (supported) type"
                    ),
                }),
            },
            // Comparison
            Op2::GreaterThan => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 > i2)),
                _ => Err(RuntimeError::TypeError {
                    msg: format!("> operator expects operands of the same (supported) type"),
                }),
            },
            Op2::GreaterEqual => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 >= i2)),
                _ => Err(RuntimeError::TypeError {
                    msg: format!(">= operator expects operands of the same (supported) type"),
                }),
            },
            Op2::LessThan => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 < i2)),
                _ => Err(RuntimeError::TypeError {
                    msg: format!("< operator expects operands of the same (supported) type"),
                }),
            },
            Op2::LessEqual => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 <= i2)),
                _ => Err(RuntimeError::TypeError {
                    msg: format!("<= operator expects operands of the same (supported) type"),
                }),
            },
            _ => Err(RuntimeError::Adhoc(format!(
                "greater-than operator expects operands of the same (supported) type"
            ))),
        }
    }

    fn interp_unary(&mut self, op: Op1, expr: Expression) -> Answer {
        let e = self.interp_expr(expr)?;
        match op {
            Op1::Not => match e {
                Value::Bool(b) => Ok(Value::Bool(!b)),
                _ => Err(RuntimeError::TypeError {
                    msg: format!("expected bool, found {}", e),
                }),
            },
            Op1::Negate => match e {
                Value::Int(i) => Ok(Value::Int(-i)),
                _ => Err(RuntimeError::TypeError {
                    msg: format!("expected int, found {}", e),
                }),
            },
        }
    }

    fn interp_literal(&self, p: Literal) -> Answer {
        match p {
            Literal::Int(i) => Ok(Value::Int(i)),
            Literal::String(s) => Ok(Value::String(s.to_string())),
            Literal::Bool(b) => Ok(Value::Bool(b)),
        }
    }

    fn type_check(v: &Value, t: Type) -> Result<(), RuntimeError> {
        if matches!(t, Type::Any) {
            return Ok(());
        }

        let other: Type = v.clone().into();
        if other == t {
            Ok(())
        } else {
            Err(RuntimeError::TypeError {
                msg: format!("cannot cast {other} to {t}"),
            })
        }
    }
}

fn run_builtin(name: Id, args: Vec<Value>) -> Answer {
    match name.as_str() {
        "println" => {
            let sargs: Vec<String> = args.iter().map(|a| a.to_string()).collect();
            println!("{}", sargs.join(" "));
            Ok(Value::Void)
        }
        _ => unreachable!(),
    }
}
