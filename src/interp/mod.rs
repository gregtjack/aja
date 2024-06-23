use std::{collections::HashMap, fmt::Display};

use crate::ast::{Definition, Expression, Id, Literal, Op1, Op2, Program, Statement};
use color_eyre::{
    eyre::{bail, eyre, Ok},
    Result,
};

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Bool(bool),
    String(String),
    Function(Vec<Id>, Statement),
    Void,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{i}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Function(_, _) => write!(f, "<function>"),
            Value::Void => write!(f, "()"),
        }
    }
}

type Answer = Result<Value>;

/// Run environment
#[derive(Debug, Clone)]
pub struct REnvironment {
    pub values: HashMap<Id, Value>,
    pub enclosing: Option<Box<REnvironment>>,
}

impl REnvironment {
    /// Create a new blank run environment
    pub fn new() -> Self {
        REnvironment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    /// Create a new run environment from an enclosing scope
    pub fn from_enclosing(env: REnvironment) -> Self {
        REnvironment {
            values: HashMap::new(),
            enclosing: Some(Box::new(env)),
        }
    }

    pub fn get(&self, id: Id) -> Answer {
        if let Some(ans) = self.values.get(&id) {
            return Ok(ans.clone());
        }

        if let Some(env) = &self.enclosing {
            return env.get(id);
        }

        bail!("Undefined variable: '{id}'")
    }

    pub fn define(&mut self, id: Id, value: Value) -> Option<Value> {
        self.values.insert(id, value)
    }

    pub fn assign(&mut self, id: Id, value: Value) -> Result<()> {
        if self.values.contains_key(&id) {
            self.values.insert(id, value);
            return Ok(());
        }

        bail!("Undefined variable: '{id}'")
    }
}

const BUILTINS: [&str; 1] = ["println"];

pub struct Interpreter {
    pub program: Program,
    pub env: REnvironment,
}

impl Interpreter {
    pub fn new(prog: Program) -> Self {
        let mut env = REnvironment::new();

        for def in &prog.ds {
            match def {
                Definition::Function { name, args, body } => {
                    // TODO: in the Some case (function already declared), handle more gracefully
                    assert!(env
                        .define(name.clone(), Value::Function(args.clone(), body.clone()))
                        .is_none());
                }
                _ => unimplemented!(),
            }
        }

        Interpreter { program: prog, env }
    }

    pub fn run(mut self) -> Answer {
        for binding in &self.env.values {
            if binding.0 == "main" {
                match &binding.1 {
                    Value::Function(_, e) => return self.interp_stmt(e.clone()),
                    _ => unreachable!(),
                }
            }
        }

        bail!("No entrypoint to program. Write a main() function.")
    }

    fn interp_stmt(&mut self, stmt: Statement) -> Answer {
        match stmt {
            Statement::Block(stmts) => {
                let prev_env = self.env.clone();
                let mut tail_val: Value = Value::Void;
                if let Some(tail) = stmts.last() {
                    self.env = REnvironment::from_enclosing(prev_env.clone());
                    for i in 0..(stmts.len() - 1) {
                        self.interp_stmt(stmts[i].clone());
                    }
                    tail_val = self.interp_stmt(tail.clone())?;
                }
                self.env = prev_env;
                Ok(tail_val)
            }
            Statement::Expr(e) => self.interp_expr(e),
            Statement::Let { var, value } => self.interp_let(var, value),
            Statement::If(e1, s1, s2) => self.interp_if(e1, *s1, *s2),
            Statement::Assign { lhs, rhs } => todo!(),
            Statement::Return(maybe_e) => self.interp_return(maybe_e),
        }
    }

    fn interp_expr(&mut self, expr: Expression) -> Answer {
        match expr {
            Expression::Literal(p) => self.interp_literal(p),
            Expression::Unary(op, e1) => self.interp_unary(op, *e1),
            Expression::BinOp(e1, op, e2) => self.interp_binop(*e1, op, *e2),
            Expression::Grouping(e) => self.interp_expr(*e),
            Expression::If(e1, e2, e3) => self.interp_if(*e1, *e2, Some(*e3)),
            Expression::Var(id) => self.env.get(id),
            Expression::Call(name, args) => self.interp_call(name, args),
            Expression::Empty => Ok(Value::Void),
            _ => bail!("Unknown expression"),
        }
    }

    fn interp_call(&mut self, id: Id, args: Vec<Expression>) -> Answer {
        let args: Vec<Value> = args
            .iter()
            .map(|e| self.interp_expr(e.clone()).unwrap())
            .collect();

        if BUILTINS.contains(&id.as_str()) {
            return run_builtin(id, args);
        }

        let var = self.env.get(id)?;

        match var {
            Value::Function(params, body) => {
                assert_eq!(args.len(), params.len(), "Number of arguments passed to function does not equal the number of parameters.");
                params.iter().zip(args.iter()).for_each(|(name, value)| {
                    self.env.define(name.clone(), value.clone());
                });

                self.interp_stmt(body)
            }
            _ => bail!("Can't call a non-function"),
        }
    }

    fn interp_let(&mut self, id: Id, e: Expression) -> Answer {
        let r = self.interp_expr(e)?;
        self.env.define(id, r);
        Ok(Value::Void)
    }

    fn interp_return(&mut self, e: Option<Expression>) -> Answer {
        Ok(Value::Void)
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
            _ => bail!("Expected a boolean expression in if condition"),
        }
    }

    fn interp_binop(&mut self, e1: Expression, op: Op2, e2: Expression) -> Answer {
        let r1 = self.interp_expr(e1)?;
        let r2 = self.interp_expr(e2)?;

        match op {
            Op2::Addition => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Int(i1 + i2)),
                _ => bail!("cannot add non-integers"),
            },
            Op2::Subtraction => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Int(i1 - i2)),
                _ => bail!("cannot subtract non-integers"),
            },
            Op2::Multiplication => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Int(i1 * i2)),
                _ => bail!("cannot multiply non-integers"),
            },
            Op2::Division => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Int(i1 / i2)),
                _ => bail!("cannot divide non-integers"),
            },
            // Equality
            Op2::Equal => match (r1, r2) {
                (Value::Bool(b1), Value::Bool(b2)) => Ok(Value::Bool(b1 == b2)),
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 == i2)),
                (Value::String(s1), Value::String(s2)) => Ok(Value::Bool(s1.eq(&s2))),
                _ => bail!("TypeError: equality operator expects operands of the same type"),
            },
            Op2::NotEqual => match (r1, r2) {
                (Value::Bool(b1), Value::Bool(b2)) => Ok(Value::Bool(b1 != b2)),
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 != i2)),
                (Value::String(s1), Value::String(s2)) => Ok(Value::Bool(s1.ne(&s2))),
                _ => bail!("TypeError: inequality operator expects booleans"),
            },
            // Comparison
            Op2::GreaterThan => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 > i2)),
                _ => bail!("TypeError: > operator expects integers"),
            },
            Op2::GreaterEqual => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 >= i2)),
                _ => bail!("TypeError: >= operator expects integers"),
            },
            Op2::LessThan => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 < i2)),
                _ => bail!("TypeError: < operator expects integers"),
            },
            Op2::LessEqual => match (r1, r2) {
                (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 <= i2)),
                _ => bail!("TypeError: <= operator expects integers"),
            },
            _ => bail!("invalid binary operator"),
        }
    }

    fn interp_unary(&mut self, op: Op1, expr: Expression) -> Answer {
        let e = self.interp_expr(expr)?;
        match op {
            Op1::Not => match e {
                Value::Bool(b) => Ok(Value::Bool(!b)),
                _ => bail!("expected boolean"),
            },
            Op1::Negate => match e {
                Value::Int(i) => Ok(Value::Int(-i)),
                _ => bail!("expected integer"),
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
