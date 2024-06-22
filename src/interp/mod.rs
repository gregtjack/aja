use color_eyre::{eyre::{bail, eyre}, Result};
use crate::{
    ast::{Definition, Expr, Id, Literal, Op1, Op2, Program},
    token::TokenType,
};

mod translate;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Bool(bool),
    String(String),
    Function(Vec<Id>, Expr),
    Void,
}

/// Variable binding
#[derive(Debug, Clone)]
pub struct Binding(Id, Value);

/// Run environment.
#[derive(Debug, Clone)]
pub struct REnv {
    pub bindings: Vec<Binding>,
}

type Answer = Result<Value>;

const BUILTINS: [&str; 1] = ["println"];

pub struct Interpreter {
    pub program: Program,
    pub globals: Vec<Binding>,
}

impl Interpreter {
    pub fn new(prog: Program) -> Self {
        let mut env = vec![];

        for def in &prog.ds {
            match def {
                Definition::Function { name, args, body } => {
                    env.push(Binding(
                        name.clone(),
                        Value::Function(args.clone(), expr.clone()),
                    ))
                },
                Definition::Struct() => unimplemented!(),
            }
        }

        Interpreter {
            program: prog,
            globals: env,
        }
    }

    pub fn run(self) -> Answer {
        for binding in &self.globals {
            if binding.0 == "main" {
                match &binding.1 {
                    Value::Function(_, e) => {
                        return self.interp_expr(
                            e.clone(),
                            REnv {
                                bindings: self.globals.clone(),
                            },
                        )
                    }
                    _ => unreachable!(),
                }
            }
        }

        bail!(
            "No entrypoint to program. Write a main() function."
        )
    }

    fn interp_expr(&self, expr: Expr, env: REnv) -> Answer {
        match expr {
            Expr::Literal(p) => self.interp_literal(p),
            Expr::Unary(op, e1) => self.interp_unary(op, *e1, env),
            Expr::BinOp(e1, op, e2) => self.interp_binop(*e1, op, *e2, env),
            Expr::Grouping(e) => self.interp_expr(*e, env),
            Expr::If(e1, e2, e3) => self.interp_if(*e1, *e2, *e3, env),
            Expr::Let(id, e1, e2) => self.interp_let(id, *e1, *e2, env),
            Expr::Var(id) => lookup(id, env),
            Expr::Call(name, args) => self.interp_call(name, args, env),
            Expr::Empty => Ok(Value::Void),
            _ => bail!("Unknown expression"),
        }
    }

    fn interp_call(&self, id: Id, args: Vec<Expr>, mut env: REnv) -> Answer {
        let args: Vec<Value> = args
            .iter()
            .map(|e| self.interp_expr(e.clone(), env.clone()).unwrap())
            .collect();

        if BUILTINS.contains(&id.as_str()) {
            return run_builtin(id, args);
        }

        let var = lookup(id, env).unwrap();

        match var {
            Value::Function(params, body) => {
                assert_eq!(args.len(), params.len(), "Number of arguments passed to function does not equal the number of parameters.");
                let mut bindings: Vec<Binding> = params
                    .iter()
                    .zip(args.iter())
                    .map(|(name, value)| Binding(name.clone(), value.clone()))
                    .collect();

                env = REnv {
                    bindings: self.globals.clone(),
                };
                env.bindings.append(&mut bindings);
                self.interp_expr(body, env)
            }
            _ => bail!("Can't call a non-function"),
        }
    }

    fn interp_let(&self, id: Id, e1: Expr, e2: Expr, mut env: REnv) -> Answer {
        let r1 = self.interp_expr(e1, env.clone())?;
        let binding = Binding(id, r1);
        env.bindings.push(binding);
        self.interp_expr(e2, env)
    }

    fn interp_if(&self, e1: Expr, e2: Expr, e3: Expr, env: REnv) -> Answer {
        let cond = self.interp_expr(e1, env.clone())?;
        match cond {
            Value::Bool(true) => self.interp_expr(e2, env.clone()),
            Value::Bool(false) => self.interp_expr(e3, env),
            _ => bail!("Expected a boolean expression in if condition"),
        }
    }

    fn interp_binop(&self, e1: Expr, op: Op2, e2: Expr, env: REnv) -> Answer {
        let r1 = self.interp_expr(e1, env.clone())?;
        let r2 = self.interp_expr(e2, env)?;

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
                _ => bail!(
                    "TypeError: equality operator expects operands of the same type"
                ),
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

    fn interp_unary(&self, op: Op1, expr: Expr, env: REnv) -> Answer {
        let e = self.interp_expr(expr, env)?;
        match op {
            Op1::Not => match e {
                Value::Bool(b) => Ok(Value::Bool(!b)),
                _ => bail!("expected boolean"),
            },
            Op1::Negate => match e {
                Value::Int(i) => Ok(Value::Int(-i)),
                _ => bail!("expected integer"),
            },
            _ => bail!("invalid unary operator"),
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

fn lookup(id: Id, env: REnv) -> Answer {
    match env.bindings.iter().find(|&b| b.0 == id) {
        Some(v) => Ok(v.1.clone()),
        None => bail!("Undeclared variable: '{}'", id),
    }
}

fn run_builtin(name: Id, args: Vec<Value>) -> Answer {
    match name.as_str() {
        "println" => {
            let sargs: Vec<String> = args
                .iter()
                .map(|a| match a {
                    Value::Int(i) => format!("{i}"),
                    Value::Bool(b) => format!("{b}"),
                    Value::String(s) => s.to_string(),
                    Value::Function(_, _) => format!("<function>"),
                    Value::Void => format!("()"),
                })
                .collect();

            println!("{}", sargs.join(" "));

            Ok(Value::Void)
        }
        _ => unreachable!(),
    }
}
