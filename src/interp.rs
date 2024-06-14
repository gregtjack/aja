use std::{any, clone};

use anyhow::anyhow;

use crate::{
    ast::{Expr, Id, Literal, Op, Program},
    token::TokenType,
};

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Bool(bool),
    String(String),
    Void,
}

#[derive(Debug, Clone)]
pub struct Binding(Id, Value);

type InterpResult<T> = Result<T, anyhow::Error>;
type REnv = Vec<Binding>;

pub fn interp(prog: Program) -> InterpResult<Value> {
    // for def in prog.ds {
    //     if def.f == "main" {

    //     }
    // }
    todo!()
}

fn interp_expr(expr: Expr, env: REnv) -> InterpResult<Value> {
    match expr {
        Expr::Literal(p) => interp_literal(p),
        Expr::Unary(op, e1) => interp_unary(op, *e1, env),
        Expr::BinOp(e1, op, e2) => interp_binop(*e1, op, *e2, env),
        Expr::Grouping(e) => interp_expr(*e, env),
        Expr::If(e1, e2, e3) => interp_if(*e1, *e2, *e3, env),
        Expr::Let(id, e1, e2) => interp_let(id, *e1, *e2, env),
        Expr::Var(id) => lookup(env, id),
        _ => Err(anyhow!("Unknown expression")),
    }
}

fn interp_let(id: Id, e1: Expr, e2: Expr, mut env: REnv) -> InterpResult<Value> {
    let r1 = interp_expr(e1, env.clone())?;
    let binding = Binding(id, r1);
    env.push(binding);
    interp_expr(e2, env)
}

fn interp_if(e1: Expr, e2: Expr, e3: Expr, env: REnv) -> InterpResult<Value> {
    let cond = interp_expr(e1, env.clone())?;
    match cond {
        Value::Bool(true) => interp_expr(e2, env.clone()),
        Value::Bool(false) => interp_expr(e3, env),
        _ => Err(anyhow!("Expected a boolean expression in if condition")),
    }
}

fn interp_binop(e1: Expr, op: TokenType, e2: Expr, env: REnv) -> InterpResult<Value> {
    let r1 = interp_expr(e1, env.clone())?;
    let r2 = interp_expr(e2, env)?;

    match op {
        TokenType::Plus => match (r1, r2) {
            (Value::Int(i1), Value::Int(i2)) => Ok(Value::Int(i1 + i2)),
            _ => Err(anyhow!("cannot add non-integers")),
        },
        TokenType::Minus => match (r1, r2) {
            (Value::Int(i1), Value::Int(i2)) => Ok(Value::Int(i1 - i2)),
            _ => Err(anyhow!("cannot subtract non-integers")),
        },
        TokenType::Mult => match (r1, r2) {
            (Value::Int(i1), Value::Int(i2)) => Ok(Value::Int(i1 * i2)),
            _ => Err(anyhow!("cannot multiply non-integers")),
        },
        TokenType::Div => match (r1, r2) {
            (Value::Int(i1), Value::Int(i2)) => Ok(Value::Int(i1 / i2)),
            _ => Err(anyhow!("cannot divide non-integers")),
        },
        // Equality
        TokenType::EqualEqual => match (r1, r2) {
            (Value::Bool(b1), Value::Bool(b2)) => Ok(Value::Bool(b1 == b2)),
            (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 == i2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::Bool(s1.eq(&s2))),
            _ => Err(anyhow!(
                "TypeError: equality operator expects operands of the same type"
            )),
        },
        TokenType::BangEqual => match (r1, r2) {
            (Value::Bool(b1), Value::Bool(b2)) => Ok(Value::Bool(b1 != b2)),
            (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 != i2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::Bool(s1.ne(&s2))),
            _ => Err(anyhow!("TypeError: inequality operator expects booleans")),
        },
        // Comparison
        TokenType::GreaterThan => match (r1, r2) {
            (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 > i2)),
            _ => Err(anyhow!("TypeError: > operator expects integers")),
        },
        TokenType::LessThan => match (r1, r2) {
            (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 < i2)),
            _ => Err(anyhow!("TypeError: < operator expects integers")),
        },
        TokenType::GreaterEqual => match (r1, r2) {
            (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 >= i2)),
            _ => Err(anyhow!("TypeError: >= operator expects integers")),
        },
        TokenType::LessEqual => match (r1, r2) {
            (Value::Int(i1), Value::Int(i2)) => Ok(Value::Bool(i1 <= i2)),
            _ => Err(anyhow!("TypeError: <= operator expects integers")),
        },
        _ => Err(anyhow!("invalid binary operator")),
    }
}

fn interp_unary(op: TokenType, expr: Expr, env: REnv) -> InterpResult<Value> {
    let e = interp_expr(expr, env)?;
    match op {
        TokenType::Bang => match e {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            _ => Err(anyhow!("expected boolean")),
        },
        TokenType::Minus => match e {
            Value::Int(i) => Ok(Value::Int(-i)),
            _ => Err(anyhow!("expected integer")),
        },
        _ => Err(anyhow!("invalid unary operator")),
    }
}

fn interp_literal(p: Literal) -> InterpResult<Value> {
    match p {
        Literal::Int(i) => Ok(Value::Int(i)),
        Literal::String(s) => Ok(Value::String(s.to_string())),
        Literal::Bool(b) => Ok(Value::Bool(b)),
    }
}

fn lookup(env: REnv, id: Id) -> InterpResult<Value> {
    match env.iter().find(|&b| b.0 == id) {
        Some(v) => Ok(v.1.clone()),
        None => Err(anyhow!("Undeclared variable: '{}'", id)),
    }
}
