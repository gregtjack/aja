use std::any;

use anyhow::anyhow;

use crate::{ast::{Expr, Literal}, lexer::token::TokenType};

#[derive(Debug)]
pub enum InterpValue {
    Int(i32),
    Bool(bool),
    String(String),
}

type InterpResult<T> = Result<T, anyhow::Error>;

pub fn interp_expr(expr: Expr) -> InterpResult<InterpValue> {
    match expr {
        Expr::Literal(p) => interp_literal(p),
        Expr::Unary(op, e1) => interp_unary(op, *e1),
        Expr::BinOp(e1, op, e2) => interp_binop(*e1, op, *e2),
        Expr::Grouping(e) => interp_expr(*e),
        Expr::If(e1, e2, e3) => interp_if(*e1, *e2, *e3),
        _ => Err(anyhow!("Unknown expression"))
    }
}

fn interp_if(e1: Expr, e2: Expr, e3: Expr) -> InterpResult<InterpValue> {
    let cond = interp_expr(e1)?;
    match cond {
        InterpValue::Bool(true) => interp_expr(e2),
        InterpValue::Bool(false) => interp_expr(e3),
        _ => Err(anyhow!("Expected boolean expression in if condition")),
    }
}

fn interp_binop(e1: Expr, op: TokenType, e2: Expr) -> InterpResult<InterpValue> {
    let r1 = interp_expr(e1)?;
    let r2 = interp_expr(e2)?;

    match op {
        TokenType::Plus => match (r1, r2) {
            (InterpValue::Int(i1), InterpValue::Int(i2)) => Ok(InterpValue::Int(i1 + i2)),
            _ => Err(anyhow!("cannot add non-integers"))
        }
        TokenType::Minus => match (r1, r2) {
            (InterpValue::Int(i1), InterpValue::Int(i2)) => Ok(InterpValue::Int(i1 - i2)),
            _ => Err(anyhow!("cannot subtract non-integers"))
        }
        TokenType::Mult => match (r1, r2) {
            (InterpValue::Int(i1), InterpValue::Int(i2)) => Ok(InterpValue::Int(i1 * i2)),
            _ => Err(anyhow!("cannot multiply non-integers"))
        }
        TokenType::Div => match (r1, r2) {
            (InterpValue::Int(i1), InterpValue::Int(i2)) => Ok(InterpValue::Int(i1 / i2)),
            _ => Err(anyhow!("cannot divide non-integers"))
        }
        // Equality
        TokenType::EqualEqual => match (r1, r2) {
            (InterpValue::Bool(b1), InterpValue::Bool(b2)) => Ok(InterpValue::Bool(b1 == b2)),
            (InterpValue::Int(i1), InterpValue::Int(i2)) => Ok(InterpValue::Bool(i1 == i2)),
            (InterpValue::String(s1), InterpValue::String(s2)) => Ok(InterpValue::Bool(s1.eq(&s2))),
            _ => Err(anyhow!("TypeError: equality operator expects operands of the same type"))
        }
        TokenType::BangEqual => match (r1, r2) {
            (InterpValue::Bool(b1), InterpValue::Bool(b2)) => Ok(InterpValue::Bool(b1 != b2)),
            (InterpValue::Int(i1), InterpValue::Int(i2)) => Ok(InterpValue::Bool(i1 != i2)),
            (InterpValue::String(s1), InterpValue::String(s2)) => Ok(InterpValue::Bool(s1.ne(&s2))),
            _ => Err(anyhow!("TypeError: inequality operator expects booleans"))
        }
        // Comparison
        TokenType::GreaterThan => match (r1, r2) {
            (InterpValue::Int(i1), InterpValue::Int(i2)) => Ok(InterpValue::Bool(i1 > i2)),
            _ => Err(anyhow!("TypeError: > operator expects integers"))
        }
        TokenType::LessThan => match (r1, r2) {
            (InterpValue::Int(i1), InterpValue::Int(i2)) => Ok(InterpValue::Bool(i1 < i2)),
            _ => Err(anyhow!("TypeError: < operator expects integers"))
        }
        TokenType::GreaterEqual => match (r1, r2) {
            (InterpValue::Int(i1), InterpValue::Int(i2)) => Ok(InterpValue::Bool(i1 >= i2)),
            _ => Err(anyhow!("TypeError: >= operator expects integers"))
        } 
        TokenType::LessEqual => match (r1, r2) {
            (InterpValue::Int(i1), InterpValue::Int(i2)) => Ok(InterpValue::Bool(i1 <= i2)),
            _ => Err(anyhow!("TypeError: <= operator expects integers"))
        } 
        _ => Err(anyhow!("invalid binary operator"))
    }
}

fn interp_unary(op: TokenType, expr: Expr) -> InterpResult<InterpValue> {
    let e = interp_expr(expr)?;
    match op {
        TokenType::Bang => match e {
            InterpValue::Bool(b) => Ok(InterpValue::Bool(!b)),
            _ => Err(anyhow!("expected boolean"))
        }
        TokenType::Minus => match e {
            InterpValue::Int(i) => Ok(InterpValue::Int(-i)), 
            _ => Err(anyhow!("expected integer"))
        }
        _ => Err(anyhow!("invalid unary operator"))
    }
}

fn interp_literal(p: Literal) -> InterpResult<InterpValue> {
    match p {
        Literal::Int(i) => Ok(InterpValue::Int(i)),
        Literal::String(s) => Ok(InterpValue::String(s.to_string())),
        Literal::Bool(b) => Ok(InterpValue::Bool(b)),
    }
}
