use ecow::EcoString;

use crate::token::TokenType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub line: u32,
    pub col: u32,
    pub pos: u32,
}

impl Location {
    pub fn new(pos: u32, line: u32, col: u32) -> Self {
        Location { line, col, pos }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Span(Location, Location);

pub type Id = EcoString;
pub type Op = TokenType;

#[derive(Debug)]
pub struct Prog {
    pub ds: Vec<Defn>,
}

#[derive(Debug)]
pub struct Defn {
    f: Id,
    xs: Vec<Id>,
    e: Expr,
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    BinOp(Box<Expr>, Op, Box<Expr>),
    Unary(Op, Box<Expr>),
    Grouping(Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(Id, Box<Expr>, Box<Expr>),
    Var(Id),
}

#[derive(Debug)]
pub enum Literal {
    Int(i32),
    Bool(bool),
    String(EcoString),
}
