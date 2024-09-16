use std::fmt::Display;

use color_eyre::eyre::{bail, Result};
use ecow::EcoString;
use types::Type;

use crate::token::TokenType;

pub(crate) mod types;

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

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Id,
    pub t: Type,
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.t)
    }
}

impl Variable {
    pub fn new(name: Id, t: Type) -> Self {
        Self { name, t }
    }

    pub fn any(name: Id) -> Self {
        Self {
            name,
            t: Default::default(),
        }
    }

    pub fn to_string(&self) -> String {
        format!("{}: {}", self.name, self.t)
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub ds: Vec<Definition>,
}

#[derive(Debug, Clone)]
pub enum Definition {
    Function {
        name: Id,
        params: Vec<Variable>,
        body: Statement,
        rtype: Type,
    },
    Struct {
        name: Id,
        fields: Vec<Variable>,
    },
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Statement>, Option<Expression>),
    Expr(Expression),
    Let { var: Variable, value: Expression },
    If(Expression, Box<Statement>),
    While(Expression, Box<Statement>),
    Return(Option<Expression>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Empty,
    Literal(Literal),
    BinOp(Box<Expression>, Op2, Box<Expression>),
    Unary(Op1, Box<Expression>),
    Grouping(Box<Expression>),
    If(Box<Expression>, Box<Statement>, Box<Statement>),
    Var(Id),
    Call(Box<Expression>, Vec<Expression>),
    Assign(Id, Box<Expression>),
    Closure(Vec<Variable>, Type, Box<Statement>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    String(EcoString),
}

/// Unary operation
#[derive(Debug, Clone)]
pub enum Op1 {
    Not,    // !
    Negate, // -
}

impl TryFrom<TokenType> for Op1 {
    type Error = color_eyre::eyre::Error;
    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Bang => Ok(Self::Not),
            TokenType::Minus => Ok(Self::Negate),
            _ => bail!("Token {:?} is not a valid op1", value),
        }
    }
}

/// Binary operation
#[derive(Debug, Clone)]
pub enum Op2 {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

impl TryFrom<TokenType> for Op2 {
    type Error = color_eyre::eyre::Error;
    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Minus => Ok(Self::Subtraction),
            TokenType::Plus => Ok(Self::Addition),
            TokenType::Mult => Ok(Self::Multiplication),
            TokenType::Div => Ok(Self::Division),
            TokenType::EqualEqual => Ok(Self::Equal),
            TokenType::BangEqual => Ok(Self::NotEqual),
            TokenType::GreaterThan => Ok(Self::GreaterThan),
            TokenType::GreaterEqual => Ok(Self::GreaterEqual),
            TokenType::LessThan => Ok(Self::LessThan),
            TokenType::LessEqual => Ok(Self::LessEqual),
            _ => bail!("Token {:?} is not a valid op2", value),
        }
    }
}
