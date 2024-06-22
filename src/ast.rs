
use color_eyre::eyre::{bail, Result};
use ecow::EcoString;

use crate::{parse::ParseError, token::TokenType};

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
pub type Addr = usize;

#[derive(Debug, Clone)]
pub struct Program {
    pub ds: Vec<Definition>,
}

#[derive(Debug, Clone)]
pub enum Definition {
    Function {
        name: Id,
        args: Vec<Id>,
        body: Statement,
    },
    Struct {
        name: Id,
        fields: Vec<Id>,
    },
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Statement>),
    Expression(Expression),
    Let {
        var: Id,
        value: Expression,
    },
    Assign {
        lhs: Expression,
        rhs: Expression,
    },
    Return(Option<Expression>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Eof,
    Empty,
    Literal(Literal),
    BinOp(Box<Expression>, Op2, Box<Expression>),
    Unary(Op1, Box<Expression>),
    Grouping(Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Var(Id),
    Call(Id, Vec<Expression>),
}

#[derive(Debug, Clone)]
pub enum IExpression {
    Eof,
    Empty,
    Literal(Literal),
    BinOp(Box<IExpression>, Op2, Box<IExpression>),
    Unary(Op1, Box<IExpression>),
    Grouping(Box<IExpression>),
    If(Box<IExpression>, Box<IExpression>, Box<IExpression>),
    Let(Box<IExpression>, Box<IExpression>),
    Var(Addr),
    Call(Id, Vec<IExpression>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i32),
    Bool(bool),
    String(EcoString),
}

/// Unary operation
#[derive(Debug, Clone)]
pub enum Op1 {
    Not, // !
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
            _ => bail!("Token {:?} is not a valid op2", value)
        }
    }
}