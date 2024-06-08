use std::iter::Peekable;

use crate::{
    ast::{Expr, Prog},
    lexer::token::Token,
};

#[derive(Debug)]
enum ParseError {
    UnexpectedToken,
}

pub struct Parser<T>
where
    T: Iterator<Item = Token>,
{
    iter: Peekable<T>,
    current: u32,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(iter: Peekable<T>) -> Self {
        Self { iter, current: 0 }
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.iter.peek()
    }

    pub fn next(&mut self) -> Option<Token> {
        self.iter.next()
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Expr, ParseError> {}
