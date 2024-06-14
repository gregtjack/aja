use ecow::EcoString;

use crate::ast::Location;

/// Token(start, type, end)
#[derive(Debug, Clone)]
pub struct Token(pub Location, pub TokenType, pub Location);

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Eof,
    Empty,
    Literal(Literal),
    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Semicolon,
    Colon,
    RightArrow,
    LeftArrow,
    // Operators
    Minus,
    Plus,
    Mult, 
    Div,
    Equal, 
    EqualEqual,
    Bang,
    BangEqual,
    GreaterThan,
    GreaterEqual,
    LessThan,
    LessEqual,
    // Keywords
    Keyword(Keyword),
    Ident(EcoString),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i32),
    String(EcoString),
    True,
    False
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Let,
    Var,
    Fn,
    If,
    Else,
    Match,
    Return,
}
