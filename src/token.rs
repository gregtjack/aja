use std::fmt::Display;

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
    DoubleColon,
    RightArrow,
    LeftArrow,
    // Operators
    Minus,
    Plus,
    Mult,
    Div,
    Modulo,
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

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Eof => write!(f, "eof"),
            TokenType::Empty => write!(f, "()"),
            TokenType::Literal(_) => write!(f, "literal"),
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::LeftBracket => write!(f, "["),
            TokenType::RightBracket => write!(f, "]"),
            TokenType::Comma => write!(f, ","),
            TokenType::Dot => write!(f, "."),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Colon => write!(f, ":"),
            TokenType::DoubleColon => write!(f, "::"),
            TokenType::RightArrow => write!(f, "->"),
            TokenType::LeftArrow => write!(f, "<-"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Mult => write!(f, "*"),
            TokenType::Div => write!(f, "/"),
            TokenType::Modulo => write!(f, "%"),
            TokenType::Equal => write!(f, "="),
            TokenType::EqualEqual => write!(f, "=="),
            TokenType::Bang => write!(f, "!"),
            TokenType::BangEqual => write!(f, "!="),
            TokenType::GreaterThan => write!(f, ">"),
            TokenType::GreaterEqual => write!(f, ">="),
            TokenType::LessThan => write!(f, "<"),
            TokenType::LessEqual => write!(f, "<="),
            TokenType::Keyword(k) => write!(f, "{k}"),
            TokenType::Ident(_) => write!(f, "identifier"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i32),
    Float(f32),
    String(EcoString),
    True,
    False,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Let,
    In,
    Var,
    Fn,
    If,
    Else,
    While,
    Match,
    Return,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Let => write!(f, "let"),
            Keyword::In => write!(f, "in"),
            Keyword::Var => write!(f, "var"),
            Keyword::Fn => write!(f, "fn"),
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::While => write!(f, "while"),
            Keyword::Match => write!(f, "match"),
            Keyword::Return => write!(f, "return"),
        }
    }
}
