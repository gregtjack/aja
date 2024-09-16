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
    Pipe,
    Comma,
    Dot,
    Semicolon,
    Colon,
    DoubleColon,
    RightArrow,
    FatArrow,
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
            Self::Eof => write!(f, "eof"),
            Self::Empty => write!(f, "()"),
            Self::Literal(_) => write!(f, "literal"),
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::LeftBrace => write!(f, "{{"),
            Self::RightBrace => write!(f, "}}"),
            Self::LeftBracket => write!(f, "["),
            Self::RightBracket => write!(f, "]"),
            Self::Comma => write!(f, ","),
            Self::Dot => write!(f, "."),
            Self::Semicolon => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::DoubleColon => write!(f, "::"),
            Self::RightArrow => write!(f, "->"),
            Self::FatArrow => write!(f, "=>"),
            Self::LeftArrow => write!(f, "<-"),
            Self::Minus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
            Self::Mult => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
            Self::Equal => write!(f, "="),
            Self::EqualEqual => write!(f, "=="),
            Self::Bang => write!(f, "!"),
            Self::BangEqual => write!(f, "!="),
            Self::GreaterThan => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::LessThan => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
            Self::Keyword(k) => write!(f, "{k}"),
            Self::Ident(_) => write!(f, "identifier"),
            Self::Pipe => write!(f, "|"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    UInt(u32),
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
