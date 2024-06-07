#[derive(Debug)]
pub struct Position {
    pub line: u32,
    pub col: u32,
    pub pos: u32,
}

impl Position {
    pub fn new(pos: u32, line: u32, col: u32) -> Self {
        Position { line, col, pos }
    }
}

#[derive(Debug)]
pub struct Spanning(Position, Position);

#[derive(Debug)]
pub struct Token {
    pub token: TokenType,
    pub span: Spanning, 
}

impl Token {
    pub fn new(t_type: TokenType, from: Position, to: Position) -> Self {
        Token { token: t_type, span: Spanning(from, to) }
    }
}

#[derive(Debug)]
pub enum TokenType {
    Eof,
    Empty, 
    // Items
    Ident(String),
    Int(i32),
    String(String),
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
    Asterisk, 
    Slash,
    Equal, 
    EqualEqual,
    Bang,
    BangEqual,
    GreaterThan,
    GreaterEqual,
    LessThan,
    LessEqual,
    // Keywords
    Let,
    True,
    False,
    Fn,
    If,
    Else,
    Return,
    Macro,
}
