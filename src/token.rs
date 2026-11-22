use ecow::EcoString;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(EcoString),
    Int(i32),
    Float(f32),
    String(EcoString),
    Eof,
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Pipe,         // |
    Comma,        // ,
    Dot,          // .
    Semicolon,    // ;
    Colon,        // :
    DoubleColon,  // ::
    RightArrow,   // ->
    FatArrow,     // =>
    LeftArrow,    // <-
    Minus,        // -
    Plus,         // +
    Mult,         // *
    Div,          // /
    Modulo,       // %
    Equal,        // =
    EqualEqual,   // ==
    Bang,         // !
    BangEqual,    // !=
    GreaterThan,  // >
    GreaterEqual, // >=
    LessThan,     // <
    LessEqual,    // <=
    And,          // &&
    Or,           // ||
    DotDot,       // ..
    Else,         // else
    Enum,         // enum
    Fn,           // fn
    If,           // if
    Let,          // let
    Match,        // match
    Return,       // return
    Struct,       // struct
    While,        // while
}

impl std::fmt::Display for Token {
    /// How the token is described in error messages, e.g. `'}'` or `end of file`.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sym = match self {
            Token::Ident(name) => return write!(f, "'{name}'"),
            Token::Int(i) => return write!(f, "'{i}'"),
            Token::Float(x) => return write!(f, "'{x:?}'"),
            Token::String(s) => return write!(f, "string {s:?}"),
            Token::Eof => return write!(f, "end of file"),
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::LeftBrace => "{",
            Token::RightBrace => "}",
            Token::LeftBracket => "[",
            Token::RightBracket => "]",
            Token::Pipe => "|",
            Token::Comma => ",",
            Token::Dot => ".",
            Token::Semicolon => ";",
            Token::Colon => ":",
            Token::DoubleColon => "::",
            Token::RightArrow => "->",
            Token::FatArrow => "=>",
            Token::LeftArrow => "<-",
            Token::Minus => "-",
            Token::Plus => "+",
            Token::Mult => "*",
            Token::Div => "/",
            Token::Modulo => "%",
            Token::Equal => "=",
            Token::EqualEqual => "==",
            Token::Bang => "!",
            Token::BangEqual => "!=",
            Token::GreaterThan => ">",
            Token::GreaterEqual => ">=",
            Token::LessThan => "<",
            Token::LessEqual => "<=",
            Token::And => "&&",
            Token::Or => "||",
            Token::DotDot => "..",
            Token::Else => "else",
            Token::Enum => "enum",
            Token::Fn => "fn",
            Token::If => "if",
            Token::Let => "let",
            Token::Match => "match",
            Token::Return => "return",
            Token::Struct => "struct",
            Token::While => "while",
        };
        write!(f, "'{sym}'")
    }
}
