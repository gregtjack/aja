use ecow::EcoString;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(EcoString),
    Int(i64),
    Float(f64),
    String(EcoString),
    /// The text of a template string up to its first `{`: `"a {` in
    /// `"a {x} b {y} c"`. The tokens of the expression `x` follow.
    TemplateStart(EcoString),
    /// The text between two of a template's expressions: `} b {`.
    TemplateMiddle(EcoString),
    /// The text after a template's last expression: `} c"`.
    TemplateEnd(EcoString),
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
    Question,     // ?
    Else,         // else
    Enum,         // enum
    Fn,           // fn
    If,           // if
    Let,          // let
    Match,        // match
    Return,       // return
    Struct,       // struct
    Impl,         // impl
    Import,       // import
    Pub,          // pub
    Var,          // var
    While,        // while
    For,          // for
    In,           // in
    Break,        // break
    Continue,     // continue
}

impl std::fmt::Display for Token {
    /// How the token is described in error messages, e.g. `'}'` or `end of file`.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sym = match self {
            Token::Ident(name) => return write!(f, "'{name}'"),
            Token::Int(i) => return write!(f, "'{i}'"),
            Token::Float(x) => return write!(f, "'{x:?}'"),
            Token::String(s) => return write!(f, "string {s:?}"),
            Token::TemplateStart(_) => return write!(f, "a template string"),
            Token::TemplateMiddle(_) | Token::TemplateEnd(_) => {
                return write!(f, "'}}' (the end of a template expression)")
            }
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
            Token::Question => "?",
            Token::Else => "else",
            Token::Enum => "enum",
            Token::Fn => "fn",
            Token::If => "if",
            Token::Let => "let",
            Token::Var => "var",
            Token::Match => "match",
            Token::Return => "return",
            Token::Struct => "struct",
            Token::Impl => "impl",
            Token::Import => "import",
            Token::Pub => "pub",
            Token::While => "while",
            Token::For => "for",
            Token::In => "in",
            Token::Break => "break",
            Token::Continue => "continue",
        };
        write!(f, "'{sym}'")
    }
}
