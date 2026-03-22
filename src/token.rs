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
    Else,         // else
    Fn,           // fn
    If,           // if
    Let,          // let
    Match,        // match
    Return,       // return
    While,        // while
}
