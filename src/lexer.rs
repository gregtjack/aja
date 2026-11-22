use crate::token::Token;
use ecow::EcoString;
use std::{fmt, iter::Peekable};

use crate::{ast::Location, diagnostic::Diagnostic};

#[derive(Debug)]
pub struct LexicalError {
    pub start: Location,
    pub end: Location,
    pub message: String,
    pub help: Option<String>,
}

impl std::error::Error for LexicalError {}

impl LexicalError {
    fn new(start: Location, end: Location, msg: impl Into<String>) -> Self {
        LexicalError {
            start,
            end,
            message: msg.into(),
            help: None,
        }
    }

    fn help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        Diagnostic::new(self.message.clone())
            .with_span(self.start, self.end)
            .with_help(self.help.clone())
    }
}

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}: {}", self.start.line, self.start.col, self.message)
    }
}

pub type SrcSpan = (Location, Token, Location);
pub type LexResult = Result<SrcSpan, LexicalError>;

pub struct Lexer<T>
where
    T: Iterator<Item = char>,
{
    inner: Peekable<T>,
    position: u32,
    line: u32,
    col: u32,
    eof: bool,
}

impl<T> Lexer<T>
where
    T: Iterator<Item = char>,
{
    pub fn new(input: T) -> Self {
        Lexer {
            inner: input.peekable(),
            position: 0,
            line: 1,
            col: 1,
            eof: false,
        }
    }

    /// Process one token.
    pub fn token(&mut self) -> LexResult {
        let mut start = self.get_location();
        while let Some(c) = self.inner_next() {
            match c {
                '0'..='9' => {
                    return self.tok_number(c, start);
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    return self.tok_keyword_or_ident(c, start);
                }
                // ignore whitespace
                ' ' | '\t' | '\r' => {}
                '\n' => self.advance_line(),
                '(' => return Ok((start, Token::LeftParen, self.get_location())),
                ')' => return Ok((start, Token::RightParen, self.get_location())),
                '{' => return Ok((start, Token::LeftBrace, self.get_location())),
                '}' => return Ok((start, Token::RightBrace, self.get_location())),
                '[' => return Ok((start, Token::LeftBracket, self.get_location())),
                ']' => return Ok((start, Token::RightBracket, self.get_location())),
                '|' => {
                    if self.match_advance('|') {
                        return Ok((start, Token::Or, self.get_location()));
                    } else {
                        return Ok((start, Token::Pipe, self.get_location()));
                    }
                }
                ',' => return Ok((start, Token::Comma, self.get_location())),
                '.' => {
                    if self.match_advance('.') {
                        return Ok((start, Token::DotDot, self.get_location()));
                    }
                    return Ok((start, Token::Dot, self.get_location()));
                }
                ':' => {
                    if self.match_advance(':') {
                        return Ok((start, Token::DoubleColon, self.get_location()));
                    } else {
                        return Ok((start, Token::Colon, self.get_location()));
                    }
                }
                ';' => return Ok((start, Token::Semicolon, self.get_location())),
                '"' => {
                    let tok = self.tok_str(start)?;
                    return Ok(tok);
                }
                '+' => return Ok((start, Token::Plus, self.get_location())),
                '-' => {
                    if self.match_advance('>') {
                        return Ok((start, Token::RightArrow, self.get_location()));
                    } else {
                        return Ok((start, Token::Minus, self.get_location()));
                    }
                }
                '*' => return Ok((start, Token::Mult, self.get_location())),
                '!' | '=' | '>' | '<' | '&' => {
                    let op = self.tok_op(c, start)?;
                    return Ok(op);
                }
                '/' => {
                    if self.match_advance('/') {
                        self.tok_comment();
                    } else {
                        return Ok((start, Token::Div, self.get_location()));
                    }
                }
                '%' => return Ok((start, Token::Modulo, self.get_location())),
                _ => {
                    return Err(LexicalError::new(
                        start,
                        self.get_location(),
                        format!("unexpected character '{}'", c),
                    ))
                }
            }
            // Only whitespace and comments reach here: the next token starts after them.
            start = self.get_location();
        }

        self.eof = true;
        Ok((start, Token::Eof, self.get_location()))
    }

    /// Advance the iterator
    fn inner_next(&mut self) -> Option<char> {
        self.col += 1;
        self.position += 1;
        self.inner.next()
    }

    /// Peek the next character in the iterator
    fn inner_peek(&mut self) -> Option<&char> {
        self.inner.peek()
    }

    /// Increment the line counter and reset the current column
    fn advance_line(&mut self) {
        self.col = 1;
        self.line += 1;
    }

    /// Advance self if the current char is a match
    fn match_advance(&mut self, expect: char) -> bool {
        match self.inner_peek() {
            None => false,
            Some(&c) => {
                if c == expect {
                    self.inner_next();
                    true
                } else {
                    false
                }
            }
        }
    }

    fn get_location(&self) -> Location {
        Location {
            line: self.line,
            col: self.col,
            pos: self.position,
        }
    }

    /// Tokenize double slash (//) comments
    fn tok_comment(&mut self) {
        while let Some(&c) = self.inner_peek() {
            if c == '\n' {
                // Leave the newline for `token` so line counting stays in one place.
                return;
            }
            self.inner_next();
        }
    }

    /// Tokenize
    fn tok_op(&mut self, c: char, start: Location) -> LexResult {
        match c {
            '!' => {
                if self.match_advance('=') {
                    Ok((start, Token::BangEqual, self.get_location()))
                } else {
                    Ok((start, Token::Bang, self.get_location()))
                }
            }
            '=' => {
                if self.match_advance('=') {
                    Ok((start, Token::EqualEqual, self.get_location()))
                } else if self.match_advance('>') {
                    Ok((start, Token::FatArrow, self.get_location()))
                } else {
                    Ok((start, Token::Equal, self.get_location()))
                }
            }
            '>' => {
                if self.match_advance('=') {
                    Ok((start, Token::GreaterEqual, self.get_location()))
                } else {
                    Ok((start, Token::GreaterThan, self.get_location()))
                }
            }
            '<' => {
                if self.match_advance('=') {
                    Ok((start, Token::LessEqual, self.get_location()))
                } else if self.match_advance('-') {
                    Ok((start, Token::LeftArrow, self.get_location()))
                } else {
                    Ok((start, Token::LessThan, self.get_location()))
                }
            }
            '&' => {
                if self.match_advance('&') {
                    Ok((start, Token::And, self.get_location()))
                } else {
                    Err(
                        LexicalError::new(start, self.get_location(), "unexpected character '&'")
                            .help("use '&&' for logical and"),
                    )
                }
            }
            _ => Err(LexicalError::new(start, self.get_location(), "invalid operator")),
        }
    }

    fn tok_str(&mut self, start: Location) -> LexResult {
        let mut literal = EcoString::new();
        while let Some(&c) = self.inner_peek() {
            match c {
                '"' => {
                    self.inner_next();
                    return Ok((start, Token::String(literal), self.get_location()));
                }
                '\\' => {
                    self.inner_next();
                    if let Some(&c) = self.inner_peek() {
                        match c {
                            // new line
                            'n' => {
                                self.inner_next();
                                literal.push('\n')
                            }
                            // horizontal tab
                            't' => {
                                self.inner_next();
                                literal.push('\t')
                            }
                            // carriage return
                            'r' => {
                                self.inner_next();
                                literal.push('\r')
                            }
                            '\\' => {
                                self.inner_next();
                                literal.push('\\')
                            }
                            _ => {
                                self.warn(format!("unknown escape sequence '\\{}'", c));
                                self.inner_next();
                                literal.push('\\');
                                literal.push(c)
                            }
                        }
                    }
                }
                '\n' => {
                    self.inner_next();
                    self.advance_line();
                    literal.push(c)
                }
                _ => {
                    self.inner_next();
                    literal.push(c)
                }
            };
        }

        let mut quote_end = start;
        quote_end.col += 1;
        Err(LexicalError::new(start, quote_end, "unterminated string literal")
            .help("add a closing '\"' to end the string"))
    }

    fn tok_number(&mut self, c: char, start: Location) -> LexResult {
        let mut is_float = false;
        let mut num_str = String::new();
        num_str.push(c);

        while let Some(c) = self.inner_peek() {
            match c {
                '0'..='9' => {
                    num_str.push(*c);
                    self.inner_next();
                }
                '.' => {
                    num_str.push(*c);
                    is_float = true;
                    self.inner_next();
                }
                '_' => {
                    self.inner_next();
                }
                _ => break,
            }
        }

        if is_float {
            match num_str.parse::<f32>() {
                Ok(num) => Ok((start, Token::Float(num), self.get_location())),
                Err(_) => Err(LexicalError::new(
                    start,
                    self.get_location(),
                    format!("invalid float literal '{}'", num_str),
                )),
            }
        } else {
            match num_str.parse::<i32>() {
                Ok(num) => Ok((start, Token::Int(num), self.get_location())),
                Err(_) => Err(LexicalError::new(
                    start,
                    self.get_location(),
                    format!("integer literal '{}' is too large", num_str),
                )
                .help(format!("int values must be between {} and {}", i32::MIN, i32::MAX))),
            }
        }
    }

    fn tok_keyword_or_ident(&mut self, c: char, start: Location) -> LexResult {
        let mut raw = String::new();
        raw.push(c);
        while let Some(&c) = self.inner_peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                    self.inner_next();
                    raw.push(c);
                }
                _ => break,
            }
        }

        let keyword = match raw.as_str() {
            "let" => Some(Token::Let),
            "fn" => Some(Token::Fn),
            "if" => Some(Token::If),
            "while" => Some(Token::While),
            "else" => Some(Token::Else),
            "match" => Some(Token::Match),
            "return" => Some(Token::Return),
            "struct" => Some(Token::Struct),
            "enum" => Some(Token::Enum),
            _ => None,
        };

        if let Some(kw) = keyword {
            Ok((start, kw, self.get_location()))
        } else {
            Ok((start, Token::Ident(raw.into()), self.get_location()))
        }
    }

    fn warn(&self, msg: String) {
        eprintln!("warning: {} (line {}, column {})", msg, self.line, self.col)
    }
}
/// Tokenize a whole source string, stopping at the first lexical error.
/// The returned tokens always end with `Token::Eof`.
pub fn tokenize(src: &str) -> Result<Vec<SrcSpan>, LexicalError> {
    tokenize_from_line(src, 1)
}

/// Like [`tokenize`], but numbering lines from `first_line` (used by the REPL,
/// where each input is a new line of one growing session).
pub fn tokenize_from_line(src: &str, first_line: u32) -> Result<Vec<SrcSpan>, LexicalError> {
    let mut lexer = Lexer::new(src.chars());
    lexer.line = first_line;
    let mut tokens = Vec::new();
    loop {
        let span = lexer.token()?;
        let eof = span.1 == Token::Eof;
        tokens.push(span);
        if eof {
            return Ok(tokens);
        }
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = SrcSpan;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof {
            return None;
        }

        match self.token() {
            Ok(t) => return Some(t),
            Err(e) => panic!("{}", e),
        }
    }
}
