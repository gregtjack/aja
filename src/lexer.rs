use crate::token::{Keyword, Literal, Token, TokenType};
use core::panic;
use ecow::EcoString;
use std::{error::Error, fmt, fs::File, iter::Peekable};

use crate::ast::Location;

#[derive(Debug)]
pub struct LexicalError {
    line: u32,
    col: u32,
    message: String,
}

impl std::error::Error for LexicalError {}

impl LexicalError {
    fn new(line: u32, col: u32, msg: String) -> Self {
        LexicalError {
            line,
            col,
            message: msg,
        }
    }
}

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}:{}]: {}", self.line, self.col, self.message)
    }
}

type LexResult<T> = Result<T, LexicalError>;

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
    pub fn token(&mut self) -> LexResult<Token> {
        // represents the start of a token
        let start = self.pos();
        while let Some(c) = self.inner_next() {
            match c {
                '(' => return Ok(Token(start, TokenType::LeftParen, self.pos())),
                ')' => return Ok(Token(start, TokenType::RightParen, self.pos())),
                '{' => return Ok(Token(start, TokenType::LeftBrace, self.pos())),
                '}' => return Ok(Token(start, TokenType::RightBrace, self.pos())),
                '[' => return Ok(Token(start, TokenType::LeftBracket, self.pos())),
                ']' => return Ok(Token(start, TokenType::RightBracket, self.pos())),
                ',' => return Ok(Token(start, TokenType::Comma, self.pos())),
                '.' => return Ok(Token(start, TokenType::Dot, self.pos())),
                ':' => {
                    if self.match_advance(':') {
                        return Ok(Token(start, TokenType::DoubleColon, self.pos()));
                    } else {
                        return Ok(Token(start, TokenType::Colon, self.pos()));
                    }
                }
                ';' => return Ok(Token(start, TokenType::Semicolon, self.pos())),
                '"' => {
                    let tok = self.tok_str(start)?;
                    return Ok(tok);
                }
                '+' => return Ok(Token(start, TokenType::Plus, self.pos())),
                '-' => {
                    if self.match_number() {
                        let c = self.inner_next().unwrap();
                        let num = self.tok_number(c, start, true);
                        return Ok(num);
                    } else if self.match_advance('>') {
                        return Ok(Token(start, TokenType::RightArrow, self.pos()));
                    } else {
                        return Ok(Token(start, TokenType::Minus, self.pos()));
                    }
                }
                '*' => return Ok(Token(start, TokenType::Mult, self.pos())),
                '!' | '=' | '>' | '<' => {
                    let op = self.tok_op(c, start)?;
                    return Ok(op);
                }
                '/' => {
                    if self.match_advance('/') {
                        self.tok_comment();
                    } else {
                        return Ok(Token(start, TokenType::Div, self.pos()));
                    }
                }
                '%' => return Ok(Token(start, TokenType::Modulo, self.pos())),
                '0'..='9' => {
                    let num = self.tok_number(c, start, false);
                    return Ok(num);
                }
                '_' | 'a'..='z' | 'A'..='Z' => {
                    let res = self.tok_keyword_or_ident(c, start);
                    return Ok(res);
                }
                // ignore whitespace
                ' ' | '\t' | '\r' => {}
                '\n' => self.advance_line(),
                _ => {
                    return Err(LexicalError::new(
                        self.pos().line,
                        self.pos().col - 1,
                        format!("Unrecognized char: '{}'", c),
                    ))
                }
            }
        }

        self.eof = true;
        Ok(Token(start, TokenType::Eof, self.pos()))
    }

    fn inner_next(&mut self) -> Option<char> {
        self.col += 1;
        self.position += 1;
        self.inner.next()
    }

    fn inner_peek(&mut self) -> Option<&char> {
        self.inner.peek()
    }

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

    /// Check if the current char is a number
    fn match_number(&mut self) -> bool {
        match self.inner_peek() {
            None => false,
            Some(&c) => c.is_ascii_digit(),
        }
    }

    fn pos(&self) -> Location {
        Location {
            line: self.line,
            col: self.col,
            pos: self.position,
        }
    }

    // TODO: add doc comments
    fn tok_comment(&mut self) {
        while let Some(c) = self.inner_next() {
            if c == '\n' {
                return;
            }
        }
    }

    fn tok_op(&mut self, c: char, start: Location) -> LexResult<Token> {
        match c {
            '!' => {
                if self.match_advance('=') {
                    Ok(Token(start, TokenType::BangEqual, self.pos()))
                } else {
                    Ok(Token(start, TokenType::Bang, self.pos()))
                }
            }
            '=' => {
                if self.match_advance('=') {
                    Ok(Token(start, TokenType::EqualEqual, self.pos()))
                } else {
                    Ok(Token(start, TokenType::Equal, self.pos()))
                }
            }
            '>' => {
                if self.match_advance('=') {
                    Ok(Token(start, TokenType::GreaterEqual, self.pos()))
                } else {
                    Ok(Token(start, TokenType::GreaterThan, self.pos()))
                }
            }
            '<' => {
                if self.match_advance('=') {
                    Ok(Token(start, TokenType::LessEqual, self.pos()))
                } else if self.match_advance('-') {
                    Ok(Token(start, TokenType::LeftArrow, self.pos()))
                } else {
                    Ok(Token(start, TokenType::LessThan, self.pos()))
                }
            }
            _ => {
                Err(LexicalError::new(self.line, self.col, "Invalid delimiter".to_string()).into())
            }
        }
    }

    fn tok_str(&mut self, start: Location) -> LexResult<Token> {
        let mut literal = EcoString::new();
        while let Some(&c) = self.inner_peek() {
            match c {
                '"' => {
                    self.inner_next();
                    return Ok(Token(
                        start,
                        TokenType::Literal(Literal::String(literal)),
                        self.pos(),
                    ));
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

        Err(LexicalError::new(
            self.line,
            self.col,
            "Unterminated string".to_string(),
        ))
    }

    fn tok_number(&mut self, c: char, start: Location, negative: bool) -> Token {
        let mut number = c
            .to_string()
            .parse::<i32>()
            .expect("The caller should have passed a digit");
        while let Some(Ok(digit)) = self.inner_peek().map(|c| c.to_string().parse::<i32>()) {
            number = number * 10 + digit;
            self.inner_next();
        }

        if negative {
            Token(start, TokenType::Literal(Literal::Int(-number)), self.pos())
        } else {
            Token(start, TokenType::Literal(Literal::Int(number)), self.pos())
        }
    }

    fn tok_keyword_or_ident(&mut self, c: char, start: Location) -> Token {
        let mut raw = String::new();
        raw.push(c);
        while let Some(&c) = self.inner_peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '_' | '-' | '0'..='9' => {
                    self.inner_next();
                    raw.push(c);
                }
                _ => break,
            }
        }

        let keyword = match raw.as_str() {
            "let" => Some(TokenType::Keyword(Keyword::Let)),
            "in" => Some(TokenType::Keyword(Keyword::In)),
            "var" => Some(TokenType::Keyword(Keyword::Var)),
            "fn" => Some(TokenType::Keyword(Keyword::Fn)),
            "if" => Some(TokenType::Keyword(Keyword::If)),
            "else" => Some(TokenType::Keyword(Keyword::Else)),
            "match" => Some(TokenType::Keyword(Keyword::Match)),
            "return" => Some(TokenType::Keyword(Keyword::Return)),
            // boolean literal
            "true" => Some(TokenType::Literal(Literal::True)),
            "false" => Some(TokenType::Literal(Literal::False)),
            _ => None,
        };

        if let Some(kw) = keyword {
            Token(start, kw, self.pos())
        } else {
            Token(start, TokenType::Ident(raw.into()), self.pos())
        }
    }
}
impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof {
            return None;
        }

        match self.token() {
            Ok(t) => Some(t),
            Err(e) => panic!("{e}"),
        }
    }
}
