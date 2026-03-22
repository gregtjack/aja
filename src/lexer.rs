use crate::token::Token;
use core::panic;
use ecow::EcoString;
use std::{fmt, iter::Peekable};

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
        write!(
            f,
            "SyntaxError: [line {}:{}] {}",
            self.line, self.col, self.message
        )
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
        let start = self.get_location();
        while let Some(c) = self.inner_next() {
            match c {
                '0'..='9' => {
                    return self.tok_number(c, start, false);
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
                '.' => return Ok((start, Token::Dot, self.get_location())),
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
                    if self.match_number() {
                        let c = self.inner_next().unwrap();
                        return self.tok_number(c, start, true);
                    } else if self.match_advance('>') {
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
                        self.get_location().line,
                        self.get_location().col - 1,
                        format!("Unrecognized char: '{}'", c),
                    ))
                }
            }
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

    /// Check if the next char is a number
    fn match_number(&mut self) -> bool {
        match self.inner_peek() {
            None => false,
            Some(&c) => c.is_ascii_digit(),
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
        while let Some(c) = self.inner_next() {
            if c == '\n' {
                return;
            }
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
                    Err(LexicalError::new(
                        self.line,
                        self.col,
                        "Invalid operator: '&'. Expected '&&'".to_string(),
                    ))
                }
            }
            _ => Err(LexicalError::new(self.line, self.col, "Invalid operator".to_string()).into()),
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
                                self.warn("Invalid escape sequence".to_string());
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

        Err(LexicalError::new(
            self.line,
            self.col,
            "Unterminated string".to_string(),
        ))
    }

    fn tok_number(&mut self, c: char, start: Location, negative: bool) -> LexResult {
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

        let sign: i8 = if negative { -1 } else { 1 };
        if is_float {
            let num = num_str
                .parse::<f32>()
                .expect("Number should have been a 32-bit float");
            Ok((
                start,
                Token::Float(f32::from(sign) * num),
                self.get_location(),
            ))
        } else {
            let num = num_str
                .parse::<i32>()
                .expect("Number should have been a 32-bit integer");
            Ok((
                start,
                Token::Int(i32::from(sign) * num),
                self.get_location(),
            ))
        }
    }

    fn tok_keyword_or_ident(&mut self, c: char, start: Location) -> LexResult {
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
            "let" => Some(Token::Let),
            "fn" => Some(Token::Fn),
            "if" => Some(Token::If),
            "while" => Some(Token::While),
            "else" => Some(Token::Else),
            "match" => Some(Token::Match),
            "return" => Some(Token::Return),
            _ => None,
        };

        if let Some(kw) = keyword {
            Ok((start, kw, self.get_location()))
        } else {
            Ok((start, Token::Ident(raw.into()), self.get_location()))
        }
    }

    fn warn(&self, msg: String) {
        println!("Syntax warning [line {}:{}]: {}", self.line, self.col, msg)
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
