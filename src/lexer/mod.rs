use anyhow::{anyhow, Result};
use std::{error::Error, fmt, fs::File, iter::Peekable};
use token::{Token, TokenType, Position};
pub mod token;

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



struct Lexer<T>
where 
    T: Iterator<Item = char> 
{
    iter: Peekable<T>, 
    position: u32,
    line: u32,
    col: u32,
    prev: Option<char>,
}

impl<T> Lexer<T>
where
    T: Iterator<Item = char>
{
    pub fn new(input: Peekable<T>) -> Self {
        Lexer { 
            iter: input,
            position: 0, 
            line: 1, 
            col: 1, 
            prev: None,
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    pub fn next(&mut self) -> Option<char> {
        if let Some(c) = self.iter.next() {
            self.position += 1;
            self.col += 1;
            self.prev = Some(c);
            Some(c)
        } else {
            None
        }
    }

    pub fn advance_line(&mut self) {
        self.col = 1;
        self.line += 1;
    }

    pub fn prev_char(&self) -> Option<char> {
        self.prev
    }

    /// Advance the lexer if the current char is a match
    pub fn match_advance(&mut self, expect: char) -> bool {
        match self.peek() {
            None => false,
            Some(&c) => if c == expect {
                self.next();
                true
            } else {
                false
            }
        }
    }

    /// Check if the current char is a number
    pub fn match_number(&mut self) -> bool {
        match self.peek() {
            None => false,
            Some(&c) => c.is_ascii_digit(), 
        }
    }
    
    pub fn position(&self) -> Position {
        Position { line: self.line, col: self.col, pos: self.position }
    }
}


pub fn tokenize(input: String) -> Result<Vec<Token>> {
    let mut result = Vec::new();
    let mut lexer = Lexer::new(input.chars().peekable());
    // represents the start of a token
    let mut prev_pos = lexer.position();
    // main loop of the lexer
    while let Some(c) = lexer.next() {
        match c {
            '(' => {
                result.push(Token::new(TokenType::LeftParen, prev_pos, lexer.position()))
            },
            ')' => {
                result.push(Token::new(TokenType::RightParen, prev_pos, lexer.position()))
            }
            '{' => {
                result.push(Token::new(TokenType::LeftBrace, prev_pos, lexer.position()))
            }
            '}' => { 
                result.push(Token::new(TokenType::RightBrace, prev_pos, lexer.position())) 
            }
            ',' => {
                result.push(Token::new(TokenType::Comma, prev_pos, lexer.position()))
            }
            '.' => {
                result.push(Token::new(TokenType::Dot, prev_pos, lexer.position()))
            }
            '"' => { 
                result.push(read_str(&mut lexer, prev_pos)?)
            }
            '+' => {
                result.push(Token::new(TokenType::Plus, prev_pos, lexer.position()))
            }
            '-' => {
                if lexer.match_number() {
                    result.push(read_number(lexer.next().expect("should be number"), &mut lexer, prev_pos, true))
                } else if lexer.match_advance('>') {
                    result.push(Token::new(TokenType::RightArrow, prev_pos, lexer.position()))
                } else {
                    result.push(Token::new(TokenType::Minus, prev_pos, lexer.position()))
                }
            }
            // '!' | '=' | '>' | '<' => {
            //     result.push(read_op(c, &mut lexer)?)
            // }
            // '/' => {
            //     if lexer.match_advance('/') {
            //         read_comment(&mut lexer);
            //     } else {
            //         result.push(Token::Slash)
            //     }
            // }
            '0'..='9' => {
                result.push(read_number(c, &mut lexer, prev_pos, false))
            }
            // ignore whitespace
            ' ' | '\t' | '\r' => {},
            '\n' => {
                lexer.advance_line()
            }
            _ => return Err(LexicalError::new(lexer.position().line, lexer.position().col - 1, format!("Unrecognized token: '{}'", c)).into()),
        }

        prev_pos = lexer.position();
    }

    result.push(Token::new(TokenType::Eof, prev_pos, lexer.position()));

    Ok(result)
}

fn read_comment<T: Iterator<Item = char>>(lexer: &mut Lexer<T>) {
    while let Some(c) = lexer.next() {
        if c == '\n' {
            return;
        }
    }
}

// fn read_op<T: Iterator<Item = char>>(c: char, lexer: &mut Lexer<T>) -> Result<Token> {
//     match c {
//         '!' => if lexer.match_advance('=') {
//             Ok(Token::BangEqual)
//         } else {
//             Ok(Token::Bang)
//         }
//         '=' => if lexer.match_advance('=') {
//             Ok(Token::EqualEqual)
//         } else {
//             Ok(Token::Equal)
//         }
//         '>' => if lexer.match_advance('=') {
//             Ok(Token::GreaterEqual)
//         } else {
//             Ok(Token::GreaterThan)
//         }
//         '<' => if lexer.match_advance('=') {
//             Ok(Token::LessEqual)
//         } else if lexer.match_advance('-') {
//             Ok(Token::LeftArrow)
//         } else {
//             Ok(Token::LessThan)
//         }
//         _ => Err(LexicalError::new(lexer.line, lexer.col, "Invalid delimiter".to_string()).into())
//     }
// }

fn read_str<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, start: Position) -> Result<Token> {
    let mut literal = String::new();
    while let Some(&c) = lexer.peek() {
        match c {
            '"' => {
                lexer.next();
                return Ok(Token::new(TokenType::String(literal), start, lexer.position()));
            }
            '\n' => {
                lexer.next();
                lexer.advance_line();
                literal.push(c)
            }
            _ => {
                lexer.next();
                literal.push(c)
            }
        };
    }

    Err(LexicalError::new(lexer.line, lexer.col, "Unterminated string".to_string()).into())
}

fn read_number<T: Iterator<Item = char>>(c: char, lexer: &mut Lexer<T>, start: Position, negative: bool) -> Token {
    let mut number = c.to_string().parse::<i32>().expect("The caller should have passed a digit");
    while let Some(Ok(digit)) = lexer.peek().map(|c| c.to_string().parse::<i32>()) {
        number = number * 10 + digit;
        lexer.next();
    }

    if negative {
        Token::new(TokenType::Int(-number), start, lexer.position())
    } else {
        Token::new(TokenType::Int(number), start, lexer.position())
    }
}

