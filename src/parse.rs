use std::{error::Error, fmt, iter::Peekable};

use crate::{
    ast::{self, Expr, Literal, Prog},
    lexer::token::{self, Keyword, Token, TokenType},
};

#[derive(Debug)]
pub enum ParseError {
    InvalidToken {
        line: u32,
        col: u32,
        message: String,
    },
    Unknown,
}

impl std::error::Error for ParseError {}

impl fmt::Display for ParseError { 
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidToken { line, col, message } => write!(f, "[line {}:{}]: {}", line, col, message),
            _ => write!(f, "unknown error")
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
    prev_token: Option<Token>,
    position: u32,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(input: T) -> Self {
        Self {
            tokens: input.peekable(),
            position: 0,
            prev_token: None,
        }
    }

    pub fn parse(&mut self) -> ParseResult<Expr> {
        self.parse_expr()
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        // TODO: include src spans
        match self.peek() {
            // if expression
            Some(Token(_, TokenType::Keyword(Keyword::If), _)) => self.parse_if(),
            // all other exprs
            Some(_) => self.parse_equality(),
            None => Err(self.error(self.prev_token.clone(), "idk".to_string())),
        }
    }

    
    fn parse_if(&mut self) -> ParseResult<Expr> {
        self.consume(TokenType::Keyword(Keyword::If), "Expected 'if'".to_string())?;
        let e1 = self.parse_expr()?;
        self.consume(TokenType::LeftBrace, "Expected '{'".to_string())?;
        let e2 = self.parse_expr()?;
        self.consume(TokenType::RightBrace, "expected '}'".to_string())?;

        // TODO: make else optional
        self.consume(TokenType::Keyword(Keyword::Else), "Expected 'else'".to_string())?;

        self.consume(TokenType::LeftBrace, "Expected '{'".to_string())?;
        let e3 = self.parse_expr()?;
        self.consume(TokenType::RightBrace, "expected '}'".to_string())?;

        Ok(Expr::If(Box::new(e1), Box::new(e2), Box::new(e3)))
    }

    fn parse_equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_comparison()?;

        while matches!(
            self.peek(),
            Some(&Token(_, TokenType::BangEqual, _)) | Some(&Token(_, TokenType::EqualEqual, _))
        ) {
            let op = self.next().unwrap();
            let right = self.parse_comparison()?;
            expr = Expr::BinOp(
                Box::new(expr),
                op.1,
                Box::new(right),
            )
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_term()?;

        // TODO: include src spans
        while matches!(
            self.peek(),
            Some(&Token(_, TokenType::GreaterThan, _))
                | Some(&Token(_, TokenType::GreaterEqual, _))
                | Some(&Token(_, TokenType::LessThan, _))
                | Some(&Token(_, TokenType::LessEqual, _))
        ) {
            let op = self.next().unwrap();
            let right = self.parse_term()?;
            expr = Expr::BinOp(
                Box::new(expr),
                op.1,
                Box::new(right),
            )
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_factor()?;

        // TODO: include src spans
        while matches!(
            self.peek(),
            Some(&Token(_, TokenType::Plus, _))
                | Some(&Token(_, TokenType::Minus, _))
        ) {
            let op = self.next().unwrap();
            let right = self.parse_factor()?;
            expr = Expr::BinOp(
                Box::new(expr),
                op.1,
                Box::new(right),
            )
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_unary()?;

        // TODO: include src spans
        while matches!(
            self.peek(),
            Some(&Token(_, TokenType::Mult, _))
                | Some(&Token(_, TokenType::Div, _))
        ) {
            let op = self.next().unwrap();
            let right = self.parse_unary()?;
            expr = Expr::BinOp(
                Box::new(expr),
                op.1,
                Box::new(right),
            )
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParseResult<Expr> {
        if matches!(self.peek(), Some(&Token(_, TokenType::Bang, _))
        | Some(&Token(_, TokenType::Minus, _))) {
            let op = self.next().unwrap();
            let right = self.parse_unary()?;
            return Ok(Expr::Unary(op.1, Box::new(right)))
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        if let Some(next_tok) = self.next() {
            match next_tok {
                Token(_, TokenType::Literal(token::Literal::True), _) => Ok(Expr::Literal(Literal::Bool(true))),
                Token(_, TokenType::Literal(token::Literal::False), _) => Ok(Expr::Literal(Literal::Bool(false))),
                Token(_, TokenType::Literal(token::Literal::Int(i)), _) => Ok(Expr::Literal(Literal::Int(i))),
                Token(_, TokenType::Literal(token::Literal::String(s)), _) => Ok(Expr::Literal(Literal::String(s))),
                Token(_, TokenType::LeftParen, _) => {
                    let expr = self.parse_expr()?;
                    self.consume(TokenType::RightParen, "Expected ')' after expression.".to_string())?;
                    Ok(Expr::Grouping(Box::new(expr)))
                }
                tok => Err(self.error(Some(tok), "unimplemented primary token".to_string()))
            }
        } else {
            Err(self.error(self.prev_token.clone(), "??? what".to_string()))
        }
    }

    fn consume(&mut self, t: TokenType, msg: String) -> ParseResult<()> {
        if self.check(t) {
            self.next();
            return Ok(())
        }
        let tok = self.peek().unwrap().clone();
        Err(self.error(Some(tok), msg))
    }

    fn check(&mut self, b: TokenType) -> bool {
        self.peek().is_some_and(|a| a.1 == b)
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn next(&mut self) -> Option<Token> {
        self.position += 1;
        self.prev_token = self.peek().map(|t| t.clone());
        self.tokens.next()
    }

    fn is_eof(&mut self) -> bool {
        self.tokens.peek().is_none()
    }

    fn error(&mut self, tok: Option<Token>, msg: String) -> ParseError {
        match tok {
            Some(Token(start, TokenType::Eof, end)) => {
                eprintln!("[{} at end]: {}", start.line, msg);
                ParseError::InvalidToken { line: start.line, col: start.col, message: msg }
            },
            Some(Token(start, tok, end)) => { 
                eprintln!("[{}:{} at '{:?}']: {}", start.line, start.col, tok, msg);
                ParseError::InvalidToken { line: start.line, col: start.col, message: msg }
            }
            None => { 
                eprintln!("[error]: {}", msg);
                ParseError::Unknown
            }
        }
    }
}
