use std::{fmt, iter::Peekable};

use color_eyre::eyre::Error;

use crate::{
    ast::{Definition, Expression, Id, Literal, Program, Statement},
    token::{self, Keyword, Token, TokenType},
};

macro_rules! tok_matches {
    ($self:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {
        $self.peek().is_some_and(|t|
            match t.1 {
                $pattern $(if $guard)? => true,
                _ => false
            }
        )
    };
}

macro_rules! consume {
    ($self:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {
        if tok_matches!($self, $pattern $(if $guard)?) {
            $self.next();
            Ok(())
        } else {
            let tok = $self.peek().unwrap().clone();
            Err($self.error(Some(tok), format!("expected {:?}", tok.1)))
        }
    };
}

#[derive(Debug)]
pub enum ParseError {
    InvalidToken {
        line: u32,
        col: u32,
        message: String,
    },
    Adhoc(String),
    Eof,
    Unknown,
}

impl std::error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidToken { line, col, message } => {
                write!(f, "[line {}:{}]: {}", line, col, message)
            }
            Self::Adhoc(s) => write!(f, "{}", s),
            Self::Eof => write!(f, "End of token stream"),
            _ => write!(f, "unknown error"),
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

    pub fn parse(&mut self) -> ParseResult<Program> {
        let mut ds = Vec::new();
        while tok_matches!(self, TokenType::Keyword(Keyword::Fn)) {
            let d = self.parse_defn()?;
            ds.push(d);
        }
        Ok(Program { ds })
    }

    fn parse_defn(&mut self) -> ParseResult<Definition> {
        match self.next() {
            Some(Token(_, TokenType::Keyword(Keyword::Fn), _)) => self.parse_fn_defn(),
            tok => Err(self.error(tok, "unknown definition".to_string())),
        }
    }

    fn parse_fn_defn(&mut self) -> ParseResult<Definition> {
        if !tok_matches!(self, TokenType::Ident(_)) {
            let tok = self.peek().cloned();
            return Err(self.error(tok, "Expected identifier".to_string()));
        }

        let ident = match self.next() {
            Some(Token(_, TokenType::Ident(i), _)) => i,
            _ => unreachable!(),
        };

        // parse arguments
        let es = self.parse_expr_seq(
            TokenType::LeftParen,
            TokenType::Comma,
            TokenType::RightParen,
        )?;
        let mut xs: Vec<Id> = vec![];

        for e in es {
            match e {
                Expression::Var(id) => xs.push(id),
                _ => {
                    return Err(self.error(
                        self.prev_token.clone(),
                        "Function parameters must be identifiers".to_string(),
                    ))
                }
            }
        }

        let s = self.parse_block()?;

        Ok(Definition::Function {
            name: ident,
            args: xs,
            body: s,
        })
    }

    fn parse_stmt(&mut self) -> ParseResult<Statement> {
        let Some(tok) = self.peek() else {
            return Err(ParseError::Eof);
        };

        match tok.1 {
            TokenType::LeftBrace => self.parse_block(),
            _ => unimplemented!(),
        }
    }

    fn parse_expr(&mut self) -> ParseResult<Expression> {
        let Some(tok) = self.peek() else {
            return Err(ParseError::Eof);
        };

        // TODO: include src spans
        match tok.1 {
            TokenType::Keyword(Keyword::If) => self.parse_if(),
            // Some(Token(_, TokenType::Keyword(Keyword::Let), _)) => self.parse_let(),
            // all other exprs
            _ => self.parse_equality(),
        }
    }

    /// Parse a sequence of expressions,
    /// defined by a separator and opening and closing token
    fn parse_expr_seq(
        &mut self,
        open: TokenType,
        sep: TokenType,
        close: TokenType,
    ) -> ParseResult<Vec<Expression>> {
        let mut xs = Vec::new();
        self.consume(open.clone())?;
        if !tok_matches!(self, TokenType::RightParen) {
            loop {
                let e = self.parse_expr()?;
                xs.push(e);
                match self.peek() {
                    Some(Token(_, tt, _)) if *tt == sep => {
                        self.next();
                    }
                    _ => break,
                }
            }
        }
        self.consume(close.clone())?;
        Ok(xs)
    }

    fn parse_if(&mut self) -> ParseResult<Expression> {
        consume!(self, TokenType::Keyword(Keyword::If))?;
        let e1 = self.parse_expr()?;
        let e2 = self.parse_block()?;
        self.consume(TokenType::Keyword(Keyword::Else))?;
        let e3 = self.parse_block()?;
        Ok(Expression::If(Box::new(e1), Box::new(e2), Box::new(e3)))
    }

    fn parse_let(&mut self) -> ParseResult<Statement> {
        self.consume(TokenType::Keyword(Keyword::Let))?;

        let ident = self.parse_var_or_call()?;

        match ident {
            Expression::Var(id) => {
                self.consume(TokenType::Equal)?;
                let e2 = self.parse_expr()?;
                self.consume(TokenType::Semicolon)?;

                Ok(Statement::Let(id, Box::new(e2)))
            }
            _ => Err(self.error(
                self.prev_token.clone(),
                "variable declaration must be an identifier".to_string(),
            )),
        }
    }

    fn parse_block(&mut self) -> ParseResult<Statement> {
        let mut stmts = Vec::new();
        self.consume(TokenType::LeftBrace)?;
        if !tok_matches!(self, TokenType::RightBrace) {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }
        self.consume(TokenType::RightBrace)?;
        Ok(Statement::Block(stmts))
    }

    fn parse_equality(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_comparison()?;

        while self.matches(vec![TokenType::EqualEqual, TokenType::BangEqual]) {
            let op = self.next().unwrap();
            let right = self.parse_comparison()?;
            expr = Expression::BinOp(Box::new(expr), op.1.try_into().unwrap(), Box::new(right))
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_term()?;

        // TODO: include src spans
        while tok_matches!(
            self,
            TokenType::GreaterThan
                | TokenType::GreaterEqual
                | TokenType::LessThan
                | TokenType::LessEqual
        ) {
            let op = self.next().unwrap();
            let right = self.parse_term()?;
            expr = Expression::BinOp(Box::new(expr), op.1.try_into().unwrap(), Box::new(right))
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_factor()?;

        // TODO: include src spans
        while tok_matches!(self, TokenType::Plus | TokenType::Minus) {
            let op = self.next().unwrap();
            let right = self.parse_factor()?;
            expr = Expression::BinOp(Box::new(expr), op.1.try_into().unwrap(), Box::new(right))
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_unary()?;

        // TODO: include src spans
        while tok_matches!(self, TokenType::Mult | TokenType::Div) {
            let op = self.next().unwrap();
            let right = self.parse_unary()?;
            expr = Expression::BinOp(Box::new(expr), op.1.try_into().unwrap(), Box::new(right))
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParseResult<Expression> {
        if tok_matches!(self, TokenType::Bang | TokenType::Minus) {
            let op = self.next().unwrap();
            let right = self.parse_unary()?;
            return Ok(Expression::Unary(op.1.try_into().unwrap(), Box::new(right)));
        }

        self.parse_var_or_call()
    }

    fn parse_var_or_call(&mut self) -> ParseResult<Expression> {
        if tok_matches!(self, TokenType::Ident(_)) {
            let tok = self.next().unwrap();
            if let Token(_, TokenType::Ident(id), _) = tok {
                if tok_matches!(self, TokenType::LeftParen) {
                    let es = self.parse_expr_seq(
                        TokenType::LeftParen,
                        TokenType::Comma,
                        TokenType::RightParen,
                    )?;
                    return Ok(Expression::Call(id.clone(), es));
                } else {
                    return Ok(Expression::Var(id.clone()));
                }
            } else {
                unreachable!()
            }
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> ParseResult<Expression> {
        if let Some(next_tok) = self.next() {
            match next_tok.1 {
                TokenType::Literal(token::Literal::True) => {
                    Ok(Expression::Literal(Literal::Bool(true)))
                }
                TokenType::Literal(token::Literal::False) => {
                    Ok(Expression::Literal(Literal::Bool(false)))
                }
                TokenType::Literal(token::Literal::Int(i)) => {
                    Ok(Expression::Literal(Literal::Int(i)))
                }
                TokenType::Literal(token::Literal::String(s)) => {
                    Ok(Expression::Literal(Literal::String(s)))
                }
                TokenType::LeftParen => {
                    let expr = self.parse_expr()?;
                    self.consume(TokenType::RightParen)?;
                    Ok(Expression::Grouping(Box::new(expr)))
                }
                tok => {
                    Err(self.error(Some(tok.clone()), "unimplemented primary token".to_string()))
                }
            }
        } else {
            Err(self.error(self.prev_token.clone(), "??? what".to_string()))
        }
    }

    fn consume(&mut self, t: TokenType) -> ParseResult<()> {
        if self.check(t) {
            self.next();
            return Ok(());
        }
        let tok = self.peek().unwrap().clone();
        Err(self.error(Some(tok), format!("expected {:?}", tok.1)))
    }

    fn matches(&mut self, ts: Vec<TokenType>) -> bool {
        self.peek().is_some_and(|a| ts.contains(&a.t_type))
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn next(&mut self) -> Option<Token> {
        self.position += 1;
        self.prev_token = self.peek().map(|t| t.clone());
        let t = self.tokens.next();
        // println!("{:?}", t);
        t
    }

    fn is_eof(&mut self) -> bool {
        self.tokens.peek().is_none()
    }

    fn error(&mut self, tok: Option<Token>, msg: String) -> ParseError {
        match tok {
            Some(Token(start, TokenType::Eof, end)) => {
                eprintln!("[{} at end]: {}", start.line, msg);
                ParseError::InvalidToken {
                    line: start.line,
                    col: start.col,
                    message: msg,
                }
            }
            Some(Token(start, tok, end)) => {
                eprintln!("[{}:{} at '{:?}']: {}", start.line, start.col, tok, msg);
                ParseError::InvalidToken {
                    line: start.line,
                    col: start.col,
                    message: msg,
                }
            }
            None => {
                eprintln!("[unknown error]: {}", msg);
                ParseError::Unknown
            }
        }
    }
}
