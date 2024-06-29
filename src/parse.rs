use tracing::{debug, error};

use crate::{
    ast::{types::Type, Definition, Expression, Id, Literal, Program, Statement, Variable},
    token::{self, Keyword, Token, TokenType},
};
use std::{fmt, iter::Peekable};

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

#[derive(Debug)]
pub enum ParseError {
    InvalidToken {
        line: u32,
        col: u32,
        message: String,
    },
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
            // Self::Adhoc(s) => write!(f, "{}", s),
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

        let params = self.parse_fn_params()?;
        let rtype = self.parse_fn_type_annotation()?;
        let body = self.parse_block()?;

        Ok(Definition::Function {
            name: ident,
            params,
            body,
            rtype,
        })
    }

    fn parse_fn_params(&mut self) -> ParseResult<Vec<Variable>> {
        let mut xs = Vec::new();
        self.consume(TokenType::LeftParen)?;
        if !tok_matches!(self, TokenType::RightParen) {
            loop {
                let e = self.parse_primary()?;
                let vtype: Type = self.parse_type_annotation()?;
                let v = match e {
                    Expression::Var(id) => Variable::new(id, vtype),
                    _ => {
                        return Err(self.error(
                            self.prev_token.clone(),
                            "Function parameter must be variable".to_string(),
                        ))
                    }
                };
                xs.push(v);
                if tok_matches!(self, TokenType::Comma) {
                    self.next();
                } else {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen)?;
        Ok(xs)
    }

    fn parse_stmt(&mut self) -> ParseResult<Statement> {
        let Some(tok) = self.peek() else {
            return Err(ParseError::Eof);
        };

        match tok.1 {
            TokenType::LeftBrace => self.parse_block(),
            TokenType::Keyword(Keyword::Let) => self.parse_let(),
            TokenType::Keyword(Keyword::Return) => self.parse_return(),
            TokenType::Keyword(Keyword::If) => self.parse_if_stmt(),
            // TokenType::Keyword(Keyword::While) => self.parse_while(),
            _ => self.parse_stmt_expr(),
        }
    }

    fn parse_stmt_expr(&mut self) -> ParseResult<Statement> {
        let e = self.parse_expr()?;
        Ok(Statement::Expr(e))
    }

    fn parse_expr(&mut self) -> ParseResult<Expression> {
        let Some(tok) = self.peek() else {
            return Err(ParseError::Eof);
        };

        match tok.1 {
            TokenType::Keyword(Keyword::If) => self.parse_if_expr(),
            _ => self.parse_assignment(),
        }
    }

    /// Parse a sequence of expressions,
    /// defined by a separator, opening, and closing token
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

    fn parse_let(&mut self) -> ParseResult<Statement> {
        self.consume(TokenType::Keyword(Keyword::Let))?;

        let ident = self.parse_primary()?;

        match ident {
            Expression::Var(id) => {
                let ttype: Type = self.parse_type_annotation()?;

                self.consume(TokenType::Equal)?;
                let e = self.parse_expr()?;
                Ok(Statement::Let {
                    var: Variable::new(id, ttype),
                    value: e,
                })
            }
            _ => Err(self.error(
                self.prev_token.clone(),
                "variable declaration must be an identifier".to_string(),
            )),
        }
    }

    fn parse_block(&mut self) -> ParseResult<Statement> {
        let mut stmts = Vec::new();
        let mut tail = None;
        self.consume(TokenType::LeftBrace)?;
        while !tok_matches!(self, TokenType::RightBrace) {
            let stmt = self.parse_stmt()?;
            if tok_matches!(self, TokenType::Semicolon) {
                self.consume(TokenType::Semicolon)?;
                stmts.push(stmt);
            } else {
                match stmt {
                    Statement::Expr(e) => {
                        tail = Some(e);
                        break;
                    }
                    Statement::If(_, _) => {
                        // return Err(self.error(
                        //     self.prev_token.clone(),
                        //     "Expected expression at tail position. If expressions must have an `else` block.".to_string(),
                        // ))
                        stmts.push(stmt);
                        continue;
                    }
                    Statement::Block(_, t) => {
                        tail = t;
                        break;
                    }
                    _ => {
                        return Err(self.error(
                            self.prev_token.clone(),
                            "Expected expression at tail position".to_string(),
                        ))
                    }
                };
            }
        }

        self.consume(TokenType::RightBrace)?;

        Ok(Statement::Block(stmts, tail))
    }

    fn parse_return(&mut self) -> ParseResult<Statement> {
        self.consume(TokenType::Keyword(Keyword::Return))?;
        let res = if tok_matches!(self, TokenType::Semicolon) {
            Statement::Return(None)
        } else {
            Statement::Return(Some(self.parse_expr()?))
        };
        Ok(res)
    }

    fn parse_if_stmt(&mut self) -> ParseResult<Statement> {
        self.consume(TokenType::Keyword(Keyword::If))?;
        let e = self.parse_expr()?;
        let s1 = self.parse_block()?;
        if tok_matches!(self, TokenType::Keyword(Keyword::Else)) {
            self.consume(TokenType::Keyword(Keyword::Else))?;
            let s2 = self.parse_block()?;
            return Ok(Statement::Expr(Expression::If(
                Box::new(e),
                Box::new(s1),
                Box::new(s2),
            )));
        }
        Ok(Statement::If(e, Box::new(s1)))
    }

    fn parse_if_expr(&mut self) -> ParseResult<Expression> {
        self.consume(TokenType::Keyword(Keyword::If))?;
        let e1 = self.parse_expr()?;
        let e2 = self.parse_block()?;
        self.consume(TokenType::Keyword(Keyword::Else))?;
        let e3 = self.parse_block()?;
        Ok(Expression::If(Box::new(e1), Box::new(e2), Box::new(e3)))
    }

    fn parse_assignment(&mut self) -> ParseResult<Expression> {
        let expr = self.parse_equality()?;

        if tok_matches!(self, TokenType::Equal) {
            self.consume(TokenType::Equal)?;
            let equals = self.prev_token.clone();
            let value = self.parse_assignment()?;

            match expr.clone() {
                Expression::Var(v) => return Ok(Expression::Assign(v, Box::new(value))),
                _ => {
                    return Err(self.error(equals.clone(), "invalid assignment target".to_string()))
                }
            };
        }

        Ok(expr)
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

        self.parse_call()
    }

    fn parse_call(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_primary()?;
        while tok_matches!(self, TokenType::LeftParen) {
            let es = self.parse_expr_seq(
                TokenType::LeftParen,
                TokenType::Comma,
                TokenType::RightParen,
            )?;

            // if es.len() <= 255 {
            //     self.error(
            //         self.peek().cloned(),
            //         "Number of arguments exceeds 255".to_string(),
            //     );
            // }

            expr = Expression::Call(Box::new(expr), es);
        }

        return Ok(expr);
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
                TokenType::Ident(var) => Ok(Expression::Var(var)),
                TokenType::LeftParen => {
                    let expr = self.parse_expr()?;
                    self.consume(TokenType::RightParen)?;
                    Ok(Expression::Grouping(Box::new(expr)))
                }
                _ => Err(self.error(Some(next_tok), "unimplemented primary token".to_string())),
            }
        } else {
            Err(ParseError::Eof)
        }
    }

    fn parse_type_annotation(&mut self) -> ParseResult<Type> {
        if tok_matches!(self, TokenType::Colon) {
            self.consume(TokenType::Colon)?;
            let type_expr = self.parse_primary()?;
            match type_expr {
                Expression::Var(v) => Ok(v.to_string().try_into().unwrap()),
                _ => {
                    return Err(self.error(
                        self.prev_token.clone(),
                        "variable declaration must be an identifier".to_string(),
                    ))
                }
            }
        } else {
            Ok(Type::Any)
        }
    }

    fn parse_fn_type_annotation(&mut self) -> ParseResult<Type> {
        if tok_matches!(self, TokenType::RightArrow) {
            self.consume(TokenType::RightArrow)?;
            let type_expr = self.parse_primary()?;
            match type_expr {
                Expression::Var(v) => Ok(v.to_string().try_into().unwrap()),
                _ => {
                    return Err(self.error(
                        self.prev_token.clone(),
                        "variable declaration must be an identifier".to_string(),
                    ))
                }
            }
        } else {
            Ok(Type::Any)
        }
    }

    fn consume(&mut self, t: TokenType) -> ParseResult<Token> {
        if self.matches(vec![t.clone()]) {
            return Ok(self.next().unwrap());
        }
        let tok = self.peek().unwrap().clone();
        Err(self.error(Some(tok.clone()), format!("expected '{}'", t)))
    }

    fn matches(&mut self, ts: Vec<TokenType>) -> bool {
        self.peek().is_some_and(|a| ts.contains(&a.1))
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn next(&mut self) -> Option<Token> {
        self.position += 1;
        self.prev_token = self.peek().map(|t| t.clone());
        let t = self.tokens.next();
        t
    }

    fn error(&mut self, tok: Option<Token>, msg: String) -> ParseError {
        match tok {
            Some(Token(start, TokenType::Eof, _)) => {
                error!("[{} at end]: {}", start.line, msg);
                ParseError::InvalidToken {
                    line: start.line,
                    col: start.col,
                    message: msg,
                }
            }
            Some(Token(start, tok, _)) => {
                error!("[{}:{} at {:?}]: {}", start.line, start.col, tok, msg);
                ParseError::InvalidToken {
                    line: start.line,
                    col: start.col,
                    message: msg,
                }
            }
            None => {
                error!("[unknown error]: {}", msg);
                ParseError::Unknown
            }
        }
    }
}
