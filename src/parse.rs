use crate::{
    ast::{types::Type, Definition, Expression, Program, Statement, Variable},
    lexer::SrcSpan,
    token::Token,
};
use std::{fmt, iter::Peekable};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{types::Type, Definition, Expression, Statement},
        lexer::Lexer,
    };

    // Helper function to create a parser from a string
    fn create_parser(input: &str) -> Parser<std::vec::IntoIter<crate::lexer::SrcSpan>> {
        let lexer = Lexer::new(input.chars());
        let tokens: Vec<_> = lexer.collect();
        Parser::new(tokens.into_iter())
    }

    #[test]
    fn test_parse_integer_literal() {
        let mut parser = create_parser("42");
        let result = parser.parse_expr_only().unwrap();
        match result {
            Expression::Int(42) => {}
            _ => panic!("Expected Expression::Int(42), got {:?}", result),
        }
    }

    #[test]
    fn test_parse_float_literal() {
        let mut parser = create_parser("3.14");
        let result = parser.parse_expr_only().unwrap();
        match result {
            Expression::Float(f) => assert!((f - 3.14).abs() < 0.001),
            _ => panic!("Expected Expression::Float(3.14), got {:?}", result),
        }
    }

    #[test]
    fn test_parse_string_literal() {
        let mut parser = create_parser("\"hello world\"");
        let result = parser.parse_expr_only().unwrap();
        match result {
            Expression::String(s) => assert_eq!(s, "hello world"),
            _ => panic!("Expected Expression::String, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_boolean_literals() {
        let mut parser_true = create_parser("true");
        let result_true = parser_true.parse_expr_only().unwrap();
        match result_true {
            Expression::True => {}
            _ => panic!("Expected Expression::True, got {:?}", result_true),
        }

        let mut parser_false = create_parser("false");
        let result_false = parser_false.parse_expr_only().unwrap();
        match result_false {
            Expression::False => {}
            _ => panic!("Expected Expression::False, got {:?}", result_false),
        }
    }

    #[test]
    fn test_parse_variable() {
        let mut parser = create_parser("x");
        let result = parser.parse_expr_only().unwrap();
        match result {
            Expression::Var(name) => assert_eq!(name, "x"),
            _ => panic!("Expected Expression::Var, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_grouping() {
        let mut parser = create_parser("(42)");
        let result = parser.parse_expr_only().unwrap();
        match result {
            Expression::Grouping(expr) => match *expr {
                Expression::Int(42) => {}
                _ => panic!("Expected Expression::Int(42) inside grouping"),
            },
            _ => panic!("Expected Expression::Grouping, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_unary_operations() {
        // Test negation - the lexer treats -42 as a single negative integer token
        let mut parser = create_parser("-42");
        let result = parser.parse_expr_only().unwrap();
        match result {
            Expression::Int(-42) => {}
            _ => panic!("Expected Expression::Int(-42), got {:?}", result),
        }

        // Test logical not
        let mut parser = create_parser("!true");
        let result = parser.parse_expr_only().unwrap();
        match result {
            Expression::Unary(op, expr) => {
                match op {
                    crate::ast::Op1::Not => {}
                    _ => panic!("Expected Not operator"),
                }
                match *expr {
                    Expression::True => {}
                    _ => panic!("Expected Expression::True"),
                }
            }
            _ => panic!("Expected Expression::Unary, got {:?}", result),
        }

        // Test unary minus on a variable (this should create a unary operation)
        let mut parser = create_parser("-x");
        let result = parser.parse_expr_only().unwrap();
        match result {
            Expression::Unary(op, expr) => {
                match op {
                    crate::ast::Op1::Negate => {}
                    _ => panic!("Expected Negate operator"),
                }
                match *expr {
                    Expression::Var(name) => assert_eq!(name, "x"),
                    _ => panic!("Expected Expression::Var(\"x\")"),
                }
            }
            _ => panic!("Expected Expression::Unary, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_binary_operations() {
        // Test addition
        let mut parser = create_parser("1 + 2");
        let result = parser.parse_expr_only().unwrap();
        match result {
            Expression::BinOp(left, op, right) => {
                match op {
                    crate::ast::Op2::Addition => {}
                    _ => panic!("Expected Addition operator"),
                }
                match *left {
                    Expression::Int(1) => {}
                    _ => panic!("Expected left operand to be 1"),
                }
                match *right {
                    Expression::Int(2) => {}
                    _ => panic!("Expected right operand to be 2"),
                }
            }
            _ => panic!("Expected Expression::BinOp, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_assignment() {
        let mut parser = create_parser("x = 42");
        let result = parser.parse_expr_only().unwrap();
        match result {
            Expression::Assign(name, value) => {
                assert_eq!(name, "x");
                match *value {
                    Expression::Int(42) => {}
                    _ => panic!("Expected value to be 42"),
                }
            }
            _ => panic!("Expected Expression::Assign, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_function_call() {
        let mut parser = create_parser("foo()");
        let result = parser.parse_expr_only().unwrap();
        match result {
            Expression::Call(func, args) => {
                match *func {
                    Expression::Var(name) => assert_eq!(name, "foo"),
                    _ => panic!("Expected function to be a variable"),
                }
                assert_eq!(args.len(), 0);
            }
            _ => panic!("Expected Expression::Call, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_let_statement() {
        let mut parser = create_parser("let x: int = 42;");
        let result = parser.parse_stmt_only().unwrap();
        match result {
            Statement::Let { var, value } => {
                assert_eq!(var.name, "x");
                assert_eq!(var.t, Type::Int);
                match value {
                    Expression::Int(42) => {}
                    _ => panic!("Expected value to be 42"),
                }
            }
            _ => panic!("Expected Statement::Let, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_return_statement() {
        // Return with value
        let mut parser = create_parser("return 42;");
        let result = parser.parse_stmt_only().unwrap();
        match result {
            Statement::Return(Some(value)) => match value {
                Expression::Int(42) => {}
                _ => panic!("Expected return value to be 42"),
            },
            _ => panic!("Expected Statement::Return(Some(_)), got {:?}", result),
        }

        // Return without value
        let mut parser = create_parser("return;");
        let result = parser.parse_stmt_only().unwrap();
        match result {
            Statement::Return(None) => {}
            _ => panic!("Expected Statement::Return(None), got {:?}", result),
        }
    }

    #[test]
    fn test_parse_function_definition() {
        let mut parser = create_parser("fn add(x: int, y: int) -> int { x + y }");
        let result = parser.parse().unwrap();

        assert_eq!(result.definitions.len(), 1);

        match &result.definitions[0] {
            Definition::Function {
                name,
                params,
                body: _,
                rtype,
            } => {
                assert_eq!(name, "add");
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].name, "x");
                assert_eq!(params[0].t, Type::Int);
                assert_eq!(params[1].name, "y");
                assert_eq!(params[1].t, Type::Int);
                assert_eq!(rtype, &Type::Int);
            }
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_parse_error_cases() {
        // Test unexpected token
        let mut parser = create_parser("}");
        let result = parser.parse_expr_only();
        assert!(result.is_err());

        // Test incomplete expression
        let mut parser = create_parser("1 +");
        let result = parser.parse_expr_only();
        assert!(result.is_err());

        // Test invalid assignment target
        let mut parser = create_parser("1 = 2");
        let result = parser.parse_expr_only();
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_empty_program() {
        let mut parser = create_parser("");
        let result = parser.parse().unwrap();
        assert_eq!(result.definitions.len(), 0);
    }
}

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
                write!(f, "ParseError: [line {}:{}] {}", line, col, message)
            }
            Self::Eof => write!(f, "End of token stream"),
            _ => write!(f, "unknown error"),
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<T: Iterator<Item = SrcSpan>> {
    tokens: Peekable<T>,
    prev_token: Option<SrcSpan>,
    position: u32,
}

impl<T> Parser<T>
where
    T: Iterator<Item = SrcSpan>,
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
        while tok_matches!(self, Token::Fn) {
            let d = self.parse_defn()?;
            ds.push(d);
        }

        Ok(Program { definitions: ds })
    }

    pub fn parse_expr_only(&mut self) -> ParseResult<Expression> {
        self.parse_expr()
    }

    pub fn parse_stmt_only(&mut self) -> ParseResult<Statement> {
        self.parse_stmt()
    }

    fn parse_defn(&mut self) -> ParseResult<Definition> {
        let res = match self.next() {
            Some((_, Token::Fn, _)) => self.parse_fn_defn(),
            tok => Err(self.error(tok, "Unknown definition".to_string())),
        };

        res
    }

    fn parse_fn_defn(&mut self) -> ParseResult<Definition> {
        if !tok_matches!(self, Token::Ident(_)) {
            let tok = self.peek().cloned();
            return Err(self.error(tok, "Expected identifier".to_string()));
        }

        let ident = match self.next() {
            Some((_, Token::Ident(i), _)) => i,
            _ => unreachable!(),
        };

        let params = self.parse_fn_params(false)?;
        let rtype = self.parse_fn_return_type_annotation()?;
        let body = self.parse_block()?;

        Ok(Definition::Function {
            name: ident,
            params,
            body,
            rtype,
        })
    }

    fn parse_fn_params(&mut self, closure: bool) -> ParseResult<Vec<Variable>> {
        let mut xs = Vec::new();

        if closure {
            self.consume(Token::Pipe)?;
        } else {
            self.consume(Token::LeftParen)?;
        }

        if !tok_matches!(self, Token::RightParen) {
            loop {
                let e = self.parse_primary()?;
                let vtype: Type = self.parse_type_annotation()?;
                let v = match e {
                    Expression::Var(id) => Variable::new(id, vtype),
                    _ => {
                        return Err(self.error(
                            self.prev_token.clone(),
                            "Function parameter must be an identifier".to_string(),
                        ))
                    }
                };
                xs.push(v);

                if tok_matches!(self, Token::Comma) {
                    self.next();
                } else {
                    break;
                }
            }
        }

        if closure {
            self.consume(Token::Pipe)?;
        } else {
            self.consume(Token::RightParen)?;
        }

        Ok(xs)
    }

    /// Parse a statement
    fn parse_stmt(&mut self) -> ParseResult<Statement> {
        let Some(tok) = self.peek() else {
            return Err(ParseError::Eof);
        };

        match tok.1 {
            Token::LeftBrace => self.parse_block(),
            Token::Let => self.parse_let(),
            Token::Return => self.parse_return(),
            Token::If => self.parse_if_stmt(),
            Token::While => self.parse_while(),
            _ => self.parse_stmt_expr(),
        }
    }

    /// Parse a statement-form expression
    fn parse_stmt_expr(&mut self) -> ParseResult<Statement> {
        let e = self.parse_expr()?;
        Ok(Statement::Expr(e))
    }

    /// Parse an expression
    fn parse_expr(&mut self) -> ParseResult<Expression> {
        let Some(tok) = self.peek() else {
            return Err(ParseError::Eof);
        };

        match tok.1 {
            Token::If => self.parse_if_expr(),
            _ => self.parse_assignment(),
        }
    }

    /// Parse a sequence of expressions,
    /// defined by a separator, opening, and closing token
    fn parse_expr_seq(
        &mut self,
        open: Token,
        sep: Token,
        close: Token,
    ) -> ParseResult<Vec<Expression>> {
        let mut xs = Vec::new();
        self.consume(open.clone())?;
        if !self.matches(vec![close.clone()]) {
            loop {
                let e = self.parse_expr()?;
                xs.push(e);
                match self.peek() {
                    Some((_, tt, _)) if *tt == sep => {
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
        self.consume(Token::Let)?;

        let ident = self.parse_primary()?;

        match ident {
            Expression::Var(id) => {
                let ttype: Type = self.parse_type_annotation()?;

                self.consume(Token::Equal)?;
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
        self.consume(Token::LeftBrace)?;

        while !tok_matches!(self, Token::RightBrace) {
            let stmt = self.parse_stmt()?;
            if tok_matches!(self, Token::Semicolon) {
                self.consume(Token::Semicolon)?;
                stmts.push(stmt);
            } else {
                match stmt {
                    Statement::Expr(e) => {
                        tail = Some(e);
                        break;
                    }
                    Statement::If(_, _) => {
                        stmts.push(stmt);
                        continue;
                    }
                    Statement::While(_, _) => {
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

        self.consume(Token::RightBrace)?;

        Ok(Statement::Block(stmts, tail))
    }

    fn parse_return(&mut self) -> ParseResult<Statement> {
        self.consume(Token::Return)?;
        let res = if tok_matches!(self, Token::Semicolon) {
            Statement::Return(None)
        } else {
            Statement::Return(Some(self.parse_expr()?))
        };
        Ok(res)
    }

    fn parse_if_stmt(&mut self) -> ParseResult<Statement> {
        self.consume(Token::If)?;
        let e = self.parse_expr()?;
        let s1 = self.parse_block()?;

        if tok_matches!(self, Token::Else) {
            self.consume(Token::Else)?;
            let s2 = self.parse_block()?;
            return Ok(Statement::Expr(Expression::If(
                Box::new(e),
                Box::new(s1),
                Box::new(s2),
            )));
        }

        Ok(Statement::If(e, Box::new(s1)))
    }

    fn parse_while(&mut self) -> ParseResult<Statement> {
        self.consume(Token::While)?;
        let e = self.parse_expr()?;
        let s1 = self.parse_block()?;
        Ok(Statement::While(e, Box::new(s1)))
    }

    fn parse_if_expr(&mut self) -> ParseResult<Expression> {
        self.consume(Token::If)?;
        let e1 = self.parse_expr()?;
        let e2 = self.parse_block()?;
        self.consume(Token::Else)?;
        let e3 = self.parse_block()?;

        Ok(Expression::If(Box::new(e1), Box::new(e2), Box::new(e3)))
    }

    fn parse_assignment(&mut self) -> ParseResult<Expression> {
        let expr = self.parse_equality()?;

        if tok_matches!(self, Token::Equal) {
            self.consume(Token::Equal)?;
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
        let mut expr = self.parse_logical_or()?;

        while self.matches(vec![Token::EqualEqual, Token::BangEqual]) {
            let op = self.next().unwrap();
            let right = self.parse_logical_or()?;
            expr = Expression::BinOp(Box::new(expr), op.1.try_into().unwrap(), Box::new(right))
        }

        Ok(expr)
    }

    fn parse_logical_or(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_logical_and()?;

        while tok_matches!(self, Token::Or) {
            let op = self.next().unwrap();
            let right = self.parse_logical_and()?;
            expr = Expression::BinOp(Box::new(expr), op.1.try_into().unwrap(), Box::new(right))
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_comparison()?;

        while tok_matches!(self, Token::And) {
            let op = self.next().unwrap();
            let right = self.parse_comparison()?;
            expr = Expression::BinOp(Box::new(expr), op.1.try_into().unwrap(), Box::new(right))
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_term()?;

        while tok_matches!(
            self,
            Token::GreaterThan | Token::GreaterEqual | Token::LessThan | Token::LessEqual
        ) {
            let op = self.next().unwrap();
            let right = self.parse_term()?;
            expr = Expression::BinOp(Box::new(expr), op.1.try_into().unwrap(), Box::new(right))
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_factor()?;

        while tok_matches!(self, Token::Plus | Token::Minus) {
            let op = self.next().unwrap();
            let right = self.parse_factor()?;
            expr = Expression::BinOp(Box::new(expr), op.1.try_into().unwrap(), Box::new(right))
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_unary()?;

        // TODO: include src spans
        while tok_matches!(self, Token::Mult | Token::Div) {
            let op = self.next().unwrap();
            let right = self.parse_unary()?;
            expr = Expression::BinOp(Box::new(expr), op.1.try_into().unwrap(), Box::new(right))
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParseResult<Expression> {
        if tok_matches!(self, Token::Bang | Token::Minus) {
            let op = self.next().unwrap();
            let right = self.parse_unary()?;
            return Ok(Expression::Unary(op.1.try_into().unwrap(), Box::new(right)));
        }

        self.parse_call()
    }

    fn parse_call(&mut self) -> ParseResult<Expression> {
        let mut expr = self.parse_primary()?;
        while tok_matches!(self, Token::LeftParen) {
            let es = self.parse_expr_seq(Token::LeftParen, Token::Comma, Token::RightParen)?;

            if es.len() > 255 {
                return Err(self.error(None, "Number of arguments exceeds 255".to_string()));
            }

            expr = Expression::Call(Box::new(expr), es);
        }

        return Ok(expr);
    }

    fn parse_primary(&mut self) -> ParseResult<Expression> {
        let next = self.peek().cloned();

        if let Some(next_tok) = next {
            match &next_tok.1 {
                Token::Int(i) => {
                    self.next();
                    Ok(Expression::Int(*i))
                }
                Token::Float(f) => {
                    self.next();
                    Ok(Expression::Float(*f))
                }
                Token::String(s) => {
                    self.next();
                    Ok(Expression::String(s.clone()))
                }
                Token::Ident(var) => {
                    self.next();
                    match var.as_str() {
                        "true" => Ok(Expression::True),
                        "false" => Ok(Expression::False),
                        _ => Ok(Expression::Var(var.clone())),
                    }
                }
                Token::LeftParen => {
                    self.next();
                    let expr = self.parse_expr()?;
                    self.consume(Token::RightParen)?;
                    Ok(Expression::Grouping(Box::new(expr)))
                }
                Token::Pipe => {
                    let params = self.parse_fn_params(true)?;
                    let rt = self.parse_fn_return_type_annotation()?;
                    let body = Box::new(self.parse_stmt()?);
                    Ok(Expression::Closure(params, rt, body))
                }
                _ => Err(self.error(Some(next_tok.clone()), "Unexpected token".to_string())),
            }
        } else {
            Err(ParseError::Eof)
        }
    }

    fn parse_type_annotation(&mut self) -> ParseResult<Type> {
        if tok_matches!(self, Token::Colon) {
            self.consume(Token::Colon)?;
            let type_expr = self.parse_primary()?;
            match type_expr {
                Expression::Var(v) => Ok(v.to_string().try_into().unwrap()),
                _ => return Err(self.error(self.prev_token.clone(), "type must be ".to_string())),
            }
        } else {
            Ok(Type::Any)
        }
    }

    fn parse_fn_return_type_annotation(&mut self) -> ParseResult<Type> {
        if tok_matches!(self, Token::RightArrow) {
            self.consume(Token::RightArrow)?;
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

    fn consume(&mut self, t: Token) -> ParseResult<SrcSpan> {
        if self.matches(vec![t.clone()]) {
            return Ok(self.next().unwrap());
        }
        let tok = self.peek().unwrap().clone();
        Err(self.error(Some(tok.clone()), format!("expected '{:?}'", t)))
    }

    fn matches(&mut self, ts: Vec<Token>) -> bool {
        self.peek().is_some_and(|a| ts.contains(&a.1))
    }

    fn peek(&mut self) -> Option<&SrcSpan> {
        self.tokens.peek()
    }

    fn next(&mut self) -> Option<SrcSpan> {
        self.position += 1;
        self.prev_token = self.peek().map(|t| t.clone());
        self.tokens.next()
    }

    fn error(&mut self, tok: Option<SrcSpan>, msg: String) -> ParseError {
        match tok {
            Some((start, Token::Eof, _)) => ParseError::InvalidToken {
                line: start.line,
                col: start.col,
                message: msg,
            },
            Some((start, _, _)) => ParseError::InvalidToken {
                line: start.line,
                col: start.col,
                message: msg,
            },
            None => {
                println!("[unknown error]: {}", msg);
                ParseError::Unknown
            }
        }
    }
}
