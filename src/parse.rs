use crate::{
    ast::{
        types::Type, Arm, Definition, Expr, Expression, FieldInit, Location, Pattern, PatternKind,
        Program, Span, Statement, Variable, VariantDef,
    },
    diagnostic::Diagnostic,
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
        match result.kind {
            Expression::Int(42) => {}
            _ => panic!("Expected Expression::Int(42), got {:?}", result),
        }
    }

    #[test]
    fn test_parse_float_literal() {
        let mut parser = create_parser("3.14");
        let result = parser.parse_expr_only().unwrap();
        match result.kind {
            Expression::Float(f) => assert!((f - 3.14).abs() < 0.001),
            _ => panic!("Expected Expression::Float(3.14), got {:?}", result),
        }
    }

    #[test]
    fn test_parse_string_literal() {
        let mut parser = create_parser("\"hello world\"");
        let result = parser.parse_expr_only().unwrap();
        match result.kind {
            Expression::String(s) => assert_eq!(s, "hello world"),
            _ => panic!("Expected Expression::String, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_boolean_literals() {
        let mut parser_true = create_parser("true");
        let result_true = parser_true.parse_expr_only().unwrap();
        match result_true.kind {
            Expression::True => {}
            _ => panic!("Expected Expression::True, got {:?}", result_true),
        }

        let mut parser_false = create_parser("false");
        let result_false = parser_false.parse_expr_only().unwrap();
        match result_false.kind {
            Expression::False => {}
            _ => panic!("Expected Expression::False, got {:?}", result_false),
        }
    }

    #[test]
    fn test_parse_variable() {
        let mut parser = create_parser("x");
        let result = parser.parse_expr_only().unwrap();
        match result.kind {
            Expression::Var(name) => assert_eq!(name, "x"),
            _ => panic!("Expected Expression::Var, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_grouping() {
        let mut parser = create_parser("(42)");
        let result = parser.parse_expr_only().unwrap();
        match result.kind {
            Expression::Grouping(expr) => match expr.kind {
                Expression::Int(42) => {}
                _ => panic!("Expected Expression::Int(42) inside grouping"),
            },
            _ => panic!("Expected Expression::Grouping, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_unary_operations() {
        // Test negation - the parser folds -42 into a single negative integer literal
        let mut parser = create_parser("-42");
        let result = parser.parse_expr_only().unwrap();
        match result.kind {
            Expression::Int(-42) => {}
            _ => panic!("Expected Expression::Int(-42), got {:?}", result),
        }

        // Test logical not
        let mut parser = create_parser("!true");
        let result = parser.parse_expr_only().unwrap();
        match result.kind {
            Expression::Unary(op, expr) => {
                match op {
                    crate::ast::Op1::Not => {}
                    _ => panic!("Expected Not operator"),
                }
                match expr.kind {
                    Expression::True => {}
                    _ => panic!("Expected Expression::True"),
                }
            }
            _ => panic!("Expected Expression::Unary, got {:?}", result),
        }

        // Test unary minus on a variable (this should create a unary operation)
        let mut parser = create_parser("-x");
        let result = parser.parse_expr_only().unwrap();
        match result.kind {
            Expression::Unary(op, expr) => {
                match op {
                    crate::ast::Op1::Negate => {}
                    _ => panic!("Expected Negate operator"),
                }
                match expr.kind {
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
        match result.kind {
            Expression::BinOp(left, op, right) => {
                match op {
                    crate::ast::Op2::Addition => {}
                    _ => panic!("Expected Addition operator"),
                }
                match left.kind {
                    Expression::Int(1) => {}
                    _ => panic!("Expected left operand to be 1"),
                }
                match right.kind {
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
        match result.kind {
            Expression::Assign(name, value) => {
                assert_eq!(name, "x");
                match value.kind {
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
        match result.kind {
            Expression::Call(func, args) => {
                match func.kind {
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
                assert_eq!(var.t, Some(Type::Int));
                match value.kind {
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
            Statement::Return(Some(value)) => match value.kind {
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
                ..
            } => {
                assert_eq!(name, "add");
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].name, "x");
                assert_eq!(params[0].t, Some(Type::Int));
                assert_eq!(params[1].name, "y");
                assert_eq!(params[1].t, Some(Type::Int));
                assert_eq!(rtype, &Some(Type::Int));
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
    fn test_parse_reports_bad_input_without_panicking() {
        // `any` is not a type
        assert!(create_parser("fn main() { let x: any = 1; x }").parse().is_err());
        // Trailing tokens after the last definition
        assert!(create_parser("fn main() { 1 } 42").parse().is_err());
        // Input that ends mid-definition
        assert!(create_parser("fn main(").parse().is_err());
    }

    #[test]
    fn test_parse_structs_enums_and_match() {
        let src = "struct P { x: int, y: int, }
                   enum E { A, B(int, P), }
                   fn main() -> int {
                       let p = P { x: 1, y: 2 };
                       let q = P { x: 3, ..p };
                       match E::B(p.x, q) {
                           E::A => 0,
                           E::B(n, P { x, .. }) if n > 0 => { n + x }
                           _ => -1
                       }
                   }";
        let program = create_parser(src).parse().unwrap();
        assert_eq!(program.definitions.len(), 3);
        match &program.definitions[1] {
            Definition::Enum { name, variants, .. } => {
                assert_eq!(name, "E");
                assert_eq!(variants[1].fields.len(), 2);
            }
            d => panic!("expected an enum, got {d:?}"),
        }
    }

    #[test]
    fn test_struct_literals_need_parens_in_conditions() {
        // `p == P` is the condition, and `{ x: 1 }` is a (malformed) body.
        let err = create_parser("fn main() { if p == P { x: 1 } { } }").parse().unwrap_err();
        assert!(err.help.unwrap().contains("(P { ... })"));
        assert!(create_parser("fn main() { if p == (P { x: 1 }) { } }").parse().is_ok());
        // Plain variables before a block still work.
        assert!(create_parser("fn main() { while ok { } match v { _ => 1 } }").parse().is_ok());
    }

    #[test]
    fn test_fields_cannot_be_assigned() {
        let err = create_parser("p.x = 1").parse_expr_only().unwrap_err();
        assert_eq!(err.message, "cannot assign to a field");
    }

    #[test]
    fn test_lexer_reports_errors() {
        assert!(crate::lexer::tokenize("let x = @;").is_err());
        assert!(crate::lexer::tokenize("\"unterminated").is_err());
        assert!(crate::lexer::tokenize("99999999999").is_err());
    }

    #[test]
    fn test_parse_empty_program() {
        let mut parser = create_parser("");
        let result = parser.parse().unwrap();
        assert_eq!(result.definitions.len(), 0);
    }

    fn parse_closure(src: &str) -> (Vec<crate::ast::Variable>, Option<Type>) {
        match create_parser(src).parse_expr_only().unwrap().kind {
            Expression::Closure(params, rt, body) => {
                assert!(matches!(*body, Statement::Block(..)), "body: {:?}", body);
                (params, rt)
            }
            other => panic!("expected a closure, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_closures() {
        let (ps, rt) = parse_closure("fn() { 1 }");
        assert!(ps.is_empty());
        assert_eq!(rt, None);

        let (ps, _) = parse_closure("fn(x) { x * 2 }");
        assert_eq!(ps.len(), 1);
        assert_eq!(ps[0].t, None);

        let (ps, rt) = parse_closure("fn(a: int, b: int) -> int { a + b }");
        assert_eq!(ps.len(), 2);
        assert_eq!(ps[1].t, Some(Type::Int));
        assert_eq!(rt, Some(Type::Int));
    }

    #[test]
    fn test_parse_function_types() {
        let (ps, rt) = parse_closure("fn(f: fn(int, str) -> bool, g: fn()) -> fn(int) -> int { f }");
        let fn_t = |ps: Vec<Type>, r: Type| Type::Fn {
            param_types: ps,
            ret_type: Box::new(r),
        };
        assert_eq!(ps[0].t, Some(fn_t(vec![Type::Int, Type::String], Type::Bool)));
        assert_eq!(ps[1].t, Some(fn_t(vec![], Type::Void)));
        assert_eq!(rt, Some(fn_t(vec![Type::Int], Type::Int)));
        let err = create_parser("fn main() { let x: any = 1; x }").parse().unwrap_err();
        assert!(err.help.unwrap().contains("no 'any' type"));
    }

    #[test]
    fn test_parse_closure_errors_have_help() {
        let help = |src: &str| create_parser(src).parse_expr_only().unwrap_err().help;
        // Old pipe syntax, with and without parameters
        assert!(help("|x| x + 1").unwrap().contains("fn(x)"));
        assert!(help("|| 1").unwrap().contains("fn(x)"));
        // Body must be a block
        assert!(help("fn(x) x + 1").unwrap().contains("always a block"));
        // Named function in expression position
        assert!(help("fn foo() { 1 }").unwrap().contains("top level"));
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
pub struct ParseError {
    pub start: Location,
    pub end: Location,
    pub message: String,
    pub help: Option<String>,
}

impl ParseError {
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

impl std::error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}: {}", self.start.line, self.start.col, self.message)
    }
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<T: Iterator<Item = SrcSpan>> {
    tokens: Peekable<T>,
    prev_token: Option<SrcSpan>,
    position: u32,
    /// Set while parsing the head of an `if`, `while`, or `match`, where
    /// `name {` starts the body rather than a struct literal (as in Rust).
    /// Parentheses, blocks, and argument lists allow them again.
    no_struct: bool,
    /// A name followed by `{` in such a head, which might have been meant
    /// as a struct literal, for a hint if the body then fails to parse.
    struct_hint: Option<crate::ast::Id>,
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
            no_struct: false,
            struct_hint: None,
        }
    }

    pub fn parse(&mut self) -> ParseResult<Program> {
        let mut ds = Vec::new();
        while tok_matches!(self, Token::Fn | Token::Struct | Token::Enum) {
            let d = self.parse_defn()?;
            ds.push(d);
        }

        if !tok_matches!(self, Token::Eof) && self.peek().is_some() {
            return Err(self
                .expected("a definition")
                .help("only 'fn', 'struct', and 'enum' definitions are allowed at the top level of a file"));
        }

        Ok(Program { definitions: ds })
    }

    pub fn parse_expr_only(&mut self) -> ParseResult<Expr> {
        self.parse_expr()
    }

    pub fn parse_stmt_only(&mut self) -> ParseResult<Statement> {
        self.parse_stmt()
    }

    fn parse_defn(&mut self) -> ParseResult<Definition> {
        let res = match self.next() {
            Some((_, Token::Fn, _)) => self.parse_fn_defn(),
            Some((_, Token::Struct, _)) => self.parse_struct_defn(),
            Some((_, Token::Enum, _)) => self.parse_enum_defn(),
            tok => Err(self.error(tok, "expected a definition".to_string())),
        };

        res
    }

    fn parse_fn_defn(&mut self) -> ParseResult<Definition> {
        if !tok_matches!(self, Token::Ident(_)) {
            return Err(self.expected("a function name after 'fn'"));
        }

        let (ident, name_span) = match self.next() {
            Some((start, Token::Ident(i), end)) => (i, Span::new(start, end)),
            _ => unreachable!(),
        };

        let params = self.parse_fn_params()?;
        let rtype = self.parse_fn_return_type_annotation()?;
        let body = self.parse_block()?;

        Ok(Definition::Function {
            name: ident,
            name_span,
            params,
            body,
            rtype,
        })
    }

    /// The name after `struct` or `enum`.
    fn parse_type_def_name(&mut self, keyword: &str) -> ParseResult<(crate::ast::Id, Span)> {
        match self.peek().cloned() {
            Some((start, Token::Ident(name), end)) => {
                self.next();
                Ok((name, Span::new(start, end)))
            }
            _ => Err(self.expected(&format!("a name after '{keyword}'"))),
        }
    }

    /// `struct Name { field: type, ... }`
    fn parse_struct_defn(&mut self) -> ParseResult<Definition> {
        let (name, name_span) = self.parse_type_def_name("struct")?;
        if tok_matches!(self, Token::LeftParen) {
            return Err(self
                .expected("'{'")
                .help(format!("struct fields have names: `struct {name} {{ x: int, y: int }}`")));
        }
        self.consume(Token::LeftBrace)?;
        let mut fields = Vec::new();
        while !tok_matches!(self, Token::RightBrace) {
            let (field, span) = match self.peek().cloned() {
                Some((start, Token::Ident(f), end)) => (f, Span::new(start, end)),
                _ => return Err(self.expected("a field name")),
            };
            self.next();
            if !tok_matches!(self, Token::Colon) {
                return Err(self
                    .expected("':'")
                    .help(format!("every field needs a type: `{field}: int`")));
            }
            let t = self.parse_type_annotation()?;
            fields.push(Variable::new(field, t, span));
            if tok_matches!(self, Token::Comma) {
                self.next();
            } else {
                break;
            }
        }
        if !tok_matches!(self, Token::RightBrace) {
            return Err(self.expected("',' or '}'"));
        }
        self.consume(Token::RightBrace)?;
        Ok(Definition::Struct { name, name_span, fields })
    }

    /// `enum Name { A, B(int, str), ... }`
    fn parse_enum_defn(&mut self) -> ParseResult<Definition> {
        let (name, name_span) = self.parse_type_def_name("enum")?;
        self.consume(Token::LeftBrace)?;
        let mut variants = Vec::new();
        while !tok_matches!(self, Token::RightBrace) {
            let (variant, span) = match self.peek().cloned() {
                Some((start, Token::Ident(v), end)) => (v, Span::new(start, end)),
                _ => return Err(self.expected("a variant name")),
            };
            self.next();
            let mut fields = Vec::new();
            if tok_matches!(self, Token::LeftParen) {
                self.next();
                if !tok_matches!(self, Token::RightParen) {
                    loop {
                        fields.push(self.parse_type_name()?);
                        if tok_matches!(self, Token::Comma) {
                            self.next();
                        } else {
                            break;
                        }
                    }
                }
                self.consume(Token::RightParen)?;
            } else if tok_matches!(self, Token::LeftBrace) {
                return Err(self.expected("',' or '}'").help(format!(
                    "a variant's values are positional: `{variant}(int, str)`; \
                     for named fields, define a struct and hold it: `{variant}({variant}Info)`"
                )));
            }
            variants.push(VariantDef { name: variant, span, fields });
            if tok_matches!(self, Token::Comma) {
                self.next();
            } else {
                break;
            }
        }
        if !tok_matches!(self, Token::RightBrace) {
            return Err(self.expected("',' or '}'"));
        }
        self.consume(Token::RightBrace)?;
        Ok(Definition::Enum { name, name_span, variants })
    }

    /// Parse with struct literals allowed (`true`) or not, restoring the
    /// previous setting afterwards.
    fn with_structs<R>(&mut self, allowed: bool, f: impl FnOnce(&mut Self) -> R) -> R {
        let saved = self.no_struct;
        self.no_struct = !allowed;
        let r = f(self);
        self.no_struct = saved;
        r
    }

    /// The condition of an `if` or `while`, or the value a `match` matches on.
    fn parse_head(&mut self) -> ParseResult<Expr> {
        self.struct_hint = None;
        self.with_structs(false, |p| p.parse_expr())
    }

    /// The block after the head of an `if` or `while`.
    fn parse_body(&mut self) -> ParseResult<Statement> {
        let hint = self.struct_hint.take();
        let body = self.parse_block();
        Self::with_struct_hint(hint, body)
    }

    /// Add a hint to an error in what follows a head that might have
    /// contained a struct literal.
    fn with_struct_hint<R>(hint: Option<crate::ast::Id>, result: ParseResult<R>) -> ParseResult<R> {
        result.map_err(|e| match hint {
            Some(name) => e.help(format!(
                "a struct literal can't appear here without parentheses: `({name} {{ ... }})`"
            )),
            None => e,
        })
    }

    fn parse_fn_params(&mut self) -> ParseResult<Vec<Variable>> {
        let mut xs = Vec::new();

        self.consume(Token::LeftParen)?;

        if !tok_matches!(self, Token::RightParen) {
            loop {
                let (name, span) = match self.peek() {
                    Some((start, Token::Ident(id), end)) => (id.clone(), Span::new(*start, *end)),
                    _ => return Err(self.expected("a parameter name")),
                };
                self.next();
                let vtype = self.parse_type_annotation()?;
                xs.push(Variable::new(name, vtype, span));

                if tok_matches!(self, Token::Comma) {
                    self.next();
                } else {
                    break;
                }
            }
        }

        self.consume(Token::RightParen)?;

        Ok(xs)
    }

    /// Parse a statement
    fn parse_stmt(&mut self) -> ParseResult<Statement> {
        let Some(tok) = self.peek() else {
            return Err(self.expected("a statement"));
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
    fn parse_expr(&mut self) -> ParseResult<Expr> {
        let Some(tok) = self.peek() else {
            return Err(self.expected("an expression"));
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
    ) -> ParseResult<Vec<Expr>> {
        let mut xs = Vec::new();
        self.consume(open.clone())?;
        if !self.matches(vec![close.clone()]) {
            loop {
                let e = self.with_structs(true, |p| p.parse_expr())?;
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

        let (id, span) = match self.peek() {
            Some((start, Token::Ident(id), end)) if id != "true" && id != "false" => {
                (id.clone(), Span::new(*start, *end))
            }
            _ => return Err(self.expected("a variable name after 'let'")),
        };
        self.next();

        let ttype = self.parse_type_annotation()?;
        if !tok_matches!(self, Token::Equal) {
            return Err(self
                .expected("'='")
                .help(format!("variables must be initialized: `let {id} = <value>;`")));
        }
        self.consume(Token::Equal)?;
        let e = self.parse_expr()?;
        Ok(Statement::Let {
            var: Variable::new(id, ttype, span),
            value: e,
        })
    }

    fn parse_block(&mut self) -> ParseResult<Statement> {
        self.with_structs(true, |p| p.parse_block_inner())
    }

    fn parse_block_inner(&mut self) -> ParseResult<Statement> {
        let mut stmts = Vec::new();
        let mut tail = None;
        self.consume(Token::LeftBrace)?;

        while !tok_matches!(self, Token::RightBrace) {
            let stmt = self.parse_stmt()?;
            if tok_matches!(self, Token::Semicolon) {
                self.consume(Token::Semicolon)?;
                stmts.push(stmt);
            } else {
                let at_end = tok_matches!(self, Token::RightBrace);
                match stmt {
                    // A block whose only content is a final expression is
                    // that expression.
                    Statement::Block(inner, Some(t)) if at_end && inner.is_empty() => {
                        tail = Some(t);
                    }
                    Statement::Expr(e) if at_end => tail = Some(e),
                    // Statements that end in a block need no `;` before the
                    // next statement.
                    Statement::Expr(Expr {
                        kind: Expression::If(..) | Expression::Match(..),
                        ..
                    })
                    | Statement::If(..)
                    | Statement::While(..)
                    | Statement::Block(..) => stmts.push(stmt),
                    _ => return Err(self.missing_semicolon()),
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
        let (start, _, _) = self.consume(Token::If)?;
        let e = self.parse_head()?;
        let s1 = self.parse_body()?;

        if tok_matches!(self, Token::Else) {
            self.consume(Token::Else)?;
            let s2 = self.parse_block()?;
            let span = Span::new(start, self.last_end());
            return Ok(Statement::Expr(Expr::new(
                Expression::If(Box::new(e), Box::new(s1), Box::new(s2)),
                span,
            )));
        }

        Ok(Statement::If(e, Box::new(s1)))
    }

    fn parse_while(&mut self) -> ParseResult<Statement> {
        self.consume(Token::While)?;
        let e = self.parse_head()?;
        let s1 = self.parse_body()?;
        Ok(Statement::While(e, Box::new(s1)))
    }

    fn parse_if_expr(&mut self) -> ParseResult<Expr> {
        let (start, _, _) = self.consume(Token::If)?;
        let e1 = self.parse_head()?;
        let e2 = self.parse_body()?;
        self.consume(Token::Else)?;
        let e3 = self.parse_block()?;

        Ok(Expr::new(
            Expression::If(Box::new(e1), Box::new(e2), Box::new(e3)),
            Span::new(start, self.last_end()),
        ))
    }

    fn parse_assignment(&mut self) -> ParseResult<Expr> {
        let expr = self.parse_equality()?;

        if tok_matches!(self, Token::Equal) {
            self.consume(Token::Equal)?;
            let equals = self.prev_token.clone();
            let value = self.parse_expr()?;

            match expr.kind {
                Expression::Var(v) => {
                    let span = expr.span.to(value.span);
                    return Ok(Expr::new(Expression::Assign(v, Box::new(value)), span));
                }
                Expression::Field(..) => {
                    return Err(self
                        .error(equals.clone(), "cannot assign to a field".to_string())
                        .help("struct fields can't be changed; make a new value instead, \
                               e.g. `p = Point { x: 1, ..p }`"))
                }
                _ => {
                    return Err(self
                        .error(equals.clone(), "invalid assignment target".to_string())
                        .help("only variables can be assigned to, e.g. `x = 1`"))
                }
            };
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_logical_or()?;

        while self.matches(vec![Token::EqualEqual, Token::BangEqual]) {
            let op = self.next().unwrap();
            let right = self.parse_logical_or()?;
            expr = Self::binop(expr, op.1, right)
        }

        Ok(expr)
    }

    fn parse_logical_or(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_logical_and()?;

        while tok_matches!(self, Token::Or) {
            let op = self.next().unwrap();
            let right = self.parse_logical_and()?;
            expr = Self::binop(expr, op.1, right)
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_comparison()?;

        while tok_matches!(self, Token::And) {
            let op = self.next().unwrap();
            let right = self.parse_comparison()?;
            expr = Self::binop(expr, op.1, right)
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_term()?;

        while tok_matches!(
            self,
            Token::GreaterThan | Token::GreaterEqual | Token::LessThan | Token::LessEqual
        ) {
            let op = self.next().unwrap();
            let right = self.parse_term()?;
            expr = Self::binop(expr, op.1, right)
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_factor()?;

        while tok_matches!(self, Token::Plus | Token::Minus) {
            let op = self.next().unwrap();
            let right = self.parse_factor()?;
            expr = Self::binop(expr, op.1, right)
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_unary()?;

        // TODO: include src spans
        while tok_matches!(self, Token::Mult | Token::Div | Token::Modulo) {
            let op = self.next().unwrap();
            let right = self.parse_unary()?;
            expr = Self::binop(expr, op.1, right)
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParseResult<Expr> {
        if tok_matches!(self, Token::Bang | Token::Minus) {
            let op = self.next().unwrap();
            let right = self.parse_unary()?;
            let span = Span::new(op.0, right.span.end);
            // Fold negated numeric literals so `-42` is a literal, not a unary op.
            let kind = match (&op.1, right.kind) {
                (Token::Minus, Expression::Int(i)) => Expression::Int(i.wrapping_neg()),
                (Token::Minus, Expression::Float(f)) => Expression::Float(-f),
                (_, kind) => Expression::Unary(
                    op.1.try_into().unwrap(),
                    Box::new(Expr::new(kind, right.span)),
                ),
            };
            return Ok(Expr::new(kind, span));
        }

        self.parse_call()
    }

    fn parse_call(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary()?;
        loop {
            if tok_matches!(self, Token::LeftParen) {
                let es = self.parse_expr_seq(Token::LeftParen, Token::Comma, Token::RightParen)?;

                if es.len() > 255 {
                    let tok = self.prev_token.clone();
                    return Err(self.error(tok, "a call cannot have more than 255 arguments".to_string()));
                }

                let span = Span::new(expr.span.start, self.last_end());
                expr = Expr::new(Expression::Call(Box::new(expr), es), span);
            } else if tok_matches!(self, Token::Dot) {
                self.next();
                let (name, name_span) = match self.peek().cloned() {
                    Some((start, Token::Ident(name), end)) => (name, Span::new(start, end)),
                    _ => return Err(self.expected("a field name after '.'")),
                };
                self.next();
                let span = Span::new(expr.span.start, name_span.end);
                expr = Expr::new(Expression::Field(Box::new(expr), name, name_span), span);
            } else {
                break;
            }
        }

        return Ok(expr);
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        let Some((start, tok, end)) = self.peek().cloned() else {
            return Err(self.expected("an expression"));
        };
        let span = Span::new(start, end);

        let kind = match tok {
            Token::Int(i) => {
                self.next();
                Expression::Int(i)
            }
            Token::Float(f) => {
                self.next();
                Expression::Float(f)
            }
            Token::String(s) => {
                self.next();
                Expression::String(s)
            }
            Token::Ident(var) => {
                self.next();
                match var.as_str() {
                    "true" => Expression::True,
                    "false" => Expression::False,
                    _ if tok_matches!(self, Token::DoubleColon) => {
                        self.next();
                        let variant = match self.peek().cloned() {
                            Some((_, Token::Ident(v), _)) => v,
                            _ => return Err(self.expected("a variant name after '::'")),
                        };
                        self.next();
                        Expression::Path(var, variant)
                    }
                    _ if tok_matches!(self, Token::LeftBrace) && !self.no_struct => {
                        self.parse_struct_lit(var)?
                    }
                    _ if tok_matches!(self, Token::LeftBrace) => {
                        self.struct_hint = Some(var.clone());
                        Expression::Var(var)
                    }
                    _ => Expression::Var(var),
                }
            }
            Token::LeftParen => {
                self.next();
                let expr = self.with_structs(true, |p| p.parse_expr())?;
                self.consume(Token::RightParen)?;
                Expression::Grouping(Box::new(expr))
            }
            Token::Match => {
                self.next();
                let scrutinee = self.parse_head()?;
                let hint = self.struct_hint.take();
                let arms = self.with_structs(true, |p| p.parse_arms());
                let arms = Self::with_struct_hint(hint, arms)?;
                Expression::Match(Box::new(scrutinee), arms)
            }
            Token::Fn => {
                self.next();
                if let Some((_, Token::Ident(name), _)) = self.peek().cloned() {
                    return Err(self.expected("'(' after 'fn'").help(format!(
                        "named functions can only be defined at the top level; \
                         to make a closure, write 'let {name} = fn(...) {{ ... }}'"
                    )));
                }
                let params = self.parse_fn_params()?;
                let rt = self.parse_fn_return_type_annotation()?;
                if !tok_matches!(self, Token::LeftBrace) {
                    return Err(self
                        .expected("'{' to start the function body")
                        .help("a closure's body is always a block, e.g. 'fn(x) { x + 1 }'"));
                }
                let body = Box::new(self.parse_block()?);
                Expression::Closure(params, rt, body)
            }
            Token::Pipe | Token::Or => {
                return Err(self
                    .expected("an expression")
                    .help("closures are written 'fn(x) { x + 1 }', not '|x| x + 1'"));
            }
            _ => return Err(self.expected("an expression")),
        };

        // Compound expressions extend to the last token they consumed.
        Ok(Expr::new(kind, Span::new(span.start, self.last_end())))
    }

    /// The fields of a struct literal, after its name.
    fn parse_struct_lit(&mut self, name: crate::ast::Id) -> ParseResult<Expression> {
        self.consume(Token::LeftBrace)?;
        let mut fields = Vec::new();
        let mut base = None;
        while !tok_matches!(self, Token::RightBrace) {
            if tok_matches!(self, Token::DotDot) {
                self.next();
                base = Some(Box::new(self.parse_expr()?));
                if !tok_matches!(self, Token::RightBrace) {
                    return Err(self
                        .expected("'}'")
                        .help("`..base` comes last in a struct literal: `Point { x: 1, ..p }`"));
                }
                break;
            }
            let (field, span) = match self.peek().cloned() {
                Some((start, Token::Ident(f), end)) => (f, Span::new(start, end)),
                _ => {
                    return Err(self
                        .expected("a field name")
                        .help(format!("struct literals name their fields: `{name} {{ x: 1, y: 2 }}`")))
                }
            };
            self.next();
            let value = if tok_matches!(self, Token::Colon) {
                self.next();
                self.parse_expr()?
            } else {
                // Shorthand: `Point { x }` means `Point { x: x }`.
                Expr::new(Expression::Var(field.clone()), span)
            };
            fields.push(FieldInit { name: field, span, value });
            if tok_matches!(self, Token::Comma) {
                self.next();
            } else if !tok_matches!(self, Token::RightBrace) {
                return Err(self.expected("',' or '}'"));
            }
        }
        self.consume(Token::RightBrace)?;
        Ok(Expression::StructLit { name, fields, base })
    }

    /// `{ pattern => body, ... }` after `match value`.
    fn parse_arms(&mut self) -> ParseResult<Vec<Arm>> {
        self.consume(Token::LeftBrace)?;
        let mut arms = Vec::new();
        while !tok_matches!(self, Token::RightBrace) {
            let pattern = self.parse_pattern()?;
            let guard = if tok_matches!(self, Token::If) {
                self.next();
                Some(self.parse_expr()?)
            } else {
                None
            };
            if !tok_matches!(self, Token::FatArrow) {
                let err = self.expected("'=>'");
                return Err(if guard.is_none() {
                    err.help("each arm is written `pattern => value`")
                } else {
                    err
                });
            }
            self.next();
            let (body, is_block) = if tok_matches!(self, Token::LeftBrace) {
                (self.parse_block()?, true)
            } else {
                (Statement::Expr(self.parse_expr()?), false)
            };
            arms.push(Arm { pattern, guard, body: Box::new(body) });
            if tok_matches!(self, Token::Comma) {
                self.next();
            } else if !is_block && !tok_matches!(self, Token::RightBrace) {
                return Err(self
                    .expected("',' or '}'")
                    .help("separate match arms with ','"));
            }
        }
        self.consume(Token::RightBrace)?;
        Ok(arms)
    }

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        let Some((start, tok, _)) = self.peek().cloned() else {
            return Err(self.expected("a pattern"));
        };
        let kind = match tok {
            Token::Int(i) => {
                self.next();
                PatternKind::Int(i)
            }
            Token::Minus => {
                self.next();
                match self.peek().cloned() {
                    Some((_, Token::Int(i), _)) => {
                        self.next();
                        PatternKind::Int(i.wrapping_neg())
                    }
                    _ => return Err(self.expected("an integer after '-' in a pattern")),
                }
            }
            Token::String(s) => {
                self.next();
                PatternKind::Str(s)
            }
            Token::Float(_) => {
                let tok = self.peek().cloned();
                return Err(self
                    .error(tok, "float literals can't be used as patterns".to_string())
                    .help("bind the value and compare it in a guard: `x if x < 0.5 => ...`"));
            }
            Token::Ident(name) => {
                let name_end = self.peek().unwrap().2;
                self.next();
                match name.as_str() {
                    "_" => PatternKind::Wildcard,
                    "true" => PatternKind::Bool(true),
                    "false" => PatternKind::Bool(false),
                    _ if tok_matches!(self, Token::DoubleColon) => {
                        self.next();
                        let variant = match self.peek().cloned() {
                            Some((_, Token::Ident(v), _)) => v,
                            _ => return Err(self.expected("a variant name after '::'")),
                        };
                        self.next();
                        let mut fields = Vec::new();
                        let parens = tok_matches!(self, Token::LeftParen);
                        if parens {
                            self.next();
                            while !tok_matches!(self, Token::RightParen) {
                                if tok_matches!(self, Token::DotDot) {
                                    return Err(self.expected("a pattern").help(
                                        "a variant's values are matched by position; write `_` for each one to ignore",
                                    ));
                                }
                                fields.push(self.parse_pattern()?);
                                if tok_matches!(self, Token::Comma) {
                                    self.next();
                                } else {
                                    break;
                                }
                            }
                            self.consume(Token::RightParen)?;
                        }
                        PatternKind::Variant { enum_name: name, variant, fields, parens }
                    }
                    _ if tok_matches!(self, Token::LeftBrace) => {
                        self.next();
                        let mut fields = Vec::new();
                        let mut rest = false;
                        while !tok_matches!(self, Token::RightBrace) {
                            if tok_matches!(self, Token::DotDot) {
                                self.next();
                                rest = true;
                                break;
                            }
                            let (field, span) = match self.peek().cloned() {
                                Some((s, Token::Ident(f), e)) => (f, Span::new(s, e)),
                                _ => return Err(self.expected("a field name")),
                            };
                            self.next();
                            let pattern = if tok_matches!(self, Token::Colon) {
                                self.next();
                                self.parse_pattern()?
                            } else {
                                // Shorthand: `Point { x }` binds the field to `x`.
                                Pattern {
                                    kind: PatternKind::Binding(Variable::new(field.clone(), None, span)),
                                    span,
                                }
                            };
                            fields.push((field, span, pattern));
                            if tok_matches!(self, Token::Comma) {
                                self.next();
                            } else {
                                break;
                            }
                        }
                        if !tok_matches!(self, Token::RightBrace) {
                            return Err(self.expected("',' or '}'"));
                        }
                        self.next();
                        PatternKind::Struct { name, fields, rest }
                    }
                    _ => PatternKind::Binding(Variable::new(name, None, Span::new(start, name_end))),
                }
            }
            _ => return Err(self.expected("a pattern")),
        };
        Ok(Pattern { kind, span: Span::new(start, self.last_end()) })
    }

    fn parse_type_annotation(&mut self) -> ParseResult<Option<Type>> {
        if tok_matches!(self, Token::Colon) {
            self.consume(Token::Colon)?;
            Ok(Some(self.parse_type_name()?))
        } else {
            Ok(None)
        }
    }

    fn parse_fn_return_type_annotation(&mut self) -> ParseResult<Option<Type>> {
        if tok_matches!(self, Token::RightArrow) {
            self.consume(Token::RightArrow)?;
            Ok(Some(self.parse_type_name()?))
        } else {
            Ok(None)
        }
    }

    /// Parse a type such as `int`, `str`, or `fn(int, bool) -> str`.
    fn parse_type_name(&mut self) -> ParseResult<Type> {
        let tok = self.peek().cloned();
        match tok {
            Some((_, Token::Fn, _)) => {
                self.next();
                self.consume(Token::LeftParen)?;
                let mut param_types = Vec::new();
                if !tok_matches!(self, Token::RightParen) {
                    loop {
                        param_types.push(self.parse_type_name()?);
                        if tok_matches!(self, Token::Comma) {
                            self.next();
                        } else {
                            break;
                        }
                    }
                }
                self.consume(Token::RightParen)?;
                let ret_type = self.parse_fn_return_type_annotation()?.unwrap_or(Type::Void);
                Ok(Type::Fn {
                    param_types,
                    ret_type: Box::new(ret_type),
                })
            }
            Some((start, Token::Ident(ref v), end)) => {
                let v = v.clone();
                self.next();
                if v == "any" {
                    return Err(self.error(tok, "unknown type 'any'".to_string()).help(
                        "Aja has no 'any' type; every value has one static type. \
                         Remove the annotation to have it inferred",
                    ));
                }
                // Anything else that isn't built in names a struct or enum,
                // which the type checker looks up.
                Ok(Type::try_from(v.to_string()).unwrap_or(Type::Named(v, Span::new(start, end))))
            }
            _ => Err(self.expected("a type")),
        }
    }

    fn consume(&mut self, t: Token) -> ParseResult<SrcSpan> {
        if self.matches(vec![t.clone()]) {
            return Ok(self.next().unwrap());
        }
        if t == Token::Semicolon {
            return Err(self.missing_semicolon());
        }
        Err(self.expected(&t.to_string()))
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

    fn binop(left: Expr, op: Token, right: Expr) -> Expr {
        let span = left.span.to(right.span);
        Expr::new(
            Expression::BinOp(Box::new(left), op.try_into().unwrap(), Box::new(right)),
            span,
        )
    }

    /// Where the most recently consumed token ends.
    fn last_end(&self) -> Location {
        self.prev_token
            .as_ref()
            .map_or(Location::new(0, 1, 1), |(_, _, end)| *end)
    }

    /// An error at `tok` (or, if absent, just after the last consumed token).
    fn error(&mut self, tok: Option<SrcSpan>, msg: String) -> ParseError {
        let (start, end) = match tok.or_else(|| self.prev_token.clone()) {
            Some((start, _, end)) => (start, end),
            None => {
                let loc = Location::new(0, 1, 1);
                (loc, loc)
            }
        };
        ParseError {
            start,
            end,
            message: msg,
            help: None,
        }
    }

    /// "expected <what>, found <next token>", pointing at the next token.
    fn expected(&mut self, what: &str) -> ParseError {
        let tok = self.peek().cloned();
        let found = match &tok {
            Some((_, t, _)) => t.to_string(),
            None => Token::Eof.to_string(),
        };
        self.error(tok, format!("expected {what}, found {found}"))
    }

    /// Error for a statement that isn't followed by ';', pointing just after it.
    fn missing_semicolon(&mut self) -> ParseError {
        let found = match self.peek() {
            Some((_, t, _)) => t.to_string(),
            None => Token::Eof.to_string(),
        };
        let mut err = self.error(None, format!("expected ';', found {found}"));
        err.start = err.end;
        err.end.col += 1;
        err.help("add ';' at the end of the statement")
    }
}
