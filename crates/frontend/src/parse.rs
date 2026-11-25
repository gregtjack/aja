use crate::{
    ast::{
        types::Type, Arm, Definition, Expr, Expression, FieldDef, FieldInit, Id, Import, Location,
        Method, Pattern, PatternKind, Program, Span, Statement, TemplatePart, TypeParam, Variable,
        VariantDef,
    },
    diagnostic::Diagnostic,
    lexer::SrcSpan,
    prelude,
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

    /// An expression with its grouping shown by parentheses.
    fn grouped(src: &str) -> String {
        fn show(e: &Expr) -> String {
            match &e.kind {
                Expression::BinOp(l, op, r) => format!("({} {op} {})", show(l), show(r)),
                Expression::Unary(crate::ast::Op1::Not, x) => format!("!{}", show(x)),
                Expression::Unary(crate::ast::Op1::Negate, x) => format!("-{}", show(x)),
                Expression::Var(v) => v.to_string(),
                Expression::Int(i) => i.to_string(),
                Expression::Call(callee, args) => {
                    let args: Vec<String> = args.iter().map(show).collect();
                    match &callee.kind {
                        Expression::Path(t, f) => format!("{t}::{f}({})", args.join(", ")),
                        _ => format!("{}({})", show(callee), args.join(", ")),
                    }
                }
                other => panic!("unexpected {other:?}"),
            }
        }
        show(&create_parser(src).parse_expr_only().unwrap())
    }

    #[test]
    fn test_operator_precedence() {
        // Comparisons bind tighter than `&&` and `||`, as in C and Rust.
        assert_eq!(grouped("a == b && c != d"), "((a == b) && (c != d))");
        assert_eq!(grouped("a || b && c == d"), "(a || (b && (c == d)))");
        assert_eq!(grouped("a == b || c < d"), "((a == b) || (c < d))");
        assert_eq!(grouped("a < b == c > d"), "((a < b) == (c > d))");
        assert_eq!(grouped("a + b * c == d - e"), "((a + (b * c)) == (d - e))");
        assert_eq!(grouped("!a == b && -c < d"), "((!a == b) && (-c < d))");
        // Each level groups from the left.
        assert_eq!(grouped("a - b - c"), "((a - b) - c)");
        assert_eq!(grouped("a == b == c"), "((a == b) == c)");
        assert_eq!(grouped("a && b && c || d"), "(((a && b) && c) || d)");
        // `..` is the loosest.
        assert_eq!(grouped("0..n + 1"), "Iter::range(0, (n + 1))");
        assert_eq!(grouped("a || b..c"), "Iter::range((a || b), c)");
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
    fn test_parse_var_statement() {
        let mut parser = create_parser("var x = 1;");
        match parser.parse_stmt_only().unwrap() {
            Statement::Let { var, mutable, .. } => {
                assert!(mutable);
                assert_eq!(var.name, "x");
                assert_eq!(var.t, None);
            }
            result => panic!("Expected Statement::Let, got {:?}", result),
        }
    }

    #[test]
    fn test_parse_let_statement() {
        let mut parser = create_parser("let x: int = 42;");
        let result = parser.parse_stmt_only().unwrap();
        match result {
            Statement::Let {
                var,
                value,
                mutable,
            } => {
                assert!(!mutable);
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
    fn test_parse_tests() {
        let program = create_parser(
            "fn test() -> int { 1 }\n\
             test \"adds\" { assert(test() == 1); }\n\
             test \"reads\" -> Result<void, str> { Ok(void) }",
        )
        .parse()
        .unwrap();
        let names: Vec<(String, bool)> = program
            .definitions
            .iter()
            .map(|d| match d {
                Definition::Function {
                    name,
                    params,
                    rtype,
                    ..
                } => {
                    assert!(params.is_empty());
                    (name.to_string(), rtype.is_some())
                }
                _ => panic!("expected functions"),
            })
            .collect();
        assert_eq!(
            names,
            [
                ("test".to_string(), true),
                ("test \"adds\"".to_string(), false),
                ("test \"reads\"".to_string(), true),
            ]
        );
    }

    #[test]
    fn test_parse_bad_tests() {
        let message = |src: &str| create_parser(src).parse().unwrap_err().message;
        assert_eq!(
            message("test adds {}"),
            "expected the test's name, a string, after 'test', found 'adds'"
        );
        assert_eq!(message("test \"\" {}"), "a test's name can't be empty");
        assert_eq!(
            message("test \"a {1}\" {}"),
            "a test's name can't hold expressions"
        );
        assert_eq!(
            message("test \"a\" {} test \"a\" {}"),
            "there is already a test named \"a\" in this file"
        );
        assert_eq!(
            message("test \"a\" -> int { 1 }"),
            "a test returns 'void' or a 'Result<void, E>', not 'int'"
        );
        assert_eq!(
            message("test \"a\" -> Result<int, str> { Ok(1) }"),
            "a test returns 'void' or a 'Result<void, E>', not 'Result<int, str>'"
        );
        // Only where a definition could start.
        assert!(create_parser("fn f() { test \"a\" {} }").parse().is_err());
    }

    #[test]
    fn test_parse_reports_bad_input_without_panicking() {
        // `any` is not a type
        assert!(create_parser("fn main() { let x: any = 1; x }")
            .parse()
            .is_err());
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
    fn test_parse_bounds() {
        let src = "struct Map<K: Hash, V> { k: K, v: V }
                   impl<K: Eq + Hash + Ord, V> Map<K, V> { fn f<U: Ord>(self, u: U) {} }";
        let program = create_parser(src).parse().unwrap();
        let bounds = |p: &TypeParam| p.bounds.iter().map(|b| b.0.to_string()).collect::<Vec<_>>();
        match &program.definitions[0] {
            Definition::Struct { type_params, .. } => {
                assert_eq!(bounds(&type_params[0]), ["Hash"]);
                assert!(type_params[1].bounds.is_empty());
            }
            d => panic!("expected a struct, got {d:?}"),
        }
        match &program.definitions[1] {
            Definition::Impl {
                type_params,
                methods,
                ..
            } => {
                assert_eq!(bounds(&type_params[0]), ["Eq", "Hash", "Ord"]);
                assert_eq!(bounds(&methods[0].type_params[0]), ["Ord"]);
            }
            d => panic!("expected an impl, got {d:?}"),
        }
        let err = create_parser("fn f<T: >(x: T) {}").parse().unwrap_err();
        assert_eq!(err.message, "expected a trait after ':', found '>'");
        assert!(create_parser("fn f<T: Eq +>(x: T) {}").parse().is_err());
    }

    #[test]
    fn test_parse_generics() {
        let src = "enum Option<T> { Some(T), None }
                   struct Pair<A, B> { first: A, second: B }
                   fn swap<A, B>(p: Pair<A, B>) -> Pair<B, Option<Option<A>>> { p }";
        let program = create_parser(src).parse().unwrap();
        let names = |ps: &[TypeParam]| ps.iter().map(|p| p.name.to_string()).collect::<Vec<_>>();
        match &program.definitions[0] {
            Definition::Enum { type_params, .. } => assert_eq!(names(type_params), ["T"]),
            d => panic!("expected an enum, got {d:?}"),
        }
        match &program.definitions[2] {
            Definition::Function {
                type_params,
                params,
                rtype,
                ..
            } => {
                assert_eq!(names(type_params), ["A", "B"]);
                assert_eq!(params[0].t.as_ref().unwrap().to_string(), "Pair<A, B>");
                // `>>` closes two type argument lists.
                assert_eq!(
                    rtype.as_ref().unwrap().to_string(),
                    "Pair<B, Option<Option<A>>>"
                );
            }
            d => panic!("expected a function, got {d:?}"),
        }

        let err = |src: &str| create_parser(src).parse().unwrap_err();
        assert_eq!(
            err("fn f<>() {}").help.as_deref(),
            Some("leave out the '<>' if there are no type parameters")
        );
        assert_eq!(
            err("fn f<int>(x: List<) {}").message,
            "expected a type, found ')'"
        );
        let e = err("fn f(x: int<str>) {}");
        assert_eq!(e.message, "type 'int' takes no type arguments");
        assert_eq!(e.help.as_deref(), Some("write `int`"));
    }

    #[test]
    fn test_parse_prelude_variants_and_question_mark() {
        // Bare prelude variants are the enum's variants.
        let e = create_parser("Some(1)").parse_expr_only().unwrap();
        match &e.kind {
            Expression::Call(callee, _) => {
                assert!(
                    matches!(&callee.kind, Expression::Path(en, v) if en == "Option" && v == "Some")
                )
            }
            k => panic!("expected a call, got {k:?}"),
        }
        let e = create_parser("None").parse_expr_only().unwrap();
        assert!(matches!(&e.kind, Expression::Path(en, v) if en == "Option" && v == "None"));
        let p = create_parser("Err(e)").parse_pattern().unwrap();
        assert!(
            matches!(&p.kind, PatternKind::Variant { enum_name, variant, parens: true, .. }
            if enum_name == "Result" && variant == "Err")
        );

        // `?` is postfix, binding like a call: `f(x)?.y` is `((f(x))?).y`.
        let e = create_parser("f(x)?.y").parse_expr_only().unwrap();
        let Expression::Field(inner, name, _) = &e.kind else {
            panic!("expected a field, got {e:?}")
        };
        assert_eq!(name, "y");
        assert!(
            matches!(&inner.kind, Expression::Try(call) if matches!(call.kind, Expression::Call(..)))
        );
        let e = create_parser("a? + b?").parse_expr_only().unwrap();
        assert!(matches!(&e.kind, Expression::BinOp(l, _, r)
            if matches!(l.kind, Expression::Try(_)) && matches!(r.kind, Expression::Try(_))));

        // The variants' names can't be declared as other names.
        let err = |src: &str| create_parser(src).parse().unwrap_err().message;
        assert_eq!(
            err("fn main() { let None = 1; }"),
            "'None' is a variant of 'Option', so it can't be used as a variable name"
        );
        assert_eq!(
            err("fn f(Ok: int) { }"),
            "'Ok' is a variant of 'Result', so it can't be used as a parameter name"
        );
        assert_eq!(
            err("fn Some() { }"),
            "'Some' is a variant of 'Option', so it can't be used as a function name"
        );
    }

    #[test]
    fn test_parse_impls() {
        let src =
            "impl<T> Option<T> { fn get(self, d: T) -> T { d } fn none() -> Option<T> { None } }";
        let program = create_parser(src).parse().unwrap();
        let Definition::Impl {
            type_params,
            target,
            methods,
        } = &program.definitions[0]
        else {
            panic!("expected an impl, got {:?}", program.definitions[0]);
        };
        assert_eq!(type_params.len(), 1);
        assert_eq!(target.to_string(), "Option<T>");
        assert_eq!(methods.len(), 2);
        assert!(methods[0].has_self);
        assert_eq!(
            methods[0]
                .params
                .iter()
                .map(|p| p.name.as_str())
                .collect::<Vec<_>>(),
            ["self", "d"]
        );
        assert!(!methods[1].has_self);

        let err = |src: &str| create_parser(src).parse().unwrap_err();
        assert_eq!(
            err("impl P { fn f(self: P) { } }").message,
            "'self' has no type annotation"
        );
        assert_eq!(
            err("fn f(self) { }").message,
            "'self' can't be used as a parameter name"
        );
        assert_eq!(
            err("impl P { fn f(x: int, self) { } }").message,
            "'self' can't be used as a parameter name"
        );
        assert_eq!(
            err("fn main() { let self = 1; }").message,
            "'self' can't be used as a variable name"
        );
        assert_eq!(
            err("impl P { let x = 1; }").help.as_deref(),
            Some("an impl can only hold functions")
        );
    }

    #[test]
    fn test_struct_literals_need_parens_in_conditions() {
        // `p == P` is the condition, and `{ x: 1 }` is a (malformed) body.
        let err = create_parser("fn main() { if p == P { x: 1 } { } }")
            .parse()
            .unwrap_err();
        assert!(err.help.unwrap().contains("(P { ... })"));
        assert!(create_parser("fn main() { if p == (P { x: 1 }) { } }")
            .parse()
            .is_ok());
        // Plain variables before a block still work.
        assert!(
            create_parser("fn main() { while ok { } match v { _ => 1 } }")
                .parse()
                .is_ok()
        );
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
        assert!(crate::lexer::tokenize("99999999999999999999").is_err());
    }

    #[test]
    fn test_parse_else_if_and_or_patterns() {
        // `else if` is `else { if ... }`.
        let e = create_parser("if a { 1 } else if b { 2 } else { 3 }")
            .parse_expr_only()
            .unwrap();
        let Expression::If(_, _, otherwise) = e.kind else {
            panic!("expected an if");
        };
        let Statement::Block(stmts, Some(inner)) = *otherwise else {
            panic!("expected a block holding the inner if");
        };
        assert!(stmts.is_empty());
        assert!(matches!(inner.kind, Expression::If(..)));

        let err = create_parser("if a { 1 } else if b { 2 }")
            .parse_expr_only()
            .unwrap_err();
        assert_eq!(err.message, "expected 'else', found end of file");

        let e = create_parser("match n { 1 | 2 | 3 => 0, Some(0) | None => 1 }")
            .parse_expr_only()
            .unwrap();
        let Expression::Match(_, arms) = e.kind else {
            panic!("expected a match");
        };
        let counts: Vec<usize> = arms
            .iter()
            .map(|a| match &a.pattern.kind {
                PatternKind::Or(alts) => alts.len(),
                _ => 1,
            })
            .collect();
        assert_eq!(counts, [3, 2]);
    }

    #[test]
    fn test_parse_template_strings() {
        let src = r#""{a} + {b} = {a + b}!""#;
        let Expression::Template(parts) = create_parser(src).parse_expr_only().unwrap().kind else {
            panic!("expected a template");
        };
        let text = |e: &Expr| -> String {
            src.chars()
                .skip(e.span.start.col as usize - 1)
                .take((e.span.end.col - e.span.start.col) as usize)
                .collect()
        };
        let shown: Vec<String> = parts
            .iter()
            .map(|p| match p {
                TemplatePart::Text(t) => format!("{t:?}"),
                TemplatePart::Expr(e) => format!("<{}>", text(e)),
            })
            .collect();
        assert_eq!(
            shown,
            ["<a>", "\" + \"", "<b>", "\" = \"", "<a + b>", "\"!\""]
        );

        for (src, message) in [
            (r#""a {} b""#, "a template string has an empty '{}'"),
            (
                r#""a {x y} b""#,
                "expected '}' to end the expression in a template string, found 'y'",
            ),
        ] {
            let err = create_parser(src).parse_expr_only().unwrap_err();
            assert_eq!(err.message, message, "{src}");
        }
        let err = create_parser(r#"match s { "{x}" => 1 }"#)
            .parse_expr_only()
            .unwrap_err();
        assert_eq!(err.message, "template strings can't be used as patterns");
    }

    #[test]
    fn test_parse_for_loops_and_ranges() {
        let tokens = crate::lexer::tokenize("0..10 1.5 x..y").unwrap();
        let kinds: Vec<Token> = tokens.into_iter().map(|(_, t, _)| t).collect();
        assert_eq!(
            kinds,
            [
                Token::Int(0),
                Token::DotDot,
                Token::Int(10),
                Token::Float(1.5),
                Token::Ident("x".into()),
                Token::DotDot,
                Token::Ident("y".into()),
                Token::Eof
            ]
        );

        let mut parser = create_parser("for i in 0..n + 1 { break; continue; }");
        let Statement::For {
            pattern,
            iter,
            body,
        } = parser.parse_stmt_only().unwrap()
        else {
            panic!("expected a for loop");
        };
        assert!(matches!(&pattern.kind, PatternKind::Binding(v) if v.name == "i"));
        // `a..b` is a call of `Iter::range`, with the end's arithmetic inside it.
        let Expression::Call(callee, args) = &iter.kind else {
            panic!("expected a call, got {iter:?}")
        };
        assert!(matches!(&callee.kind, Expression::Path(t, f) if t == "Iter" && f == "range"));
        assert!(matches!(args[1].kind, Expression::BinOp(..)));
        assert!(matches!(
            *body,
            Statement::Block(ref s, None) if matches!(s[..], [Statement::Break(_), Statement::Continue(_)])
        ));

        // A for loop needs no ';' after it.
        assert!(create_parser("fn main() { for i in 0..3 { } let x = 1; }")
            .parse()
            .is_ok());

        let err = |src: &str| create_parser(src).parse().unwrap_err();
        assert!(err("fn main() { for i 0..3 { } }")
            .help
            .unwrap()
            .contains("for i in"));
        assert_eq!(
            err("fn main() { for i: int in 0..3 { } }").message,
            "a loop variable has no type annotation"
        );
        assert!(err("fn main() { for i in 0.. { } }")
            .message
            .contains("the end of the range"));
        assert_eq!(
            err("fn main() { let r = 0..1..2; }").message,
            "ranges can't be chained"
        );
        assert!(err("fn main() { for None in 0..3 { } }")
            .message
            .contains("variant"));
    }

    #[test]
    fn test_parse_tuples() {
        let e = create_parser("(1, (a, b), c,)").parse_expr_only().unwrap();
        let Expression::Tuple(items) = &e.kind else {
            panic!("expected a tuple, got {e:?}")
        };
        assert_eq!(items.len(), 3);
        assert!(matches!(items[1].kind, Expression::Tuple(_)));
        assert!(matches!(
            create_parser("(1)").parse_expr_only().unwrap().kind,
            Expression::Grouping(_)
        ));
        // `.0.1` is two positions, not the float `0.1`.
        let e = create_parser("t.0.1").parse_expr_only().unwrap();
        let Expression::Field(inner, one, _) = &e.kind else {
            panic!("expected a field access, got {e:?}")
        };
        assert_eq!(one, "1");
        assert!(matches!(&inner.kind, Expression::Field(_, zero, _) if zero == "0"));

        let err = |src: &str| create_parser(src).parse().unwrap_err();
        assert_eq!(
            err("fn main() { let x = (1,); }").message,
            "a tuple holds two or more values"
        );
        assert!(err("fn main() { let x = (); }")
            .help
            .unwrap()
            .contains("Aja has no `()` value, and `void` is the value of type `void`"));
        assert!(matches!(
            create_parser("void").parse_expr_only().unwrap().kind,
            Expression::Void
        ));
        // `defer` takes an expression, which needs a `;`, or a block, which
        // doesn't.
        let Statement::Block(stmts, _) = create_parser("{ defer f(); defer { g(); } h() }")
            .parse_stmt_only()
            .unwrap()
        else {
            panic!("expected a block")
        };
        assert!(
            matches!(&stmts[0], Statement::Defer(body, _) if matches!(**body, Statement::Expr(_)))
        );
        assert!(
            matches!(&stmts[1], Statement::Defer(body, _) if matches!(**body, Statement::Block(..)))
        );
        assert_eq!(
            err("fn main() { defer f() }").message,
            "expected ';', found '}'"
        );
        // `void` names nothing else.
        for (src, what) in [
            ("fn main() { let void = 1; }", "a variable name"),
            ("fn void() {}", "a function name"),
            ("fn f(void: int) {}", "a parameter name"),
        ] {
            assert_eq!(
                err(src).message,
                format!("'void' is a value, so it can't be used as {what}")
            );
        }
        assert_eq!(
            err("fn main() { var (a, b) = (1, 2); }").message,
            "'var' declares a single name, not a pattern"
        );
    }

    #[test]
    fn test_parse_tuple_types_and_patterns() {
        let (params, rt) = parse_closure("fn(p: (int, str)) -> ((int, str)) -> (bool, int) { p }");
        assert_eq!(
            params[0].t,
            Some(Type::Tuple(vec![Type::Int, Type::String]))
        );
        assert_eq!(
            rt,
            Some(Type::Fn {
                param_types: vec![Type::Tuple(vec![Type::Int, Type::String])],
                ret_type: Box::new(Type::Tuple(vec![Type::Bool, Type::Int])),
            })
        );
        let mut parser = create_parser("let (a, (_, b)) = t");
        let Statement::LetPattern { pattern, .. } = parser.parse_stmt_only().unwrap() else {
            panic!("expected a let with a pattern");
        };
        assert!(matches!(&pattern.kind, PatternKind::Tuple(items) if items.len() == 2));
        let mut parser = create_parser("let x: int = 1");
        assert!(matches!(
            parser.parse_stmt_only().unwrap(),
            Statement::Let { .. }
        ));
        let mut parser = create_parser("for (k, v) in m { }");
        let Statement::For { pattern, .. } = parser.parse_stmt_only().unwrap() else {
            panic!("expected a for loop");
        };
        assert!(matches!(pattern.kind, PatternKind::Tuple(_)));
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
        let (ps, rt) =
            parse_closure("fn(f: (int, str) -> bool, g: () -> void) -> (int) -> int { f }");
        let fn_t = |ps: Vec<Type>, r: Type| Type::Fn {
            param_types: ps,
            ret_type: Box::new(r),
        };
        assert_eq!(
            ps[0].t,
            Some(fn_t(vec![Type::Int, Type::String], Type::Bool))
        );
        assert_eq!(ps[1].t, Some(fn_t(vec![], Type::Void)));
        assert_eq!(rt, Some(fn_t(vec![Type::Int], Type::Int)));
        let err = create_parser("fn main() { let x: any = 1; x }")
            .parse()
            .unwrap_err();
        assert!(err.help.unwrap().contains("no 'any' type"));
    }

    #[test]
    fn test_function_type_errors_have_help() {
        let err = |src: &str| create_parser(src).parse().unwrap_err();
        // The old spelling with `fn`, with the exact fix when it parses.
        let e = err("fn apply(f: fn(int, str) -> bool) {}");
        assert_eq!(e.message, "function types are written without 'fn'");
        assert_eq!(e.help.as_deref(), Some("write `(int, str) -> bool`"));
        assert_eq!(
            err("fn run(f: fn()) {}").help.as_deref(),
            Some("write `() -> void`")
        );
        assert!(err("fn run(f: fn) {}")
            .help
            .unwrap()
            .contains("`(int, str) -> bool`"));
        // The return type can't be left out.
        let e = err("fn run(f: (int)) {}");
        assert_eq!(e.message, "expected '->' and a return type, found ')'");
        assert_eq!(
            e.help.as_deref(),
            Some("a function type always names its return type: `(int) -> void` if it returns nothing")
        );
        // Function types nest, and `->` groups to the right.
        let (_, rt) = parse_closure("fn() -> (int) -> (str) -> bool { f }");
        let fn_t = |ps: Vec<Type>, r: Type| Type::Fn {
            param_types: ps,
            ret_type: Box::new(r),
        };
        assert_eq!(
            rt,
            Some(fn_t(vec![Type::Int], fn_t(vec![Type::String], Type::Bool)))
        );
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
        write!(
            f,
            "{}:{}: {}",
            self.start.line, self.start.col, self.message
        )
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
        let mut imports = Vec::new();
        let mut ds = Vec::new();
        // The names of this file's tests, which must differ.
        let mut tests: Vec<Id> = Vec::new();
        loop {
            if tok_matches!(self, Token::Import) {
                if !ds.is_empty() {
                    let tok = self.peek().cloned();
                    return Err(self
                        .error(tok, "imports must come before any definition".to_string())
                        .help("move this import to the top of the file"));
                }
                imports.push(self.parse_import()?);
            } else if tok_matches!(
                self,
                Token::Fn | Token::Struct | Token::Enum | Token::Impl | Token::Pub
            ) || self.at_native()
            {
                ds.push(self.parse_defn()?);
            } else if self.at_test() {
                ds.push(self.parse_test(&mut tests)?);
            } else {
                break;
            }
        }

        if !tok_matches!(self, Token::Eof) && self.peek().is_some() {
            return Err(self
                .expected("a definition")
                .help("only imports, 'test' blocks, and 'fn', 'struct', 'enum', and 'impl' definitions are allowed at the top level of a file"));
        }

        Ok(Program {
            imports,
            definitions: ds,
        })
    }

    /// `import geo::shapes;` or `import geo::shapes::{Shape, area};`
    fn parse_import(&mut self) -> ParseResult<Import> {
        self.consume(Token::Import)?;
        let mut path = Vec::new();
        let mut items = None;
        loop {
            match self.peek().cloned() {
                Some((s, Token::Ident(name), e)) => {
                    self.next();
                    path.push((name, Span::new(s, e)));
                }
                Some((_, Token::LeftBrace, _)) if !path.is_empty() => {
                    items = Some(self.parse_import_items()?);
                    break;
                }
                _ if path.is_empty() => {
                    return Err(self.expected("a module name after 'import'").help(
                        "name a module by its path from the program's root: `import geo::shapes;`",
                    ))
                }
                _ => return Err(self.expected("a name or '{' after '::'")),
            }
            if tok_matches!(self, Token::DoubleColon) {
                self.next();
            } else {
                break;
            }
        }
        if tok_matches!(self, Token::Comma) && items.is_none() {
            return Err(self.expected("';'").help(
                "to import several names from a module, put them in braces: `import geo::{Point, area};`",
            ));
        }
        if !tok_matches!(self, Token::Semicolon) {
            return Err(self.missing_semicolon());
        }
        self.next();
        Ok(Import { path, items })
    }

    /// The names in braces at the end of an import: `{Shape, area}`.
    fn parse_import_items(&mut self) -> ParseResult<Vec<(Id, Span)>> {
        self.consume(Token::LeftBrace)?;
        let mut items = Vec::new();
        while !tok_matches!(self, Token::RightBrace) {
            match self.peek().cloned() {
                Some((s, Token::Ident(name), e)) => {
                    self.next();
                    items.push((name, Span::new(s, e)));
                }
                Some((_, Token::Mult, _)) => {
                    let tok = self.peek().cloned();
                    return Err(self
                        .error(tok, "glob imports are not allowed".to_string())
                        .help(
                        "name each item to import, or import the module and write `module::item`",
                    ));
                }
                _ => return Err(self.expected("a name to import")),
            }
            if tok_matches!(self, Token::Comma) {
                self.next();
            } else if !tok_matches!(self, Token::RightBrace) {
                return Err(self.expected("',' or '}'"));
            }
        }
        if items.is_empty() {
            return Err(self.expected("a name to import").help(
                "name what to import in the braces, or leave them out to import the module",
            ));
        }
        self.consume(Token::RightBrace)?;
        Ok(items)
    }

    /// Whether the next token is `native`. It is a keyword only where a
    /// definition starts, where a name couldn't be, so it can still be used
    /// as a name (as in `import std::native;`).
    fn at_native(&mut self) -> bool {
        tok_matches!(self, Token::Ident(ref n) if n == "native")
    }

    /// Whether the next token is `test`, which starts a test where a
    /// definition could. Like `native`, it is a keyword only there, so it
    /// can still be used as a name.
    fn at_test(&mut self) -> bool {
        tok_matches!(self, Token::Ident(ref n) if n == "test")
    }

    /// `test "name" { ... }`, or `test "name" -> Result<void, E> { ... }` to
    /// use `?` in it, as a function no code can call (see
    /// [`crate::ast::test_fn_name`]). `names` are the file's tests so far.
    fn parse_test(&mut self, names: &mut Vec<Id>) -> ParseResult<Definition> {
        let (start, _, _) = self.next().expect("at 'test'");
        let tok = self.peek().cloned();
        let (name, end) = match tok.clone() {
            Some((_, Token::String(name), end)) => {
                self.next();
                (name, end)
            }
            Some((_, Token::TemplateStart(_), _)) => {
                return Err(self
                    .error(tok, "a test's name can't hold expressions".to_string())
                    .help("write the name as plain text, with `\\{` for a brace"));
            }
            _ => {
                return Err(self
                    .expected("the test's name, a string, after 'test'")
                    .help("name the test by what it checks: `test \"adds two numbers\" { ... }`"));
            }
        };
        if name.trim().is_empty() {
            return Err(self
                .error(tok, "a test's name can't be empty".to_string())
                .help("name the test by what it checks, which is shown when it fails"));
        }
        if names.contains(&name) {
            return Err(self
                .error(
                    tok,
                    format!("there is already a test named {name:?} in this file"),
                )
                .help("give each test a name of its own, so a failure says which one it was"));
        }
        names.push(name.clone());

        let arrow = self.peek().cloned();
        let rtype = self.parse_fn_return_type_annotation()?;
        if let Some(t) = &rtype {
            let ok = matches!(t, Type::Named(n, args, _)
                if n == "Result" && args.len() == 2 && args[0] == Type::Void);
            if !ok {
                let mut err = self.error(
                    arrow,
                    format!("a test returns 'void' or a 'Result<void, E>', not '{t}'"),
                );
                err.end = self.prev_token.as_ref().map_or(err.end, |(_, _, e)| *e);
                return Err(err.help(
                    "leave out the return type, or write `-> Result<void, E>` to use `?` in the test",
                ));
            }
        }
        let body = self.parse_block()?;
        Ok(Definition::Function {
            public: false,
            native: false,
            name: crate::ast::test_fn_name(&name),
            name_span: Span::new(start, end),
            type_params: Vec::new(),
            params: Vec::new(),
            body,
            rtype,
        })
    }

    /// `native` before a definition or a function in an impl, if it is
    /// there.
    fn parse_native(&mut self) -> bool {
        let native = self.at_native();
        if native {
            self.next();
        }
        native
    }

    /// `pub` before a definition, a function in an impl, or a field, if it
    /// is there.
    fn parse_pub(&mut self) -> bool {
        let public = tok_matches!(self, Token::Pub);
        if public {
            self.next();
        }
        public
    }

    pub fn parse_expr_only(&mut self) -> ParseResult<Expr> {
        self.parse_expr()
    }

    pub fn parse_stmt_only(&mut self) -> ParseResult<Statement> {
        self.parse_stmt()
    }

    fn parse_defn(&mut self) -> ParseResult<Definition> {
        let public = self.parse_pub();
        let native = self.parse_native();
        match self.next() {
            Some((_, Token::Fn, _)) => self.parse_fn_defn(public, native),
            Some((_, Token::Struct, _)) if native => self.parse_native_struct(public),
            Some(tok) if native => Err(self
                .error(Some(tok), "expected 'fn' or 'struct' after 'native'".to_string())),
            Some((_, Token::Struct, _)) => self.parse_struct_defn(public),
            Some((_, Token::Enum, _)) => self.parse_enum_defn(public),
            Some(tok @ (_, Token::Impl, _)) if public => Err(self
                .error(Some(tok), "an impl can't be 'pub'".to_string())
                .help("mark the functions in it that other modules may call as 'pub'")),
            Some((_, Token::Impl, _)) => self.parse_impl(),
            Some(tok @ (_, Token::Import, _)) => Err(self
                .error(Some(tok), "an import can't be 'pub'".to_string())
                .help("an import only brings names into this file; code that needs them imports them too")),
            tok if public => Err(self.error(tok, "expected 'fn', 'struct', or 'enum' after 'pub'".to_string())),
            tok => Err(self.error(tok, "expected a definition".to_string())),
        }
    }

    fn parse_fn_defn(&mut self, public: bool, native: bool) -> ParseResult<Definition> {
        let m = self.parse_fn(false, public, native)?;
        Ok(Definition::Function {
            public,
            native,
            name: m.name,
            name_span: m.name_span,
            type_params: m.type_params,
            params: m.params,
            body: m.body,
            rtype: m.rtype,
        })
    }

    /// A function after `fn`; in an `impl` (`in_impl`), it may take `self`.
    fn parse_fn(&mut self, in_impl: bool, public: bool, native: bool) -> ParseResult<Method> {
        if !tok_matches!(self, Token::Ident(_)) {
            return Err(self.expected("a function name after 'fn'"));
        }

        let tok = self.peek().cloned();
        let (ident, name_span) = match self.next() {
            Some((start, Token::Ident(i), end)) => (i, Span::new(start, end)),
            _ => unreachable!(),
        };
        self.check_new_name(tok, &ident, "a function name")?;

        let type_params = self.parse_type_params()?;
        let (params, has_self) = self.parse_params(in_impl)?;
        let rtype = self.parse_fn_return_type_annotation()?;
        let body = if native {
            // Implemented by each backend, so it has no body.
            if !tok_matches!(self, Token::Semicolon) {
                return Err(self
                    .expected("';'")
                    .help("a native function has no body: `native fn len(self) -> int;`"));
            }
            self.next();
            Statement::Block(Vec::new(), None)
        } else {
            self.parse_block()?
        };

        Ok(Method {
            public,
            native,
            name: ident,
            name_span,
            type_params,
            params,
            has_self,
            body,
            rtype,
        })
    }

    /// `impl Point { fn ... }`, or `impl<T> Option<T> { ... }`.
    fn parse_impl(&mut self) -> ParseResult<Definition> {
        let type_params = self.parse_type_params()?;
        if !tok_matches!(self, Token::Ident(_)) {
            return Err(self.expected("a type after 'impl'"));
        }
        let target = self.parse_type_name()?;
        if tok_matches!(self, Token::LeftParen) {
            return Err(self
                .expected("'{'")
                .help("an impl holds functions: `impl Point { fn norm(self) -> int { ... } }`"));
        }
        self.consume(Token::LeftBrace)?;
        let mut methods = Vec::new();
        while tok_matches!(self, Token::Fn | Token::Pub) || self.at_native() {
            let public = self.parse_pub();
            let native = self.parse_native();
            if !tok_matches!(self, Token::Fn) {
                return Err(self.expected("'fn'"));
            }
            self.next();
            methods.push(self.parse_fn(true, public, native)?);
        }
        if !tok_matches!(self, Token::RightBrace) {
            return Err(self
                .expected("'fn' or '}'")
                .help("an impl can only hold functions"));
        }
        self.next();
        Ok(Definition::Impl {
            type_params,
            target,
            methods,
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

    /// The type parameters after a definition's name, `<A, B>`, if any.
    fn parse_type_params(&mut self) -> ParseResult<Vec<TypeParam>> {
        let mut params = Vec::new();
        if !tok_matches!(self, Token::LessThan) {
            return Ok(params);
        }
        self.next();
        loop {
            match self.peek().cloned() {
                Some((start, Token::Ident(name), end)) => {
                    self.next();
                    let mut bounds = Vec::new();
                    if tok_matches!(self, Token::Colon) {
                        self.next();
                        loop {
                            match self.peek().cloned() {
                                Some((s, Token::Ident(b), e)) => {
                                    self.next();
                                    bounds.push((b, Span::new(s, e)));
                                }
                                _ => {
                                    return Err(self.expected("a trait after ':'").help(format!(
                                    "bounds name traits, e.g. `{name}: Eq` or `{name}: Hash + Ord`"
                                )))
                                }
                            }
                            if tok_matches!(self, Token::Plus) {
                                self.next();
                            } else {
                                break;
                            }
                        }
                    }
                    params.push(TypeParam {
                        name,
                        span: Span::new(start, end),
                        bounds,
                    });
                }
                Some((_, Token::GreaterThan, _)) if params.is_empty() => {
                    return Err(self
                        .expected("a type parameter name")
                        .help("leave out the '<>' if there are no type parameters"));
                }
                _ => {
                    return Err(self
                        .expected("a type parameter name")
                        .help("type parameters are names, e.g. `<T>` or `<K: Hash, V>`"))
                }
            }
            if tok_matches!(self, Token::Comma) {
                self.next();
            } else {
                break;
            }
        }
        self.consume(Token::GreaterThan)?;
        Ok(params)
    }

    /// `native struct List<T>;`, after `native struct`.
    fn parse_native_struct(&mut self, public: bool) -> ParseResult<Definition> {
        let (name, name_span) = self.parse_type_def_name("struct")?;
        let type_params = self.parse_type_params()?;
        if !tok_matches!(self, Token::Semicolon) {
            return Err(self.expected("';'").help(format!(
                "a native struct has no fields: `native struct {name};`"
            )));
        }
        self.next();
        Ok(Definition::Struct {
            public,
            native: true,
            name,
            name_span,
            type_params,
            fields: Vec::new(),
        })
    }

    /// `struct Name { field: type, ... }`
    fn parse_struct_defn(&mut self, public: bool) -> ParseResult<Definition> {
        let (name, name_span) = self.parse_type_def_name("struct")?;
        let type_params = self.parse_type_params()?;
        if tok_matches!(self, Token::LeftParen) {
            return Err(self.expected("'{'").help(format!(
                "struct fields have names: `struct {name} {{ x: int, y: int }}`"
            )));
        }
        self.consume(Token::LeftBrace)?;
        let mut fields = Vec::new();
        while !tok_matches!(self, Token::RightBrace) {
            let field_public = self.parse_pub();
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
            let t = self.parse_type_annotation()?.expect("a ':' was seen");
            fields.push(FieldDef {
                public: field_public,
                name: field,
                t,
                span,
            });
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
        Ok(Definition::Struct {
            public,
            native: false,
            name,
            name_span,
            type_params,
            fields,
        })
    }

    /// `enum Name { A, B(int, str), ... }`
    fn parse_enum_defn(&mut self, public: bool) -> ParseResult<Definition> {
        let (name, name_span) = self.parse_type_def_name("enum")?;
        let type_params = self.parse_type_params()?;
        self.consume(Token::LeftBrace)?;
        let mut variants = Vec::new();
        while !tok_matches!(self, Token::RightBrace) {
            if tok_matches!(self, Token::Pub) {
                let tok = self.peek().cloned();
                return Err(self
                    .error(tok, "a variant can't be 'pub'".to_string())
                    .help(format!("every variant of a 'pub' enum can be used where the enum can; write `pub enum {name}`")));
            }
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
            variants.push(VariantDef {
                name: variant,
                span,
                fields,
            });
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
        Ok(Definition::Enum {
            public,
            name,
            name_span,
            type_params,
            variants,
        })
    }

    /// The rest of a template string, after its first text: its
    /// expressions, each followed by the text after it.
    fn parse_template(&mut self, first: ecow::EcoString) -> ParseResult<Expression> {
        let mut parts = Vec::new();
        let mut text = first;
        loop {
            if !text.is_empty() {
                parts.push(TemplatePart::Text(text));
            }
            if let Some((_, Token::TemplateMiddle(_) | Token::TemplateEnd(_), _)) = self.peek() {
                let tok = self.peek().cloned();
                return Err(self
                    .error(tok, "a template string has an empty '{}'".to_string())
                    .help("put an expression in it, or write '\\{' for a literal brace"));
            }
            let expr = self.with_structs(true, |p| p.parse_expr())?;
            parts.push(TemplatePart::Expr(expr));
            match self.peek().cloned() {
                Some((_, Token::TemplateMiddle(t), _)) => {
                    self.next();
                    text = t;
                }
                Some((_, Token::TemplateEnd(t), _)) => {
                    self.next();
                    if !t.is_empty() {
                        parts.push(TemplatePart::Text(t));
                    }
                    return Ok(Expression::Template(parts));
                }
                _ => {
                    return Err(self
                        .expected("'}' to end the expression in a template string")
                        .help("write '\\{' for a literal brace in a string"))
                }
            }
        }
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
        let start = self.position;
        let body = self.parse_block();
        self.with_struct_hint(hint, start, body)
    }

    /// Add a hint to an error in what follows a head that might have
    /// contained a struct literal, which began at token `start`. Only an
    /// error right at the start counts: fields read as a block fail by their
    /// second token (`{ x: 1 }`, `{ x, y }`, `{ ..p }`), and an error further
    /// in is about the body itself.
    fn with_struct_hint<R>(
        &self,
        hint: Option<crate::ast::Id>,
        start: u32,
        result: ParseResult<R>,
    ) -> ParseResult<R> {
        let near_start = self.position - start <= 2;
        result.map_err(|e| match hint {
            Some(name) if near_start => e.help(format!(
                "a struct literal can't appear here without parentheses: `({name} {{ ... }})`"
            )),
            _ => e,
        })
    }

    fn parse_fn_params(&mut self) -> ParseResult<Vec<Variable>> {
        Ok(self.parse_params(false)?.0)
    }

    /// A parameter list, and whether it starts with `self`, which only a
    /// function in an impl (`self_ok`) can.
    fn parse_params(&mut self, self_ok: bool) -> ParseResult<(Vec<Variable>, bool)> {
        let mut xs = Vec::new();
        let mut has_self = false;

        self.consume(Token::LeftParen)?;

        if !tok_matches!(self, Token::RightParen) {
            loop {
                let (name, span) = match self.peek() {
                    Some((start, Token::Ident(id), end)) => (id.clone(), Span::new(*start, *end)),
                    _ => return Err(self.expected("a parameter name")),
                };
                let tok = self.peek().cloned();
                if name == "self" && self_ok && xs.is_empty() {
                    self.next();
                    if tok_matches!(self, Token::Colon) {
                        let tok = self.peek().cloned();
                        return Err(self
                            .error(tok, "'self' has no type annotation".to_string())
                            .help("its type is always the type the impl is for: write `self`"));
                    }
                    has_self = true;
                    xs.push(Variable::new(name, None, span));
                    if tok_matches!(self, Token::Comma) {
                        self.next();
                        continue;
                    }
                    break;
                }
                self.check_new_name(tok, &name, "a parameter name")?;
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

        Ok((xs, has_self))
    }

    /// Parse a statement
    fn parse_stmt(&mut self) -> ParseResult<Statement> {
        let Some(tok) = self.peek() else {
            return Err(self.expected("a statement"));
        };

        match tok.1 {
            Token::LeftBrace => self.parse_block(),
            Token::Let | Token::Var => self.parse_let(),
            Token::Return => self.parse_return(),
            Token::Defer => self.parse_defer(),
            Token::If => self.parse_if_stmt(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for(),
            Token::Break | Token::Continue => {
                let (start, tok, end) = self.next().unwrap();
                let span = Span::new(start, end);
                Ok(if tok == Token::Break {
                    Statement::Break(span)
                } else {
                    Statement::Continue(span)
                })
            }
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
        if self.peek().is_none() {
            return Err(self.expected("an expression"));
        }
        self.parse_assignment()
    }

    /// Parse a sequence of expressions,
    /// defined by a separator, opening, and closing token
    fn parse_expr_seq(&mut self, open: Token, sep: Token, close: Token) -> ParseResult<Vec<Expr>> {
        let mut xs = Vec::new();
        self.consume(open.clone())?;
        if !self.matches(vec![close.clone()]) {
            loop {
                let e = self.with_structs(true, |p| p.parse_expr())?;
                xs.push(e);
                match self.peek() {
                    Some((_, tt, _)) if *tt == sep => {
                        self.next();
                        // A trailing separator: `[1, 2,]`.
                        if self.matches(vec![close.clone()]) {
                            break;
                        }
                    }
                    _ => break,
                }
            }
        }
        self.consume(close.clone())?;
        Ok(xs)
    }

    /// `let name = value;`, `var name = value;`, or `let pattern = value;`
    /// for a pattern that is more than a name, such as `let (a, b) = pair;`.
    fn parse_let(&mut self) -> ParseResult<Statement> {
        let mutable = tok_matches!(self, Token::Var);
        let keyword = if mutable { "var" } else { "let" };
        self.next();

        let Some(pattern) = self.parse_binding_pattern("a variable name")? else {
            return Err(self.expected(&format!("a variable name after '{keyword}'")));
        };
        if mutable && !matches!(pattern.kind, PatternKind::Binding(_)) {
            return Err(ParseError {
                start: pattern.span.start,
                end: pattern.span.end,
                message: "'var' declares a single name, not a pattern".to_string(),
                help: Some(
                    "the names a pattern binds can't change: use `let`, and copy a value \
                     into a `var` if it needs to"
                        .to_string(),
                ),
            });
        }

        let ttype = self.parse_type_annotation()?;
        if !tok_matches!(self, Token::Equal) {
            let shown = match &pattern.kind {
                PatternKind::Binding(v) => v.name.to_string(),
                _ => "...".to_string(),
            };
            return Err(self.expected("'='").help(format!(
                "variables must be initialized: `{keyword} {shown} = <value>;`"
            )));
        }
        self.consume(Token::Equal)?;
        let e = self.parse_expr()?;
        Ok(match pattern.kind {
            PatternKind::Binding(v) => Statement::Let {
                var: Variable::new(v.name, ttype, v.span),
                value: e,
                mutable,
            },
            _ => Statement::LetPattern {
                pattern,
                t: ttype,
                value: e,
            },
        })
    }

    /// What a `let` or `for` binds: a name, or a pattern such as `(a, b)`
    /// or `Point { x, y }`. `None` if there is neither. `what` describes a
    /// name declared there, for errors.
    fn parse_binding_pattern(&mut self, what: &str) -> ParseResult<Option<Pattern>> {
        let tok = self.peek().cloned();
        match tok.clone() {
            Some((start, Token::Ident(name), end)) if name != "true" && name != "false" => {
                self.next();
                let span = Span::new(start, end);
                let structured = tok_matches!(self, Token::DoubleColon | Token::LeftBrace)
                    || (prelude::variant_enum(&name).is_some()
                        && tok_matches!(self, Token::LeftParen));
                if !structured && name != "_" {
                    self.check_new_name(tok, &name, what)?;
                }
                let kind = self.pattern_after_name(name, span)?;
                Ok(Some(Pattern {
                    kind,
                    span: Span::new(start, self.last_end()),
                }))
            }
            Some((_, Token::LeftParen, _)) => Ok(Some(self.parse_single_pattern()?)),
            _ => Ok(None),
        }
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
                    | Statement::For { .. }
                    | Statement::Block(..) => stmts.push(stmt),
                    Statement::Defer(ref body, _) if matches!(**body, Statement::Block(..)) => {
                        stmts.push(stmt)
                    }
                    _ => return Err(self.missing_semicolon()),
                };
            }
        }

        self.consume(Token::RightBrace)?;

        Ok(Statement::Block(stmts, tail))
    }

    /// `defer expr` or `defer { ... }`.
    fn parse_defer(&mut self) -> ParseResult<Statement> {
        let (start, _, end) = self.consume(Token::Defer)?;
        let body = if tok_matches!(self, Token::LeftBrace) {
            self.parse_block()?
        } else {
            self.parse_stmt_expr()?
        };
        Ok(Statement::Defer(Box::new(body), Span::new(start, end)))
    }

    fn parse_return(&mut self) -> ParseResult<Statement> {
        self.consume(Token::Return)?;
        // A `return` without a value ends at a `;`, or, as a match arm, at
        // the `,` or `}` after it.
        let res = if tok_matches!(self, Token::Semicolon | Token::Comma | Token::RightBrace) {
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
            let s2 = self.parse_else(false)?;
            let span = Span::new(start, self.last_end());
            return Ok(Statement::Expr(Expr::new(
                Expression::If(Box::new(e), Box::new(s1), Box::new(s2)),
                span,
            )));
        }

        Ok(Statement::If(e, Box::new(s1)))
    }

    /// What follows `else`: a block, or another `if`, as in
    /// `if a { ... } else if b { ... } else { ... }`. The `if` becomes a
    /// block holding only it, so later passes see `else { if b ... }`. An
    /// `if` used for its value (`value`) needs a final `else`, as any does.
    fn parse_else(&mut self, value: bool) -> ParseResult<Statement> {
        if !tok_matches!(self, Token::If) {
            return self.parse_block();
        }
        if value {
            let e = self.parse_if_expr()?;
            return Ok(Statement::Block(Vec::new(), Some(e)));
        }
        match self.parse_if_stmt()? {
            Statement::Expr(e) => Ok(Statement::Block(Vec::new(), Some(e))),
            s => Ok(Statement::Block(vec![s], None)),
        }
    }

    fn parse_while(&mut self) -> ParseResult<Statement> {
        self.consume(Token::While)?;
        let e = self.parse_head()?;
        let s1 = self.parse_body()?;
        Ok(Statement::While(e, Box::new(s1)))
    }

    /// `for x in iter { ... }`
    fn parse_for(&mut self) -> ParseResult<Statement> {
        self.consume(Token::For)?;
        let Some(pattern) = self.parse_binding_pattern("a loop variable")? else {
            return Err(self
                .expected("a name after 'for'")
                .help("a for loop names each value: `for x in 0..10 { ... }`"));
        };
        if tok_matches!(self, Token::Colon) {
            let tok = self.peek().cloned();
            return Err(self
                .error(tok, "a loop variable has no type annotation".to_string())
                .help("its type is the type of the values being looped over"));
        }
        if !tok_matches!(self, Token::In) {
            let shown = match &pattern.kind {
                PatternKind::Binding(v) => v.name.to_string(),
                _ => "x".to_string(),
            };
            return Err(self
                .expected("'in'")
                .help(format!("write `for {shown} in values {{ ... }}`")));
        }
        self.next();
        let iter = self.parse_head()?;
        let body = self.parse_body()?;
        Ok(Statement::For {
            pattern,
            iter,
            body: Box::new(body),
        })
    }

    fn parse_if_expr(&mut self) -> ParseResult<Expr> {
        let (start, _, _) = self.consume(Token::If)?;
        let e1 = self.parse_head()?;
        let e2 = self.parse_body()?;
        if !tok_matches!(self, Token::Else) {
            return Err(self
                .expected("'else'")
                .help("an `if` used as a value needs an `else`, so there is a value either way"));
        }
        self.consume(Token::Else)?;
        let e3 = self.parse_else(true)?;

        Ok(Expr::new(
            Expression::If(Box::new(e1), Box::new(e2), Box::new(e3)),
            Span::new(start, self.last_end()),
        ))
    }

    // Binary operators, from the loosest to the tightest: `..`, `||`, `&&`,
    // `==` and `!=`, the orderings, `+` and `-`, then `*`, `/`, and `%`. So
    // `a == b && c < d + 1` means `(a == b) && (c < (d + 1))`, as in C,
    // Rust, Go, and Swift.

    fn parse_assignment(&mut self) -> ParseResult<Expr> {
        let expr = self.parse_range()?;

        if tok_matches!(self, Token::Equal) {
            self.consume(Token::Equal)?;
            let equals = self.prev_token.clone();
            let value = self.parse_expr()?;

            match expr.kind {
                Expression::Var(v) => {
                    let span = expr.span.to(value.span);
                    return Ok(Expr::new(Expression::Assign(v, Box::new(value)), span));
                }
                Expression::Index(list, index) => {
                    let (l, i) = (snippet(&list), snippet(&index));
                    return Err(self
                        .error(equals.clone(), "cannot assign to an element of a list".to_string())
                        .help(format!(
                            "lists can't be changed; make a new one instead: `{l} = {l}.set({i}, ...)`"
                        )));
                }
                Expression::Field(..) => {
                    return Err(self
                        .error(equals.clone(), "cannot assign to a field".to_string())
                        .help(
                            "struct fields can't be changed; make a new value instead, \
                               e.g. `p = Point { x: 1, ..p }`",
                        ))
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

    /// `start..end`, which means `Iter::range(start, end)`: the ints from
    /// `start` up to, but not including, `end`.
    fn parse_range(&mut self) -> ParseResult<Expr> {
        let start = self.parse_logical_or()?;
        if !tok_matches!(self, Token::DotDot) {
            return Ok(start);
        }
        let (dots_start, _, dots_end) = self.next().unwrap();
        if tok_matches!(
            self,
            Token::LeftBrace | Token::RightParen | Token::Semicolon | Token::Comma
        ) {
            return Err(self
                .expected("the end of the range")
                .help("a range has a start and an end: `0..n`"));
        }
        let end = self.parse_logical_or()?;
        if tok_matches!(self, Token::DotDot) {
            let tok = self.peek().cloned();
            return Err(self
                .error(tok, "ranges can't be chained".to_string())
                .help("write one range, `a..b`"));
        }
        let span = start.span.to(end.span);
        // The callee's span is the `..`, which no other expression has.
        let callee = Expr::new(
            Expression::Path("Iter".into(), "range".into()),
            Span::new(dots_start, dots_end),
        );
        Ok(Expr::new(
            Expression::Call(Box::new(callee), vec![start, end]),
            span,
        ))
    }

    fn parse_equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_comparison()?;

        while self.matches(vec![Token::EqualEqual, Token::BangEqual]) {
            let op = self.next().unwrap();
            let right = self.parse_comparison()?;
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
        let mut expr = self.parse_equality()?;

        while tok_matches!(self, Token::And) {
            let op = self.next().unwrap();
            let right = self.parse_equality()?;
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
                    return Err(self.error(
                        tok,
                        "a call cannot have more than 255 arguments".to_string(),
                    ));
                }

                let span = Span::new(expr.span.start, self.last_end());
                expr = Expr::new(Expression::Call(Box::new(expr), es), span);
            } else if tok_matches!(self, Token::Dot) {
                self.next();
                let (name, name_span) = match self.peek().cloned() {
                    Some((start, Token::Ident(name), end)) => (name, Span::new(start, end)),
                    // A tuple's value by position: `pair.0`.
                    Some((start, Token::Int(i), end)) if i >= 0 => {
                        (Id::from(i.to_string()), Span::new(start, end))
                    }
                    _ => return Err(self.expected("a field name after '.'")),
                };
                self.next();
                let span = Span::new(expr.span.start, name_span.end);
                expr = Expr::new(Expression::Field(Box::new(expr), name, name_span), span);
            } else if tok_matches!(self, Token::Question) {
                self.next();
                let span = Span::new(expr.span.start, self.last_end());
                expr = Expr::new(Expression::Try(Box::new(expr)), span);
            } else if tok_matches!(self, Token::LeftBracket) {
                self.next();
                let index = self.with_structs(true, |p| p.parse_expr())?;
                self.consume(Token::RightBracket)?;
                let span = Span::new(expr.span.start, self.last_end());
                expr = Expr::new(Expression::Index(Box::new(expr), Box::new(index)), span);
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
            Token::TemplateStart(text) => {
                self.next();
                self.parse_template(text)?
            }
            Token::Ident(var) => {
                self.next();
                match var.as_str() {
                    "true" => Expression::True,
                    "false" => Expression::False,
                    "void" => Expression::Void,
                    _ if tok_matches!(self, Token::DoubleColon) => {
                        let mut path = self.parse_path_tail(var)?;
                        let name_span = Span::new(start, self.last_end());
                        if tok_matches!(self, Token::LeftBrace) && !self.no_struct {
                            self.parse_struct_lit(path.join("::").into(), name_span)?
                        } else {
                            if tok_matches!(self, Token::LeftBrace) {
                                self.struct_hint = Some(path.join("::").into());
                            }
                            let last = path.pop().expect("a path has two names or more");
                            Expression::Path(path.join("::").into(), last)
                        }
                    }
                    // `Some` for `Option::Some`, and the like.
                    v if prelude::variant_enum(v).is_some() => {
                        Expression::Path(prelude::variant_enum(v).unwrap().into(), var)
                    }
                    _ if tok_matches!(self, Token::LeftBrace) && !self.no_struct => {
                        self.parse_struct_lit(var, span)?
                    }
                    _ if tok_matches!(self, Token::LeftBrace) => {
                        self.struct_hint = Some(var.clone());
                        Expression::Var(var)
                    }
                    _ => Expression::Var(var),
                }
            }
            Token::LeftBracket => {
                let items =
                    self.parse_expr_seq(Token::LeftBracket, Token::Comma, Token::RightBracket)?;
                Expression::List(items)
            }
            Token::LeftParen => self.with_structs(true, |p| p.parse_paren_expr())?,
            // An `if` with an `else` is a value like any other, so it can be
            // an operand: `"\n".join(lines) + if done { "" } else { "\n" }`.
            Token::If => return self.parse_if_expr(),
            Token::Match => {
                self.next();
                let scrutinee = self.parse_head()?;
                let hint = self.struct_hint.take();
                let start = self.position;
                let arms = self.with_structs(true, |p| p.parse_arms());
                let arms = self.with_struct_hint(hint, start, arms)?;
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

    /// `(e)`, a grouping, or `(a, b, ...)`, a tuple, from the `(`.
    fn parse_paren_expr(&mut self) -> ParseResult<Expression> {
        self.next();
        if tok_matches!(self, Token::RightParen) {
            return Err(self.expected("an expression").help(
                "a tuple holds two or more values, e.g. `(1, \"a\")`; Aja has no `()` value, and `void` is the value of type `void`",
            ));
        }
        let first = self.parse_expr()?;
        if !tok_matches!(self, Token::Comma) {
            self.consume(Token::RightParen)?;
            return Ok(Expression::Grouping(Box::new(first)));
        }
        let mut items = vec![first];
        while tok_matches!(self, Token::Comma) {
            let comma = self.next();
            if tok_matches!(self, Token::RightParen) {
                if items.len() == 1 {
                    return Err(self
                        .error(comma, "a tuple holds two or more values".to_string())
                        .help("remove the ',' to use the value by itself"));
                }
                break;
            }
            items.push(self.parse_expr()?);
        }
        self.consume(Token::RightParen)?;
        Ok(Expression::Tuple(items))
    }

    /// The fields of a struct literal, after its name.
    fn parse_struct_lit(&mut self, name: Id, name_span: Span) -> ParseResult<Expression> {
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
                    return Err(self.expected("a field name").help(format!(
                        "struct literals name their fields: `{name} {{ x: 1, y: 2 }}`"
                    )))
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
            fields.push(FieldInit {
                name: field,
                span,
                value,
            });
            if tok_matches!(self, Token::Comma) {
                self.next();
            } else if !tok_matches!(self, Token::RightBrace) {
                return Err(self.expected("',' or '}'"));
            }
        }
        self.consume(Token::RightBrace)?;
        Ok(Expression::StructLit {
            name,
            name_span,
            fields,
            base,
        })
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
            } else if tok_matches!(self, Token::Return | Token::Break | Token::Continue) {
                // `None => return Err(e),`: an arm can leave instead of
                // giving a value, as if its body were `{ return Err(e); }`.
                (self.parse_stmt()?, false)
            } else {
                (Statement::Expr(self.parse_expr()?), false)
            };
            arms.push(Arm {
                pattern,
                guard,
                body: Box::new(body),
            });
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

    /// A pattern, or several separated by `|`, which matches a value any of
    /// them does: `4 | 6 | 9 | 11`, `Some(0) | None`.
    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        let first = self.parse_single_pattern()?;
        if !tok_matches!(self, Token::Pipe) {
            return Ok(first);
        }
        let start = first.span.start;
        let mut alternatives = vec![first];
        while tok_matches!(self, Token::Pipe) {
            self.next();
            alternatives.push(self.parse_single_pattern()?);
        }
        let span = Span::new(start, self.last_end());
        Ok(Pattern {
            kind: PatternKind::Or(alternatives),
            span,
        })
    }

    fn parse_single_pattern(&mut self) -> ParseResult<Pattern> {
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
            Token::TemplateStart(_) => {
                let tok = self.peek().cloned();
                return Err(self
                    .error(tok, "template strings can't be used as patterns".to_string())
                    .help("bind the value and compare it in a guard: `s if s == \"...\" => ...`, or write '\\{' for a literal brace"));
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
                self.pattern_after_name(name, Span::new(start, name_end))?
            }
            Token::LeftParen => self.parse_tuple_pattern()?,
            _ => return Err(self.expected("a pattern")),
        };
        Ok(Pattern {
            kind,
            span: Span::new(start, self.last_end()),
        })
    }

    /// The pattern that starts with the name `name` (at `span`), which has
    /// just been read: `_`, a literal, a variant, a struct, or a binding.
    fn pattern_after_name(&mut self, name: Id, span: Span) -> ParseResult<PatternKind> {
        Ok(match name.as_str() {
            "_" => PatternKind::Wildcard,
            "true" => PatternKind::Bool(true),
            "false" => PatternKind::Bool(false),
            "void" => PatternKind::Void,
            _ if tok_matches!(self, Token::DoubleColon) => {
                let mut path = self.parse_path_tail(name)?;
                let name_span = Span::new(span.start, self.last_end());
                if tok_matches!(self, Token::LeftBrace) {
                    self.parse_struct_pattern(path.join("::").into(), name_span)?
                } else {
                    let last = path.pop().expect("a path has two names or more");
                    self.parse_variant_pattern(path.join("::").into(), last)?
                }
            }
            // `Some(x)` for `Option::Some(x)`, and the like.
            v if prelude::variant_enum(v).is_some() => {
                self.parse_variant_pattern(prelude::variant_enum(v).unwrap().into(), name)?
            }
            _ if tok_matches!(self, Token::LeftBrace) => self.parse_struct_pattern(name, span)?,
            _ => PatternKind::Binding(Variable::new(name, None, span)),
        })
    }

    /// `(a, b, ...)`, a tuple pattern, or `(p)`, which is just `p`, from
    /// the `(`.
    fn parse_tuple_pattern(&mut self) -> ParseResult<PatternKind> {
        self.next();
        if tok_matches!(self, Token::RightParen) {
            return Err(self
                .expected("a pattern")
                .help("a tuple pattern matches two or more values, e.g. `(a, b)`"));
        }
        let first = self.parse_pattern()?;
        if !tok_matches!(self, Token::Comma) {
            self.consume(Token::RightParen)?;
            return Ok(first.kind);
        }
        let mut items = vec![first];
        while tok_matches!(self, Token::Comma) {
            let comma = self.next();
            if tok_matches!(self, Token::RightParen) {
                if items.len() == 1 {
                    return Err(self
                        .error(
                            comma,
                            "a tuple pattern matches two or more values".to_string(),
                        )
                        .help("remove the ','"));
                }
                break;
            }
            items.push(self.parse_pattern()?);
        }
        self.consume(Token::RightParen)?;
        Ok(PatternKind::Tuple(items))
    }

    /// The fields of a struct pattern, after its name: `{ x, y: 0, .. }`.
    fn parse_struct_pattern(&mut self, name: Id, name_span: Span) -> ParseResult<PatternKind> {
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
        Ok(PatternKind::Struct {
            name,
            name_span,
            fields,
            rest,
        })
    }

    /// The names after the first in a path, `::b::c` in `a::b::c`, giving
    /// every name in it.
    fn parse_path_tail(&mut self, first: Id) -> ParseResult<Vec<Id>> {
        let mut path = vec![first];
        while tok_matches!(self, Token::DoubleColon) {
            self.next();
            match self.peek().cloned() {
                Some((_, Token::Ident(name), _)) => {
                    self.next();
                    path.push(name);
                }
                _ => return Err(self.expected("a name after '::'")),
            }
        }
        Ok(path)
    }

    /// The values of a variant pattern, after its name: `(a, b)`, or nothing.
    fn parse_variant_pattern(
        &mut self,
        enum_name: crate::ast::Id,
        variant: crate::ast::Id,
    ) -> ParseResult<PatternKind> {
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
        Ok(PatternKind::Variant {
            enum_name,
            variant,
            fields,
            parens,
        })
    }

    /// Reject a prelude variant's name (`None`) where a new name is
    /// declared: written by itself, it always means the variant.
    fn check_new_name(&mut self, tok: Option<SrcSpan>, name: &str, what: &str) -> ParseResult<()> {
        if name == "self" {
            return Err(self
                .error(tok, format!("'self' can't be used as {what}"))
                .help("'self' can only be the first parameter of a function in an impl"));
        }
        if name == "void" {
            return Err(self
                .error(
                    tok,
                    format!("'void' is a value, so it can't be used as {what}"),
                )
                .help("choose a different name"));
        }
        match prelude::variant_enum(name) {
            Some(e) => Err(self
                .error(
                    tok,
                    format!("'{name}' is a variant of '{e}', so it can't be used as {what}"),
                )
                .help("choose a different name")),
            None => Ok(()),
        }
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

    /// Parse a type such as `int`, `str`, or `(int, bool) -> str`.
    ///
    /// A function type is its parameter types in parentheses, then `->` and
    /// the return type, which is always written (`() -> void`). Unlike a
    /// closure it has no `fn`, so a type can't be mistaken for a closure.
    fn parse_type_name(&mut self) -> ParseResult<Type> {
        let tok = self.peek().cloned();
        match tok {
            Some((_, Token::Fn, _)) => {
                // The old spelling, `fn(int) -> int` or `fn(int)`: say how to
                // write it now, giving the exact type when it parses.
                self.next();
                let err = self.error(tok, "function types are written without 'fn'".to_string());
                let fixed = if tok_matches!(self, Token::LeftParen) {
                    self.parse_fn_type().ok()
                } else {
                    None
                };
                Err(match fixed {
                    Some(t) => err.help(format!("write `{t}`")),
                    None => err.help(
                        "write the parameter types in parentheses and the return type after '->', \
                         e.g. `(int, str) -> bool`, or `() -> void` for a function that returns nothing",
                    ),
                })
            }
            Some((_, Token::LeftParen, _)) => self.parse_paren_type(),
            Some((start, Token::Ident(ref v), _)) => {
                let v = v.clone();
                self.next();
                let v: Id = if tok_matches!(self, Token::DoubleColon) {
                    self.parse_path_tail(v)?.join("::").into()
                } else {
                    v
                };
                if v == "any" {
                    return Err(self.error(tok, "unknown type 'any'".to_string()).help(
                        "Aja has no 'any' type; every value has one static type. \
                         Remove the annotation to have it inferred",
                    ));
                }
                let builtin = Type::try_from(v.to_string()).ok();
                let mut args = Vec::new();
                if tok_matches!(self, Token::LessThan) {
                    self.next();
                    loop {
                        args.push(self.parse_type_name()?);
                        if tok_matches!(self, Token::Comma) {
                            self.next();
                        } else {
                            break;
                        }
                    }
                    self.consume(Token::GreaterThan)?;
                }
                let span = Span::new(start, self.last_end());
                match builtin {
                    Some(_) if !args.is_empty() => Err(self
                        .error(tok, format!("type '{v}' takes no type arguments"))
                        .help(format!("write `{v}`"))),
                    Some(t) => Ok(t),
                    // Anything else that isn't built in names a struct, an
                    // enum, or a type parameter, which the checker looks up.
                    None => Ok(Type::Named(v, args, span)),
                }
            }
            _ => Err(self.expected("a type")),
        }
    }

    /// A type that starts with `(`: a function type, `(int, bool) -> str`,
    /// or a tuple type, `(int, bool)`, which has two or more types and no
    /// `->`.
    fn parse_paren_type(&mut self) -> ParseResult<Type> {
        let types = self.parse_paren_types()?;
        if tok_matches!(self, Token::RightArrow) {
            self.next();
            let ret_type = self.parse_type_name()?;
            return Ok(Type::Fn {
                param_types: types,
                ret_type: Box::new(ret_type),
            });
        }
        if types.len() >= 2 {
            return Ok(Type::Tuple(types));
        }
        let params: Vec<String> = types.iter().map(|t| t.to_string()).collect();
        Err(self.expected("'->' and a return type").help(format!(
            "a function type always names its return type: `({}) -> void` if it returns nothing",
            params.join(", ")
        )))
    }

    /// `(T, U, ...)`: the types in parentheses, which may end in a ','.
    fn parse_paren_types(&mut self) -> ParseResult<Vec<Type>> {
        self.consume(Token::LeftParen)?;
        let mut types = Vec::new();
        while !tok_matches!(self, Token::RightParen) {
            types.push(self.parse_type_name()?);
            if tok_matches!(self, Token::Comma) {
                self.next();
            } else {
                break;
            }
        }
        self.consume(Token::RightParen)?;
        Ok(types)
    }

    /// Parse the old spelling of a function type after `fn`, `(int) -> str`
    /// or `(int)`, where a missing `-> T` means `void`, to suggest a fix.
    fn parse_fn_type(&mut self) -> ParseResult<Type> {
        let param_types = self.parse_paren_types()?;
        let ret_type = self
            .parse_fn_return_type_annotation()?
            .unwrap_or(Type::Void);
        Ok(Type::Fn {
            param_types,
            ret_type: Box::new(ret_type),
        })
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

/// How a simple expression is written, for suggestions: a variable's name
/// or a literal, and `...` for anything else.
fn snippet(e: &Expr) -> String {
    match &e.kind {
        Expression::Var(name) => name.to_string(),
        Expression::Int(i) => i.to_string(),
        _ => "...".to_string(),
    }
}
