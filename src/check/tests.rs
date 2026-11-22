use super::*;
use crate::{lexer::tokenize, parse::Parser};

fn check(src: &str) -> Result<(), Vec<TypeError>> {
    let ast = Parser::new(tokenize(src).unwrap().into_iter()).parse().unwrap();
    check_program(&ast)
}

#[track_caller]
fn ok(src: &str) {
    if let Err(errors) = check(src) {
        let msgs: Vec<&str> = errors.iter().map(|e| e.message.as_str()).collect();
        panic!("expected no type errors, got {msgs:#?}");
    }
}

/// The single error a program has: its message, and the source text it points at.
#[track_caller]
fn error(src: &str) -> (String, String) {
    let errors = check(src).expect_err("program should have a type error");
    assert_eq!(errors.len(), 1, "expected one error, got {errors:#?}");
    let e = &errors[0];
    let line = src.lines().nth(e.span.start.line as usize - 1).unwrap();
    let text = line
        .chars()
        .skip(e.span.start.col as usize - 1)
        .take((e.span.end.col - e.span.start.col) as usize)
        .collect();
    (e.message.clone(), text)
}

#[track_caller]
fn help(src: &str) -> String {
    check(src).unwrap_err()[0].help.clone().expect("error should have help")
}

#[test]
fn well_typed_programs_pass() {
    ok("fn fib(n: int) -> int { if n < 2 { n } else { fib(n - 1) + fib(n - 2) } }
        fn main() -> int { fib(10) }");
    ok("fn main() { let x = 1; let y = x * 2; assert(y == 2); print(\"y =\", y, \"\\n\"); }");
    ok("fn main() -> bool { let a = true; a && !false || 1.5 < 2.0 }");
    ok("fn main() -> str { let s = \"a\"; s = \"b\"; s }");
}

#[test]
fn functions_can_be_used_before_they_are_defined() {
    ok("fn main() -> bool { is_even(10) }
        fn is_even(n: int) -> bool { if n == 0 { true } else { is_odd(n - 1) } }
        fn is_odd(n: int) -> bool { if n == 0 { false } else { is_even(n - 1) } }");
}

#[test]
fn closure_types_are_inferred_from_use() {
    // `x` is inferred from the `+`, the closure's return type from its body.
    ok("fn main() -> int { let inc = fn(x) { x + 1 }; inc(41) }");
    // Parameters of a higher-order closure are inferred from how they're used.
    ok("fn main() -> int { let twice = fn(f, x) { f(f(x)) }; twice(fn(y) { y * 2 }, 3) }");
    // A parameter's type can come from a later call.
    ok("fn main() -> float { let add = fn(a, b) { a + b }; add(1.5, 2.0) }");
    // Closures capture and mutate their environment.
    ok("fn counter() -> fn(int) -> int { let c = 0; fn(x) { c = c + x; c } }
        fn main() -> int { let k = counter(); k(1); k(5) }");
}

#[test]
fn closures_are_not_polymorphic() {
    // No let-generalization: `id` gets one type, fixed by its first use.
    let (msg, at) = error("fn main() { let id = fn(x) { x }; id(1); id(true); }");
    assert_eq!(msg, "mismatched types: argument 1 of function 'id' expects int, found bool");
    assert_eq!(at, "true");
}

#[test]
fn function_types_in_signatures() {
    ok("fn apply(f: fn(int) -> int, x: int) -> int { f(x) }
        fn main() -> int { apply(fn(n) { n * 3 }, 2) }");
    let (msg, at) = error(
        "fn apply(f: fn(int) -> int, x: int) -> int { f(x) }
fn main() -> int { apply(fn(s: str) { 1 }, 2) }",
    );
    assert_eq!(
        msg,
        "mismatched types: parameter 'f' of function 'apply' expects fn(int) -> int, found fn(str) -> int"
    );
    assert_eq!(at, "fn(s: str) { 1 }");
}

#[test]
fn signatures_must_be_annotated() {
    let (msg, at) = error("fn double(n) -> int { n * 2 } fn main() -> int { double(2) }");
    assert_eq!(msg, "parameter 'n' of function 'double' needs a type annotation");
    assert_eq!(at, "n");
    // The suggestion uses what the body reveals.
    assert_eq!(
        help("fn neg(b) -> bool { !b } fn main() -> bool { neg(true) }"),
        "add a type: `b: bool`"
    );
}

#[test]
fn missing_return_type_means_void() {
    ok("fn log(s: str) { print(s); } fn main() { log(\"x\") }");
    let (msg, at) = error("fn one() { 1 } fn main() { one(); }");
    assert_eq!(
        msg,
        "mismatched types: function 'one' has no return type, so it returns void, but this is int"
    );
    assert_eq!(at, "1");
    assert_eq!(
        help("fn one() { 1 } fn main() { one(); }"),
        "add a return type after the parameters: `-> int`"
    );
}

#[test]
fn return_statements() {
    ok("fn f(n: int) -> int { if n == 0 { return 1; } n * f(n - 1) } fn main() -> int { f(3) }");
    // Both branches return, so the `if` itself never produces a value.
    ok("fn f(b: bool) -> int { if b { return 1; } else { return 2; } } fn main() -> int { f(true) }");
    ok("fn f(b: bool) -> int { let x = if b { return 0; } else { 5 }; x } fn main() -> int { f(true) }");
    let (msg, at) = error("fn f() -> int { return \"no\"; } fn main() -> int { f() }");
    assert_eq!(msg, "mismatched types: function 'f' is declared to return int, but this is str");
    assert_eq!(at, "\"no\"");
    // A path that falls off the end of the body produces void.
    let (msg, _) = error("fn f(b: bool) -> int { if b { return 1; } } fn main() -> int { f(true) }");
    assert_eq!(msg, "mismatched types: function 'f' is declared to return int, but this is void");
}

#[test]
fn closure_return_types_must_agree() {
    let (msg, at) = error("fn main() { let f = fn(b: bool) { if b { return 1; } \"s\" }; f(true); }");
    assert_eq!(msg, "mismatched types: this closure returns int elsewhere, but this is str");
    assert_eq!(at, "\"s\"");
}

#[test]
fn operator_errors() {
    let (msg, at) = error("fn main() { let a = 1; a + \"s\"; }");
    assert_eq!(msg, "cannot apply '+' to int and str");
    assert_eq!(at, "a + \"s\"");
    let errors = check("fn main() { 1 + 2.0; }").unwrap_err();
    assert_eq!(errors[0].notes, vec!["int and float are not converted automatically"]);
    assert_eq!(error("fn main() { \"a\" < \"b\"; }").0, "cannot apply '<' to str and str");
    assert_eq!(error("fn main() { -true; }").0, "cannot apply '-' to bool; it expects an int or float");
    assert_eq!(error("fn main() { !1; }").0, "cannot apply '!' to int; it expects a bool");
    let (msg, at) = error("fn main() { true && 5; }");
    assert_eq!(msg, "'&&' expects bool operands, but the right operand is int");
    assert_eq!(at, "5");
    let (msg, _) = error("fn main() { let f = fn() { 1 }; f == f; }");
    assert_eq!(msg, "cannot apply '==' to fn() -> int and fn() -> int");
}

#[test]
fn numeric_operators_constrain_inferred_types() {
    // `a + b` restricts `a` to int or float, so a bool doesn't fit.
    let (msg, at) = error("fn main() { let add = fn(a, b) { a + b }; add(true, false); }");
    assert_eq!(msg, "mismatched types: argument 1 of function 'add' expects {number}, found bool");
    assert_eq!(at, "true");
}

#[test]
fn conditions_must_be_bool() {
    let (msg, at) = error("fn main() { if 1 { 2 } else { 3 }; }");
    assert_eq!(msg, "'if' condition must be a bool, found int");
    assert_eq!(at, "1");
    assert_eq!(error("fn main() { while \"x\" { } }").0, "'while' condition must be a bool, found str");
    let (msg, at) = error("fn main() { let x = if true { 1 } else { \"one\" }; }");
    assert_eq!(msg, "'if' and 'else' have incompatible types: int and str");
    assert_eq!(at, "\"one\"");
}

#[test]
fn let_and_assignment() {
    let (msg, at) = error("fn main() { let x: str = 5; }");
    assert_eq!(msg, "mismatched types: 'x' is declared as str but was given int");
    assert_eq!(at, "5");
    let (msg, at) = error("fn main() { let x = 1; x = \"s\"; }");
    assert_eq!(msg, "mismatched types: cannot assign str to 'x', which has type int");
    assert_eq!(at, "\"s\"");
    // Shadowing introduces a new variable, which may have a new type.
    ok("fn main() -> str { let a: int = 1; let a: str = \"s\"; a }");
}

#[test]
fn calls() {
    let (msg, at) = error("fn g(x: int) -> int { x } fn main() -> int { g(1, 2) }");
    assert_eq!(msg, "function 'g' takes 1 argument but 2 were given");
    assert_eq!(at, "g(1, 2)");
    assert_eq!(error("fn main() { assert(); }").0, "function 'assert' takes 1 argument but 0 were given");
    assert_eq!(
        error("fn main() { sleep(\"long\"); }").0,
        "mismatched types: argument 1 of function 'sleep' expects int, found str"
    );
    let (msg, at) = error("fn main() { let x = 1; x(2); }");
    assert_eq!(msg, "cannot call a value of type int ('x' is not a function)");
    assert_eq!(at, "x");
    assert_eq!(error("fn main() { let p = print; }").0, "'print' can only be called directly");
    ok("fn main() { println(\"n =\", 1, true); }");
}

#[test]
fn undefined_variables_suggest_close_names() {
    let (msg, at) = error("fn main() { let count = 1; cout; }");
    assert_eq!(msg, "cannot find variable 'cout' in this scope");
    assert_eq!(at, "cout");
    assert_eq!(
        help("fn main() { let count = 1; cout; }"),
        "a variable with a similar name exists: 'count'"
    );
    // Parameters stay inside their function.
    assert_eq!(
        error("fn g(x: int) -> int { x } fn main() -> int { g(4); x }").0,
        "cannot find variable 'x' in this scope"
    );
}

#[test]
fn types_that_cannot_be_inferred_need_annotations() {
    let (msg, at) = error("fn main() { let f = fn(x) { x }; }");
    assert_eq!(msg, "type annotations needed: cannot infer the type of parameter 'x'");
    assert_eq!(at, "x");
    let (msg, _) = error("fn main() { let add = fn(a: int, b) { a }; }");
    assert_eq!(msg, "type annotations needed: cannot infer the type of parameter 'b'");
}

#[test]
fn one_mistake_gives_one_error() {
    // The undefined variable isn't also reported as a bad operand, argument, etc.
    let (msg, _) = error("fn f(n: int) -> int { n } fn main() -> int { f(nope + 1) * 2 }");
    assert_eq!(msg, "cannot find variable 'nope' in this scope");
}

#[test]
fn infinite_types_are_rejected() {
    let (msg, at) = error("fn main() { let f = fn(g) { g(g) }; }");
    assert_eq!(msg, "mismatched types: argument 1 of function 'g' expects _, found fn(_) -> _");
    assert_eq!(at, "g");
    let errors = check("fn main() { let f = fn(g) { g(g) }; }").unwrap_err();
    assert!(errors[0].notes[0].contains("infinite type"));
}

#[test]
fn duplicate_and_builtin_names() {
    let (msg, at) = error("fn f() { } fn f() { } fn main() { }");
    assert_eq!(msg, "function 'f' is defined more than once");
    assert_eq!(at, "f");
    assert_eq!(
        error("fn clock() -> int { 1 } fn main() { }").0,
        "function 'clock' conflicts with the built-in function of the same name"
    );
}

#[test]
fn repl_keeps_bindings_only_from_inputs_that_check() {
    let mut c = Checker::new();
    let stmt = |src: &str| Parser::new(tokenize(src).unwrap().into_iter()).parse_stmt_only().unwrap();
    assert!(c.check_repl_stmt(&stmt("let x = 1")).is_ok());
    assert!(c.check_repl_stmt(&stmt("x + 1")).is_ok());
    assert!(c.check_repl_stmt(&stmt("let y: str = x")).is_err());
    // `y` was rejected, so it isn't defined.
    let errors = c.check_repl_stmt(&stmt("y")).unwrap_err();
    assert_eq!(errors[0].message, "cannot find variable 'y' in this scope");
    assert!(c.check_repl_stmt(&stmt("return 1")).is_err());
}
