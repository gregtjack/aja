use super::*;
use crate::{check, lexer::tokenize, parse::Parser};

/// Check, compile, and run a program, returning `main`'s result.
fn run(src: &str) -> Result<Value, RuntimeError> {
    let ast = Parser::new(tokenize(src).unwrap().into_iter()).parse().unwrap();
    let types = match check::check_program(&ast) {
        Ok(types) => types,
        Err(errors) => {
            let msgs: Vec<&str> = errors.iter().map(|e| e.message.as_str()).collect();
            panic!("test program has type errors: {msgs:#?}");
        }
    };
    let program = Compiler::new().program(&ast, &types).expect("program compiles");
    Vm::new().run_program(&program)
}

/// Run a program and show its result, e.g. `Int(3)`.
#[track_caller]
fn value(src: &str) -> String {
    match run(src) {
        Ok(v) => format!("{v:?}"),
        Err(e) => panic!("program failed: {}", e.message),
    }
}

/// The result of `main` with the given return type and body.
#[track_caller]
fn main_value(ret: &str, body: &str) -> String {
    value(&format!("fn main() -> {ret} {{ {body} }}"))
}

/// Run a program that should fail, returning its error message.
#[track_caller]
fn failure(src: &str) -> String {
    run(src).expect_err("program should fail").message
}

#[test]
fn every_builtin_is_implemented() {
    for b in BUILTINS {
        assert!(native::implementation(b.name).is_some(), "built-in '{}' has no implementation", b.name);
    }
}

#[test]
fn arithmetic_is_typed() {
    assert_eq!(main_value("int", "7 % 3"), "Int(1)");
    assert_eq!(main_value("int", "2 + 7 % 3 * 2"), "Int(4)");
    assert_eq!(main_value("int", "-7 / 2"), "Int(-3)");
    assert_eq!(main_value("float", "7.0 / 2.0"), "Float(3.5)");
    assert_eq!(main_value("float", "7.5 % 2.0"), "Float(1.5)");
    assert_eq!(main_value("float", "let x = 1.5; -x"), "Float(-1.5)");
    assert_eq!(main_value("int", "let x = 3; -x"), "Int(-3)");
}

#[test]
fn minus_is_an_operator() {
    assert_eq!(main_value("int", "let a = 5; let b = 2; a-b"), "Int(3)");
    assert_eq!(main_value("int", "let a = 5; a -1"), "Int(4)");
    assert_eq!(main_value("int", "5-1"), "Int(4)");
    assert_eq!(main_value("int", "-3 * -2"), "Int(6)");
}

#[test]
fn comparisons() {
    assert_eq!(main_value("bool", "1 < 2 && 2 <= 2 && 3 > 2 && 3 >= 3"), "Bool(true)");
    assert_eq!(main_value("bool", "2 < 1 || 3 <= 2 || 2 > 3 || 2 >= 3"), "Bool(false)");
    assert_eq!(main_value("bool", "1.5 < 2.5 && 2.5 > 1.5"), "Bool(true)");
    assert_eq!(main_value("bool", "(1 == 1) && (1 != 2)"), "Bool(true)");
    assert_eq!(main_value("bool", "(1.5 == 1.5) && (1.5 != 2.5)"), "Bool(true)");
    assert_eq!(main_value("bool", "(true == true) && (true != false)"), "Bool(true)");
    assert_eq!(main_value("bool", "(\"ab\" == \"ab\") && (\"ab\" != \"b\")"), "Bool(true)");
    assert_eq!(main_value("bool", "!(1 == 2)"), "Bool(true)");
}

#[test]
fn logical_operators_short_circuit() {
    let boom = "fn boom() -> bool { assert(false); true }";
    assert_eq!(value(&format!("{boom} fn main() -> bool {{ false && boom() }}")), "Bool(false)");
    assert_eq!(value(&format!("{boom} fn main() -> bool {{ true || boom() }}")), "Bool(true)");
    assert_eq!(failure(&format!("{boom} fn main() -> bool {{ true && boom() }}")), "assertion failed");
}

#[test]
fn control_flow() {
    assert_eq!(main_value("int", "if 1 < 2 { 10 } else { 20 }"), "Int(10)");
    assert_eq!(main_value("int", "let i = 0; let s = 0; while i < 5 { s = s + i; i = i + 1; } s"), "Int(10)");
    assert_eq!(main_value("int", "let x = 1; if true { x = 2; } x"), "Int(2)");
    assert_eq!(
        value("fn sign(n: int) -> int { if n < 0 { return -1; } if n == 0 { return 0; } 1 }
               fn main() -> int { sign(-5) * 100 + sign(0) * 10 + sign(7) }"),
        "Int(-99)"
    );
}

#[test]
fn recursion_uses_per_call_registers() {
    let src = "fn fib(n: int) -> int { if n < 2 { n } else { fib(n - 1) + fib(n - 2) } }
               fn main() -> int { fib(20) }";
    assert_eq!(value(src), "Int(6765)");
}

#[test]
fn deep_recursion_does_not_overflow_the_rust_stack() {
    let src = "fn sum(n: int) -> int { if n == 0 { 0 } else { n + sum(n - 1) } }
               fn main() -> int { sum(50000) }";
    assert_eq!(value(src), "Int(1250025000)");
}

#[test]
fn runaway_recursion_is_a_stack_overflow() {
    let msg = failure("fn f(n: int) -> int { f(n + 1) } fn main() -> int { f(0) }");
    assert!(msg.starts_with("stack overflow"), "got {msg}");
}

#[test]
fn functions_can_be_called_before_they_are_defined() {
    let src = "fn main() -> int { double(21) } fn double(x: int) -> int { x * 2 }";
    assert_eq!(value(src), "Int(42)");
}

#[test]
fn assignment_is_an_expression() {
    assert_eq!(main_value("int", "let a = 1; let b = (a = 5) + a; b"), "Int(10)");
    // The right operand changes `a` only after the left one has been read.
    assert_eq!(main_value("int", "let a = 1; a + (a = 5)"), "Int(6)");
    assert_eq!(main_value("bool", "let a = true; let b = false; a = b && a; a"), "Bool(false)");
}

#[test]
fn let_shadowing() {
    assert_eq!(main_value("int", "let a = 1; let a = a + 1; a"), "Int(2)");
    assert_eq!(main_value("str", "let a: int = 1; let a: str = \"s\"; a"), "Str(\"s\")");
    assert_eq!(main_value("int", "let a = 1; if true { let a = 2; } a"), "Int(1)");
}

#[test]
fn closures_capture_and_mutate() {
    let src = "fn mk() -> fn(int) -> int { let c = 0; fn(x) { c = c + x; c } }
               fn main() -> int { let k = mk(); k(1); k(1); k(5) }";
    assert_eq!(value(src), "Int(7)");
}

#[test]
fn closures_share_variables_with_their_function() {
    // The function sees the closure's writes, and the closure the function's.
    let src = "fn main() -> int {
                   let n = 1;
                   let bump = fn() { n = n * 10; };
                   bump();
                   n = n + 1;
                   bump();
                   n
               }";
    assert_eq!(value(src), "Int(110)");
}

#[test]
fn each_counter_has_its_own_variable() {
    let src = "fn counter() -> fn() -> int { let c = 0; fn() { c = c + 1; c } }
               fn main() -> int { let a = counter(); let b = counter(); a(); a(); b(); a() * 10 + b() }";
    assert_eq!(value(src), "Int(32)");
}

#[test]
fn closures_capture_through_several_levels() {
    let src = "fn main() -> int {
                   let x = 1;
                   let outer = fn() -> fn() -> int { fn() { x = x + 1; x } };
                   let inner = outer();
                   inner();
                   inner() + x
               }";
    assert_eq!(value(src), "Int(6)");
}

#[test]
fn closures_capture_parameters() {
    let src = "fn adder(n: int) -> fn(int) -> int { fn(x) { x + n } }
               fn main() -> int { let add2 = adder(2); add2(40) }";
    assert_eq!(value(src), "Int(42)");
}

#[test]
fn each_loop_iteration_has_its_own_variable() {
    let src = "fn main() -> int {
                   let first = fn() { 0 };
                   let i = 0;
                   while i < 3 {
                       let j = i;
                       if i == 0 { first = fn() { j }; }
                       i = i + 1;
                   }
                   first()
               }";
    assert_eq!(value(src), "Int(0)");
}

#[test]
fn shadowing_does_not_change_captured_variables() {
    let src = "fn main() -> int { let a = 1; let f = fn() { a }; let a = 2; f() * 10 + a }";
    assert_eq!(value(src), "Int(12)");
}

#[test]
fn functions_are_values() {
    let src = "fn apply(f: fn(int) -> int, x: int) -> int { f(x) }
               fn triple(n: int) -> int { n * 3 }
               fn main() -> int { apply(triple, 2) + apply(fn(n) { n + 1 }, 2) }";
    assert_eq!(value(src), "Int(9)");
    assert_eq!(main_value("int", "clock(); 1"), "Int(1)");
}

#[test]
fn values_display_like_print() {
    let shown = |src: &str| run(src).unwrap().to_string();
    assert_eq!(shown("fn main() -> float { 1.0 }"), "1.0");
    assert_eq!(shown("fn main() -> str { \"a\\tb\" }"), "a\tb");
    assert_eq!(shown("fn main() -> fn(int) -> int { fn(x: int) -> int { x } }"), "<fn (x: int) -> int>");
    assert_eq!(shown("fn main() -> fn() -> int { clock }"), "<native fn: clock>");
    assert_eq!(shown("fn main() { }"), "()");
}

#[test]
fn integer_errors() {
    assert_eq!(failure("fn main() -> int { 7 / 0 }"), "division by zero");
    assert_eq!(failure("fn main() -> int { 7 % 0 }"), "modulo by zero");
    assert_eq!(failure("fn main() -> int { 2147483647 + 1 }"), "integer overflow in '+'");
    assert_eq!(failure("fn main() -> int { let m = -2147483647 - 1; -m }"), "integer overflow in '-'");
    assert_eq!(failure("fn main() -> int { let m = -2147483647 - 1; m / -1 }"), "integer overflow in '/'");
}

#[test]
fn program_level_errors() {
    let compile = |src: &str| {
        let ast = Parser::new(tokenize(src).unwrap().into_iter()).parse().unwrap();
        let types = check::check_program(&ast).unwrap();
        Compiler::new().program(&ast, &types).err().map(|d| d.message)
    };
    assert!(compile("fn helper() { }").unwrap().starts_with("no 'main' function found"));
    assert_eq!(compile("fn main(x: int) { }").unwrap(), "'main' cannot take parameters");
}

/// The source text an error points at, plus its call-trace notes.
fn located(src: &str) -> (String, Vec<String>) {
    let err = run(src).expect_err("program should fail");
    let span = err.span.expect("error should have a location");
    let line = src.lines().nth(span.start.line as usize - 1).unwrap();
    let text: String = line
        .chars()
        .skip(span.start.col as usize - 1)
        .take((span.end.col - span.start.col) as usize)
        .collect();
    (text, err.to_diagnostic().notes)
}

#[test]
fn runtime_errors_point_at_the_failing_expression() {
    assert_eq!(located("fn main() -> int { let a = 0; 1 + 6 / a }").0, "6 / a");
    assert_eq!(located("fn main() { assert(1 == 2) }").0, "assert(1 == 2)");
}

#[test]
fn runtime_errors_carry_a_call_trace() {
    let (text, notes) = located(
        "fn c(x: int) -> int { x / 0 }\nfn b(x: int) -> int { c(x) }\nfn main() { b(1); }",
    );
    assert_eq!(text, "x / 0");
    assert_eq!(
        notes,
        vec![
            "in function 'c', called at line 2, column 23",
            "in function 'b', called at line 3, column 13",
        ]
    );

    // Recursive frames collapse into a single note.
    let (_, notes) = located(
        "fn down(n: int) -> int { if n == 0 { 1 / n } else { down(n - 1) } }\nfn main() { down(5); }",
    );
    assert_eq!(
        notes,
        vec![
            "in function 'down', called at line 1, column 53 (5 times)",
            "in function 'down', called at line 2, column 13",
        ]
    );

    // Closures are described by the name they were called through.
    let (_, notes) = located("fn main() { let f = fn() { assert(false); }; f(); }");
    assert_eq!(notes, vec!["in function 'f', called at line 1, column 46"]);
}

#[test]
fn repl_inputs_share_globals() {
    let mut checker = check::Checker::new();
    let mut compiler = Compiler::new();
    let mut vm = Vm::new();
    let mut eval = |src: &str| {
        let stmt = Parser::new(tokenize(src).unwrap().into_iter()).parse_stmt_only().unwrap();
        let types = checker.check_repl_stmt(&stmt).unwrap();
        let span = Span::new(crate::ast::Location::new(0, 1, 1), crate::ast::Location::new(0, 1, 1));
        let input = compiler.repl(&stmt, &types, span);
        format!("{:?}", vm.run_repl(input).unwrap())
    };
    assert_eq!(eval("let x = 1"), "Void");
    assert_eq!(eval("let add = fn(n: int) -> int { x = x + n; x }"), "Void");
    assert_eq!(eval("add(4)"), "Int(5)");
    assert_eq!(eval("x"), "Int(5)");
    assert_eq!(eval("{ let y = 2; let g = fn() -> int { y * x }; g() }"), "Int(10)");
    // A new `x` doesn't change the one `add` uses.
    assert_eq!(eval("let x = \"s\""), "Void");
    assert_eq!(eval("add(1)"), "Int(6)");
    assert_eq!(eval("x"), "Str(\"s\")");
}
