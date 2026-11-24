use super::*;
use crate::{check, lexer::tokenize, parse::Parser};

/// Check, compile, and run a program, returning the VM and `main`'s
/// result. The collector runs before every allocation, so a value the VM
/// forgot to keep reachable is freed, and its use then panics.
fn run_vm(src: &str) -> (Vm, Result<Value, RuntimeError>) {
    let ast = Parser::new(tokenize(src).unwrap().into_iter())
        .parse()
        .unwrap();
    let types = match check::check_program(&ast) {
        Ok(types) => types,
        Err(errors) => {
            let msgs: Vec<&str> = errors.iter().map(|e| e.message.as_str()).collect();
            panic!("test program has type errors: {msgs:#?}");
        }
    };
    let program = Compiler::new()
        .program(&ast, &types)
        .expect("program compiles");
    let mut vm = Vm::new();
    vm.heap.stress = true;
    let result = vm.run_program(&program);
    (vm, result)
}

fn run(src: &str) -> Result<Value, RuntimeError> {
    run_vm(src).1
}

/// A value as `{:?}` shows it, but with a string's contents, e.g.
/// `Str("hi")`, rather than its heap handle.
fn show(vm: &Vm, v: &Value) -> String {
    match v {
        Value::Str(gc) => format!("Str({:?})", vm.heap.str(*gc)),
        v => format!("{v:?}"),
    }
}

/// Run a program and show its result, e.g. `Int(3)`.
#[track_caller]
fn value(src: &str) -> String {
    match run_vm(src) {
        (vm, Ok(v)) => show(&vm, &v),
        (_, Err(e)) => panic!("program failed: {}", e.message),
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
    assert_eq!(
        main_value("bool", "1 < 2 && 2 <= 2 && 3 > 2 && 3 >= 3"),
        "Bool(true)"
    );
    assert_eq!(
        main_value("bool", "2 < 1 || 3 <= 2 || 2 > 3 || 2 >= 3"),
        "Bool(false)"
    );
    assert_eq!(main_value("bool", "1.5 < 2.5 && 2.5 > 1.5"), "Bool(true)");
    assert_eq!(main_value("bool", "1 == 1 && 1 != 2"), "Bool(true)");
    assert_eq!(main_value("bool", "1.5 == 1.5 && 1.5 != 2.5"), "Bool(true)");
    assert_eq!(
        main_value("bool", "true == true && true != false"),
        "Bool(true)"
    );
    assert_eq!(
        main_value("bool", "\"ab\" == \"ab\" && \"ab\" != \"b\""),
        "Bool(true)"
    );
    assert_eq!(main_value("bool", "!(1 == 2)"), "Bool(true)");
}

#[test]
fn logical_operators_short_circuit() {
    let boom = "fn boom() -> bool { assert(false); true }";
    assert_eq!(
        value(&format!("{boom} fn main() -> bool {{ false && boom() }}")),
        "Bool(false)"
    );
    assert_eq!(
        value(&format!("{boom} fn main() -> bool {{ true || boom() }}")),
        "Bool(true)"
    );
    assert_eq!(
        failure(&format!("{boom} fn main() -> bool {{ true && boom() }}")),
        "assertion failed"
    );
}

#[test]
fn control_flow() {
    assert_eq!(main_value("int", "if 1 < 2 { 10 } else { 20 }"), "Int(10)");
    assert_eq!(
        main_value(
            "int",
            "var i = 0; var s = 0; while i < 5 { s = s + i; i = i + 1; } s"
        ),
        "Int(10)"
    );
    assert_eq!(
        main_value("int", "var x = 1; if true { x = 2; } x"),
        "Int(2)"
    );
    assert_eq!(
        value(
            "fn sign(n: int) -> int { if n < 0 { return -1; } if n == 0 { return 0; } 1 }
               fn main() -> int { sign(-5) * 100 + sign(0) * 10 + sign(7) }"
        ),
        "Int(-99)"
    );
}

#[test]
fn recursion_uses_per_call_registers() {
    let src = "fn fib(n: int) -> int { if n < 2 { n } else { fib(n - 1) + fib(n - 2) } }
               fn main() -> int { fib(20) }";
    assert_eq!(value(src), "Int(6765)");
}

/// Run a program with the call stack limited to `limit` bytes.
fn run_limited(src: &str, limit: usize) -> Result<Value, RuntimeError> {
    let ast = Parser::new(tokenize(src).unwrap().into_iter())
        .parse()
        .unwrap();
    let types = check::check_program(&ast).unwrap();
    let program = Compiler::new().program(&ast, &types).unwrap();
    let mut vm = Vm::new();
    vm.stack_limit = limit;
    vm.run_program(&program)
}

#[test]
fn deep_recursion_does_not_overflow_the_rust_stack() {
    // 200,000 calls in progress at once.
    let src = "fn depth(n: int) -> int { if n == 0 { 0 } else { 1 + depth(n - 1) } }
               fn main() -> int { depth(200000) }";
    assert_eq!(value(src), "Int(200000)");
}

#[test]
fn runaway_recursion_is_a_stack_overflow() {
    let src = "fn f(n: int) -> int { 1 + f(n + 1) } fn main() -> int { f(0) }";
    let msg = run_limited(src, 1 << 20)
        .expect_err("should overflow")
        .message;
    assert!(
        msg.starts_with("stack overflow: the call stack has grown past 1 MB ("),
        "got {msg}"
    );
}

#[test]
fn tail_calls_run_in_constant_space() {
    // A million calls deep, with room for only a handful of frames.
    let limit = 4096;
    let count =
        "fn count(n: int, acc: int) -> int { if n == 0 { acc } else { count(n - 1, acc + 1) } }
                 fn main() -> int { count(1000000, 0) }";
    assert_eq!(
        format!("{:?}", run_limited(count, limit).unwrap()),
        "Int(1000000)"
    );

    // `return` and both branches of an `if` are tail positions, and mutual
    // recursion works too.
    let even = "fn is_even(n: int) -> bool { if n == 0 { return true; } return is_odd(n - 1); }
                fn is_odd(n: int) -> bool { if n == 0 { false } else { is_even(n - 1) } }
                fn main() -> bool { is_even(1000001) }";
    assert_eq!(
        format!("{:?}", run_limited(even, limit).unwrap()),
        "Bool(false)"
    );

    // Closures, including ones that capture variables, can be tail-called.
    let closure = "fn main() -> int {
                       let step = 1;
                       var f = fn(n: int) -> int { n };
                       f = fn(n: int) -> int { if n >= 1000000 { n } else { f(n + step) } };
                       f(0)
                   }";
    assert_eq!(
        format!("{:?}", run_limited(closure, limit).unwrap()),
        "Int(1000000)"
    );

    // A call that isn't last still needs its frame.
    let not_tail = "fn depth(n: int) -> int { if n == 0 { 0 } else { 1 + depth(n - 1) } }
                    fn main() -> int { depth(1000000) }";
    assert!(run_limited(not_tail, limit).is_err());
}

#[test]
fn tail_calls_pass_arguments_correctly() {
    // Arguments that read the caller's parameters, in a different order.
    let src = "fn swap(a: int, b: int, n: int) -> int { if n == 0 { a * 10 + b } else { swap(b, a, n - 1) } }
               fn main() -> int { swap(1, 2, 3) }";
    assert_eq!(value(src), "Int(21)");
    // Tail calls to a function with more registers than the caller, and to
    // a built-in.
    let src =
        "fn big(x: int) -> int { let a = x + 1; let b = a + 1; let c = b + 1; let d = c + 1; d }
               fn small(x: int) -> int { big(x) }
               fn main() -> int { small(1) }";
    assert_eq!(value(src), "Int(5)");
    assert_eq!(
        value("fn main() { println(\"tail-called built-in\") }"),
        "Void"
    );
}

#[test]
fn functions_can_be_called_before_they_are_defined() {
    let src = "fn main() -> int { double(21) } fn double(x: int) -> int { x * 2 }";
    assert_eq!(value(src), "Int(42)");
}

#[test]
fn assignment_is_an_expression() {
    assert_eq!(
        main_value("int", "var a = 1; let b = (a = 5) + a; b"),
        "Int(10)"
    );
    // The right operand changes `a` only after the left one has been read.
    assert_eq!(main_value("int", "var a = 1; a + (a = 5)"), "Int(6)");
    assert_eq!(
        main_value("bool", "var a = true; let b = false; a = b && a; a"),
        "Bool(false)"
    );
}

#[test]
fn let_shadowing() {
    assert_eq!(main_value("int", "let a = 1; let a = a + 1; a"), "Int(2)");
    assert_eq!(
        main_value("str", "let a: int = 1; let a: str = \"s\"; a"),
        "Str(\"s\")"
    );
    assert_eq!(
        main_value("int", "let a = 1; if true { let a = 2; } a"),
        "Int(1)"
    );
}

#[test]
fn closures_capture_and_mutate() {
    let src = "fn mk() -> (int) -> int { var c = 0; fn(x) { c = c + x; c } }
               fn main() -> int { let k = mk(); k(1); k(1); k(5) }";
    assert_eq!(value(src), "Int(7)");
}

#[test]
fn closures_share_variables_with_their_function() {
    // The function sees the closure's writes, and the closure the function's.
    let src = "fn main() -> int {
                   var n = 1;
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
    let src = "fn counter() -> () -> int { var c = 0; fn() { c = c + 1; c } }
               fn main() -> int { let a = counter(); let b = counter(); a(); a(); b(); a() * 10 + b() }";
    assert_eq!(value(src), "Int(32)");
}

/// How many `NewCell` instructions a program compiles to, in all its
/// functions and the closures in them (not counting the prelude's).
fn cells_made(src: &str) -> usize {
    fn count(p: &Proto) -> usize {
        let here = p
            .code
            .iter()
            .filter(|op| matches!(op, Op::NewCell { .. }))
            .count();
        here + p.protos.iter().map(|p| count(p)).sum::<usize>()
    }
    let ast = Parser::new(tokenize(src).unwrap().into_iter())
        .parse()
        .unwrap();
    let types = check::check_program(&ast).unwrap();
    let program = Compiler::new().program(&ast, &types).unwrap();
    program
        .functions
        .iter()
        .filter(|(_, p)| {
            p.spans
                .first()
                .is_some_and(|s| s.start.source != crate::ast::Source::Prelude)
        })
        .map(|(_, p)| count(p))
        .sum()
}

#[test]
fn only_captured_vars_get_cells() {
    // Captured `let`s, parameters, and pattern bindings are copied into the
    // closure, and a closure that calls itself captures itself.
    let src = "fn make(n: int) -> () -> int {
                   let m = n * 2;
                   let f = fn(k: int) -> int { if k == 0 { m } else { f(k - 1) } };
                   match Some(n) { Some(p) => fn() { f(p) + n + m }, None => fn() { 0 } }
               }
               fn main() -> int { make(3)() }";
    assert_eq!(cells_made(src), 0);
    assert_eq!(value(src), "Int(15)");
    // A captured `var` is shared, so it needs one; an uncaptured one doesn't.
    let src =
        "fn main() -> int { var a = 1; var b = 2; let f = fn() { a = a + 1; }; f(); b = a; b }";
    assert_eq!(cells_made(src), 1);
    assert_eq!(value(src), "Int(2)");
}

#[test]
fn closures_can_call_themselves() {
    let src = "fn main() -> int {
                   let fact = fn(n: int) { if n == 0 { 1 } else { n * fact(n - 1) } };
                   let base = 100;
                   let outer = fn() -> int {
                       let down = fn(k: int) -> int { if k == 0 { base } else { down(k - 1) + 1 } };
                       down(3)
                   };
                   fact(5) + outer()
               }";
    assert_eq!(value(src), "Int(223)");
    // Through its own variable, in tail position, in constant space.
    let src = "fn main() -> int {
                   let count = fn(n: int, acc: int) -> int { if n == 0 { acc } else { count(n - 1, acc + 1) } };
                   count(100000, 0)
               }";
    assert_eq!(
        format!("{:?}", run_limited(src, 4096).unwrap()),
        "Int(100000)"
    );
    // Through a closure nested inside it, and with an outer `f` shadowed.
    let src = "fn main() -> int {
                   let f = 7;
                   let f = fn(n: int) -> int { let again = fn() -> int { f(n - 1) }; if n == 0 { 0 } else { 1 + again() } };
                   f(4)
               }";
    assert_eq!(value(src), "Int(4)");
    // A `var` closure calls whatever is in the variable when it runs.
    let src = "fn main() -> int {
                   var f = fn(n: int) -> int { if n == 0 { 0 } else { 10 + f(n - 1) } };
                   let first = f;
                   f = fn(n: int) -> int { 1 };
                   first(3)
               }";
    assert_eq!(value(src), "Int(11)");
    // A value that isn't a closure still sees the earlier variable.
    assert_eq!(
        value("fn main() -> int { let a = 1; let a = a + 1; a }"),
        "Int(2)"
    );
}

#[test]
fn closures_capture_through_several_levels() {
    let src = "fn main() -> int {
                   var x = 1;
                   let outer = fn() -> () -> int { fn() { x = x + 1; x } };
                   let inner = outer();
                   inner();
                   inner() + x
               }";
    assert_eq!(value(src), "Int(6)");
}

#[test]
fn closures_capture_parameters() {
    let src = "fn adder(n: int) -> (int) -> int { fn(x) { x + n } }
               fn main() -> int { let add2 = adder(2); add2(40) }";
    assert_eq!(value(src), "Int(42)");
}

#[test]
fn each_loop_iteration_has_its_own_variable() {
    let src = "fn main() -> int {
                   var first = fn() { 0 };
                   var i = 0;
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
    let src = "fn apply(f: (int) -> int, x: int) -> int { f(x) }
               fn triple(n: int) -> int { n * 3 }
               fn main() -> int { apply(triple, 2) + apply(fn(n) { n + 1 }, 2) }";
    assert_eq!(value(src), "Int(9)");
    assert_eq!(main_value("int", "clock(); 1"), "Int(1)");
}

#[test]
fn values_display_like_print() {
    let shown = |src: &str| {
        let (vm, result) = run_vm(src);
        vm.heap.display(&result.unwrap())
    };
    assert_eq!(shown("fn main() -> float { 1.0 }"), "1.0");
    assert_eq!(shown("fn main() -> str { \"a\\tb\" }"), "a\tb");
    assert_eq!(
        shown("fn main() -> (int) -> int { fn(x: int) -> int { x } }"),
        "<fn (x: int) -> int>"
    );
    assert_eq!(
        shown("fn main() -> () -> int { clock }"),
        "<native fn: clock>"
    );
    assert_eq!(shown("fn main() { }"), "void");
}

#[test]
fn integer_errors() {
    assert_eq!(failure("fn main() -> int { 7 / 0 }"), "division by zero");
    assert_eq!(failure("fn main() -> int { 7 % 0 }"), "modulo by zero");
    assert_eq!(
        failure("fn main() -> int { 9223372036854775807 + 1 }"),
        "integer overflow in '+'"
    );
    assert_eq!(
        failure("fn main() -> int { let m = -9223372036854775807 - 1; -m }"),
        "integer overflow in '-'"
    );
    assert_eq!(
        failure("fn main() -> int { let m = -9223372036854775807 - 1; m / -1 }"),
        "integer overflow in '/'"
    );
}

#[test]
fn ints_are_64_bit() {
    assert_eq!(main_value("int", "2147483647 + 1"), "Int(2147483648)");
    assert_eq!(main_value("int", "3_000_000_000 * 3"), "Int(9000000000)");
    assert_eq!(
        main_value("int", "-9223372036854775807 - 1"),
        "Int(-9223372036854775808)"
    );
    // Milliseconds since 1970 fit, and are after 2020-01-01.
    assert_eq!(
        main_value("bool", "clock() > 1_577_836_800_000"),
        "Bool(true)"
    );
}

#[test]
fn floats_are_64_bit() {
    // An f32 would round these; an f64 keeps them.
    assert_eq!(main_value("float", "16777217.0"), "Float(16777217.0)");
    assert_eq!(
        main_value("float", "0.1 + 0.2"),
        "Float(0.30000000000000004)"
    );
}

#[test]
fn program_level_errors() {
    let compile = |src: &str| {
        let ast = Parser::new(tokenize(src).unwrap().into_iter())
            .parse()
            .unwrap();
        let types = check::check_program(&ast).unwrap();
        Compiler::new()
            .program(&ast, &types)
            .err()
            .map(|d| d.message)
    };
    assert!(compile("fn helper() { }")
        .unwrap()
        .starts_with("no 'main' function found"));
    assert_eq!(
        compile("fn main(x: int) { }").unwrap(),
        "'main' cannot take parameters"
    );
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
    assert_eq!(
        located("fn main() -> int { let a = 0; 1 + 6 / a }").0,
        "6 / a"
    );
    assert_eq!(located("fn main() { assert(1 == 2) }").0, "assert(1 == 2)");
}

#[test]
fn runtime_errors_carry_a_call_trace() {
    let (text, notes) = located(
        "fn c(x: int) -> int { x / 0 }\nfn b(x: int) -> int { c(x) + 1 }\nfn main() { b(1); }",
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
        "fn down(n: int) -> int { if n == 0 { 1 / n } else { 0 + down(n - 1) } }\nfn main() { down(5); }",
    );
    assert_eq!(
        notes,
        vec![
            "in function 'down', called at line 1, column 57 (5 times)",
            "in function 'down', called at line 2, column 13",
        ]
    );

    // A tail call replaces its caller's frame, so the caller isn't listed,
    // but the note doesn't mention tail calls: it reads like any other call.
    let (_, notes) =
        located("fn c(x: int) -> int { x / 0 }\nfn b(x: int) -> int { c(x) }\nfn main() { b(1); }");
    assert_eq!(notes, vec!["in function 'c', called at line 2, column 23"]);

    // Closures are described by the name they were called through.
    let (_, notes) = located("fn main() { let f = fn() { assert(false); }; f(); }");
    assert_eq!(notes, vec!["in function 'f', called at line 1, column 46"]);
}

#[test]
fn repl_inputs_share_globals() {
    let mut checker = check::Checker::new();
    let mut compiler = Compiler::new();
    let mut vm = Vm::new();
    vm.heap.stress = true;
    let mut eval = |src: &str| {
        let stmt = Parser::new(tokenize(src).unwrap().into_iter())
            .parse_stmt_only()
            .unwrap();
        let types = checker.check_repl_stmt(&stmt).unwrap();
        let span = Span::new(
            crate::ast::Location::new(0, 1, 1),
            crate::ast::Location::new(0, 1, 1),
        );
        let input = compiler.repl(&stmt, &types, span);
        let v = vm.run_repl(&input).unwrap();
        show(&vm, &v)
    };
    assert_eq!(eval("var x = 1"), "Void");
    assert_eq!(eval("let add = fn(n: int) -> int { x = x + n; x }"), "Void");
    assert_eq!(eval("add(4)"), "Int(5)");
    assert_eq!(eval("x"), "Int(5)");
    assert_eq!(
        eval("{ let y = 2; let g = fn() -> int { y * x }; g() }"),
        "Int(10)"
    );
    // A new `x` doesn't change the one `add` uses.
    assert_eq!(eval("let x = \"s\""), "Void");
    assert_eq!(eval("add(1)"), "Int(6)");
    assert_eq!(eval("x"), "Str(\"s\")");
    // A top-level closure can call itself.
    assert_eq!(
        eval("let f = fn(n: int) -> int { if n == 0 { 0 } else { 2 + f(n - 1) } }"),
        "Void"
    );
    assert_eq!(eval("f(4)"), "Int(8)");
}

/// How many heap objects are left once `src` has run and the collector has
/// freed what is unreachable, not counting the prelude's functions. Its own
/// top-level functions are always left.
fn live_after(src: &str) -> usize {
    live_with_prelude(src) - prelude_objects()
}

fn live_with_prelude(src: &str) -> usize {
    let (mut vm, result) = run_vm(src);
    result.expect("program runs");
    vm.collect_garbage();
    vm.heap.live()
}

/// The heap objects the prelude leaves: one per function in it.
fn prelude_objects() -> usize {
    // An empty program leaves only its `main` besides the prelude.
    live_with_prelude("fn main() { }") - 1
}

#[test]
fn cycles_through_closures_are_freed() {
    // `f` captures itself: a cycle.
    let recursive = "fn make() -> int {
                         let f = fn(n: int) -> int { if n == 0 { 0 } else { f(n - 1) } };
                         f(3)
                     }
                     fn main() { var i = 0; while i < 200 { make(); i = i + 1; } }";
    assert_eq!(live_after(recursive), 2);

    // Two closures that call each other through captured variables.
    let mutual = "fn make() -> bool {
                      var even = fn(n: int) -> bool { true };
                      let odd = fn(n: int) -> bool { if n == 0 { false } else { even(n - 1) } };
                      even = fn(n: int) -> bool { if n == 0 { true } else { odd(n - 1) } };
                      even(10)
                  }
                  fn main() { var i = 0; while i < 200 { assert(make()); i = i + 1; } }";
    assert_eq!(live_after(mutual), 2);
}

#[test]
fn reachable_objects_survive_collection() {
    // Every allocation collects (stress mode), while counters and their
    // cells are only reachable from registers and other closures.
    let src = "fn counter() -> () -> int { var c = 0; fn() { c = c + 1; c } }
               fn main() -> int {
                   let a = counter();
                   var total = 0;
                   var i = 0;
                   while i < 50 {
                       let b = counter();
                       b();
                       total = total + a() + b();
                       i = i + 1;
                   }
                   total
               }";
    assert_eq!(value(src), "Int(1375)");
}

#[test]
fn collection_runs_without_stress_mode() {
    // Enough garbage to pass the collector's threshold several times.
    let src = "fn main() -> int {
                   var i = 0;
                   let keep = fn() -> int { i };
                   while i < 10000 { var c = 0; let g = fn() -> int { c = c + 1; c }; g(); i = i + 1; }
                   keep()
               }";
    let ast = Parser::new(tokenize(src).unwrap().into_iter())
        .parse()
        .unwrap();
    let types = check::check_program(&ast).unwrap();
    let program = Compiler::new().program(&ast, &types).unwrap();
    let mut vm = Vm::new();
    assert_eq!(
        format!("{:?}", vm.run_program(&program).unwrap()),
        "Int(10000)"
    );
    assert!(vm.heap.live() < 3000, "{} objects live", vm.heap.live());
}

// ---- Structs, enums, and match ----------------------------------------------

const DATA: &str = "
struct Point { x: int, y: int }
enum Shape { Circle(Point, int), Rect(Point, Point), Empty }
enum Seq { Nil, Cons(int, Seq) }
fn range(n: int) -> Seq { var n = n; var l = Seq::Nil; while n > 0 { n = n - 1; l = Seq::Cons(n, l); } l }
";

/// The value `main` returns, shown as `print` shows it, with the
/// definitions in [`DATA`].
#[track_caller]
fn data_value(ret: &str, body: &str) -> String {
    let (vm, result) = run_vm(&format!("{DATA} fn main() -> {ret} {{ {body} }}"));
    match result {
        Ok(v) => vm.heap.display(&v),
        Err(e) => panic!("program failed: {}", e.message),
    }
}

#[test]
fn structs_are_built_and_read() {
    assert_eq!(
        data_value("Point", "Point { x: 1, y: 2 }"),
        "Point { x: 1, y: 2 }"
    );
    // Fields can be written in any order.
    assert_eq!(
        data_value("Point", "Point { y: 2, x: 1 }"),
        "Point { x: 1, y: 2 }"
    );
    assert_eq!(
        data_value("int", "let p = Point { x: 3, y: 4 }; p.x * 10 + p.y"),
        "34"
    );
    assert_eq!(
        data_value("Point", "let x = 5; let y = 6; Point { x, y }"),
        "Point { x: 5, y: 6 }"
    );
    assert_eq!(
        data_value("Point", "let p = Point { x: 1, y: 2 }; Point { y: 9, ..p }"),
        "Point { x: 1, y: 9 }"
    );
    // Fields of fields, and of call results.
    let src = "struct Line { a: Point, b: Point }
               fn line() -> Line { Line { a: Point { x: 1, y: 2 }, b: Point { x: 3, y: 4 } } }";
    assert_eq!(
        value(&format!("{DATA} {src} fn main() -> int {{ line().b.x }}")),
        "Int(3)"
    );
}

#[test]
fn fields_are_evaluated_in_the_order_written() {
    let src = "fn main() -> int {
                   var order = 0;
                   let note = fn(n: int) -> int { order = order * 10 + n; n };
                   let p = Point { y: note(2), x: note(1) };
                   order * 100 + p.x * 10 + p.y
               }";
    assert_eq!(value(&format!("{DATA} {src}")), "Int(2112)");
}

#[test]
fn enums_are_built_and_shown() {
    assert_eq!(data_value("Shape", "Shape::Empty"), "Shape::Empty");
    assert_eq!(
        data_value(
            "Shape",
            "Shape::Rect(Point { x: 0, y: 0 }, Point { x: 2, y: 3 })"
        ),
        "Shape::Rect(Point { x: 0, y: 0 }, Point { x: 2, y: 3 })"
    );
    assert_eq!(
        data_value("Seq", "range(3)"),
        "Seq::Cons(0, Seq::Cons(1, Seq::Cons(2, Seq::Nil)))"
    );
    // Strings inside a value are quoted, so `\"1\"` and `1` look different.
    let src = "enum E { S(str) } fn main() -> E { E::S(\"hi\") }";
    let (vm, result) = run_vm(src);
    assert_eq!(vm.heap.display(&result.unwrap()), "E::S(\"hi\")");
}

#[test]
fn match_selects_the_first_matching_arm() {
    let area = "fn area(s: Shape) -> int {
                    match s {
                        Shape::Circle(_, r) => 3 * r * r,
                        Shape::Rect(a, b) => (b.x - a.x) * (b.y - a.y),
                        Shape::Empty => 0,
                    }
                }";
    let main = "area(Shape::Circle(Point { x: 0, y: 0 }, 2)) * 10000
                + area(Shape::Rect(Point { x: 1, y: 1 }, Point { x: 4, y: 3 })) * 100
                + area(Shape::Empty)";
    assert_eq!(
        value(&format!("{DATA} {area} fn main() -> int {{ {main} }}")),
        "Int(120600)"
    );

    let describe = "fn describe(p: Point) -> str {
                        match p {
                            Point { x: 0, y: 0 } => \"origin\",
                            Point { x: 0, .. } => \"y axis\",
                            Point { y: 0, .. } => \"x axis\",
                            Point { x, y } if x == y => \"diagonal\",
                            Point { x: -1, y: _ } => \"left\",
                            _ => \"elsewhere\",
                        }
                    }";
    for (p, expected) in [
        ("0, y: 0", "origin"),
        ("0, y: 5", "y axis"),
        ("5, y: 0", "x axis"),
        ("4, y: 4", "diagonal"),
        ("-1, y: 3", "left"),
        ("2, y: 3", "elsewhere"),
    ] {
        let src = format!("{DATA} {describe} fn main() -> str {{ describe(Point {{ x: {p} }}) }}");
        assert_eq!(
            value(&src),
            format!("Str({expected:?})"),
            "for Point {{ x: {p} }}"
        );
    }
}

#[test]
fn match_on_literals() {
    let src = "fn name(n: int) -> str { match n { 1 => \"one\", 2 => \"two\", _ => \"many\" } }
               fn yes(b: bool) -> int { match b { true => 1, false => 0 } }
               fn greet(s: str) -> int { match s { \"hi\" => 1, \"bye\" => 2, _ => 0 } }
               fn main() -> str {
                   assert(yes(true) == 1); assert(yes(false) == 0);
                   assert(greet(\"hi\") == 1); assert(greet(\"bye\") == 2); assert(greet(\"x\") == 0);
                   name(2)
               }";
    assert_eq!(value(src), "Str(\"two\")");
}

#[test]
fn nested_patterns() {
    let src = "fn second(l: Seq) -> int {
                   match l {
                       Seq::Cons(_, Seq::Cons(x, _)) => x,
                       Seq::Cons(x, Seq::Nil) => -x,
                       Seq::Nil => -100,
                   }
               }
               fn main() -> int { second(range(5)) * 100 + second(Seq::Cons(7, Seq::Nil)) * 10 + second(Seq::Nil) }";
    assert_eq!(value(&format!("{DATA} {src}")), "Int(-70)");
}

#[test]
fn match_is_an_expression_and_a_statement() {
    assert_eq!(
        data_value(
            "int",
            "var n = 0; match Shape::Empty { Shape::Empty => { n = 5; }, _ => { } } n"
        ),
        "5"
    );
    assert_eq!(
        data_value(
            "int",
            "let s = Shape::Empty; 1 + match s { Shape::Empty => 1, _ => 2 } * 10"
        ),
        "11"
    );
    // A `return` inside an arm returns from the function.
    let src = "fn f(s: Shape) -> int { let n = match s { Shape::Empty => { return 7; }, _ => 1 }; n * 2 }
               fn main() -> int { f(Shape::Empty) * 10 + f(Shape::Circle(Point { x: 0, y: 0 }, 1)) }";
    assert_eq!(value(&format!("{DATA} {src}")), "Int(72)");
}

#[test]
fn guards_see_bindings_and_fall_through() {
    let src = "fn classify(l: Seq) -> int {
                   match l {
                       Seq::Cons(n, _) if n > 10 => 2,
                       Seq::Cons(n, _) if n > 0 => 1,
                       _ => 0,
                   }
               }
               fn main() -> int { classify(Seq::Cons(50, Seq::Nil)) * 100 + classify(Seq::Cons(5, Seq::Nil)) * 10 + classify(Seq::Cons(-1, Seq::Nil)) }";
    assert_eq!(value(&format!("{DATA} {src}")), "Int(210)");
    // A guard that changes the value matched on doesn't change what later arms see.
    let src = "fn main() -> int {
                   var n = 1;
                   let bump = fn() { n = 2; false };
                   match n { _ if bump() => 0, 1 => 1, _ => 2 }
               }";
    assert_eq!(value(src), "Int(1)");
}

#[test]
fn closures_capture_pattern_bindings() {
    let src = "fn adder(l: Seq) -> (int) -> int {
                   match l { Seq::Cons(n, _) => fn(x) { x + n }, Seq::Nil => fn(x) { x } }
               }
               fn main() -> int { let f = adder(Seq::Cons(40, Seq::Nil)); f(2) }";
    assert_eq!(value(&format!("{DATA} {src}")), "Int(42)");
}

#[test]
fn match_arms_are_tail_positions() {
    let src = "fn len(l: Seq, acc: int) -> int { match l { Seq::Nil => acc, Seq::Cons(_, t) => len(t, acc + 1) } }
               fn main() -> int { len(range(100000), 0) }";
    assert_eq!(
        format!("{:?}", run_limited(&format!("{DATA} {src}"), 4096).unwrap()),
        "Int(100000)"
    );
}

#[test]
fn structural_equality() {
    assert_eq!(
        data_value("bool", "Point { x: 1, y: 2 } == Point { x: 1, y: 2 }"),
        "true"
    );
    assert_eq!(
        data_value("bool", "Point { x: 1, y: 2 } != Point { x: 1, y: 3 }"),
        "true"
    );
    assert_eq!(data_value("bool", "Shape::Empty == Shape::Empty"), "true");
    assert_eq!(
        data_value(
            "bool",
            "Shape::Empty == Shape::Circle(Point { x: 0, y: 0 }, 1)"
        ),
        "false"
    );
    assert_eq!(data_value("bool", "range(3) == range(3)"), "true");
    assert_eq!(data_value("bool", "range(3) == range(4)"), "false");
}

#[test]
fn long_lists_are_compared_and_shown_without_deep_recursion() {
    // Without the stress-mode collector, which would make building these
    // lists quadratic.
    let run = |ret: &str, body: &str| {
        let src = format!("{DATA} fn main() -> {ret} {{ {body} }}");
        let ast = Parser::new(tokenize(&src).unwrap().into_iter())
            .parse()
            .unwrap();
        let types = check::check_program(&ast).unwrap();
        let program = Compiler::new().program(&ast, &types).unwrap();
        let mut vm = Vm::new();
        let v = vm.run_program(&program).unwrap();
        vm.heap.display(&v)
    };
    let shown = run("Seq", "range(200000)");
    assert!(shown.starts_with("Seq::Cons(0, Seq::Cons(1, "));
    assert!(shown.ends_with(&format!("Seq::Cons(199999, Seq::Nil{}", ")".repeat(200000))));
    assert_eq!(run("bool", "range(200000) == range(200000)"), "true");
    // `Seq::Nil` is declared first, so a shorter sequence sorts first.
    assert_eq!(
        run(
            "bool",
            "range(200000) <= range(200000) && range(199999) < range(200000)"
        ),
        "true"
    );
}

#[test]
fn struct_and_enum_values_are_collected() {
    // Nothing a program builds outlives it; only the top-level functions
    // stay, as in a program that builds nothing.
    let busy = format!(
        "{DATA} fn main() {{
             var i = 0;
             var keep = range(10);
             while i < 20 {{ range(50); keep = Seq::Cons(i, keep); i = i + 1; }}
             assert(keep != Seq::Nil);
         }}"
    );
    let idle = format!("{DATA} fn main() {{ }}");
    assert_eq!(live_after(&busy), live_after(&idle));
}

#[test]
fn statements_ending_in_a_block_need_no_semicolon() {
    // An `if` / `else` followed by more statements.
    assert_eq!(
        main_value(
            "int",
            "var x = 0; if x == 0 { x = 1; } else { x = 2; } x = x * 10; x"
        ),
        "Int(10)"
    );
    // A nested block keeps its statements, and its scope.
    assert_eq!(
        main_value("int", "var a = 1; { var a = 2; a = a + 1; } a"),
        "Int(1)"
    );
    assert_eq!(main_value("int", "{ var b = 2; b = b * 3; } 4"), "Int(4)");
    assert_eq!(main_value("int", "{ 5 }"), "Int(5)");
}

#[test]
fn if_expressions_can_be_assigned() {
    assert_eq!(
        main_value("int", "var n = 3; n = if n > 2 { n * 2 } else { 0 }; n"),
        "Int(6)"
    );
}

/// Run an Aja source file through every stage, as `main` does (so it can
/// import the standard library), returning the messages of the errors that
/// stopped it.
fn run_source(src: &str) -> Result<(), Vec<String>> {
    let root: std::collections::HashMap<String, String> =
        [("main.aja".to_string(), src.to_string())].into();
    let (_, program) = crate::module::build(&root, "main.aja");
    let program = program.map_err(|es| es.into_iter().map(|e| e.message).collect::<Vec<_>>())?;
    Vm::new()
        .run_program(&program)
        .map_err(|e| vec![e.message])?;
    Ok(())
}

fn aja_files(dir: &str) -> Vec<std::path::PathBuf> {
    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join(dir);
    let mut files: Vec<_> = std::fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("cannot read {}: {e}", dir.display()))
        .map(|entry| entry.unwrap().path())
        .filter(|p| p.extension().is_some_and(|e| e == "aja"))
        .collect();
    files.sort();
    assert!(!files.is_empty(), "no examples in {}", dir.display());
    files
}

#[test]
fn examples_run() {
    for path in aja_files("examples") {
        let src = std::fs::read_to_string(&path).unwrap();
        if let Err(errors) = run_source(&src) {
            panic!("{} failed: {errors:#?}", path.display());
        }
    }
}

#[test]
fn task_examples_run_under_stress() {
    // Switching tasks at every safepoint and collecting at every
    // allocation mustn't change whether an example works.
    let mut ran = 0;
    for path in aja_files("examples") {
        let src = std::fs::read_to_string(&path).unwrap();
        if !src.contains("import std::task;") {
            continue;
        }
        if let Err(e) = run_std(&src, true).1 {
            panic!("{} failed under stress: {}", path.display(), e.message);
        }
        ran += 1;
    }
    assert!(ran > 0, "no example uses std::task");
}

#[test]
fn error_examples_fail_as_described() {
    // Each file lists the errors it should produce in `// expect:` lines.
    for path in aja_files("examples/errors") {
        let src = std::fs::read_to_string(&path).unwrap();
        let expected: Vec<&str> = src
            .lines()
            .filter_map(|l| l.trim().strip_prefix("// expect: "))
            .collect();
        assert!(
            !expected.is_empty(),
            "{} has no `// expect:` lines",
            path.display()
        );
        match run_source(&src) {
            Ok(()) => panic!("{} should fail", path.display()),
            Err(errors) => assert_eq!(errors, expected, "{}", path.display()),
        }
    }
}

// ---- Strings ------------------------------------------------------------------

#[test]
fn strings_concatenate() {
    assert_eq!(main_value("str", "\"foo\" + \"bar\""), "Str(\"foobar\")");
    assert_eq!(
        main_value(
            "str",
            "var s = \"\"; var i = 0; while i < 3 { s = s + \"ab\"; i = i + 1; } s"
        ),
        "Str(\"ababab\")"
    );
    // Inferred: `a + b` on strings.
    assert_eq!(
        main_value(
            "str",
            "let join = fn(a, b) { a + \" \" + b }; join(\"hello\", \"world\")"
        ),
        "Str(\"hello world\")"
    );
    // A built string equals a literal with the same contents.
    assert_eq!(
        main_value("bool", "\"ab\" + \"c\" == \"abc\""),
        "Bool(true)"
    );
    assert_eq!(main_value("bool", "\"ab\" + \"c\" != \"ab\""), "Bool(true)");
    assert_eq!(
        main_value(
            "int",
            "match \"a\" + \"b\" { \"a\" => 1, \"ab\" => 2, _ => 3 }"
        ),
        "Int(2)"
    );
    // Multi-byte characters survive.
    assert_eq!(main_value("str", "\"é\" + \"✓\""), "Str(\"é✓\")");
}

#[test]
fn strings_survive_collection_wherever_they_are_kept() {
    // The stress-mode collector runs before every allocation, so a string
    // the VM forgot to keep reachable is freed before it is read.
    let src = "struct Named { name: str, n: int }
               enum Tree { Leaf(str), Node(Tree, Tree) }
               fn label(t: Tree) -> str {
                   match t { Tree::Leaf(s) => s, Tree::Node(l, r) => label(l) + \",\" + label(r) }
               }
               fn main() -> str {
                   let a = \"x\" + \"y\";
                   let p = Named { name: a + \"z\", n: 1 };
                   var tail = \"!\";
                   let shout = fn(s: str) -> str { s + tail };
                   tail = \"?\" + \"!\";
                   let t = Tree::Node(Tree::Leaf(p.name), Tree::Node(Tree::Leaf(a), Tree::Leaf(shout(\"hi\"))));
                   label(t) + \" \" + p.name
               }";
    assert_eq!(value(src), "Str(\"xyz,xy,hi?! xyz\")");
}

#[test]
fn strings_built_at_runtime_are_collected() {
    // Only the literals outlive the program (they are kept for good), so
    // building strings leaves no more behind than not building them.
    let program = |n: usize| {
        format!(
            "struct Box {{ s: str }}
             fn main() {{
                 var i = 0;
                 var s = \"\";
                 while i < {n} {{ s = s + \"abc\"; let b = Box {{ s: s + \"!\" }}; i = i + 1; }}
                 assert(s != \"x\");
             }}"
        )
    };
    let after = |n: usize| {
        let (mut vm, result) = run_vm(&program(n));
        result.expect("program runs");
        vm.collect_garbage();
        (vm.heap.live(), vm.heap.bytes())
    };
    assert_eq!(after(50), after(0));
}

#[test]
fn large_strings_trigger_collection() {
    // Collection is triggered by bytes, not object count: each pass of the
    // loop makes one 256 KiB string, which a count-based trigger would let
    // pile up to hundreds of megabytes.
    let src = "fn main() -> int {
                   var big = \"0123456789abcdef\";
                   var i = 0;
                   while i < 14 { big = big + big; i = i + 1; }
                   i = 0;
                   while i < 1000 { let copy = big + big; i = i + 1; }
                   i
               }";
    let ast = Parser::new(tokenize(src).unwrap().into_iter())
        .parse()
        .unwrap();
    let types = check::check_program(&ast).unwrap();
    let program = Compiler::new().program(&ast, &types).unwrap();
    let mut vm = Vm::new();
    assert_eq!(
        format!("{:?}", vm.run_program(&program).unwrap()),
        "Int(1000)"
    );
    assert!(vm.heap.bytes() < 4 << 20, "{} bytes live", vm.heap.bytes());
}

#[test]
fn repl_literals_are_kept_and_renumbered_after_a_failed_input() {
    let mut checker = check::Checker::new();
    let mut compiler = Compiler::new();
    let mut vm = Vm::new();
    vm.heap.stress = true;
    let mut eval = |src: &str| {
        let stmt = Parser::new(tokenize(src).unwrap().into_iter())
            .parse_stmt_only()
            .unwrap();
        let saved = (checker.clone(), compiler.clone());
        let types = checker.check_repl_stmt(&stmt).unwrap();
        let span = Span::new(
            crate::ast::Location::new(0, 1, 1),
            crate::ast::Location::new(0, 1, 1),
        );
        let input = compiler.repl(&stmt, &types, span);
        match vm.run_repl(&input) {
            Ok(v) => show(&vm, &v),
            Err(e) => {
                // As the REPL does: forget what the failed input defined.
                (checker, compiler) = saved;
                e.message
            }
        }
    };
    assert_eq!(
        eval("let greet = fn(n: str) -> str { \"hi \" + n }"),
        "Void"
    );
    // This input's literal is numbered, then rolled back with the compiler.
    assert_eq!(
        eval("{ let s = \"lost\"; assert(false); s }"),
        "assertion failed"
    );
    // So the next literal reuses its number, and must not see "lost".
    assert_eq!(eval("greet(\"there\")"), "Str(\"hi there\")");
    assert_eq!(eval("\"hi \" + \"again\""), "Str(\"hi again\")");
}

#[test]
fn generic_code_runs() {
    let types = "enum Seq<T> { Nil, Cons(T, Seq<T>) }
                 struct Pair<A, B> { first: A, second: B }
                 fn map<T, U>(xs: Seq<T>, f: (T) -> U) -> Seq<U> {
                     match xs { Seq::Nil => Seq::Nil, Seq::Cons(x, rest) => Seq::Cons(f(x), map(rest, f)) }
                 }
                 fn fold<T, A>(xs: Seq<T>, acc: A, f: (A, T) -> A) -> A {
                     match xs { Seq::Nil => acc, Seq::Cons(x, rest) => fold(rest, f(acc, x), f) }
                 }
                 fn get<T>(o: Option<T>, d: T) -> T { match o { Some(x) => x, None => d } }";
    let main = |ret: &str, body: &str| value(&format!("{types} fn main() -> {ret} {{ {body} }}"));

    let xs = "Seq::Cons(1, Seq::Cons(2, Seq::Cons(3, Seq::Nil)))";
    assert_eq!(
        main(
            "int",
            &format!("fold(map({xs}, fn(x) {{ x * 10 }}), 0, fn(a, x) {{ a + x }})")
        ),
        "Int(60)"
    );
    assert_eq!(
        main(
            "str",
            &format!("fold(map({xs}, fn(x: int) -> str {{ \"ab\" }}), \"\", fn(a, s) {{ a + s }})")
        ),
        "Str(\"ababab\")"
    );
    assert_eq!(main("int", "get(Some(5), 0) + get(None, 7)"), "Int(12)");
    assert_eq!(main("str", "let p = Pair { first: 1, second: \"one\" }; Pair { first: p.second, second: p.first }.first"), "Str(\"one\")");
    assert_eq!(
        main(
            "bool",
            "Some(Pair { first: 1, second: \"x\" }) == Some(Pair { first: 1, second: \"x\" })"
        ),
        "Bool(true)"
    );
    assert_eq!(main("bool", "Some(1) == None"), "Bool(false)");
    // Generic recursion in tail position runs in constant space.
    let src = format!(
        "{types} fn count<T>(xs: Seq<T>, n: int) -> int {{ match xs {{ Seq::Nil => n, Seq::Cons(_, t) => count(t, n + 1) }} }}
         fn build(n: int, acc: Seq<bool>) -> Seq<bool> {{ if n == 0 {{ acc }} else {{ build(n - 1, Seq::Cons(true, acc)) }} }}
         fn main() -> int {{ count(build(100000, Seq::Nil), 0) }}"
    );
    assert_eq!(
        format!("{:?}", run_limited(&src, 4096).unwrap()),
        "Int(100000)"
    );
}

#[test]
fn question_mark_returns_early() {
    let lib = "fn digit(s: str) -> Result<int, str> { match s { \"1\" => Ok(1), \"2\" => Ok(2), _ => Err(\"bad: \" + s) } }
               fn sum(a: str, b: str) -> Result<int, str> { Ok(digit(a)? + digit(b)?) }
               fn show(r: Result<int, str>) -> str { match r { Ok(_) => \"ok\", Err(e) => e } }
               fn first(o: Option<int>) -> Option<int> { let n = o?; Some(n * 10) }";
    let main = |ret: &str, body: &str| value(&format!("{lib} fn main() -> {ret} {{ {body} }}"));
    assert_eq!(
        main(
            "int",
            "match sum(\"1\", \"2\") { Ok(n) => n, Err(_) => -1 }"
        ),
        "Int(3)"
    );
    // The first error is returned, and nothing after it runs.
    assert_eq!(main("str", "show(sum(\"x\", \"y\"))"), "Str(\"bad: x\")");
    assert_eq!(main("str", "show(sum(\"1\", \"y\"))"), "Str(\"bad: y\")");
    assert_eq!(
        main("bool", "first(Some(4)) == Some(40) && first(None) == None"),
        "Bool(true)"
    );
    // In a closure, `?` returns from the closure.
    assert_eq!(
        main("int", "let f = fn(s) { let n = digit(s)?; Ok(n + 100) }; match f(\"2\") { Ok(n) => n, Err(_) => 0 } + match f(\"z\") { Ok(n) => n, Err(_) => 7 }"),
        "Int(109)"
    );
    // A `?` that doesn't return leaves the rest of the function running.
    let src = format!("{lib} fn count() -> Result<int, str> {{ var n = 0; while n < 5 {{ n = n + digit(\"1\")?; }} Ok(n) }}
                       fn main() -> bool {{ count() == Ok(5) }}");
    assert_eq!(value(&src), "Bool(true)");
}

#[test]
fn prelude_values_print_as_written() {
    let (vm, v) = run_vm("fn main() -> Option<Result<int, str>> { Some(Err(\"no\")) }");
    assert_eq!(vm.heap.display(&v.unwrap()), "Some(Err(\"no\"))");
}

#[test]
fn methods_run() {
    let src = "struct Point { x: int, y: int }
               impl Point {
                   fn origin() -> Point { Point { x: 0, y: 0 } }
                   fn add(self, o: Point) -> Point { Point { x: self.x + o.x, y: self.y + o.y } }
                   fn sum(self) -> int { self.x + self.y }
               }
               enum Seq<T> { Nil, Cons(T, Seq<T>) }
               impl<T> Seq<T> {
                   fn len(self) -> int { self.count(0) }
                   fn count(self, n: int) -> int { match self { Seq::Nil => n, Seq::Cons(_, t) => t.count(n + 1) } }
                   fn push(self, x: T) -> Seq<T> { Seq::Cons(x, self) }
               }
               fn build(n: int, acc: Seq<int>) -> Seq<int> { if n == 0 { acc } else { build(n - 1, acc.push(n)) } }";
    let main = |ret: &str, body: &str| value(&format!("{src} fn main() -> {ret} {{ {body} }}"));
    assert_eq!(
        main(
            "int",
            "Point::origin().add(Point { x: 2, y: 3 }).add(Point { x: 1, y: 1 }).sum()"
        ),
        "Int(7)"
    );
    assert_eq!(main("int", "let f = Point::origin; f().sum()"), "Int(0)");
    // The receiver is evaluated before the arguments.
    assert_eq!(
        main("int", "var log = 0; let p = fn() { log = log * 10 + 1; Point::origin() }; let q = fn() { log = log * 10 + 2; Point::origin() }; p().add(q()); log"),
        "Int(12)"
    );
    // A method calling itself in tail position runs in constant space.
    let deep = format!("{src} fn main() -> int {{ build(100000, Seq::Nil).len() }}");
    assert_eq!(
        format!("{:?}", run_limited(&deep, 4096).unwrap()),
        "Int(100000)"
    );
}

#[test]
fn prelude_methods_run() {
    let main = |ret: &str, body: &str| value(&format!("fn main() -> {ret} {{ {body} }}"));
    assert_eq!(
        main(
            "int",
            "Some(4).map(fn(n) { n * 10 }).unwrap_or(0) + None.unwrap_or(2)"
        ),
        "Int(42)"
    );
    assert_eq!(
        main("bool", "Some(1).is_some() && None.or(Some(2)) == Some(2)"),
        "Bool(true)"
    );
    let parse =
        "fn parse(s: str) -> Result<int, str> { if s == \"1\" { Ok(1) } else { Err(\"bad\") } }";
    assert_eq!(
        value(&format!("{parse} fn code(s: str) -> Result<int, int> {{ let n = parse(s).map_err(fn(e) {{ 404 }})?; Ok(n) }}
                        fn main() -> int {{ code(\"x\").unwrap_or(0) + code(\"1\").unwrap_or(0) }}")),
        "Int(1)"
    );
    let (vm, v) = run_vm("fn main() -> Option<Result<int, str>> { Some(Some(1).ok_or(\"none\")) }");
    assert_eq!(vm.heap.display(&v.unwrap()), "Some(Ok(1))");
}

#[test]
fn errors_in_closures_called_by_the_prelude_say_so() {
    let err = run("fn main() { let o = Some(0).map(fn(x) { 10 / x }); }").unwrap_err();
    assert_eq!(err.message, "division by zero");
    let notes = err.to_diagnostic().notes;
    assert!(
        notes[0].starts_with("in function 'f', called in the prelude at line "),
        "got {notes:?}"
    );
    assert_eq!(
        notes[1],
        "in function 'Option::map', called at line 1, column 21"
    );
}

// ---- Loops and iterators ---------------------------------------------------------

#[test]
fn for_loops_count_through_ranges() {
    assert_eq!(
        main_value("int", "var s = 0; for i in 0..5 { s = s * 10 + i; } s"),
        "Int(1234)"
    );
    assert_eq!(
        main_value(
            "int",
            "var n = 0; for i in 5..5 { n = n + 1; } for i in 3..-2 { n = n + 1; } n"
        ),
        "Int(0)"
    );
    assert_eq!(
        main_value("int", "var s = 0; for i in -3..0 { s = s + i; } s"),
        "Int(-6)"
    );
    // The end is evaluated once.
    assert_eq!(
        main_value(
            "int",
            "var calls = 0; let end = fn() -> int { calls = calls + 1; 3 };
             for i in 0..end() { } calls"
        ),
        "Int(1)"
    );
    // A range is an Iter like any other.
    assert_eq!(main_value("int", "let r = 2..6; r.count()"), "Int(4)");
}

#[test]
fn break_and_continue() {
    assert_eq!(
        main_value(
            "int",
            "var s = 0;
             for i in 0..100 { if i % 2 == 0 { continue; } if i > 9 { break; } s = s + i; }
             s"
        ),
        "Int(25)"
    );
    assert_eq!(
        main_value(
            "int",
            "var s = 0;
             for x in (0..100).map(fn(x) { x * 2 }) { if x == 4 { continue; } if x > 8 { break; } s = s + x; }
             s"
        ),
        "Int(16)"
    );
    assert_eq!(
        main_value(
            "int",
            "var i = 0; var s = 0;
             while true { i = i + 1; if i == 3 { continue; } if i > 5 { break; } s = s + i; }
             s"
        ),
        "Int(12)"
    );
    // `break` leaves only the innermost loop.
    assert_eq!(
        main_value(
            "int",
            "var n = 0; for i in 0..4 { for j in 0..4 { if j == i { break; } n = n + 1; } } n"
        ),
        "Int(6)"
    );
}

#[test]
fn return_leaves_a_loop() {
    assert_eq!(
        value(
            "fn first_square_over(n: int) -> int { for i in 0..n { if i * i > n { return i; } } -1 }
             fn main() -> int { first_square_over(50) * 100 + first_square_over(0) }"
        ),
        "Int(799)"
    );
}

#[test]
fn each_iteration_has_its_own_variable() {
    // Closures made in a loop each keep that iteration's value.
    assert_eq!(
        main_value(
            "int",
            "let fs = (0..3).map(fn(i) { fn() -> int { i } });
             var s = 0; for f in fs { s = s * 10 + f() + 1; } s"
        ),
        "Int(123)"
    );
    assert_eq!(
        main_value(
            "int",
            "var last = fn() -> int { 0 };
             for i in 0..3 { let j = i * 2; last = fn() -> int { i + j }; }
             last()"
        ),
        "Int(6)"
    );
}

#[test]
fn iterators_are_lazy_and_used_up() {
    // `map` runs only as values are taken.
    assert_eq!(
        main_value(
            "int",
            "var calls = 0;
             let it = (0..1000).map(fn(x) { calls = calls + 1; x });
             let first = it.take(3).count();
             calls * 10 + first"
        ),
        "Int(33)"
    );
    // A copy of an Iter shares its position.
    assert_eq!(
        main_value(
            "int",
            "let it = 0..10; let same = it; same.next(); same.next(); it.next().unwrap_or(-1)"
        ),
        "Int(2)"
    );
}

#[test]
fn iter_methods() {
    let cases = [
        (
            "Iter<int>",
            "(0..10).filter(fn(x) { x % 3 == 0 })",
            "0 3 6 9",
        ),
        ("Iter<int>", "(0..10).skip(7)", "7 8 9"),
        ("Iter<int>", "(0..2).chain(5..7)", "0 1 5 6"),
        ("Iter<int>", "Iter::empty()", ""),
        ("Iter<str>", "Iter::once(\"x\")", "x"),
        ("Iter<int>", "Some(4).iter()", "4"),
        ("Iter<int>", "None.iter()", ""),
    ];
    for (t, iter, expected) in cases {
        // There's no way to turn values into strings yet, so count them.
        let count = expected.split_whitespace().count();
        let src = format!("fn main() -> int {{ let it: {t} = {iter}; it.count() }}");
        assert_eq!(value(&src), format!("Int({count})"), "{iter}");
    }
    assert_eq!(
        main_value("int", "(1..5).fold(1, fn(a, b) { a * b })"),
        "Int(24)"
    );
    assert_eq!(main_value("bool", "(1..5).last() == Some(4)"), "Bool(true)");
    assert_eq!(main_value("bool", "(1..1).last() == None"), "Bool(true)");
    assert_eq!(
        main_value("bool", "(1..50).find(fn(x) { x * x > 30 }) == Some(6)"),
        "Bool(true)"
    );
    assert_eq!(
        main_value("bool", "(1..5).any(fn(x) { x == 4 })"),
        "Bool(true)"
    );
    assert_eq!(
        main_value("bool", "(1..5).all(fn(x) { x < 4 })"),
        "Bool(false)"
    );
    assert_eq!(
        main_value(
            "int",
            "(0..10).filter(fn(x) { x % 2 == 1 }).map(fn(x) { x * x }).fold(0, fn(a, b) { a + b })"
        ),
        "Int(165)"
    );
}

#[test]
fn types_can_be_looped_over_through_iter() {
    assert_eq!(
        value(
            "enum Seq { Nil, Cons(int, Seq) }
             impl Seq {
                 fn iter(self) -> Iter<int> {
                     var rest = self;
                     Iter::new(fn() -> Option<int> {
                         match rest {
                             Seq::Cons(x, more) => { rest = more; Some(x) }
                             Seq::Nil => None,
                         }
                     })
                 }
             }
             fn main() -> int {
                 let xs = Seq::Cons(1, Seq::Cons(2, Seq::Cons(3, Seq::Nil)));
                 var s = 0; for x in xs { s = s * 10 + x; } s
             }"
        ),
        "Int(123)"
    );
}

#[test]
fn errors_in_loops_point_at_their_cause() {
    let (text, notes) = located("fn main() { for x in (0..3).map(fn(x) { 10 / (x - 1) }) { } }");
    assert_eq!(text, "10 / (x - 1)");
    assert!(
        notes.iter().any(|n| n.contains("in the prelude")),
        "{notes:#?}"
    );
}

#[test]
fn long_loops_run_in_constant_space() {
    // Neither kind of loop grows the call stack.
    assert_eq!(
        main_value("int", "var s = 0; for i in 0..2000 { s = s + i; } s"),
        "Int(1999000)"
    );
    assert_eq!(
        main_value(
            "int",
            "(0..2000).map(fn(x) { x }).fold(0, fn(a, b) { a + b })"
        ),
        "Int(1999000)"
    );
}

#[test]
fn logical_operators_combine_comparisons() {
    // `false && (false == false)`, not `(false && false) == false`.
    assert_eq!(main_value("bool", "false && false == false"), "Bool(false)");
    assert_eq!(main_value("bool", "true || 1 == 2"), "Bool(true)");
    assert_eq!(
        main_value(
            "int",
            "var n = 0; for i in 0..20 { if i % 2 == 0 && i % 3 == 0 || i == 7 { n = n + 1; } } n"
        ),
        "Int(5)"
    );
}

// ---- Lists ---------------------------------------------------------------------

#[test]
fn lists_are_values() {
    let shown = |body: &str| {
        let (vm, result) = run_vm(&format!("fn main() -> List<int> {{ {body} }}"));
        vm.heap.repr(&result.unwrap())
    };
    assert_eq!(shown("[1, 2, 3]"), "[1, 2, 3]");
    assert_eq!(shown("[]"), "[]");
    assert_eq!(shown("[1, 2,]"), "[1, 2]");
    // Changing a list makes a new one; the old one stays as it was.
    assert_eq!(
        shown("let a = [1, 2]; let b = a.push(3).set(0, 9); a.concat(b)"),
        "[1, 2, 9, 2, 3]"
    );
    assert_eq!(shown("[1, 2, 3, 4].slice(1, 3)"), "[2, 3]");
    assert_eq!(
        shown("[1, 2, 3].drop_last().drop_last().drop_last().drop_last()"),
        "[]"
    );
    assert_eq!(shown("[3, 1, 2].reverse()"), "[2, 1, 3]");
    assert_eq!(shown("List::repeat(7, 3)"), "[7, 7, 7]");
    assert_eq!(shown("(0..4).collect()"), "[0, 1, 2, 3]");
    assert_eq!(
        shown("[1, 2, 3, 4].map(fn(x) { x * x }).filter(fn(x) { x > 2 })"),
        "[4, 9, 16]"
    );
    let (vm, result) = run_vm("fn main() -> List<List<str>> { [[\"a\"], [], [\"b\", \"c\"]] }");
    assert_eq!(
        vm.heap.repr(&result.unwrap()),
        "[[\"a\"], [], [\"b\", \"c\"]]"
    );
}

#[test]
fn list_queries() {
    assert_eq!(
        main_value("int", "let xs = [10, 20, 30]; xs[0] + xs[2] + xs.len()"),
        "Int(43)"
    );
    assert_eq!(
        main_value(
            "bool",
            "[1, 2].get(1) == Some(2) && [1, 2].get(2) == None && [1].get(-1) == None"
        ),
        "Bool(true)"
    );
    assert_eq!(
        main_value(
            "bool",
            "[5, 6].first() == Some(5) && [5, 6].last() == Some(6) && [].first() == None"
        ),
        "Bool(true)"
    );
    assert_eq!(
        main_value("bool", "[].is_empty() && ![1].is_empty()"),
        "Bool(true)"
    );
    assert_eq!(
        main_value("int", "[1, 2, 3, 4].fold(0, fn(a, b) { a * 10 + b })"),
        "Int(1234)"
    );
    assert_eq!(
        main_value(
            "bool",
            "[1, 2, 3].any(fn(x) { x == 2 }) && ![1, 2].all(fn(x) { x == 2 })"
        ),
        "Bool(true)"
    );
    assert_eq!(
        main_value("bool", "[1, 5, 9].find(fn(x) { x > 3 }) == Some(5)"),
        "Bool(true)"
    );
}

#[test]
fn lists_compare_by_their_values() {
    assert_eq!(
        main_value(
            "bool",
            "[1, 2] == [1, 2] && [1, 2] != [2, 1] && [1] != [1, 1]"
        ),
        "Bool(true)"
    );
    assert_eq!(
        main_value("bool", "[[1], [2, 3]] == [[1], [2].push(3)]"),
        "Bool(true)"
    );
    assert_eq!(
        main_value("bool", "[Some(\"a\")] == [Some(\"a\")]"),
        "Bool(true)"
    );
}

#[test]
fn for_loops_go_over_lists() {
    assert_eq!(
        main_value("int", "var s = 0; for x in [1, 2, 3] { s = s * 10 + x; } s"),
        "Int(123)"
    );
    assert_eq!(
        main_value("int", "var s = 0; for x in [1, 2, 3, 4, 5] { if x == 2 { continue; } if x == 5 { break; } s = s + x; } s"),
        "Int(8)"
    );
    // The loop goes over the list as it was, even if the variable changes.
    assert_eq!(main_value("int", "var xs = [1, 2, 3]; var n = 0; for x in xs { xs = xs.push(x); n = n + 1; } n * 10 + xs.len()"), "Int(36)");
    assert_eq!(
        main_value("int", "var n = 0; for x in [] { n = n + 1; } n"),
        "Int(0)"
    );
}

#[test]
fn long_lists() {
    // Past one 32-wide leaf, one full branch, and into a third level, with
    // the collector running before every allocation.
    let n = 32 * 32 + 50;
    assert_eq!(
        main_value(
            "int",
            &format!(
                "var xs = []; for i in 0..{n} {{ xs = xs.push(i); }}
                 var ys = xs; for i in 0..{n} {{ ys = ys.set(i, ys[i] * 2); }}
                 var s = 0; for y in ys {{ s = s + y; }}
                 s - 2 * xs.fold(0, fn(a, b) {{ a + b }}) + xs[{n} - 1]"
            )
        ),
        format!("Int({})", n - 1)
    );
}

#[test]
fn out_of_range_is_an_error() {
    assert_eq!(
        located("fn main() -> int { let xs = [1, 2, 3]; xs[3] }").0,
        "xs[3]"
    );
    assert_eq!(
        run("fn main() -> int { [1, 2, 3][-1] }")
            .unwrap_err()
            .message,
        "index -1 is out of range for a list of length 3"
    );
    assert_eq!(
        run("fn main() { [1].set(1, 2); }").unwrap_err().message,
        "index 1 is out of range for a list of length 1"
    );
    assert_eq!(
        run("fn main() { [1, 2].slice(1, 3); }")
            .unwrap_err()
            .message,
        "slice 1..3 is out of range for a list of length 2"
    );
}

#[test]
fn string_methods() {
    let cases = [
        ("\"héllo\".len()", "Int(5)"),
        ("\"héllo\".byte_len()", "Int(6)"),
        ("\"héllo\".slice(1, 3)", "Str(\"él\")"),
        ("\"héllo\".slice(5, 5)", "Str(\"\")"),
        ("\"héllo\".find(\"l\").unwrap_or(-1)", "Int(2)"),
        ("\"abc\".find(\"z\").is_none()", "Bool(true)"),
        ("\"  x y  \".trim()", "Str(\"x y\")"),
        ("\"  x\".trim_start() + \"y  \".trim_end()", "Str(\"xy\")"),
        ("\"MiXed\".to_upper() + \"MiXed\".to_lower()", "Str(\"MIXEDmixed\")"),
        ("\"a-b-c\".replace(\"-\", \"+\")", "Str(\"a+b+c\")"),
        ("\"abc\".replace(\"\", \"x\")", "Str(\"abc\")"),
        ("\"ab\".repeat(3) + \"z\".repeat(-1)", "Str(\"ababab\")"),
        ("\", \".join([\"a\", \"b\", \"c\"])", "Str(\"a, b, c\")"),
        ("\"-\".join([])", "Str(\"\")"),
        ("\"a,b,,c\".split(\",\").len()", "Int(4)"),
        ("\"abc\".split(\"\")[2]", "Str(\"c\")"),
        ("\"l1\\nl2\\r\\n\".lines()[1]", "Str(\"l2\")"),
        ("\" one  two\\tthree \".words().len()", "Int(3)"),
        ("\"héllo\".chars()[1]", "Str(\"é\")"),
        ("\"Hello\".starts_with(\"He\") && \"Hello\".ends_with(\"lo\") && \"Hello\".contains(\"ell\")", "Bool(true)"),
        ("\"\".is_empty() && !\" \".is_empty()", "Bool(true)"),
        ("\"-42\".parse_int().unwrap_or(0)", "Int(-42)"),
        ("\" 42\".parse_int().is_none() && \"4x\".parse_int().is_none()", "Bool(true)"),
        ("\"1e3\".parse_float().unwrap_or(0.0)", "Float(1000.0)"),
        ("\"\".parse_float().is_none()", "Bool(true)"),
    ];
    for (expr, expected) in cases {
        let src = format!("fn main() -> _ {{ {expr} }}");
        let src = src.replace("-> _", &format!("-> {}", type_of(expected)));
        assert_eq!(value(&src), expected, "{expr}");
    }
}

/// The Aja type of a value as `show` writes it, for a test's `main`.
fn type_of(shown: &str) -> &'static str {
    match shown.split('(').next() {
        Some("Int") => "int",
        Some("Float") => "float",
        Some("Bool") => "bool",
        Some("Str") => "str",
        _ => panic!("no type for {shown}"),
    }
}

#[test]
fn number_methods() {
    let cases = [
        (
            "(-5).abs() + 3.min(9) + 3.max(9) + 15.clamp(0, 10)",
            "Int(27)",
        ),
        (
            "2.pow(10) + 7.pow(0) + (-1).pow(5_000_000_001)",
            "Int(1024)",
        ),
        ("(-2.7).to_int() + 2.7.to_int()", "Int(0)"),
        (
            "7.to_float() + 2.5.round() + 2.5.floor() + 2.5.ceil() + 9.0.sqrt()",
            "Float(18.0)",
        ),
        ("(-1.5).abs().min(4.0).max(0.5)", "Float(1.5)"),
        ("(0.0 / 0.0).is_nan()", "Bool(true)"),
        (
            "1.to_str() + 1.5.to_str() + 1.0.to_str() + false.to_str()",
            "Str(\"11.51.0false\")",
        ),
    ];
    for (expr, expected) in cases {
        let src = format!("fn main() -> {} {{ {expr} }}", type_of(expected));
        assert_eq!(value(&src), expected, "{expr}");
    }
    for (expr, message) in [
        (
            "2.pow(-1)",
            "'pow' needs an exponent of 0 or more, but it was -1",
        ),
        ("10.pow(19)", "integer overflow in 'pow'"),
        ("(0.0 / 0.0).to_int()", "NaN can't be converted to an int"),
        (
            "(9223372036854775807.to_float() * 2.0).to_int()",
            "1.8446744073709552e19 can't be converted to an int",
        ),
        (
            "\"abc\".slice(2, 5).len()",
            "slice 2..5 is out of range for a string of length 3",
        ),
    ] {
        let src = format!("fn main() {{ {expr}; }}");
        assert_eq!(run(&src).unwrap_err().message, message, "{expr}");
    }
}

#[test]
fn template_strings() {
    let src = "
struct P { x: int, y: int }
impl P {
    fn to_str(self) -> str { \"({self.x}, {self.y})\" }
}
fn main() -> str {
    let name = \"Ada\";
    var s = \"<\";
    s = \"{s}{s}\";
    \"{name} is {36 + 1}, {1.5 * 2.0}, {true}, at {P { x: 1, y: -2 }}; {\"in {name.to_upper()}\"} {s} \\{x}\"
}";
    assert_eq!(
        value(src),
        "Str(\"Ada is 37, 3.0, true, at (1, -2); in ADA << {x}\")"
    );
}

#[test]
fn else_if_and_or_patterns() {
    let src = "
enum Shape { Circle(int), Square(int), Rect(int, int), Empty }

fn size(s: Shape) -> int {
    match s {
        Shape::Circle(n) | Shape::Square(n) | Shape::Rect(n, _) => n,
        Shape::Empty => 0,
    }
}

fn days(m: int) -> int {
    match m {
        2 => 28,
        4 | 6 | 9 | 11 => 30,
        _ => 31,
    }
}

fn sign(n: int) -> str {
    if n < 0 { \"-\" } else if n == 0 { \"0\" } else { \"+\" }
}

fn main() -> str {
    let total = size(Shape::Circle(1)) + size(Shape::Square(20)) + size(Shape::Rect(300, 9)) + size(Shape::Empty);
    // An or-pattern inside a variant, and a closure capturing its name.
    let get = match Some(Some(5)) {
        Some(Some(0) | None) | None => fn() -> int { 0 },
        Some(Some(n)) => fn() -> int { n },
    };
    var steps = \"\";
    for n in 0..4 {
        if n == 0 {
            steps = steps + \"a\";
        } else if n == 1 {
            steps = steps + \"b\";
        } else if n == 2 {
            steps = steps + \"c\";
        }
    }
    \"{total} {days(2)} {days(9)} {days(10)} {sign(-3)}{sign(0)}{sign(3)} {get()} {steps}\"
}";
    assert_eq!(value(src), "Str(\"321 28 30 31 -0+ 5 abc\")");
}

#[test]
fn tuples_hold_values_by_position() {
    assert_eq!(
        main_value("int", "let t = (1, (2, 3)); t.0 + t.1.0 * t.1.1"),
        "Int(7)"
    );
    assert_eq!(
        value(
            "fn divmod(a: int, b: int) -> (int, int) { (a / b, a % b) }
             fn main() -> int { let (q, r) = divmod(17, 5); q * 10 + r }"
        ),
        "Int(32)"
    );
    assert_eq!(
        value(
            "fn swap<A, B>(p: (A, B)) -> (B, A) { (p.1, p.0) }
             fn main() -> str { swap((1, \"one\")).0 }"
        ),
        "Str(\"one\")"
    );
    // A tuple shows as its values in parentheses.
    let (vm, v) = run_vm("fn main() -> (int, (str, bool)) { (1, (\"a\", true)) }");
    assert_eq!(vm.heap.repr(&v.unwrap()), "(1, (\"a\", true))");
}

#[test]
fn tuples_compare_by_value() {
    assert_eq!(
        main_value("bool", "(1, \"a\") == (1, \"a\") && (1, 2) != (2, 1)"),
        "Bool(true)"
    );
    assert_eq!(
        main_value("bool", "((1, 2), Some(3)) == ((1, 2), Some(4))"),
        "Bool(false)"
    );
}

#[test]
fn tuple_patterns_in_match_let_and_for() {
    assert_eq!(
        value(
            "fn classify(p: (int, bool)) -> str {
                 match p { (0, _) => \"zero\", (_, true) => \"yes\", (n, false) => \"no {n}\" }
             }
             fn main() -> str { classify((0, true)) + classify((1, true)) + classify((2, false)) }"
        ),
        "Str(\"zeroyesno 2\")"
    );
    assert_eq!(
        main_value(
            "int",
            "var total = 0;
             for (i, (a, b)) in [(1, (2, 3)), (4, (5, 6))] { total = total + i * (a + b); }
             for (k, v) in [(\"x\", 1)].iter() { total = total + v; }
             total"
        ),
        "Int(50)"
    );
    // Struct patterns, `_`, and or-patterns that match everything work in `let`.
    assert_eq!(
        value(
            "struct P { x: int, y: int }
             fn main() -> int {
                 let P { x, y: _ } = P { x: 3, y: 4 };
                 let (true | false, n) = (true, 5);
                 let _ = 9;
                 x + n
             }"
        ),
        "Int(8)"
    );
    // Each iteration's names are new, so closures keep their own.
    assert_eq!(
        main_value(
            "int",
            "var fs = [];
             for (a, b) in [(1, 2), (3, 4)] { fs = fs.push(fn() -> int { a * b }); }
             fs[0]() + fs[1]()"
        ),
        "Int(14)"
    );
}

#[test]
fn closures_get_parameter_types_from_where_they_are_passed() {
    assert_eq!(
        main_value(
            "int",
            "let pairs = [(1, \"a\"), (2, \"b\")];
             let firsts = pairs.map(fn(p) { p.0 });
             firsts[0] + firsts[1]"
        ),
        "Int(3)"
    );
    assert_eq!(
        value(
            "struct P { x: int }
             fn apply(f: (P) -> int, p: P) -> int { f(p) }
             fn main() -> int {
                 let g: (P) -> int = fn(p) { p.x * 2 };
                 apply(fn(p) { p.x }, P { x: 1 }) + g(P { x: 5 })
             }"
        ),
        "Int(11)"
    );
}

#[test]
fn repl_let_patterns_define_globals() {
    let mut checker = check::Checker::new();
    let mut compiler = Compiler::new();
    let mut vm = Vm::new();
    vm.heap.stress = true;
    let mut eval = |src: &str| {
        let stmt = Parser::new(tokenize(src).unwrap().into_iter())
            .parse_stmt_only()
            .unwrap();
        let types = checker.check_repl_stmt(&stmt).unwrap();
        let span = Span::new(
            crate::ast::Location::new(0, 1, 1),
            crate::ast::Location::new(0, 1, 1),
        );
        let input = compiler.repl(&stmt, &types, span);
        let v = vm.run_repl(&input).unwrap();
        show(&vm, &v)
    };
    assert_eq!(eval("let (a, b) = (2, 3)"), "Void");
    assert_eq!(eval("a * b"), "Int(6)");
}

/// Run the functions `names` of a program (which may import the standard
/// library) as tasks, returning the VM and what each task returned, shown.
fn run_tasks(src: &str, names: &[&str], stress: bool) -> (Vm, Vec<String>) {
    let root: std::collections::HashMap<String, String> =
        [("main.aja".to_string(), format!("{src}\nfn main() {{}}"))].into();
    let (_, program) = crate::module::build(&root, "main.aja");
    let program = program.unwrap_or_else(|es| {
        let msgs: Vec<String> = es.into_iter().map(|e| e.message).collect();
        panic!("test program has errors: {msgs:#?}")
    });
    let mut vm = Vm::new();
    vm.heap.stress = stress;
    vm.scheduler.stress = stress;
    let results = match vm.run_as_tasks(&program, names) {
        Ok(results) => results,
        Err(e) => panic!("program failed: {}", e.message),
    };
    let shown = results.iter().map(|v| show(&vm, v)).collect();
    (vm, shown)
}

#[test]
fn tasks_take_turns() {
    let src =
        "fn count(n: int) -> int { var total = 0; for i in 0..n { total = total + i; } total }
               fn a() -> int { count(10000) }
               fn b() -> int { count(20000) }";
    let (vm, results) = run_tasks(src, &["a", "b"], false);
    assert_eq!(results, ["Int(49995000)", "Int(199990000)"]);
    // Neither loop blocks, but each gives way after a slice of safepoints:
    // `a` runs first, and the two alternate until `a` is done.
    let switches = &vm.scheduler.switches;
    assert!(switches.len() > 10, "only switched {switches:?}");
    assert_eq!(switches[..4], [0, 1, 0, 1]);
    assert_eq!(*switches.last().unwrap(), 1);
}

#[test]
fn recursion_is_a_safepoint() {
    // No loops: only calls give way.
    let src = "fn fib(n: int) -> int { if n < 2 { n } else { fib(n - 1) + fib(n - 2) } }
               fn a() -> int { fib(20) }
               fn b() -> int { fib(21) }";
    let (vm, results) = run_tasks(src, &["a", "b"], false);
    assert_eq!(results, ["Int(6765)", "Int(10946)"]);
    assert!(vm.scheduler.switches.len() > 10);
}

#[test]
fn tasks_switched_at_every_safepoint_keep_their_values() {
    // Every safepoint switches and every allocation collects, so a value
    // a parked task holds is freed (and its use panics) unless it is a root.
    let src = "struct Box { items: List<str> }
               fn build(prefix: str, n: int) -> str {
                   var b = Box { items: [] };
                   for i in 0..n {
                       let keep = fn() -> str { \"{prefix}{i}\" };
                       b = Box { items: b.items.push(keep()) };
                   }
                   \",\".join(b.items)
               }
               fn a() -> str { build(\"a\", 4) }
               fn b() -> str { build(\"b\", 3) }
               fn c() -> (str, int) { (build(\"c\", 2), 7) }";
    let (vm, results) = run_tasks(src, &["a", "b", "c"], true);
    assert_eq!(results[..2], ["Str(\"a0,a1,a2,a3\")", "Str(\"b0,b1,b2\")"]);
    assert!(vm.scheduler.switches.len() > 30);
}

#[test]
fn each_task_has_its_own_intrinsic_status() {
    // `fs::read_to_string` calls an intrinsic and then reads its status,
    // and a switch can come between the two. Another task's successful
    // intrinsic mustn't make the failed read look like it succeeded.
    let src = "import std::fs;
               fn fails() -> int {
                   var failed = 0;
                   for _ in 0..50 {
                       match fs::read_to_string(\"/no/such/aja/file\") {
                           Err(_) => { failed = failed + 1; }
                           Ok(_) => {}
                       }
                   }
                   failed
               }
               fn succeeds() -> int {
                   var ok = 0;
                   for _ in 0..200 {
                       if fs::read_dir(\".\").is_ok() { ok = ok + 1; }
                   }
                   ok
               }";
    let (_, results) = run_tasks(src, &["fails", "succeeds"], true);
    assert_eq!(results, ["Int(50)", "Int(200)"]);
}

#[test]
fn a_task_that_fails_stops_the_run() {
    let src = "fn a() -> int { var n = 0; for i in 0..100000 { n = n + i; } n }
               fn b() -> int { 1 / 0 }
               fn main() {}";
    let root: std::collections::HashMap<String, String> =
        [("main.aja".to_string(), src.to_string())].into();
    let (_, program) = crate::module::build(&root, "main.aja");
    let err = Vm::new()
        .run_as_tasks(&program.unwrap(), &["a", "b"])
        .unwrap_err();
    assert_eq!(err.message, "division by zero");
}

/// Run a program that may import the standard library, switching tasks at
/// every safepoint and collecting at every allocation if `stress`.
fn run_std(src: &str, stress: bool) -> (Vm, Result<Value, RuntimeError>) {
    let root: std::collections::HashMap<String, String> =
        [("main.aja".to_string(), src.to_string())].into();
    let (_, program) = crate::module::build(&root, "main.aja");
    let program = program.unwrap_or_else(|es| {
        let msgs: Vec<String> = es.into_iter().map(|e| e.message).collect();
        panic!("test program has errors: {msgs:#?}")
    });
    let mut vm = Vm::new();
    vm.heap.stress = stress;
    vm.scheduler.stress = stress;
    let result = vm.run_program(&program);
    (vm, result)
}

/// `main`'s value, shown, both normally and under stress, which must agree.
#[track_caller]
fn std_value(src: &str) -> String {
    let shown = [false, true].map(|stress| match run_std(src, stress) {
        (vm, Ok(v)) => vm.heap.repr(&v),
        (_, Err(e)) => panic!("program failed (stress: {stress}): {}", e.message),
    });
    assert_eq!(shown[0], shown[1], "stress changed the result");
    shown[0].clone()
}

#[test]
fn scopes_spawn_and_join_tasks() {
    assert_eq!(
        std_value(
            "import std::task;
             fn fib(n: int) -> int { if n < 2 { n } else { fib(n - 1) + fib(n - 2) } }
             fn main() -> int {
                 task::scope(fn(s) {
                     let a = s.spawn(fn() { fib(15) });
                     let b = s.spawn(fn() { fib(16) });
                     a.join() + b.join()
                 })
             }"
        ),
        "1597"
    );
    // Joining again gives the same value, and so does joining a task from
    // another task, or after it has finished.
    assert_eq!(
        std_value(
            "import std::task;
             fn main() -> (int, int, str) {
                 task::scope(fn(s) {
                     let a = s.spawn(fn() { \"a\" + \"b\" });
                     let b = s.spawn(fn() { a.join() + \"c\" });
                     let first = b.join();
                     (first.len(), b.join().len(), a.join())
                 })
             }"
        ),
        "(3, 3, \"ab\")"
    );
}

#[test]
fn a_scope_waits_for_its_tasks() {
    // Nothing joins the tasks, but `scope` doesn't return until they are
    // done, and what each makes is in the list it returns.
    assert_eq!(
        std_value(
            "import std::task;
             fn main() -> List<int> {
                 let tasks = task::scope(fn(s) {
                     [1, 2, 3].map(fn(x) { s.spawn(fn() { x * 10 }) })
                 });
                 tasks.map(fn(t) { t.join() })
             }"
        ),
        "[10, 20, 30]"
    );
    // A task that returns nothing, and scopes inside tasks.
    assert_eq!(
        std_value(
            "import std::task;
             fn main() -> int {
                 task::scope(fn(outer) {
                     let quiet = outer.spawn(fn() { assert(true); });
                     let t = outer.spawn(fn() {
                         task::scope(fn(inner) {
                             let ts = [1, 2, 3].map(fn(x) { inner.spawn(fn() { x * x }) });
                             ts.fold(0, fn(sum, t) { sum + t.join() })
                         })
                     });
                     quiet.join();
                     t.join()
                 })
             }"
        ),
        "14"
    );
}

#[test]
fn many_tasks_reuse_their_slots() {
    let src = "import std::task;
               fn main() -> int {
                   var total = 0;
                   for i in 0..20000 {
                       total = total + task::scope(fn(s) { s.spawn(fn() { i % 3 }).join() });
                   }
                   total
               }";
    let (vm, result) = run_std(src, false);
    assert_eq!(show(&vm, &result.unwrap()), "Int(19999)");
    // One task ran at a time, so only two slots were ever needed: `main`'s
    // and the one each new task took over.
    assert!(vm.scheduler.switches.iter().all(|&id| id < 2));
}

#[test]
fn a_task_holds_its_values_while_it_waits() {
    // Every safepoint switches and every allocation collects: the tasks'
    // results live only in their `Task`s, and a waiting task's values only
    // in its parked registers.
    assert_eq!(
        std_value(
            "import std::task;
             fn word(i: int) -> str { \"w{i}\" }
             fn main() -> str {
                 task::scope(fn(s) {
                     let ts = [0, 1, 2, 3].map(fn(i) {
                         s.spawn(fn() { [word(i), word(i + 1)] })
                     });
                     let joined = ts.map(fn(t) { \"-\".join(t.join()) });
                     \",\".join(joined)
                 })
             }"
        ),
        "\"w0-w1,w1-w2,w2-w3,w3-w4\""
    );
}

#[test]
fn a_scope_cannot_be_used_after_it_ends() {
    let (_, result) = run_std(
        "import std::task;
         fn main() {
             var kept: Option<task::Scope> = None;
             task::scope(fn(s) { kept = Some(s); });
             match kept {
                 Some(s) => { s.spawn(fn() { 1 }); }
                 None => {}
             }
         }",
        false,
    );
    assert!(result
        .unwrap_err()
        .message
        .starts_with("cannot spawn a task in a scope that has ended"));
}

#[test]
fn a_task_waiting_for_itself_is_a_deadlock() {
    let (_, result) = run_std(
        "import std::task;
         fn main() {
             var me: Option<task::Task<int>> = None;
             task::scope(fn(s) {
                 let t = s.spawn(fn() {
                     match me { Some(t) => t.join(), None => 0 }
                 });
                 me = Some(t);
             });
         }",
        false,
    );
    assert!(result.unwrap_err().message.starts_with("deadlock"));
}

#[test]
fn an_error_in_a_task_stops_the_program() {
    let (_, result) = run_std(
        "import std::task;
         fn main() -> int {
             task::scope(fn(s) {
                 let a = s.spawn(fn() { 1 / 0 });
                 a.join()
             })
         }",
        false,
    );
    assert_eq!(result.unwrap_err().message, "division by zero");
}

#[test]
fn tasks_are_equal_only_to_themselves() {
    assert_eq!(
        std_value(
            "import std::task;
             fn main() -> (bool, bool) {
                 task::scope(fn(s) {
                     let a = s.spawn(fn() { 1 });
                     let b = s.spawn(fn() { 1 });
                     (a == a, a == b)
                 })
             }"
        ),
        "(true, false)"
    );
}

#[test]
fn stale_registers_in_a_wider_frame_are_not_roots() {
    // `scope`'s frame leaves its scope in registers that `List::map`'s
    // larger frame, which starts at the same place, covers before writing
    // them. The scope has been freed by then, which must not matter.
    let src = "import std::task;
               fn main() -> List<int> {
                   task::scope(fn(s) { s.spawn(fn() { 10 }); 1 });
                   [1].map(fn(t) { 5 })
               }";
    let (vm, result) = run_std(src, true);
    assert_eq!(vm.heap.repr(&result.unwrap()), "[5]");
}

#[test]
fn a_task_nobody_joins_keeps_its_task_and_scope() {
    // The handle `spawn` returns is dropped, so only the scheduler knows it
    // while the task runs, and the task allocates (and so collects) before
    // it finishes and puts its result there.
    assert_eq!(
        std_value(
            "import std::task;
             fn main() -> int {
                 task::scope(fn(s) {
                     s.spawn(fn() { let x = \"a\" + \"b\"; [x, x].len() });
                     1
                 })
             }"
        ),
        "1"
    );
}

#[test]
fn sleeping_tasks_sleep_at_the_same_time() {
    // Three 100 ms sleeps one after another would take 300 ms; at the same
    // time they take a little over 100.
    let src = "import std::task;
               fn main() -> int {
                   let start = clock();
                   task::scope(fn(s) {
                       for _ in 0..3 { s.spawn(fn() { sleep(100); }); }
                   });
                   clock() - start
               }";
    for stress in [false, true] {
        let (vm, result) = run_std(src, stress);
        let elapsed: i64 = vm.heap.repr(&result.unwrap()).parse().unwrap();
        assert!((100..250).contains(&elapsed), "took {elapsed} ms");
    }
}

#[test]
fn sleepers_wake_in_order_of_their_times() {
    assert_eq!(
        std_value(
            "import std::task;
             fn main() -> bool {
                 let woke = task::scope(fn(s) {
                     [60, 20, 40].map(fn(ms) { s.spawn(fn() { sleep(ms); clock() }) })
                 }).map(fn(t) { t.join() });
                 woke[1] <= woke[2] && woke[2] <= woke[0]
             }"
        ),
        "true"
    );
}

#[test]
fn a_sleeper_wakes_while_another_task_computes() {
    // The busy task never waits, but it gives way at safepoints, and then
    // the sleeper, whose time has come, gets a turn.
    let src = "import std::task;
               fn main() -> int {
                   let start = clock();
                   task::scope(fn(s) {
                       s.spawn(fn() { var n = 0; while clock() - start < 400 { n = n + 1; } n });
                       let sleeper = s.spawn(fn() { sleep(50); clock() - start });
                       sleeper.join()
                   })
               }";
    let (vm, result) = run_std(src, false);
    let woke: i64 = vm.heap.repr(&result.unwrap()).parse().unwrap();
    assert!((50..300).contains(&woke), "woke after {woke} ms");
}

#[test]
fn sleep_needs_a_time_of_zero_or_more() {
    assert_eq!(main_value("int", "sleep(0); 1"), "Int(1)");
    assert_eq!(
        failure("fn main() { sleep(-5); }"),
        "cannot sleep for -5 milliseconds: the time must be 0 or more"
    );
}

#[test]
fn bounded_type_parameters_compare_their_values() {
    let src = "struct P { x: int, s: str }
        fn index_of<T: Eq>(xs: List<T>, x: T) -> int {
            var i = 0;
            for y in xs { if y == x { return i; } i = i + 1; }
            -1
        }
        fn main() -> int {
            let a = index_of([1, 2, 3], 3);
            let b = index_of([\"a\", \"b\"], \"b\");
            let c = index_of([P { x: 1, s: \"a\" }, P { x: 2, s: \"b\" }], P { x: 2, s: \"b\" });
            let d = index_of([1.5, 2.5], 2.5);
            let e = index_of([(1, true), (2, false)], (3, true));
            let f = index_of([Some([1]), None], None);
            a * 100000 + b * 10000 + c * 1000 + d * 100 + (e + 2) * 10 + f
        }";
    assert_eq!(value(src), "Int(211111)");
}

#[test]
fn ord_values_compare_structurally() {
    // Each comparison is one digit: 1 if it holds.
    let src = "struct P { x: int, s: str }
        enum E { A(int), B(str), C }
        fn d(b: bool) -> int { if b { 1 } else { 0 } }
        fn main() -> str {
            let checks = [
                \"apple\" < \"banana\", \"b\" > \"a\", \"é\" > \"z\", \"ab\" < \"b\", \"a\" <= \"a\",
                false < true, !(true <= false),
                P { x: 1, s: \"z\" } < P { x: 2, s: \"a\" }, P { x: 1, s: \"a\" } < P { x: 1, s: \"b\" },
                E::A(9) < E::B(\"a\"), E::B(\"a\") < E::B(\"b\"), E::C > E::A(100),
                [1, 2] < [1, 2, 0], [1, 3] > [1, 2, 9], [] < [0], [[1], [2]] < [[1], [3]],
                (1, \"b\") < (1, \"c\"), None < Some(0), Some(1) < Some(2),
                !(Some(2) < Some(2)), Some(2) <= Some(2), Some(2) >= Some(2),
            ];
            \"\".join(checks.map(fn(b) { d(b).to_str() }))
        }";
    assert_eq!(value(src), format!("Str({:?})", "1".repeat(22)));
}

#[test]
fn compare_gives_an_ordering() {
    let src = "struct P { name: str, age: int }
        fn show(o: Ordering) -> str {
            match o { Ordering::Less => \"<\", Ordering::Equal => \"=\", Ordering::Greater => \">\" }
        }
        fn rev<T: Ord>(a: T, b: T) -> Ordering { a.compare(b).reverse() }
        fn main() -> str {
            let a = P { name: \"Ada\", age: 36 };
            let b = P { name: \"Bob\", age: 36 };
            let os = [1.compare(2), \"b\".compare(\"a\"), (1, 2).compare((1, 2)),
                a.age.compare(b.age).then(a.name.compare(b.name)), rev([1], []),
                Ordering::Less.compare(Ordering::Greater), a.compare(b)];
            \"\".join(os.map(show))
        }";
    assert_eq!(value(src), "Str(\"<>=<<<<\")");
}

#[test]
fn list_methods_for_eq_values() {
    let src = "struct P { x: int }
        fn main() -> str {
            let xs = [P { x: 3 }, P { x: 1 }, P { x: 3 }];
            let a = [xs.contains(P { x: 1 }), !xs.contains(P { x: 2 }), [\"a\"].contains(\"a\"),
                     xs.index_of(P { x: 3 }) == Some(0), xs.index_of(P { x: 2 }) == None,
                     [1, 1, 2, 2, 2, 1].dedup() == [1, 2, 1], [[1], [1]].dedup() == [[1]],
                     [].dedup() == [0].slice(0, 0)];
            \"\".join(a.map(fn(b) { if b { \"1\" } else { \"0\" } }))
        }";
    assert_eq!(value(src), "Str(\"11111111\")");
}

#[test]
fn list_methods_for_ord_values() {
    let src = "fn main() -> str {
            // A shuffle of 0..100, from a linear congruential generator.
            var xs = [];
            var seed = 7;
            for _ in 0..100 { seed = (seed * 1103515245 + 12345) % 2147483648; xs = xs.push(seed % 50); }
            let sorted = xs.sort();
            var ok = sorted.len() == 100;
            for i in 1..sorted.len() { ok = ok && sorted[i - 1] <= sorted[i]; }
            let by = xs.sort_by(fn(a, b) { a.compare(b) });
            let down = xs.sort_by(fn(a, b) { b.compare(a) });
            let checks = [ok, by == sorted, down == sorted.reverse(),
                xs.min() == sorted.first(), xs.max() == sorted.last(),
                sorted.binary_search(sorted[37]).map(fn(i) { sorted[i] }) == Some(sorted[37]),
                sorted.binary_search(50) == None, sorted.binary_search(-1) == None,
                [\"b\", \"a\", \"c\"].sort() == [\"a\", \"b\", \"c\"], [(2, \"a\"), (1, \"b\")].sort() == [(1, \"b\"), (2, \"a\")]];
            let e: List<int> = [];
            let empty = [e.sort() == e, e.min() == None, e.max() == None, e.binary_search(0) == None];
            \"\".join(checks.concat(empty).map(fn(b) { if b { \"1\" } else { \"0\" } }))
        }";
    assert_eq!(value(src), format!("Str({:?})", "1".repeat(14)));
}

#[test]
fn sort_by_keeps_equal_values_in_order() {
    let src = "struct P { name: str, age: int }
        fn main() -> str {
            let ps = [P { name: \"c\", age: 30 }, P { name: \"a\", age: 25 }, P { name: \"d\", age: 30 },
                      P { name: \"b\", age: 25 }, P { name: \"e\", age: 20 }];
            let by_age = ps.sort_by(fn(a, b) { a.age.compare(b.age) });
            // Sorting by name and then by age sorts by age, then name.
            let both = ps.sort_by(fn(a, b) { a.name.compare(b.name) }).sort_by(fn(a, b) { a.age.compare(b.age) });
            \"\".join(by_age.map(fn(p) { p.name })) + \" \" + \"\".join(both.map(fn(p) { p.name }))
        }";
    assert_eq!(value(src), "Str(\"eabcd eabcd\")");
}

/// The messages of the errors a program using the standard library has.
fn std_errors(src: &str) -> Vec<String> {
    let root: std::collections::HashMap<String, String> =
        [("main.aja".to_string(), src.to_string())].into();
    match crate::module::build(&root, "main.aja").1 {
        Ok(_) => Vec::new(),
        Err(es) => es.into_iter().map(|e| e.message).collect(),
    }
}

#[test]
fn maps_insert_get_and_remove() {
    let src = "import std::collections::{Map};
        struct P { x: int, y: int }
        fn main() -> str {
            var m = Map::new();
            for i in 0..300 { m = m.insert(\"k{i}\", i); }
            let before = m;
            for i in 0..150 { m = m.remove(\"k{i * 2}\"); }
            let grid = Map::from([(P { x: 0, y: 0 }, \"o\"), (P { x: 1, y: 2 }, \"a\"), (P { x: 0, y: 0 }, \"O\")]);
            let checks = [
                m.len() == 150, before.len() == 300, m.get(\"k3\") == Some(3), m.get(\"k4\") == None,
                before.get(\"k4\") == Some(4), m.contains_key(\"k299\"), !m.contains_key(\"k300\"),
                m.remove(\"nope\").len() == 150, m.insert(\"k3\", 33).get(\"k3\") == Some(33),
                grid.len() == 2, grid.get(P { x: 0, y: 0 }) == Some(\"O\"),
                m.keys().len() == 150, m.values().fold(0, fn(a, b) { a + b }) == 150 * 150,
                m.to_list().all(fn(p) { m.get(p.0) == Some(p.1) }),
            ];
            \"\".join(checks.map(fn(b) { if b { \"1\" } else { \"0\" } }))
        }";
    assert_eq!(std_value(src), format!("{:?}", "1".repeat(14)));
}

#[test]
fn map_methods_written_in_aja() {
    let src = "import std::collections::{Map};
        fn main() -> str {
            var counts = Map::new();
            for w in \"a b a c a b\".words() {
                counts = counts.update(w, fn(n) { Some(n.unwrap_or(0) + 1) });
            }
            let dropped = counts.update(\"c\", fn(n) { None });
            var seen = \"\";
            for (k, v) in counts.to_list().sort() { seen = seen + \"{k}{v}\"; }
            let ten = counts.map_values(fn(n) { n * 10 });
            let big = counts.filter(fn(k, v) { v > 1 });
            let merged = counts.merge(Map::from([(\"a\", 0), (\"z\", 9)]));
            \"{seen} {dropped.len()} {ten.get(\"b\").unwrap_or(0)} {big.len()} {merged.get(\"a\").unwrap_or(-1)} {merged.len()} {counts.is_empty()}\"
        }";
    assert_eq!(std_value(src), "\"a3b2c1 2 20 2 0 4 false\"");
}

#[test]
fn sets_and_their_operations() {
    let src = "import std::collections::{Set};
        fn sorted(s: Set<int>) -> List<int> { s.to_list().sort() }
        fn main() -> str {
            let a = Set::from([1, 2, 3, 2, 1]);
            let b = Set::from([3, 4]);
            let checks = [
                a.len() == 3, a.contains(2), !a.contains(4), a.insert(2).len() == 3,
                a.remove(2).len() == 2, a.remove(9) == a,
                sorted(a.union(b)) == [1, 2, 3, 4], sorted(b.union(a)) == [1, 2, 3, 4],
                sorted(a.intersection(b)) == [3], sorted(a.difference(b)) == [1, 2],
                Set::from([3]).is_subset(a), !b.is_subset(a), Set::new().is_subset(b),
                Set::from([\"x\"]).iter().count() == 1, Set::new().is_empty(),
            ];
            \"\".join(checks.map(fn(b) { if b { \"1\" } else { \"0\" } }))
        }";
    assert_eq!(std_value(src), format!("{:?}", "1".repeat(15)));
}

#[test]
fn maps_and_sets_compare_and_hash_by_contents() {
    let src = "import std::collections::{Map, Set};
        fn main() -> str {
            // The same entries, put in in different orders and with removals.
            var a = Map::new();
            for i in 0..100 { a = a.insert(i, \"v{i}\"); }
            var b = Map::new();
            for i in 0..150 { b = b.insert(149 - i, \"v{149 - i}\"); }
            for i in 100..150 { b = b.remove(i); }
            let checks = [
                a == b, a != b.insert(0, \"x\"), a != b.remove(5),
                Set::from([1, 2]) == Set::from([2, 1]), Set::from([1]) != Set::from([2]),
                // As keys and values, equal maps and sets are one.
                Set::from([a, b]).len() == 1, Set::from([Set::from([1, 2]), Set::from([2, 1])]).len() == 1,
                Map::from([(Set::from([1, 2]), 1)]).get(Set::from([2, 1])) == Some(1),
                Some(a) == Some(b), [Set::from([1])].contains(Set::from([1])),
            ];
            \"\".join(checks.map(fn(b) { if b { \"1\" } else { \"0\" } }))
        }";
    assert_eq!(std_value(src), format!("{:?}", "1".repeat(10)));
}

#[test]
fn maps_and_sets_print_their_entries() {
    let (vm, v) = run_std(
        "import std::collections::{Map, Set};
         fn main() -> Map<str, Set<str>> { Map::from([(\"a\", Set::from([\"x\"]))]) }",
        false,
    );
    assert_eq!(vm.heap.repr(&v.unwrap()), "{\"a\": {\"x\"}}");
    let (vm, v) = run_std(
        "import std::collections::{Set};
         fn main() -> Set<int> { Set::new() }",
        false,
    );
    assert_eq!(vm.heap.display(&v.unwrap()), "{}");
}

#[test]
fn map_keys_need_hash() {
    assert_eq!(
        std_errors("import std::collections::{Map}; fn main() { Map::from([(1.5, 1)]); }"),
        ["mismatched types: argument 1 of function 'std::collections::Map::from' expects List<({Hash}, _)>, found List<(float, int)>"]
    );
    assert_eq!(
        std_errors("import std::collections::{Map}; fn f<K, V>(m: Map<K, V>) -> int { m.len() } fn main() {}"),
        ["type 'std::collections::Map' needs its type argument 'K' to be Hash, but K isn't"]
    );
    assert!(std_errors("import std::collections::{Map}; fn f<K: Hash, V>(m: Map<K, V>) -> int { m.len() } fn main() {}").is_empty());
    // Maps and sets have no order.
    assert_eq!(
        std_errors(
            "import std::collections::{Set}; fn main() { Set::from([1]) < Set::from([2]); }"
        )
        .len(),
        1
    );
    assert_eq!(
        std_errors("import std::collections::{Map}; fn main() { let m: Map<str, int> = Map::new(); m.value(\"a\"); }"),
        ["method 'value' of 'std::collections::Map' is private"]
    );
}

#[test]
fn sorted_maps_keep_keys_in_order() {
    let src = "import std::collections::{SortedMap};
        fn main() -> str {
            // Keys put in out of order, some twice, and some removed.
            var m = SortedMap::new();
            for i in 0..200 { m = m.insert((i * 37) % 200, i); }
            let before = m;
            for i in 0..100 { m = m.remove(i * 2); }
            var ordered = true;
            let keys = m.keys();
            for i in 1..keys.len() { ordered = ordered && keys[i - 1] < keys[i]; }
            let checks = [
                m.len() == 100, before.len() == 200, ordered, keys == before.keys().filter(fn(k) { k % 2 == 1 }),
                m.first() == Some((1, m.get(1).unwrap_or(-1))), m.last().map(fn(p) { p.0 }) == Some(199),
                m.range(10, 20).map(fn(p) { p.0 }) == [11, 13, 15, 17, 19], m.range(20, 10) == [],
                m.get(2) == None, before.get(2) == Some(146),
                m.to_list().all(fn(p) { m.get(p.0) == Some(p.1) }), m.values().len() == 100,
                SortedMap::from([(\"b\", 1), (\"a\", 2), (\"b\", 3)]).to_list() == [(\"a\", 2), (\"b\", 3)],
                m.update(1, fn(v) { None }).first().map(fn(p) { p.0 }) == Some(3),
                m.filter(fn(k, v) { k < 5 }).keys() == [1, 3], m.map_values(fn(v) { v * 0 }).values().all(fn(v) { v == 0 }),
                m.merge(SortedMap::from([(0, 0)])).first() == Some((0, 0)),
            ];
            let e: SortedMap<str, int> = SortedMap::new();
            let empty = [e.first() == None, e.last() == None, e.is_empty(), e.range(\"a\", \"z\") == []];
            \"\".join(checks.concat(empty).map(fn(b) { if b { \"1\" } else { \"0\" } }))
        }";
    assert_eq!(std_value(src), format!("{:?}", "1".repeat(21)));
}

#[test]
fn sorted_sets_and_their_operations() {
    let src = "import std::collections::{SortedSet};
        fn main() -> str {
            let a = SortedSet::from([\"pear\", \"apple\", \"fig\", \"apple\"]);
            let b = SortedSet::from([\"fig\", \"kiwi\"]);
            let checks = [
                a.to_list() == [\"apple\", \"fig\", \"pear\"], a.len() == 3, a.contains(\"fig\"), !a.contains(\"kiwi\"),
                a.first() == Some(\"apple\"), a.last() == Some(\"pear\"), a.range(\"b\", \"g\") == [\"fig\"],
                a.union(b).to_list() == [\"apple\", \"fig\", \"kiwi\", \"pear\"], a.intersection(b).to_list() == [\"fig\"],
                a.difference(b).to_list() == [\"apple\", \"pear\"], SortedSet::from([\"fig\"]).is_subset(a), !b.is_subset(a),
                a.remove(\"fig\").remove(\"nope\").len() == 2, a.insert(\"fig\") == a,
            ];
            \"\".join(checks.map(fn(b) { if b { \"1\" } else { \"0\" } }))
        }";
    assert_eq!(std_value(src), format!("{:?}", "1".repeat(14)));
}

#[test]
fn sorted_collections_compare_hash_and_print_in_order() {
    let src = "import std::collections::{SortedMap, SortedSet, Set};
        fn main() -> str {
            var a = SortedMap::new();
            for i in 0..100 { a = a.insert(i, i * i); }
            var b = SortedMap::new();
            for i in 0..150 { b = b.insert(149 - i, (149 - i) * (149 - i)); }
            for i in 100..150 { b = b.remove(i); }
            let checks = [
                a == b, a != b.insert(0, 1), Set::from([a, b]).len() == 1,
                SortedSet::from([1, 2]) < SortedSet::from([1, 3]), SortedSet::from([1]) < SortedSet::from([1, 2]),
                SortedMap::from([(1, \"b\")]) > SortedMap::from([(1, \"a\")]),
                SortedSet::from([SortedSet::from([2]), SortedSet::from([1, 5])]).first() == Some(SortedSet::from([1, 5])),
                [SortedSet::from([3]), SortedSet::from([1, 2])].sort() == [SortedSet::from([1, 2]), SortedSet::from([3])],
            ];
            \"\".join(checks.map(fn(b) { if b { \"1\" } else { \"0\" } }))
        }";
    assert_eq!(std_value(src), format!("{:?}", "1".repeat(8)));
    let (vm, v) = run_std(
        "import std::collections::{SortedMap, SortedSet};
         fn main() -> SortedMap<str, SortedSet<int>> {
             SortedMap::from([(\"b\", SortedSet::from([3, 1])), (\"a\", SortedSet::new())])
         }",
        false,
    );
    assert_eq!(vm.heap.repr(&v.unwrap()), "{\"a\": {}, \"b\": {1, 3}}");
}

#[test]
fn sorted_keys_need_ord() {
    assert_eq!(
        std_errors("import std::collections::{SortedSet}; fn main() { SortedSet::from([0.5]); }"),
        ["mismatched types: argument 1 of function 'std::collections::SortedSet::from' expects List<{Ord}>, found List<float>"]
    );
    // A map has no order, so it can't be a sorted map's key.
    assert_eq!(
        std_errors("import std::collections::{Map, SortedSet}; fn main() { SortedSet::from([Map::from([(1, 1)])]); }").len(),
        1
    );
    assert_eq!(
        std_errors("import std::collections::{SortedMap}; fn f<K: Hash>(m: SortedMap<K, int>) {} fn main() {}"),
        ["type 'std::collections::SortedMap' needs its type argument 'K' to be Ord, but K isn't"]
    );
}

#[test]
fn durations_show_in_their_largest_unit() {
    assert_eq!(
        std_value(
            "import std::time::Duration;
             fn main() -> List<str> {
                 [Duration::zero(), Duration::from_nanos(800), Duration::from_nanos(12_345),
                  Duration::from_micros(1_050), Duration::from_millis(250),
                  Duration::from_secs_float(1.5), Duration::from_nanos(1_000_000_001),
                  Duration::from_secs(-2), Duration::from_secs(3600)]
                     .map(fn(d) { d.to_str() })
             }"
        ),
        "[\"0s\", \"800ns\", \"12.345µs\", \"1.05ms\", \"250ms\", \"1.5s\", \"1s\", \"-2s\", \"3600s\"]"
    );
}

#[test]
fn durations_convert_add_and_compare() {
    assert_eq!(
        std_value(
            "import std::time::Duration;
             fn main() -> (int, int, int, float, str, bool, bool, bool) {
                 let d = Duration::from_millis(1_500);
                 (d.as_nanos(), d.as_millis(), d.as_secs(), d.as_secs_float(),
                  d.add(Duration::from_micros(250)).sub(Duration::from_secs(1)).mul(2).div(4).to_str(),
                  Duration::from_millis(1_000) == Duration::from_secs(1),
                  Duration::from_millis(999) < Duration::from_secs(1),
                  Duration::from_secs(1).sub(d).is_negative())
             }"
        ),
        "(1500000000, 1500, 1, 1.5, \"250.125ms\", true, true, true)"
    );
}

#[test]
fn instants_measure_sleeps_in_tasks() {
    // Two 50 ms sleeps in tasks of their own overlap, and a deadline
    // already past returns straight away.
    let src = "import std::task;
               import std::time;
               import std::time::{Duration, Instant};
               fn main() -> int {
                   let start = Instant::now();
                   task::scope(fn(s) {
                       for _ in 0..2 { s.spawn(fn() { time::sleep_for(Duration::from_millis(50)); }); }
                   });
                   time::sleep_until(start);
                   time::sleep_for(Duration::from_secs(-1));
                   let later = Instant::now();
                   assert(later > start && later.since(start) == later.sub(Duration::zero()).since(start));
                   start.elapsed().as_millis()
               }";
    for stress in [false, true] {
        let (vm, result) = run_std(src, stress);
        let elapsed: i64 = vm.heap.repr(&result.unwrap()).parse().unwrap();
        assert!((50..200).contains(&elapsed), "took {elapsed} ms");
    }
}

#[test]
fn tcp_servers_and_clients_talk_in_tasks() {
    // Three clients at once, each with a server task of its own, and
    // reading lines, to the end, and both line endings.
    assert_eq!(
        std_value(
            "import std::io;
             import std::net;
             import std::task;
             fn serve(conn: net::TcpStream) -> Result<int, io::Error> {
                 var n = 0;
                 for line in conn.lines() {
                     conn.write_line(\"<\" + line? + \">\")?;
                     n = n + 1;
                 }
                 conn.close();
                 Ok(n)
             }
             fn main() -> List<str> {
                 let listener = match net::listen(\"127.0.0.1:0\") { Ok(l) => l, Err(e) => { return [e.message]; } };
                 let addr = listener.local_addr();
                 task::scope(fn(s) {
                     s.spawn(fn() {
                         for _ in 0..3 {
                             match listener.accept() {
                                 Ok(conn) => { s.spawn(fn() { serve(conn) }); }
                                 Err(e) => println(e.message),
                             }
                         }
                     });
                     [\"a\", \"b\", \"c\"].map(fn(name) {
                         s.spawn(fn() -> str {
                             let reply = net::connect(addr).and_then(fn(c) {
                                 c.write(name + \"1\\r\\n\" + name + \"2\\n\" + name + \"3\")
                                     .and_then(fn(_) { c.close_write() })
                                     .and_then(fn(_) { c.read_to_string() })
                             });
                             match reply { Ok(r) => r, Err(e) => e.message }
                         })
                     }).map(fn(t) { t.join() })
                 })
             }"
        ),
        "[\"<a1>\\n<a2>\\n<a3>\\n\", \"<b1>\\n<b2>\\n<b3>\\n\", \"<c1>\\n<c2>\\n<c3>\\n\"]"
    );
}

#[test]
fn tcp_writes_wait_while_the_reader_catches_up() {
    // Eight megabytes is more than the socket buffers hold, so the write waits
    // for the reader, in another task, more than once.
    assert_eq!(
        std_value(
            "import std::net;
             import std::task;
             fn main() -> (int, int) {
                 let listener = match net::listen(\"127.0.0.1:0\") { Ok(l) => l, Err(_) => { return (-1, -1); } };
                 let big = \"0123456789abcdef\".repeat(524_288);
                 task::scope(fn(s) {
                     let reader = s.spawn(fn() -> int {
                         match listener.accept() {
                             Ok(conn) => match conn.read_to_string() { Ok(all) => all.byte_len(), Err(_) => -2 },
                             Err(_) => -3,
                         }
                     });
                     let wrote = match net::connect(listener.local_addr()) {
                         Ok(c) => match c.write(big) {
                             Ok(_) => { c.close(); big.byte_len() }
                             Err(_) => -4,
                         },
                         Err(_) => -5,
                     };
                     (wrote, reader.join())
                 })
             }"
        ),
        "(8388608, 8388608)"
    );
}

#[test]
fn tcp_errors_say_what_went_wrong() {
    assert_eq!(
        std_value(
            "import std::io;
             import std::net;
             import std::task;
             fn kind(r: Result<net::TcpStream, io::Error>) -> str {
                 match r { Ok(_) => \"ok\", Err(e) => \"{e.kind == io::ErrorKind::ConnectionRefused} {e.message}\" }
             }
             fn main() -> List<str> {
                 let listener = match net::listen(\"127.0.0.1:0\") { Ok(l) => l, Err(e) => { return [e.message]; } };
                 let addr = listener.local_addr();
                 let in_use = match net::listen(addr) {
                     Ok(_) => \"listened twice\",
                     Err(e) => if e.kind == io::ErrorKind::AddrInUse { \"in use\" } else { e.message },
                 };
                 // Closing the listener wakes a task waiting to accept.
                 let woken = task::scope(fn(s) {
                     let waiting = s.spawn(fn() { listener.accept() });
                     s.spawn(fn() { sleep(20); listener.close(); });
                     match waiting.join() {
                         Ok(_) => \"accepted\",
                         Err(e) => if e.kind == io::ErrorKind::NotConnected { \"woken\" } else { e.message },
                     }
                 });
                 let refused = kind(net::connect(addr));
                 [in_use, woken, refused.starts_with(\"true could not connect to '127.0.0.1:\").to_str()]
             }"
        ),
        "[\"in use\", \"woken\", \"true\"]"
    );
}

#[test]
fn void_compares_hashes_and_matches() {
    assert_eq!(
        std_value(
            "import std::collections::{Set, SortedSet};
             fn f(fail: bool) -> Result<void, str> { if fail { Err(\"no\") } else { Ok(void) } }
             fn main() -> (bool, bool, bool, bool, int, int, str) {
                 let shown = match f(false) { Ok(void) => \"ok\", Err(e) => e };
                 (f(false) == Ok(void), f(true) == Ok(void), void < void, void <= void,
                  Set::from([void, void]).len(), SortedSet::from([void, void]).len(), shown)
             }"
        ),
        "(true, false, false, true, 1, 1, \"ok\")"
    );
    assert_eq!(
        std_value("fn main() -> Result<void, str> { Ok(void) }"),
        "Ok(void)"
    );
}

#[test]
fn deferred_code_runs_when_its_block_is_left() {
    // Each function logs to a `var` list, so the order can be checked.
    // Last deferred runs first, after the block's own code and value.
    assert_eq!(
        std_value(
            "fn main() -> List<str> {
                 var log: List<str> = [];
                 let f = fn() -> int {
                     defer { log = log.push(\"a\"); }
                     defer { log = log.push(\"b\"); }
                     log = log.push(\"body\");
                     1
                 };
                 f();
                 log
             }"
        ),
        "[\"body\", \"b\", \"a\"]"
    );
    // `return`, `?`, `break`, and `continue` run the defers they leave,
    // and only those.
    assert_eq!(
        std_value(
            "fn main() -> List<str> {
                 var log: List<str> = [];
                 let early = fn(n: int) -> int {
                     defer { log = log.push(\"early {n}\"); }
                     if n < 0 { return -1; }
                     n
                 };
                 let tried = fn(s: str) -> Result<int, str> {
                     defer { log = log.push(\"tried {s}\"); }
                     let n = s.parse_int().ok_or(\"bad\")?;
                     Ok(n)
                 };
                 early(-1); early(1); tried(\"x\"); tried(\"2\");
                 defer { log = log.push(\"never seen: main's block is left after its value\"); }
                 for i in 0..4 {
                     defer { log = log.push(\"end {i}\"); }
                     if i == 1 { continue; }
                     if i == 2 { break; }
                     log = log.push(\"body {i}\");
                 }
                 log
             }"
        ),
        "[\"early -1\", \"early 1\", \"tried x\", \"tried 2\", \
         \"body 0\", \"end 0\", \"end 1\", \"end 2\"]"
    );
}

#[test]
fn deferred_code_sees_the_names_in_scope_at_the_defer() {
    // A `let` after the `defer` shadows `x` for the rest of the block, but
    // not for the deferred code. A `var` is read when the code runs.
    assert_eq!(
        std_value(
            "fn main() -> (int, int) {
                 var seen = 0;
                 var at_exit = 0;
                 var n = 1;
                 let f = fn() {
                     let x = 10;
                     defer { seen = x; at_exit = n; }
                     let x = 20;
                     n = x + 1;
                 };
                 f();
                 (seen, at_exit)
             }"
        ),
        "(10, 21)"
    );
}

#[test]
fn deferred_code_can_have_blocks_loops_and_defers_of_its_own() {
    assert_eq!(
        std_value(
            "fn main() -> List<str> {
                 var log: List<str> = [];
                 let f = fn() {
                     defer {
                         for i in 0..5 {
                             if i == 2 { break; }
                             log = log.push(\"loop {i}\");
                         }
                         defer { log = log.push(\"inner defer\"); }
                         let g = fn() -> int { return 3; };
                         log = log.push(\"closure {g()}\");
                     }
                     {
                         defer { log = log.push(\"nested block\"); }
                         log = log.push(\"in nested\");
                         return;
                     }
                 };
                 f();
                 log
             }"
        ),
        "[\"in nested\", \"nested block\", \"loop 0\", \"loop 1\", \
         \"closure 3\", \"inner defer\"]"
    );
}

#[test]
fn deferred_code_can_wait_in_a_task() {
    // A deferred `sleep` and `join` wait like any other code, under
    // switching and collection stress too.
    assert_eq!(
        std_value(
            "import std::task;
             fn main() -> List<int> {
                 task::scope(fn(s) {
                     [30, 10, 20].map(fn(ms) {
                         s.spawn(fn() -> int {
                             var done = 0;
                             let inner = fn() { defer { sleep(ms); done = ms; } };
                             inner();
                             done
                         })
                     }).map(fn(t) { t.join() })
                 })
             }"
        ),
        "[30, 10, 20]"
    );
}

#[test]
fn a_call_returned_while_code_is_deferred_isnt_a_tail_call() {
    // The deferred code runs after the call, so the call needs its frame.
    let msg = failure(
        "fn f(n: int) -> int { defer print(\"\"); if n == 0 { 0 } else { f(n - 1) } }
         fn main() { f(100_000_000); }",
    );
    assert!(msg.starts_with("stack overflow"), "{msg}");
}

/// The error a program using the standard library stops with, under
/// stress or not, rendered with its notes.
#[track_caller]
fn std_failure(src: &str, stress: bool) -> String {
    let (_, result) = run_std(src, stress);
    let e = result.expect_err("program should fail");
    let mut text = e.message.clone();
    for n in e.to_diagnostic().notes {
        text.push_str("\n");
        text.push_str(&n);
    }
    text
}

#[test]
fn cancelling_a_scope_stops_its_tasks_and_runs_their_deferred_code() {
    // A busy loop, a sleeper, a joiner, and one waiting on a socket all
    // stop; their deferred code runs; and `scope` returns once they have.
    // Each task has a variable of its own to mark, since a `var` that two
    // tasks change at once can lose a change under switching stress.
    assert_eq!(
        std_value(
            "import std::net;
             import std::task;
             fn main() -> (List<bool>, List<bool>) {
                 var busy_done = false;
                 var sleeper_done = false;
                 var joiner_done = false;
                 var accepter_done = false;
                 let listener = match net::listen(\"127.0.0.1:0\") { Ok(l) => l, Err(_) => { return ([], []); } };
                 let stopped = task::scope(fn(s) {
                     let busy = s.spawn(fn() -> int {
                         defer { busy_done = true; }
                         var n = 0;
                         while true { n = n + 1; }
                         n
                     });
                     let sleeper = s.spawn(fn() -> int { defer { sleeper_done = true; } sleep(100_000); 1 });
                     let joiner = s.spawn(fn() -> int { defer { joiner_done = true; } sleeper.join() });
                     let accepter = s.spawn(fn() -> int {
                         defer { accepter_done = true; }
                         match listener.accept() { Ok(_) => 1, Err(_) => 2 }
                     });
                     sleep(20);
                     s.cancel();
                     [busy.try_join().is_none(), sleeper.try_join().is_none(),
                      joiner.try_join().is_none(), accepter.try_join().is_none()]
                 });
                 ([busy_done, sleeper_done, joiner_done, accepter_done], stopped)
             }"
        ),
        "([true, true, true, true], [true, true, true, true])"
    );
}

#[test]
fn a_cancelled_task_unwinds_every_frame_innermost_first() {
    // Deferred code pending in each frame, in blocks and loop bodies, runs
    // once each, from the innermost frame out; and a task's nested scope's
    // tasks stop before it leaves the scope. Only the cancelled task
    // writes `log`; `seq = seq + 1` has no safepoint in it, so no switch
    // can split it, and it orders the two tasks.
    assert_eq!(
        std_value(
            "import std::task;
             fn main() -> (List<str>, bool) {
                 var log: List<str> = [];
                 var seq = 0;
                 var nested_at = 0;
                 var task_at = 0;
                 let inner = fn(depth: int) {
                     defer { log = log.push(\"frame {depth}\"); }
                     for i in 0..3 {
                         defer { log = log.push(\"iteration {i} of frame {depth}\"); }
                         if i == 1 { sleep(100_000); }
                     }
                 };
                 task::scope(fn(s) {
                     s.spawn(fn() {
                         defer { seq = seq + 1; task_at = seq; }
                         task::scope(fn(nested) {
                             nested.spawn(fn() { defer { seq = seq + 1; nested_at = seq; } sleep(100_000); });
                             inner(1);
                         });
                     });
                     sleep(20);
                     s.cancel();
                 });
                 (log, 0 < nested_at && nested_at < task_at)
             }"
        ),
        "([\"iteration 0 of frame 1\", \"iteration 1 of frame 1\", \"frame 1\"], true)"
    );
}

#[test]
fn deferred_code_runs_to_its_end_when_its_task_is_cancelled() {
    // The cancel comes while the task is in deferred code, asleep; the
    // rest of it runs, and the task stops at its next safepoint, running
    // the deferred code around it.
    assert_eq!(
        std_value(
            "import std::task;
             fn main() -> List<str> {
                 var log: List<str> = [];
                 task::scope(fn(s) {
                     s.spawn(fn() {
                         defer { log = log.push(\"outer\"); }
                         {
                             defer { log = log.push(\"start\"); sleep(40); log = log.push(\"end\"); }
                             log = log.push(\"body\");
                         }
                         log = log.push(\"after the block\");
                         sleep(100_000);
                     });
                     sleep(10);
                     s.cancel();
                 });
                 log
             }"
        ),
        "[\"body\", \"start\", \"end\", \"outer\"]"
    );
}

#[test]
fn a_sleep_in_deferred_code_isnt_cut_short_or_restarted() {
    // Cancelled 10 ms into a 60 ms sleep in deferred code, the task sleeps
    // out the rest of it: not less, and not 60 ms more.
    let src = "import std::task;
               fn main() -> int {
                   let start = clock();
                   var woke = 0;
                   task::scope(fn(s) {
                       s.spawn(fn() {
                           { defer { sleep(60); woke = clock() - start; } }
                           sleep(100_000);
                       });
                       sleep(10);
                       s.cancel();
                   });
                   woke
               }";
    for stress in [false, true] {
        let (vm, result) = run_std(src, stress);
        let woke: i64 = vm.heap.repr(&result.unwrap()).parse().unwrap();
        // Restarting the sleep would make it 70.
        assert!((60..67).contains(&woke), "woke after {woke} ms");
    }
}

#[test]
fn cancelled_scopes_start_no_tasks_and_cancel_once() {
    assert_eq!(
        std_value(
            "import std::task;
             fn main() -> (bool, bool, int) {
                 var ran = 0;
                 let (never, own) = task::scope(fn(s) {
                     s.cancel();
                     s.cancel();
                     let never = s.spawn(fn() -> int { ran = ran + 1; 1 });
                     (never.try_join().is_none(), s.spawn(fn() { ran = ran + 1; }).try_join().is_none())
                 });
                 // The code that called `scope` isn't a task of it, so it
                 // carried on; and a scope that has ended can't be cancelled.
                 let kept = task::scope(fn(s) { s });
                 kept.cancel();
                 (never, own, ran)
             }"
        ),
        "(true, true, 0)"
    );
}

#[test]
fn joining_a_cancelled_task_stops_the_program() {
    for stress in [false, true] {
        let msg = std_failure(
            "import std::task;
             fn main() {
                 task::scope(fn(s) {
                     let t = s.spawn(fn() -> int { sleep(100_000); 1 });
                     s.cancel();
                     t.join();
                 });
             }",
            stress,
        );
        assert!(
            msg.starts_with("cannot join a task that was cancelled"),
            "{msg}"
        );
    }
}

#[test]
fn a_task_s_error_says_where_it_was_spawned() {
    let msg = std_failure(
        "import std::task;
fn main() {
    task::scope(fn(s) {
        s.spawn(fn() {
            task::scope(fn(t) { t.spawn(fn() -> int { [1][5] }); });
        });
    });
}",
        false,
    );
    assert_eq!(
        msg,
        "index 5 is out of range for a list of length 1\n\
         in a task spawned at line 5, column 33\n\
         in a task spawned at line 4, column 9"
    );
}

#[test]
fn a_deadlock_says_what_each_task_waits_for() {
    let msg = std_failure(
        "import std::task;
fn main() {
    task::scope(fn(s) {
        var other: Option<task::Task<int>> = None;
        let a = s.spawn(fn() -> int { sleep(5); match other { Some(t) => t.join(), None => 0 } });
        other = Some(s.spawn(fn() -> int { a.join() }));
        a.join()
    });
}",
        false,
    );
    assert_eq!(
        msg,
        "deadlock: every task is waiting for another task, so none can go on\n\
         the task spawned at line 5, column 17 is joining a task, at line 5, column 74\n\
         the main task is joining a task, at line 7, column 9\n\
         the task spawned at line 6, column 22 is joining a task, at line 6, column 44"
    );
}

#[test]
fn a_cancelled_sleeper_doesnt_keep_the_program_running() {
    let start = std::time::Instant::now();
    std_value(
        "import std::task;
         fn main() { task::scope(fn(s) { s.spawn(fn() { sleep(100_000); }); s.cancel(); }); }",
    );
    assert!(start.elapsed().as_secs() < 5, "took {:?}", start.elapsed());
}
