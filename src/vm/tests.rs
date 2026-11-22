use super::*;
use crate::{check, lexer::tokenize, parse::Parser};

/// Check, compile, and run a program, returning the VM and `main`'s
/// result. The collector runs before every allocation, so a value the VM
/// forgot to keep reachable is freed, and its use then panics.
fn run_vm(src: &str) -> (Vm, Result<Value, RuntimeError>) {
    let ast = Parser::new(tokenize(src).unwrap().into_iter()).parse().unwrap();
    let types = match check::check_program(&ast) {
        Ok(types) => types,
        Err(errors) => {
            let msgs: Vec<&str> = errors.iter().map(|e| e.message.as_str()).collect();
            panic!("test program has type errors: {msgs:#?}");
        }
    };
    let program = Compiler::new().program(&ast, &types).expect("program compiles");
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
    assert_eq!(main_value("int", "var i = 0; var s = 0; while i < 5 { s = s + i; i = i + 1; } s"), "Int(10)");
    assert_eq!(main_value("int", "var x = 1; if true { x = 2; } x"), "Int(2)");
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

/// Run a program with the call stack limited to `limit` bytes.
fn run_limited(src: &str, limit: usize) -> Result<Value, RuntimeError> {
    let ast = Parser::new(tokenize(src).unwrap().into_iter()).parse().unwrap();
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
    let msg = run_limited(src, 1 << 20).expect_err("should overflow").message;
    assert!(msg.starts_with("stack overflow: the call stack has grown past 1 MB ("), "got {msg}");
}

#[test]
fn tail_calls_run_in_constant_space() {
    // A million calls deep, with room for only a handful of frames.
    let limit = 4096;
    let count = "fn count(n: int, acc: int) -> int { if n == 0 { acc } else { count(n - 1, acc + 1) } }
                 fn main() -> int { count(1000000, 0) }";
    assert_eq!(format!("{:?}", run_limited(count, limit).unwrap()), "Int(1000000)");

    // `return` and both branches of an `if` are tail positions, and mutual
    // recursion works too.
    let even = "fn is_even(n: int) -> bool { if n == 0 { return true; } return is_odd(n - 1); }
                fn is_odd(n: int) -> bool { if n == 0 { false } else { is_even(n - 1) } }
                fn main() -> bool { is_even(1000001) }";
    assert_eq!(format!("{:?}", run_limited(even, limit).unwrap()), "Bool(false)");

    // Closures, including ones that capture variables, can be tail-called.
    let closure = "fn main() -> int {
                       let step = 1;
                       var f = fn(n: int) -> int { n };
                       f = fn(n: int) -> int { if n >= 1000000 { n } else { f(n + step) } };
                       f(0)
                   }";
    assert_eq!(format!("{:?}", run_limited(closure, limit).unwrap()), "Int(1000000)");

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
    let src = "fn big(x: int) -> int { let a = x + 1; let b = a + 1; let c = b + 1; let d = c + 1; d }
               fn small(x: int) -> int { big(x) }
               fn main() -> int { small(1) }";
    assert_eq!(value(src), "Int(5)");
    assert_eq!(value("fn main() { println(\"tail-called built-in\") }"), "Void");
}

#[test]
fn functions_can_be_called_before_they_are_defined() {
    let src = "fn main() -> int { double(21) } fn double(x: int) -> int { x * 2 }";
    assert_eq!(value(src), "Int(42)");
}

#[test]
fn assignment_is_an_expression() {
    assert_eq!(main_value("int", "var a = 1; let b = (a = 5) + a; b"), "Int(10)");
    // The right operand changes `a` only after the left one has been read.
    assert_eq!(main_value("int", "var a = 1; a + (a = 5)"), "Int(6)");
    assert_eq!(main_value("bool", "var a = true; let b = false; a = b && a; a"), "Bool(false)");
}

#[test]
fn let_shadowing() {
    assert_eq!(main_value("int", "let a = 1; let a = a + 1; a"), "Int(2)");
    assert_eq!(main_value("str", "let a: int = 1; let a: str = \"s\"; a"), "Str(\"s\")");
    assert_eq!(main_value("int", "let a = 1; if true { let a = 2; } a"), "Int(1)");
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
/// functions and the closures in them.
fn cells_made(src: &str) -> usize {
    fn count(p: &Proto) -> usize {
        let here = p.code.iter().filter(|op| matches!(op, Op::NewCell { .. })).count();
        here + p.protos.iter().map(|p| count(p)).sum::<usize>()
    }
    let ast = Parser::new(tokenize(src).unwrap().into_iter()).parse().unwrap();
    let types = check::check_program(&ast).unwrap();
    let program = Compiler::new().program(&ast, &types).unwrap();
    program.functions.iter().map(|(_, p)| count(p)).sum()
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
    let src = "fn main() -> int { var a = 1; var b = 2; let f = fn() { a = a + 1; }; f(); b = a; b }";
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
    assert_eq!(format!("{:?}", run_limited(src, 4096).unwrap()), "Int(100000)");
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
    assert_eq!(value("fn main() -> int { let a = 1; let a = a + 1; a }"), "Int(2)");
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
    assert_eq!(shown("fn main() -> (int) -> int { fn(x: int) -> int { x } }"), "<fn (x: int) -> int>");
    assert_eq!(shown("fn main() -> () -> int { clock }"), "<native fn: clock>");
    assert_eq!(shown("fn main() { }"), "()");
}

#[test]
fn integer_errors() {
    assert_eq!(failure("fn main() -> int { 7 / 0 }"), "division by zero");
    assert_eq!(failure("fn main() -> int { 7 % 0 }"), "modulo by zero");
    assert_eq!(failure("fn main() -> int { 9223372036854775807 + 1 }"), "integer overflow in '+'");
    assert_eq!(failure("fn main() -> int { let m = -9223372036854775807 - 1; -m }"), "integer overflow in '-'");
    assert_eq!(failure("fn main() -> int { let m = -9223372036854775807 - 1; m / -1 }"), "integer overflow in '/'");
}

#[test]
fn ints_are_64_bit() {
    assert_eq!(main_value("int", "2147483647 + 1"), "Int(2147483648)");
    assert_eq!(main_value("int", "3_000_000_000 * 3"), "Int(9000000000)");
    assert_eq!(main_value("int", "-9223372036854775807 - 1"), "Int(-9223372036854775808)");
    // Milliseconds since 1970 fit, and are after 2020-01-01.
    assert_eq!(main_value("bool", "clock() > 1_577_836_800_000"), "Bool(true)");
}

#[test]
fn floats_are_64_bit() {
    // An f32 would round these; an f64 keeps them.
    assert_eq!(main_value("float", "16777217.0"), "Float(16777217.0)");
    assert_eq!(main_value("float", "0.1 + 0.2"), "Float(0.30000000000000004)");
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
    let (_, notes) = located(
        "fn c(x: int) -> int { x / 0 }\nfn b(x: int) -> int { c(x) }\nfn main() { b(1); }",
    );
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
        let stmt = Parser::new(tokenize(src).unwrap().into_iter()).parse_stmt_only().unwrap();
        let types = checker.check_repl_stmt(&stmt).unwrap();
        let span = Span::new(crate::ast::Location::new(0, 1, 1), crate::ast::Location::new(0, 1, 1));
        let input = compiler.repl(&stmt, &types, span);
        let v = vm.run_repl(&input).unwrap();
        show(&vm, &v)
    };
    assert_eq!(eval("var x = 1"), "Void");
    assert_eq!(eval("let add = fn(n: int) -> int { x = x + n; x }"), "Void");
    assert_eq!(eval("add(4)"), "Int(5)");
    assert_eq!(eval("x"), "Int(5)");
    assert_eq!(eval("{ let y = 2; let g = fn() -> int { y * x }; g() }"), "Int(10)");
    // A new `x` doesn't change the one `add` uses.
    assert_eq!(eval("let x = \"s\""), "Void");
    assert_eq!(eval("add(1)"), "Int(6)");
    assert_eq!(eval("x"), "Str(\"s\")");
    // A top-level closure can call itself.
    assert_eq!(eval("let f = fn(n: int) -> int { if n == 0 { 0 } else { 2 + f(n - 1) } }"), "Void");
    assert_eq!(eval("f(4)"), "Int(8)");
}

/// How many heap objects are left once `src` has run and the collector has
/// freed what is unreachable. Its top-level functions are always left.
fn live_after(src: &str) -> usize {
    let (mut vm, result) = run_vm(src);
    result.expect("program runs");
    vm.collect_garbage();
    vm.heap.live()
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
    let ast = Parser::new(tokenize(src).unwrap().into_iter()).parse().unwrap();
    let types = check::check_program(&ast).unwrap();
    let program = Compiler::new().program(&ast, &types).unwrap();
    let mut vm = Vm::new();
    assert_eq!(format!("{:?}", vm.run_program(&program).unwrap()), "Int(10000)");
    assert!(vm.heap.live() < 3000, "{} objects live", vm.heap.live());
}

// ---- Structs, enums, and match ----------------------------------------------

const DATA: &str = "
struct Point { x: int, y: int }
enum Shape { Circle(Point, int), Rect(Point, Point), Empty }
enum List { Nil, Cons(int, List) }
fn range(n: int) -> List { var n = n; var l = List::Nil; while n > 0 { n = n - 1; l = List::Cons(n, l); } l }
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
    assert_eq!(data_value("Point", "Point { x: 1, y: 2 }"), "Point { x: 1, y: 2 }");
    // Fields can be written in any order.
    assert_eq!(data_value("Point", "Point { y: 2, x: 1 }"), "Point { x: 1, y: 2 }");
    assert_eq!(data_value("int", "let p = Point { x: 3, y: 4 }; p.x * 10 + p.y"), "34");
    assert_eq!(data_value("Point", "let x = 5; let y = 6; Point { x, y }"), "Point { x: 5, y: 6 }");
    assert_eq!(data_value("Point", "let p = Point { x: 1, y: 2 }; Point { y: 9, ..p }"), "Point { x: 1, y: 9 }");
    // Fields of fields, and of call results.
    let src = "struct Line { a: Point, b: Point }
               fn line() -> Line { Line { a: Point { x: 1, y: 2 }, b: Point { x: 3, y: 4 } } }";
    assert_eq!(value(&format!("{DATA} {src} fn main() -> int {{ line().b.x }}")), "Int(3)");
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
        data_value("Shape", "Shape::Rect(Point { x: 0, y: 0 }, Point { x: 2, y: 3 })"),
        "Shape::Rect(Point { x: 0, y: 0 }, Point { x: 2, y: 3 })"
    );
    assert_eq!(data_value("List", "range(3)"), "List::Cons(0, List::Cons(1, List::Cons(2, List::Nil)))");
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
    assert_eq!(value(&format!("{DATA} {area} fn main() -> int {{ {main} }}")), "Int(120600)");

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
        assert_eq!(value(&src), format!("Str({expected:?})"), "for Point {{ x: {p} }}");
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
    let src = "fn second(l: List) -> int {
                   match l {
                       List::Cons(_, List::Cons(x, _)) => x,
                       List::Cons(x, List::Nil) => -x,
                       List::Nil => -100,
                   }
               }
               fn main() -> int { second(range(5)) * 100 + second(List::Cons(7, List::Nil)) * 10 + second(List::Nil) }";
    assert_eq!(value(&format!("{DATA} {src}")), "Int(-70)");
}

#[test]
fn match_is_an_expression_and_a_statement() {
    assert_eq!(
        data_value("int", "var n = 0; match Shape::Empty { Shape::Empty => { n = 5; }, _ => { } } n"),
        "5"
    );
    assert_eq!(data_value("int", "let s = Shape::Empty; 1 + match s { Shape::Empty => 1, _ => 2 } * 10"), "11");
    // A `return` inside an arm returns from the function.
    let src = "fn f(s: Shape) -> int { let n = match s { Shape::Empty => { return 7; }, _ => 1 }; n * 2 }
               fn main() -> int { f(Shape::Empty) * 10 + f(Shape::Circle(Point { x: 0, y: 0 }, 1)) }";
    assert_eq!(value(&format!("{DATA} {src}")), "Int(72)");
}

#[test]
fn guards_see_bindings_and_fall_through() {
    let src = "fn classify(l: List) -> int {
                   match l {
                       List::Cons(n, _) if n > 10 => 2,
                       List::Cons(n, _) if n > 0 => 1,
                       _ => 0,
                   }
               }
               fn main() -> int { classify(List::Cons(50, List::Nil)) * 100 + classify(List::Cons(5, List::Nil)) * 10 + classify(List::Cons(-1, List::Nil)) }";
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
    let src = "fn adder(l: List) -> (int) -> int {
                   match l { List::Cons(n, _) => fn(x) { x + n }, List::Nil => fn(x) { x } }
               }
               fn main() -> int { let f = adder(List::Cons(40, List::Nil)); f(2) }";
    assert_eq!(value(&format!("{DATA} {src}")), "Int(42)");
}

#[test]
fn match_arms_are_tail_positions() {
    let src = "fn len(l: List, acc: int) -> int { match l { List::Nil => acc, List::Cons(_, t) => len(t, acc + 1) } }
               fn main() -> int { len(range(100000), 0) }";
    assert_eq!(format!("{:?}", run_limited(&format!("{DATA} {src}"), 4096).unwrap()), "Int(100000)");
}

#[test]
fn structural_equality() {
    assert_eq!(data_value("bool", "Point { x: 1, y: 2 } == Point { x: 1, y: 2 }"), "true");
    assert_eq!(data_value("bool", "Point { x: 1, y: 2 } != Point { x: 1, y: 3 }"), "true");
    assert_eq!(data_value("bool", "Shape::Empty == Shape::Empty"), "true");
    assert_eq!(data_value("bool", "Shape::Empty == Shape::Circle(Point { x: 0, y: 0 }, 1)"), "false");
    assert_eq!(data_value("bool", "range(3) == range(3)"), "true");
    assert_eq!(data_value("bool", "range(3) == range(4)"), "false");
}

#[test]
fn long_lists_are_compared_and_shown_without_deep_recursion() {
    // Without the stress-mode collector, which would make building these
    // lists quadratic.
    let run = |ret: &str, body: &str| {
        let src = format!("{DATA} fn main() -> {ret} {{ {body} }}");
        let ast = Parser::new(tokenize(&src).unwrap().into_iter()).parse().unwrap();
        let types = check::check_program(&ast).unwrap();
        let program = Compiler::new().program(&ast, &types).unwrap();
        let mut vm = Vm::new();
        let v = vm.run_program(&program).unwrap();
        vm.heap.display(&v)
    };
    let shown = run("List", "range(200000)");
    assert!(shown.starts_with("List::Cons(0, List::Cons(1, "));
    assert!(shown.ends_with(&format!("List::Cons(199999, List::Nil{}", ")".repeat(200000))));
    assert_eq!(run("bool", "range(200000) == range(200000)"), "true");
}

#[test]
fn struct_and_enum_values_are_collected() {
    // Nothing a program builds outlives it; only the top-level functions
    // stay, as in a program that builds nothing.
    let busy = format!(
        "{DATA} fn main() {{
             var i = 0;
             var keep = range(10);
             while i < 20 {{ range(50); keep = List::Cons(i, keep); i = i + 1; }}
             assert(keep != List::Nil);
         }}"
    );
    let idle = format!("{DATA} fn main() {{ }}");
    assert_eq!(live_after(&busy), live_after(&idle));
}

#[test]
fn statements_ending_in_a_block_need_no_semicolon() {
    // An `if` / `else` followed by more statements.
    assert_eq!(
        main_value("int", "var x = 0; if x == 0 { x = 1; } else { x = 2; } x = x * 10; x"),
        "Int(10)"
    );
    // A nested block keeps its statements, and its scope.
    assert_eq!(main_value("int", "var a = 1; { var a = 2; a = a + 1; } a"), "Int(1)");
    assert_eq!(main_value("int", "{ var b = 2; b = b * 3; } 4"), "Int(4)");
    assert_eq!(main_value("int", "{ 5 }"), "Int(5)");
}

#[test]
fn if_expressions_can_be_assigned() {
    assert_eq!(main_value("int", "var n = 3; n = if n > 2 { n * 2 } else { 0 }; n"), "Int(6)");
}

/// Run an Aja source file through every stage, returning the messages of
/// the errors that stopped it.
fn run_source(src: &str) -> Result<(), Vec<String>> {
    let tokens = tokenize(src).map_err(|e| vec![e.to_diagnostic().message])?;
    let ast = Parser::new(tokens.into_iter())
        .parse()
        .map_err(|e| vec![e.to_diagnostic().message])?;
    let types = check::check_program(&ast)
        .map_err(|errors| errors.into_iter().map(|e| e.message).collect::<Vec<_>>())?;
    let program = Compiler::new().program(&ast, &types).map_err(|d| vec![d.message])?;
    Vm::new().run_program(&program).map_err(|e| vec![e.message])?;
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
fn error_examples_fail_as_described() {
    // Each file lists the errors it should produce in `// expect:` lines.
    for path in aja_files("examples/errors") {
        let src = std::fs::read_to_string(&path).unwrap();
        let expected: Vec<&str> = src
            .lines()
            .filter_map(|l| l.trim().strip_prefix("// expect: "))
            .collect();
        assert!(!expected.is_empty(), "{} has no `// expect:` lines", path.display());
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
    assert_eq!(main_value("str", "var s = \"\"; var i = 0; while i < 3 { s = s + \"ab\"; i = i + 1; } s"), "Str(\"ababab\")");
    // Inferred: `a + b` on strings.
    assert_eq!(main_value("str", "let join = fn(a, b) { a + \" \" + b }; join(\"hello\", \"world\")"), "Str(\"hello world\")");
    // A built string equals a literal with the same contents.
    assert_eq!(main_value("bool", "\"ab\" + \"c\" == \"abc\""), "Bool(true)");
    assert_eq!(main_value("bool", "\"ab\" + \"c\" != \"ab\""), "Bool(true)");
    assert_eq!(main_value("int", "match \"a\" + \"b\" { \"a\" => 1, \"ab\" => 2, _ => 3 }"), "Int(2)");
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
    let ast = Parser::new(tokenize(src).unwrap().into_iter()).parse().unwrap();
    let types = check::check_program(&ast).unwrap();
    let program = Compiler::new().program(&ast, &types).unwrap();
    let mut vm = Vm::new();
    assert_eq!(format!("{:?}", vm.run_program(&program).unwrap()), "Int(1000)");
    assert!(vm.heap.bytes() < 4 << 20, "{} bytes live", vm.heap.bytes());
}

#[test]
fn repl_literals_are_kept_and_renumbered_after_a_failed_input() {
    let mut checker = check::Checker::new();
    let mut compiler = Compiler::new();
    let mut vm = Vm::new();
    vm.heap.stress = true;
    let mut eval = |src: &str| {
        let stmt = Parser::new(tokenize(src).unwrap().into_iter()).parse_stmt_only().unwrap();
        let saved = (checker.clone(), compiler.clone());
        let types = checker.check_repl_stmt(&stmt).unwrap();
        let span = Span::new(crate::ast::Location::new(0, 1, 1), crate::ast::Location::new(0, 1, 1));
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
    assert_eq!(eval("let greet = fn(n: str) -> str { \"hi \" + n }"), "Void");
    // This input's literal is numbered, then rolled back with the compiler.
    assert_eq!(eval("{ let s = \"lost\"; assert(false); s }"), "assertion failed");
    // So the next literal reuses its number, and must not see "lost".
    assert_eq!(eval("greet(\"there\")"), "Str(\"hi there\")");
    assert_eq!(eval("\"hi \" + \"again\""), "Str(\"hi again\")");
}

#[test]
fn generic_code_runs() {
    let types = "enum List<T> { Nil, Cons(T, List<T>) }
                 struct Pair<A, B> { first: A, second: B }
                 fn map<T, U>(xs: List<T>, f: (T) -> U) -> List<U> {
                     match xs { List::Nil => List::Nil, List::Cons(x, rest) => List::Cons(f(x), map(rest, f)) }
                 }
                 fn fold<T, A>(xs: List<T>, acc: A, f: (A, T) -> A) -> A {
                     match xs { List::Nil => acc, List::Cons(x, rest) => fold(rest, f(acc, x), f) }
                 }
                 fn get<T>(o: Option<T>, d: T) -> T { match o { Some(x) => x, None => d } }";
    let main = |ret: &str, body: &str| value(&format!("{types} fn main() -> {ret} {{ {body} }}"));

    let xs = "List::Cons(1, List::Cons(2, List::Cons(3, List::Nil)))";
    assert_eq!(main("int", &format!("fold(map({xs}, fn(x) {{ x * 10 }}), 0, fn(a, x) {{ a + x }})")), "Int(60)");
    assert_eq!(
        main("str", &format!("fold(map({xs}, fn(x: int) -> str {{ \"ab\" }}), \"\", fn(a, s) {{ a + s }})")),
        "Str(\"ababab\")"
    );
    assert_eq!(main("int", "get(Some(5), 0) + get(None, 7)"), "Int(12)");
    assert_eq!(main("str", "let p = Pair { first: 1, second: \"one\" }; Pair { first: p.second, second: p.first }.first"), "Str(\"one\")");
    assert_eq!(main("bool", "Some(Pair { first: 1, second: \"x\" }) == Some(Pair { first: 1, second: \"x\" })"), "Bool(true)");
    assert_eq!(main("bool", "Some(1) == None"), "Bool(false)");
    // Generic recursion in tail position runs in constant space.
    let src = format!(
        "{types} fn count<T>(xs: List<T>, n: int) -> int {{ match xs {{ List::Nil => n, List::Cons(_, t) => count(t, n + 1) }} }}
         fn build(n: int, acc: List<bool>) -> List<bool> {{ if n == 0 {{ acc }} else {{ build(n - 1, List::Cons(true, acc)) }} }}
         fn main() -> int {{ count(build(100000, List::Nil), 0) }}"
    );
    assert_eq!(format!("{:?}", run_limited(&src, 4096).unwrap()), "Int(100000)");
}

#[test]
fn question_mark_returns_early() {
    let lib = "fn digit(s: str) -> Result<int, str> { match s { \"1\" => Ok(1), \"2\" => Ok(2), _ => Err(\"bad: \" + s) } }
               fn sum(a: str, b: str) -> Result<int, str> { Ok(digit(a)? + digit(b)?) }
               fn show(r: Result<int, str>) -> str { match r { Ok(_) => \"ok\", Err(e) => e } }
               fn first(o: Option<int>) -> Option<int> { let n = o?; Some(n * 10) }";
    let main = |ret: &str, body: &str| value(&format!("{lib} fn main() -> {ret} {{ {body} }}"));
    assert_eq!(main("int", "match sum(\"1\", \"2\") { Ok(n) => n, Err(_) => -1 }"), "Int(3)");
    // The first error is returned, and nothing after it runs.
    assert_eq!(main("str", "show(sum(\"x\", \"y\"))"), "Str(\"bad: x\")");
    assert_eq!(main("str", "show(sum(\"1\", \"y\"))"), "Str(\"bad: y\")");
    assert_eq!(main("bool", "(first(Some(4)) == Some(40)) && (first(None) == None)"), "Bool(true)");
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
