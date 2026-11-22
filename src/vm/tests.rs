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
                       let f = fn(n: int) -> int { n };
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
    let shown = |src: &str| {
        let (vm, result) = run_vm(src);
        vm.heap.display(&result.unwrap())
    };
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

    // A tail call replaces its caller's frame, so the caller isn't listed.
    let (_, notes) = located(
        "fn c(x: int) -> int { x / 0 }\nfn b(x: int) -> int { c(x) }\nfn main() { b(1); }",
    );
    assert_eq!(notes, vec!["in function 'c', tail-called at line 2, column 23"]);

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
    // `f` captures the cell it is stored in: a cycle.
    let recursive = "fn make() -> int {
                         let f = fn(n: int) -> int { n };
                         f = fn(n: int) -> int { if n == 0 { 0 } else { f(n - 1) } };
                         f(3)
                     }
                     fn main() { let i = 0; while i < 200 { make(); i = i + 1; } }";
    assert_eq!(live_after(recursive), 2);

    // Two closures that call each other through captured variables.
    let mutual = "fn make() -> bool {
                      let even = fn(n: int) -> bool { true };
                      let odd = fn(n: int) -> bool { if n == 0 { false } else { even(n - 1) } };
                      even = fn(n: int) -> bool { if n == 0 { true } else { odd(n - 1) } };
                      even(10)
                  }
                  fn main() { let i = 0; while i < 200 { assert(make()); i = i + 1; } }";
    assert_eq!(live_after(mutual), 2);
}

#[test]
fn reachable_objects_survive_collection() {
    // Every allocation collects (stress mode), while counters and their
    // cells are only reachable from registers and other closures.
    let src = "fn counter() -> fn() -> int { let c = 0; fn() { c = c + 1; c } }
               fn main() -> int {
                   let a = counter();
                   let total = 0;
                   let i = 0;
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
                   let i = 0;
                   let keep = fn() -> int { i };
                   while i < 10000 { let c = 0; let g = fn() -> int { c = c + 1; c }; g(); i = i + 1; }
                   keep()
               }";
    let ast = Parser::new(tokenize(src).unwrap().into_iter()).parse().unwrap();
    let types = check::check_program(&ast).unwrap();
    let program = Compiler::new().program(&ast, &types).unwrap();
    let mut vm = Vm::new();
    assert_eq!(format!("{:?}", vm.run_program(&program).unwrap()), "Int(10000)");
    assert!(vm.heap.live() < 3000, "{} objects live", vm.heap.live());
}

#[test]
fn statements_ending_in_a_block_need_no_semicolon() {
    // An `if` / `else` followed by more statements.
    assert_eq!(
        main_value("int", "let x = 0; if x == 0 { x = 1; } else { x = 2; } x = x * 10; x"),
        "Int(10)"
    );
    // A nested block keeps its statements, and its scope.
    assert_eq!(main_value("int", "let a = 1; { let a = 2; a = a + 1; } a"), "Int(1)");
    assert_eq!(main_value("int", "{ let b = 2; b = b * 3; } 4"), "Int(4)");
    assert_eq!(main_value("int", "{ 5 }"), "Int(5)");
}

#[test]
fn if_expressions_can_be_assigned() {
    assert_eq!(main_value("int", "let n = 3; n = if n > 2 { n * 2 } else { 0 }; n"), "Int(6)");
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
