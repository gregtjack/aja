use super::*;
use crate::{lexer::tokenize, parse::Parser};

fn check(src: &str) -> Result<(), Vec<TypeError>> {
    let ast = Parser::new(tokenize(src).unwrap().into_iter()).parse().unwrap();
    check_program(&ast).map(|_| ())
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
    ok("fn main() { let x = 1; var y = x * 2; assert(y == 2); print(\"y =\", y, \"\\n\"); }");
    ok("fn main() -> bool { let a = true; a && !false || 1.5 < 2.0 }");
    ok("fn main() -> str { var s = \"a\"; s = \"b\"; s }");
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
    ok("fn counter() -> (int) -> int { var c = 0; fn(x) { c = c + x; c } }
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
    ok("fn apply(f: (int) -> int, x: int) -> int { f(x) }
        fn main() -> int { apply(fn(n) { n * 3 }, 2) }");
    let (msg, at) = error(
        "fn apply(f: (int) -> int, x: int) -> int { f(x) }
fn main() -> int { apply(fn(s: str) { 1 }, 2) }",
    );
    assert_eq!(
        msg,
        "mismatched types: parameter 'f' of function 'apply' expects (int) -> int, found (str) -> int"
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
    assert_eq!(msg, "cannot apply '==' to () -> int and () -> int");
}

#[test]
fn numeric_operators_constrain_inferred_types() {
    // `a + b` restricts `a` to int, float, or str, so a bool doesn't fit.
    let (msg, at) = error("fn main() { let add = fn(a, b) { a + b }; add(true, false); }");
    assert_eq!(msg, "mismatched types: argument 1 of function 'add' expects {number or str}, found bool");
    assert_eq!(at, "true");
    // `a - b` restricts it to int or float.
    let (msg, _) = error("fn main() { let sub = fn(a, b) { a - b }; sub(\"x\", \"y\"); }");
    assert_eq!(msg, "mismatched types: argument 1 of function 'sub' expects {number}, found str");
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
    let (msg, at) = error("fn main() { var x = 1; x = \"s\"; }");
    assert_eq!(msg, "mismatched types: cannot assign str to 'x', which has type int");
    assert_eq!(at, "\"s\"");
    // Shadowing introduces a new variable, which may have a new type.
    ok("fn main() -> str { let a: int = 1; let a: str = \"s\"; a }");
}

#[test]
fn closures_can_call_themselves() {
    // The return type is inferred through the recursive call.
    ok("fn main() -> int { let fact = fn(n: int) { if n == 0 { 1 } else { n * fact(n - 1) } }; fact(5) }");
    ok("fn main() -> int { let fib = fn(n) { if n < 2 { n } else { fib(n - 1) + fib(n - 2) } }; fib(10) }");
    ok("fn main() { let f: (int) -> int = fn(n) { f(n) }; }");
    let (msg, _) = error("fn main() { let f = fn(x) { f(true); x + 1 }; }");
    assert_eq!(
        msg,
        "mismatched types: 'f' is used as (bool) -> _ inside its own definition, but it is (int) -> int"
    );
    // Only a closure sees its own name; other values see the earlier one.
    let (msg, _) = error("fn main() { let x = x + 1; }");
    assert_eq!(msg, "cannot find variable 'x' in this scope");
}

#[test]
fn only_var_can_be_assigned() {
    ok("fn main() -> int { var x = 1; x = x + 1; x }");
    ok("fn main() -> int { var x: int = 1; let f = fn() { x = 2; }; f(); x }");
    let (msg, at) = error("fn main() { let x = 1; x = 2; }");
    assert_eq!(msg, "cannot assign to 'x' because it was declared with 'let'");
    assert_eq!(at, "x = 2");
    assert_eq!(help("fn main() { let x = 1; x = 2; }"), "declare it with 'var' to make it mutable: `var x = ...;`");
    // Closures can't assign to a captured `let` either.
    let (msg, _) = error("fn main() { let x = 1; let f = fn() { x = 2; }; }");
    assert_eq!(msg, "cannot assign to 'x' because it was declared with 'let'");
    let (msg, at) = error("fn f(n: int) { n = 2; }");
    assert_eq!(msg, "cannot assign to 'n' because it is a parameter");
    assert_eq!(at, "n = 2");
    assert_eq!(help("fn f(n: int) { n = 2; }"), "copy it into a mutable variable first: `var n = n;`");
    let (msg, _) = error("fn f(x: int) { match x { n => { n = 2; } } }");
    assert_eq!(msg, "cannot assign to 'n' because it is bound by a pattern");
    // A `var` can shadow a `let` or a parameter, and the other way round.
    ok("fn f(n: int) -> int { var n = n; n = n + 1; n }");
    let (msg, _) = error("fn main() { var x = 1; let x = x; x = 2; }");
    assert_eq!(msg, "cannot assign to 'x' because it was declared with 'let'");
}

#[test]
fn calls() {
    let (msg, at) = error("fn g(x: int) -> int { x } fn main() -> int { g(1, 2) }");
    assert_eq!(msg, "function 'g' takes 1 argument but 2 were given");
    assert_eq!(at, "g(1, 2)");
    assert_eq!(error("fn main() { assert(); }").0, "function 'assert' takes 1 argument but 0 were given");
    assert_eq!(
        error("fn main() { sleep(\"long\"); }").0,
        "mismatched types: parameter 'ms' of function 'sleep' expects int, found str"
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
    assert_eq!(msg, "mismatched types: argument 1 of function 'g' expects _, found (_) -> _");
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

// ---- Structs, enums, and match ----------------------------------------------

const SHAPES: &str = "
struct Point { x: int, y: int }
enum Shape { Circle(Point, int), Rect(Point, Point), Empty }
enum Color { Red, Green, Blue }
";

/// Check `src` with the definitions in [`SHAPES`] before it.
fn with_shapes(src: &str) -> String {
    format!("{SHAPES}{src}")
}

#[track_caller]
fn shapes_ok(src: &str) {
    ok(&with_shapes(src));
}

#[track_caller]
fn shapes_error(src: &str) -> (String, String) {
    error(&with_shapes(src))
}

#[test]
fn structs_and_enums_check() {
    shapes_ok("fn main() -> int { let p = Point { x: 1, y: 2 }; p.x + p.y }");
    shapes_ok("fn main() -> Shape { Shape::Circle(Point { y: 2, x: 1 }, 3) }");
    shapes_ok("fn main() -> Color { Color::Red }");
    shapes_ok("fn origin(p: Point) -> Point { Point { x: 0, ..p } } fn main() { }");
    // Field shorthand.
    shapes_ok("fn main() -> Point { let x = 1; let y = 2; Point { x, y } }");
    // Types can refer to each other and to themselves, in any order.
    ok("enum List { Nil, Cons(Item, List) } struct Item { n: int } fn main() { }");
    // Inferred from use.
    shapes_ok("fn main() -> int { let f = fn(c) { match c { Color::Red => 1, _ => 2 } }; f(Color::Blue) }");
}

#[test]
fn struct_and_enum_equality() {
    shapes_ok("fn main() -> bool { (Point { x: 1, y: 2 } == Point { x: 1, y: 2 }) && (Color::Red != Color::Blue) }");
    let (msg, _) = error("struct F { f: () -> void } fn main() -> bool { let a = F { f: fn() { } }; a == a }");
    assert_eq!(msg, "cannot apply '==' to F and F");
    // Comparable types can contain themselves.
    ok("enum L { Nil, Cons(int, L) } fn main() -> bool { L::Nil == L::Cons(1, L::Nil) }");
}

#[test]
fn unknown_types_and_names() {
    let (msg, at) = error("fn f(p: Pont) { } struct Point { x: int } fn main() { }");
    assert_eq!(msg, "cannot find type 'Pont'");
    assert_eq!(at, "Pont");
    assert_eq!(help("fn f(p: Pont) { } struct Point { x: int } fn main() { }"), "a type with a similar name exists: 'Point'");
    let (msg, at) = shapes_error("fn main() { let p = Pointt { x: 1, y: 2 }; }");
    assert_eq!(msg, "cannot find struct 'Pointt'");
    assert_eq!(at, "Pointt");
    assert_eq!(shapes_error("fn main() { Shape { x: 1 }; }").0, "'Shape' is an enum, not a struct");
    assert_eq!(shapes_error("fn main() { Point::Origin; }").0, "'Point' is a struct, not an enum");
    let (msg, at) = shapes_error("fn main() { Shape::Square; }");
    assert_eq!(msg, "enum 'Shape' has no variant 'Square'");
    assert_eq!(at, "Shape::Square");
    assert_eq!(
        help(&with_shapes("fn main() { Color::Gren; }")),
        "a variant with a similar name exists: 'Color::Green'"
    );
    assert_eq!(
        help(&with_shapes("fn main() { let p = Point; }")),
        "'Point' is a struct; make one with `Point { ... }`"
    );
}

#[test]
fn bad_definitions() {
    assert_eq!(error("struct A { } enum A { } fn main() { }").0, "type 'A' is defined more than once");
    assert_eq!(error("struct int { } fn main() { }").0, "'int' is a built-in type, so it can't be redefined");
    assert_eq!(
        error("struct P { x: int, x: int } fn main() { }").0,
        "struct 'P' has more than one field named 'x'"
    );
    assert_eq!(error("enum E { A, A } fn main() { }").0, "enum 'E' has more than one variant named 'A'");
    assert_eq!(error("struct P { q: Q } fn main() { }").0, "cannot find type 'Q'");
}

#[test]
fn struct_literal_errors() {
    let (msg, at) = shapes_error("fn main() { Point { x: 1 }; }");
    assert_eq!(msg, "missing field 'y' in 'Point'");
    assert_eq!(at, "Point");
    assert_eq!(shapes_error("fn main() { Point { }; }").0, "missing fields 'x' and 'y' in 'Point'");
    let (msg, at) = shapes_error("fn main() { Point { x: 1, y: 2, z: 3 }; }");
    assert_eq!(msg, "struct 'Point' has no field 'z'");
    assert_eq!(at, "z");
    assert_eq!(shapes_error("fn main() { Point { x: 1, x: 2, y: 3 }; }").0, "field 'x' is given more than once");
    let (msg, at) = shapes_error("fn main() { Point { x: 1, y: true }; }");
    assert_eq!(msg, "mismatched types: field 'y' of 'Point' is int, but was given bool");
    assert_eq!(at, "true");
    assert_eq!(
        shapes_error("fn main() { Point { x: 1, ..Color::Red }; }").0,
        "mismatched types: the value after '..' must be a Point, found Color"
    );
}

#[test]
fn field_access_errors() {
    let (msg, at) = shapes_error("fn main() -> int { let p = Point { x: 1, y: 2 }; p.z }");
    assert_eq!(msg, "struct 'Point' has no field 'z'");
    assert_eq!(at, "z");
    assert_eq!(shapes_error("fn main() { let s = Shape::Empty; s.x; }").0, "'Shape' is an enum, so it has no fields");
    assert_eq!(shapes_error("fn main() { let n = 1; n.x; }").0, "int has no field 'x'");
    let (msg, at) = shapes_error("fn main() { let f = fn(p) { p.x }; }");
    assert!(msg.starts_with("type annotations needed"), "{msg}");
    assert_eq!(at, "p");
    // Annotating the parameter fixes it.
    shapes_ok("fn main() -> int { let f = fn(p: Point) { p.x }; f(Point { x: 1, y: 2 }) }");
}

#[test]
fn variant_construction_errors() {
    let (msg, at) = shapes_error("fn main() { Shape::Circle; }");
    assert_eq!(msg, "variant 'Shape::Circle' holds 2 values, so it must be given them");
    assert_eq!(at, "Shape::Circle");
    assert_eq!(
        shapes_error("fn main() { Shape::Empty(); }").0,
        "variant 'Shape::Empty' holds no values, so it can't be called"
    );
    assert_eq!(
        shapes_error("fn main() { Shape::Circle(Point { x: 1, y: 2 }); }").0,
        "variant 'Shape::Circle' holds 2 values but 1 was given"
    );
    let (msg, at) = shapes_error("fn main() { Shape::Circle(Point { x: 1, y: 2 }, 1.5); }");
    assert_eq!(msg, "mismatched types: value 2 of 'Shape::Circle' is int, found float");
    assert_eq!(at, "1.5");
}

#[test]
fn match_types() {
    shapes_ok(
        "fn area(s: Shape) -> int {
            match s {
                Shape::Circle(_, r) => 3 * r * r,
                Shape::Rect(a, b) => (b.x - a.x) * (b.y - a.y),
                Shape::Empty => 0,
            }
        }
        fn main() { }",
    );
    // Arms that return don't need to agree with the others.
    shapes_ok("fn f(c: Color) -> int { let n = match c { Color::Red => { return 0; }, _ => 1 }; n } fn main() { }");
    // A match can be a statement.
    shapes_ok("fn main() { match Color::Red { Color::Red => println(1), _ => { } } }");
    let (msg, at) = shapes_error("fn f(c: Color) -> int { match c { Color::Red => 1, _ => true } } fn main() { }");
    assert_eq!(msg, "match arms have incompatible types: int and bool");
    assert_eq!(at, "true");
    let (msg, at) = shapes_error("fn f(c: Color) -> int { match c { 1 => 1, _ => 2 } } fn main() { }");
    assert_eq!(msg, "mismatched types: this pattern matches int, but the value is Color");
    assert_eq!(at, "1");
    let (msg, _) = shapes_error("fn f(c: Color) -> int { match c { Color::Red if 1 => 1, _ => 2 } } fn main() { }");
    assert_eq!(msg, "a match guard must be a bool, found int");
}

#[test]
fn pattern_errors() {
    let (msg, at) = shapes_error("fn f(c: Color) -> int { match c { Red => 1, _ => 2 } } fn main() { }");
    assert_eq!(msg, "this pattern binds a new variable 'Red', which matches anything");
    assert_eq!(at, "Red");
    assert_eq!(
        help(&with_shapes("fn f(c: Color) -> int { match c { Red => 1, _ => 2 } } fn main() { }")),
        "to match the variant, write `Color::Red`"
    );
    assert_eq!(
        shapes_error("fn f(s: Shape) -> int { match s { Shape::Circle => 1, _ => 2 } } fn main() { }").0,
        "variant 'Shape::Circle' holds 2 values"
    );
    assert_eq!(
        shapes_error("fn f(s: Shape) -> int { match s { Shape::Circle(p) => 1, _ => 2 } } fn main() { }").0,
        "this pattern has 1 field but variant 'Shape::Circle' holds 2 values"
    );
    assert_eq!(
        shapes_error("fn f(p: Point) -> int { match p { Point { x } => x } } fn main() { }").0,
        "pattern does not mention field 'y'"
    );
    shapes_ok("fn f(p: Point) -> int { match p { Point { x, .. } => x } } fn main() { }");
    assert_eq!(
        shapes_error("fn f(p: Point) -> int { match p { Point { x: a, y: a } => a } } fn main() { }").0,
        "'a' is bound more than once in this pattern"
    );
}

#[test]
fn match_must_be_exhaustive() {
    let (msg, at) = shapes_error("fn f(c: Color) -> int { match c { Color::Red => 1, Color::Green => 2 } } fn main() { }");
    assert_eq!(msg, "non-exhaustive match: `Color::Blue` not covered");
    assert_eq!(at, "c");
    assert_eq!(
        help(&with_shapes("fn f(c: Color) -> int { match c { Color::Red => 1, Color::Green => 2 } } fn main() { }")),
        "add an arm for it: `Color::Blue => ...`"
    );
    assert_eq!(
        shapes_error("fn f(c: Color) -> int { match c { Color::Red => 1 } } fn main() { }").0,
        "non-exhaustive match: `Color::Green` and `Color::Blue` not covered"
    );
    assert_eq!(
        shapes_error("fn f(s: Shape) -> int { match s { Shape::Empty => 1 } } fn main() { }").0,
        "non-exhaustive match: `Shape::Circle(_, _)` and `Shape::Rect(_, _)` not covered"
    );
    assert_eq!(shapes_error("fn f(n: int) -> int { match n { 0 => 1 } } fn main() { }").0, "non-exhaustive match: `_` not covered");
    assert_eq!(shapes_error("fn f(b: bool) -> int { match b { true => 1 } } fn main() { }").0, "non-exhaustive match: `false` not covered");
    // Nested patterns.
    assert_eq!(
        error("enum L { Nil, Cons(int, L) } fn f(l: L) -> int { match l { L::Nil => 0, L::Cons(n, L::Nil) => n } } fn main() { }").0,
        "non-exhaustive match: `L::Cons(_, L::Cons(_, _))` not covered"
    );
    assert_eq!(
        shapes_error("fn f(p: Point) -> int { match p { Point { x: 0, y } => y } } fn main() { }").0,
        "non-exhaustive match: `Point { .. }` not covered"
    );
    // A guarded arm doesn't count.
    let src = with_shapes("fn f(c: Color) -> int { match c { Color::Red => 1, Color::Green => 2, Color::Blue if true => 3 } } fn main() { }");
    let errors = check(&src).unwrap_err();
    assert_eq!(errors[0].message, "non-exhaustive match: `Color::Blue` not covered");
    assert!(errors[0].notes.iter().any(|n| n.contains("guard")));
    // Many missing cases are summarized.
    assert_eq!(
        error("enum E { A, B, C, D, E } fn f(e: E) -> int { match e { E::A => 1 } } fn main() { }").0,
        "non-exhaustive match: `E::B`, `E::C`, `E::D` and more not covered"
    );
    // An enum with no variants needs no arms.
    ok("enum Never { } fn f(n: Never) -> int { match n { } } fn main() { }");
}

#[test]
fn unreachable_arms_are_errors() {
    let (msg, at) = shapes_error("fn f(c: Color) -> int { match c { _ => 1, Color::Red => 2 } } fn main() { }");
    assert_eq!(msg, "unreachable pattern");
    assert_eq!(at, "Color::Red");
    assert_eq!(
        shapes_error("fn f(n: int) -> int { match n { 1 => 1, 1 => 2, _ => 3 } } fn main() { }").0,
        "unreachable pattern"
    );
    assert_eq!(
        shapes_error("fn f(b: bool) -> int { match b { true => 1, false => 2, _ => 3 } } fn main() { }").0,
        "unreachable pattern"
    );
    // A guarded arm doesn't make later arms unreachable.
    shapes_ok("fn f(c: Color) -> int { match c { _ if false => 1, Color::Red => 2, _ => 3 } } fn main() { }");
}

#[test]
fn plus_concatenates_strings() {
    ok("fn main() -> str { \"a\" + \"b\" }");
    ok("fn main() -> str { let f = fn(a, b) { a + b }; f(\"x\", \"y\") }");
    assert_eq!(error("fn main() { \"a\" + 1; }").0, "cannot apply '+' to str and int");
    assert_eq!(error("fn main() { \"a\" - \"b\"; }").0, "cannot apply '-' to str and str");
    assert_eq!(error("fn main() { \"a\" < \"b\"; }").0, "cannot apply '<' to str and str");
}
