use std::collections::HashMap;

use super::*;
use crate::vm::Vm;

/// The files of a program, `main.aja` being the one run.
fn files(files: &[(&str, &str)]) -> HashMap<String, String> {
    files
        .iter()
        .map(|(p, t)| (p.to_string(), t.to_string()))
        .collect()
}

/// Every stage's diagnostics, or the program's value, for a program whose
/// files are `fs`.
fn run(fs: &[(&str, &str)]) -> Result<String, Vec<Diagnostic>> {
    let (files, compiled) = build(&files(fs), "main.aja");
    let compiled = compiled?;
    let mut vm = Vm::new();
    vm.heap.stress = true;
    match vm.run_program(&compiled) {
        Ok(v) => Ok(vm.heap.repr(&v)),
        Err(e) => Err(vec![e.to_diagnostic_in(&files)]),
    }
}

fn value(fs: &[(&str, &str)]) -> String {
    match run(fs) {
        Ok(v) => v,
        Err(errors) => {
            let msgs: Vec<&str> = errors.iter().map(|e| e.message.as_str()).collect();
            panic!("program failed: {msgs:#?}")
        }
    }
}

/// The messages of the errors that stop a program.
fn errors(fs: &[(&str, &str)]) -> Vec<String> {
    match run(fs) {
        Ok(v) => panic!("program should fail, but gave {v}"),
        Err(errors) => errors.into_iter().map(|e| e.message).collect(),
    }
}

/// The first error, rendered without color.
fn rendered(fs: &[(&str, &str)]) -> String {
    let (files, compiled) = build(&files(fs), "main.aja");
    match compiled {
        Ok(_) => panic!("program should fail"),
        Err(errors) => errors[0].render_in(&files, false),
    }
}

const SHAPES: &str = "
pub struct Point { pub x: int, pub y: int }

pub enum Shape {
    Circle(Point, int),
    Rect(Point, Point),
}

pub fn area(s: Shape) -> int {
    match s {
        Shape::Circle(_, r) => 3 * r * r,
        Shape::Rect(a, b) => width(a, b) * (b.y - a.y),
    }
}

fn width(a: Point, b: Point) -> int { b.x - a.x }

impl Point {
    pub fn origin() -> Point { Point { x: 0, y: 0 } }
    pub fn sum(self) -> int { self.x + self.y }
    fn secret(self) -> int { 7 }
}
";

#[test]
fn a_module_is_used_through_its_name() {
    let main = "
import shapes;

fn main() -> int {
    let p = shapes::Point { x: 2, y: 3 };
    let s = shapes::Shape::Rect(shapes::Point::origin(), p);
    let r = match s {
        shapes::Shape::Rect(_, shapes::Point { x, .. }) => x,
        shapes::Shape::Circle(_, _) => 0,
    };
    shapes::area(s) + p.sum() * 100 + r * 1000
}";
    // area 6, sum 5, r 2
    assert_eq!(value(&[("main.aja", main), ("shapes.aja", SHAPES)]), "2506");
}

#[test]
fn items_can_be_imported_by_name() {
    let main = "
import shapes::{Point, Shape, area};

fn main() -> int {
    let s: Shape = Shape::Circle(Point::origin(), 2);
    area(s)
}";
    assert_eq!(value(&[("main.aja", main), ("shapes.aja", SHAPES)]), "12");
}

#[test]
fn directories_are_module_paths() {
    let main = "
import geo::shapes;
import geo::util::{double};

fn main() -> int { double(shapes::area(shapes::Shape::Circle(shapes::Point::origin(), 1))) }";
    let util = "pub fn double(x: int) -> int { x * 2 }";
    let fs = [
        ("main.aja", main),
        ("geo/shapes.aja", SHAPES),
        ("geo/util.aja", util),
    ];
    assert_eq!(value(&fs), "6");
}

#[test]
fn modules_can_share_names() {
    let a = "pub fn get() -> int { helper() } fn helper() -> int { 1 } pub struct T { pub v: int }";
    let b =
        "pub fn get() -> int { helper() } fn helper() -> int { 20 } pub struct T { pub v: int }";
    let main = "
import a;
import b;

fn helper() -> int { 300 }

fn main() -> int {
    let t = a::T { v: 4000 };
    a::get() + b::get() + helper() + t.v
}";
    assert_eq!(
        value(&[("main.aja", main), ("a.aja", a), ("b.aja", b)]),
        "4321"
    );
}

#[test]
fn locals_shadow_imported_names() {
    let util = "pub fn twice(x: int) -> int { x * 2 }";
    let main = "
import util::{twice};

fn main() -> int {
    let before = twice(1);
    let twice = fn(x: int) -> int { x * 10 };
    let after = twice(1);
    let m = match 5 { twice => twice };
    before + after + m
}";
    assert_eq!(value(&[("main.aja", main), ("util.aja", util)]), "17");
}

#[test]
fn modules_can_import_modules() {
    let a = "import b; pub fn f() -> int { b::g() + 1 }";
    let b = "import c::{Num}; pub fn g() -> int { let n = Num { v: 41 }; n.v } ";
    let c = "pub struct Num { pub v: int }";
    let main = "import a; import b; fn main() -> int { a::f() + b::g() }";
    let fs = [("main.aja", main), ("a.aja", a), ("b.aja", b), ("c.aja", c)];
    assert_eq!(value(&fs), "83");
}

#[test]
fn private_items_are_errors() {
    let main = "
import shapes;
import shapes::{width};

fn main() -> int { shapes::width(shapes::Point::origin(), shapes::Point::origin()) }";
    assert_eq!(
        errors(&[("main.aja", main), ("shapes.aja", SHAPES)]),
        [
            "'width' is private to module 'shapes'",
            "'width' is private to module 'shapes'"
        ]
    );
    let hidden = "struct Hidden { x: int }";
    let main = "import hidden; fn main() { let h: hidden::Hidden = hidden::Hidden { x: 1 }; }";
    assert_eq!(
        errors(&[("main.aja", main), ("hidden.aja", hidden)]),
        [
            "'Hidden' is private to module 'hidden'",
            "'Hidden' is private to module 'hidden'",
            "struct 'hidden::Hidden' has private fields, so it can only be built in module 'hidden'",
        ]
    );
}

#[test]
fn name_and_type_errors_are_reported_together() {
    // A private item is still checked where it's used, and a name that
    // couldn't be resolved isn't reported again by the checker.
    let main = "
import shapes;
import shapes::{width, nothing};

fn main() -> int {
    let a: int = shapes::width(shapes::Point::origin(), true);
    let b = nothing(1) + shapes::missing;
    let c: str = 1;
    let d: shapes::Nope = 2;
    match a {
        nothing => 0,
    };
    a + b
}";
    assert_eq!(
        errors(&[("main.aja", main), ("shapes.aja", SHAPES)]),
        [
            "'width' is private to module 'shapes'",
            "module 'shapes' has no item named 'nothing'",
            "'width' is private to module 'shapes'",
            "mismatched types: parameter 'b' of function 'shapes::width' expects shapes::Point, found bool",
            "module 'shapes' has no item named 'missing'",
            "mismatched types: 'c' is declared as str but was given int",
            "module 'shapes' has no item named 'Nope'",
        ]
    );
    // Errors in other files come after the file being run's, in order.
    let fs = [
        (
            "main.aja",
            "import m;\nfn main() { let x: bool = 1; m::f(); }",
        ),
        ("m.aja", "import main2;\npub fn f() -> int { true }"),
    ];
    let msgs = errors(&fs);
    assert_eq!(msgs[0], "no module named 'main2'");
    let fs = [
        (
            "main.aja",
            "import m;\nfn main() { let x: bool = 1; m::g(); }",
        ),
        ("m.aja", "pub fn f() -> int { true }"),
    ];
    assert_eq!(
        errors(&fs),
        [
            "mismatched types: 'x' is declared as bool but was given int",
            "module 'm' has no item named 'g'",
            "mismatched types: function 'm::f' is declared to return int, but this is bool",
        ]
    );
}

#[test]
fn private_methods_and_fields_are_errors() {
    let m = "
pub struct Counter { pub name: str, n: int }
impl Counter {
    pub fn new(name: str) -> Counter { Counter { name, n: 0 } }
    pub fn count(self) -> int { self.n }
    fn reset(self) -> Counter { Counter { n: 0, ..self } }
    fn zero() -> int { 0 }
}";
    let fs = |main| [("main.aja", main), ("m.aja", m)];
    assert_eq!(
        value(&fs(
            "import m; fn main() -> int { let c = m::Counter::new(\"a\"); c.count() }"
        )),
        "0"
    );
    assert_eq!(
        errors(&fs(
            "import m; fn main() -> int { let c = m::Counter::new(\"a\"); c.reset().n }"
        )),
        [
            "method 'reset' of 'm::Counter' is private",
            "field 'n' of struct 'm::Counter' is private"
        ]
    );
    assert_eq!(
        errors(&fs("import m; fn main() -> int { m::Counter::zero() }")),
        ["associated function 'zero' of 'm::Counter' is private"]
    );
    assert_eq!(
        errors(&fs(
            "import m; fn main() { let c = m::Counter { name: \"a\", n: 1 }; }"
        )),
        ["struct 'm::Counter' has private fields, so it can only be built in module 'm'"]
    );
    assert_eq!(
        errors(&fs(
            "import m; fn main() -> str { match m::Counter::new(\"a\") { m::Counter { name } => name } }"
        )),
        ["struct 'm::Counter' has private fields, so a pattern for it must end in '..'"]
    );
    assert_eq!(
        errors(&fs(
            "import m; fn main() -> int { match m::Counter::new(\"a\") { m::Counter { n, .. } => n } }"
        )),
        ["field 'n' of struct 'm::Counter' is private"]
    );
    assert_eq!(
        value(&fs(
            "import m; fn main() -> str { match m::Counter::new(\"a\") { m::Counter { name, .. } => name } }"
        )),
        "\"a\""
    );
}

#[test]
fn missing_modules_and_items_are_errors() {
    let errs = errors(&[("main.aja", "import geo::shapes; fn main() {}")]);
    assert_eq!(errs, ["no module named 'geo::shapes'"]);
    let out = rendered(&[("main.aja", "import geo::shapes; fn main() {}")]);
    assert!(out.contains("--> main.aja:1:8"), "{out}");
    assert!(out.contains("would be in 'geo/shapes.aja'"), "{out}");

    let fs = [
        ("main.aja", "import shapes::{areas}; fn main() {}"),
        ("shapes.aja", SHAPES),
    ];
    assert_eq!(errors(&fs), ["module 'shapes' has no item named 'areas'"]);
    assert!(rendered(&fs).contains("a similar name exists: 'area'"));

    let fs = [
        ("main.aja", "fn main() -> int { shapes::area(1) }"),
        ("shapes.aja", SHAPES),
    ];
    // Without an import, `shapes` is unknown to the checker as a type.
    assert!(!errors(&fs).is_empty());

    let fs = [
        ("main.aja", "fn main() { let p: shapes::Point = 1; }"),
        ("shapes.aja", SHAPES),
    ];
    assert_eq!(
        errors(&fs),
        ["'shapes' is not a module imported in this file"]
    );
}

#[test]
fn import_cycles_are_errors() {
    let fs = [
        ("main.aja", "import a; fn main() {}"),
        ("a.aja", "import b; pub fn f() {}"),
        ("b.aja", "import c;"),
        ("c.aja", "import a;"),
    ];
    assert_eq!(errors(&fs), ["import cycle: 'a' -> 'b' -> 'c' -> 'a'"]);
    let out = rendered(&fs);
    assert!(out.contains("--> c.aja:1:8"), "{out}");
    assert!(out.contains("'a' imports 'b' at a.aja:1:8"), "{out}");

    let fs = [("main.aja", "import main; fn main() {}")];
    assert_eq!(errors(&fs), ["module 'main' imports itself"]);
}

#[test]
fn imports_must_not_clash() {
    let fs = [
        ("main.aja", "import a::util; import b::util; fn main() {}"),
        ("a/util.aja", ""),
        ("b/util.aja", ""),
    ];
    assert_eq!(
        errors(&fs),
        ["two modules are imported as 'util': 'a::util' and 'b::util'"]
    );
    let fs = [
        (
            "main.aja",
            "import shapes::{area}; fn area() {} fn main() {}",
        ),
        ("shapes.aja", SHAPES),
    ];
    assert_eq!(
        errors(&fs),
        ["'area' is imported from 'shapes', but this file already has a 'area'"]
    );
}

#[test]
fn methods_live_with_their_type() {
    let fs = [
        (
            "main.aja",
            "import shapes::{Point}; impl Point { fn f(self) {} } fn main() {}",
        ),
        ("shapes.aja", SHAPES),
    ];
    assert_eq!(
        errors(&fs),
        ["methods for 'Point' can only be defined in module 'shapes', where it is defined"]
    );
}

#[test]
fn modules_cannot_take_reserved_names() {
    let fs = [
        ("main.aja", "import m; fn main() {}"),
        (
            "m.aja",
            "pub fn print() {} pub enum Option { A } struct int { }",
        ),
    ];
    assert_eq!(
        errors(&fs),
        [
            "function 'print' conflicts with the built-in function of the same name",
            "'Option' is defined in the prelude, so it can't be redefined",
            "'int' is a built-in type, so it can't be redefined",
        ]
    );
}

#[test]
fn errors_point_into_the_right_file() {
    let fs = [
        ("main.aja", "import m;\nfn main() -> int { m::f() }"),
        ("m.aja", "pub fn f() -> int {\n    true\n}"),
    ];
    let out = rendered(&fs);
    assert!(out.contains("--> m.aja:2:5"), "{out}");
    assert!(out.contains("2 |     true"), "{out}");

    let fs = [
        ("main.aja", "import m;\nfn main() -> int { m::f(0) }"),
        ("m.aja", "pub fn f(x: int) -> int {\n    10 / x\n}"),
    ];
    let Err(errs) = run(&fs) else {
        panic!("should fail at runtime")
    };
    assert_eq!(errs[0].message, "division by zero");
    let root = files(&fs);
    let (loaded, _) = load(&root, "main.aja");
    let out = errs[0].render_in(&loaded.files, false);
    assert!(out.contains("--> m.aja:2:5"), "{out}");
}

#[test]
fn the_example_runs() {
    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/modules");
    let (_, compiled) = build(&Dir(dir), "main.aja");
    let compiled = compiled.expect("the example compiles");
    Vm::new().run_program(&compiled).expect("the example runs");
}

// ---- The standard library ---------------------------------------------------------

#[test]
fn std_io_can_be_imported() {
    let main = "
import std::io;
import std::io::{Writer, stderr};

fn main() -> Result<void, io::Error> {
    let out: Writer = io::stdout();
    out.write(\"\")?;
    out.flush()?;
    stderr().write(\"\")
}";
    assert_eq!(value(&[("main.aja", main)]), "Ok(())");
}

#[test]
fn std_modules_must_exist() {
    let fs = [("main.aja", "import std::nope; fn main() {}")];
    assert_eq!(errors(&fs), ["no module named 'std::nope'"]);
    assert!(rendered(&fs)
        .contains("the standard library's modules are: std::collections, std::fs, std::io"));
}

#[test]
fn intrinsics_are_only_for_the_standard_library() {
    for main in [
        "import std::native; fn main() { native::flush(1); }",
        "import std::native::{flush}; fn main() { flush(1); }",
    ] {
        assert_eq!(
            errors(&[("main.aja", main)]),
            ["module 'std::native' is internal to the standard library"]
        );
    }
}

#[test]
fn strings_and_templates_survive_collection() {
    // `run` collects garbage at every allocation.
    let main = "
fn main() -> str {
    var out = [];
    for i in 0..20 {
        let word = \"w{i}\".repeat(2).to_upper();
        out = out.push(\"{word}:{i * 2}:{word.len()}\");
    }
    \",\".join(out.slice(18, 20)) + \" {\"a b c\".split(\" \").len()}\"
}";
    assert_eq!(
        value(&[("main.aja", main)]),
        "\"W18W18:36:6,W19W19:38:6 3\""
    );
}

#[test]
fn std_fs_can_be_imported() {
    let main = "
import std::fs;
import std::io;

fn main() -> Result<bool, io::Error> {
    Ok(fs::is_dir(\".\") && !fs::is_file(\".\") && !fs::exists(\"no/such/path\"))
}";
    assert_eq!(value(&[("main.aja", main)]), "Ok(true)");
}

#[test]
fn std_modules_share_private_items_only_with_each_other() {
    // `std::fs` uses `std::io`'s private `checked`; a program can't.
    let main = "import std::io; fn main() { io::checked(1); }";
    assert_eq!(
        errors(&[("main.aja", main)]),
        ["'checked' is private to module 'std::io'"]
    );
    let main = "import std::io::{checked}; fn main() { checked(1); }";
    assert_eq!(
        errors(&[("main.aja", main)]),
        ["'checked' is private to module 'std::io'"]
    );
}

#[test]
fn std_types_keep_their_fields_private() {
    let main = "import std::io; fn main() { let w = io::Writer { fd: 7 }; }";
    assert_eq!(
        errors(&[("main.aja", main)]),
        ["struct 'std::io::Writer' has private fields, so it can only be built in module 'std::io'"]
    );
}

#[test]
fn an_err_from_main_is_an_error() {
    let fs = [("main.aja", "fn main() -> Result<int, str> { Err(\"no\") }")];
    assert_eq!(errors(&fs), ["main returned an error: \"no\""]);
    let fs = [("main.aja", "fn main() -> Result<int, str> { Ok(1) }")];
    assert_eq!(value(&fs), "Ok(1)");
}

#[test]
fn only_the_standard_library_declares_natives() {
    let fs = [(
        "main.aja",
        "native fn secret() -> int; native struct Blob; fn main() {}",
    )];
    assert_eq!(
        errors(&fs),
        [
            "only the standard library can declare native functions and types",
            "only the standard library can declare native functions and types",
        ]
    );
    // `native` is still an ordinary name everywhere else.
    assert_eq!(
        value(&[("main.aja", "fn main() -> int { let native = 2; native }")]),
        "2"
    );
}

#[test]
fn every_native_function_is_implemented() {
    use crate::{ast::Definition, prelude, stdlib, vm::native_implemented};
    let mut natives: Vec<String> = Vec::new();
    let mut collect = |defs: &[Definition], prefix: &str| {
        for d in defs {
            match d {
                Definition::Function {
                    native: true, name, ..
                } => natives.push(format!("{prefix}{name}")),
                Definition::Impl {
                    target: crate::ast::types::Type::Named(t, ..),
                    methods,
                    ..
                } => natives.extend(
                    methods
                        .iter()
                        .filter(|m| m.native)
                        .map(|m| format!("{prefix}{t}::{}", m.name)),
                ),
                _ => {}
            }
        }
    };
    collect(&prelude::definitions(), "");
    for (name, source) in stdlib::MODULES {
        let tokens = crate::lexer::tokenize(source).unwrap();
        let program = crate::parse::Parser::new(tokens.into_iter())
            .parse()
            .unwrap();
        collect(&program.definitions, &format!("std::{name}::"));
    }
    assert!(natives.len() > 10, "{natives:?}");
    for n in natives {
        assert!(
            native_implemented(&n),
            "native function '{n}' has no implementation in the VM"
        );
    }
}
