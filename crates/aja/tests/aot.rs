//! Programs compiled ahead of time, with `aja build`, do what they do when
//! `aja` runs them on the VM.

use std::{
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Output, Stdio},
};

/// A directory for a test's files, removed when the test ends.
struct Dir(PathBuf);

impl Dir {
    fn new(name: &str) -> Dir {
        let dir = std::env::temp_dir().join(format!("aja-aot-{name}-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        Dir(dir)
    }

    /// Write a program to `main.aja` in the directory.
    fn program(&self, source: &str) -> PathBuf {
        let file = self.0.join("main.aja");
        std::fs::write(&file, source).unwrap();
        file
    }
}

impl Drop for Dir {
    fn drop(&mut self) {
        std::fs::remove_dir_all(&self.0).ok();
    }
}

fn aja() -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_aja"));
    command.env("NO_COLOR", "1");
    command
}

/// Compile `file` to an executable in `dir`.
fn build(file: &Path, dir: &Dir) -> PathBuf {
    let exe = dir.0.join(file.file_stem().unwrap());
    let out = aja()
        .arg("build")
        .arg(file)
        .arg("-o")
        .arg(&exe)
        .output()
        .expect("aja runs");
    assert!(
        out.status.success(),
        "building {} failed: {}",
        file.display(),
        String::from_utf8_lossy(&out.stderr)
    );
    exe
}

/// Run `command` with `input` as its standard input.
fn run(command: &mut Command, input: &str) -> Output {
    let mut child = command
        .env("NO_COLOR", "1")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("the program starts");
    // A program that doesn't read its input may have exited already.
    let _ = child.stdin.take().unwrap().write_all(input.as_bytes());
    child.wait_with_output().unwrap()
}

/// What a run printed and how it ended.
fn outcome(out: &Output) -> (String, String, Option<i32>) {
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code(),
    )
}

/// Check that `file`, given `args` and `input`, does the same compiled as
/// on the VM: what it prints, the error that stops it, and its status.
fn same_as_vm(file: &Path, dir: &Dir, args: &[&str], input: &str) {
    let exe = build(file, dir);
    let vm = run(aja().arg(file).args(args), input);
    let native = run(Command::new(&exe).args(args), input);
    assert_eq!(
        outcome(&native),
        outcome(&vm),
        "{} compiled (left) and on the VM (right)",
        file.display()
    );
}

/// The examples that print nothing that depends on how long they took or
/// which port they were given.
#[test]
fn examples_run_the_same_compiled() {
    let dir = Dir::new("examples");
    for name in [
        "closures",
        "collections",
        "data",
        "defer",
        "fizzbuzz",
        "generics",
        "hello",
        "inference",
        "iterators",
        "lists",
        "math",
        "methods",
        "recursion",
        "results",
        "strings",
        "tuples",
        "modules/main",
        "channels",
        "tasks",
        "shutdown",
        "http",
        "errors/cancelled_join",
        "errors/deadlock",
        "errors/scope_ended",
        "errors/task_failure",
        "errors/assert",
        "errors/call_trace",
        "errors/overflow",
    ] {
        same_as_vm(
            &Path::new("../../examples").join(format!("{name}.aja")),
            &dir,
            &[],
            "",
        );
    }
}

#[test]
fn a_compiled_program_gets_its_arguments_and_input() {
    let dir = Dir::new("io");
    let file = dir.program(
        "import std::env;
import std::io;
fn main() -> Result<void, io::Error> {
    println(env::args());
    for line in io::stdin().lines() {
        println(\"> {line?}\");
    }
    Ok(void)
}
",
    );
    same_as_vm(&file, &dir, &["8080", "-v", "--port=1"], "one\ntwo\n");
    let out = run(&mut Command::new(build(&file, &dir)), "one\n");
    assert_eq!(
        outcome(&out),
        ("[]\n> one\n".into(), String::new(), Some(0))
    );
}

#[test]
fn compiled_tail_calls_run_in_constant_space() {
    let dir = Dir::new("tail");
    let file = dir.program(
        "fn count(n: int, acc: int) -> int {
    if n == 0 { return acc; }
    count(n - 1, acc + 1)
}
fn even(n: int) -> bool { if n == 0 { true } else { odd(n - 1) } }
fn odd(n: int) -> bool { if n == 0 { false } else { even(n - 1) } }
fn main() {
    // Far deeper than the call stack could go.
    println(count(30_000_000, 0), even(30_000_001));
    // Through a function value, too.
    let loop_ = fn(f: (int, int) -> int) -> int { f(30_000_000, 1) };
    println(loop_(count));
}
",
    );
    let out = run(&mut Command::new(build(&file, &dir)), "");
    assert_eq!(
        outcome(&out),
        ("30000000 false\n30000001\n".into(), String::new(), Some(0))
    );
}

#[test]
fn a_compiled_program_that_recurses_too_deep_stops_with_an_error() {
    let dir = Dir::new("deep");
    let file = dir.program(
        "fn down(n: int) -> int {
    if n == 0 { return 0; }
    1 + down(n - 1)
}
fn main() {
    println(down(100_000));
    println(down(1_000_000_000));
}
",
    );
    let (stdout, stderr, status) = outcome(&run(&mut Command::new(build(&file, &dir)), ""));
    assert_eq!(stdout, "100000\n");
    assert_eq!(status, Some(1), "{stderr}");
    assert!(
        stderr.starts_with("error: stack overflow: the call stack has grown past 256 MB\n"),
        "{stderr}"
    );
    assert!(stderr.contains("1 + down(n - 1)"), "{stderr}");
    assert!(
        stderr.contains("in function 'down', called at line 7, column 13"),
        "{stderr}"
    );
}

#[test]
fn compiled_errors_and_garbage_collection_match_the_vm() {
    let dir = Dir::new("errors");
    // Enough garbage for the collector to run many times, with live
    // values in registers, closures, cells, and lists across it; then a
    // `main` that returns an error.
    let file = dir.program(
        "struct Point { x: int, y: float }
enum Shape { Dot, Line(Point, Point) }
fn churn(n: int) -> List<str> {
    var kept: List<str> = [];
    var total = 0;
    let add = fn(k: int) { total = total + k; };
    for i in 0..n {
        let p = Point { x: i, y: 0.5 };
        let s = Shape::Line(p, Point { x: i + 1, y: 1.5 });
        let text = \"point {p.x} of {n}\";
        match s {
            Shape::Line(a, b) => add(b.x - a.x),
            Shape::Dot => add(0),
        }
        if i % 5000 == 0 { kept = kept.push(text); }
    }
    kept.push(\"total {total}\")
}
fn check(n: int) -> Result<int, str> {
    if n > 2 { Err(\"too many: {n}\") } else { Ok(n) }
}
fn main() -> Result<void, str> {
    println(churn(20_000));
    println(check(1)?, 7 % -1, -7 / 2, 7.5 % 2.0, 1 < 2, \"a\" < \"b\", [1, 2] == [1, 2]);
    check(3)?;
    Ok(void)
}
",
    );
    same_as_vm(&file, &dir, &[], "");

    for (code, message) in [
        (
            "let xs = [1, 2, 3]; println(xs[3]);",
            "error: index 3 is out of range",
        ),
        ("println(1 / (1 - 1));", "error: division by zero"),
        (
            "println(9223372036854775807 * 2);",
            "error: integer overflow in '*'",
        ),
    ] {
        let file = dir.program(&format!("fn main() {{ {code} }}\n"));
        same_as_vm(&file, &dir, &[], "");
        let (_, stderr, _) = outcome(&run(&mut Command::new(build(&file, &dir)), ""));
        assert!(stderr.starts_with(message), "{stderr}");
    }
}

/// Tasks that are cancelled while they compute, sleep, wait in a scope of
/// their own, and run deferred code, then tasks that share a channel.
const TASKS: &str = r#"import std::task;
import std::channel;
import std::time;

fn worker(name: str, n: int) -> int {
    defer println("{name}: outer cleanup");
    var total = 0;
    for i in 0..n {
        defer { total = total + 1; }
        if i % 1000 == 0 { sleep(1); }
    }
    println("{name}: finished");
    total
}

fn slow_cleanup(name: str) {
    defer {
        // Deferred code runs to its end even when cancelled, waits included.
        sleep(5);
        var k = 0;
        while k < 5000 { k = k + 1; }
        println("{name}: slow cleanup done {k}");
    }
    sleep(10_000);
    println("{name}: never");
}

fn fib(n: int) -> int { if n < 2 { n } else { fib(n - 1) + fib(n - 2) } }

fn main() {
    let got = task::scope(fn(s) {
        let a = s.spawn(fn() { worker("a", 3000) });
        let b = s.spawn(fn() { worker("b", 100_000_000) });
        let c = s.spawn(fn() { slow_cleanup("c") });
        let d = s.spawn(fn() { fib(20) });
        let e = s.spawn(fn() {
            task::scope(fn(inner) {
                let x = inner.spawn(fn() { slow_cleanup("e.x") });
                defer println("e: leaving inner scope");
                sleep(10_000);
                0
            })
        });
        let first = a.join();
        println("a gave", first, "d gave", d.join());
        s.cancel();
        println(b.try_join(), c.try_join(), e.try_join());
        first
    });
    println("scope gave", got);

    let (tx, rx) = channel::new(2);
    let sum = task::scope(fn(s) {
        for w in 0..4 {
            s.spawn(fn() {
                for i in 0..2000 { tx.send(w * 10_000 + i); }
            });
        }
        let reader = s.spawn(fn() {
            var total = 0;
            var strings: List<str> = [];
            for n in 0..8000 {
                let v = rx.recv();
                match v {
                    Some(x) => { total = total + x; }
                    None => {}
                }
                if n % 500 == 0 { strings = strings.push("at {n}: {total}"); }
            }
            println(strings.len());
            total
        });
        reader.join()
    });
    println("sum", sum);
}
"#;

#[test]
fn compiled_tasks_are_cancelled_and_cleaned_up_as_on_the_vm() {
    let dir = Dir::new("tasks");
    let file = dir.program(TASKS);
    same_as_vm(&file, &dir, &[], "");

    // And the same when every turn ends at its first safepoint and every
    // allocation collects.
    let exe = build(&file, &dir);
    let usual = run(&mut Command::new(&exe), "");
    let stressed = run(Command::new(&exe).env("AJA_STRESS", "1"), "");
    assert_eq!(outcome(&stressed), outcome(&usual));
    assert_eq!(usual.status.code(), Some(0));
}

#[test]
fn a_compiled_program_can_run_many_tasks() {
    let dir = Dir::new("many");
    let file = dir.program(
        "import std::task;
import std::channel;
fn main() {
    let (tx, rx) = channel::new(0);
    let total = task::scope(fn(s) {
        var tasks: List<task::Task<int>> = [];
        for i in 0..5000 {
            tasks = tasks.push(s.spawn(fn() { sleep(5); i }));
        }
        s.spawn(fn() { for i in 0..20000 { tx.send(i); } tx.close(); });
        var sum = 0;
        for v in rx { sum = sum + v; }
        for t in tasks { sum = sum + t.join(); }
        sum
    });
    println(total);
}
",
    );
    same_as_vm(&file, &dir, &[], "");
}

#[test]
fn compiled_examples_run_the_same_under_stress() {
    let dir = Dir::new("stress");
    // Ones whose output doesn't depend on which task runs when.
    for name in ["channels", "shutdown", "defer", "closures", "collections"] {
        let exe = build(
            &Path::new("../../examples").join(format!("{name}.aja")),
            &dir,
        );
        let usual = run(&mut Command::new(&exe), "");
        let stressed = run(Command::new(&exe).env("AJA_STRESS", "1"), "");
        assert_eq!(outcome(&stressed), outcome(&usual), "{name}");
    }
}

#[test]
fn compiled_assertions_fail_as_in_the_vm() {
    let dir = Dir::new("testing");
    let file = dir.program(
        "import std::testing::{assert_eq, assert_ne};
fn main() {
    assert_eq([1, 2], [1, 2]);
    assert_ne(\"a\", \"b\");
    println(\"so far so good\");
    assert_eq(Some(1), None);
}
test \"is left out\" { assert(false); }",
    );
    same_as_vm(&file, &dir, &[], "");
}
