//! The `aja` tool's commands: `run`, `check`, `test`, and `repl`.
//! (`build` is in `aot.rs`.)

use std::{
    io::Write,
    path::PathBuf,
    process::{Command, Output, Stdio},
};

/// A directory for a test's files, removed when the test ends.
struct Dir(PathBuf);

impl Dir {
    fn new(name: &str) -> Dir {
        let dir = std::env::temp_dir().join(format!("aja-cli-{name}-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        Dir(dir)
    }

    /// Write a program to `main.aja` in the directory.
    fn program(&self, source: &str) -> PathBuf {
        let file = self.0.join("main.aja");
        std::fs::write(&file, source).unwrap();
        file
    }

    /// The names of the files in the directory.
    fn files(&self) -> Vec<String> {
        let mut names: Vec<String> = std::fs::read_dir(&self.0)
            .unwrap()
            .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
            .collect();
        names.sort();
        names
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

/// Run `command` with `input` as its standard input, and give what it
/// printed and how it ended.
fn run(command: &mut Command, input: &str) -> (String, String, Option<i32>) {
    let mut child = command
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("aja starts");
    let _ = child.stdin.take().unwrap().write_all(input.as_bytes());
    let Output {
        stdout,
        stderr,
        status,
    } = child.wait_with_output().unwrap();
    (
        String::from_utf8_lossy(&stdout).into_owned(),
        String::from_utf8_lossy(&stderr).into_owned(),
        status.code(),
    )
}

#[test]
fn run_compiles_and_runs_a_program_and_leaves_nothing() {
    let dir = Dir::new("run");
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
    // Its temporary files go in a directory of their own, to see what is
    // left of them. Flags after the file are the program's, not `aja`'s.
    let tmp = Dir::new("run-tmp");
    let out = run(
        aja().current_dir(&dir.0).env("TMPDIR", &tmp.0).args([
            "run",
            "main.aja",
            "8080",
            "-v",
            "--output=x",
        ]),
        "one\ntwo\n",
    );
    assert_eq!(
        out,
        (
            "[\"8080\", \"-v\", \"--output=x\"]\n> one\n> two\n".into(),
            String::new(),
            Some(0)
        )
    );
    assert_eq!(dir.files(), ["main.aja"]);
    // Only the runtime library, which is kept for the next build.
    let left = tmp.files();
    assert!(
        left.iter()
            .all(|f| f.starts_with("aja-runtime-") && f.ends_with(".a")),
        "{left:?}"
    );
}

#[test]
fn run_ends_as_the_program_does() {
    let dir = Dir::new("run-fails");
    let file = dir.program("fn main() { let xs = [1]; println(\"before\"); println(xs[2]); }\n");
    let (stdout, stderr, status) = run(aja().arg("run").arg(&file), "");
    assert_eq!(stdout, "before\n");
    assert!(
        stderr.starts_with("error: index 2 is out of range for a list of length 1\n"),
        "{stderr}"
    );
    assert_eq!(status, Some(1));

    // A program that doesn't compile isn't run.
    let file = dir.program("fn main() { println(nope); }\n");
    let (stdout, stderr, status) = run(aja().arg("run").arg(&file), "");
    assert_eq!(stdout, "");
    assert!(
        stderr.starts_with("error: cannot find variable 'nope' in this scope\n"),
        "{stderr}"
    );
    assert_eq!(status, Some(1));
}

#[test]
fn check_reports_type_errors_and_runs_nothing() {
    let dir = Dir::new("check");
    // It would print, and then fail, if it ran.
    let file = dir.program("fn main() { println(\"ran\"); println(1 / (1 - 1)); }\n");
    assert_eq!(
        run(aja().arg("check").arg(&file), ""),
        (String::new(), String::new(), Some(0))
    );
    assert_eq!(dir.files(), ["main.aja"]);

    std::fs::write(
        dir.0.join("shapes.aja"),
        "pub fn area(w: int) -> int { w * \"2\" }\n",
    )
    .unwrap();
    let file = dir.program(
        "import shapes;
fn main() {
    let n: str = shapes::area(2);
    println(n);
}
",
    );
    let (stdout, stderr, status) = run(aja().arg("check").arg(&file), "");
    assert_eq!(stdout, "");
    assert_eq!(status, Some(1));
    // Every module's errors, each in its file.
    assert!(stderr.contains("shapes.aja:1:"), "{stderr}");
    assert!(stderr.contains("main.aja:3:"), "{stderr}");
    assert!(stderr.ends_with("2 errors found\n"), "{stderr}");
}

#[test]
fn repl_evaluates_what_it_is_given() {
    let (stdout, stderr, status) = run(aja().arg("repl"), "let x = 20\nx * 2 + 2\nexit\n");
    assert!(stdout.contains("> 42\n"), "{stdout}");
    assert!(stdout.ends_with("Goodbye!\n"), "{stdout}");
    assert_eq!((stderr.as_str(), status), ("", Some(0)));
}

#[test]
fn test_runs_each_test_and_reports_failures() {
    let dir = Dir::new("test");
    std::fs::write(
        dir.0.join("half.aja"),
        "pub fn half(n: int) -> int { n / 2 }
test \"halves\" { assert(half(4) == 2); }",
    )
    .unwrap();
    let file = dir.program(
        "import half::{half};
import std::testing::{assert_eq};
test \"prints\" { println(\"hi\"); }
test \"is wrong\" { assert_eq(half(5), 3); }
test \"divides by zero\" { println(10 / (half(1))); }",
    );
    let (out, err, code) = run(aja().arg("test").arg(&file), "");
    // Each test's line comes after what it printed. (The summary says how
    // long the tests took, so it's checked apart.)
    let lines: Vec<&str> = out.lines().filter(|l| !l.contains(" passed, ")).collect();
    assert_eq!(
        lines,
        [
            "hi",
            "pass \"prints\"",
            "FAIL \"is wrong\"",
            "FAIL \"divides by zero\"",
            "pass half: \"halves\"",
            "",
            "failed:",
            "    \"is wrong\"",
            "    \"divides by zero\"",
        ]
    );
    assert!(out.contains("\n2 passed, 2 failed ("), "{out}");
    assert!(
        err.contains(
            "error: assertion failed: the values are not equal
  expected: 3
     found: 2
 --> "
        ),
        "{err}"
    );
    assert!(
        err.contains("4 | test \"is wrong\" { assert_eq(half(5), 3); }"),
        "{err}"
    );
    assert!(err.contains("= note: in test \"is wrong\""), "{err}");
    assert!(err.contains("error: division by zero"), "{err}");
    assert_eq!(code, Some(1));

    // A filter runs only the tests whose names contain it.
    let (out, err, code) = run(aja().arg("test").arg(&file).arg("halve"), "");
    assert!(
        out.starts_with("pass half: \"halves\"\n\n1 passed, 0 failed, 3 filtered out ("),
        "{out}"
    );
    assert_eq!((err.as_str(), code), ("", Some(0)));
}

#[test]
fn test_fails_when_there_are_no_tests_to_run() {
    let dir = Dir::new("test-none");
    let file = dir.program("fn main() {}");
    let (_, err, code) = run(aja().arg("test").arg(&file), "");
    assert!(err.starts_with("error: no tests found in "), "{err}");
    assert_eq!(code, Some(1));
    let file = dir.program("test \"a\" {}");
    let (_, err, code) = run(aja().arg("test").arg(&file).arg("b"), "");
    assert!(err.contains("have \"b\" in their names"), "{err}");
    assert_eq!(code, Some(1));
}

#[test]
fn test_reports_errors_in_tests_before_running_any() {
    let dir = Dir::new("test-errors");
    let file = dir.program("test \"a\" { assert(true); }\ntest \"b\" { 1 + \"x\"; }");
    let (out, err, code) = run(aja().arg("test").arg(&file), "");
    assert_eq!(out, "");
    assert!(err.starts_with("error: "), "{err}");
    assert_eq!(code, Some(1));
    // `run` and `check` find them too.
    let (_, _, code) = run(aja().arg("check").arg(&file), "");
    assert_eq!(code, Some(1));
}

#[test]
fn the_testing_example_passes() {
    let (out, err, code) = run(aja().arg("test").arg("../../examples/testing.aja"), "");
    assert_eq!((err.as_str(), code), ("", Some(0)), "{out}");
    assert!(out.contains("\n4 passed, 0 failed ("), "{out}");
}
