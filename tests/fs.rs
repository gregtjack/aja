//! Running programs that use `std::fs` as a user would, each in a fresh
//! working directory of its own.

use std::{
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Output, Stdio},
};

/// A new, empty directory for one test, removed when it is dropped.
struct TempDir(PathBuf);

impl TempDir {
    fn new(name: &str) -> Self {
        let dir = std::env::temp_dir().join(format!("aja-fs-{name}-{}", std::process::id()));
        std::fs::remove_dir_all(&dir).ok();
        std::fs::create_dir_all(&dir).unwrap();
        TempDir(dir)
    }

    fn path(&self) -> &Path {
        &self.0
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        std::fs::remove_dir_all(&self.0).ok();
    }
}

/// Run the `aja` binary on `file` in `dir`, with `input` as its standard
/// input.
fn run_in(dir: &Path, file: &Path, input: &[u8]) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_aja"))
        .arg(file)
        .current_dir(dir)
        .env("NO_COLOR", "1")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("aja runs");
    child.stdin.take().unwrap().write_all(input).unwrap();
    child.wait_with_output().unwrap()
}

/// Write `program` to `main.aja` in `dir` and run it there.
fn run_program(dir: &Path, program: &str) -> Output {
    let file = dir.join("main.aja");
    std::fs::write(&file, program).unwrap();
    run_in(dir, &file, b"")
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

#[test]
fn the_notes_example_keeps_notes() {
    let dir = TempDir::new("notes");
    let example = Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/fs/notes.aja");

    let out = run_in(dir.path(), &example, b"");
    assert!(out.status.success(), "{}", text(&out.stderr));
    assert_eq!(text(&out.stdout), "New note: No notes yet.\n");

    run_in(dir.path(), &example, b"buy milk\n");
    let out = run_in(dir.path(), &example, b"call home\n");
    assert!(out.status.success(), "{}", text(&out.stderr));
    assert_eq!(
        text(&out.stdout),
        "New note: Notes (notes.txt, 19 bytes):\n- buy milk\n- call home\n"
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("notes.txt")).unwrap(),
        "buy milk\ncall home\n"
    );
}

#[test]
fn files_and_directories_can_be_made_read_and_removed() {
    let dir = TempDir::new("ops");
    let out = run_program(
        dir.path(),
        r#"
import std::fs;
import std::io;

fn main() -> Result<void, io::Error> {
    fs::create_dir_all("a/b/c")?;
    fs::create_dir_all("a/b")?;
    fs::write("a/one.txt", "first\r\nsecond\n")?;
    fs::append("a/one.txt", "third")?;
    fs::copy("a/one.txt", "a/two.txt")?;
    fs::rename("a/two.txt", "a/b/three.txt")?;
    println(fs::read_dir("a")?, fs::read_dir("a/b")?);
    println(fs::read_lines("a/b/three.txt")?, fs::size("a/one.txt")?);
    println(fs::is_dir("a/b"), fs::is_file("a/b"), fs::is_file("a/one.txt"));
    fs::remove_file("a/one.txt")?;
    fs::remove_dir("a/b/c")?;
    println(fs::exists("a/one.txt"), fs::exists("a/b/c"));
    fs::remove_dir_all("a")?;
    println(fs::exists("a"));
    io::stdout().flush()
}
"#,
    );
    assert!(out.status.success(), "{}", text(&out.stderr));
    assert_eq!(
        text(&out.stdout),
        "[\"b\", \"one.txt\"] [\"c\", \"three.txt\"]\n\
         [\"first\", \"second\", \"third\"] 19\n\
         true false true\n\
         false false\n\
         false\n"
    );
}

#[test]
fn failures_say_what_kind_they_are_and_name_the_path() {
    let dir = TempDir::new("errors");
    std::fs::create_dir_all(dir.path().join("full/inside")).unwrap();
    std::fs::write(dir.path().join("file.txt"), "").unwrap();
    let out = run_program(
        dir.path(),
        r#"
import std::fs;
import std::io;
import std::io::{ErrorKind};

fn show<T>(r: Result<T, io::Error>) {
    match r {
        Ok(_) => println("ok"),
        Err(e) => {
            let kind = match e.kind {
                ErrorKind::NotFound => "NotFound",
                ErrorKind::AlreadyExists => "AlreadyExists",
                ErrorKind::NotADirectory => "NotADirectory",
                ErrorKind::IsADirectory => "IsADirectory",
                ErrorKind::DirectoryNotEmpty => "DirectoryNotEmpty",
                _ => "something else",
            };
            println(kind + ": " + e.message);
        }
    }
}

fn main() {
    show(fs::read_to_string("missing.txt"));
    show(fs::create_dir("full"));
    show(fs::read_dir("file.txt"));
    show(fs::read_lines("full"));
    show(fs::remove_dir("full"));
    show(fs::size("missing.txt"));
}
"#,
    );
    assert!(out.status.success(), "{}", text(&out.stderr));
    assert_eq!(
        text(&out.stdout),
        "NotFound: could not read 'missing.txt': no such file or directory\n\
         AlreadyExists: could not create the directory 'full': file exists\n\
         NotADirectory: could not read the directory 'file.txt': not a directory\n\
         IsADirectory: could not read 'full': is a directory\n\
         DirectoryNotEmpty: could not remove the directory 'full': directory not empty\n\
         NotFound: could not read the size of 'missing.txt': no such file or directory\n"
    );
}
