//! Running programs that use `std::io` as a user would, with their
//! standard input, output, and error connected to pipes.

use std::{
    io::Write,
    process::{Command, Output, Stdio},
};

fn example(path: &str) -> String {
    format!("{}/../../examples/{path}", env!("CARGO_MANIFEST_DIR"))
}

/// Run the `aja` binary on `file` with `input` as its standard input.
fn run(file: &str, input: &[u8]) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_aja"))
        .arg(file)
        .env("NO_COLOR", "1")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("aja runs");
    child.stdin.take().unwrap().write_all(input).unwrap();
    child.wait_with_output().unwrap()
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

#[test]
fn reads_lines_and_writes_both_streams() {
    let out = run(&example("io/greet.aja"), b"Ada\nfirst\r\nsecond");
    assert!(out.status.success(), "{}", text(&out.stderr));
    assert_eq!(
        text(&out.stdout),
        "What's your name? Hello, Ada! Now some lines, please.\n1 first\n2 second\nread 2 lines\n"
    );
    assert_eq!(text(&out.stderr), "(that went to standard error)\n");
}

#[test]
fn the_end_of_input_is_none() {
    let out = run(&example("io/greet.aja"), b"");
    assert!(out.status.success(), "{}", text(&out.stderr));
    assert_eq!(
        text(&out.stdout),
        "What's your name? \nNo name, no greeting.\n"
    );
}

#[test]
fn an_error_returned_from_main_fails_the_program() {
    let out = run(&example("io/greet.aja"), b"\xff\xfe\n");
    assert!(!out.status.success());
    let err = text(&out.stderr);
    assert!(
        err.contains(
            "error: main returned an error: std::io::Error { kind: std::io::ErrorKind::InvalidData"
        ),
        "{err}"
    );
}

#[test]
fn reads_everything_at_once() {
    let dir = std::env::temp_dir().join(format!("aja-io-test-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join("all.aja");
    std::fs::write(
        &file,
        "import std::io;\n\
         fn main() -> Result<void, io::Error> {\n\
             let all = io::stdin().read_to_string()?;\n\
             io::stdout().write(\"[\" + all + \"]\")?;\n\
             io::stdout().flush()\n\
         }\n",
    )
    .unwrap();
    let out = run(file.to_str().unwrap(), b"a\nb\n");
    std::fs::remove_dir_all(&dir).ok();
    assert!(out.status.success(), "{}", text(&out.stderr));
    assert_eq!(text(&out.stdout), "[a\nb\n]");
}

#[test]
fn a_closed_pipe_stops_the_program_quietly() {
    let dir = std::env::temp_dir().join(format!("aja-io-pipe-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join("forever.aja");
    std::fs::write(&file, "fn main() { while true { println(\"y\"); } }\n").unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_aja"))
        .arg(&file)
        .env("NO_COLOR", "1")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("aja runs");
    // Read a little, then hang up, as `head` does.
    let mut stdout = child.stdout.take().unwrap();
    let mut first = [0; 4];
    std::io::Read::read_exact(&mut stdout, &mut first).unwrap();
    assert_eq!(&first, b"y\ny\n");
    drop(stdout);
    let out = child.wait_with_output().unwrap();
    std::fs::remove_dir_all(&dir).ok();
    assert_eq!(text(&out.stderr), "");
    assert_eq!(out.status.code(), Some(141));
}
