//! A program's arguments and environment variables, from `std::env`.

use std::process::Command;

#[test]
fn a_program_gets_its_arguments_and_environment() {
    let dir = std::env::temp_dir().join(format!("aja-env-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join("main.aja");
    std::fs::write(
        &file,
        "import std::env;
fn main() {
    println(env::args());
    println(env::get(\"AJA_TEST_GREETING\"), env::get(\"AJA_TEST_UNSET\"), env::get(\"\"));
}
",
    )
    .unwrap();
    // Flags after the file are the program's, not `aja`'s.
    let out = Command::new(env!("CARGO_BIN_EXE_aja"))
        .arg(&file)
        .args(["8080", "-v", "--port=1"])
        .env("AJA_TEST_GREETING", "hi")
        .env_remove("AJA_TEST_UNSET")
        .env("NO_COLOR", "1")
        .output()
        .expect("aja runs");
    std::fs::remove_dir_all(&dir).ok();
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "[\"8080\", \"-v\", \"--port=1\"]\nSome(\"hi\") None None\n"
    );
}
