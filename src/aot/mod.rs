//! Ahead-of-time compilation: `aja build` turns a program into a native
//! executable.
//!
//! [`codegen`] compiles the program's bytecode to an object file with
//! Cranelift, and [`link`] has the system's linker join that with the
//! runtime ([`rt`]), which is this library built as a static library. The
//! result is one file with nothing to install beside it. See
//! `plans/aot.md` for where this is going.

use std::{
    path::{Path, PathBuf},
    process::Command,
};

pub mod codegen;
pub mod layout;
pub mod meta;
pub mod rt;

use crate::{diagnostic::Files, vm::Compiled};

/// The runtime's file name, next to the `aja` executable.
const RUNTIME_LIB: &str = "libaja.a";
/// The environment variable that says where the runtime is instead.
const RUNTIME_LIB_VAR: &str = "AJA_RUNTIME_LIB";

/// Compile `program` to a native executable at `output`.
pub fn build(program: &Compiled, files: &Files, output: &Path) -> Result<(), String> {
    let object = codegen::compile(program, files)?;
    link(&object, output)
}

/// Link `object` with the runtime into an executable at `output`.
fn link(object: &[u8], output: &Path) -> Result<(), String> {
    let runtime = runtime_lib()?;
    // Next to the output, so two builds at once can't share it.
    let mut object_path = output.as_os_str().to_owned();
    object_path.push(format!(".{}.o", std::process::id()));
    let object_path = PathBuf::from(object_path);
    std::fs::write(&object_path, object)
        .map_err(|e| format!("could not write {}: {e}", object_path.display()))?;

    let mut cc = Command::new(std::env::var_os("CC").unwrap_or_else(|| "cc".into()));
    cc.arg(&object_path).arg(&runtime).arg("-o").arg(output);
    // The runtime library holds the whole compiler too; keep only what
    // the program can reach. Then what the Rust standard library needs.
    if cfg!(target_os = "macos") {
        cc.args(["-Wl,-dead_strip", "-lSystem", "-lc", "-lm"]);
    } else {
        cc.args(["-Wl,--gc-sections", "-lpthread", "-ldl", "-lm", "-lc"]);
    }
    let result = cc.output();
    let _ = std::fs::remove_file(&object_path);
    let result = result.map_err(|e| format!("could not run the linker ({cc:?}): {e}"))?;
    if !result.status.success() {
        return Err(format!(
            "the linker failed:\n{}",
            String::from_utf8_lossy(&result.stderr).trim_end()
        ));
    }
    Ok(())
}

/// Where the runtime library is: where `AJA_RUNTIME_LIB` says, or next to
/// the `aja` executable.
fn runtime_lib() -> Result<PathBuf, String> {
    if let Some(path) = std::env::var_os(RUNTIME_LIB_VAR) {
        return Ok(PathBuf::from(path));
    }
    let exe = std::env::current_exe().map_err(|e| format!("could not find `aja` itself: {e}"))?;
    let path = exe.with_file_name(RUNTIME_LIB);
    if path.exists() {
        Ok(path)
    } else {
        Err(format!(
            "could not find the runtime library, {}: it belongs next to `aja` \
             (or set {RUNTIME_LIB_VAR} to where it is)",
            path.display()
        ))
    }
}
