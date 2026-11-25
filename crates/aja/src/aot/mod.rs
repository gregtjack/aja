//! Ahead-of-time compilation: `aja build` turns a program into a native
//! executable.
//!
//! [`codegen`] compiles the program's bytecode to an object file with
//! Cranelift, and [`link`] has the system's linker join that with the
//! runtime ([`rt`], in the `aja-runtime` crate), which is built into `aja`
//! as a static library. The result is one file with nothing to install
//! beside it. See
//! `plans/aot.md` for where this is going.

use std::{
    path::{Path, PathBuf},
    process::Command,
};

pub mod codegen;
pub use aja_runtime::{layout, meta, rt};

use crate::{diagnostic::Files, vm::Compiled};

/// The runtime, as a static library: the `aja-runtime` crate, built by
/// `build.rs` along with this compiler, so the two always agree.
static RUNTIME: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/libaja_runtime.a"));
/// What tells this build of the runtime from any other.
const RUNTIME_ID: &str = env!("AJA_RUNTIME_ID");
/// The environment variable that names a runtime library to link with
/// instead.
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
    // Keep only what the program can reach of the runtime, then what
    // the Rust standard library needs.
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

/// A file holding the runtime library, for the linker: the one
/// `AJA_RUNTIME_LIB` names, or a copy of the one built into `aja`, written
/// to the system's temporary directory the first time it is needed and
/// whenever it has gone.
fn runtime_lib() -> Result<PathBuf, String> {
    if let Some(path) = std::env::var_os(RUNTIME_LIB_VAR) {
        return Ok(PathBuf::from(path));
    }
    let path = std::env::temp_dir().join(format!("aja-runtime-{RUNTIME_ID}.a"));
    let complete = std::fs::metadata(&path).is_ok_and(|m| m.len() == RUNTIME.len() as u64);
    if !complete {
        // Written under another name and then renamed, so the linker of
        // another `aja build` never sees half of it.
        let partial = path.with_extension(format!("{}.partial", std::process::id()));
        std::fs::write(&partial, RUNTIME)
            .and_then(|()| std::fs::rename(&partial, &path))
            .map_err(|e| {
                let _ = std::fs::remove_file(&partial);
                format!(
                    "could not write the runtime library to {}: {e}",
                    path.display()
                )
            })?;
    }
    Ok(path)
}
