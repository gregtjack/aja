//! Builds the runtime (the `aja-runtime` crate) as a static library and
//! puts it where `src/aot/mod.rs` (in this crate) embeds it in `aja`, so that `aja build`
//! always links a program with the runtime its compiler was built with.
//!
//! `aja` also depends on the runtime as an ordinary crate, for the VM, but
//! Cargo only makes a static library of a package it was asked to build,
//! and one left over from an earlier build would not match the compiler.
//! So this runs Cargo again, with a target directory of its own.

use std::{env, fs, path::PathBuf, process::Command};

fn main() {
    for path in [
        "../runtime/src",
        "../runtime/Cargo.toml",
        "../diagnostic/src",
        "../diagnostic/Cargo.toml",
        "../../Cargo.lock",
    ] {
        println!("cargo:rerun-if-changed={path}");
    }
    let out = PathBuf::from(env::var_os("OUT_DIR").expect("Cargo sets OUT_DIR"));
    let profile = env::var("PROFILE").expect("Cargo sets PROFILE");
    let (target, host) = (env::var("TARGET").unwrap(), env::var("HOST").unwrap());
    let target_dir = out.join("runtime");

    let mut cargo = Command::new(env::var_os("CARGO").unwrap_or_else(|| "cargo".into()));
    cargo
        .args(["rustc", "--manifest-path", "../runtime/Cargo.toml", "--lib"])
        .args(["--crate-type", "staticlib", "--target-dir"])
        .arg(&target_dir);
    if profile == "release" {
        cargo.arg("--release");
    }
    let mut built = target_dir.clone();
    if target != host {
        cargo.args(["--target", &target]);
        built.push(&target);
    }
    built.push(&profile);
    built.push("libaja_runtime.a");

    let status = cargo.status().expect("Cargo runs");
    assert!(status.success(), "building the runtime library failed");

    let library = fs::read(&built).expect("Cargo wrote the runtime library");
    fs::write(out.join("libaja_runtime.a"), &library).expect("OUT_DIR can be written to");
    // What tells one build of the runtime from another, to name the copy
    // that `aja build` keeps for the linker.
    let id = library.iter().fold(0xcbf2_9ce4_8422_2325_u64, |hash, &b| {
        (hash ^ b as u64).wrapping_mul(0x0000_0100_0000_01b3)
    });
    println!("cargo:rustc-env=AJA_RUNTIME_ID={id:016x}");
}
