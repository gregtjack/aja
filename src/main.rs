use clap::Parser;
use std::{fs::read_to_string, path::PathBuf};
use anyhow::Result;

use crate::lexer::tokenize;
mod lexer;
mod ast;
mod parse;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    /// source file
    script: Option<PathBuf>,
}

fn main() -> Result<()> {
    let cli: Cli = Cli::parse();

    let script = read_to_string(cli.script.unwrap())?;
    let tokens = tokenize(script)?;

    println!("{:#?}", tokens);

    Ok(())
}
