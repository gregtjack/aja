use color_eyre::{eyre::bail, Result};
use lexer::Lexer;
use std::{fs::read_to_string, path::PathBuf};

use crate::interp::Interpreter;
use parse::Parser;
mod ast;
mod compile;
mod interp;
mod lexer;
mod parse;
mod token;

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct AjaCli {
    /// source file
    file: Option<PathBuf>,
}

fn main() -> Result<()> {
    let cli = <AjaCli as clap::Parser>::parse();

    let script = read_to_string(cli.file.expect("should have been given a source file"))?;
    let lexer = Lexer::new(script.chars());
    let mut parser = Parser::new(lexer);
    let ast = match parser.parse() {
        Ok(p) => p,
        Err(_) => bail!("parse input: see report above"),
    };

    println!("AST = {:#?}", ast);
    let res = Interpreter::new(ast).run()?;
    println!("{res}");
    Ok(())
}
