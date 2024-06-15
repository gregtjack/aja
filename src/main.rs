use anyhow::Result;
use lexer::Lexer;
use std::{fs::read_to_string, path::PathBuf};

use crate::interp::interp;
use parse::Parser;
mod ast;
mod interp;
mod lexer;
mod parse;
mod token;

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct AjaCli {
    /// source file
    filename: Option<PathBuf>,
}

fn main() -> Result<()> {
    let cli = <AjaCli as clap::Parser>::parse();

    let script = read_to_string(cli.filename.unwrap())?;
    let lexer = Lexer::new(script.chars());

    // println!("{:#?}", lexer.collect::<Vec<Token>>());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse()?;
    println!("{:#?}", ast);
    let res = interp(ast)?;
    println!("{:?}", res);

    Ok(())
}
