use lexer::Lexer;
use std::{fs::read_to_string, path::PathBuf};
use color_eyre::Result;

use crate::interp::Interpreter;
use parse::Parser;
mod ast;
mod interp;
mod lexer;
mod parse;
mod token;
mod compile;

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
    
    // println!("{:#?}", lexer.collect::<Vec<Token>>());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse()?;

    let res = Interpreter::new(ast).run()?;

    Ok(())
}
