use anyhow::Result;
use lexer::token::Token;
use std::{fs::read_to_string, path::PathBuf};

use crate::{interp::interp_expr, lexer::Lexer};
use parse::Parser;
mod lexer;
mod ast;
mod parse;
mod interp;

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
    let res = interp_expr(ast)?;

    println!("{:?}", res);

    Ok(())
}
