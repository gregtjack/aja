use color_eyre::{eyre::bail, Result};
use lexer::Lexer;
use std::{fs::read_to_string, path::PathBuf};
use tracing::debug;
use tracing_subscriber::{
    fmt::layer,
    layer::{Filter, SubscriberExt},
};

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
    let subscriber = tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .compact()
        .finish();
    tracing::subscriber::set_global_default(subscriber)?;

    let cli = <AjaCli as clap::Parser>::parse();

    let script = read_to_string(cli.file.expect("should have been given a source file"))?;

    let lexer = Lexer::new(script.chars());
    let mut parser = Parser::new(lexer);
    let ast = match parser.parse() {
        Ok(p) => p,
        Err(e) => bail!(e),
    };

    debug!("{:?}", ast);

    let res = Interpreter::new(ast).run()?;
    debug!("{res:?}");
    Ok(())
}
