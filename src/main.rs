use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
    process::ExitCode,
};

use crate::{diagnostic::Diagnostic, interp::Interpreter};
use parse::Parser;
mod ast;
mod compile;
mod diagnostic;
mod interp;
mod lexer;
mod parse;
mod token;

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Source file
    file: Option<PathBuf>,
}

fn main() -> ExitCode {
    let cli = <Cli as clap::Parser>::parse();

    let Some(file) = cli.file else {
        // REPL mode
        run_repl();
        return ExitCode::SUCCESS;
    };

    // File mode
    let path = file.display().to_string();
    let script = match fs::read_to_string(&file) {
        Ok(s) => s,
        Err(e) => {
            report(
                &Diagnostic::new(format!("could not read '{path}': {e}")),
                "",
                &path,
            );
            return ExitCode::FAILURE;
        }
    };

    let tokens = match lexer::tokenize(&script) {
        Ok(tokens) => tokens,
        Err(e) => {
            report(&e.to_diagnostic(), &script, &path);
            return ExitCode::FAILURE;
        }
    };

    let ast = match Parser::new(tokens.into_iter()).parse() {
        Ok(ast) => ast,
        Err(e) => {
            report(&e.to_diagnostic(), &script, &path);
            return ExitCode::FAILURE;
        }
    };

    match Interpreter::new(ast).run() {
        Ok(value) => ExitCode::SUCCESS,
        Err(err) => {
            report(&err.to_diagnostic(), &script, &path);
            ExitCode::FAILURE
        }
    }
}

/// Print a diagnostic to stderr, with a source snippet when it has a location.
fn report(diagnostic: &Diagnostic, source: &str, path: &str) {
    eprint!(
        "{}",
        diagnostic.render(source, path, diagnostic::use_color())
    );
}

fn run_repl() {
    println!("Welcome to the Aja REPL! Type expressions or statements to evaluate them.");
    println!("Type 'exit' or 'quit' to exit, or 'help' for more information.");

    let mut interpreter = Interpreter::new(ast::Program {
        definitions: Vec::new(),
    });

    // Initialize builtins
    if let Err(e) = interpreter.defines() {
        report(&e.to_diagnostic(), "", "<repl>");
        return;
    }

    // Every input becomes the next line of one session, so errors in code from
    // earlier inputs (e.g. a closure defined earlier) still point at the right line.
    let mut history = String::new();
    let mut line_no: u32 = 0;

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            // End of input (e.g. Ctrl-D)
            Ok(0) => {
                println!();
                break;
            }
            Ok(_) => {
                let input = input.trim();

                // Handle special commands
                match input {
                    "exit" | "quit" => {
                        println!("Goodbye!");
                        break;
                    }
                    "help" => {
                        println!("Available commands:");
                        println!("  exit, quit - Exit the REPL");
                        println!("  help - Show this help message");
                        println!("  Any valid expression or statement - Evaluate it");
                        continue;
                    }
                    "" => continue,
                    _ => {}
                }

                // Parse and evaluate the input (try statement first, then expression)
                line_no += 1;
                history.push_str(input);
                history.push('\n');

                let tokens = match lexer::tokenize_from_line(input, line_no) {
                    Ok(tokens) => tokens,
                    Err(e) => {
                        report(&e.to_diagnostic(), &history, "<repl>");
                        continue;
                    }
                };
                let mut parser = Parser::new(tokens.clone().into_iter());

                // Try parsing as a statement first
                match parser.parse_stmt_only() {
                    Ok(stmt) => match interpreter.interp_stmt(stmt) {
                        Ok(value) => {
                            // Only print the value if it's not Void
                            if !matches!(value, crate::interp::value::Value::Void) {
                                println!("{:?}", value);
                            }
                        }
                        Err(err) => report(&err.to_diagnostic(), &history, "<repl>"),
                    },
                    Err(_) => {
                        // If statement parsing fails, try parsing as an expression
                        let mut parser = Parser::new(tokens.into_iter());
                        match parser.parse_expr_only() {
                            Ok(expr) => match interpreter.interp_expr(expr) {
                                Ok(value) => println!("{:?}", value),
                                Err(err) => report(&err.to_diagnostic(), &history, "<repl>"),
                            },
                            Err(e) => report(&e.to_diagnostic(), &history, "<repl>"),
                        }
                    }
                }
            }
            Err(error) => {
                report(
                    &Diagnostic::new(format!("could not read input: {error}")),
                    "",
                    "<repl>",
                );
                break;
            }
        }
    }
}
