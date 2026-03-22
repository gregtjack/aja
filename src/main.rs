use lexer::Lexer;
use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
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
struct Cli {
    /// Source file
    file: Option<PathBuf>,
}

fn main() {
    let cli = <Cli as clap::Parser>::parse();

    if let Some(file) = cli.file {
        // File mode
        let script = fs::read_to_string(file).unwrap();
        let lexer = Lexer::new(script.chars());
        let mut parser = Parser::new(lexer);
        let ast = match parser.parse() {
            Ok(ast) => ast,
            Err(e) => panic!("failed to parse the source files: {:?}", e),
        };

        match Interpreter::new(ast).run() {
            Ok(value) => println!("{:?}", value),
            Err(err) => eprintln!("Runtime Error: {}", err),
        }
    } else {
        // REPL mode
        run_repl();
    }
}

fn run_repl() {
    println!("Welcome to the Aja REPL! Type expressions or statements to evaluate them.");
    println!("Type 'exit' or 'quit' to exit, or 'help' for more information.");

    let mut interpreter = Interpreter::new(ast::Program {
        definitions: Vec::new(),
    });

    // Initialize builtins
    if let Err(e) = interpreter.defines() {
        eprintln!("Failed to initialize builtins: {}", e);
        return;
    }

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
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
                let lexer = Lexer::new(input.chars());
                let mut parser = Parser::new(lexer);

                // Try parsing as a statement first
                match parser.parse_stmt_only() {
                    Ok(stmt) => match interpreter.interp_stmt(stmt) {
                        Ok(value) => {
                            // Only print the value if it's not Void
                            if !matches!(value, crate::interp::value::Value::Void) {
                                println!("{:?}", value);
                            }
                        }
                        Err(err) => eprintln!("Runtime Error: {}", err),
                    },
                    Err(_) => {
                        // If statement parsing fails, try parsing as an expression
                        let lexer = Lexer::new(input.chars());
                        let mut parser = Parser::new(lexer);
                        match parser.parse_expr_only() {
                            Ok(expr) => match interpreter.interp_expr(expr) {
                                Ok(value) => println!("{:?}", value),
                                Err(err) => eprintln!("Runtime Error: {}", err),
                            },
                            Err(e) => eprintln!("Parse Error: {}", e),
                        }
                    }
                }
            }
            Err(error) => {
                eprintln!("Error reading input: {}", error);
                break;
            }
        }
    }
}
