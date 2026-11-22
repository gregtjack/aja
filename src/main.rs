use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
    process::ExitCode,
};

use crate::{
    ast::Statement,
    diagnostic::Diagnostic,
    vm::{value::Value, Compiler, Vm},
};
use parse::Parser;
mod ast;
mod builtins;
mod check;
mod compile;
mod diagnostic;
mod lexer;
mod parse;
mod token;
mod vm;

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

    let types = match check::check_program(&ast) {
        Ok(types) => types,
        Err(errors) => {
            report_type_errors(&errors, &script, &path);
            return ExitCode::FAILURE;
        }
    };

    let program = match Compiler::new().program(&ast, &types) {
        Ok(program) => program,
        Err(e) => {
            report(&e, &script, &path);
            return ExitCode::FAILURE;
        }
    };

    match Vm::new().run_program(&program) {
        Ok(_) => ExitCode::SUCCESS,
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

/// Print every type error, then a summary line.
fn report_type_errors(errors: &[check::TypeError], source: &str, path: &str) {
    for (i, e) in errors.iter().enumerate() {
        if i > 0 {
            eprintln!();
        }
        report(&e.to_diagnostic(), source, path);
    }
    if errors.len() > 1 {
        eprintln!("\n{} type errors found", errors.len());
    }
}

fn run_repl() {
    println!("Welcome to the Aja REPL! Type expressions or statements to evaluate them.");
    println!("Type 'exit' or 'quit' to exit, or 'help' for more information.");

    let mut repl = Repl {
        checker: check::Checker::new(),
        compiler: Compiler::new(),
        vm: Vm::new(),
    };

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

                // Try parsing as a statement first, then as an expression.
                let parsed = match Parser::new(tokens.clone().into_iter()).parse_stmt_only() {
                    Ok(stmt) => Ok(stmt),
                    Err(_) => Parser::new(tokens.into_iter())
                        .parse_expr_only()
                        .map(Statement::Expr),
                };
                match parsed {
                    Ok(stmt) => repl.eval(&stmt, &history),
                    Err(e) => report(&e.to_diagnostic(), &history, "<repl>"),
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

/// The REPL's state, which carries over from one input to the next.
struct Repl {
    checker: check::Checker,
    compiler: Compiler,
    vm: Vm,
}

impl Repl {
    /// Check, compile, and run one input, printing its value or its errors.
    fn eval(&mut self, stmt: &Statement, history: &str) {
        // An input that fails at runtime must not leave behind the variables
        // it would have defined, so keep what to go back to.
        let saved = (self.checker.clone(), self.compiler.clone());

        let types = match self.checker.check_repl_stmt(stmt) {
            Ok(types) => types,
            Err(errors) => return report_type_errors(&errors, history, "<repl>"),
        };
        let input = self.compiler.repl(stmt, &types, input_span(history));
        match self.vm.run_repl(input) {
            Ok(Value::Void) => {}
            Ok(value) => println!("{}", self.vm.heap.repr(&value)),
            Err(err) => {
                report(&err.to_diagnostic(), history, "<repl>");
                (self.checker, self.compiler) = saved;
            }
        }
    }
}

/// The span of the most recent REPL input: the last line of `history`.
fn input_span(history: &str) -> ast::Span {
    let line = history.lines().count() as u32;
    let len = history.lines().last().map_or(0, |l| l.chars().count() as u32);
    ast::Span::new(ast::Location::new(0, line, 1), ast::Location::new(len, line, len + 1))
}
