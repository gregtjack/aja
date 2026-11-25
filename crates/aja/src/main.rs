use std::{
    io::{self, Write},
    path::{Path, PathBuf},
    process::ExitCode,
};

use aja::{
    aot,
    ast::{self, Statement},
    check, diagnostic,
    diagnostic::Diagnostic,
    lexer, module,
    parse::Parser,
    vm,
    vm::{value::Value, Compiler, Vm},
};

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
#[command(args_conflicts_with_subcommands = true)]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,
    /// Source file, to run on the bytecode interpreter (with no file, and
    /// no command, starts the REPL)
    file: Option<PathBuf>,
    /// Arguments for the program, which `std::env::args` gives it
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    args: Vec<String>,
}

#[derive(clap::Subcommand)]
enum Command {
    /// Compile a program to a native executable
    Build {
        /// Source file
        file: PathBuf,
        /// Where to write the executable (default: the file's name without
        /// `.aja`, in the current directory)
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    /// Compile a program and run it, leaving nothing behind
    Run {
        /// Source file
        file: PathBuf,
        /// Arguments for the program, which `std::env::args` gives it
        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        args: Vec<String>,
    },
    /// Type-check a program without running it
    Check {
        /// Source file
        file: PathBuf,
    },
    /// Start an interactive session on the bytecode interpreter
    Repl,
}

/// The directory `file` is in, which its imports are found from, and its
/// name there.
fn root_and_name(file: &Path) -> (module::Dir, String) {
    let root = file
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .map_or_else(|| PathBuf::from(""), PathBuf::from);
    let name = file
        .file_name()
        .map_or_else(String::new, |n| n.to_string_lossy().into_owned());
    (module::Dir(root), name)
}

/// Load the program in `file` and the modules it imports, from the
/// directory it is in, and compile it to bytecode.
fn load(file: &Path) -> (diagnostic::Files, Result<vm::Compiled, Vec<Diagnostic>>) {
    let (root, name) = root_and_name(file);
    module::build(&root, &name)
}

/// `aja check`: report the errors the type checker finds in `file` and the
/// modules it imports, and nothing else.
fn check_only(file: &Path) -> ExitCode {
    let (root, name) = root_and_name(file);
    match module::check(&root, &name) {
        (_, Ok(_)) => ExitCode::SUCCESS,
        (files, Err(errors)) => {
            report_all(&errors, &files);
            ExitCode::FAILURE
        }
    }
}

/// `aja run`: compile `file` to a native executable, run it with `args`,
/// and exit as it did. The executable is in the system's temporary
/// directory, and is removed once the program has ended.
fn run(file: &Path, args: &[String]) -> ExitCode {
    let (files, program) = load(file);
    let program = match program {
        Ok(program) => program,
        Err(errors) => {
            report_all(&errors, &files);
            return ExitCode::FAILURE;
        }
    };
    let stem = file.file_stem().unwrap_or(file.as_os_str());
    let exe = std::env::temp_dir().join(format!(
        "aja-run-{}-{}",
        std::process::id(),
        stem.to_string_lossy()
    ));
    if let Err(message) = aot::build(&program, &files, &exe) {
        report(&Diagnostic::new(message), "", "");
        return ExitCode::FAILURE;
    }
    let mut child = match std::process::Command::new(&exe).args(args).spawn() {
        Ok(child) => child,
        Err(error) => {
            let _ = std::fs::remove_file(&exe);
            report(
                &Diagnostic::new(format!("could not run the program: {error}")),
                "",
                "",
            );
            return ExitCode::FAILURE;
        }
    };
    // Ctrl-C is for the program, which gets it too: wait for it to finish
    // with it instead of stopping first, so that the executable is still
    // removed. (It can't be removed while the program runs: macOS stops a
    // program whose file has gone.)
    // SAFETY: ignoring a signal is always allowed.
    unsafe { libc::signal(libc::SIGINT, libc::SIG_IGN) };
    let status = child.wait();
    let _ = std::fs::remove_file(&exe);
    match status {
        Ok(status) => {
            use std::os::unix::process::ExitStatusExt;
            // As a shell reports a program a signal stopped.
            let code = status
                .code()
                .or(status.signal().map(|signal| 128 + signal))
                .unwrap_or(1);
            ExitCode::from(code as u8)
        }
        Err(error) => {
            report(
                &Diagnostic::new(format!("could not wait for the program: {error}")),
                "",
                "",
            );
            ExitCode::FAILURE
        }
    }
}

/// `aja build`: compile `file` to a native executable.
fn build(file: &Path, output: Option<PathBuf>) -> ExitCode {
    let (files, program) = load(file);
    let program = match program {
        Ok(program) => program,
        Err(errors) => {
            report_all(&errors, &files);
            return ExitCode::FAILURE;
        }
    };
    let output = output.unwrap_or_else(|| {
        let name = file.file_stem().unwrap_or(file.as_os_str());
        // A file with no extension would be its own output.
        if file.extension().is_some() {
            PathBuf::from(name)
        } else {
            PathBuf::from(name).with_extension("out")
        }
    });
    match aot::build(&program, &files, &output) {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            report(&Diagnostic::new(message), "", "");
            ExitCode::FAILURE
        }
    }
}

fn main() -> ExitCode {
    let cli = <Cli as clap::Parser>::parse();
    match cli.command {
        Some(Command::Build { file, output }) => return build(&file, output),
        Some(Command::Run { file, args }) => return run(&file, &args),
        Some(Command::Check { file }) => return check_only(&file),
        Some(Command::Repl) => {
            run_repl();
            return ExitCode::SUCCESS;
        }
        // With no command: a file runs on the bytecode interpreter, and
        // no file is the REPL.
        None => {}
    }

    vm::set_args(cli.args);
    let Some(file) = cli.file else {
        // REPL mode
        run_repl();
        return ExitCode::SUCCESS;
    };

    let (files, program) = load(&file);
    let program = match program {
        Ok(program) => program,
        Err(errors) => {
            report_all(&errors, &files);
            return ExitCode::FAILURE;
        }
    };

    match Vm::new().run_program(&program) {
        Ok(_) => ExitCode::SUCCESS,
        // Whoever was reading has all they wanted, so there's nothing to
        // report; the status is that of a program stopped by SIGPIPE.
        Err(err) if err.message == vm::OUTPUT_CLOSED => ExitCode::from(141),
        Err(err) => {
            report_all(&[err.to_diagnostic_in(&files)], &files);
            ExitCode::FAILURE
        }
    }
}

/// Print diagnostics from a program's files to stderr, then a summary line
/// if there is more than one.
fn report_all(diagnostics: &[Diagnostic], files: &diagnostic::Files) {
    for (i, d) in diagnostics.iter().enumerate() {
        if i > 0 {
            eprintln!();
        }
        eprint!("{}", d.render_in(files, diagnostic::use_color()));
    }
    if diagnostics.len() > 1 {
        eprintln!("\n{} errors found", diagnostics.len());
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
        let input = match self.compiler.repl(stmt, &types, input_span(history)) {
            Ok(input) => input,
            Err(err) => {
                report(&err, history, "<repl>");
                (self.checker, self.compiler) = saved;
                return;
            }
        };
        match self.vm.run_repl(&input) {
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
    let len = history
        .lines()
        .last()
        .map_or(0, |l| l.chars().count() as u32);
    ast::Span::new(
        ast::Location::new(0, line, 1),
        ast::Location::new(len, line, len + 1),
    )
}
