
# Aja

Aja is a small, expression-oriented programming language written in Rust. The
goal is the language that is easiest for humans and agents to write correctly:
statically typed with inference, garbage collected, with green-thread
concurrency, running on a bytecode VM during development and compiled ahead
of time to native code through Cranelift. See [DESIGN.md](DESIGN.md) for the
principles, non-goals, and roadmap.

Today, Aja runs on a **tree-walking interpreter** with a REPL, and type
annotations are optional and checked at runtime.

```rust
fn fact(n: int) -> int {
    if n == 0 { return 1; }
    n * fact(n - 1)
}

fn main() -> int {
    let start = clock();
    let add = fn(a: int, b: int) -> int { a + b };

    let i = 0;
    while i < 3 {
        print("i =", i, "\n");
        i = i + 1;
    }

    print("took", clock() - start, "ms\n");
    add(fact(5), 1)
}
```

## Getting started

Requires a Rust toolchain. A Nix flake (`flake.nix`, with `.envrc` for direnv)
provides a dev shell with Rust, `rust-analyzer`, and `qbe`.

```bash
cargo build
```

Run a source file (executes `main` and prints its return value):

```bash
cargo run -- 42.aja
```

Start the REPL (no arguments):

```bash
cargo run
```

The REPL accepts single-line statements or expressions; `help` lists
commands and `exit` / `quit` leaves.

Run the tests:

```bash
cargo test
```

## The language today

### Programs

A program is a sequence of top-level `fn` definitions. Execution starts at
`fn main`. Its return value is printed when the program finishes.

### Types and values

| Type    | Literal examples        | Notes                                |
|---------|-------------------------|--------------------------------------|
| `int`   | `42`, `-7`, `1_000`     | 32-bit signed integer                |
| `float` | `3.14`, `-0.5`          | 32-bit float                         |
| `bool`  | `true`, `false`         |                                      |
| `str`   | `"hi\n"`                | Escapes: `\n`, `\t`, `\r`, `\\`      |
| `void`  | —                       | Result of statements / no value      |
| `any`   | —                       | Default when no annotation is given  |
| functions | `fn(x: int) -> int { x + 1 }` | First-class; closures capture their scope |

Type annotations are optional and are **checked at runtime**: on `let`
bindings, function parameters, and function return values. An unannotated
binding has type `any`, which accepts anything.

```rust
let x: int = 5;      // ok
let s: str = 5;      // TypeError: cannot cast int to str
```

### Syntax

- **Variables:** `let name: type = expr;` declares; `name = expr` assigns.
  Blocks introduce a new lexical scope, and a later `let` with the same name
  shadows the earlier one.
- **Functions:** `fn name(a: int, b) -> int { ... }`. Parameter and return
  annotations are optional.
- **Closures:** an anonymous `fn` with the same syntax as a named function,
  minus the name: `fn(a, b) { a + b }`, `fn(x: int) -> int { x * 2 }`,
  `fn() { ... }`. The body is always a block. Closures capture their
  enclosing environment and can mutate it (e.g. counters).
- **Blocks are expressions:** the final expression without a trailing `;` is
  the block's value. `return expr;` exits a function early.
- **`if`:** `if cond { ... } else { ... }` is an expression; `if` without
  `else` is a statement.
- **`while`:** `while cond { ... }`.
- **Operators** (lowest to highest precedence):
  `=` → `==` `!=` → `||` → `&&` → `<` `<=` `>` `>=` → `+` `-` → `*` `/` `%`
  → unary `!` `-` → calls `f(x)(y)`.
  `&&` and `||` short-circuit. Integer division or modulo by zero is a
  runtime error.
  Arithmetic and comparison require both operands to be the same numeric
  type (no implicit int/float conversion).
- **Comments:** `// to end of line`.

### Built-in functions

| Function         | Description                                      |
|------------------|--------------------------------------------------|
| `print(a, b, …)` | Prints its arguments separated by spaces (no trailing newline) |
| `clock()`        | Current time in milliseconds, as an `int`        |
| `sleep(ms)`      | Sleeps for `ms` milliseconds                     |
| `assert(cond)`   | Stops execution with an error if `cond` is false |

## Architecture

```
source ──► Lexer ──► Parser ──► AST ──► Interpreter
          lexer.rs   parse.rs   ast/    interp/
                                  └───► compile/ (QBE backend, stub)
```

| Path                  | Role |
|-----------------------|------|
| `src/main.rs`         | CLI (clap): file mode and REPL |
| `src/token.rs`        | Token definitions |
| `src/lexer.rs`        | Hand-written lexer; tracks line/column for errors |
| `src/parse.rs`        | Recursive-descent parser (plus unit tests) |
| `src/ast/`            | AST nodes (`Definition`, `Statement`, `Expression`) and `Type` |
| `src/interp/`         | Tree-walking interpreter, environments, values, builtins |
| `src/diagnostic.rs`   | Error rendering (source snippets, carets, help, color) |
| `src/compile/`        | Code-generation scaffolding targeting QBE (not yet wired up) |

The interpreter uses a chain of reference-counted environments for lexical
scoping. Early `return` is implemented by propagating a special
`RuntimeError::Return(value)` up to the call site.

## Status

**Working:** ints, floats, bools, strings; `let`/assignment with lexical
scoping; functions and first-class closures; `if`/`else` expressions;
`while` loops; `return`; recursion (each call gets its own scope); arithmetic, comparison, and logical operators;
optional runtime type checks; built-ins (`print`, `clock`, `sleep`,
`assert`); a REPL; parser and interpreter unit tests.

**Reserved or partially present, not yet usable:**
- `match`, `::`, `=>`, `<-`, `[` `]` are lexed but have no grammar.
- `struct` definitions and a `List<T>` type exist in the AST only.
- The QBE compiler backend is a stub; it will be replaced by Cranelift (see
  [DESIGN.md](DESIGN.md)).

**Errors:** errors are reported in a rustc-like format and exit with a
non-zero status. Syntax and parse errors point at the offending source:

```
error: expected ';', found 'x'
 --> main.aja:2:12
  |
2 |   let x = 1
  |            ^
  = help: add ';' at the end of the statement
```

Runtime errors point at the expression that failed, name the types,
variables, and functions involved, suggest close matches for misspelled
names, and list the calls that led there (repeated recursive calls are
collapsed):

```
error: cannot apply '+' to int and str
 --> main.aja:1:11
  |
1 | fn c(x) { x + "!" }
  |           ^^^^^^^
  = note: in function 'c', called at line 2, column 11
  = note: in function 'b', called at line 3, column 10
```

Every AST expression carries a `Span` (`ast::Expr { kind, span }`), which is
how runtime errors know their location. In the REPL, each input counts as a
new line of the session, so errors in code from an earlier input still point
at that input.

Output is colored when stderr is a terminal; set `NO_COLOR` to disable.
`src/diagnostic.rs` holds the shared renderer.

**Known limitations:**
- The REPL handles one line at a time and can't define top-level functions.
- `main`'s return value is printed in debug form (e.g. `Int(0)`).
