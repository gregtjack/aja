
# Aja

Aja is a small, expression-oriented programming language written in Rust. The
long-term goal is a lightweight, fast, compiled systems language (with a
[QBE](https://c9x.me/compile/) backend). Today, Aja runs on a **tree-walking
interpreter** with a REPL; the compiler backend is scaffolded but not yet
implemented.

```rust
fn fact(n: int) -> int {
    if n == 0 { return 1; }
    n * fact(n - 1)
}

fn main() -> int {
    let start = clock();
    let add = |a: int, b: int| -> int a + b;

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
| functions | `\|x: int\| -> int x + 1` | First-class; closures capture their scope |

Type annotations are optional and are **checked at runtime**: on `let`
bindings, function parameters, and function return values. An unannotated
binding has type `any`, which accepts anything.

```rust
let x: int = 5;      // ok
let s: str = 5;      // TypeError: cannot cast int to str
```

### Syntax

- **Variables:** `let name: type = expr;` declares; `name = expr` assigns.
  Blocks introduce a new lexical scope.
- **Functions:** `fn name(a: int, b) -> int { ... }`. Parameter and return
  annotations are optional.
- **Closures:** `|a, b| expr` or `|a: int| -> int { ... }`. Closures capture
  their enclosing environment and can mutate it (e.g. counters).
- **Blocks are expressions:** the final expression without a trailing `;` is
  the block's value. `return expr;` exits a function early.
- **`if`:** `if cond { ... } else { ... }` is an expression; `if` without
  `else` is a statement.
- **`while`:** `while cond { ... }`.
- **Operators** (lowest to highest precedence):
  `=` → `==` `!=` → `||` → `&&` → `<` `<=` `>` `>=` → `+` `-` → `*` `/`
  → unary `!` `-` → calls `f(x)(y)`.
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
- `%` is lexed but not parsed or evaluated.
- `match`, `::`, `=>`, `<-`, `[` `]` are lexed but have no grammar.
- `struct` definitions and a `List<T>` type exist in the AST only.
- The QBE compiler backend is a stub.

**Known issues:**
- Identifiers may contain `-`, so `a-b` is read as a single name; write
  `a - b`. Similarly, `x -1` lexes `-1` as a negative literal.
- `&&` and `||` evaluate both operands (no short-circuiting).
- Redeclaring a variable in the same scope (`let a = 1; let a = 2;`) errors
  with a misleading "Undefined variable" message.
- Unary `-` does not work on floats.
- Some errors panic instead of being reported cleanly: parse errors, wrong
  argument counts, and errors raised while evaluating call arguments.
- `test_assert.aja` currently fails on `assert(b && true)` where `b` is false.
