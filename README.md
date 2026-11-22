
# Aja

Aja is a small, expression-oriented programming language written in Rust. The
goal is the language that is easiest for humans and agents to write correctly:
statically typed with inference, garbage collected, with green-thread
concurrency, running on a bytecode VM during development and compiled ahead
of time to native code through Cranelift. See [DESIGN.md](DESIGN.md) for the
principles, non-goals, and roadmap.

Today, Aja is **statically type-checked** with type inference, and runs on a
**register-based bytecode VM** with a REPL.

```rust
fn fact(n: int) -> int {
    if n == 0 { return 1; }
    n * fact(n - 1)
}

fn main() -> int {
    let start = clock();
    let add = fn(a: int, b: int) -> int { a + b };

    var i = 0;
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

Run a source file (executes `main`):

```bash
cargo run -- examples/hello.aja
```

[`examples/`](examples) has programs that show off the language, plus
some that fail on purpose to show the error messages.

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
`fn main`, which takes no parameters. Its return value is not printed; use
`println` for output.

### Types and values

| Type    | Literal examples        | Notes                                |
|---------|-------------------------|--------------------------------------|
| `int`   | `42`, `-7`, `1_000`     | 64-bit signed integer                |
| `float` | `3.14`, `-0.5`          | 64-bit float                         |
| `bool`  | `true`, `false`         |                                      |
| `str`   | `"hi\n"`                | Escapes: `\n`, `\t`, `\r`, `\\`; join with `+` |
| `void`  | —                       | Result of statements / no value      |
| `(int, str) -> bool` | `fn(x: int) -> int { x + 1 }` | First-class; closures capture their scope |
| structs and enums | `Point { x: 1, y: 2 }`, `Shape::Circle(p, 3)` | Defined at the top level; see below |

### Structs, enums, and `match`

```rust
struct Point { x: int, y: int }

enum Shape {
    Circle(Point, int),
    Rect(Point, Point),
    Empty,
}

fn area(s: Shape) -> int {
    match s {
        Shape::Circle(_, r) => 3 * r * r,
        Shape::Rect(a, b) => (b.x - a.x) * (b.y - a.y),
        Shape::Empty => 0,
    }
}
```

- **Structs** have named fields: build one with `Point { x: 1, y: 2 }` (or
  `Point { x, y }` when variables have the fields' names) and read a field
  with `p.x`. Fields can't be assigned; make a new value instead, copying
  the fields you don't give from another with `Point { y: 5, ..p }`.
- **Enums** have variants that hold values by position, or none:
  `Shape::Circle(p, 3)`, `Shape::Empty`. Variants are always written with
  their enum's name. Types can be recursive (`enum List { Nil, Cons(int, List) }`)
  and can refer to each other in any order.
- **`match`** tries its arms in order: `pattern => value,` or
  `pattern if guard => value,`, where a value can be a block. Patterns are
  `_`, a name (which binds the value), int, bool, and string literals, variant
  patterns `Shape::Circle(center, r)`, and struct patterns
  `Point { x: 0, y }` (end with `..` to skip the other fields). They nest.
  A `match` is an expression, and its arms are tail positions.
- **`match` is exhaustive.** Leaving a case out is a compile error that names
  the missing cases (`Shape::Empty` not covered), so adding a variant
  points you at every `match` to update. An arm that can never match, because
  earlier arms cover it, is an error too. Guarded arms don't count toward
  covering a case.
- **`==`** compares structs and enums field by field, as long as everything
  in them can be compared (no functions).
- In the head of an `if`, `while`, or `match`, a struct literal needs
  parentheses: `if p == (Point { x: 0, y: 0 }) { ... }`, as in Rust.

The checker for these lives in `src/check/data.rs`. In the VM, struct and
enum values are immutable heap objects: a tag, and the values they hold.
`Option` and `Result` need generics, which come next.

### Type checking

Every program is type-checked before it runs, and every type error is
reported (all of them, not just the first) without running anything. There
is no `any` type: every value has one static type.

- **Signatures are written out.** Top-level functions annotate every
  parameter. A function with no `-> T` returns `void`. Function types are
  written without `fn`, as the parameter types in parentheses and the
  return type, which is always given: `(int, str) -> bool`, or
  `(int) -> void` for one that returns nothing. Leaving out `fn` keeps a
  type from looking like a closure. Higher-order functions are declared
  with them:
  `fn apply(f: (int) -> int, x: int) -> int { f(x) }`.
- **Everything inside a body is inferred.** `let` bindings, closure
  parameters, and closure return types need no annotations; their types come
  from how they're used:

  ```rust
  let twice = fn(f, x) { f(f(x)) };       // f: (int) -> int, x: int
  twice(fn(y) { y * 2 }, 3)
  ```

- **One type per binding.** A closure has a single type, fixed by its uses,
  so `let id = fn(x) { x }; id(1); id(true);` is an error. (Generic
  functions, when they arrive, will be declared in signatures.)
- If a type can't be worked out (e.g. a closure parameter that's never
  used), the checker asks for an annotation rather than guessing.

The checker lives in `src/check/`. It is unification-based, the core of
Hindley–Milner inference, without let-polymorphism. Operators that accept
several types (`+` on int, float, or str; `==` on int, float, bool, or str) keep
an unresolved type restricted until it's known, much like Rust's `{integer}`.

```
error: mismatched types: function 'one' has no return type, so it returns void, but this is int
 --> main.aja:1:12
  |
1 | fn one() { 1 }
  |            ^
  = help: add a return type after the parameters: `-> int`
```

### Syntax

- **Variables:** `let name: type = expr;` declares an immutable variable;
  `var name: type = expr;` declares a mutable one, which `name = expr`
  assigns. Assigning to a `let`, a parameter, or a name bound by a pattern is
  a type error; to change a parameter, copy it first (`var n = n;`). Blocks
  introduce a new lexical scope, and a later `let` or `var` with the same
  name shadows the earlier one.
- **Functions:** `fn name(a: int, b: str) -> int { ... }`. Parameters must be
  annotated; omitting `-> T` means the function returns `void`.
- **Closures:** an anonymous `fn` with the same syntax as a named function,
  minus the name: `fn(a, b) { a + b }`, `fn(x: int) -> int { x * 2 }`,
  `fn() { ... }`. The body is always a block, and annotations are optional.
  Closures capture their
  enclosing environment and can mutate its `var`s (e.g. counters). A closure
  bound with `let` or `var` can call itself by that name:
  `let fact = fn(n: int) { if n == 0 { 1 } else { n * fact(n - 1) } };`.
  Other values don't see the name they are bound to, so `let a = a + 1;`
  uses an earlier `a`.
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
| `println(a, b, …)` | Like `print`, followed by a newline            |
| `clock()`        | Current time in milliseconds, as an `int`        |
| `sleep(ms)`      | Sleeps for `ms` milliseconds                     |
| `assert(cond)`   | Stops execution with an error if `cond` is false |

To add a built-in, declare its name and signature in `BUILTINS`
(`src/builtins.rs`), which the type checker reads, and implement it in each
backend (for the VM, `src/vm/native.rs`). A built-in without an
implementation fails the tests and stops the VM at startup.

## Architecture

```
source ──► Lexer ──► Parser ──► AST ──► Type checker ──► Compiler ──► Bytecode VM
          lexer.rs   parse.rs   ast/    check/           vm/compile.rs  vm/mod.rs
                                  └───► compile/ (QBE backend, stub)
```

| Path                  | Role |
|-----------------------|------|
| `src/main.rs`         | CLI (clap): file mode and REPL |
| `src/token.rs`        | Token definitions |
| `src/lexer.rs`        | Hand-written lexer; tracks line/column for errors |
| `src/parse.rs`        | Recursive-descent parser (plus unit tests) |
| `src/ast/`            | AST nodes (`Definition`, `Statement`, `Expression`) and `Type` |
| `src/check/`          | Static type checker with local type inference; `data.rs` checks structs, enums, and `match` exhaustiveness |
| `src/builtins.rs`     | Built-in function signatures, shared by the checker and backends |
| `src/vm/bytecode.rs`  | The instruction set and compiled functions (`Proto`) |
| `src/vm/compile.rs`   | Compiles the type-checked AST to bytecode |
| `src/vm/mod.rs`       | The VM: call frames, the dispatch loop, runtime errors |
| `src/vm/value.rs`     | Runtime values |
| `src/vm/heap.rs`      | Garbage-collected heap for closures and cells |
| `src/vm/native.rs`    | Built-in implementations |
| `src/diagnostic.rs`   | Error rendering (source snippets, carets, help, color) |
| `src/compile/`        | Code-generation scaffolding targeting QBE (not yet wired up) |

The VM is register-based. Each call gets a window of registers holding its
parameters, locals, and temporaries, and instructions name registers
directly. Instructions are typed: the type checker records whether each
operator works on ints, floats, bools, or strings, and the compiler emits,
say, `AddI` or `AddF` accordingly, so the VM never inspects operand types to
decide what to do and has no runtime type checks.

Calls are guaranteed to be tail calls when they're in tail position: the
final expression of a function (including both branches of a final
`if`/`else`) or the value of a `return`. A tail call reuses the current
frame, so tail recursion runs in constant space, like a loop. Other calls
keep their frame until they return. Frames are kept in a growable array, not
on the Rust stack, so deep recursion can't crash the VM. The call stack may
use up to 256 MB (about 1.5 million simple non-tail calls); beyond that the
program stops with a stack overflow error. Error traces list a tail call
like any other call, without mentioning tail calls; the caller it replaced
is no longer on the stack, so it isn't listed.

A closure copies the variables it captures when it is made, which is all an
immutable variable needs. A `var` that a closure captures is stored in a
shared cell instead, and closures copy the cell, so the function and its
closures see each other's assignments. The compiler finds these variables in
a pass over each function before compiling it. A closure that calls itself
through its `let` captures itself, filled in as soon as it is made.

Closures, cells, struct and enum values, and strings live on a garbage-collected heap (`src/vm/heap.rs`). The
collector is a precise mark-and-sweep: it starts from the registers in use,
the globals, and the call frames, marks every object reachable from them, and
frees the rest, so cycles (such as a closure that calls itself through a
variable it captures) are freed too. It runs when the VM allocates, once the
heap's size in bytes has doubled since the last collection, so a few large
strings trigger it as surely as many small objects. String literals are put
on the heap once, when the code using them is loaded, and kept as roots;
strings built with `+` are freed once unreachable. A string may be at most
`i32::MAX` bytes; building a longer one is a runtime error. The VM tests run with the
collector in stress mode, collecting before every allocation, so any value
the VM forgets to keep reachable is caught.

## Status

**Working:** ints, floats, bools, strings; immutable `let` and mutable
`var` with lexical scoping; functions and first-class closures; `if`/`else` expressions;
`while` loops; `return`; recursion, with guaranteed tail calls; arithmetic, comparison, and logical operators;
structs, enums with data, and exhaustive `match`;
static type checking with inference inside function bodies; built-ins (`print`, `clock`, `sleep`,
`assert`); a REPL; parser, type checker, and VM unit tests.

**Reserved or partially present, not yet usable:**
- `<-`, `[` `]` are lexed but have no grammar.
- A `List<T>` type exists in the AST only; generics aren't implemented yet.
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

Type errors (see [Type checking](#type-checking)) point at the offending
expression, say what was expected and what was found, and suggest close
matches for misspelled names.

Runtime errors, such as division by zero or a failed `assert`, point at the
expression that failed and list the calls that led there (repeated
recursive calls are collapsed):

```
error: division by zero
 --> main.aja:1:23
  |
1 | fn c(x: int) -> int { 100 / x }
  |                       ^^^^^^^
  = note: in function 'c', called at line 2, column 23
  = note: in function 'b', called at line 3, column 20
```

Every AST expression carries a `Span` (`ast::Expr { kind, span }`), and the
compiler records the span each instruction came from, which is how runtime
errors know their location. In the REPL, each input counts as a
new line of the session, so errors in code from an earlier input still point
at that input.

Output is colored when stderr is a terminal; set `NO_COLOR` to disable.
`src/diagnostic.rs` holds the shared renderer.

**Known limitations:**
- The REPL handles one line at a time and can't define top-level functions, structs, or enums.
- There is no `else if`; nest an `if` inside the `else` block instead.
- Only `//` line comments are supported.
- `==` and `!=` bind more loosely than `&&` and `||`, so write
  `(a == b) && (c == d)`.
