# Aja Design

This document records what Aja is trying to be, the rules that follow from
that, and the things Aja deliberately will not do. When a design question comes
up, the answer should follow from this file. If it doesn't, this file is
incomplete and should be updated.

## Principle

> **Aja is the language that is easiest for humans and agents to write
> correctly.**

"Correctly" is the key word. Ease of *writing* code is cheap; ease of writing
code that works, and of knowing that it works, is what we optimize for. Humans
and agents mostly want the same things: mistakes caught early, code that can be
understood locally, one obvious way to do things, and a fast feedback loop.

## Design rules

1. **Errors are found before the program runs.** Aja is statically typed, with
   type inference so annotations are needed mainly at function boundaries.
   Every type error is a compile error.
2. **No null.** Absence is `Option<T>`. There is no value that silently
   inhabits every type.
3. **Errors are values.** Fallible functions return `Result<T, E>`, and a `?`
   operator propagates errors. There are no exceptions. A panic is for bugs,
   not for control flow.
4. **No hidden control flow.** No implicit conversions, no exceptions, no
   user-defined operator overloading that can do arbitrary work, and no
   constructors or destructors that run without being visible at the call site.
5. **Local reasoning.** A function can be understood from its signature and
   its body. Signatures are always fully typed; inference stays inside bodies.
   Bindings are immutable unless declared mutable: `let` names a value that
   never changes, and only a `var` can be assigned to, so a reader knows which
   names can change without reading the rest of the body. Parameters and
   names bound by patterns are immutable too.
6. **Data is modeled with structs and enums.** Enums carry data (sum types), and
   `match` is exhaustive: adding a variant produces a compile error at every
   `match` that doesn't handle it.
7. **Memory is managed automatically.** There is no manual allocation or
   freeing, and no lifetimes or borrow checking.
8. **Concurrency without colored functions.** Any function can block; tasks
   are cheap green threads scheduled by the runtime. There is no `async`/`await`
   split. Concurrency is structured: a task cannot outlive the scope that
   spawned it, and cancellation propagates.
9. **Diagnostics are a product.** Every error points at the source, says what
   was expected and what was found, and suggests a fix where possible. Every
   diagnostic is also available in a machine-readable form (JSON, with spans and
   suggested edits) so editors and agents can apply fixes automatically.
10. **One way to do it.** One official formatter with no configuration, one
    build tool, one test runner, one package layout. Code written by different
    people (or agents) should look the same.
11. **Fast feedback beats peak speed.** Compile times and test times are
    features. A change that makes programs 5% faster but compiles 2× slower is
    a regression.

## What we borrow

| From            | Take                                                                  | Leave behind                          |
|-----------------|-----------------------------------------------------------------------|---------------------------------------|
| Rust            | enums with data, exhaustive `match`, `Result`/`Option`, traits, expressions everywhere, diagnostic quality | borrow checker, lifetimes, macro complexity |
| Go              | goroutine-style green threads, GC'd runtime, fast compiles, single-binary deploys, one formatter | `nil`, `if err != nil` boilerplate |
| Swift / Kotlin  | type inference, null safety, readable syntax, `let`/`var`                          | feature sprawl                        |
| Elixir / Erlang | structured concurrency, supervision, "let it crash" for bugs          | dynamic typing                        |
| Zig             | no hidden control flow; explicit allocation where it matters          | manual memory management              |

## Non-goals

These are features Aja will not have, even though other languages have them
and people will ask for them. Each one conflicts with a rule above.

- **Null, or nullable-by-default types.** (Rule 2)
- **Exceptions.** (Rules 3, 4)
- **Implicit numeric or string conversions.** (Rule 4)
- **Dynamic typing or an `any` escape hatch as a default.** Unannotated code is
  inferred, never untyped. (Rule 1)
- **Inheritance.** Use structs, enums, and traits.
- **User-defined syntax macros.** They break local reasoning and tooling.
  Compile-time code generation, if added, must produce ordinary, inspectable
  Aja code. (Rule 5)
- **Function overloading by argument type.** (Rule 5)
- **Glob imports** (`import foo::*`). Every name in a file is either defined
  there, in the prelude, or named in an `import`. (Rule 5)
- **Cyclic imports between modules.** They make modules impossible to check
  one at a time and usually mean two modules are really one. (Rules 5, 11)
- **A separate closure syntax.** A closure is an anonymous `fn` with the same
  parameters, return type, and block body as a named function:
  `fn(x) { x * 2 }`. No `|x| x * 2`, no `x => x * 2`, and no brace-less
  bodies. Requiring braces also removes any ambiguity between a return type
  and the start of the body. (Rule 10)
- **`fn` in function types.** A function type is written `(int) -> bool`,
  never `fn(int) -> bool`, so a type can't be mistaken for a closure, and
  its return type is always written (`() -> void`). (Rules 5, 10)
- **`async`/`await` or any other function coloring.** (Rule 8)
- **Manual memory management or lifetimes in the surface language.** (Rule 7)
- **Formatter options or style configuration.** (Rule 10)
- **A JIT.** With static types, ahead-of-time compilation delivers the
  performance a JIT would, without warm-up or runtime complexity. (Rule 11)

## Implementation direction

```
source ─► lexer ─► parser ─► AST ─► type checker ─► typed IR ─┬─► bytecode VM   (dev, REPL, tests)
                                                              └─► Cranelift     (AoT native binary)
```

- **Type checker.** A new pass between parsing and execution. It infers types
  inside function bodies, checks signatures, and reports errors with spans. The
  backends trust its output and do no type checks of their own.
- **Bytecode VM.** Replaced the tree-walking interpreter. It is register-based
  and uses typed instructions (for example `add.i64` rather than a generic `add`
  that inspects tags at runtime). It is where the garbage collector, the task
  scheduler, and the concurrency semantics are designed and tested first, while
  they are still cheap to change. It also powers the REPL.
- **Tail calls are guaranteed.** A call in tail position (a function's final
  expression, or the value of a `return`) replaces the caller's frame, so tail
  recursion runs in constant space. This is part of the language, not an
  optimization a backend may skip: code that relies on it must work
  everywhere, so the Cranelift backend has to keep it (Cranelift's `tail`
  calling convention supports this). Other calls use a call stack that grows
  as needed, up to a memory limit (256 MB in the VM), and overflowing it is a
  runtime error, never a crash.
- **Ahead-of-time compilation with Cranelift.** Cranelift replaces the planned
  QBE backend because it supports stack maps, which a precise garbage collector
  needs in order to find pointers on the stack. QBE has no such support.
  Cranelift is also written in Rust, like the rest of Aja.
- **Memory management.** A precise tracing GC, chosen over reference counting
  with compile-time elision plus a cycle collector (as in Swift, Nim's ORC, or
  Koka's Perceus). Tracing collects cycles without extra machinery, needs no
  atomic count updates once tasks run on several OS threads, and matches the
  stack maps Cranelift provides for native code. The VM's collector is a
  simple non-moving mark-and-sweep; it can become generational or
  incremental later without changing the language.
- **Concurrency runtime.** Stackful green threads multiplexed over a small pool
  of OS threads, with I/O through kqueue / epoll / io_uring. In the VM, a task's
  frames live on the heap. In native code, each task gets its own growable stack
  (a large virtual memory reservation with a guard page).

## Modules and packages

- **A file is a module, and the directory tree is the module tree.**
  `src/geo/shapes.aja` is the module `geo::shapes`. There are no `mod`
  declarations: where a file lives is the only thing that decides its name.
  (Rule 10)
- **`import` brings in modules or named items, and code stays qualified.**
  `import std::fs;` makes `fs::read(path)` available; `import
  std::collections::{Map, Set};` brings in those names directly. A reader can
  always tell where a name comes from without leaving the file. `::` already
  separates an enum from its variants (`Shape::Circle`), and module paths
  extend the same syntax: `geo::shapes::Shape::Circle`. (Rule 5)
- **Private by default.** Functions, types, and struct fields are visible only
  inside their module unless marked `pub`. Private fields let a module keep
  its invariants, so a library type such as `Map` can hide its
  representation.
- **No import cycles.** Modules form a directed acyclic graph, and a cycle is a
  compile error that names every module in it. Because signatures are always
  fully typed, a module can be checked against the signatures of the modules
  it imports without reading their bodies, which makes checking incremental
  and parallel. (Rule 11)
- **Methods live next to their type or their trait.** An `impl` must be in the
  module that defines the type or the trait (Rust's orphan rule), so there is
  exactly one place to look for a method.
- **A small prelude.** `Option`, `Result`, `Some`, `None`, `Ok`, `Err`, `List`,
  `print`, `println`, `assert`, and `panic` are in scope everywhere. Everything
  else needs an `import`.
- **One package layout.** A package is a directory with an `aja.toml` and a
  `src/`, built and run by one tool (`aja run`, `aja build`, `aja test`).
  Dependency management comes later, but import paths leave room for it: a
  dependency's package name is the first segment of its paths, as `std` is
  for the standard library. (Rule 10)

## The standard library

Aja ships a large standard library so that programs are useful without
reaching for dependencies, in the spirit of Go and Python but typed, with
errors as values.

- **Written in Aja, over a thin native layer.** Most of the library is
  ordinary Aja code. Only a small set of intrinsics (system calls, memory, and
  operations Aja can't express) is implemented natively, and each backend
  implements just that set. This keeps the VM and the Cranelift backend in
  step, and makes the library the largest test of the language.
- **Embedded in the compiler.** The library is built into the `aja` binary and
  versioned with it, so it works with nothing to install.
- **Fallible functions return `Result`.** Functions that panic on bad input are
  for bugs, and say so in their names (`unwrap`, `expect`). (Rule 3)
- **I/O is blocking.** Concurrency comes from green threads, so `fs::read`
  simply blocks its task. No API has a separate async form, and none needs to
  change when the scheduler arrives. (Rule 8)
- **One of each.** One string type, one list, one map, one formatting
  mechanism. (Rule 10)
- **Documented with examples that run.** Examples in doc comments run as tests,
  so documentation can't drift from behavior.

The library grows in layers:

| Layer      | Modules |
|------------|---------|
| Prelude    | `Option`, `Result`, `List`; methods on `str`, `int`, and `float` (`len`, `split`, `trim`, `parse`, `abs`, `min`, `to_str`, …) |
| Core       | `collections` (`Map`, `Set`, `Deque`), `string` (`Builder`, UTF-8 iteration), `math`, `fmt`, `testing` |
| System     | `fs`, `io` (stdin, stdout, readers and writers), `path`, `env` (arguments, environment variables), `process`, `time` (`Instant`, `Duration`), `random` |
| Data       | `json`, `encoding` (base64, hex), `csv` |
| Later      | `task` and `channel` (with green threads), `net`, `http`, `regex`, `hash` (SHA-256 and friends), `log`, `cli` |

Before most of this can be written, the language needs:

1. **Generics** ✅, for `Option<T>`, `Result<T, E>`, `List<T>`, and `Map<K, V>`.
2. **Methods** (`impl` blocks), so that `s.trim().split(",")` reads left to
   right and editors and agents can discover an API by typing `.`.
3. **A few traits**: `Eq`, `Hash`, `Ord`, and `Show`, which map keys, sorting,
   and printing need.
4. **A `for` loop** over an `Iter` trait.

`int` is 64-bit, so lengths, file sizes, and millisecond timestamps all fit
in it, and library signatures use `int` for them. `float` is 64-bit too
(IEEE 754 double), so it matches what JSON, C, and most other languages mean
by a number with a fractional part.

## Roadmap

1. **Static type checker.** ✅ Annotations are required on function
   signatures, local types are inferred by unification (without
   let-polymorphism), and `any` is gone.
2. **Data types.** Structs ✅, enums with data ✅, and exhaustive `match` ✅
   (checked with Maranget's usefulness algorithm, which also rejects
   unreachable arms). Struct and enum values are immutable and compare
   structurally with `==`. Generics ✅: functions, structs, and enums take
   type parameters, and type arguments are inferred at each use. A generic
   body is checked once, with each type parameter as a type equal only to
   itself, and compiled once, since VM values have one representation
   whatever their type. Next: `Option<T>` and `Result<T, E>` in the
   prelude, and `?`.
3. **Bytecode VM.** ✅ The type-checked AST compiles to typed, register-based
   bytecode, and the tree-walking interpreter is gone. The checker hands the
   compiler operand types rather than a full typed IR; a typed IR can come
   when data types need it.
4. **Memory management.** ✅ Closures, captured variables, struct and enum
   values, and strings live on a garbage-collected heap with a precise
   mark-and-sweep collector, which runs when the heap's size in bytes has
   doubled. String literals are allocated once and kept as roots; strings
   built at runtime (with `+`) are collected. Nothing is reference-counted.
5. **Language features for libraries.** Generics ✅, methods, the core traits,
   and `for` loops (see [The standard library](#the-standard-library)).
6. **Modules and packages.** `import`, `pub`, the prelude, `aja.toml`, and the
   `aja` tool (see [Modules and packages](#modules-and-packages)).
7. **Standard library**, layer by layer, starting with the prelude and core.
8. **Tooling.** Formatter, JSON diagnostics, and a test runner; then a language
   server.
9. **Green threads and structured concurrency** in the VM.
10. **Cranelift AoT backend** producing native binaries, with the same runtime
    semantics as the VM.

## Open questions

- **Sized integers.** `int` is 64-bit. Do sized types (`i8` … `u64`) arrive
  as well, for binary formats and interop, and how do conversions between
  them look without being implicit (rule 4)?
- **Indexing.** `[` is lexed but has no grammar. Does `xs[i]` exist, and if so
  does it panic when out of range (with `xs.get(i)` returning `Option`)?
- **String interpolation.** `"x = {x}"` is much easier to write than
  concatenation, but it calls `Show` implicitly. Is that hidden control flow
  (rule 4), or acceptable because `Show` is meant to have no side effects?
- **User-defined traits.** Do traits start as a fixed built-in set (`Eq`,
  `Hash`, `Ord`, `Show`, `Iter`), or can programs define their own from the
  start?
- **Traits and generics.** How far do generics go (no higher-kinded types is a
  likely non-goal), and are traits implemented nominally or structurally?
  Today a type parameter has no bounds, so generic code can't use `==` or
  operators on it; bounds (`fn max<T: Ord>`) come with traits.
- **Explicit type arguments.** Type arguments are always inferred, and an
  annotation fixes one inference can't. Is a call-site syntax
  (`none::<int>()`) ever needed, and does it fit "one way to do it"?
- **Strings.** UTF-8 everywhere, and is indexing by byte, by code point, or not
  allowed at all?
