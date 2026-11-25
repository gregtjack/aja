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

The code is four crates in `crates/`, each depending only on those above
it:

| Crate | Directory | Holds |
|---|---|---|
| `aja-diagnostic` | `crates/diagnostic/` | Places in source (spans, files) and how errors about them are shown. |
| `aja-frontend` | `crates/frontend/` | The lexer, parser, module loader, and type checker, with the prelude and standard library sources. It runs nothing, so a formatter or language server needs only this. |
| `aja-runtime` | `crates/runtime/` | What a running program needs: values, the heap and collector, native functions, the scheduler. It has none of the compiler, and is what a native program is linked with. |
| `aja` | `crates/aja/` | The backends (the bytecode compiler and VM, and Cranelift code generation) and the `aja` tool. |

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
- **A path without braces names a module first.** `import a::b;` imports
  the module `a::b` if there is one (`a/b.aja`), and otherwise the item `b`
  of the module `a`, as `import a::{b};` would. A module wins so that adding
  an item to `a` never changes what an existing import means; the braces
  say "item" outright. A module is used through the last name of its
  path, so two modules imported whole can't share a last name; import
  what's needed from one of them by name instead. (Rules 5, 10)
- **A small prelude.** `Option`, `Result`, `Ordering`, `Iter`, `Some`, `None`, `Ok`, `Err`, `List`,
  `print`, `println`, `assert`, and `panic` are in scope everywhere. Everything
  else needs an `import`, including the other assertions, which are in
  `std::testing`.
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
  The intrinsics are declared in Aja, as `native fn`s with no body, in the
  module `std::native`, which only the standard library can import.
  They take and return only `int`, `float`, `bool`, `str`, `void`, and
  `List`s of those, so a backend implements them without knowing how Aja
  lays out structs and enums. One that can fail records a status and message, which the library
  reads straight after the call (`std::native::status`) and turns into a
  `Result`. Each task has its own status, so another task's intrinsics
  can't change it between the call and the read. The prelude declares its own `native fn`s (the methods of
  `List`) and `native struct`s (`List`), whose values a backend represents
  however it likes; programs can declare neither.
- **One package.** The library's modules can use each other's private
  items, as the files of one Rust crate can with `pub(crate)`, so shared
  helpers (such as turning an intrinsic's status into an `io::Error`) need
  not be `pub` for programs to see.
- **Embedded in the compiler.** The library is built into the `aja` binary and
  versioned with it, so it works with nothing to install.
- **Fallible functions return `Result`.** Functions that panic on bad input are
  for bugs, and say so in their names (`unwrap`, `expect`). (Rule 3)
  `main` may return a `Result` too, so `?` works there, and an `Err` from
  it fails the program with the error.
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

1. **Generics** ✅, for `Option<T>`, `Result<T, E>`, `List<T>` ✅, and `Map<K, V>`.
2. **Methods** ✅ (`impl` blocks), so that `s.trim().split(",")` reads left to
   right and editors and agents can discover an API by typing `.`.
3. **A few traits**: `Eq` ✅, `Hash` ✅, `Ord` ✅, and `Show`, which map keys,
   sorting, and printing need. For now they are built in, usable as bounds
   (`fn f<K: Hash>`), and a type has them by its shape (see
   [the collections plan](plans/collections.md)).
4. **A `for` loop** ✅ over an `Iter` (for now a prelude struct wrapping a
   "next value" function, until programs can define traits; see the open
   question).

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
   whatever their type. `Option<T>` and `Result<T, E>` ✅ are defined in
   Aja in the prelude, and `?` ✅ passes on an `Err` or `None` without
   converting it.
3. **Bytecode VM.** ✅ The type-checked AST compiles to typed, register-based
   bytecode, and the tree-walking interpreter is gone. The checker hands the
   compiler operand types rather than a full typed IR; a typed IR can come
   when data types need it.
4. **Memory management.** ✅ Closures, captured variables, struct and enum
   values, and strings live on a garbage-collected heap with a precise
   mark-and-sweep collector, which runs when the heap's size in bytes has
   doubled. String literals are allocated once and kept as roots; strings
   built at runtime (with `+`) are collected. Nothing is reference-counted.
5. **Language features for libraries.** Generics ✅, methods ✅, the core traits
   (as bounds ✅),
   and `for` loops ✅ with `break` and `continue` (see
   [The standard library](#the-standard-library)).
6. **Modules and packages.** `import` ✅, `pub` ✅ (on functions, types,
   methods, and fields), cycle errors ✅, the prelude, `aja.toml`, and the
   `aja` tool (see [Modules and packages](#modules-and-packages)). For now
   the root is the directory of the file being run, and modules are not
   yet checked one at a time: the resolver gives every definition a
   module-qualified name and hands the checker one program, deciding
   privacy within types by which file a span is in. Checking each module
   against its imports' signatures, for incremental and parallel checking,
   can replace that without changing the language.
7. **Standard library**, layer by layer, starting with the prelude and core.
   Started: the library is embedded and imported as `std::...` ✅, the
   intrinsics layer ✅, `std::io` (standard input, output, and error) ✅,
   `std::fs` (whole-file reads and writes, directories) ✅,
   `std::collections` (`Map`, `Set`, `SortedMap`, and `SortedSet`, persistent, see
   [the plan](plans/collections.md)) ✅, and methods on
   `str`, `int`, `float`, and `bool` in the prelude, with template strings ✅.
8. **Tooling**, in four parts. The first three are independent; the
   language server builds on all of them. Started: the `aja` tool's
   commands, `run` (compile to native code, run, and remove the
   executable) ✅, `build` ✅, `check` (stop after type checking) ✅, and
   `repl` ✅.
   1. **Formatter** (`aja fmt`). One style with no options (rule 10),
      printing from the AST so its output doesn't depend on how the input
      was laid out. Comments must survive, so the lexer keeps them (with
      their positions) instead of dropping them. Formatting twice gives
      the same result as formatting once, and every example and standard
      library file is formatted by it, checked in the tests.
   2. **JSON diagnostics** (`aja check --json`). Every error and warning as
      one JSON object per line: message, severity, file, start and end
      line and column, help, and notes, the same data the rustc-like
      rendering shows. A `check` command that stops after type checking,
      for editors and CI, comes with it.
   3. **Test runner** (`aja test`) ✅. A test is a block at the top level,
      `test "what it checks" { ... }`, not an attribute on a function:
      Aja has no attributes, and a test is never called, so it needs no
      function name, only a description, which is what a failure is
      reported by. `test` is a keyword only where a definition could
      start, as `native` is. A test may be declared `-> Result<void, E>`,
      as `main` may, so `?` works in it and an `Err` fails it. It is in
      its module, so it sees private items. Every build checks tests, so
      they can't rot (rule 1), but only `aja test` compiles and runs
      them. The tests of the file given and of the modules it imports
      (not the standard library's) run one by one on the VM, each on a
      fresh one, so no test sees another's state; each failure is reported
      with its assertion and call trace, then a summary. Running none,
      with no tests or a filter that matches none, is a failure, so a typo
      isn't taken for a pass. `std::testing` has `assert_eq` (which shows
      both values), `assert_ne`, and `fail`; they are native, so a
      failure points at the assertion rather than into the library. Still
      to come: running tests in parallel, capturing their output, and doc
      comment examples as tests (see "Documented with examples that run").
   4. **Language server** (`aja lsp`). Diagnostics as you type from the
      checker, then hover (a value's type), go to definition, completion
      of methods and fields, and formatting through the formatter. It needs
      the checker to keep the types and definitions it finds by position,
      and to check a file whose imports are already checked without
      checking them again.
9. **Green threads and structured concurrency** in the VM, first with every
   task on one OS thread (see [the plan](plans/green-threads.md)). A
   library, not syntax: `task::scope(fn(s) { s.spawn(fn() { ... }).join() })`.
   Tasks share no mutable state, since a `var` cell can't reach another task,
   and they communicate over bounded channels. A panic in a task cancels
   its scope and propagates to the parent. A cancelled task stops silently
   at its next safepoint (a call or a loop's back edge). Blocking
   intrinsics run on worker threads so that they block only their task.
   Several OS threads, `select`, and timeouts come after. Started: tasks
   in the VM ✅, `std::task` (scopes, `spawn`, `join`) ✅, a `sleep`
   that sleeps only its task ✅, and `std::time` (`Duration`, `Instant`,
   `sleep_for`, `sleep_until`) ✅. Cancelling a scope, which unwinds its
   tasks through their deferred code, and deadlock reports that say what
   each task waits for ✅. A `var` stays in one task, checked at compile
   time where the code shows it and when spawning otherwise ✅. Bounded
   channels (`std::channel`) ✅. A poller in the scheduler, so a socket
   operation blocks only its task, and `std::net` over TCP ✅ (see
   [plans/net.md](plans/net.md)).
10. **Cranelift AoT backend** producing native binaries, with the same runtime
    semantics as the VM (see [the plan](plans/aot.md)). Started:
    `aja build` compiles the VM's bytecode to native code and links it with
    the VM's heap, collector, and native functions as the runtime ✅;
    guaranteed tail calls ✅; stack overflow as an error ✅; the same
    diagnostics as the VM ✅; tasks, channels, and sockets, each task on a
    machine stack of its own, with the VM's scheduler ✅; the runtime as
    its own crate, built into `aja` ✅. Not yet: values kept in machine
    registers.

## Open questions

- **Sized integers.** `int` is 64-bit. Do sized types (`i8` … `u64`) arrive
  as well, for binary formats and interop, and how do conversions between
  them look without being implicit (rule 4)?
- **Indexing** (decided). `xs[i]` exists for lists and stops the program
  when `i` is out of range, as division by zero does: an index that might be
  out of range is a job for `xs.get(i)`, which returns an `Option`. Lists
  are immutable, so there is no `xs[i] = v`.
- **String interpolation** (decided). Every string literal can hold
  expressions, `"x = {x}"`, with `\{` for a literal brace. A value in one
  must be a `str`, `int`, `float`, or `bool`, or have a method
  `to_str(self) -> str`, which the template calls. That call is the only
  hidden one, and it is acceptable because showing a value is meant to have
  no side effects; with traits, `to_str` becomes the `Show` trait. Lists
  and other generic types can't be shown this way until then, since their
  `to_str` would need their items' types to have one too.
- **`Iter` as a trait.** A `for` loop takes an `Iter<T>`, a struct around a
  closure, or a value with a method `iter(self) -> Iter<T>`, found by name.
  With traits, should the loop take anything implementing an `Iter` trait
  (so iterators can be structs with their own state and no closure), and
  does the `iter` method become an `IntoIter` trait? Programs written today
  would keep working either way.
- **User-defined traits.** Traits start as a fixed built-in set (`Eq`,
  `Hash`, `Ord`), which a type has by its shape. When can programs define
  their own, and can a type then implement `Eq` or `Ord` by hand (say, to
  order a struct by one field), overriding its shape?
- **Traits and generics.** How far do generics go (no higher-kinded types is a
  likely non-goal), and are traits implemented nominally or structurally?
  A type parameter can have bounds (`fn max<T: Ord>`) from the built-in
  traits.
- **Tuples** (decided). `(int, str)` is a type and `(1, "a")` a value,
  with two or more parts: `(x)` is just `x`, `(x,)` is an error, and
  `()` is not a value (see "A `void` value"). A part is read by position,
  `t.0`, or with a pattern, and tuples compare part by part with `==`.
  Anything whose parts deserve names should be a struct. With tuples,
  `let` and `for` take any pattern that matches every value of its type
  (`let (q, r) = divmod(a, b);`, `let Point { x, y } = p;`,
  `for (k, v) in pairs`), checked by the same usefulness algorithm as
  `match`; one that might not match is an error that points to `match`.
  `var` still declares a single name, since names bound by patterns are
  immutable (rule 5).
- **Closure parameter types from context.** An unannotated closure
  parameter takes its type from where the closure is passed or bound
  (a parameter of a function or method, or an annotated `let`) before
  its body is checked, so `pairs.map(fn(p) { p.0 })` needs no annotation.
  This is the only place inference uses an expected type; everywhere
  else it is plain unification.
- **Explicit type arguments.** Type arguments are always inferred, and an
  annotation fixes one inference can't. Is a call-site syntax
  (`none::<int>()`) ever needed, and does it fit "one way to do it"?
- **Methods on built-in types** (decided). The prelude has `impl str`,
  `impl int`, `impl float`, and `impl bool`; it counts as where those types
  are defined, so no other module can add to them.
- **A `void` value** (decided). The type's one value is written `void`,
  the same as the type: `Ok(void)` for a `Result<void, E>`, which could
  otherwise only be written by wrapping a call (`Ok(write(...))`). Not
  `()`, which would give one idea two names and look like a function
  type's empty parameter list. `void` is `Eq`, `Hash`, and `Ord`, can't
  name anything else, and is also a pattern.
- **`defer`** (decided). `defer expr;` or `defer { ... }` runs code when
  the block it is in is left, by its end, `return`, `?`, `break`, or
  `continue`, last first. Block-scoped, as in Swift and Zig, not
  function-scoped as in Go: it is settled at compile time (each way out of
  a block compiles the pending code again, so there is no list at run
  time), and a `defer` in a loop body runs each iteration instead of piling
  up. The code sees the names in scope where it is written and reads a
  `var` when it runs. It can't `return`, `?`, `break`, or `continue` out of
  itself. It is visible where it takes effect, so it fits rule 4. It
  doesn't run when a runtime error stops the program, but does when a task
  is cancelled (the compiler gives each function cleanup blocks for that),
  and deferred code that is running when the cancel comes runs to its end
  first. A call returned while code is deferred isn't a tail call.
- **Strings** (decided for now). UTF-8 everywhere. Positions and lengths
  count code points, which matches what people mean by "the third
  character" more often than bytes do; `byte_len` gives the size. There is
  no `s[i]`: `slice`, `find`, and `chars` cover it. Whether to move to
  grapheme clusters, or to give `slice` byte positions for speed, is open;
  a string type that remembers where its characters start would make
  positions cheap either way.
