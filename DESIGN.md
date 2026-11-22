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
- **A separate closure syntax.** A closure is an anonymous `fn` with the same
  parameters, return type, and block body as a named function:
  `fn(x) { x * 2 }`. No `|x| x * 2`, no `x => x * 2`, and no brace-less
  bodies. Requiring braces also removes any ambiguity between a return type
  and the start of the body. (Rule 10)
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

## Roadmap

1. **Static type checker.** ✅ Annotations are required on function
   signatures, local types are inferred by unification (without
   let-polymorphism), and `any` is gone.
2. **Data types.** Structs ✅, enums with data ✅, and exhaustive `match` ✅
   (checked with Maranget's usefulness algorithm, which also rejects
   unreachable arms). Struct and enum values are immutable and compare
   structurally with `==`. Next: generics, which `Option<T>`, `Result<T, E>`,
   and `?` need.
3. **Bytecode VM.** ✅ The type-checked AST compiles to typed, register-based
   bytecode, and the tree-walking interpreter is gone. The checker hands the
   compiler operand types rather than a full typed IR; a typed IR can come
   when data types need it.
4. **Memory management.** ✅ Closures, captured variables, struct and enum
   values, and strings live on a garbage-collected heap with a precise
   mark-and-sweep collector, which runs when the heap's size in bytes has
   doubled. String literals are allocated once and kept as roots; strings
   built at runtime (with `+`) are collected. Nothing is reference-counted.
5. **Tooling.** Formatter, JSON diagnostics, and a test runner; then a language
   server.
6. **Green threads and structured concurrency** in the VM.
7. **Cranelift AoT backend** producing native binaries, with the same runtime
   semantics as the VM.

## Open questions

- **Integer width.** `int` is currently 32-bit. Should it be 64-bit by
  default, with sized types (`i8` … `u64`) available explicitly?
- **Traits and generics.** How far do generics go (no higher-kinded types is a
  likely non-goal), and are traits implemented nominally or structurally?
- **Strings.** UTF-8 everywhere, and is indexing by byte, by code point, or not
  allowed at all?
