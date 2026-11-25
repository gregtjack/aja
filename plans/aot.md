# Plan: ahead-of-time compilation with Cranelift

Roadmap item 10. `aja build main.aja` compiles a program to a native
executable that does what `aja main.aja` does on the VM. This plan covers
the first two milestones, which are done, and what the later ones need.

## Decisions

| Question | Decision |
|---|---|
| What is compiled | **The VM's bytecode.** It is already typed and register-based, and the checker and bytecode compiler stay the one front end, so the two backends can't disagree about what a program means. Each bytecode function becomes one native function, translated an instruction at a time. |
| Values | **The VM's, for now.** A register is a tagged `Value` in memory, in one array per task, as in the VM. Compiled code does int, float, and bool instructions, moves, and jumps itself, and calls the runtime for the rest. Keeping values in machine registers, untagged, is a later milestone (see [Speed](#3-speed)). |
| Runtime | **`aja`'s own library.** The heap, the collector, lists, maps, strings, and every native function are the VM's code, built as a static library (`libaja.a`) that the program is linked with. Only the instruction loop is replaced. |
| Calls | **Native calls.** A function takes the runtime and the address of its register window, and returns a status. Functions use Cranelift's `tail` calling convention, and a bytecode `TailCall` is a native tail call, so tail recursion runs in constant space, as the language promises. A call of a top-level function by name is a direct call. |
| GC roots | **The register arrays.** Every live value is in a register below the end of the window its task is in, a global, or a literal, exactly as in the VM, so the collector needs no stack maps yet. They come when values leave memory. |
| Errors | **A status, not unwinding.** A function that fails returns 1 to its caller, which adds its call to the error's trace and returns 1 in turn. Nothing is paid until an error happens. The binary holds the program's source, so the message is the one the VM prints: snippet, trace, and all. |
| Tasks | **A machine stack each, and the VM's scheduler.** A task is its registers plus a machine stack that its compiled code runs on, switched with the `corosensei` crate. `vm/scheduler.rs` is generic over what a task's stack is, and the intrinsics (`vm/intrinsics.rs`) over what runs them, so which task runs next, channels, timers, sockets, and cancellation are one piece of code for both backends. |
| Stack overflow | **An error, never a crash.** Each task gets the VM's 256 MB of registers and a 256 MB machine stack (only what is touched is backed by memory), and each function checks both on entry. |
| Linking | **The system's `cc`.** `aja build` writes an object file and links it with the runtime. The runtime is found next to the `aja` executable, or where `AJA_RUNTIME_LIB` says. |
| Target | **The machine `aja` runs on.** Cross-compiling comes later. |

## How a compiled program is laid out

```
main.o  (from Cranelift)                  libaja.a  (the runtime)
  aja_fn_0 … aja_fn_N   the functions       aja_rt_main     start the program
  aja_code              their addresses     aja_rt_call     find or run a callee
  aja_meta              metadata            aja_rt_new_data, aja_rt_index, …
  aja_enter             runtime → code      the heap, collector, native functions
  main                  calls aja_rt_main
```

The metadata (`src/aot/meta.rs`) is what the runtime needs besides code:
string literals, struct and enum layouts, which native function belongs in
which global, each function's name and signature, and, for error messages,
the places in the source that can fail and the source files themselves.

Registers are an array of 32-byte `Value`s per task. A call's window starts at its
first argument, so arguments are never copied; the slot just below it holds
the closure being called, and the callee leaves its result there. `Value`
is `repr(u8)`, so that compiled code and Rust agree on where a value's
parts are (`src/aot/layout.rs`, checked by tests).

A `Fn` value uses 8 of its 32 bytes, so the slot of a function being called
has room for the call stack: which call put it there, the caller's window,
and the tail call that has replaced it since, if any. The runtime walks
these for error traces and to say where each task of a deadlock waits, so
compiled code keeps no separate list of frames.

## Milestones

### 1. Programs with one task ✅

All of the language, the
prelude, `std::collections`, `std::io`, `std::fs`, `std::env`, `std::time`.
`tests/aot.rs` builds the examples and checks that each prints the same
output, the same errors, and exits with the same status as on the VM.

With a release build of `aja`, on an M-series Mac:

| | VM | Native |
|---|---|---|
| `fib(35)` | 688 ms | 150 ms |
| 50 million loop iterations of int arithmetic | 912 ms | 224 ms |
| 3 million structs made and read | 129 ms | 77 ms |
| A million `push`es, then a loop over the list | 227 ms | 220 ms |

Calls and arithmetic are four to five times faster. Code that spends its
time in the runtime is no faster, since the runtime is the same.

`aja build` takes about half a second, nearly all of it linking, and
"hello world" is a 1 MB executable that needs nothing installed.

### 2. Tasks ✅

`std::task`, `std::channel`, `std::net`, `std::http`, and a `sleep` that
sleeps only its task.

- **Switching.** The runtime's loop resumes the running task's stack; the
  task switches back when it waits, when its turn ends, or when its
  function returns, and the scheduler says who is next. A task only
  switches inside a runtime function, never between its own instructions.
- **Waiting.** An intrinsic that must wait switches away from inside the
  call, and runs again when the task is woken, as the VM reruns the call
  instruction.
- **Safepoints.** At a function's start and at each jump back to a loop's
  start, compiled code counts down the task's turn and calls the runtime
  when it runs out. They are the VM's safepoints, counted the same way, so
  tasks take turns at the same places and a program prints the same thing
  on both.
- **Cancellation.** A cancelled task returns a third status from each
  frame. Where deferred code is pending, the call site branches to the
  function's cleanup block (the bytecode compiler already emits them),
  which runs it and passes the status on. A task in deferred code finishes
  it first: the runtime knows from a flag on the safepoint or call, and a
  count of calls that deferred code has in progress.
- **Errors** in a task stop the program at once, with where the task was
  spawned; a deadlock says what each task waits for and where.

`AJA_STRESS=1` makes a compiled program end every turn at its first
safepoint and collect at every allocation, as the VM's tests do. The tests
run examples that way too.

Starting 20,000 tasks that each sleep takes 0.7 s and 143 MB (about 7 KB a
task), against 0.03 s on the VM: each task maps and unmaps its registers
and stack. Keeping finished tasks' stacks for new ones would close most of
that gap.

Several OS threads can come after, as for the VM.

### 3. Speed

- **Unboxed scalars.** Keep ints, floats, and bools in machine registers.
  The bytecode doesn't say what type a register holds, only what each
  instruction's operands are, so this needs the typed IR that DESIGN.md
  anticipates, or register types added to `Proto`. Values that are held
  across a call that can collect must be findable: either they stay in the
  register array at those points, or Cranelift's stack maps describe them.
- **Smaller values.** `Value` is 32 bytes only because `Native` holds a
  name and a function pointer; pointing at a static pair makes it 16,
  which also speeds up the VM.
- **Fewer runtime calls.** Field reads, tag tests, and calls of closures
  can be done in compiled code once the heap's layout is fixed for it.

### 4. Distribution

- **The runtime as its own crate**, without the compiler in it, built for
  release, and shipped with or embedded in `aja`. Today `libaja.a` is the
  whole of `aja`, and the linker drops what the program doesn't reach.
- **Cross-compilation**, and `aja.toml` settings for it.

## Open questions

- **Source in the binary.** Error messages show the program's source, so
  the binary carries it, the standard library's modules included. Should a
  release build leave it out and print only file, line, and column?
- **Debug information.** Should `aja build` emit DWARF, so debuggers and
  profilers can show Aja functions and lines?
- **Task limits.** Each task reserves 512 MB of address space, which the
  system caps at some tens of thousands of tasks. Smaller reservations
  that grow, or segmented registers, would lift that.
- **The linker.** Needing a C toolchain to build is at odds with "nothing
  to install". Linking in-process, or shipping a linker, would remove it.
- **Stack overflow depth.** The VM counts its frames against the 256 MB;
  compiled code only its registers, so it gets about a third deeper before
  the error. Should the two agree exactly?
