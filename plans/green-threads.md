# Plan: green threads and structured concurrency in the VM

Roadmap item 9. This plan covers the first milestone, in which every task
runs on one OS thread (M:1). It settles what concurrency means in Aja:
spawning, scopes, cancellation, panics, channels, and blocking I/O. Running
tasks on several OS threads (M:N) is a later milestone, and nothing here
should get in its way.

## Decisions

| Question | Decision |
|---|---|
| OS threads | **M:1.** All tasks run on the VM's thread. There is no parallelism yet. `Rc`, `&mut Heap`, and the stop-the-world GC stay as they are. |
| Shared mutable state | **None.** A `var` cell must never reach a second task. The checker rejects the cases it can see, and spawn and send check a per-value bit at runtime (see [Keeping `var` cells in one task](#keeping-var-cells-in-one-task)). Tasks share immutable data and communicate over channels. |
| Surface | **A library, no new syntax.** `std::task::scope(fn(s) { ... })`, `s.spawn(fn() { ... })` returns a `Task<T>`, and `t.join()` returns `T`. |
| A child's `Err` | **Just a value.** `join` returns it like any other value. Only a panic cancels anything. An errgroup-style helper can be written later, as a library function. |
| A child's panic | **Cancels the scope and propagates.** The panicking task's siblings are cancelled, and once they have stopped the scope panics in its parent, so the panic reaches `main` and the program fails. The diagnostic shows the child's trace and where it was spawned. |
| Cancellation | **A silent stop.** At its next safepoint, a cancelled task's frames are dropped and none of its code runs again. Nothing needs to unwind, because Aja has no destructors or `defer`. |
| Blocking intrinsics | **Run on worker threads.** The task that called one is parked, and the others keep running. A kqueue/epoll poller comes with `std::net`. |
| Primitives | **Bounded channels, and `sleep`.** The built-in `sleep(ms)` sleeps only its task (added after step 2). `select` and timeouts come later. |

## The surface

```aja
import std::task;
import std::channel;

fn main() {
    let total = task::scope(fn(s) {
        let a = s.spawn(fn() { fib(30) });
        let b = s.spawn(fn() { fib(31) });
        a.join() + b.join()
    });
    println("{total}");

    let (tx, rx) = channel::new(16);
    task::scope(fn(s) {
        s.spawn(fn() {
            for i in range(0, 100) { tx.send(i); }
            tx.close();
        });
        for v in rx { println("{v}"); }
    });
}
```

`std::task`:

```aja
pub native struct Scope;
pub native struct Task<T>;

// Run `body` with a new scope. Returns once `body` has returned and every
// task spawned in the scope has finished.
pub native fn scope<T>(body: (Scope) -> T) -> T;

impl Scope {
    // Start `f` as a new task in this scope. Panics if the scope has ended,
    // which can only happen if `s` was kept after `scope` returned.
    pub native fn spawn<T>(self, f: () -> T) -> Task<T>;
}

impl Task<T> {
    // Wait for the task and return its result. Joining twice returns the
    // same value.
    pub native fn join(self) -> T;
}
```

`std::channel`:

```aja
pub native struct Sender<T>;
pub native struct Receiver<T>;

// A channel holding up to `capacity` values. With capacity 0, each send
// waits for a receiver.
pub native fn new<T>(capacity: int) -> (Sender<T>, Receiver<T>);

impl Sender<T> {
    pub native fn send(self, value: T) -> Result<void, Closed>;
    pub native fn close(self);
}

impl Receiver<T> {
    // The next value, or None once the channel is closed and empty.
    pub native fn recv(self) -> Option<T>;
    pub fn iter(self) -> Iter<T> { ... }   // so `for v in rx` works
}
```

Some details still to settle while writing these, none of which change the
design:

- Whether `close` is explicit or happens once every `Sender` is unreachable.
  Explicit is easier to reason about, and the GC would make it timing-dependent,
  so explicit.
- Whether a send on a closed channel panics or returns `Err`. It is
  recoverable for the sender, so `Err`.

## Runtime design

### Tasks and switching

`Vm` keeps one task's state in `regs` and `frames` today
([src/vm/mod.rs](../src/vm/mod.rs)). That state moves into a task:

```rust
struct Task {
    id: TaskId,
    regs: Vec<Value>,
    frames: Vec<Frame>,
    state: State,        // Runnable | Blocked(Reason) | Done(Value) | Cancelled
    scope: ScopeId,      // the scope it was spawned in
    status: (i64, String),
    spawned_at: Option<Span>,
}
```

The running task's `regs` and `frames` stay in `Vm::regs` and `Vm::frames`,
so the hot loop doesn't change. A switch saves `ip` into the top frame,
swaps both vectors with the next task's (`mem::swap`, three pointers each),
and reloads `execute`'s cached `closure`, `proto`, `base`, and `ip`.
Natives never call back into Aja, so the Rust stack never holds Aja state
and a switch can happen at any instruction boundary.

`execute` stops returning when the outermost frame returns. It returns when
the root task does. A task whose outermost frame returns becomes
`Done(value)`, and its joiners wake.

### Scheduling and safepoints

- A FIFO run queue of `TaskId`s. The order is deterministic, so programs and
  tests behave the same on every run.
- **Preemption by reduction count**, as in Erlang. A per-slice budget, about
  2,000 to start, is decremented at `Call`, `TailCall`, and backward `Jump`s.
  When it reaches zero the task goes to the back of the queue. Every loop and
  every recursion passes one of these points, so a CPU-bound task can't
  starve the others, and a cancelled task is stopped promptly.
- Blocking operations (`join`, `send`, `recv`, `scope` waiting for its
  children, offloaded intrinsics) are also safepoints. They park the task
  and switch.
- **A stress mode**, like `Heap::stress`, switches at every safepoint. The
  test suite runs the concurrency tests under it to flush out ordering
  bugs.

### Scheduler intrinsics

`NativeFn` (`fn(&mut Heap, &[Value]) -> Result<Value, String>`) can't park
a task or start one. The task and channel functions become a second kind of
native that the VM handles itself, returning
`Done(Value) | Block(Reason) | Spawn(Gc) | Error(String)`. Implementing them
as `Op`s is the alternative, but natives keep the bytecode small and match
how `List`'s methods work. They take closures and generic values, which the
intrinsic rule (scalars, `str`, and lists only) doesn't allow. That's fine:
like `List`'s methods, they are part of the runtime, not of `std::native`.
DESIGN should say that a backend implements this set too.

### Heap objects and GC roots

New `Obj` variants: `Scope`, `TaskHandle` (a `TaskId` plus, once the task is
done, its result), and `Channel` (a bounded `VecDeque<Value>`, the parked
senders and receivers, and a closed flag), with `Value` variants to match.
Roots become every task's live registers and frame closures, as
`collect_garbage_with` already computes for one task, plus the
`Done(value)` results, the values in channel buffers (traced through the
`Channel` object), and values held by tasks parked in `send`. An
allocation can still collect only at a point where every live value is in a
register, so the invariant the code relies on today holds.

### Scopes, panics, and cancellation

- `scope(body)` creates a `Scope`, runs `body` in the calling task, then
  blocks until every child is `Done` or `Cancelled`. Afterwards the scope is
  marked ended, and `spawn` on it panics.
- The root task is `main`'s. Tasks can only be spawned inside a scope, so
  every task has a parent that outlives it.
- **A panic in a child**: record the `RuntimeError` on the scope, cancel
  every other task in it (recursively, through any scopes they opened), and
  wake the parent. When the parent's `scope` call returns, it raises a
  `RuntimeError` whose message and trace are the child's, with a note
  saying where the child was spawned. A panic in the parent's own `body`
  cancels the children too, before propagating.
- **Cancelling** sets a flag. At its next safepoint the task's frames are
  cleared and it becomes `Cancelled`. A task parked in `recv` or `send` is
  removed from the channel's wait list. A task parked in an offloaded
  intrinsic can't be interrupted, so the scope waits for that call to finish
  and throws its result away. That way no side effect from a scope's tasks
  happens after the scope has returned.
- **Deadlock**: if no task can run and none is waiting on a worker thread,
  the program stops with a runtime error that lists each blocked task, what
  it is waiting on, and where it was spawned, as Go does. Without this, a
  missing `close` would hang forever.

### Blocking intrinsics on worker threads

- Each entry in the native table gains a `blocking: bool`. That is true for
  `read_line`, `read_all`, the file reads, `write_file`, `read_dir`, and the
  directory operations. `write` and `flush` stay inline: they are buffered
  and usually fast.
- A blocking call copies its arguments into owned Rust values (they are
  scalars, `str`, and lists of those, so this is always possible), sends
  them to a small worker pool (`std::thread` plus an mpsc channel), and
  parks the task. The VM thread waits on a completion channel whenever the
  run queue is empty.
- The status moves from the `thread_local!` in
  [src/vm/native.rs](../src/vm/native.rs) to `Task::status`. A worker
  returns `(result, status)`, and the VM stores the status on the task, so
  `std::native::status()` reads the calling task's own status, as DESIGN
  requires.

## Keeping `var` cells in one task

**Static check.** The checker gets its own capture analysis. Today only the
compiler computes captures, in `Capture`
([src/vm/compile.rs](../src/vm/compile.rs)). A closure literal is *local*
if it captures a `var`, or captures a `let` bound to a local closure, struct,
or list in the same function. The checker reports an error when a local
value is passed to `Scope::spawn` or `Sender::send`, with a span on the
captured `var` and help such as "send the value over a channel instead, or
bind it with `let` before spawning". The two functions are marked by name
for now. With traits, the mark becomes a bound.

**Runtime check.** Each `Closure`, `Data`, and `List` object gets a
`reaches_cell` bit, computed when it is allocated: it is set if any field or
upvalue is a `Cell` or a value with the bit set. Values never change after
they are made, so the bit stays correct, and computing it costs O(fields),
which allocation already pays. `spawn` and `send` panic in O(1) if it is
set. This catches what the checker can't see, such as a closure passed in as
a parameter or pulled from a list.

**Later.** A built-in `Share` trait (Rust's `Send`), derived for every type
and missing from function types that might capture a `var`. That makes the
indirect cases compile errors too, and the runtime check becomes a debug
assertion. It belongs with the open questions on traits.

## Steps

Each step lands on its own with tests, in order:

1. **Task-shaped VM.** Move `regs`, `frames`, and the status into `Task`.
   Add the run queue, switching, the reduction budget, and stress mode.
   Spawn only through a test-only hook. All existing tests pass unchanged. ✅
   A `Scheduler` ([src/vm/scheduler.rs](../src/vm/scheduler.rs)) owns the
   tasks, the run queue, and switching; the `Vm` keeps the running task's
   stack, which the interpreter loop reads directly, and lends it to the
   scheduler to switch. Blocking, scopes, and cancellation go there too. The
   status stays in its `thread_local`, saved into the task when it is
   switched out and restored when it is switched back in. Safepoints come
   after a call or backward jump has run, so a task resumes at the next
   instruction. On a single task they cost nothing measurable.
2. **`std::task`.** `Scope`, `Task<T>`, `scope`, `spawn`, and `join`, with
   the scheduler intrinsics, new heap objects, and GC roots. Waiting for the
   children at the end of a scope, and a panic on spawning into an ended
   scope. ✅ `scope` is written in Aja
   ([src/stdlib/task.aja](../src/stdlib/task.aja)) over two private
   intrinsics, `open_scope` and `close_scope`, so the VM never calls a
   closure from inside an intrinsic. An intrinsic that must wait (`join`,
   `close_scope`) leaves the task's `ip` on its call, which runs again when
   the task is woken. A finished task's result moves into its `Task` on the
   heap, so the collector decides how long it lives, and its slot in the
   scheduler is reused. Deadlock is reported, briefly; step 3 lists the
   waiting tasks. GC stress found a bug older than tasks: a frame's window
   can cover registers that earlier calls left stale handles in, which may
   point at freed objects. Marking now skips those.

   Added after step 2: the built-in `sleep(ms)`, which used to block the
   VM's thread, sleeps only its task. The scheduler keeps timers by
   deadline, wakes the tasks that are due whenever it picks the next task
   (so a sleeper wakes on time even while others compute), and when no
   task can run it sleeps the thread until the next deadline. A sleeping
   task isn't a deadlock. `sleep` runs again when its task wakes, as the
   other waiting intrinsics do, and a flag on the task makes that call
   return. [examples/sleep.aja](../examples/sleep.aja) shows tasks overlap.
3. **Panics and cancellation.** Cancelling a scope, propagating a panic with
   the combined trace, silent stops at safepoints, and deadlock detection.
4. **The `var` rule.** The checker's capture analysis and diagnostic, and
   the `reaches_cell` bit with its runtime check.
5. **`std::channel`.** Bounded channels with capacity 0 as a rendezvous,
   `close`, `recv` returning `None`, `for v in rx`, and parked tasks as GC
   roots.
6. **Blocking intrinsics off-thread.** Worker pool, `blocking` flags, per-task
   status, and waiting for an in-flight call when its task is cancelled.
7. **Documentation.** Examples grow with each step: so far
   [examples/tasks.aja](../examples/tasks.aja) and, in `examples/errors/`,
   `task_failure.aja` and `scope_ended.aja`, which a test also runs with
   switching and collection stress. Channels (step 5) get an example of
   their own, and step 3 adds a deadlock example to `errors/`. DESIGN updates (the M:1
   note in "Concurrency runtime", the scheduler intrinsics as part of a
   backend's set), and doc comments for both modules.

## Tests

- VM unit tests ([src/vm/tests.rs](../src/vm/tests.rs)) for each step, run
  both normally and in switch stress mode combined with `Heap::stress`.
- Ordering tests that rely on the deterministic queue, for example two tasks
  printing alternately through a rendezvous channel.
- Failure tests: a panic in a child, a panic in the parent's body, nested
  scopes, a cancelled task parked in `recv`, a deadlock, a spawn into an
  ended scope, and a leaked `var` through a parameter (the runtime check).
- Checker tests for the `var` rule: a direct capture, a capture through a
  `let` closure, a capture through a struct, and no false positives on `let`
  captures.
- An integration test like [tests/fs.rs](../tests/fs.rs) showing that a task
  blocked on stdin doesn't stop a CPU-bound task.

## Not in this milestone

M:N, `select`, timeouts, a `Duration` for `sleep` (with `std::time`), unbounded
channels, a kqueue/epoll poller, supervision or restarting tasks, and the
`Share` trait.

## Open questions

- **Reduction budget.** Pick a starting value by measuring switch cost
  against latency on `fib` and a channel ping-pong benchmark.
- **Task-local state.** Is anything besides the status per task: the
  working directory, or the stdout buffer? The simple answer is that those
  belong to the process.
- **The REPL.** Is each input its own root task, or can a scope persist
  across inputs? The suggestion: each input is a root task, and a scope
  can't outlive it.
