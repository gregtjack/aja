//! The runtime of a program compiled ahead of time.
//!
//! A native program is its compiled functions plus this code, linked in
//! from `aja`'s library. It shares the VM's heap, garbage collector, values,
//! native functions, and scheduler, so everything they do is the same in
//! both. What it replaces is the VM's instruction loop: compiled code does
//! the simple instructions itself and calls the `aja_rt_*` functions here
//! for the rest.
//!
//! Registers are as in the VM: an array of [`Value`]s per task, each call
//! using a window of it that starts at its first argument. A function is
//! called with the address of its window, `base`; the slot just below it
//! holds the closure being called, and the function leaves its result
//! there. Calls use the machine's stack too, one native frame per Aja call
//! (and none for a tail call).
//!
//! Each task is a [`Fiber`]: its registers, and a machine stack of its own
//! that its compiled code runs on. [`run`] resumes the running task's
//! fiber, which runs until the task waits, ends its turn, or finishes, and
//! then switches back; the scheduler says which task is next. A task only
//! switches away inside a runtime function ([`aja_rt_call`] or
//! [`aja_rt_safepoint`]), never between two of its own instructions.
//!
//! The collector's roots are each task's registers below the end of the
//! window it is in (`top`, which compiled code passes to every function
//! here that allocates or can switch tasks), the globals, and the string
//! literals.

// What the `aja_rt_*` functions need from their caller is the same for all
// of them, and is said once, above the first.
#![allow(clippy::missing_safety_doc)]

use std::{
    collections::HashMap,
    ffi::{c_char, CStr},
    io::Write,
    mem::size_of,
    rc::Rc,
};

use corosensei::{
    stack::{DefaultStack, Stack as _},
    Coroutine, CoroutineResult, Yielder,
};

use crate::{
    ast::{Id, Source, Span},
    builtins,
    bytecode::{Capture, Layout, Proto},
    diagnostic::{self, Files},
    heap::{Closure, Gc, Heap, MAX_STR_LEN},
    internal_error, intrinsics, layout,
    meta::Meta,
    native,
    scheduler::{Intrinsic, Next, Outcome, Scheduler, TaskStack},
    value::Value,
    Note, RuntimeError, TraceEntry, DEFAULT_STACK_LIMIT,
};

/// How much of the machine's stack each task gets. Only what is used is
/// ever backed by memory.
const MACHINE_STACK: usize = 256 << 20;
/// How much of its machine stack a task's compiled code leaves free, for
/// the runtime functions it calls.
const MACHINE_STACK_MARGIN: usize = 1 << 20;

/// Why compiled code is stopping the program, for [`aja_rt_error`].
pub const ERR_ADD: i64 = 0;
pub const ERR_SUB: i64 = 1;
pub const ERR_MUL: i64 = 2;
pub const ERR_DIV: i64 = 3;
pub const ERR_NEG: i64 = 4;
pub const ERR_DIV_ZERO: i64 = 5;
pub const ERR_MOD_ZERO: i64 = 6;
pub const ERR_NO_MATCH: i64 = 7;
pub const ERR_STACK: i64 = 8;

/// Compiled code's entry point: calls the function at `code` with its
/// register window at `base`, and returns its status.
type Enter = extern "C" fn(rt: *mut Rt, base: *mut Value, code: usize) -> i64;

/// Why a task's fiber switched back to [`run`] before finishing.
enum Yield {
    /// The task waits until it is woken.
    Wait,
    /// Its turn is over, and another task wants one.
    EndTurn,
}

/// A task's registers and machine stack.
struct Fiber {
    /// The task's code, running on its own machine stack: it returns the
    /// status of the task's function.
    coroutine: Option<Coroutine<(), Yield, i64>>,
    /// How the code switches back, once it has started.
    yielder: *const Yielder<(), Yield>,
    /// The first register, which holds the task's function.
    regs: *mut Value,
    /// How many tasks have used the registers and stack (see [`Reserved`]).
    uses: u32,
    /// The end of the registers.
    regs_limit: *mut Value,
    /// The lowest address the machine's stack pointer may have when a
    /// function starts.
    stack_limit: usize,
    /// While the task isn't running: the end of the window it is in, so
    /// the registers below it are the collector's roots.
    top: *const Value,
    /// While the task isn't running: how many of its calls in progress
    /// were made by deferred code.
    deferred_calls: i64,
    /// While the task waits: the call of the intrinsic it waits in (a
    /// [`layout::site_note`]), the window of the function making it, and
    /// whether the call is in deferred code.
    wait_note: i64,
    wait_base: *const Value,
    waits_in_deferred: bool,
}

impl Fiber {
    /// How many bytes of registers a task gets: the VM's limit.
    const REGISTERS: usize = DEFAULT_STACK_LIMIT;

    /// The registers that can hold live values.
    fn live(&self) -> &[Value] {
        let count = (self.top as usize - self.regs as usize) / size_of::<Value>();
        // SAFETY: `top` is in the registers. Every register holds a valid
        // value, since they start as `Void` (all zero bytes) and compiled
        // code only writes whole values.
        unsafe { std::slice::from_raw_parts(self.regs, count) }
    }
}

impl Drop for Fiber {
    fn drop(&mut self) {
        let uses = self.uses;
        let reserved = |regs, stack| Reserved { regs, stack, uses };
        match self.coroutine.take() {
            // The task has finished: the next one can use what it had.
            Some(coroutine) if coroutine.done() => {
                reserved(self.regs, Some(coroutine.into_stack())).keep();
            }
            // Dropping code that hasn't finished would unwind its stack,
            // which compiled code can't do, so the stack is left as it is.
            // It only happens as the program ends.
            Some(coroutine) if coroutine.started() => {
                std::mem::forget(coroutine);
                drop(reserved(self.regs, None));
            }
            _ => drop(reserved(self.regs, None)),
        }
    }
}

/// The memory reserved for a task: its registers, and its machine stack
/// unless the task's code still has it.
struct Reserved {
    regs: *mut Value,
    stack: Option<DefaultStack>,
    /// How many tasks have used it since its memory was last given back.
    uses: u32,
}

thread_local! {
    /// What finished tasks had reserved, for new tasks to use: reserving
    /// and releasing address space is most of what a short task costs.
    static SPARE: std::cell::RefCell<Vec<Reserved>> = const { std::cell::RefCell::new(Vec::new()) };
}

impl Reserved {
    /// How many finished tasks' reservations are kept.
    const SPARES: usize = 1024;
    /// How much of a kept reservation's registers, and of its stack, stay
    /// backed by memory when the rest is given back to the system, so
    /// that a task that recursed deeply doesn't hold what it used for
    /// good.
    const KEPT: usize = 256 << 10;
    /// How many tasks use a reservation between giving its memory back:
    /// asking the system to costs more than a short task does, so it
    /// isn't done every time.
    const USES: u32 = 64;

    /// Registers and a machine stack for a new task: a finished task's, or
    /// newly reserved. Registers that were used before hold values that
    /// are no longer live, which a call's window may anyway (see
    /// `Heap::collect`).
    fn take() -> (*mut Value, DefaultStack, u32) {
        if let Some(spare) = SPARE.with(|spare| spare.borrow_mut().pop()) {
            let mut spare = std::mem::ManuallyDrop::new(spare);
            let stack = spare.stack.take().expect("a spare has a stack");
            return (spare.regs, stack, spare.uses);
        }
        // SAFETY: an anonymous mapping, which nothing else refers to.
        // Fresh pages from the system are zero, which is `Void`, and one
        // that is never touched is never backed by memory, so a task costs
        // only what it uses. (The allocator's zeroed memory isn't as cheap
        // for a block this size.)
        let regs = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                Fiber::REGISTERS,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANON,
                -1,
                0,
            )
        };
        if regs == libc::MAP_FAILED {
            panic!(
                "could not reserve registers for a task: {}",
                std::io::Error::last_os_error()
            );
        }
        let stack = DefaultStack::new(MACHINE_STACK)
            .unwrap_or_else(|e| panic!("could not reserve a stack for a task: {e}"));
        (regs.cast(), stack, 0)
    }

    /// Keep the reservation for a new task, if there is room for it.
    fn keep(mut self) {
        let Some(stack) = &self.stack else { return };
        if SPARE.with(|spare| spare.borrow().len()) >= Reserved::SPARES {
            return;
        }
        self.uses += 1;
        if self.uses < Reserved::USES {
            SPARE.with(|spare| spare.borrow_mut().push(self));
            return;
        }
        self.uses = 0;
        // SAFETY: both ranges are inside mappings this owns and nothing is
        // using: the registers past the first `KEPT` bytes, and the stack
        // (which grows down from its base) below its top `KEPT` bytes,
        // above the margin compiled code never reaches into. Giving pages
        // back only makes them zero when next touched.
        unsafe {
            libc::madvise(
                self.regs.cast::<u8>().add(Reserved::KEPT).cast(),
                Fiber::REGISTERS - Reserved::KEPT,
                libc::MADV_DONTNEED,
            );
            let low = stack.limit().get() + MACHINE_STACK_MARGIN;
            libc::madvise(
                low as *mut libc::c_void,
                stack.base().get() - Reserved::KEPT - low,
                libc::MADV_DONTNEED,
            );
        }
        SPARE.with(|spare| spare.borrow_mut().push(self));
    }
}

impl Drop for Reserved {
    fn drop(&mut self) {
        // SAFETY: the registers are a mapping of this size, made by
        // `Reserved::take`, that nothing uses any more.
        unsafe { libc::munmap(self.regs.cast(), Fiber::REGISTERS) };
    }
}

/// A task's call stack, as the scheduler keeps it; empty in the place of
/// the running task's, which is in [`Rt::stack`].
#[derive(Default)]
pub struct Stack(Option<Box<Fiber>>);

impl TaskStack for Stack {
    fn waits_in_deferred(&self) -> bool {
        self.0.as_ref().is_some_and(|f| f.waits_in_deferred)
    }
}

/// The state of a running native program. Compiled code reads and writes
/// the first fields directly (see [`crate::layout`]), so they stay first
/// and in this order.
#[repr(C)]
pub struct Rt {
    /// The end of the running task's registers: a window may not reach
    /// past it.
    regs_limit: *mut Value,
    /// The lowest address the machine's stack pointer may have when a
    /// function starts.
    stack_limit: usize,
    /// The first global.
    globals_ptr: *mut Value,
    /// The first string literal.
    literals_ptr: *mut Value,
    /// How many safepoints the running task has before its turn ends.
    /// Compiled code counts it down at each one, and calls
    /// [`aja_rt_safepoint`] when it runs out.
    budget: i64,
    /// How many of the running task's calls in progress were made by
    /// deferred code, which a cancelled task finishes before it stops.
    deferred_calls: i64,

    heap: Heap,
    /// The running task's stack.
    stack: Stack,
    scheduler: Scheduler<Stack>,
    globals: Vec<Value>,
    literals: Vec<Value>,
    /// The program's functions, in the order of the code table.
    protos: Vec<Rc<Proto>>,
    layouts: Vec<Rc<Layout>>,
    /// Each function's fallible instructions and call names.
    sites: Vec<FnSites>,
    /// The source files of the standard library.
    library: Vec<Source>,
    /// The error that is stopping the program, while compiled code returns
    /// to where it was entered.
    error: Option<RuntimeError>,
    /// The call the last entry of the error's trace is for.
    last_call: Option<(u32, u32)>,
    /// While the running task has been asked to stop but can't yet (it is
    /// in deferred code): `budget` is kept at 1, so that every safepoint
    /// asks again, and this is what it would be otherwise.
    turn_left: Option<i64>,
    /// Whether the running task, which has been asked to stop, has yet to
    /// start: the safepoint at the start of its function isn't one, since
    /// no call led there.
    at_start: bool,
    /// The call of the intrinsic that is running (a [`layout::site_note`]),
    /// and the window of the function making it and that window's end.
    call_note: i64,
    call_base: *const Value,
    call_top: *const Value,
    /// This runtime, for the tasks it starts.
    this: *mut Rt,
    enter: Enter,
}

#[derive(Default)]
struct FnSites {
    spans: HashMap<u32, Span>,
    call_names: HashMap<u32, Id>,
}

impl Rt {
    /// Collect garbage if the heap has grown enough to want it, counting
    /// `extra` bytes about to be allocated. Every live value must be in a
    /// register below `top` or of another task, a global, or a literal.
    fn collect_if_wanted(&mut self, extra: usize, top: *const Value) {
        if !self.heap.wants_collection(extra) {
            return;
        }
        let running = self.stack.0.as_ref().expect("a task is running");
        let live = (top as usize - running.regs as usize) / size_of::<Value>();
        // SAFETY: `top` is the end of a function's window, which the
        // function checked is inside the registers when it started.
        let running = unsafe { std::slice::from_raw_parts(running.regs, live) };
        let others = self
            .scheduler
            .stacks()
            .filter_map(|s| s.0.as_ref())
            .flat_map(|f| f.live());
        self.heap.collect(
            running
                .iter()
                .chain(others)
                .chain(self.scheduler.results())
                .chain(&self.globals)
                .chain(&self.literals),
            self.scheduler.held(),
        );
    }

    fn fail(&mut self, message: String) {
        self.error = Some(RuntimeError {
            message,
            span: None,
            trace: Vec::new(),
            notes: Vec::new(),
        });
        self.last_call = None;
    }

    /// Add a call to the error's trace: the tail call that last replaced
    /// the frame of the callee in `slot`, if one did, as in the VM, and
    /// otherwise `call`, the call that put the callee there.
    ///
    /// # Safety
    ///
    /// `slot` must hold an Aja function that was called.
    unsafe fn trace(&mut self, slot: *const Value, call: Option<(u32, u32)>) {
        let note = slot
            .cast::<u8>()
            .add(layout::TAIL_NOTE_OFFSET as usize)
            .cast::<i64>()
            .read();
        let Some((function, ip)) = layout::site_of(note).or(call) else {
            return;
        };
        let Some(site) = self.span(function, ip) else {
            return;
        };
        let name = self.sites[function as usize].call_names.get(&ip).cloned();
        let repeated = self.last_call == Some((function, ip));
        let Some(error) = &mut self.error else { return };
        if repeated {
            if let Some(last) = error.trace.last_mut() {
                last.count += 1;
                return;
            }
        }
        error.trace.push(TraceEntry {
            function: match name {
                Some(name) => format!("function '{name}'"),
                None => "an anonymous function".to_string(),
            },
            site,
            count: 1,
        });
        self.last_call = Some((function, ip));
    }

    fn span(&self, function: u32, ip: u32) -> Option<Span> {
        self.sites[function as usize].spans.get(&ip).copied()
    }

    /// A stack for a new task that will call `closure`.
    fn fiber(&mut self, closure: Gc) -> Stack {
        let (regs, stack, uses) = Reserved::take();
        let stack_limit = stack.limit().get() + MACHINE_STACK_MARGIN;
        // SAFETY: the registers hold far more than two values.
        let (base, regs_limit) = unsafe {
            regs.write(Value::Fn(closure));
            // No call put the task's function there (see `layout`), and
            // writing a value leaves the bytes it doesn't use unset.
            let words = regs.cast::<u8>();
            for offset in [
                layout::CALL_NOTE_OFFSET,
                layout::CALLER_OFFSET,
                layout::TAIL_NOTE_OFFSET,
            ] {
                words.add(offset as usize).cast::<i64>().write(0);
            }
            (regs.add(1), regs.add(Fiber::REGISTERS / size_of::<Value>()))
        };
        let mut fiber = Box::new(Fiber {
            coroutine: None,
            yielder: std::ptr::null(),
            regs,
            uses,
            regs_limit,
            stack_limit,
            top: base,
            deferred_calls: 0,
            wait_note: 0,
            wait_base: std::ptr::null(),
            waits_in_deferred: false,
        });
        let (rt, enter) = (self.this, self.enter);
        let code = self.heap.closure(closure).proto.native;
        let this: *mut Fiber = &mut *fiber;
        fiber.coroutine = Some(Coroutine::with_stack(stack, move |yielder, ()| {
            // SAFETY: the fiber outlives its code, and nothing else uses
            // it while its code runs.
            unsafe { (*this).yielder = yielder };
            enter(rt, base, code)
        }));
        Stack(Some(fiber))
    }

    /// Give the running task a new turn: point compiled code at its
    /// registers and stack, and start its budget.
    fn begin_turn(&mut self) {
        let fiber = self.stack.0.as_ref().expect("a task is running");
        self.regs_limit = fiber.regs_limit;
        self.stack_limit = fiber.stack_limit;
        self.deferred_calls = fiber.deferred_calls;
        // The safepoint at the start of a task's function isn't one of
        // its turn's, since no call led there.
        let starting = !fiber.coroutine.as_ref().is_some_and(|c| c.started());
        self.budget = self.scheduler.slice() as i64 + starting as i64;
        self.turn_left = None;
        self.at_start = false;
        if self.scheduler.cancel_requested() {
            self.ask_at_every_safepoint();
            self.at_start = starting;
        }
    }

    /// The running task has been asked to stop: make its next safepoint
    /// call the runtime, whatever is left of its turn.
    fn ask_at_every_safepoint(&mut self) {
        if self.turn_left.is_none() {
            self.turn_left = Some(self.budget);
            self.budget = 1;
        }
    }

    /// If the running task has been asked to stop, and isn't in deferred
    /// code, which always runs to its end, start stopping it: returns
    /// whether it did. `in_deferred` is whether the instruction asking is
    /// in deferred code.
    fn stopped(&mut self, in_deferred: bool) -> bool {
        if !self.scheduler.cancel_requested() {
            return false;
        }
        if in_deferred || self.deferred_calls > 0 {
            self.ask_at_every_safepoint();
            return false;
        }
        self.scheduler.start_unwinding();
        if let Some(left) = self.turn_left.take() {
            self.budget = left;
        }
        true
    }

    /// The calls in progress, innermost first, as function number and
    /// instruction, starting from the call `note` is of, which a function
    /// whose window is `base` is making.
    ///
    /// # Safety
    ///
    /// The task making the call must not have returned from it.
    unsafe fn calls(&self, mut note: i64, mut base: *const Value) -> Vec<(u32, u32)> {
        let word = |slot: *const Value, offset: i32| {
            slot.cast::<u8>().add(offset as usize).cast::<i64>().read()
        };
        let mut calls = Vec::new();
        while let Some(call) = layout::site_of(note) {
            calls.push(call);
            // Beside the function making the call is the call of it.
            let slot = base.sub(1);
            note = word(slot, layout::CALL_NOTE_OFFSET);
            base = word(slot, layout::CALLER_OFFSET) as *const Value;
        }
        calls
    }

    /// Where a waiting task is waiting, in the program's own code: the
    /// call it waits in, or the call into the standard library that led
    /// there.
    fn waiting_at(&self, fiber: &Fiber) -> Option<Span> {
        // SAFETY: a waiting task is still in the call it waits in.
        let calls = unsafe { self.calls(fiber.wait_note, fiber.wait_base) };
        let spans: Vec<Span> = calls
            .into_iter()
            .filter_map(|(function, ip)| self.span(function, ip))
            .collect();
        let mine = |s: &&Span| {
            !matches!(s.start.source, Source::Prelude) && !self.library.contains(&s.start.source)
        };
        spans.iter().find(mine).or(spans.first()).copied()
    }

    /// The error when every task is waiting for another, found while the
    /// running task waits (or finishes, if `finished`), as the VM reports
    /// it.
    fn deadlock(&self, finished: bool) -> RuntimeError {
        let mut notes = Vec::new();
        let mut describe = |what: &str, origin: &[Span], stack: &Stack| {
            let (who, mut places) = match origin.last() {
                Some(&at) => ("the task spawned at {}", vec![at]),
                None => ("the main task", vec![]),
            };
            match stack.0.as_ref().and_then(|f| self.waiting_at(f)) {
                Some(at) => {
                    places.push(at);
                    notes.push(Note {
                        text: format!("{who} is {what}, at {{}}"),
                        places,
                    });
                }
                None => notes.push(Note {
                    text: format!("{who} is {what}"),
                    places,
                }),
            }
        };
        if !finished {
            describe(
                self.scheduler.current_wait(),
                &self.scheduler.origin(),
                &self.stack,
            );
        }
        for (what, origin, stack) in self.scheduler.waiting() {
            describe(what, origin, stack);
        }
        RuntimeError {
            message: "deadlock: every task is waiting for another task, so none can go on"
                .to_string(),
            span: None,
            trace: Vec::new(),
            notes,
        }
    }
}

impl intrinsics::Host for Rt {
    type Stack = Stack;

    fn parts(&mut self) -> (&mut Heap, &mut Scheduler<Stack>) {
        (&mut self.heap, &mut self.scheduler)
    }

    fn collect_if_wanted(&mut self) {
        self.collect_if_wanted(0, self.call_top);
    }

    fn call_site(&self) -> Span {
        // SAFETY: the intrinsic's call is in progress.
        let calls = unsafe { self.calls(self.call_note, self.call_base) };
        calls
            .first()
            .and_then(|&(function, ip)| self.span(function, ip))
            .expect("compiled code notes every call")
    }

    fn new_stack(&mut self, closure: Gc) -> Stack {
        self.fiber(closure)
    }
}

// ---- Called by compiled code ------------------------------------------------
//
// Each takes the runtime and pointers to the registers it reads and writes.
// One that can fail returns a status (see `layout`); one that allocates, or
// can switch tasks, takes `top`.
//
// SAFETY, for all of them: compiled code passes the runtime it was entered
// with, and pointers into the registers of the running function's window
// (or its closure's slot, just below it), which no Rust reference covers.
//
// The two that can switch tasks take the runtime as a pointer and borrow it
// only between switches: while a task is switched away, the others use the
// runtime, so no reference to it may be held across a switch.

/// Start stopping the program for the reason `code`, found at instruction
/// `ip` of `function` (`u32::MAX` for none: the call that led here is
/// reported instead).
#[no_mangle]
pub unsafe extern "C" fn aja_rt_error(rt: &mut Rt, code: i64, function: u32, ip: u32) {
    let message = match code {
        ERR_ADD => "integer overflow in '+'".to_string(),
        ERR_SUB => "integer overflow in '-'".to_string(),
        ERR_MUL => "integer overflow in '*'".to_string(),
        ERR_DIV => "integer overflow in '/'".to_string(),
        ERR_NEG => "integer overflow in '-'".to_string(),
        ERR_DIV_ZERO => "division by zero".to_string(),
        ERR_MOD_ZERO => "modulo by zero".to_string(),
        ERR_NO_MATCH => "internal error: no arm of this match matched".to_string(),
        ERR_STACK => format!(
            "stack overflow: the call stack has grown past {} MB",
            DEFAULT_STACK_LIMIT >> 20
        ),
        _ => panic!("internal error: unknown error code {code}"),
    };
    rt.fail(message);
    aja_rt_at(rt, function, ip);
}

/// Say where the error a runtime function just reported happened.
#[no_mangle]
pub unsafe extern "C" fn aja_rt_at(rt: &mut Rt, function: u32, ip: u32) {
    let span = rt.span(function, ip);
    if let Some(error) = &mut rt.error {
        error.span = error.span.or(span);
    }
}

/// Add the call at instruction `ip` of `function`, which the error is
/// passing through, to the error's trace. `slot` is where the callee was
/// put: if a tail call replaced the frame since, that call is what the
/// trace shows instead, as in the VM.
#[no_mangle]
pub unsafe extern "C" fn aja_rt_trace(rt: &mut Rt, function: u32, ip: u32, slot: *const Value) {
    // An error with no place yet came from starting the call itself.
    if let Some(error) = &mut rt.error {
        if error.span.is_none() {
            error.span = rt.sites[function as usize].spans.get(&ip).copied();
            return;
        }
    }
    rt.trace(slot, Some((function, ip)));
}

/// Switch from the running task back to [`run`], which runs the others
/// until this one's next turn. `top` is the end of the window the task is
/// in.
unsafe fn switch_away(rt: *mut Rt, why: Yield, top: *const Value) {
    let fiber: *mut Fiber = &mut **(*rt).stack.0.as_mut().expect("a task is running");
    (*fiber).top = top;
    (*fiber).deferred_calls = (*rt).deferred_calls;
    (*(*fiber).yielder).suspend(why);
}

/// A safepoint whose count has run out: stop the running task if it has
/// been cancelled, or else end its turn if another task wants one.
unsafe fn safepoint(rt: *mut Rt, top: *const Value, in_deferred: bool) -> i64 {
    {
        let rt = &mut *rt;
        if std::mem::take(&mut rt.at_start) {
            rt.turn_left = rt.turn_left.map(|left| left - 1);
            rt.budget = 1;
            return layout::STATUS_OK;
        }
        // The count ran out for real, or was kept at 1 to get here.
        let before = rt.turn_left.unwrap_or(1);
        if rt.stopped(in_deferred) {
            rt.budget = rt.budget.max(1);
            return layout::STATUS_STOPPED;
        }
        let asked = rt.turn_left.take().is_some();
        if before > 1 {
            rt.budget = before - 1;
            if asked {
                rt.ask_at_every_safepoint();
            }
            return layout::STATUS_OK;
        }
        if !rt.scheduler.turn_is_wanted() {
            rt.budget = rt.scheduler.slice() as i64;
            if asked {
                rt.ask_at_every_safepoint();
            }
            return layout::STATUS_OK;
        }
    }
    // The next turn starts with a new budget.
    switch_away(rt, Yield::EndTurn, top);
    layout::STATUS_OK
}

/// What compiled code calls at a safepoint (a function's start, or a jump
/// back to a loop's start) once the running task's budget has run out.
/// `flags` has [`layout::IN_DEFERRED`] set if the safepoint is in deferred
/// code. Returns a status: OK, or stopped if the task was cancelled.
#[no_mangle]
pub unsafe extern "C" fn aja_rt_safepoint(rt: *mut Rt, top: *const Value, flags: u64) -> i64 {
    safepoint(rt, top, flags as i64 & layout::IN_DEFERRED != 0)
}

/// Start a call of the value in `slot`, whose arguments are in the
/// registers after it; `argc` is how many, with [`layout::IN_DEFERRED`]
/// set if the call is in deferred code and [`layout::IS_TAIL_CALL`] if it
/// is a tail call. `note` is the call (a [`layout::site_note`]), and
/// `base` the window of the function making it. For an Aja function,
/// notes the call beside it (see [`layout`]) and returns the address of
/// its code, for the caller to call. A native function is run
/// here: returns a status, with its result in `slot` if that is OK.
#[no_mangle]
pub unsafe extern "C" fn aja_rt_call(
    rt: *mut Rt,
    slot: *mut Value,
    argc: u64,
    top: *const Value,
    note: i64,
    base: *const Value,
) -> usize {
    let in_deferred = argc as i64 & layout::IN_DEFERRED != 0;
    let is_tail_call = argc as i64 & layout::IS_TAIL_CALL != 0;
    let argc = (argc & 0xff) as usize;
    let f = match &*slot {
        Value::Fn(f) => {
            let code = (*rt).heap.closure(*f).proto.native;
            let words = slot.cast::<u8>();
            let word = |offset: i32| words.add(offset as usize).cast::<i64>();
            word(layout::CALL_NOTE_OFFSET).write(note);
            word(layout::CALLER_OFFSET).write(base as i64);
            word(layout::TAIL_NOTE_OFFSET).write(0);
            return code;
        }
        Value::Native(_, f) => {
            {
                let rt = &mut *rt;
                let args = std::slice::from_raw_parts(slot.add(1), argc);
                match f(&mut rt.heap, args) {
                    Ok(v) => slot.write(v),
                    Err(message) => {
                        rt.fail(message);
                        return layout::STATUS_ERROR as usize;
                    }
                }
                // The result is in a register by now.
                rt.collect_if_wanted(0, top);
                // A call is a safepoint, unless its result is returned.
                if is_tail_call {
                    return layout::STATUS_OK as usize;
                }
                rt.budget -= 1;
                if rt.budget > 0 {
                    return layout::STATUS_OK as usize;
                }
            }
            return safepoint(rt, top, in_deferred) as usize;
        }
        Value::Runtime(f) => *f,
        v => internal_error("Fn", v),
    };
    // An intrinsic that makes the task wait runs again once the task is
    // woken, and then finds what it waited for.
    loop {
        {
            let rt = &mut *rt;
            // A cancelled task stops before an intrinsic, which may be one
            // it was woken from, except before ending a scope, which waits
            // for the scope's tasks to stop first.
            if f != Intrinsic::CloseScope && rt.stopped(in_deferred) {
                return layout::STATUS_STOPPED as usize;
            }
            rt.call_note = note;
            rt.call_base = base;
            rt.call_top = top;
            let args = std::slice::from_raw_parts(slot.add(1), argc).to_vec();
            match intrinsics::call(rt, f, &args) {
                Outcome::Value(v) => {
                    slot.write(v);
                    rt.collect_if_wanted(0, top);
                    return if rt.stopped(in_deferred) {
                        layout::STATUS_STOPPED
                    } else {
                        layout::STATUS_OK
                    } as usize;
                }
                Outcome::Error(message) => {
                    rt.fail(message);
                    return layout::STATUS_ERROR as usize;
                }
                Outcome::Wait => {
                    let fiber = rt.stack.0.as_mut().expect("a task is running");
                    fiber.wait_note = note;
                    fiber.wait_base = base;
                    fiber.waits_in_deferred = in_deferred || rt.deferred_calls > 0;
                }
            }
        }
        switch_away(rt, Yield::Wait, top);
    }
}

#[no_mangle]
pub unsafe extern "C" fn aja_rt_new_cell(
    rt: &mut Rt,
    dst: *mut Value,
    src: *const Value,
    top: *const Value,
) {
    rt.collect_if_wanted(0, top);
    let cell = rt.heap.alloc_cell((*src).clone());
    dst.write(Value::Cell(cell));
}

#[no_mangle]
pub unsafe extern "C" fn aja_rt_get_cell(rt: &mut Rt, dst: *mut Value, cell: *const Value) {
    let Value::Cell(cell) = &*cell else {
        internal_error("Cell", &*cell);
    };
    dst.write(rt.heap.cell(*cell).clone());
}

#[no_mangle]
pub unsafe extern "C" fn aja_rt_set_cell(rt: &mut Rt, cell: *const Value, src: *const Value) {
    let Value::Cell(cell) = &*cell else {
        internal_error("Cell", &*cell);
    };
    rt.heap.set_cell(*cell, (*src).clone());
}

/// The value the closure in `closure` captured as number `index`.
unsafe fn upvalue(rt: &Rt, closure: *const Value, index: u64) -> Value {
    let Value::Fn(f) = &*closure else {
        internal_error("Fn", &*closure);
    };
    rt.heap.closure(*f).upvalues[index as usize].clone()
}

#[no_mangle]
pub unsafe extern "C" fn aja_rt_get_upvalue(
    rt: &mut Rt,
    dst: *mut Value,
    closure: *const Value,
    index: u64,
) {
    dst.write(upvalue(rt, closure, index));
}

#[no_mangle]
pub unsafe extern "C" fn aja_rt_get_upvalue_cell(
    rt: &mut Rt,
    dst: *mut Value,
    closure: *const Value,
    index: u64,
) {
    let Value::Cell(cell) = upvalue(rt, closure, index) else {
        unreachable!("upvalue {index} is not a cell")
    };
    dst.write(rt.heap.cell(cell).clone());
}

#[no_mangle]
pub unsafe extern "C" fn aja_rt_set_upvalue_cell(
    rt: &mut Rt,
    closure: *const Value,
    index: u64,
    src: *const Value,
) {
    let Value::Cell(cell) = upvalue(rt, closure, index) else {
        unreachable!("upvalue {index} is not a cell")
    };
    rt.heap.set_cell(cell, (*src).clone());
}

/// Make a closure of `function`, capturing from the registers at `base`
/// and from the running closure, which is in the slot below them.
#[no_mangle]
pub unsafe extern "C" fn aja_rt_closure(
    rt: &mut Rt,
    dst: *mut Value,
    base: *const Value,
    function: u32,
    top: *const Value,
) {
    let proto = rt.protos[function as usize].clone();
    let upvalues: Box<[Value]> = proto
        .captures
        .iter()
        .map(|c| match c {
            Capture::Local(r) => (*base.add(*r as usize)).clone(),
            Capture::Upvalue(i) => upvalue(rt, base.sub(1), *i as u64),
            // Filled in below, once the closure exists.
            Capture::Itself => Value::Void,
        })
        .collect();
    let itself = proto.captures.iter().position(|c| *c == Capture::Itself);
    // What is captured is still in a register, or in the running closure.
    rt.collect_if_wanted(upvalues.len() * size_of::<Value>(), top);
    let f = rt.heap.alloc_closure(Closure { proto, upvalues });
    if let Some(i) = itself {
        rt.heap.set_upvalue(f, i, Value::Fn(f));
    }
    dst.write(Value::Fn(f));
}

#[no_mangle]
pub unsafe extern "C" fn aja_rt_new_data(
    rt: &mut Rt,
    dst: *mut Value,
    start: *const Value,
    count: u64,
    layout: u32,
    top: *const Value,
) {
    let fields: Box<[Value]> = std::slice::from_raw_parts(start, count as usize).into();
    rt.collect_if_wanted(fields.len() * size_of::<Value>(), top);
    let layout = rt.layouts[layout as usize].clone();
    dst.write(Value::Data(rt.heap.alloc_data(layout, fields)));
}

#[no_mangle]
pub unsafe extern "C" fn aja_rt_new_list(
    rt: &mut Rt,
    dst: *mut Value,
    start: *const Value,
    count: u64,
    top: *const Value,
) {
    let items = std::slice::from_raw_parts(start, count as usize);
    rt.collect_if_wanted(std::mem::size_of_val(items), top);
    let list = items.iter().cloned().collect();
    dst.write(Value::List(rt.heap.alloc_list(list)));
}

#[no_mangle]
pub unsafe extern "C" fn aja_rt_index(
    rt: &mut Rt,
    dst: *mut Value,
    list: *const Value,
    index: *const Value,
) -> i64 {
    let (Value::List(list), Value::Int(i)) = (&*list, &*index) else {
        internal_error("List and Int", &*index);
    };
    let items = rt.heap.list(*list);
    match native::in_range(*i, items.len()) {
        Ok(i) => {
            dst.write(items.get(i).unwrap().clone());
            0
        }
        Err(message) => {
            rt.fail(message);
            1
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn aja_rt_get_field(
    rt: &mut Rt,
    dst: *mut Value,
    obj: *const Value,
    index: u64,
) {
    let Value::Data(data) = &*obj else {
        internal_error("Data", &*obj);
    };
    dst.write(rt.heap.data(*data).1[index as usize].clone());
}

/// Whether the enum value in `obj` is the variant numbered `tag`: 1 or 0.
#[no_mangle]
pub unsafe extern "C" fn aja_rt_test_tag(rt: &mut Rt, obj: *const Value, tag: u32) -> i64 {
    let Value::Data(data) = &*obj else {
        internal_error("Data", &*obj);
    };
    (rt.heap.data(*data).0.tag == tag) as i64
}

#[no_mangle]
pub unsafe extern "C" fn aja_rt_add_str(
    rt: &mut Rt,
    dst: *mut Value,
    a: *const Value,
    b: *const Value,
    top: *const Value,
) -> i64 {
    let (Value::Str(a), Value::Str(b)) = (&*a, &*b) else {
        internal_error("Str", &*a);
    };
    let (a, b) = (rt.heap.str(*a), rt.heap.str(*b));
    if a.len() + b.len() > MAX_STR_LEN {
        let message = format!(
            "string too long: '+' would make a string of {} bytes, but the most is {MAX_STR_LEN}",
            a.len() + b.len()
        );
        rt.fail(message);
        return 1;
    }
    let s = [a, b].concat().into_boxed_str();
    // The operands are still in their registers.
    rt.collect_if_wanted(s.len(), top);
    dst.write(Value::Str(rt.heap.alloc_str(s)));
    0
}

/// Whether `a` and `b` are equal: 1 or 0.
#[no_mangle]
pub unsafe extern "C" fn aja_rt_equal(rt: &mut Rt, a: *const Value, b: *const Value) -> i64 {
    rt.heap.equal(&*a, &*b) as i64
}

/// Whether `a` comes before `b`: 1 or 0.
#[no_mangle]
pub unsafe extern "C" fn aja_rt_less(rt: &mut Rt, a: *const Value, b: *const Value) -> i64 {
    rt.heap.compare(&*a, &*b).is_lt() as i64
}

/// Whether `a` comes before `b` or is equal to it: 1 or 0.
#[no_mangle]
pub unsafe extern "C" fn aja_rt_less_or_equal(
    rt: &mut Rt,
    a: *const Value,
    b: *const Value,
) -> i64 {
    rt.heap.compare(&*a, &*b).is_le() as i64
}

/// `a % b` for floats.
#[no_mangle]
pub extern "C" fn aja_rt_rem_float(a: f64, b: f64) -> f64 {
    a % b
}

// ---- Starting the program ---------------------------------------------------

/// What a native program's `main` calls: run the program whose metadata is
/// the `meta_len` bytes at `meta` and whose functions' code addresses are
/// the table at `code`, and return the process's exit status.
///
/// # Safety
///
/// The arguments must be what `aja build` generates.
#[no_mangle]
pub unsafe extern "C" fn aja_rt_main(
    argc: i32,
    argv: *const *const c_char,
    meta: *const u8,
    meta_len: usize,
    code: *const usize,
    enter: Enter,
) -> i32 {
    let args = (1..argc.max(0) as usize)
        .map(|i| CStr::from_ptr(*argv.add(i)).to_string_lossy().into_owned())
        .collect();
    native::set_args(args);

    let meta = Meta::decode(std::slice::from_raw_parts(meta, meta_len));
    // Error messages can point into the prelude.
    diagnostic::set_prelude(String::leak(meta.prelude.clone()));
    let code = std::slice::from_raw_parts(code, meta.functions.len());
    let mut files = Files::default();
    for (path, text) in &meta.files {
        files.add(path.as_str(), text.as_str());
    }
    let main_result = meta.main_result;
    let main = meta.main as usize;
    // The runtime is never freed: when the program ends with tasks still
    // waiting, their stacks can't be unwound, and the process is about to
    // exit anyway.
    let rt = Box::into_raw(load(meta, code, enter));
    (*rt).this = rt;
    // For `aja`'s own tests: collect at every allocation and end every
    // turn at its first safepoint, to flush out code that depends on when
    // either happens.
    if std::env::var_os("AJA_STRESS").is_some() {
        (*rt).heap.stress = true;
        (*rt).scheduler.stress = true;
    }

    let result = run(rt, main).and_then(|value| {
        let rt = &*rt;
        match (main_result, &value) {
            // `Result::Err` is the second variant.
            (Some(span), Value::Data(gc)) if rt.heap.data(*gc).0.tag == 1 => {
                let (_, fields) = rt.heap.data(*gc);
                Err(RuntimeError {
                    message: format!("main returned an error: {}", rt.heap.repr(&fields[0])),
                    span: Some(span),
                    trace: Vec::new(),
                    notes: Vec::new(),
                })
            }
            _ => Ok(()),
        }
    });
    // `main` isn't Rust's, so nothing else flushes what was printed.
    let _ = std::io::stdout().flush();
    match result {
        Ok(()) => 0,
        // As when the VM runs the program: whoever was reading has all they
        // wanted, and the status is that of a program stopped by SIGPIPE.
        Err(error) if error.message == native::OUTPUT_CLOSED => 141,
        Err(error) => {
            let diagnostic = error.to_diagnostic_in(&files);
            eprint!("{}", diagnostic.render_in(&files, diagnostic::use_color()));
            1
        }
    }
}

/// Run the function in global `main` as the first task, and every task it
/// starts, until all have finished: returns what it returned, or the error
/// that stopped the program.
///
/// The running task's fiber runs until the task waits, ends its turn, or
/// finishes; then the scheduler puts the next task's stack in
/// [`Rt::stack`], and that one runs.
unsafe fn run(rt: *mut Rt, main: usize) -> Result<Value, RuntimeError> {
    {
        let rt = &mut *rt;
        let Value::Fn(f) = rt.globals[main] else {
            unreachable!("'main' is a function");
        };
        let stack = rt.fiber(f);
        rt.scheduler.spawn(stack, None, None, Rc::from(Vec::new()));
        rt.scheduler.start(&mut rt.stack);
    }
    loop {
        (*rt).begin_turn();
        // No reference to the runtime is held while the task runs.
        let fiber: *mut Fiber = &mut **(*rt).stack.0.as_mut().expect("a task is running");
        let coroutine: *mut Coroutine<(), Yield, i64> =
            (*fiber).coroutine.as_mut().expect("a fiber has its code");
        let outcome = (*coroutine).resume(());

        let rt = &mut *rt;
        let next = match outcome {
            CoroutineResult::Yield(Yield::EndTurn) => {
                rt.scheduler.end_turn(&mut rt.stack);
                continue;
            }
            CoroutineResult::Yield(Yield::Wait) => match rt.scheduler.wait(&mut rt.stack) {
                Next::Switched => continue,
                Next::Finished | Next::Deadlock => {
                    let mut error = rt.deadlock(false);
                    error.span = rt.stack.0.as_ref().and_then(|f| rt.waiting_at(f));
                    return Err(error);
                }
            },
            CoroutineResult::Return(layout::STATUS_OK) => {
                let value = (*(*fiber).regs).clone();
                rt.scheduler
                    .finish(Some(value), &mut rt.heap, &mut rt.stack)
            }
            // A cancelled task has left its last frame.
            CoroutineResult::Return(layout::STATUS_STOPPED) => {
                rt.scheduler.finish(None, &mut rt.heap, &mut rt.stack)
            }
            CoroutineResult::Return(_) => {
                // No call led to the task's function, but a tail call may
                // have replaced it.
                rt.trace((*fiber).regs, None);
                let mut error = rt.error.take().expect("a failed task left its error");
                // Where the task was spawned, and the tasks it came from,
                // lead back to `main`.
                error.notes = rt
                    .scheduler
                    .origin()
                    .iter()
                    .rev()
                    .map(|&at| Note {
                        text: "in a task spawned at {}".to_string(),
                        places: vec![at],
                    })
                    .collect();
                return Err(error);
            }
        };
        match next {
            Next::Switched => {}
            Next::Finished => break,
            Next::Deadlock => return Err((*rt).deadlock(true)),
        }
    }
    Ok((*rt).scheduler.take_results().swap_remove(0))
}

/// Set up the runtime for the program `meta` describes: its globals (the
/// built-ins, native functions, and top-level functions) and string
/// literals.
fn load(meta: Meta, code: &[usize], enter: Enter) -> Box<Rt> {
    let mut heap = Heap::new();
    let native = |name: &str| {
        if let Some(f) = Intrinsic::named(name) {
            return Value::Runtime(f);
        }
        let (name, f) = native::implementation(name)
            .unwrap_or_else(|| panic!("native function '{name}' has no implementation"));
        Value::Native(name, f)
    };
    let mut globals = vec![Value::Void; (meta.globals as usize).max(builtins::NAMES.len())];
    for (slot, name) in builtins::NAMES.iter().enumerate() {
        globals[slot] = native(name);
    }
    for (slot, name) in &meta.natives {
        globals[*slot as usize] = native(name);
    }

    let mut protos = Vec::new();
    let mut sites = Vec::new();
    for (f, &native) in meta.functions.into_iter().zip(code) {
        let proto = Rc::new(Proto {
            name: f.name.map(Id::from),
            signature: f.signature,
            arity: f.arity,
            num_regs: f.num_regs,
            code: Vec::new(),
            spans: Vec::new(),
            protos: Vec::new(),
            layouts: Vec::new(),
            captures: f.captures,
            call_names: HashMap::new(),
            cleanups: Vec::new(),
            deferred: Vec::new(),
            native,
        });
        if let Some(slot) = f.slot {
            let closure = heap.alloc_closure(Closure {
                proto: proto.clone(),
                upvalues: Box::new([]),
            });
            globals[slot as usize] = Value::Fn(closure);
        }
        protos.push(proto);
        sites.push(FnSites {
            spans: f.sites.into_iter().collect(),
            call_names: f
                .call_names
                .into_iter()
                .map(|(ip, name)| (ip, Id::from(name)))
                .collect(),
        });
    }

    let mut literals = Vec::new();
    for (n, s) in meta.literals {
        let n = n as usize;
        if n >= literals.len() {
            literals.resize(n + 1, Value::Void);
        }
        literals[n] = Value::Str(heap.alloc_str(s.into_boxed_str()));
    }

    let layouts = meta
        .layouts
        .into_iter()
        .map(|l| {
            Rc::new(Layout {
                name: l.name,
                tag: l.tag,
                fields: l
                    .fields
                    .map(|fields| fields.into_iter().map(Id::from).collect()),
            })
        })
        .collect();

    Box::new(Rt {
        regs_limit: std::ptr::null_mut(),
        stack_limit: 0,
        globals_ptr: globals.as_mut_ptr(),
        literals_ptr: literals.as_mut_ptr(),
        budget: 0,
        deferred_calls: 0,
        heap,
        stack: Stack::default(),
        scheduler: Scheduler::new(),
        globals,
        literals,
        protos,
        layouts,
        sites,
        library: meta
            .library
            .into_iter()
            .map(|id| Source::File(id as u16))
            .collect(),
        error: None,
        last_call: None,
        turn_left: None,
        at_start: false,
        call_note: 0,
        call_base: std::ptr::null(),
        call_top: std::ptr::null(),
        this: std::ptr::null_mut(),
        enter,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::offset_of;

    #[test]
    fn the_runtime_is_laid_out_as_compiled_code_expects() {
        assert_eq!(offset_of!(Rt, regs_limit), layout::RT_REGS_LIMIT as usize);
        assert_eq!(offset_of!(Rt, stack_limit), layout::RT_STACK_LIMIT as usize);
        assert_eq!(offset_of!(Rt, globals_ptr), layout::RT_GLOBALS as usize);
        assert_eq!(offset_of!(Rt, literals_ptr), layout::RT_LITERALS as usize);
        assert_eq!(offset_of!(Rt, budget), layout::RT_BUDGET as usize);
        assert_eq!(
            offset_of!(Rt, deferred_calls),
            layout::RT_DEFERRED_CALLS as usize
        );
    }
}
