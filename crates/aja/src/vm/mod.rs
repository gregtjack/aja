//! A register-based bytecode VM.
//!
//! [`compile`] turns the type-checked AST into [`bytecode`]; [`Vm`] runs it.
//! Call frames are kept in [`Vm::frames`], not on the Rust stack, so deep
//! recursion in Aja can't overflow the VM's own stack: the call stack can grow
//! until it uses [`DEFAULT_STACK_LIMIT`] bytes. Tail calls reuse their frame,
//! so tail recursion runs in constant space. Closures, captured variables,
//! struct and enum values, and strings live on a garbage-collected [`heap`].
//!
//! The VM runs tasks, green threads that take turns on the VM's thread (see
//! [`scheduler`]). Each has its own frames and registers, and so its own
//! stack limit.

use std::{mem::size_of, rc::Rc};

use crate::{
    ast::{Id, Source, Span},
    builtins::BUILTINS,
};

pub mod compile;
pub use aja_runtime::{
    bytecode, hamt, heap, internal_error, intrinsics, list, native,
    native::{set_args, OUTPUT_CLOSED},
    scheduler,
    scheduler::Intrinsic,
    sorted, value, Note, RuntimeError, TraceEntry, DEFAULT_STACK_LIMIT,
};

#[cfg(test)]
mod tests;

use bytecode::{Capture, Layout, Literals, Op, Proto};
pub use compile::{Compiled, Compiler, ReplInput, Test};
use heap::{Closure, Gc, Heap, MAX_STR_LEN};
use scheduler::{Next, Outcome, Scheduler, TaskStack};
use value::Value;

/// Whether the VM implements the native function `name`.
#[cfg(test)]
pub fn native_implemented(name: &str) -> bool {
    native::implementation(name).is_some() || Intrinsic::named(name).is_some()
}

/// A call in progress.
pub struct Frame {
    closure: Gc,
    /// The closure's code, kept here to avoid a heap lookup per instruction.
    proto: Rc<Proto>,
    /// The next instruction to run. Only up to date for frames other than
    /// the innermost one, which keeps its own copy while it runs.
    ip: usize,
    /// Where the call's register window starts in [`Vm::regs`].
    base: usize,
    /// Where to put the result, in [`Vm::regs`].
    ret: usize,
    /// If a tail call replaced this frame's original function, the tail call
    /// instruction (the calling function's code and the instruction's
    /// index), for error traces.
    tail_call: Option<(Rc<Proto>, usize)>,
}

/// A task's call stack in the VM.
#[derive(Default)]
pub struct Stack {
    /// The registers, every call's window one after another.
    regs: Vec<Value>,
    /// The calls in progress.
    frames: Vec<Frame>,
}

impl Stack {
    /// The stack of a task about to call `closure`, whose code is `proto`
    /// and which takes no arguments.
    fn new(closure: Gc, proto: Rc<Proto>) -> Stack {
        Stack {
            regs: vec![Value::Void; proto.num_regs as usize],
            frames: vec![Frame {
                closure,
                proto,
                ip: 0,
                base: 0,
                ret: 0,
                tail_call: None,
            }],
        }
    }
}

impl TaskStack for Stack {
    fn waits_in_deferred(&self) -> bool {
        // A waiting task's innermost frame is at the call it waits in.
        in_deferred(&self.frames, self.frames.last().map(|f| f.ip))
    }
}

impl intrinsics::Host for Vm {
    type Stack = Stack;

    fn parts(&mut self) -> (&mut Heap, &mut Scheduler<Stack>) {
        (&mut self.heap, &mut self.scheduler)
    }

    fn collect_if_wanted(&mut self) {
        if self.heap.wants_collection(0) {
            self.collect_garbage_with(Vec::new());
        }
    }

    fn call_site(&self) -> Span {
        let top = self.stack.frames.last().unwrap();
        top.proto.spans[top.ip - 1]
    }

    fn new_stack(&mut self, closure: Gc) -> Stack {
        Stack::new(closure, self.heap.closure(closure).proto.clone())
    }
}

pub struct Vm {
    pub heap: Heap,
    globals: Vec<Value>,
    /// The string literals, by number; see [`Literals`]. They are roots.
    literals: Vec<Value>,
    /// The running task's registers and calls in progress.
    stack: Stack,
    /// The other tasks, and which runs next.
    scheduler: Scheduler<Stack>,
    /// The most memory, in bytes, each task's call stack may use.
    pub stack_limit: usize,
    /// The program's standard library files (see [`Compiled::library`]).
    library: Vec<crate::ast::Source>,
}

impl Vm {
    /// A VM with the built-ins defined, in the slots [`Compiler::new`] expects.
    pub fn new() -> Self {
        let globals = BUILTINS
            .iter()
            .map(|b| {
                if let Some(f) = Intrinsic::named(b.name) {
                    return Value::Runtime(f);
                }
                let (name, f) = native::implementation(b.name).unwrap_or_else(|| {
                    panic!("built-in '{}' has no implementation in the VM", b.name)
                });
                Value::Native(name, f)
            })
            .collect();
        Vm {
            heap: Heap::new(),
            globals,
            literals: Vec::new(),
            stack: Stack::default(),
            scheduler: Scheduler::new(),
            stack_limit: DEFAULT_STACK_LIMIT,
            library: Vec::new(),
        }
    }

    /// Define a compiled program's functions, then run `main` and return
    /// its result.
    pub fn run_program(&mut self, program: &Compiled) -> Result<Value, RuntimeError> {
        self.load_program(program);
        let main = program.main.expect("a program to run has a 'main'");
        let value = self.run_slot(main)?;
        match program.main_result {
            Some(span) => self.check_result(value, span, "main"),
            None => Ok(value),
        }
    }

    /// Define a compiled program's functions, then run its test `test`.
    /// A VM runs one test, so that each starts from nothing, as a program
    /// does.
    pub fn run_test(&mut self, program: &Compiled, test: &Test) -> Result<(), RuntimeError> {
        self.load_program(program);
        let value = self.run_slot(test.slot)?;
        if test.returns_result {
            self.check_result(value, test.span, "the test")?;
        }
        Ok(())
    }

    fn load_program(&mut self, program: &Compiled) {
        self.library = program.library.clone();
        self.load_literals(&program.literals);
        self.load_functions(&program.functions);
        self.load_natives(&program.natives);
    }

    /// Run the function in global slot `slot`, which takes no arguments.
    fn run_slot(&mut self, slot: u32) -> Result<Value, RuntimeError> {
        let Value::Fn(f) = self.globals[slot as usize] else {
            unreachable!("slot {slot} holds a function");
        };
        self.run(f)
    }

    /// `value`, a `Result` that `who` (named at `span`) returned, or an
    /// error if it is an `Err`.
    fn check_result(&self, value: Value, span: Span, who: &str) -> Result<Value, RuntimeError> {
        if let Value::Data(gc) = &value {
            let (layout, fields) = self.heap.data(*gc);
            // `Result::Err` is the second variant.
            if layout.tag == 1 {
                return Err(RuntimeError {
                    message: format!("{who} returned an error: {}", self.heap.repr(&fields[0])),
                    span: Some(span),
                    trace: Vec::new(),
                    notes: Vec::new(),
                });
            }
        }
        Ok(value)
    }

    /// Run the functions of `program` named `names`, which take no
    /// arguments, as tasks, the first one first, until all have finished;
    /// return what each returned. Programs can't start tasks themselves yet,
    /// so this is how tests exercise the scheduler.
    #[cfg(test)]
    pub fn run_as_tasks(
        &mut self,
        program: &Compiled,
        names: &[&str],
    ) -> Result<Vec<Value>, RuntimeError> {
        self.load_literals(&program.literals);
        self.load_functions(&program.functions);
        self.load_natives(&program.natives);
        let closures: Vec<Gc> = names
            .iter()
            .map(|name| {
                let (slot, _) = program
                    .functions
                    .iter()
                    .find(|(_, p)| p.name.as_ref().is_some_and(|n| n == name))
                    .unwrap_or_else(|| panic!("no function '{name}'"));
                let Value::Fn(f) = self.globals[*slot as usize] else {
                    unreachable!("'{name}' is a function");
                };
                f
            })
            .collect();
        self.run_tasks(&closures)
    }

    /// Run a compiled REPL input and return its value.
    pub fn run_repl(&mut self, input: &ReplInput) -> Result<Value, RuntimeError> {
        self.load_literals(&input.literals);
        self.load_functions(&input.functions);
        self.load_natives(&input.natives);
        let f = self.alloc_closure(input.proto.clone(), Box::new([]));
        self.run(f)
    }

    /// Define top-level functions in their global slots.
    fn load_functions(&mut self, functions: &[(u32, Rc<Proto>)]) {
        for (slot, proto) in functions {
            let f = self.alloc_closure(proto.clone(), Box::new([]));
            self.set_global(*slot, Value::Fn(f));
        }
    }

    /// Define native functions in their global slots.
    fn load_natives(&mut self, natives: &[(u32, Id)]) {
        for (slot, name) in natives {
            if let Some(f) = Intrinsic::named(name) {
                self.set_global(*slot, Value::Runtime(f));
                continue;
            }
            let (name, f) = native::implementation(name).unwrap_or_else(|| {
                panic!("native function '{name}' has no implementation in the VM")
            });
            self.set_global(*slot, Value::Native(name, f));
        }
    }

    /// Put string literals on the heap, where they stay for good.
    fn load_literals(&mut self, literals: &Literals) {
        for (n, s) in literals {
            let n = *n as usize;
            if n >= self.literals.len() {
                self.literals.resize(n + 1, Value::Void);
            }
            // Clear the slot first, so a literal it held before can be freed.
            self.literals[n] = Value::Void;
            let gc = self.alloc_str(s.clone());
            self.literals[n] = Value::Str(gc);
        }
    }

    /// Free every object the program can no longer reach.
    #[cfg(test)]
    pub fn collect_garbage(&mut self) {
        self.collect_garbage_with(Vec::new());
    }

    // Allocation may collect, so every value still in use must be in a
    // register, global, or frame, or passed in, when these are called.

    fn alloc_closure(&mut self, proto: Rc<Proto>, upvalues: Box<[Value]>) -> Gc {
        if self
            .heap
            .wants_collection(upvalues.len() * size_of::<Value>())
        {
            let extra = upvalues
                .iter()
                .filter_map(Value::handle)
                .collect::<Vec<_>>();
            self.collect_garbage_with(extra);
        }
        self.heap.alloc_closure(Closure { proto, upvalues })
    }

    fn alloc_data(&mut self, layout: Rc<Layout>, fields: Box<[Value]>) -> Gc {
        if self
            .heap
            .wants_collection(fields.len() * size_of::<Value>())
        {
            let extra = fields.iter().filter_map(Value::handle).collect();
            self.collect_garbage_with(extra);
        }
        self.heap.alloc_data(layout, fields)
    }

    fn alloc_cell(&mut self, value: Value) -> Gc {
        if self.heap.wants_collection(0) {
            self.collect_garbage_with(value.handle().into_iter().collect());
        }
        self.heap.alloc_cell(value)
    }

    fn alloc_list(&mut self, list: list::List) -> Gc {
        if self.heap.wants_collection(list.len() * size_of::<Value>()) {
            // The values are still in registers, so they stay alive.
            self.collect_garbage_with(Vec::new());
        }
        self.heap.alloc_list(list)
    }

    fn alloc_str(&mut self, s: Box<str>) -> Gc {
        if self.heap.wants_collection(s.len()) {
            self.collect_garbage_with(Vec::new());
        }
        self.heap.alloc_str(s)
    }

    /// A native function or an intrinsic allocates straight on the heap,
    /// which never collects, so the VM collects once it has returned, if
    /// what it allocated calls for that. `result` is what it returned, if
    /// that isn't in a register yet.
    fn collect_after_native(&mut self, result: Option<Gc>) {
        if self.heap.wants_collection(0) {
            self.collect_garbage_with(result.into_iter().collect());
        }
    }

    /// Collect, keeping `extra` and everything it reaches alive too.
    fn collect_garbage_with(&mut self, extra: Vec<Gc>) {
        // Every task's registers and frames, the running one's included.
        // Registers above every frame's window hold only dead values; inside
        // one, a register not yet written may hold a stale handle (see
        // `Heap::collect`).
        let stacks = std::iter::once(&self.stack)
            .chain(self.scheduler.stacks())
            .map(|s| (&s.regs[..], &s.frames[..]));
        let mut regs: Vec<&Value> = Vec::new();
        let mut roots = extra;
        roots.extend(self.scheduler.held());
        for (task_regs, frames) in stacks {
            regs.extend(&task_regs[..live_regs(task_regs, frames)]);
            roots.extend(frames.iter().map(|f| f.closure));
        }
        self.heap.collect(
            regs.into_iter()
                .chain(self.scheduler.results())
                .chain(&self.globals)
                .chain(&self.literals),
            roots,
        );
    }

    fn set_global(&mut self, slot: u32, value: Value) {
        let slot = slot as usize;
        if slot >= self.globals.len() {
            self.globals.resize(slot + 1, Value::Void);
        }
        self.globals[slot] = value;
    }

    /// Call `closure`, which takes no arguments.
    fn run(&mut self, closure: Gc) -> Result<Value, RuntimeError> {
        self.run_tasks(&[closure])
            .map(|mut results| results.swap_remove(0))
    }

    /// Run each of `closures`, which take no arguments, as a task, until
    /// every task has finished; return what each returned. The first task
    /// runs first.
    fn run_tasks(&mut self, closures: &[Gc]) -> Result<Vec<Value>, RuntimeError> {
        self.scheduler.clear();
        for &closure in closures {
            let proto = self.heap.closure(closure).proto.clone();
            self.scheduler
                .spawn(Stack::new(closure, proto), None, None, Rc::from(Vec::new()));
        }
        self.scheduler.start(&mut self.stack);
        let result = self.execute();
        self.stack.frames.clear();
        self.stack.regs.clear();
        match result {
            Ok(()) => Ok(self.scheduler.take_results()),
            Err(e) => {
                self.scheduler.clear();
                Err(e)
            }
        }
    }

    /// Run the running task, and then the others in turn, until every
    /// task has finished or one fails.
    fn execute(&mut self) -> Result<(), RuntimeError> {
        let frame = self.stack.frames.last().unwrap();
        let mut closure = frame.closure;
        let mut proto = frame.proto.clone();
        let mut base = frame.base;
        let mut ip = frame.ip;
        let mut budget = self.scheduler.slice();

        // Pick up where the innermost frame is, after a switch.
        macro_rules! load {
            () => {{
                let frame = self.stack.frames.last().unwrap();
                closure = frame.closure;
                proto = frame.proto.clone();
                base = frame.base;
                ip = frame.ip;
            }};
        }
        // Leave frames on the way to stopping a cancelled task, starting
        // with the innermost, which is at instruction `at` (`None` if it
        // hasn't started, or has just run its cleanup block). A frame with
        // deferred code pending there runs it, in its cleanup block, which
        // ends in `Unwind` and comes back here; a frame without is just
        // left. Once the outermost is left, the task has finished, without
        // a result. Every frame's `ip` but the innermost's must be up to
        // date.
        macro_rules! unwind {
            ($at:expr) => {{
                let mut at: Option<usize> = $at;
                loop {
                    let frame = self.stack.frames.last_mut().unwrap();
                    if let Some(cleanup) = at.and_then(|a| frame.proto.cleanup_at(a)) {
                        frame.ip = cleanup;
                        load!();
                        break;
                    }
                    self.stack.frames.pop();
                    if self.stack.frames.is_empty() {
                        let next = self.scheduler.finish(None, &mut self.heap, &mut self.stack);
                        match next {
                            Next::Switched => {
                                load!();
                                budget = self.scheduler.slice();
                            }
                            Next::Finished => return Ok(()),
                            Next::Deadlock => return Err(self.deadlock(true)),
                        }
                        break;
                    }
                    at = Some(self.stack.frames.last().unwrap().ip - 1);
                }
            }};
        }
        // If the running task has been asked to stop, and isn't running
        // deferred code, which always runs to its end, start stopping it
        // at instruction `at` of the innermost frame and evaluate to true.
        macro_rules! stopped {
            ($at:expr) => {{
                let at: Option<usize> = $at;
                if self.scheduler.cancel_requested() && !in_deferred(&self.stack.frames, at) {
                    self.stack.frames.last_mut().unwrap().ip = ip;
                    self.scheduler.start_unwinding();
                    unwind!(at);
                    true
                } else {
                    false
                }
            }};
        }
        // Count a safepoint, which `at` (in the innermost frame) has just
        // run, and end the task's turn once its budget runs out; or stop
        // the task there, if it has been cancelled. `ip` must be where this
        // task goes on.
        macro_rules! safepoint {
            ($at:expr) => {{
                if !stopped!($at) {
                    budget -= 1;
                    if budget == 0 {
                        budget = self.scheduler.slice();
                        self.stack.frames.last_mut().unwrap().ip = ip;
                        if self.scheduler.end_turn(&mut self.stack) {
                            load!();
                        }
                    }
                }
            }};
        }

        macro_rules! reg {
            ($r:expr) => {
                self.stack.regs[base + $r as usize]
            };
        }
        macro_rules! get {
            ($variant:ident, $r:expr) => {
                match &reg!($r) {
                    Value::$variant(v) => v.clone(),
                    v => internal_error(stringify!($variant), v),
                }
            };
        }
        macro_rules! throw {
            ($msg:expr) => {{
                self.stack.frames.last_mut().unwrap().ip = ip;
                return Err(self.error($msg));
            }};
        }
        macro_rules! int_op {
            ($dst:expr, $a:expr, $b:expr, $method:ident, $sym:literal) => {{
                match get!(Int, $a).$method(get!(Int, $b)) {
                    Some(v) => reg!($dst) = Value::Int(v),
                    None => throw!(concat!("integer overflow in '", $sym, "'").to_string()),
                }
            }};
        }
        macro_rules! float_op {
            ($dst:expr, $a:expr, $b:expr, $op:tt) => {{
                let v = get!(Float, $a) $op get!(Float, $b);
                reg!($dst) = Value::Float(v);
            }};
        }
        // Grow the registers to `needed` for a call that will have `frames`
        // frames in progress, or report a stack overflow.
        macro_rules! reserve {
            ($needed:expr, $frames:expr) => {{
                let (needed, frames) = ($needed, $frames);
                if needed * size_of::<Value>() + frames * size_of::<Frame>() > self.stack_limit {
                    throw!(format!(
                        "stack overflow: the call stack has grown past {} MB ({} calls in progress)",
                        self.stack_limit >> 20,
                        self.stack.frames.len(),
                    ));
                }
                if self.stack.regs.len() < needed {
                    self.stack.regs.resize(needed, Value::Void);
                }
            }};
        }
        // Return `v` from the innermost frame. If it is the task's last,
        // the task is done, and the next one runs.
        macro_rules! ret {
            ($v:expr) => {{
                let v = $v;
                let done = self.stack.frames.pop().unwrap();
                if self.stack.frames.is_empty() {
                    let next = self
                        .scheduler
                        .finish(Some(v), &mut self.heap, &mut self.stack);
                    match next {
                        Next::Switched => {
                            load!();
                            budget = self.scheduler.slice();
                        }
                        Next::Finished => return Ok(()),
                        Next::Deadlock => return Err(self.deadlock(true)),
                    }
                } else {
                    load!();
                    self.stack.regs[done.ret] = v;
                }
            }};
        }
        // Make the running task wait. The call it is making, the
        // instruction before `ip`, runs again once the task is woken.
        macro_rules! wait {
            () => {{
                self.stack.frames.last_mut().unwrap().ip = ip - 1;
                match self.scheduler.wait(&mut self.stack) {
                    Next::Switched => {
                        load!();
                        budget = self.scheduler.slice();
                    }
                    Next::Finished | Next::Deadlock => {
                        let mut err = self.deadlock(false);
                        err.span = self.waiting_at(&self.stack.frames);
                        return Err(err);
                    }
                }
            }};
        }
        macro_rules! compare {
            ($dst:expr, $a:expr, $b:expr, $variant:ident, $op:tt) => {{
                let v = get!($variant, $a) $op get!($variant, $b);
                reg!($dst) = Value::Bool(v);
            }};
        }

        loop {
            let op = proto.code[ip];
            ip += 1;
            match op {
                Op::LoadVoid { dst } => reg!(dst) = Value::Void,
                Op::LoadBool { dst, value } => reg!(dst) = Value::Bool(value),
                Op::LoadInt { dst, value } => reg!(dst) = Value::Int(value),
                Op::LoadFloat { dst, value } => reg!(dst) = Value::Float(value),
                Op::LoadStr { dst, index } => reg!(dst) = self.literals[index as usize].clone(),
                Op::Move { dst, src } => reg!(dst) = reg!(src).clone(),

                Op::GetGlobal { dst, index } => reg!(dst) = self.globals[index as usize].clone(),
                Op::SetGlobal { index, src } => {
                    let v = reg!(src).clone();
                    self.set_global(index, v);
                }

                Op::NewCell { dst, src } => {
                    let v = reg!(src).clone();
                    let cell = self.alloc_cell(v);
                    reg!(dst) = Value::Cell(cell);
                }
                Op::GetCell { dst, cell } => {
                    let v = self.heap.cell(get!(Cell, cell)).clone();
                    reg!(dst) = v;
                }
                Op::SetCell { cell, src } => {
                    let v = reg!(src).clone();
                    self.heap.set_cell(get!(Cell, cell), v);
                }
                Op::GetUpvalue { dst, index } => {
                    reg!(dst) = self.heap.closure(closure).upvalues[index as usize].clone();
                }
                Op::GetUpvalueCell { dst, index } => {
                    let Value::Cell(cell) = self.heap.closure(closure).upvalues[index as usize]
                    else {
                        unreachable!("upvalue {index} is not a cell")
                    };
                    reg!(dst) = self.heap.cell(cell).clone();
                }
                Op::SetUpvalueCell { index, src } => {
                    let Value::Cell(cell) = self.heap.closure(closure).upvalues[index as usize]
                    else {
                        unreachable!("upvalue {index} is not a cell")
                    };
                    let v = reg!(src).clone();
                    self.heap.set_cell(cell, v);
                }
                Op::Closure { dst, proto: index } => {
                    let inner = proto.protos[index as usize].clone();
                    let upvalues = inner
                        .captures
                        .iter()
                        .map(|c| match c {
                            Capture::Local(r) => reg!(*r).clone(),
                            Capture::Upvalue(i) => {
                                self.heap.closure(closure).upvalues[*i as usize].clone()
                            }
                            // Filled in below, once the closure exists.
                            Capture::Itself => Value::Void,
                        })
                        .collect();
                    let itself = inner.captures.iter().position(|c| *c == Capture::Itself);
                    let f = self.alloc_closure(inner, upvalues);
                    if let Some(i) = itself {
                        self.heap.set_upvalue(f, i, Value::Fn(f));
                    }
                    reg!(dst) = Value::Fn(f);
                }

                Op::NewData {
                    dst,
                    layout,
                    start,
                    count,
                } => {
                    let from = base + start as usize;
                    let fields: Box<[Value]> = self.stack.regs[from..from + count as usize].into();
                    let layout = proto.layouts[layout as usize].clone();
                    let data = self.alloc_data(layout, fields);
                    reg!(dst) = Value::Data(data);
                }
                Op::NewList { dst, start, count } => {
                    let from = base + start as usize;
                    let list: list::List = self.stack.regs[from..from + count as usize]
                        .iter()
                        .cloned()
                        .collect();
                    let gc = self.alloc_list(list);
                    reg!(dst) = Value::List(gc);
                }
                Op::Index { dst, list, index } => {
                    let Value::Int(i) = reg!(index) else {
                        internal_error("Int", &reg!(index));
                    };
                    let items = self.heap.list(get!(List, list));
                    match native::in_range(i, items.len()) {
                        Ok(i) => reg!(dst) = items.get(i).unwrap().clone(),
                        Err(msg) => throw!(msg),
                    }
                }
                Op::GetField { dst, obj, index } => {
                    let v = self.heap.data(get!(Data, obj)).1[index as usize].clone();
                    reg!(dst) = v;
                }
                Op::TestTag { dst, obj, tag } => {
                    let is = self.heap.data(get!(Data, obj)).0.tag == tag;
                    reg!(dst) = Value::Bool(is);
                }

                Op::AddI { dst, a, b } => int_op!(dst, a, b, checked_add, "+"),
                Op::SubI { dst, a, b } => int_op!(dst, a, b, checked_sub, "-"),
                Op::MulI { dst, a, b } => int_op!(dst, a, b, checked_mul, "*"),
                Op::DivI { dst, a, b } => {
                    if get!(Int, b) == 0 {
                        throw!("division by zero".to_string());
                    }
                    int_op!(dst, a, b, checked_div, "/")
                }
                Op::RemI { dst, a, b } => {
                    if get!(Int, b) == 0 {
                        throw!("modulo by zero".to_string());
                    }
                    // `int::MIN % -1` is 0, though `int::MIN / -1` overflows.
                    let v = get!(Int, a).wrapping_rem(get!(Int, b));
                    reg!(dst) = Value::Int(v);
                }
                Op::NegI { dst, src } => match get!(Int, src).checked_neg() {
                    Some(v) => reg!(dst) = Value::Int(v),
                    None => throw!("integer overflow in '-'".to_string()),
                },
                Op::AddF { dst, a, b } => float_op!(dst, a, b, +),
                Op::SubF { dst, a, b } => float_op!(dst, a, b, -),
                Op::MulF { dst, a, b } => float_op!(dst, a, b, *),
                Op::DivF { dst, a, b } => float_op!(dst, a, b, /),
                Op::RemF { dst, a, b } => float_op!(dst, a, b, %),
                Op::NegF { dst, src } => reg!(dst) = Value::Float(-get!(Float, src)),
                Op::AddS { dst, a, b } => {
                    let (a, b) = (self.heap.str(get!(Str, a)), self.heap.str(get!(Str, b)));
                    if a.len() + b.len() > MAX_STR_LEN {
                        throw!(format!(
                            "string too long: '+' would make a string of {} bytes, but the most is {MAX_STR_LEN}",
                            a.len() + b.len()
                        ));
                    }
                    let s = [a, b].concat().into_boxed_str();
                    // The operands are still in their registers, so a
                    // collection here keeps them.
                    let gc = self.alloc_str(s);
                    reg!(dst) = Value::Str(gc);
                }

                Op::LtI { dst, a, b } => compare!(dst, a, b, Int, <),
                Op::LeI { dst, a, b } => compare!(dst, a, b, Int, <=),
                Op::LtF { dst, a, b } => compare!(dst, a, b, Float, <),
                Op::LeF { dst, a, b } => compare!(dst, a, b, Float, <=),
                Op::EqI { dst, a, b } => compare!(dst, a, b, Int, ==),
                Op::EqF { dst, a, b } => compare!(dst, a, b, Float, ==),
                Op::EqB { dst, a, b } => compare!(dst, a, b, Bool, ==),
                Op::EqS { dst, a, b } => {
                    let v = self.heap.equal(&reg!(a), &reg!(b));
                    reg!(dst) = Value::Bool(v);
                }
                Op::EqD { dst, a, b } => {
                    let v = self.heap.equal(&reg!(a), &reg!(b));
                    reg!(dst) = Value::Bool(v);
                }
                Op::LtD { dst, a, b } => {
                    let v = self.heap.compare(&reg!(a), &reg!(b)).is_lt();
                    reg!(dst) = Value::Bool(v);
                }
                Op::LeD { dst, a, b } => {
                    let v = self.heap.compare(&reg!(a), &reg!(b)).is_le();
                    reg!(dst) = Value::Bool(v);
                }
                Op::Not { dst, src } => reg!(dst) = Value::Bool(!get!(Bool, src)),

                Op::Jump { target } => {
                    let at = ip - 1;
                    let back = (target as usize) < ip;
                    ip = target as usize;
                    // Every loop jumps back to its start.
                    if back {
                        safepoint!(Some(at));
                    }
                }
                Op::JumpIfFalse { cond, target } => {
                    if !get!(Bool, cond) {
                        ip = target as usize;
                    }
                }
                Op::JumpIfTrue { cond, target } => {
                    if get!(Bool, cond) {
                        ip = target as usize;
                    }
                }
                Op::Call { dst, callee, argc } => match reg!(callee).clone() {
                    Value::Fn(f) => {
                        let callee_proto = self.heap.closure(f).proto.clone();
                        debug_assert_eq!(callee_proto.arity, argc, "the type checker checks arity");
                        let new_base = base + callee as usize + 1;
                        reserve!(
                            new_base + callee_proto.num_regs as usize,
                            self.stack.frames.len() + 1
                        );
                        self.stack.frames.last_mut().unwrap().ip = ip;
                        self.stack.frames.push(Frame {
                            closure: f,
                            proto: callee_proto.clone(),
                            ip: 0,
                            base: new_base,
                            ret: base + dst as usize,
                            tail_call: None,
                        });
                        closure = f;
                        proto = callee_proto;
                        base = new_base;
                        ip = 0;
                        safepoint!(None);
                    }
                    Value::Native(_, f) => {
                        let start = base + callee as usize + 1;
                        match f(
                            &mut self.heap,
                            &self.stack.regs[start..start + argc as usize],
                        ) {
                            Ok(v) => reg!(dst) = v,
                            Err(msg) => throw!(msg),
                        }
                        // The result is in its register by now.
                        self.collect_after_native(None);
                        safepoint!(Some(ip - 1));
                    }
                    // A cancelled task stops before an intrinsic, which may be
                    // one it was woken from, except before ending a scope,
                    // which waits for the scope's tasks to stop first.
                    Value::Runtime(f) => {
                        if f == Intrinsic::CloseScope || !stopped!(Some(ip - 1)) {
                            self.stack.frames.last_mut().unwrap().ip = ip;
                            match self.intrinsic(f, base + callee as usize + 1, argc) {
                                Outcome::Value(v) => {
                                    reg!(dst) = v;
                                    self.collect_after_native(None);
                                    stopped!(Some(ip - 1));
                                }
                                Outcome::Wait => wait!(),
                                Outcome::Error(msg) => throw!(msg),
                            }
                        }
                    }
                    v => internal_error("Fn", &v),
                },
                Op::TailCall { callee, argc } => match reg!(callee).clone() {
                    Value::Fn(f) => {
                        let callee_proto = self.heap.closure(f).proto.clone();
                        debug_assert_eq!(callee_proto.arity, argc, "the type checker checks arity");
                        reserve!(
                            base + callee_proto.num_regs as usize,
                            self.stack.frames.len()
                        );
                        // Move the arguments down to the start of this frame's
                        // window, where the callee expects its parameters.
                        let args = base + callee as usize + 1;
                        for i in 0..argc as usize {
                            self.stack.regs.swap(base + i, args + i);
                        }
                        let frame = self.stack.frames.last_mut().unwrap();
                        frame.closure = f;
                        frame.proto = callee_proto.clone();
                        frame.tail_call = Some((proto.clone(), ip - 1));
                        closure = f;
                        proto = callee_proto;
                        ip = 0;
                        safepoint!(None);
                    }
                    Value::Native(_, f) => {
                        let start = base + callee as usize + 1;
                        match f(
                            &mut self.heap,
                            &self.stack.regs[start..start + argc as usize],
                        ) {
                            Ok(v) => {
                                self.collect_after_native(v.handle());
                                ret!(v)
                            }
                            Err(msg) => throw!(msg),
                        }
                    }
                    Value::Runtime(f) => {
                        if f == Intrinsic::CloseScope || !stopped!(Some(ip - 1)) {
                            self.stack.frames.last_mut().unwrap().ip = ip;
                            match self.intrinsic(f, base + callee as usize + 1, argc) {
                                Outcome::Value(v) => {
                                    self.collect_after_native(v.handle());
                                    ret!(v)
                                }
                                Outcome::Wait => wait!(),
                                Outcome::Error(msg) => throw!(msg),
                            }
                        }
                    }
                    v => internal_error("Fn", &v),
                },
                Op::Return { src } => ret!(reg!(src).clone()),
                Op::NoMatch => throw!("internal error: no arm of this match matched".to_string()),
                // A cancelled task has run a frame's deferred code; it goes
                // on stopping in the caller.
                Op::Unwind => unwind!(None),
            }
        }
    }

    /// Call the intrinsic `f`, whose `argc` arguments are in the registers
    /// from `args`.
    fn intrinsic(&mut self, f: Intrinsic, args: usize, argc: u8) -> Outcome {
        let args = self.stack.regs[args..args + argc as usize].to_vec();
        intrinsics::call(self, f, &args)
    }

    /// An error at the instruction the innermost frame just ran, with a
    /// trace of the calls in progress. Every frame's `ip` must be up to date.
    fn error(&self, message: String) -> RuntimeError {
        let top = self.stack.frames.last().unwrap();
        let span = top.proto.spans[top.ip - 1];

        // For each frame, innermost first, the call that started it, as the
        // calling function's code and the call instruction's index. A frame
        // a tail call reused records that call, since its original caller's
        // instruction no longer describes it. The outermost frame has none,
        // unless its function made a tail call. Tail calls are an
        // implementation detail, so the trace doesn't distinguish them.
        let calls =
            self.stack
                .frames
                .iter()
                .enumerate()
                .rev()
                .filter_map(|(i, frame)| match &frame.tail_call {
                    Some((proto, at)) => Some((proto, *at)),
                    None if i > 0 => {
                        let caller = &self.stack.frames[i - 1];
                        Some((&caller.proto, caller.ip - 1))
                    }
                    None => None,
                });

        let mut trace: Vec<TraceEntry> = Vec::new();
        let mut last: Option<(*const Proto, usize)> = None;
        for (proto, at) in calls {
            let key = (Rc::as_ptr(proto), at);
            if last == Some(key) {
                trace.last_mut().unwrap().count += 1;
                continue;
            }
            last = Some(key);
            let function = match proto.call_names.get(&at) {
                Some(name) => format!("function '{name}'"),
                None => "an anonymous function".to_string(),
            };
            trace.push(TraceEntry {
                function,
                site: proto.spans[at],
                count: 1,
            });
        }

        // A task's outermost frame is its function; where it was spawned,
        // and where the tasks it came from were, lead back to `main`.
        let notes = self
            .scheduler
            .origin()
            .iter()
            .rev()
            .map(|&at| Note {
                text: "in a task spawned at {}".to_string(),
                places: vec![at],
            })
            .collect();
        RuntimeError {
            message,
            span: Some(span),
            trace,
            notes,
        }
    }

    /// The error when every task is waiting for another, so none can go on,
    /// found while the running task waits (or finishes, if `finished`).
    /// It says what each task is waiting for, where, and where it was
    /// spawned. Every frame's `ip` must be up to date.
    fn deadlock(&self, finished: bool) -> RuntimeError {
        let mut notes = Vec::new();
        let mut describe = |what: &str, origin: &[Span], frames: &[Frame]| {
            let (who, mut places) = match origin.last() {
                Some(&at) => ("the task spawned at {}", vec![at]),
                None => ("the main task", vec![]),
            };
            match self.waiting_at(frames) {
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
                &self.stack.frames,
            );
        }
        for (what, origin, stack) in self.scheduler.waiting() {
            describe(what, origin, &stack.frames);
        }
        RuntimeError {
            message: "deadlock: every task is waiting for another task, so none can go on"
                .to_string(),
            span: None,
            trace: Vec::new(),
            notes,
        }
    }

    /// Where a waiting task, whose frames are `frames`, is waiting, in the
    /// program's own code: the call it waits in (its innermost frame is at
    /// it), or the call into the standard library that led there.
    fn waiting_at(&self, frames: &[Frame]) -> Option<Span> {
        let n = frames.len();
        let at = |i: usize, f: &Frame| f.proto.spans[if i + 1 == n { f.ip } else { f.ip - 1 }];
        let mine = |s: &Span| {
            !matches!(s.start.source, Source::Prelude) && !self.library.contains(&s.start.source)
        };
        frames
            .iter()
            .enumerate()
            .rev()
            .map(|(i, f)| at(i, f))
            .find(mine)
            .or_else(|| frames.last().map(|f| at(n - 1, f)))
    }
}

/// Whether a task whose frames are `frames` is running deferred code that
/// a block runs on its way out, in any of them, so it mustn't stop yet.
/// `top` is the instruction the innermost frame is at, if it has started.
fn in_deferred(frames: &[Frame], top: Option<usize>) -> bool {
    let n = frames.len();
    frames.iter().enumerate().any(|(i, f)| {
        let at = if i + 1 == n { top } else { f.ip.checked_sub(1) };
        at.is_some_and(|at| f.proto.in_deferred(at))
    })
}

/// How many of a task's registers can hold live values: those in some
/// frame's window. The rest hold only dead ones.
fn live_regs(regs: &[Value], frames: &[Frame]) -> usize {
    frames
        .iter()
        .map(|f| f.base + f.proto.num_regs as usize)
        .max()
        .unwrap_or(0)
        .min(regs.len())
}
