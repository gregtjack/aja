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
    diagnostic::{Diagnostic, Files},
};

pub mod bytecode;
pub mod compile;
pub mod hamt;
pub mod heap;
pub mod list;
mod native;
mod net;
pub use native::OUTPUT_CLOSED;
mod scheduler;
pub mod sorted;
pub mod value;

#[cfg(test)]
mod tests;

use bytecode::{Capture, Layout, Literals, Op, Proto};
pub use compile::{Compiled, Compiler, ReplInput};
use heap::{Closure, Gc, Heap, MAX_STR_LEN};
use scheduler::{Intrinsic, Next, Outcome, Scheduler};
use value::Value;

/// Whether the VM implements the native function `name`.
#[cfg(test)]
pub fn native_implemented(name: &str) -> bool {
    native::implementation(name).is_some() || Intrinsic::named(name).is_some()
}

/// How much memory the call stack (frames and their registers) may use
/// before the VM reports a stack overflow: 256 MiB.
pub const DEFAULT_STACK_LIMIT: usize = 256 << 20;

/// An error that stops the program.
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
    /// The expression that failed.
    pub span: Option<Span>,
    /// The calls in progress, innermost first, with runs of identical calls
    /// (deep recursion) collapsed into one entry.
    pub trace: Vec<TraceEntry>,
}

/// One or more identical calls in an error's trace.
#[derive(Debug, Clone)]
pub struct TraceEntry {
    /// e.g. "function 'f'" or "an anonymous function".
    pub function: String,
    /// The call expression.
    pub site: Span,
    /// How many times in a row this call is on the stack.
    pub count: usize,
}

impl RuntimeError {
    /// The error as a diagnostic, for a program in one file.
    pub fn to_diagnostic(&self) -> Diagnostic {
        self.to_diagnostic_in(&Files::default())
    }

    /// The error as a diagnostic, naming which of `files` each call in its
    /// trace is in, when it isn't the file being run.
    pub fn to_diagnostic_in(&self, files: &Files) -> Diagnostic {
        const MAX_NOTES: usize = 8;
        let mut notes: Vec<String> = self
            .trace
            .iter()
            .take(MAX_NOTES)
            .map(|e| {
                let repeat = if e.count > 1 {
                    format!(" ({} times)", e.count)
                } else {
                    String::new()
                };
                let place = match e.site.start.source {
                    Source::MAIN => String::new(),
                    Source::Prelude => " in the prelude".to_string(),
                    file => format!(" in {}", files.path(file)),
                };
                format!(
                    "in {}, called{place} at line {}, column {}{repeat}",
                    e.function, e.site.start.line, e.site.start.col
                )
            })
            .collect();
        if self.trace.len() > MAX_NOTES {
            notes.push(format!(
                "... and {} more calls",
                self.trace.len() - MAX_NOTES
            ));
        }

        let d = Diagnostic::new(self.message.clone()).with_notes(notes);
        match self.span {
            Some(span) => d.with_span(span.start, span.end),
            None => d,
        }
    }
}

/// A call in progress.
struct Frame {
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

pub struct Vm {
    pub heap: Heap,
    globals: Vec<Value>,
    /// The string literals, by number; see [`Literals`]. They are roots.
    literals: Vec<Value>,
    /// The running task's registers, every call's one window after another.
    regs: Vec<Value>,
    /// The running task's calls in progress.
    frames: Vec<Frame>,
    /// The other tasks, and which runs next.
    scheduler: Scheduler,
    /// The most memory, in bytes, each task's call stack may use.
    pub stack_limit: usize,
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
            regs: Vec::new(),
            frames: Vec::new(),
            scheduler: Scheduler::new(),
            stack_limit: DEFAULT_STACK_LIMIT,
        }
    }

    /// Define a compiled program's functions, then run `main` and return
    /// its result.
    pub fn run_program(&mut self, program: &Compiled) -> Result<Value, RuntimeError> {
        self.load_literals(&program.literals);
        self.load_functions(&program.functions);
        self.load_natives(&program.natives);
        let Value::Fn(main) = self.globals[program.main as usize] else {
            unreachable!("'main' is a function");
        };
        let value = self.run(main)?;
        if let (Some(span), Value::Data(gc)) = (program.main_result, &value) {
            let (layout, fields) = self.heap.data(*gc);
            // `Result::Err` is the second variant.
            if layout.tag == 1 {
                return Err(RuntimeError {
                    message: format!("main returned an error: {}", self.heap.repr(&fields[0])),
                    span: Some(span),
                    trace: Vec::new(),
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

    /// Collect, keeping `extra` and everything it reaches alive too.
    fn collect_garbage_with(&mut self, extra: Vec<Gc>) {
        // Every task's registers and frames, the running one's included.
        // Registers above every frame's window hold only dead values; inside
        // one, a register not yet written may hold a stale handle (see
        // `Heap::collect`).
        let stacks =
            std::iter::once((&self.regs[..], &self.frames[..])).chain(self.scheduler.stacks());
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
            self.scheduler.spawn(closure, proto, None, None);
        }
        self.scheduler.start(&mut self.regs, &mut self.frames);
        let result = self.execute();
        self.frames.clear();
        self.regs.clear();
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
        let frame = self.frames.last().unwrap();
        let mut closure = frame.closure;
        let mut proto = frame.proto.clone();
        let mut base = frame.base;
        let mut ip = frame.ip;
        let mut budget = self.scheduler.slice();

        // Pick up where the innermost frame is, after a switch.
        macro_rules! load {
            () => {{
                let frame = self.frames.last().unwrap();
                closure = frame.closure;
                proto = frame.proto.clone();
                base = frame.base;
                ip = frame.ip;
            }};
        }
        // Count a safepoint, and end the task's turn once its budget runs
        // out. `ip` must be where this task goes on.
        macro_rules! safepoint {
            () => {{
                budget -= 1;
                if budget == 0 {
                    budget = self.scheduler.slice();
                    self.frames.last_mut().unwrap().ip = ip;
                    if self.scheduler.end_turn(&mut self.regs, &mut self.frames) {
                        load!();
                    }
                }
            }};
        }

        macro_rules! reg {
            ($r:expr) => {
                self.regs[base + $r as usize]
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
                self.frames.last_mut().unwrap().ip = ip;
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
                        self.frames.len(),
                    ));
                }
                if self.regs.len() < needed {
                    self.regs.resize(needed, Value::Void);
                }
            }};
        }
        // Return `v` from the innermost frame. If it is the task's last,
        // the task is done, and the next one runs.
        macro_rules! ret {
            ($v:expr) => {{
                let v = $v;
                let done = self.frames.pop().unwrap();
                if self.frames.is_empty() {
                    let next =
                        self.scheduler
                            .finish(v, &mut self.heap, &mut self.regs, &mut self.frames);
                    match next {
                        Next::Switched => {
                            load!();
                            budget = self.scheduler.slice();
                        }
                        Next::Finished => return Ok(()),
                        Next::Deadlock => return Err(deadlock(None)),
                    }
                } else {
                    load!();
                    self.regs[done.ret] = v;
                }
            }};
        }
        // Make the running task wait. The call it is making, the
        // instruction before `ip`, runs again once the task is woken.
        macro_rules! wait {
            () => {{
                self.frames.last_mut().unwrap().ip = ip - 1;
                match self.scheduler.wait(&mut self.regs, &mut self.frames) {
                    Next::Switched => {
                        load!();
                        budget = self.scheduler.slice();
                    }
                    Next::Finished | Next::Deadlock => {
                        self.frames.last_mut().unwrap().ip = ip;
                        let mut err = self.error(String::new());
                        err.message = deadlock(None).message;
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
                    let fields: Box<[Value]> = self.regs[from..from + count as usize].into();
                    let layout = proto.layouts[layout as usize].clone();
                    let data = self.alloc_data(layout, fields);
                    reg!(dst) = Value::Data(data);
                }
                Op::NewList { dst, start, count } => {
                    let from = base + start as usize;
                    let list: list::List = self.regs[from..from + count as usize]
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
                    int_op!(dst, a, b, checked_rem, "%")
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
                    let back = (target as usize) < ip;
                    ip = target as usize;
                    // Every loop jumps back to its start.
                    if back {
                        safepoint!();
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
                            self.frames.len() + 1
                        );
                        self.frames.last_mut().unwrap().ip = ip;
                        self.frames.push(Frame {
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
                        safepoint!();
                    }
                    Value::Native(_, f) => {
                        let start = base + callee as usize + 1;
                        match f(&mut self.heap, &self.regs[start..start + argc as usize]) {
                            Ok(v) => reg!(dst) = v,
                            Err(msg) => throw!(msg),
                        }
                        safepoint!();
                    }
                    Value::Runtime(f) => match self.intrinsic(f, base + callee as usize + 1) {
                        Outcome::Value(v) => reg!(dst) = v,
                        Outcome::Wait => wait!(),
                        Outcome::Error(msg) => throw!(msg),
                    },
                    v => internal_error("Fn", &v),
                },
                Op::TailCall { callee, argc } => match reg!(callee).clone() {
                    Value::Fn(f) => {
                        let callee_proto = self.heap.closure(f).proto.clone();
                        debug_assert_eq!(callee_proto.arity, argc, "the type checker checks arity");
                        reserve!(base + callee_proto.num_regs as usize, self.frames.len());
                        // Move the arguments down to the start of this frame's
                        // window, where the callee expects its parameters.
                        let args = base + callee as usize + 1;
                        for i in 0..argc as usize {
                            self.regs.swap(base + i, args + i);
                        }
                        let frame = self.frames.last_mut().unwrap();
                        frame.closure = f;
                        frame.proto = callee_proto.clone();
                        frame.tail_call = Some((proto.clone(), ip - 1));
                        closure = f;
                        proto = callee_proto;
                        ip = 0;
                        safepoint!();
                    }
                    Value::Native(_, f) => {
                        let start = base + callee as usize + 1;
                        match f(&mut self.heap, &self.regs[start..start + argc as usize]) {
                            Ok(v) => ret!(v),
                            Err(msg) => throw!(msg),
                        }
                    }
                    Value::Runtime(f) => match self.intrinsic(f, base + callee as usize + 1) {
                        Outcome::Value(v) => ret!(v),
                        Outcome::Wait => wait!(),
                        Outcome::Error(msg) => throw!(msg),
                    },
                    v => internal_error("Fn", &v),
                },
                Op::Return { src } => ret!(reg!(src).clone()),
                Op::NoMatch => throw!("internal error: no arm of this match matched".to_string()),
            }
        }
    }

    /// Call the intrinsic `f`, whose arguments are in the registers from
    /// `args`.
    fn intrinsic(&mut self, f: Intrinsic, args: usize) -> Outcome {
        let arg = |vm: &Self, i: usize| vm.regs[args + i].clone();
        match f {
            Intrinsic::OpenScope => {
                if self.heap.wants_collection(0) {
                    self.collect_garbage_with(Vec::new());
                }
                Outcome::Value(Value::Scope(self.heap.alloc_scope()))
            }
            Intrinsic::CloseScope => {
                let Value::Scope(s) = arg(self, 0) else {
                    internal_error("Scope", &arg(self, 0));
                };
                let current = self.scheduler.current();
                let scope = self.heap.scope(s);
                if scope.live == 0 {
                    scope.ended = true;
                    Outcome::Value(Value::Void)
                } else {
                    scope.closer = Some(current);
                    Outcome::Wait
                }
            }
            Intrinsic::Spawn => {
                let (Value::Scope(s), Value::Fn(f)) = (arg(self, 0), arg(self, 1)) else {
                    internal_error("Scope and Fn", &arg(self, 1));
                };
                if self.heap.scope(s).ended {
                    return Outcome::Error(
                        "cannot spawn a task in a scope that has ended: a Scope can only be \
                         used inside the `task::scope` call that made it"
                            .to_string(),
                    );
                }
                // The scope and the function are still in their registers,
                // so a collection here keeps them.
                if self.heap.wants_collection(0) {
                    self.collect_garbage_with(Vec::new());
                }
                let handle = self.heap.alloc_task();
                self.heap.scope(s).live += 1;
                let proto = self.heap.closure(f).proto.clone();
                self.scheduler.spawn(f, proto, Some(handle), Some(s));
                Outcome::Value(Value::Task(handle))
            }
            Intrinsic::Sleep | Intrinsic::SleepNanos => {
                let Value::Int(n) = arg(self, 0) else {
                    internal_error("Int", &arg(self, 0));
                };
                let (unit, duration): (_, fn(u64) -> std::time::Duration) = if f == Intrinsic::Sleep
                {
                    ("milliseconds", std::time::Duration::from_millis)
                } else {
                    ("nanoseconds", std::time::Duration::from_nanos)
                };
                if n < 0 {
                    return Outcome::Error(format!(
                        "cannot sleep for {n} {unit}: the time must be 0 or more"
                    ));
                }
                // The call runs again when the task wakes, and then returns.
                if self.scheduler.take_slept() || n == 0 {
                    return Outcome::Value(Value::Void);
                }
                self.scheduler.set_timer(duration(n as u64));
                Outcome::Wait
            }
            Intrinsic::Net(op) => {
                let args = self.regs[args..args + op.arity()].to_vec();
                let task = self.scheduler.current();
                let outcome = self.scheduler.net.call(op, task, &mut self.heap, &args);
                self.scheduler.wake_net_woken();
                outcome
            }
            Intrinsic::Join => {
                let Value::Task(t) = arg(self, 0) else {
                    internal_error("Task", &arg(self, 0));
                };
                let current = self.scheduler.current();
                let task = self.heap.task(t);
                match &task.result {
                    Some(v) => Outcome::Value(v.clone()),
                    None => {
                        task.joiners.push(current);
                        Outcome::Wait
                    }
                }
            }
        }
    }

    /// An error at the instruction the innermost frame just ran, with a
    /// trace of the calls in progress. Every frame's `ip` must be up to date.
    fn error(&self, message: String) -> RuntimeError {
        let top = self.frames.last().unwrap();
        let span = top.proto.spans[top.ip - 1];

        // For each frame, innermost first, the call that started it, as the
        // calling function's code and the call instruction's index. A frame
        // a tail call reused records that call, since its original caller's
        // instruction no longer describes it. The outermost frame has none,
        // unless its function made a tail call. Tail calls are an
        // implementation detail, so the trace doesn't distinguish them.
        let calls = self
            .frames
            .iter()
            .enumerate()
            .rev()
            .filter_map(|(i, frame)| match &frame.tail_call {
                Some((proto, at)) => Some((proto, *at)),
                None if i > 0 => {
                    let caller = &self.frames[i - 1];
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

        RuntimeError {
            message,
            span: Some(span),
            trace,
        }
    }
}

/// The error when every task is waiting for another, so none can go on.
/// `span` is the call the running task is waiting in, if it is waiting.
fn deadlock(span: Option<Span>) -> RuntimeError {
    RuntimeError {
        message: "deadlock: every task is waiting for another task, so none can go on".to_string(),
        span,
        trace: Vec::new(),
    }
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

/// A register held the wrong kind of value. The type checker rules this
/// out, so it is a bug in the compiler or VM.
#[cold]
fn internal_error(expected: &str, found: &Value) -> ! {
    panic!("internal error: expected {expected} in a register, found {found:?}")
}
