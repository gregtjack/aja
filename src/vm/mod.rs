//! A register-based bytecode VM.
//!
//! [`compile`] turns the type-checked AST into [`bytecode`]; [`Vm`] runs it.
//! Call frames are kept in [`Vm::frames`], not on the Rust stack, so deep
//! recursion in Aja can't overflow the VM's own stack. Closures and captured
//! variables live on a garbage-collected [`heap`].

use std::rc::Rc;

use crate::{
    ast::Span,
    builtins::BUILTINS,
    diagnostic::Diagnostic,
};

pub mod bytecode;
pub mod compile;
pub mod heap;
mod native;
pub mod value;

#[cfg(test)]
mod tests;

use bytecode::{Capture, Op, Proto};
pub use compile::{Compiled, Compiler};
use heap::{Closure, Gc, Heap};
use value::Value;

/// How many calls can be in progress at once before the VM reports a stack
/// overflow.
const MAX_FRAMES: usize = 100_000;

/// An error that stops the program.
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
    /// The expression that failed.
    pub span: Option<Span>,
    /// Calls the error unwound through, innermost first: (a description of
    /// the function, where it was called).
    pub trace: Vec<(String, Span)>,
}

impl RuntimeError {
    pub fn to_diagnostic(&self) -> Diagnostic {
        // Collapse runs of identical frames (e.g. deep recursion) into one note.
        let mut frames: Vec<(&str, Span, usize)> = Vec::new();
        for (function, site) in &self.trace {
            match frames.last_mut() {
                Some((f, s, count)) if *f == function.as_str() && *s == *site => *count += 1,
                _ => frames.push((function, *site, 1)),
            }
        }
        const MAX_NOTES: usize = 8;
        let mut notes: Vec<String> = frames
            .iter()
            .take(MAX_NOTES)
            .map(|(function, site, count)| {
                let repeat = if *count > 1 {
                    format!(" ({count} times)")
                } else {
                    String::new()
                };
                format!(
                    "in {function}, called at line {}, column {}{repeat}",
                    site.start.line, site.start.col
                )
            })
            .collect();
        if frames.len() > MAX_NOTES {
            notes.push(format!("... and {} more calls", frames.len() - MAX_NOTES));
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
}

pub struct Vm {
    pub heap: Heap,
    globals: Vec<Value>,
    /// Every call's registers, one window after another.
    regs: Vec<Value>,
    frames: Vec<Frame>,
}

impl Vm {
    /// A VM with the built-ins defined, in the slots [`Compiler::new`] expects.
    pub fn new() -> Self {
        let globals = BUILTINS
            .iter()
            .map(|b| {
                let f = native::implementation(b.name)
                    .unwrap_or_else(|| panic!("built-in '{}' has no implementation in the VM", b.name));
                Value::Native(b, f)
            })
            .collect();
        Vm {
            heap: Heap::new(),
            globals,
            regs: Vec::new(),
            frames: Vec::new(),
        }
    }

    /// Define a compiled program's functions, then run `main` and return
    /// its result.
    pub fn run_program(&mut self, program: &Compiled) -> Result<Value, RuntimeError> {
        for (slot, proto) in &program.functions {
            let f = self.alloc_closure(proto.clone(), Box::new([]));
            self.set_global(*slot, Value::Fn(f));
        }
        let Value::Fn(main) = self.globals[program.main as usize] else {
            unreachable!("'main' is a function");
        };
        self.run(main)
    }

    /// Run a compiled REPL input and return its value.
    pub fn run_repl(&mut self, input: Rc<Proto>) -> Result<Value, RuntimeError> {
        let f = self.alloc_closure(input, Box::new([]));
        self.run(f)
    }

    /// Free every object the program can no longer reach.
    #[cfg(test)]
    pub fn collect_garbage(&mut self) {
        self.collect_garbage_with(Vec::new());
    }

    // Allocation may collect, so every value still in use must be in a
    // register, global, or frame, or passed in, when these are called.

    fn alloc_closure(&mut self, proto: Rc<Proto>, upvalues: Box<[Gc]>) -> Gc {
        if self.heap.wants_collection() {
            let extra = upvalues.iter().copied().collect::<Vec<_>>();
            self.collect_garbage_with(extra);
        }
        self.heap.alloc_closure(Closure { proto, upvalues })
    }

    fn alloc_cell(&mut self, value: Value) -> Gc {
        if self.heap.wants_collection() {
            self.collect_garbage_with(value.handle().into_iter().collect());
        }
        self.heap.alloc_cell(value)
    }

    /// Collect, keeping `extra` and everything it reaches alive too.
    fn collect_garbage_with(&mut self, extra: Vec<Gc>) {
        // Registers above every frame's window hold only dead values.
        let live_regs = self
            .frames
            .iter()
            .map(|f| f.base + f.proto.num_regs as usize)
            .max()
            .unwrap_or(0)
            .min(self.regs.len());
        let roots: Vec<Gc> = self.frames.iter().map(|f| f.closure).chain(extra).collect();
        self.heap
            .collect(self.regs[..live_regs].iter().chain(&self.globals), roots);
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
        let proto = self.heap.closure(closure).proto.clone();
        self.frames.clear();
        self.regs.clear();
        self.regs.resize(proto.num_regs as usize, Value::Void);
        self.frames.push(Frame {
            closure,
            proto,
            ip: 0,
            base: 0,
            ret: 0,
        });
        let result = self.execute();
        self.frames.clear();
        self.regs.clear();
        result
    }

    /// Run until the outermost frame returns.
    fn execute(&mut self) -> Result<Value, RuntimeError> {
        let frame = self.frames.last().unwrap();
        let mut closure = frame.closure;
        let mut proto = frame.proto.clone();
        let mut base = frame.base;
        let mut ip = frame.ip;

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
                Op::LoadConst { dst, index } => {
                    reg!(dst) = proto.consts[index as usize].clone()
                }
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
                    let cell = self.heap.closure(closure).upvalues[index as usize];
                    reg!(dst) = self.heap.cell(cell).clone();
                }
                Op::SetUpvalue { index, src } => {
                    let cell = self.heap.closure(closure).upvalues[index as usize];
                    let v = reg!(src).clone();
                    self.heap.set_cell(cell, v);
                }
                Op::Closure { dst, proto: index } => {
                    let inner = proto.protos[index as usize].clone();
                    let upvalues = inner
                        .captures
                        .iter()
                        .map(|c| match c {
                            Capture::Local(r) => get!(Cell, *r),
                            Capture::Upvalue(i) => self.heap.closure(closure).upvalues[*i as usize],
                        })
                        .collect();
                    let f = self.alloc_closure(inner, upvalues);
                    reg!(dst) = Value::Fn(f);
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

                Op::LtI { dst, a, b } => compare!(dst, a, b, Int, <),
                Op::LeI { dst, a, b } => compare!(dst, a, b, Int, <=),
                Op::LtF { dst, a, b } => compare!(dst, a, b, Float, <),
                Op::LeF { dst, a, b } => compare!(dst, a, b, Float, <=),
                Op::EqI { dst, a, b } => compare!(dst, a, b, Int, ==),
                Op::EqF { dst, a, b } => compare!(dst, a, b, Float, ==),
                Op::EqB { dst, a, b } => compare!(dst, a, b, Bool, ==),
                Op::EqS { dst, a, b } => compare!(dst, a, b, Str, ==),
                Op::Not { dst, src } => reg!(dst) = Value::Bool(!get!(Bool, src)),

                Op::Jump { target } => ip = target as usize,
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
                        if self.frames.len() >= MAX_FRAMES {
                            throw!(format!("stack overflow: more than {MAX_FRAMES} calls in progress"));
                        }
                        debug_assert_eq!(callee_proto.arity, argc, "the type checker checks arity");
                        self.frames.last_mut().unwrap().ip = ip;
                        let new_base = base + callee as usize + 1;
                        let needed = new_base + callee_proto.num_regs as usize;
                        if self.regs.len() < needed {
                            self.regs.resize(needed, Value::Void);
                        }
                        self.frames.push(Frame {
                            closure: f,
                            proto: callee_proto.clone(),
                            ip: 0,
                            base: new_base,
                            ret: base + dst as usize,
                        });
                        closure = f;
                        proto = callee_proto;
                        base = new_base;
                        ip = 0;
                    }
                    Value::Native(_, f) => {
                        let start = base + callee as usize + 1;
                        match f(&self.heap, &self.regs[start..start + argc as usize]) {
                            Ok(v) => reg!(dst) = v,
                            Err(msg) => throw!(msg),
                        }
                    }
                    v => internal_error("Fn", &v),
                },
                Op::Return { src } => {
                    let v = reg!(src).clone();
                    let done = self.frames.pop().unwrap();
                    let Some(caller) = self.frames.last() else {
                        return Ok(v);
                    };
                    closure = caller.closure;
                    proto = caller.proto.clone();
                    base = caller.base;
                    ip = caller.ip;
                    self.regs[done.ret] = v;
                }
            }
        }
    }

    /// An error at the instruction the innermost frame just ran, with a
    /// trace of the calls in progress. Every frame's `ip` must be up to date.
    fn error(&self, message: String) -> RuntimeError {
        fn at(frame: &Frame) -> (&Proto, usize) {
            (&frame.proto, frame.ip - 1)
        }
        let (proto, ip) = at(self.frames.last().unwrap());
        let span = proto.spans[ip];

        let trace = (1..self.frames.len())
            .rev()
            .map(|i| {
                let (caller, call) = at(&self.frames[i - 1]);
                let function = match caller.call_names.get(&call) {
                    Some(name) => format!("function '{name}'"),
                    None => "an anonymous function".to_string(),
                };
                (function, caller.spans[call])
            })
            .collect();

        RuntimeError {
            message,
            span: Some(span),
            trace,
        }
    }
}

/// A register held the wrong kind of value. The type checker rules this
/// out, so it is a bug in the compiler or VM.
#[cold]
fn internal_error(expected: &str, found: &Value) -> ! {
    panic!("internal error: expected {expected} in a register, found {found:?}")
}
