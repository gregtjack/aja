//! A register-based bytecode VM.
//!
//! [`compile`] turns the type-checked AST into [`bytecode`]; [`Vm`] runs it.
//! Call frames live on the heap (in [`Vm::frames`]), not on the Rust stack,
//! so deep recursion in Aja can't overflow the VM's own stack.

use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::Span,
    builtins::BUILTINS,
    diagnostic::Diagnostic,
};

pub mod bytecode;
pub mod compile;
mod native;
pub mod value;

#[cfg(test)]
mod tests;

use bytecode::{Capture, Op, Proto};
pub use compile::{Compiled, Compiler};
use value::{Closure, Value};

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
    closure: Rc<Closure>,
    /// The next instruction to run. Only up to date for frames other than
    /// the innermost one, which keeps its own copy while it runs.
    ip: usize,
    /// Where the call's register window starts in [`Vm::regs`].
    base: usize,
    /// Where to put the result, in [`Vm::regs`].
    ret: usize,
}

pub struct Vm {
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
            globals,
            regs: Vec::new(),
            frames: Vec::new(),
        }
    }

    /// Define a compiled program's functions, then run `main` and return
    /// its result.
    pub fn run_program(&mut self, program: &Compiled) -> Result<Value, RuntimeError> {
        for (slot, proto) in &program.functions {
            let f = Value::Fn(Rc::new(Closure {
                proto: proto.clone(),
                upvalues: Box::new([]),
            }));
            self.set_global(*slot, f);
        }
        let Value::Fn(main) = self.globals[program.main as usize].clone() else {
            unreachable!("'main' is a function");
        };
        self.run(main)
    }

    /// Run a compiled REPL input and return its value.
    pub fn run_repl(&mut self, input: Rc<Proto>) -> Result<Value, RuntimeError> {
        self.run(Rc::new(Closure {
            proto: input,
            upvalues: Box::new([]),
        }))
    }

    fn set_global(&mut self, slot: u32, value: Value) {
        let slot = slot as usize;
        if slot >= self.globals.len() {
            self.globals.resize(slot + 1, Value::Void);
        }
        self.globals[slot] = value;
    }

    /// Call `closure`, which takes no arguments.
    fn run(&mut self, closure: Rc<Closure>) -> Result<Value, RuntimeError> {
        self.frames.clear();
        self.regs.clear();
        self.regs.resize(closure.proto.num_regs as usize, Value::Void);
        self.frames.push(Frame {
            closure,
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
        let mut closure = frame.closure.clone();
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
            let op = closure.proto.code[ip];
            ip += 1;
            match op {
                Op::LoadVoid { dst } => reg!(dst) = Value::Void,
                Op::LoadBool { dst, value } => reg!(dst) = Value::Bool(value),
                Op::LoadInt { dst, value } => reg!(dst) = Value::Int(value),
                Op::LoadFloat { dst, value } => reg!(dst) = Value::Float(value),
                Op::LoadConst { dst, index } => {
                    reg!(dst) = closure.proto.consts[index as usize].clone()
                }
                Op::Move { dst, src } => reg!(dst) = reg!(src).clone(),

                Op::GetGlobal { dst, index } => reg!(dst) = self.globals[index as usize].clone(),
                Op::SetGlobal { index, src } => {
                    let v = reg!(src).clone();
                    self.set_global(index, v);
                }

                Op::NewCell { dst, src } => {
                    let v = reg!(src).clone();
                    reg!(dst) = Value::Cell(Rc::new(RefCell::new(v)));
                }
                Op::GetCell { dst, cell } => {
                    let v = get!(Cell, cell).borrow().clone();
                    reg!(dst) = v;
                }
                Op::SetCell { cell, src } => {
                    let v = reg!(src).clone();
                    *get!(Cell, cell).borrow_mut() = v;
                }
                Op::GetUpvalue { dst, index } => {
                    reg!(dst) = closure.upvalues[index as usize].borrow().clone()
                }
                Op::SetUpvalue { index, src } => {
                    *closure.upvalues[index as usize].borrow_mut() = reg!(src).clone()
                }
                Op::Closure { dst, proto } => {
                    let proto = closure.proto.protos[proto as usize].clone();
                    let upvalues = proto
                        .captures
                        .iter()
                        .map(|c| match c {
                            Capture::Local(r) => get!(Cell, *r),
                            Capture::Upvalue(i) => closure.upvalues[*i as usize].clone(),
                        })
                        .collect();
                    reg!(dst) = Value::Fn(Rc::new(Closure { proto, upvalues }));
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
                        if self.frames.len() >= MAX_FRAMES {
                            throw!(format!("stack overflow: more than {MAX_FRAMES} calls in progress"));
                        }
                        debug_assert_eq!(f.proto.arity, argc, "the type checker checks arity");
                        self.frames.last_mut().unwrap().ip = ip;
                        let new_base = base + callee as usize + 1;
                        let needed = new_base + f.proto.num_regs as usize;
                        if self.regs.len() < needed {
                            self.regs.resize(needed, Value::Void);
                        }
                        self.frames.push(Frame {
                            closure: f.clone(),
                            ip: 0,
                            base: new_base,
                            ret: base + dst as usize,
                        });
                        closure = f;
                        base = new_base;
                        ip = 0;
                    }
                    Value::Native(_, f) => {
                        let start = base + callee as usize + 1;
                        match f(&self.regs[start..start + argc as usize]) {
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
                    closure = caller.closure.clone();
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
            (&frame.closure.proto, frame.ip - 1)
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
