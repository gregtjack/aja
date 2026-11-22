//! The VM's instruction set and compiled functions.
//!
//! The VM is register-based: each call gets a window of registers, numbered
//! from 0, holding its parameters, locals, and temporaries. Instructions name
//! registers directly (`AddI { dst: 2, a: 0, b: 1 }`) instead of pushing and
//! popping an operand stack.
//!
//! Instructions are typed. The type checker has already decided whether a `+`
//! adds ints or floats, so the compiler emits `AddI` or `AddF` and the VM never
//! inspects operand types to choose what to do.

use std::{collections::HashMap, fmt, rc::Rc};

use crate::ast::{Id, Span};

use super::value::Value;

/// A register number, relative to the current call's window.
pub type Reg = u16;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    // ---- Constants and moves ----------------------------------------------
    LoadVoid { dst: Reg },
    LoadBool { dst: Reg, value: bool },
    LoadInt { dst: Reg, value: i32 },
    LoadFloat { dst: Reg, value: f32 },
    /// Load `consts[index]` (used for strings).
    LoadConst { dst: Reg, index: u32 },
    Move { dst: Reg, src: Reg },

    // ---- Globals: built-ins, top-level functions, REPL variables -------------
    GetGlobal { dst: Reg, index: u32 },
    SetGlobal { index: u32, src: Reg },

    // ---- Captured variables ----------------------------------------------------
    // A local that a closure captures lives in a cell, so the function that
    // declares it and every closure that captures it share one variable.
    /// Put the value in `src` into a new cell, stored in `dst`.
    NewCell { dst: Reg, src: Reg },
    GetCell { dst: Reg, cell: Reg },
    SetCell { cell: Reg, src: Reg },
    GetUpvalue { dst: Reg, index: u16 },
    SetUpvalue { index: u16, src: Reg },
    /// Make a closure from `protos[proto]`, capturing the cells its
    /// [`Proto::captures`] lists.
    Closure { dst: Reg, proto: u32 },

    // ---- Arithmetic --------------------------------------------------------------
    AddI { dst: Reg, a: Reg, b: Reg },
    SubI { dst: Reg, a: Reg, b: Reg },
    MulI { dst: Reg, a: Reg, b: Reg },
    DivI { dst: Reg, a: Reg, b: Reg },
    RemI { dst: Reg, a: Reg, b: Reg },
    NegI { dst: Reg, src: Reg },
    AddF { dst: Reg, a: Reg, b: Reg },
    SubF { dst: Reg, a: Reg, b: Reg },
    MulF { dst: Reg, a: Reg, b: Reg },
    DivF { dst: Reg, a: Reg, b: Reg },
    RemF { dst: Reg, a: Reg, b: Reg },
    NegF { dst: Reg, src: Reg },

    // ---- Comparison (`>` and `>=` swap the operands of `<` and `<=`) --------
    LtI { dst: Reg, a: Reg, b: Reg },
    LeI { dst: Reg, a: Reg, b: Reg },
    LtF { dst: Reg, a: Reg, b: Reg },
    LeF { dst: Reg, a: Reg, b: Reg },
    EqI { dst: Reg, a: Reg, b: Reg },
    EqF { dst: Reg, a: Reg, b: Reg },
    EqB { dst: Reg, a: Reg, b: Reg },
    EqS { dst: Reg, a: Reg, b: Reg },
    Not { dst: Reg, src: Reg },

    // ---- Control flow -------------------------------------------------------------
    Jump { target: u32 },
    JumpIfFalse { cond: Reg, target: u32 },
    JumpIfTrue { cond: Reg, target: u32 },
    /// Call the function in `callee` with the `argc` arguments in the
    /// registers right after it, and put the result in `dst`. The callee's
    /// register window starts at the first argument, so arguments become its
    /// parameters without being copied.
    Call { dst: Reg, callee: Reg, argc: u8 },
    Return { src: Reg },
}

/// Where a closure gets one of its captured cells when it is created.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Capture {
    /// A register of the function creating the closure; it holds a cell.
    Local(Reg),
    /// One of the creating function's own captured cells.
    Upvalue(u16),
}

/// A compiled function: a top-level function, a closure, or a REPL input.
#[derive(Debug)]
pub struct Proto {
    /// `None` for closures and REPL inputs.
    pub name: Option<Id>,
    /// The parameters and return type as written, e.g. `(x: int) -> int`.
    pub signature: String,
    pub arity: u8,
    /// How many registers a call needs.
    pub num_regs: u16,
    pub code: Vec<Op>,
    /// The source span each instruction came from, for error messages.
    pub spans: Vec<Span>,
    pub consts: Vec<Value>,
    /// Closures defined inside this function.
    pub protos: Vec<Rc<Proto>>,
    /// The cells a closure made from this proto captures, in upvalue order.
    pub captures: Vec<Capture>,
    /// For `Call` instructions whose callee is a name, that name (by
    /// instruction index), to describe the call in error traces.
    pub call_names: HashMap<usize, Id>,
}

impl fmt::Display for Proto {
    /// A listing of the bytecode, for debugging and tests.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.name.as_deref().unwrap_or("<anonymous>");
        writeln!(f, "fn {name}{} ({} registers)", self.signature, self.num_regs)?;
        for (i, c) in self.captures.iter().enumerate() {
            writeln!(f, "  capture {i}: {c:?}")?;
        }
        for (i, op) in self.code.iter().enumerate() {
            writeln!(f, "  {i:4}  {op:?}")?;
        }
        for p in &self.protos {
            write!(f, "{p}")?;
        }
        Ok(())
    }
}
