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

/// A register number, relative to the current call's window.
pub type Reg = u16;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    // ---- Constants and moves ----------------------------------------------
    LoadVoid {
        dst: Reg,
    },
    LoadBool {
        dst: Reg,
        value: bool,
    },
    LoadInt {
        dst: Reg,
        value: i64,
    },
    LoadFloat {
        dst: Reg,
        value: f64,
    },
    /// Load the string literal numbered `index` (see [`Literals`]).
    LoadStr {
        dst: Reg,
        index: u32,
    },
    Move {
        dst: Reg,
        src: Reg,
    },

    // ---- Globals: built-ins, top-level functions, REPL variables -------------
    GetGlobal {
        dst: Reg,
        index: u32,
    },
    SetGlobal {
        index: u32,
        src: Reg,
    },

    // ---- Captured variables ----------------------------------------------------
    // A closure copies the variables it captures when it is made. A `var`
    // that a closure captures lives in a cell, and the closure copies the
    // cell, so the function that declares the `var` and every closure that
    // captures it share one variable. Immutable variables need no cell.
    /// Put the value in `src` into a new cell, stored in `dst`.
    NewCell {
        dst: Reg,
        src: Reg,
    },
    GetCell {
        dst: Reg,
        cell: Reg,
    },
    SetCell {
        cell: Reg,
        src: Reg,
    },
    /// Load the current closure's captured value number `index`.
    GetUpvalue {
        dst: Reg,
        index: u16,
    },
    /// Load the value in the cell the current closure captured as number
    /// `index`.
    GetUpvalueCell {
        dst: Reg,
        index: u16,
    },
    /// Store into the cell the current closure captured as number `index`.
    SetUpvalueCell {
        index: u16,
        src: Reg,
    },
    /// Make a closure from `protos[proto]`, capturing the values its
    /// [`Proto::captures`] lists.
    Closure {
        dst: Reg,
        proto: u32,
    },

    // ---- Structs and enums ---------------------------------------------------
    /// Make a struct or enum value described by `layouts[layout]`, holding
    /// the `count` values in the registers from `start` on.
    NewData {
        dst: Reg,
        layout: u32,
        start: Reg,
        count: u16,
    },
    /// Read the field (or variant value) at `index` of the value in `obj`.
    GetField {
        dst: Reg,
        obj: Reg,
        index: u16,
    },
    /// Make a list of the `count` values in the registers from `start` on.
    NewList {
        dst: Reg,
        start: Reg,
        count: u32,
    },
    /// Read the value at `index` (an int) of the list in `list`, or stop
    /// the program if there is none.
    Index {
        dst: Reg,
        list: Reg,
        index: Reg,
    },
    /// Whether the enum value in `obj` is the variant numbered `tag`.
    TestTag {
        dst: Reg,
        obj: Reg,
        tag: u32,
    },

    // ---- Arithmetic --------------------------------------------------------------
    AddI {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    SubI {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    MulI {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    DivI {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    RemI {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    NegI {
        dst: Reg,
        src: Reg,
    },
    AddF {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    SubF {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    MulF {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    DivF {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    RemF {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    NegF {
        dst: Reg,
        src: Reg,
    },
    /// Concatenate two strings into a new one.
    AddS {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    // ---- Comparison (`>` and `>=` swap the operands of `<` and `<=`) --------
    LtI {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    LeI {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    LtF {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    LeF {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    EqI {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    EqF {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    EqB {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    EqS {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Structural equality of two values of the same struct or enum type.
    EqD {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Whether `a` comes before `b`, for two values of the same `Ord` type
    /// other than int (see `Heap::compare`).
    LtD {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    /// Whether `a` comes before `b` or is equal to it, as for `LtD`.
    LeD {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    Not {
        dst: Reg,
        src: Reg,
    },

    // ---- Control flow -------------------------------------------------------------
    Jump {
        target: u32,
    },
    JumpIfFalse {
        cond: Reg,
        target: u32,
    },
    JumpIfTrue {
        cond: Reg,
        target: u32,
    },
    /// Call the function in `callee` with the `argc` arguments in the
    /// registers right after it, and put the result in `dst`. The callee's
    /// register window starts at the first argument, so arguments become its
    /// parameters without being copied.
    Call {
        dst: Reg,
        callee: Reg,
        argc: u8,
    },
    /// Call the function in `callee` and return its result. The call reuses
    /// the current frame, so tail calls run in constant space however deep
    /// they recurse. The compiler emits it for every call whose result is
    /// returned directly.
    TailCall {
        callee: Reg,
        argc: u8,
    },
    Return {
        src: Reg,
    },
    /// The end of a `match` whose arms all failed. The type checker proves
    /// every `match` exhaustive, so reaching this is a bug in the compiler.
    NoMatch,
    /// The end of a cleanup block (see [`Proto::cleanups`]): the frame's
    /// deferred code has run, so a cancelled task leaves the frame and goes
    /// on stopping in its caller.
    Unwind,
}

/// String literals, numbered by the compiler. The VM allocates each one on
/// its heap once, when the code that uses it is loaded, and keeps it alive
/// for as long as the VM runs, so `LoadStr` never allocates.
///
/// A compiler numbers literals across everything it compiles (every REPL
/// input), and each compiled unit carries only the literals it introduced.
/// A REPL input that fails is rolled back with its compiler, so its numbers
/// can be given out again; loading a literal replaces whatever the VM had
/// under that number.
pub type Literals = Vec<(u32, Box<str>)>;

/// What the VM needs to know about a struct or one variant of an enum to
/// build and show its values.
#[derive(Debug)]
pub struct Layout {
    /// How values are shown: `Point`, or `Shape::Circle`.
    pub name: String,
    /// The variant's index in its enum; 0 for a struct.
    pub tag: u32,
    /// A struct's field names; `None` for a variant, whose values are positional.
    pub fields: Option<Vec<Id>>,
}

/// Where a closure gets one of its captured values when it is created.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Capture {
    /// A register of the function creating the closure: a variable's value,
    /// or its cell if it is a captured `var`.
    Local(Reg),
    /// One of the creating function's own captured values.
    Upvalue(u16),
    /// The closure itself, which `let f = fn() { ... f() ... };` captures.
    Itself,
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
    /// Closures defined inside this function.
    pub protos: Vec<Rc<Proto>>,
    /// The struct and variant layouts `NewData` instructions refer to.
    pub layouts: Vec<Rc<Layout>>,
    /// The values a closure made from this proto captures, in upvalue order.
    pub captures: Vec<Capture>,
    /// For `Call` instructions whose callee is a name, that name (by
    /// instruction index), to describe the call in error traces.
    pub call_names: HashMap<usize, Id>,
    /// Where a cancelled task goes to run the deferred code that is pending
    /// at an instruction: `(start, end, cleanup)` says the instructions
    /// from `start` up to `end` go to `cleanup`, which runs it and ends in
    /// [`Op::Unwind`]. Sorted, and not overlapping; an instruction in none
    /// has no deferred code pending.
    pub cleanups: Vec<(u32, u32, u32)>,
    /// The instructions of deferred code run on the way out of a block, as
    /// sorted, separate `(start, end)` ranges. A task asked to stop while
    /// running them stops once they are done, so deferred code always runs
    /// to its end.
    pub deferred: Vec<(u32, u32)>,
    /// In a program compiled ahead of time, the address of the function's
    /// native code (see `aot`); 0 in the VM.
    pub native: usize,
}

impl Proto {
    /// Where a task cancelled at instruction `at` runs its pending deferred
    /// code, if it has any.
    pub fn cleanup_at(&self, at: usize) -> Option<usize> {
        let at = at as u32;
        let i = self.cleanups.partition_point(|&(_, end, _)| end <= at);
        match self.cleanups.get(i) {
            Some(&(start, _, cleanup)) if start <= at => Some(cleanup as usize),
            _ => None,
        }
    }

    /// Whether instruction `at` is in deferred code run on the way out of
    /// a block.
    pub fn in_deferred(&self, at: usize) -> bool {
        let at = at as u32;
        let i = self.deferred.partition_point(|&(_, end)| end <= at);
        self.deferred.get(i).is_some_and(|&(start, _)| start <= at)
    }
}

impl fmt::Display for Proto {
    /// A listing of the bytecode, for debugging and tests.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.name.as_deref().unwrap_or("<anonymous>");
        writeln!(
            f,
            "fn {name}{} ({} registers)",
            self.signature, self.num_regs
        )?;
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
