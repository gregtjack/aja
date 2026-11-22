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
    LoadVoid { dst: Reg },
    LoadBool { dst: Reg, value: bool },
    LoadInt { dst: Reg, value: i32 },
    LoadFloat { dst: Reg, value: f32 },
    /// Load the string literal numbered `index` (see [`Literals`]).
    LoadStr { dst: Reg, index: u32 },
    Move { dst: Reg, src: Reg },

    // ---- Globals: built-ins, top-level functions, REPL variables -------------
    GetGlobal { dst: Reg, index: u32 },
    SetGlobal { index: u32, src: Reg },

    // ---- Captured variables ----------------------------------------------------
    // A closure copies the variables it captures when it is made. A `var`
    // that a closure captures lives in a cell, and the closure copies the
    // cell, so the function that declares the `var` and every closure that
    // captures it share one variable. Immutable variables need no cell.
    /// Put the value in `src` into a new cell, stored in `dst`.
    NewCell { dst: Reg, src: Reg },
    GetCell { dst: Reg, cell: Reg },
    SetCell { cell: Reg, src: Reg },
    /// Load the current closure's captured value number `index`.
    GetUpvalue { dst: Reg, index: u16 },
    /// Load the value in the cell the current closure captured as number
    /// `index`.
    GetUpvalueCell { dst: Reg, index: u16 },
    /// Store into the cell the current closure captured as number `index`.
    SetUpvalueCell { index: u16, src: Reg },
    /// Make a closure from `protos[proto]`, capturing the values its
    /// [`Proto::captures`] lists.
    Closure { dst: Reg, proto: u32 },

    // ---- Structs and enums ---------------------------------------------------
    /// Make a struct or enum value described by `layouts[layout]`, holding
    /// the `count` values in the registers from `start` on.
    NewData { dst: Reg, layout: u32, start: Reg, count: u16 },
    /// Read the field (or variant value) at `index` of the value in `obj`.
    GetField { dst: Reg, obj: Reg, index: u16 },
    /// Whether the enum value in `obj` is the variant numbered `tag`.
    TestTag { dst: Reg, obj: Reg, tag: u32 },

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
    /// Concatenate two strings into a new one.
    AddS { dst: Reg, a: Reg, b: Reg },

    // ---- Comparison (`>` and `>=` swap the operands of `<` and `<=`) --------
    LtI { dst: Reg, a: Reg, b: Reg },
    LeI { dst: Reg, a: Reg, b: Reg },
    LtF { dst: Reg, a: Reg, b: Reg },
    LeF { dst: Reg, a: Reg, b: Reg },
    EqI { dst: Reg, a: Reg, b: Reg },
    EqF { dst: Reg, a: Reg, b: Reg },
    EqB { dst: Reg, a: Reg, b: Reg },
    EqS { dst: Reg, a: Reg, b: Reg },
    /// Structural equality of two values of the same struct or enum type.
    EqD { dst: Reg, a: Reg, b: Reg },
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
    /// Call the function in `callee` and return its result. The call reuses
    /// the current frame, so tail calls run in constant space however deep
    /// they recurse. The compiler emits it for every call whose result is
    /// returned directly.
    TailCall { callee: Reg, argc: u8 },
    Return { src: Reg },
    /// The end of a `match` whose arms all failed. The type checker proves
    /// every `match` exhaustive, so reaching this is a bug in the compiler.
    NoMatch,
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
