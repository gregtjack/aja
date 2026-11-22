//! Compiles the type-checked AST to bytecode.
//!
//! The compiler trusts the type checker: it does no type checks of its own,
//! and it uses the checker's [`Types`] to pick typed instructions.
//!
//! Registers are allocated like a stack. Locals take the next free register
//! when they are declared and give it back when their block ends; temporaries
//! sit above the locals and are given back as soon as the expression that
//! needed them is done. A call puts the callee and its arguments in the
//! topmost registers, so the callee's register window can start right there.
//!
//! A local that some closure captures lives in a cell instead of directly in
//! its register (see [`captured_locals`]), so that the declaring function and
//! every closure that captured it share one variable.
//!
//! A `match` compiles to a test of each arm in turn: the arm's pattern
//! compiles to checks that jump to the next arm on failure, and to moves
//! that put the parts it binds into new locals.

use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{
    ast::{
        types::Type, Arm, Definition, Expr, Expression, FieldInit, Id, Op1, Op2, Pattern,
        PatternKind, Program, Span, Statement, Variable,
    },
    builtins::BUILTINS,
    check::{OperandType, Types},
    diagnostic::Diagnostic,
};

use super::bytecode::{Capture, Layout, Literals, Op, Proto, Reg};

/// The global names and their slots. It persists across REPL inputs, so a
/// later input can use what an earlier one defined.
#[derive(Debug, Clone)]
pub struct Compiler {
    globals: HashMap<Id, u32>,
    count: u32,
    /// The program's structs and enums, by name.
    data: HashMap<Id, DataDef>,
    /// Every string literal's number.
    literals: HashMap<Box<str>, u32>,
    /// The literals numbered since the last unit was finished.
    new_literals: Literals,
}

/// How a struct or enum is laid out at runtime.
#[derive(Debug, Clone)]
enum DataDef {
    Struct {
        layout: Rc<Layout>,
        fields: Vec<Id>,
    },
    /// Each variant's name and layout, in declaration order (the variant's
    /// tag is its index).
    Enum {
        variants: Vec<(Id, Rc<Layout>)>,
    },
}

/// A compiled program: its top-level functions with the global slot each
/// belongs in, the slot of `main`, and its string literals.
pub struct Compiled {
    pub functions: Vec<(u32, Rc<Proto>)>,
    pub main: u32,
    pub literals: Literals,
}

/// A compiled REPL input: a function that takes no arguments, and the string
/// literals it introduced.
pub struct ReplInput {
    pub proto: Rc<Proto>,
    pub literals: Literals,
}

impl Compiler {
    /// A compiler that knows the built-ins, in the slots the VM puts them.
    pub fn new() -> Self {
        let mut compiler = Compiler {
            globals: HashMap::new(),
            count: 0,
            data: HashMap::new(),
            literals: HashMap::new(),
            new_literals: Vec::new(),
        };
        for b in BUILTINS {
            compiler.define_global(b.name.into());
        }
        compiler
    }

    fn define_global(&mut self, name: Id) -> u32 {
        let slot = self.count;
        self.count += 1;
        self.globals.insert(name, slot);
        slot
    }

    /// The number of the string literal `s`.
    fn literal(&mut self, s: &str) -> u32 {
        if let Some(&n) = self.literals.get(s) {
            return n;
        }
        let n = u32::try_from(self.literals.len()).expect("too many string literals");
        self.literals.insert(s.into(), n);
        self.new_literals.push((n, s.into()));
        n
    }

    pub fn program(&mut self, program: &Program, types: &Types) -> Result<Compiled, Diagnostic> {
        // Give every function a slot first, so bodies can refer to functions
        // defined later in the file.
        let mut fns = Vec::new();
        for def in &program.definitions {
            match def {
                Definition::Function {
                    name,
                    name_span,
                    params,
                    body,
                    rtype,
                } => {
                    let slot = self.define_global(name.clone());
                    fns.push((slot, name, name_span, params, body, rtype));
                }
                Definition::Struct { name, fields, .. } => {
                    let fields: Vec<Id> = fields.iter().map(|f| f.name.clone()).collect();
                    let layout = Rc::new(Layout {
                        name: name.to_string(),
                        tag: 0,
                        fields: Some(fields.clone()),
                    });
                    self.data
                        .insert(name.clone(), DataDef::Struct { layout, fields });
                }
                Definition::Enum { name, variants, .. } => {
                    let variants = variants
                        .iter()
                        .enumerate()
                        .map(|(tag, v)| {
                            let layout = Rc::new(Layout {
                                name: format!("{name}::{}", v.name),
                                tag: tag as u32,
                                fields: None,
                            });
                            (v.name.clone(), layout)
                        })
                        .collect();
                    self.data.insert(name.clone(), DataDef::Enum { variants });
                }
            }
        }

        let mut main = None;
        let mut functions = Vec::new();
        for (slot, name, name_span, params, body, rtype) in fns {
            if name == "main" {
                if !params.is_empty() {
                    return Err(Diagnostic::new("'main' cannot take parameters")
                        .with_span(name_span.start, name_span.end)
                        .with_help(Some("remove the parameters: `fn main() { ... }`".into())));
                }
                main = Some(slot);
            }
            let captured = captured_locals(params, body, false);
            let mut fc = FnCompiler::new(self, types, &captured, false);
            let proto = fc.function(Some(name.clone()), params, rtype.as_ref(), body, *name_span);
            functions.push((slot, Rc::new(proto)));
        }

        let main = main.ok_or_else(|| {
            Diagnostic::new("no 'main' function found; add `fn main() { ... }` as the entry point")
        })?;
        Ok(Compiled {
            functions,
            main,
            literals: std::mem::take(&mut self.new_literals),
        })
    }

    /// Compile one REPL input as a function that takes no arguments and
    /// returns the input's value. Its top-level `let` defines a global.
    pub fn repl(&mut self, stmt: &Statement, types: &Types, span: Span) -> ReplInput {
        let captured = captured_locals(&[], stmt, true);
        let mut fc = FnCompiler::new(self, types, &captured, true);
        let proto = Rc::new(fc.function(None, &[], None, stmt, span));
        ReplInput {
            proto,
            literals: std::mem::take(&mut self.new_literals),
        }
    }
}

/// Identifies a variable declaration (a parameter or `let`) by where its name
/// appears.
type DeclKey = (u32, u32);

fn decl_key(v: &Variable) -> DeclKey {
    (v.span.start.pos, v.span.end.pos)
}

/// Where a name refers to.
enum Place {
    Local { reg: Reg, cell: bool },
    Upvalue(u16),
    Global(u32),
}

struct Local {
    name: Id,
    reg: Reg,
    /// The register holds a cell, because a closure captures the variable.
    cell: bool,
}

/// A function being compiled.
struct FnState {
    code: Vec<Op>,
    spans: Vec<Span>,
    protos: Vec<Rc<Proto>>,
    layouts: Vec<Rc<Layout>>,
    captures: Vec<Capture>,
    call_names: HashMap<usize, Id>,
    /// Lexical scopes, innermost last.
    scopes: Vec<Vec<Local>>,
    /// The next free register.
    next: Reg,
    /// How many registers the function needs.
    max: Reg,
    /// The function's name or the whole closure, for instructions that have
    /// no expression of their own.
    span: Span,
}

struct FnCompiler<'a> {
    compiler: &'a mut Compiler,
    types: &'a Types,
    captured: &'a HashSet<DeclKey>,
    /// The function being compiled and the ones it is nested in, innermost last.
    fns: Vec<FnState>,
    /// Whether this is a REPL input.
    repl: bool,
}

impl<'a> FnCompiler<'a> {
    fn new(
        compiler: &'a mut Compiler,
        types: &'a Types,
        captured: &'a HashSet<DeclKey>,
        repl: bool,
    ) -> Self {
        FnCompiler {
            compiler,
            types,
            captured,
            fns: Vec::new(),
            repl,
        }
    }

    // ---- Functions -----------------------------------------------------------

    fn function(
        &mut self,
        name: Option<Id>,
        params: &[Variable],
        rtype: Option<&Type>,
        body: &Statement,
        span: Span,
    ) -> Proto {
        self.fns.push(FnState {
            code: Vec::new(),
            spans: Vec::new(),
            protos: Vec::new(),
            layouts: Vec::new(),
            captures: Vec::new(),
            call_names: HashMap::new(),
            scopes: vec![Vec::new()],
            next: 0,
            max: 0,
            span,
        });

        // Arguments arrive in the first registers.
        for p in params {
            let reg = self.alloc();
            let cell = self.captured.contains(&decl_key(p));
            if cell {
                self.emit(Op::NewCell { dst: reg, src: reg }, p.span);
            }
            self.declare(p.name.clone(), reg, cell);
        }

        let ret = self.alloc();
        self.stmt(body, Some(ret));
        self.emit(Op::Return { src: ret }, span);

        let mut f = self.fns.pop().unwrap();
        mark_tail_calls(&mut f.code);
        Proto {
            name,
            signature: signature(params, rtype),
            arity: params.len() as u8,
            num_regs: f.max,
            code: f.code,
            spans: f.spans,
            protos: f.protos,
            layouts: f.layouts,
            captures: f.captures,
            call_names: f.call_names,
        }
    }

    fn cur(&mut self) -> &mut FnState {
        self.fns.last_mut().unwrap()
    }

    fn emit(&mut self, op: Op, span: Span) -> usize {
        let f = self.cur();
        f.code.push(op);
        f.spans.push(span);
        f.code.len() - 1
    }

    /// Point the jump at `at` to the next instruction.
    fn patch(&mut self, at: usize) {
        let here = self.cur().code.len() as u32;
        match &mut self.cur().code[at] {
            Op::Jump { target }
            | Op::JumpIfFalse { target, .. }
            | Op::JumpIfTrue { target, .. } => *target = here,
            op => unreachable!("patching {op:?}, which is not a jump"),
        }
    }

    fn here(&mut self) -> u32 {
        self.cur().code.len() as u32
    }

    // ---- Registers and scopes -------------------------------------------------

    fn alloc(&mut self) -> Reg {
        let f = self.cur();
        let reg = f.next;
        f.next = f
            .next
            .checked_add(1)
            .expect("a function needs more registers than the VM has");
        f.max = f.max.max(f.next);
        reg
    }

    /// `n` consecutive registers; the first one.
    fn alloc_n(&mut self, n: usize) -> Reg {
        let start = self.mark();
        for _ in 0..n {
            self.alloc();
        }
        start
    }

    fn mark(&mut self) -> Reg {
        self.cur().next
    }

    /// Give back every register allocated since `mark`.
    fn release(&mut self, mark: Reg) {
        self.cur().next = mark;
    }

    fn declare(&mut self, name: Id, reg: Reg, cell: bool) {
        self.cur()
            .scopes
            .last_mut()
            .unwrap()
            .push(Local { name, reg, cell });
    }

    fn find_local(&self, f: usize, name: &Id) -> Option<&Local> {
        self.fns[f]
            .scopes
            .iter()
            .rev()
            .flat_map(|s| s.iter().rev())
            .find(|l| l.name == *name)
    }

    fn resolve(&mut self, name: &Id) -> Place {
        let f = self.fns.len() - 1;
        if let Some(l) = self.find_local(f, name) {
            return Place::Local {
                reg: l.reg,
                cell: l.cell,
            };
        }
        if let Some(i) = self.upvalue(f, name) {
            return Place::Upvalue(i);
        }
        match self.compiler.globals.get(name) {
            Some(&slot) => Place::Global(slot),
            None => unreachable!("'{name}' is undefined; the type checker should have caught this"),
        }
    }

    /// The index of `name` among function `f`'s captured cells, capturing it
    /// if needed. `None` if no enclosing function declares it.
    fn upvalue(&mut self, f: usize, name: &Id) -> Option<u16> {
        if f == 0 {
            return None;
        }
        let capture = match self.find_local(f - 1, name) {
            Some(l) => {
                debug_assert!(l.cell, "'{name}' is captured but not in a cell");
                Capture::Local(l.reg)
            }
            None => Capture::Upvalue(self.upvalue(f - 1, name)?),
        };
        let captures = &mut self.fns[f].captures;
        let index = match captures.iter().position(|c| *c == capture) {
            Some(i) => i,
            None => {
                captures.push(capture);
                captures.len() - 1
            }
        };
        Some(index as u16)
    }

    // ---- Statements ------------------------------------------------------------

    /// Compile `stmt`, putting its value in `dst` if there is one.
    fn stmt(&mut self, stmt: &Statement, dst: Option<Reg>) {
        match stmt {
            Statement::Block(stmts, tail) => {
                let mark = self.mark();
                self.cur().scopes.push(Vec::new());
                for s in stmts {
                    self.stmt(s, None);
                }
                match (tail, dst) {
                    (Some(tail), Some(dst)) => self.expr(tail, dst),
                    (Some(tail), None) => self.discard(tail),
                    (None, Some(dst)) => self.void(dst),
                    (None, None) => {}
                }
                self.cur().scopes.pop();
                self.release(mark);
            }
            Statement::Expr(e) => match dst {
                Some(dst) => self.expr(e, dst),
                None => self.discard(e),
            },
            Statement::Let { var, value, .. } => {
                self.let_(var, value);
                if let Some(dst) = dst {
                    self.void(dst);
                }
            }
            Statement::If(cond, body) => {
                let mark = self.mark();
                let c = self.expr_any(cond);
                let skip = self.emit(Op::JumpIfFalse { cond: c, target: 0 }, cond.span);
                self.release(mark);
                self.stmt(body, None);
                self.patch(skip);
                if let Some(dst) = dst {
                    self.void(dst);
                }
            }
            Statement::While(cond, body) => {
                let start = self.here();
                let mark = self.mark();
                let c = self.expr_any(cond);
                let exit = self.emit(Op::JumpIfFalse { cond: c, target: 0 }, cond.span);
                self.release(mark);
                self.stmt(body, None);
                self.emit(Op::Jump { target: start }, cond.span);
                self.patch(exit);
                if let Some(dst) = dst {
                    self.void(dst);
                }
            }
            Statement::Return(value) => {
                let mark = self.mark();
                let src = match value {
                    Some(e) => self.expr_any(e),
                    None => {
                        let r = self.alloc();
                        self.void(r);
                        r
                    }
                };
                let span = value.as_ref().map_or(self.cur().span, |e| e.span);
                self.emit(Op::Return { src }, span);
                self.release(mark);
            }
        }
    }

    fn let_(&mut self, var: &Variable, value: &Expr) {
        let reg = self.alloc();
        // The value is compiled before the name is declared, so it sees any
        // earlier variable of the same name (`let a = a + 1;`).
        self.expr(value, reg);

        // A REPL input's top-level `let` outlives the input, so it is a global.
        if self.repl && self.fns.len() == 1 && self.cur().scopes.len() == 1 {
            let slot = self.compiler.define_global(var.name.clone());
            self.emit(
                Op::SetGlobal {
                    index: slot,
                    src: reg,
                },
                var.span,
            );
            self.release(reg);
            return;
        }

        let cell = self.captured.contains(&decl_key(var));
        if cell {
            self.emit(Op::NewCell { dst: reg, src: reg }, var.span);
        }
        self.declare(var.name.clone(), reg, cell);
    }

    fn void(&mut self, dst: Reg) {
        let span = self.cur().span;
        self.emit(Op::LoadVoid { dst }, span);
    }

    // ---- Expressions -------------------------------------------------------------

    /// Evaluate `e` for its side effects only.
    fn discard(&mut self, e: &Expr) {
        let r = self.alloc();
        self.expr(e, r);
        self.release(r);
    }

    /// Evaluate `e` into some register and return it: the variable's own
    /// register when `e` is a local variable not in a cell, else a new
    /// temporary. The caller releases it.
    fn expr_any(&mut self, e: &Expr) -> Reg {
        match &e.kind {
            Expression::Grouping(inner) => return self.expr_any(inner),
            Expression::Var(name) => {
                let f = self.fns.len() - 1;
                if let Some(Local {
                    reg, cell: false, ..
                }) = self.find_local(f, name)
                {
                    return *reg;
                }
            }
            _ => {}
        }
        let r = self.alloc();
        self.expr(e, r);
        r
    }

    /// Evaluate `e` into `dst`. Temporaries it needs are released afterwards.
    fn expr(&mut self, e: &Expr, dst: Reg) {
        let mark = self.mark();
        let span = e.span;
        match &e.kind {
            Expression::Int(value) => {
                self.emit(Op::LoadInt { dst, value: *value }, span);
            }
            Expression::Float(value) => {
                self.emit(Op::LoadFloat { dst, value: *value }, span);
            }
            Expression::True | Expression::False => {
                let value = matches!(e.kind, Expression::True);
                self.emit(Op::LoadBool { dst, value }, span);
            }
            Expression::String(s) => {
                let index = self.compiler.literal(s);
                self.emit(Op::LoadStr { dst, index }, span);
            }
            Expression::Grouping(inner) => self.expr(inner, dst),
            Expression::Unary(op, operand) => {
                let src = self.expr_any(operand);
                let op = match op {
                    Op1::Not => Op::Not { dst, src },
                    Op1::Negate => match self.types.operand(e) {
                        OperandType::Float => Op::NegF { dst, src },
                        _ => Op::NegI { dst, src },
                    },
                };
                self.emit(op, span);
            }
            Expression::BinOp(l, op @ (Op2::And | Op2::Or), r) => {
                // Short-circuit: only evaluate `r` if `l` doesn't decide.
                self.expr(l, dst);
                let jump = match op {
                    Op2::And => Op::JumpIfFalse {
                        cond: dst,
                        target: 0,
                    },
                    _ => Op::JumpIfTrue {
                        cond: dst,
                        target: 0,
                    },
                };
                let done = self.emit(jump, span);
                self.expr(r, dst);
                self.patch(done);
            }
            Expression::BinOp(l, op, r) => self.binop(e, l, op, r, dst),
            Expression::If(cond, then, otherwise) => {
                let c = self.expr_any(cond);
                let to_else = self.emit(Op::JumpIfFalse { cond: c, target: 0 }, cond.span);
                self.release(mark);
                self.stmt(then, Some(dst));
                let to_end = self.emit(Op::Jump { target: 0 }, span);
                self.patch(to_else);
                self.stmt(otherwise, Some(dst));
                self.patch(to_end);
            }
            Expression::Var(name) => {
                let op = match self.resolve(name) {
                    Place::Local { reg, cell: false } => Op::Move { dst, src: reg },
                    Place::Local { reg, cell: true } => Op::GetCell { dst, cell: reg },
                    Place::Upvalue(index) => Op::GetUpvalue { dst, index },
                    Place::Global(index) => Op::GetGlobal { dst, index },
                };
                if op != (Op::Move { dst, src: dst }) {
                    self.emit(op, span);
                }
            }
            Expression::Assign(name, value) => {
                let src = self.expr_any(value);
                let op = match self.resolve(name) {
                    Place::Local { reg, cell: false } => Op::Move { dst: reg, src },
                    Place::Local { reg, cell: true } => Op::SetCell { cell: reg, src },
                    Place::Upvalue(index) => Op::SetUpvalue { index, src },
                    Place::Global(index) => Op::SetGlobal { index, src },
                };
                self.emit(op, span);
                // An assignment's value is the value assigned.
                if src != dst {
                    self.emit(Op::Move { dst, src }, span);
                }
            }
            Expression::Call(callee, args) if matches!(callee.kind, Expression::Path(..)) => {
                let Expression::Path(enum_name, variant) = &callee.kind else {
                    unreachable!()
                };
                let layout = self.variant_layout(enum_name, variant);
                let start = self.alloc_n(args.len());
                for (i, arg) in args.iter().enumerate() {
                    self.expr(arg, start + i as Reg);
                }
                self.new_data(dst, layout, start, args.len(), span);
            }
            Expression::Path(enum_name, variant) => {
                let layout = self.variant_layout(enum_name, variant);
                self.new_data(dst, layout, 0, 0, span);
            }
            Expression::StructLit { name, fields, base } => {
                self.struct_lit(name, fields, base.as_deref(), dst, span)
            }
            Expression::Field(obj, _, _) => {
                let obj = self.expr_any(obj);
                let index = self.types.field(e);
                self.emit(Op::GetField { dst, obj, index }, span);
            }
            Expression::Match(scrutinee, arms) => self.match_(scrutinee, arms, dst, span),
            Expression::Call(callee, args) => {
                let base = self.alloc();
                self.expr(callee, base);
                for arg in args {
                    let r = self.alloc();
                    self.expr(arg, r);
                }
                let argc = args.len() as u8;
                let at = self.emit(
                    Op::Call {
                        dst,
                        callee: base,
                        argc,
                    },
                    span,
                );
                if let Expression::Var(name) = &callee.kind {
                    self.cur().call_names.insert(at, name.clone());
                }
            }
            Expression::Closure(params, rtype, body) => {
                let proto = self.function(None, params, rtype.as_ref(), body, span);
                let protos = &mut self.cur().protos;
                protos.push(Rc::new(proto));
                let index = (protos.len() - 1) as u32;
                self.emit(Op::Closure { dst, proto: index }, span);
            }
        }
        self.release(mark);
    }

    fn binop(&mut self, e: &Expr, l: &Expr, op: &Op2, r: &Expr, dst: Reg) {
        // Use a local's register for the left operand directly only if the
        // right operand can't assign to it before the operation reads it.
        let a = if is_simple(r) {
            self.expr_any(l)
        } else {
            let t = self.alloc();
            self.expr(l, t);
            t
        };
        let b = self.expr_any(r);

        use OperandType::{Bool, Float, Str};
        let negate = matches!(op, Op2::NotEqual);
        let t = self.types.operand(e);
        let float = t == Float;
        let op = match op {
            Op2::Addition if float => Op::AddF { dst, a, b },
            Op2::Addition if t == Str => Op::AddS { dst, a, b },
            Op2::Addition => Op::AddI { dst, a, b },
            Op2::Subtraction if float => Op::SubF { dst, a, b },
            Op2::Subtraction => Op::SubI { dst, a, b },
            Op2::Multiplication if float => Op::MulF { dst, a, b },
            Op2::Multiplication => Op::MulI { dst, a, b },
            Op2::Division if float => Op::DivF { dst, a, b },
            Op2::Division => Op::DivI { dst, a, b },
            Op2::Modulo if float => Op::RemF { dst, a, b },
            Op2::Modulo => Op::RemI { dst, a, b },
            Op2::LessThan if float => Op::LtF { dst, a, b },
            Op2::LessThan => Op::LtI { dst, a, b },
            Op2::LessEqual if float => Op::LeF { dst, a, b },
            Op2::LessEqual => Op::LeI { dst, a, b },
            Op2::GreaterThan if float => Op::LtF { dst, a: b, b: a },
            Op2::GreaterThan => Op::LtI { dst, a: b, b: a },
            Op2::GreaterEqual if float => Op::LeF { dst, a: b, b: a },
            Op2::GreaterEqual => Op::LeI { dst, a: b, b: a },
            Op2::Equal | Op2::NotEqual => match t {
                Float => Op::EqF { dst, a, b },
                Bool => Op::EqB { dst, a, b },
                Str => Op::EqS { dst, a, b },
                OperandType::Data => Op::EqD { dst, a, b },
                _ => Op::EqI { dst, a, b },
            },
            Op2::And | Op2::Or => unreachable!("logical operators are compiled separately"),
        };
        self.emit(op, e.span);
        if negate {
            self.emit(Op::Not { dst, src: dst }, e.span);
        }
    }
}

impl FnCompiler<'_> {
    // ---- Structs and enums -----------------------------------------------------

    fn variant_layout(&self, enum_name: &Id, variant: &Id) -> Rc<Layout> {
        match &self.compiler.data[enum_name] {
            DataDef::Enum { variants } => {
                let (_, layout) = variants
                    .iter()
                    .find(|(v, _)| v == variant)
                    .expect("the type checker checks variants");
                layout.clone()
            }
            DataDef::Struct { .. } => unreachable!("'{enum_name}' is a struct"),
        }
    }

    /// The tag of a variant, which is its index.
    fn variant_tag(&self, enum_name: &Id, variant: &Id) -> u32 {
        self.variant_layout(enum_name, variant).tag
    }

    fn struct_def(&self, name: &Id) -> (Rc<Layout>, Vec<Id>) {
        match &self.compiler.data[name] {
            DataDef::Struct { layout, fields } => (layout.clone(), fields.clone()),
            DataDef::Enum { .. } => unreachable!("'{name}' is an enum"),
        }
    }

    /// Make a value with `layout` from the `count` registers from `start`.
    fn new_data(&mut self, dst: Reg, layout: Rc<Layout>, start: Reg, count: usize, span: Span) {
        let layouts = &mut self.cur().layouts;
        let index = match layouts.iter().position(|l| Rc::ptr_eq(l, &layout)) {
            Some(i) => i,
            None => {
                layouts.push(layout);
                layouts.len() - 1
            }
        };
        let op = Op::NewData {
            dst,
            layout: index as u32,
            start,
            count: count as u16,
        };
        self.emit(op, span);
    }

    fn struct_lit(
        &mut self,
        name: &Id,
        fields: &[FieldInit],
        base: Option<&Expr>,
        dst: Reg,
        span: Span,
    ) {
        let (layout, names) = self.struct_def(name);
        let start = self.alloc_n(names.len());
        // Fields are evaluated in the order written, each into its slot.
        let mut given = vec![false; names.len()];
        for f in fields {
            let i = names
                .iter()
                .position(|n| *n == f.name)
                .expect("the type checker checks fields");
            given[i] = true;
            self.expr(&f.value, start + i as Reg);
        }
        if let Some(base) = base {
            let obj = self.expr_any(base);
            for (i, _) in given.iter().enumerate().filter(|(_, g)| !**g) {
                let op = Op::GetField {
                    dst: start + i as Reg,
                    obj,
                    index: i as u16,
                };
                self.emit(op, base.span);
            }
        }
        self.new_data(dst, layout, start, names.len(), span);
    }

    fn match_(&mut self, scrutinee: &Expr, arms: &[Arm], dst: Reg, span: Span) {
        // Copy the value, so a guard that assigns to the variable matched on
        // can't change what later arms see.
        let value = self.alloc();
        self.expr(scrutinee, value);
        let mut ends = Vec::new();
        for arm in arms {
            let mark = self.mark();
            self.cur().scopes.push(Vec::new());
            let mut fails = Vec::new();
            self.pattern(&arm.pattern, value, &mut fails);
            if let Some(guard) = &arm.guard {
                let m = self.mark();
                let cond = self.expr_any(guard);
                fails.push(self.emit(Op::JumpIfFalse { cond, target: 0 }, guard.span));
                self.release(m);
            }
            self.stmt(&arm.body, Some(dst));
            ends.push(self.emit(Op::Jump { target: 0 }, span));
            for f in fails {
                self.patch(f);
            }
            self.cur().scopes.pop();
            self.release(mark);
        }
        self.emit(Op::NoMatch, span);
        for e in ends {
            self.patch(e);
        }
    }

    /// Test that the value in `value` matches `p`, adding a jump to `fails`
    /// for each check that fails, and bind the names in `p`.
    fn pattern(&mut self, p: &Pattern, value: Reg, fails: &mut Vec<usize>) {
        match &p.kind {
            PatternKind::Wildcard => {}
            PatternKind::Binding(var) => {
                let reg = self.alloc();
                self.emit(
                    Op::Move {
                        dst: reg,
                        src: value,
                    },
                    p.span,
                );
                self.bind(var, reg);
            }
            PatternKind::Int(i) => {
                let t = self.alloc();
                self.emit(Op::LoadInt { dst: t, value: *i }, p.span);
                self.emit(
                    Op::EqI {
                        dst: t,
                        a: value,
                        b: t,
                    },
                    p.span,
                );
                fails.push(self.emit(Op::JumpIfFalse { cond: t, target: 0 }, p.span));
            }
            PatternKind::Str(s) => {
                let t = self.alloc();
                let index = self.compiler.literal(s);
                self.emit(Op::LoadStr { dst: t, index }, p.span);
                self.emit(
                    Op::EqS {
                        dst: t,
                        a: value,
                        b: t,
                    },
                    p.span,
                );
                fails.push(self.emit(Op::JumpIfFalse { cond: t, target: 0 }, p.span));
            }
            PatternKind::Bool(b) => {
                let op = if *b {
                    Op::JumpIfFalse {
                        cond: value,
                        target: 0,
                    }
                } else {
                    Op::JumpIfTrue {
                        cond: value,
                        target: 0,
                    }
                };
                fails.push(self.emit(op, p.span));
            }
            PatternKind::Variant {
                enum_name,
                variant,
                fields,
                ..
            } => {
                let tag = self.variant_tag(enum_name, variant);
                let t = self.alloc();
                self.emit(
                    Op::TestTag {
                        dst: t,
                        obj: value,
                        tag,
                    },
                    p.span,
                );
                fails.push(self.emit(Op::JumpIfFalse { cond: t, target: 0 }, p.span));
                for (i, sub) in fields.iter().enumerate() {
                    self.sub_pattern(sub, value, i, fails);
                }
            }
            PatternKind::Struct { name, fields, .. } => {
                let (_, names) = self.struct_def(name);
                for (field, _, sub) in fields {
                    let i = names
                        .iter()
                        .position(|n| n == field)
                        .expect("the type checker checks fields");
                    self.sub_pattern(sub, value, i, fails);
                }
            }
        }
    }

    /// Match field `index` of the value in `obj` against `p`.
    fn sub_pattern(&mut self, p: &Pattern, obj: Reg, index: usize, fails: &mut Vec<usize>) {
        let get = |dst| Op::GetField {
            dst,
            obj,
            index: index as u16,
        };
        match &p.kind {
            PatternKind::Wildcard => {}
            // Read a bound field straight into its variable's register.
            PatternKind::Binding(var) => {
                let reg = self.alloc();
                self.emit(get(reg), p.span);
                self.bind(var, reg);
            }
            _ => {
                let reg = self.alloc();
                self.emit(get(reg), p.span);
                self.pattern(p, reg, fails);
            }
        }
    }

    /// Declare the variable a pattern binds, whose value is in `reg`.
    fn bind(&mut self, var: &Variable, reg: Reg) {
        let cell = self.captured.contains(&decl_key(var));
        if cell {
            self.emit(Op::NewCell { dst: reg, src: reg }, var.span);
        }
        self.declare(var.name.clone(), reg, cell);
    }
}

/// Turn every call whose result is returned right away (directly, or after
/// jumps, as at the end of an `if` branch) into a tail call.
fn mark_tail_calls(code: &mut [Op]) {
    for i in 0..code.len() {
        let Op::Call { dst, callee, argc } = code[i] else {
            continue;
        };
        let mut next = i + 1;
        // Bounded, in case of a jump cycle (an infinite loop has none
        // without a condition, but there's no need to rely on that).
        for _ in 0..code.len() {
            match code[next] {
                Op::Jump { target } => next = target as usize,
                Op::Return { src } if src == dst => {
                    code[i] = Op::TailCall { callee, argc };
                    break;
                }
                _ => break,
            }
        }
    }
}

/// Whether evaluating `e` certainly assigns to no variable.
fn is_simple(e: &Expr) -> bool {
    match &e.kind {
        Expression::Int(_)
        | Expression::Float(_)
        | Expression::String(_)
        | Expression::True
        | Expression::False
        | Expression::Var(_)
        | Expression::Path(..) => true,
        Expression::Grouping(e) | Expression::Unary(_, e) | Expression::Field(e, ..) => {
            is_simple(e)
        }
        Expression::BinOp(l, _, r) => is_simple(l) && is_simple(r),
        _ => false,
    }
}

/// A function's parameters and return type as written, e.g. `(x: int) -> int`.
fn signature(params: &[Variable], rtype: Option<&Type>) -> String {
    let params: Vec<String> = params.iter().map(|p| p.to_string()).collect();
    match rtype {
        Some(t) => format!("({}) -> {t}", params.join(", ")),
        None => format!("({})", params.join(", ")),
    }
}

// ---- Captured variables ---------------------------------------------------------

/// The locals of a function (with these parameters and body) that closures
/// inside it capture. For a REPL input, top-level `let`s are globals and are
/// never captured.
///
/// This walks the body with the same scoping rules as the compiler, ahead of
/// compiling it, because a variable must go into a cell when it is declared,
/// before the closure that captures it is reached.
fn captured_locals(params: &[Variable], body: &Statement, repl: bool) -> HashSet<DeclKey> {
    let mut finder = CaptureFinder {
        scopes: vec![(
            0,
            params
                .iter()
                .map(|p| (p.name.clone(), decl_key(p)))
                .collect(),
        )],
        depth: 0,
        repl,
        captured: HashSet::new(),
    };
    finder.stmt(body);
    finder.captured
}

struct CaptureFinder {
    /// Lexical scopes, innermost last: the function nesting depth each
    /// belongs to, and the variables declared in it.
    scopes: Vec<(usize, Vec<(Id, DeclKey)>)>,
    /// How many closures deep the walk is.
    depth: usize,
    repl: bool,
    captured: HashSet<DeclKey>,
}

impl CaptureFinder {
    fn use_name(&mut self, name: &Id) {
        for (depth, vars) in self.scopes.iter().rev() {
            if let Some((_, key)) = vars.iter().rev().find(|(n, _)| n == name) {
                if *depth < self.depth {
                    self.captured.insert(*key);
                }
                return;
            }
        }
        // Not a local: a global, which needs no capturing.
    }

    fn scope<R>(&mut self, vars: Vec<(Id, DeclKey)>, f: impl FnOnce(&mut Self) -> R) -> R {
        self.scopes.push((self.depth, vars));
        let r = f(self);
        self.scopes.pop();
        r
    }

    fn stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Block(stmts, tail) => self.scope(Vec::new(), |s| {
                for stmt in stmts {
                    s.stmt(stmt);
                }
                if let Some(tail) = tail {
                    s.expr(tail);
                }
            }),
            Statement::Expr(e) => self.expr(e),
            Statement::Let { var, value, .. } => {
                self.expr(value);
                let repl_global = self.repl && self.depth == 0 && self.scopes.len() == 1;
                if !repl_global {
                    let key = decl_key(var);
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .1
                        .push((var.name.clone(), key));
                }
            }
            Statement::If(cond, body) | Statement::While(cond, body) => {
                self.expr(cond);
                self.stmt(body);
            }
            Statement::Return(value) => {
                if let Some(e) = value {
                    self.expr(e);
                }
            }
        }
    }

    fn expr(&mut self, e: &Expr) {
        match &e.kind {
            Expression::Int(_)
            | Expression::Float(_)
            | Expression::String(_)
            | Expression::True
            | Expression::False
            | Expression::Path(..) => {}
            Expression::Grouping(e) | Expression::Unary(_, e) | Expression::Field(e, ..) => {
                self.expr(e)
            }
            Expression::StructLit { fields, base, .. } => {
                for f in fields {
                    self.expr(&f.value);
                }
                if let Some(b) = base {
                    self.expr(b);
                }
            }
            Expression::Match(scrutinee, arms) => {
                self.expr(scrutinee);
                for arm in arms {
                    let mut vars = Vec::new();
                    pattern_bindings(&arm.pattern, &mut vars);
                    self.scope(vars, |s| {
                        if let Some(g) = &arm.guard {
                            s.expr(g);
                        }
                        s.stmt(&arm.body);
                    });
                }
            }
            Expression::BinOp(l, _, r) => {
                self.expr(l);
                self.expr(r);
            }
            Expression::If(cond, then, otherwise) => {
                self.expr(cond);
                self.stmt(then);
                self.stmt(otherwise);
            }
            Expression::Var(name) => self.use_name(name),
            Expression::Assign(name, value) => {
                self.expr(value);
                self.use_name(name);
            }
            Expression::Call(callee, args) => {
                self.expr(callee);
                for a in args {
                    self.expr(a);
                }
            }
            Expression::Closure(params, _, body) => {
                self.depth += 1;
                let vars = params
                    .iter()
                    .map(|p| (p.name.clone(), decl_key(p)))
                    .collect();
                self.scope(vars, |s| s.stmt(body));
                self.depth -= 1;
            }
        }
    }
}

/// The variables a pattern binds, with their declaration keys.
fn pattern_bindings(p: &Pattern, out: &mut Vec<(Id, DeclKey)>) {
    match &p.kind {
        PatternKind::Binding(var) => out.push((var.name.clone(), decl_key(var))),
        PatternKind::Variant { fields, .. } => fields.iter().for_each(|f| pattern_bindings(f, out)),
        PatternKind::Struct { fields, .. } => {
            fields.iter().for_each(|(_, _, f)| pattern_bindings(f, out))
        }
        PatternKind::Wildcard
        | PatternKind::Int(_)
        | PatternKind::Str(_)
        | PatternKind::Bool(_) => {}
    }
}
