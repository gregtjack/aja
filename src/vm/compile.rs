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
//! A closure copies the variables it captures when it is made. That is
//! enough for every variable but a `var`, which can change: a `var` that
//! some closure captures lives in a cell instead of directly in its register
//! (see [`captured_locals`]), and closures copy the cell, so that the
//! declaring function and every closure that captured it share one variable.
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
        types::Type, Arm, Definition, Expr, Expression, FieldInit, Id, Method, Op1, Op2, Pattern,
        PatternKind, Program, Span, Statement, TemplatePart, Variable,
    },
    builtins::BUILTINS,
    check::{self, OperandType, TryOn, Types},
    diagnostic::Diagnostic,
    prelude,
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
    /// The layout of the tuples with each number of values.
    tuples: HashMap<usize, Rc<Layout>>,
    /// Every string literal's number.
    literals: HashMap<Box<str>, u32>,
    /// The literals numbered since the last unit was finished.
    new_literals: Literals,
    /// Functions compiled ahead of the next unit (the prelude's), to give
    /// the VM with it.
    new_functions: Vec<(u32, Rc<Proto>)>,
    /// Native functions declared since the last unit was finished, with
    /// their slots, for the VM to define.
    new_natives: Vec<(u32, Id)>,
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
    /// Where `main` is named, if it returns a `Result`: an `Err` from it
    /// is then an error.
    pub main_result: Option<Span>,
    pub literals: Literals,
    /// The native functions, with the global slot each belongs in.
    pub natives: Vec<(u32, Id)>,
}

/// A compiled REPL input: a function that takes no arguments, and the string
/// literals it introduced.
pub struct ReplInput {
    pub proto: Rc<Proto>,
    pub literals: Literals,
    /// Functions to define first, with their global slots: the prelude's,
    /// with the first input.
    pub functions: Vec<(u32, Rc<Proto>)>,
    /// Likewise for native functions.
    pub natives: Vec<(u32, Id)>,
}

impl Compiler {
    /// A compiler that knows the built-ins, in the slots the VM puts them.
    pub fn new() -> Self {
        let mut compiler = Compiler {
            globals: HashMap::new(),
            count: 0,
            data: HashMap::new(),
            tuples: HashMap::new(),
            literals: HashMap::new(),
            new_literals: Vec::new(),
            new_functions: Vec::new(),
            new_natives: Vec::new(),
        };
        for b in BUILTINS {
            compiler.define_global(b.name.into());
        }
        // The prelude, checked on its own, is compiled once; its functions
        // go to the VM with the first program or REPL input.
        let prelude = prelude::definitions();
        let types = check::check_prelude();
        for def in &prelude {
            compiler.define_data(def);
        }
        let methods = compiler.declare_methods(&prelude);
        let methods = compiler.natives(methods);
        let functions = compiler.compile_methods(methods, &types);
        compiler.new_functions = functions;
        compiler
    }

    /// Give the native functions slots in `methods` to the VM, and return
    /// the rest to compile.
    fn natives<'d>(&mut self, methods: Vec<(u32, Id, &'d Method)>) -> Vec<(u32, Id, &'d Method)> {
        let (native, compiled): (Vec<_>, Vec<_>) =
            methods.into_iter().partition(|(_, _, m)| m.native);
        self.new_natives
            .extend(native.into_iter().map(|(slot, global, _)| (slot, global)));
        compiled
    }

    /// Give every function in the impls of `defs` a global slot, named like
    /// `Point::norm`, returning them to compile.
    fn declare_methods<'d>(&mut self, defs: &'d [Definition]) -> Vec<(u32, Id, &'d Method)> {
        let mut methods = Vec::new();
        for def in defs {
            let Definition::Impl {
                target,
                methods: ms,
                ..
            } = def
            else {
                continue;
            };
            // `Point` for `impl Point`, `str` for `impl str` (in the prelude).
            let type_name = match target {
                Type::Named(name, ..) => name.clone(),
                Type::Int | Type::Float | Type::Bool | Type::String => target.to_string().into(),
                _ => continue,
            };
            for m in ms {
                let global: Id = format!("{type_name}::{}", m.name).into();
                let slot = self.define_global(global.clone());
                methods.push((slot, global, m));
            }
        }
        methods
    }

    /// Compile functions from impls. A method is a function whose first
    /// parameter is `self`.
    fn compile_methods(
        &mut self,
        methods: Vec<(u32, Id, &Method)>,
        types: &Types,
    ) -> Vec<(u32, Rc<Proto>)> {
        methods
            .into_iter()
            .map(|(slot, global, m)| {
                let captured = captured_locals(&m.params, &m.body, false);
                let mut fc = FnCompiler::new(self, types, &captured, false);
                let proto = fc.function(
                    Some(global),
                    &m.params,
                    m.rtype.as_ref(),
                    &m.body,
                    m.name_span,
                );
                (slot, Rc::new(proto))
            })
            .collect()
    }

    /// Record the layout of a struct or enum definition.
    fn define_data(&mut self, def: &Definition) {
        match def {
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
                            // As written in source: `Some`, `Shape::Circle`.
                            name: prelude::variant_display(name, &v.name),
                            tag: tag as u32,
                            fields: None,
                        });
                        (v.name.clone(), layout)
                    })
                    .collect();
                self.data.insert(name.clone(), DataDef::Enum { variants });
            }
            Definition::Function { .. } | Definition::Impl { .. } => {}
        }
    }

    /// The layout of tuples of `n` values: shown as `(a, b)`, with no name.
    fn tuple_layout(&mut self, n: usize) -> Rc<Layout> {
        self.tuples
            .entry(n)
            .or_insert_with(|| {
                Rc::new(Layout {
                    name: String::new(),
                    tag: 0,
                    fields: None,
                })
            })
            .clone()
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
                    // Values have one representation whatever their type, so
                    // a generic function compiles once, like any other.
                    type_params: _,
                    public: _,
                    native,
                } => {
                    let slot = self.define_global(name.clone());
                    if *native {
                        self.new_natives.push((slot, name.clone()));
                    } else {
                        fns.push((slot, name, name_span, params, body, rtype));
                    }
                }
                Definition::Struct { .. } | Definition::Enum { .. } => self.define_data(def),
                Definition::Impl { .. } => {}
            }
        }
        let methods = self.declare_methods(&program.definitions);
        let methods = self.natives(methods);

        let mut main = None;
        let mut main_result = None;
        let mut functions = Vec::new();
        for (slot, name, name_span, params, body, rtype) in fns {
            if name == "main" {
                if !params.is_empty() {
                    return Err(Diagnostic::new("'main' cannot take parameters")
                        .with_span(name_span.start, name_span.end)
                        .with_help(Some("remove the parameters: `fn main() { ... }`".into())));
                }
                main = Some(slot);
                if matches!(rtype, Some(Type::Named(n, ..)) if n == "Result") {
                    main_result = Some(*name_span);
                }
            }
            let captured = captured_locals(params, body, false);
            let mut fc = FnCompiler::new(self, types, &captured, false);
            let proto = fc.function(Some(name.clone()), params, rtype.as_ref(), body, *name_span);
            functions.push((slot, Rc::new(proto)));
        }
        let methods = self.compile_methods(methods, types);
        functions.extend(methods);
        let mut all = std::mem::take(&mut self.new_functions);
        all.extend(functions);
        let functions = all;

        let main = main.ok_or_else(|| {
            Diagnostic::new("no 'main' function found; add `fn main() { ... }` as the entry point")
        })?;
        Ok(Compiled {
            functions,
            main,
            main_result,
            literals: std::mem::take(&mut self.new_literals),
            natives: std::mem::take(&mut self.new_natives),
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
            functions: std::mem::take(&mut self.new_functions),
            natives: std::mem::take(&mut self.new_natives),
        }
    }
}

/// `start` and `end` if `iter` is the range `start..end` (or
/// `Iter::range(start, end)`), which a `for` loop counts through directly.
fn range_bounds<'e>(iter: &'e Expr, types: &Types) -> Option<(&'e Expr, &'e Expr)> {
    let Expression::Call(callee, args) = &iter.kind else {
        return None;
    };
    match (types.function(callee), &args[..]) {
        (Some(f), [start, end]) if f == "Iter::range" => Some((start, end)),
        _ => None,
    }
}

/// Identifies a variable declaration (a parameter or `let`) by where its name
/// appears.
type DeclKey = (crate::ast::Source, u32, u32);

fn decl_key(v: &Variable) -> DeclKey {
    (v.span.start.source, v.span.start.pos, v.span.end.pos)
}

/// Where a name refers to.
enum Place {
    Local { reg: Reg, cell: bool },
    Upvalue { index: u16, cell: bool },
    Global(u32),
}

struct Local {
    name: Id,
    reg: Reg,
    /// The register holds a cell, because the variable is a `var` that a
    /// closure captures.
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
    /// For a closure bound by `let`, the name it is bound to, which refers
    /// to the closure itself inside it.
    itself: Option<Id>,
    /// Lexical scopes, innermost last.
    scopes: Vec<Vec<Local>>,
    /// The next free register.
    next: Reg,
    /// How many registers the function needs.
    max: Reg,
    /// The function's name or the whole closure, for instructions that have
    /// no expression of their own.
    span: Span,
    /// The loops being compiled, innermost last.
    loops: Vec<Loop>,
}

/// A loop being compiled: where `continue` goes, and the jumps to patch.
struct Loop {
    /// The start of the next iteration, if it is already known.
    next: Option<u32>,
    /// Jumps for `continue` to patch once `next` is known.
    continues: Vec<usize>,
    /// Jumps for `break` to patch to just after the loop.
    breaks: Vec<usize>,
}

struct FnCompiler<'a> {
    compiler: &'a mut Compiler,
    types: &'a Types,
    captured: &'a HashSet<DeclKey>,
    /// The function being compiled and the ones it is nested in, innermost last.
    fns: Vec<FnState>,
    /// Whether this is a REPL input.
    repl: bool,
    /// The name the next closure compiled is bound to by `let`, if any.
    let_name: Option<Id>,
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
            let_name: None,
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
            itself: self.let_name.take(),
            scopes: vec![Vec::new()],
            next: 0,
            max: 0,
            span,
            loops: Vec::new(),
        });

        // Arguments arrive in the first registers. Parameters can't be
        // assigned to, so they never need a cell.
        for p in params {
            let reg = self.alloc();
            self.declare(p.name.clone(), reg, false);
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
        if let Some((index, cell)) = self.upvalue(f, name) {
            return Place::Upvalue { index, cell };
        }
        match self.compiler.globals.get(name) {
            Some(&slot) => Place::Global(slot),
            None => unreachable!("'{name}' is undefined; the type checker should have caught this"),
        }
    }

    /// The index of `name` among function `f`'s captured values, capturing
    /// it if needed, and whether it is a cell. `None` if no enclosing
    /// function declares it.
    fn upvalue(&mut self, f: usize, name: &Id) -> Option<(u16, bool)> {
        if f == 0 {
            return None;
        }
        // Inside `let g = fn() { ... }`, `g` is the closure itself, which
        // shadows any variable of that name outside it.
        let (capture, cell) = if self.fns[f].itself.as_ref() == Some(name) {
            (Capture::Itself, false)
        } else {
            match self.find_local(f - 1, name) {
                Some(l) => (Capture::Local(l.reg), l.cell),
                None => {
                    let (index, cell) = self.upvalue(f - 1, name)?;
                    (Capture::Upvalue(index), cell)
                }
            }
        };
        let captures = &mut self.fns[f].captures;
        let index = match captures.iter().position(|c| *c == capture) {
            Some(i) => i,
            None => {
                captures.push(capture);
                captures.len() - 1
            }
        };
        Some((index as u16, cell))
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
            Statement::Let {
                var,
                value,
                mutable,
            } => {
                self.let_(var, value, *mutable);
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
                self.loop_body(Some(start), |fc| fc.stmt(body, None));
                self.emit(Op::Jump { target: start }, cond.span);
                self.patch(exit);
                self.end_loop();
                if let Some(dst) = dst {
                    self.void(dst);
                }
            }
            Statement::LetPattern { pattern, value, .. } => {
                self.let_pattern(pattern, value);
                if let Some(dst) = dst {
                    self.void(dst);
                }
            }
            Statement::For {
                pattern,
                iter,
                body,
            } => {
                let mark = self.mark();
                match range_bounds(iter, self.types) {
                    Some((start, end)) => self.for_range(pattern, start, end, body),
                    None if self.types.for_iter(iter).is_some_and(|f| f == "List::iter") => {
                        self.for_list(pattern, iter, body)
                    }
                    None => self.for_iter(pattern, iter, body),
                }
                self.release(mark);
                if let Some(dst) = dst {
                    self.void(dst);
                }
            }
            Statement::Break(span) | Statement::Continue(span) => {
                let jump = self.emit(Op::Jump { target: 0 }, *span);
                let is_break = matches!(stmt, Statement::Break(_));
                let l = self
                    .cur()
                    .loops
                    .last_mut()
                    .expect("the checker rejects 'break' and 'continue' outside a loop");
                match (is_break, l.next) {
                    (true, _) => l.breaks.push(jump),
                    (false, None) => l.continues.push(jump),
                    (false, Some(next)) => {
                        self.cur().code[jump] = Op::Jump { target: next };
                    }
                }
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

    /// Compile a loop's body with `continue` going to `next` (or, if it
    /// isn't known yet, to wherever [`FnCompiler::continue_here`] is
    /// called). [`FnCompiler::end_loop`] finishes the loop.
    fn loop_body(&mut self, next: Option<u32>, body: impl FnOnce(&mut Self)) {
        self.cur().loops.push(Loop {
            next,
            continues: Vec::new(),
            breaks: Vec::new(),
        });
        body(self);
    }

    /// Point the innermost loop's `continue`s at the next instruction.
    fn continue_here(&mut self) {
        let continues = std::mem::take(&mut self.cur().loops.last_mut().unwrap().continues);
        for at in continues {
            self.patch(at);
        }
    }

    /// Finish the innermost loop: its `break`s go to the next instruction.
    fn end_loop(&mut self) {
        let l = self.cur().loops.pop().expect("a loop is being compiled");
        debug_assert!(l.continues.is_empty(), "every continue is patched");
        for at in l.breaks {
            self.patch(at);
        }
    }

    /// `for var in start..end { body }`, as a loop that counts, without
    /// making an `Iter`. `end` is evaluated once, before the first
    /// iteration, as `Iter::range` would.
    fn for_range(&mut self, pattern: &Pattern, start: &Expr, end: &Expr, body: &Statement) {
        let i = self.alloc();
        self.expr(start, i);
        let limit = self.alloc();
        self.expr(end, limit);
        let one = self.alloc();
        let top = self.here();
        let more = self.alloc();
        self.emit(
            Op::LtI {
                dst: more,
                a: i,
                b: limit,
            },
            end.span,
        );
        let exit = self.emit(
            Op::JumpIfFalse {
                cond: more,
                target: 0,
            },
            end.span,
        );
        // The loop variable is the count itself: it is immutable, so
        // nothing in the body can change it, and a closure copies it.
        self.cur().scopes.push(Vec::new());
        self.bind_loop_pattern(pattern, i);
        self.loop_body(None, |fc| fc.stmt(body, None));
        self.cur().scopes.pop();
        self.continue_here();
        // `i < limit`, so this can't overflow.
        self.emit(Op::LoadInt { dst: one, value: 1 }, pattern.span);
        self.emit(
            Op::AddI {
                dst: i,
                a: i,
                b: one,
            },
            pattern.span,
        );
        self.emit(Op::Jump { target: top }, pattern.span);
        self.patch(exit);
        self.end_loop();
    }

    /// `for pattern in list { body }`, as a loop over the list's indexes,
    /// without making an `Iter`. A list never changes, so its length is
    /// read once, and this goes over the same values `List::iter` would.
    fn for_list(&mut self, pattern: &Pattern, list: &Expr, body: &Statement) {
        let l = self.alloc();
        self.expr(list, l);
        let len = self.alloc();
        let index = self.compiler.globals[&Id::from("List::len")];
        let arg = self.alloc();
        self.emit(Op::GetGlobal { dst: len, index }, list.span);
        self.emit(Op::Move { dst: arg, src: l }, list.span);
        self.emit(
            Op::Call {
                dst: len,
                callee: len,
                argc: 1,
            },
            list.span,
        );
        self.release(arg);
        let i = self.alloc();
        self.emit(Op::LoadInt { dst: i, value: 0 }, list.span);
        let one = self.alloc();
        let more = self.alloc();
        let x = self.alloc();
        let top = self.here();
        self.emit(
            Op::LtI {
                dst: more,
                a: i,
                b: len,
            },
            list.span,
        );
        let exit = self.emit(
            Op::JumpIfFalse {
                cond: more,
                target: 0,
            },
            list.span,
        );
        self.emit(
            Op::Index {
                dst: x,
                list: l,
                index: i,
            },
            pattern.span,
        );
        self.cur().scopes.push(Vec::new());
        self.bind_loop_pattern(pattern, x);
        self.loop_body(None, |fc| fc.stmt(body, None));
        self.cur().scopes.pop();
        self.continue_here();
        self.emit(Op::LoadInt { dst: one, value: 1 }, pattern.span);
        self.emit(
            Op::AddI {
                dst: i,
                a: i,
                b: one,
            },
            pattern.span,
        );
        self.emit(Op::Jump { target: top }, pattern.span);
        self.patch(exit);
        self.end_loop();
    }

    /// A piece of a template string, as a string, into `dst`: its text, or
    /// its expression's value, passed to the `to_str` method the checker
    /// found for it unless it is a string already.
    fn template_part(&mut self, part: &TemplatePart, dst: Reg, span: Span) {
        match part {
            TemplatePart::Text(text) => {
                let index = self.compiler.literal(text);
                self.emit(Op::LoadStr { dst, index }, span);
            }
            TemplatePart::Expr(e) => match self.types.template_to_str(e).cloned() {
                None => self.expr(e, dst),
                Some(global) => {
                    let callee = self.alloc();
                    let index = self.compiler.globals[&global];
                    self.emit(Op::GetGlobal { dst: callee, index }, e.span);
                    let arg = self.alloc();
                    self.expr(e, arg);
                    let at = self.emit(
                        Op::Call {
                            dst,
                            callee,
                            argc: 1,
                        },
                        e.span,
                    );
                    self.cur().call_names.insert(at, global);
                    self.release(callee);
                }
            },
        }
    }

    /// `for pattern in iter { body }`: call the `Iter`'s step function until
    /// it gives `None`.
    fn for_iter(&mut self, pattern: &Pattern, iter: &Expr, body: &Statement) {
        let it = self.alloc();
        self.expr(iter, it);
        if let Some(global) = self.types.for_iter(iter).cloned() {
            // Get the `Iter` from the value's `iter` method.
            let callee = self.alloc();
            let index = self.compiler.globals[&global];
            self.emit(Op::GetGlobal { dst: callee, index }, iter.span);
            let arg = self.alloc();
            self.emit(Op::Move { dst: arg, src: it }, iter.span);
            let at = self.emit(
                Op::Call {
                    dst: it,
                    callee,
                    argc: 1,
                },
                iter.span,
            );
            self.cur().call_names.insert(at, global);
            self.release(callee);
        }
        let (step_field, some_tag) = self.iter_layout();
        let step = self.alloc();
        self.emit(
            Op::GetField {
                dst: step,
                obj: it,
                index: step_field,
            },
            iter.span,
        );
        let item = self.alloc();
        let more = self.alloc();
        let callee = self.alloc();
        let top = self.here();
        self.emit(
            Op::Move {
                dst: callee,
                src: step,
            },
            iter.span,
        );
        self.emit(
            Op::Call {
                dst: item,
                callee,
                argc: 0,
            },
            iter.span,
        );
        self.emit(
            Op::TestTag {
                dst: more,
                obj: item,
                tag: some_tag,
            },
            iter.span,
        );
        let exit = self.emit(
            Op::JumpIfFalse {
                cond: more,
                target: 0,
            },
            iter.span,
        );
        let x = self.alloc();
        self.emit(
            Op::GetField {
                dst: x,
                obj: item,
                index: 0,
            },
            pattern.span,
        );
        self.cur().scopes.push(Vec::new());
        self.bind_loop_pattern(pattern, x);
        self.loop_body(Some(top), |fc| fc.stmt(body, None));
        self.cur().scopes.pop();
        self.emit(Op::Jump { target: top }, iter.span);
        self.patch(exit);
        self.end_loop();
    }

    /// The index of `Iter`'s field `step`, and the tag of `Option::Some`.
    fn iter_layout(&self) -> (u16, u32) {
        let step = match &self.compiler.data["Iter"] {
            DataDef::Struct { fields, .. } => fields.iter().position(|f| f == "step"),
            DataDef::Enum { .. } => None,
        };
        let some = match &self.compiler.data["Option"] {
            DataDef::Enum { variants } => variants.iter().position(|(v, _)| v == "Some"),
            DataDef::Struct { .. } => None,
        };
        (
            step.expect("the prelude's Iter has a field 'step'") as u16,
            some.expect("the prelude's Option has a variant 'Some'") as u32,
        )
    }

    /// Bind a `for` loop's pattern to the value in `reg`, each time
    /// around. A name is the register itself.
    fn bind_loop_pattern(&mut self, pattern: &Pattern, reg: Reg) {
        match &pattern.kind {
            PatternKind::Binding(var) => self.declare(var.name.clone(), reg, false),
            _ => self.irrefutable_pattern(pattern, reg),
        }
    }

    /// Bind the names in `pattern`, which the checker has made sure
    /// matches every value, to the parts of the value in `value`.
    fn irrefutable_pattern(&mut self, pattern: &Pattern, value: Reg) {
        let mut fails = Vec::new();
        self.pattern(pattern, value, &mut fails);
        // An or-pattern such as `true | false` still tests its
        // alternatives, though one of them always matches.
        if !fails.is_empty() {
            let matched = self.emit(Op::Jump { target: 0 }, pattern.span);
            for f in fails {
                self.patch(f);
            }
            self.emit(Op::NoMatch, pattern.span);
            self.patch(matched);
        }
    }

    /// `let pattern = value;`
    fn let_pattern(&mut self, pattern: &Pattern, value: &Expr) {
        let reg = self.alloc();
        self.expr(value, reg);
        self.irrefutable_pattern(pattern, reg);

        // A REPL input's top-level names outlive the input, so they are
        // globals too.
        if self.repl && self.fns.len() == 1 && self.cur().scopes.len() == 1 {
            let mut names = Vec::new();
            pattern_bindings(pattern, &mut names);
            for (name, _) in names {
                let src = self
                    .find_local(0, &name)
                    .expect("the pattern has just bound it")
                    .reg;
                let index = self.compiler.define_global(name);
                self.emit(Op::SetGlobal { index, src }, pattern.span);
            }
        }
    }

    fn let_(&mut self, var: &Variable, value: &Expr, mutable: bool) {
        let reg = self.alloc();

        // A REPL input's top-level `let` outlives the input, so it is a global.
        if self.repl && self.fns.len() == 1 && self.cur().scopes.len() == 1 {
            // A closure can call itself through the new global, so it is
            // defined first; any other value sees the old one, if any.
            let slot = if value.is_closure() {
                let slot = self.compiler.define_global(var.name.clone());
                self.expr(value, reg);
                slot
            } else {
                self.expr(value, reg);
                self.compiler.define_global(var.name.clone())
            };
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

        // Only a `var` can change, so only a captured `var` needs a cell.
        let cell = mutable && self.captured.contains(&decl_key(var));
        if !mutable && value.is_closure() {
            // Inside the closure, its name refers to the closure itself
            // (see `upvalue`).
            self.let_name = Some(var.name.clone());
            self.expr(value, reg);
            self.declare(var.name.clone(), reg, false);
            return;
        }
        if cell && value.is_closure() {
            // The closure captures its own `var`: make the cell first so the
            // closure can capture it, then fill it with the closure.
            self.emit(Op::LoadVoid { dst: reg }, var.span);
            self.emit(Op::NewCell { dst: reg, src: reg }, var.span);
            self.declare(var.name.clone(), reg, true);
            let closure = self.alloc();
            self.expr(value, closure);
            self.emit(
                Op::SetCell {
                    cell: reg,
                    src: closure,
                },
                var.span,
            );
            self.release(closure);
            return;
        }

        // Any other value is compiled before the name is declared, so it
        // sees an earlier variable of the same name (`let a = a + 1;`).
        self.expr(value, reg);
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
            Expression::Void => {
                self.emit(Op::LoadVoid { dst }, span);
            }
            Expression::String(s) => {
                let index = self.compiler.literal(s);
                self.emit(Op::LoadStr { dst, index }, span);
            }
            Expression::Template(parts) => {
                // Build the string up in a register of its own: `dst` may be
                // a variable that a later part reads.
                let mark = self.mark();
                let acc = self.alloc();
                for (i, part) in parts.iter().enumerate() {
                    if i == 0 {
                        self.template_part(part, acc, span);
                    } else {
                        let r = self.alloc();
                        self.template_part(part, r, span);
                        self.emit(
                            Op::AddS {
                                dst: acc,
                                a: acc,
                                b: r,
                            },
                            span,
                        );
                        self.release(r);
                    }
                }
                self.emit(Op::Move { dst, src: acc }, span);
                self.release(mark);
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
                    Place::Upvalue { index, cell: false } => Op::GetUpvalue { dst, index },
                    Place::Upvalue { index, cell: true } => Op::GetUpvalueCell { dst, index },
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
                    Place::Upvalue { index, cell: true } => Op::SetUpvalueCell { index, src },
                    Place::Upvalue { cell: false, .. } => {
                        unreachable!("'{name}' is assigned but immutable; the type checker should have caught this")
                    }
                    Place::Global(index) => Op::SetGlobal { index, src },
                };
                self.emit(op, span);
                // An assignment's value is the value assigned.
                if src != dst {
                    self.emit(Op::Move { dst, src }, span);
                }
            }
            Expression::Call(callee, args)
                if matches!(callee.kind, Expression::Path(..))
                    && self.types.function(callee).is_none() =>
            {
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
            Expression::Path(..) if self.types.function(e).is_some() => {
                let index = self.compiler.globals[self.types.function(e).unwrap()];
                self.emit(Op::GetGlobal { dst, index }, span);
            }
            Expression::Path(enum_name, variant) => {
                let layout = self.variant_layout(enum_name, variant);
                self.new_data(dst, layout, 0, 0, span);
            }
            Expression::StructLit {
                name, fields, base, ..
            } => self.struct_lit(name, fields, base.as_deref(), dst, span),
            Expression::Field(obj, _, _) => {
                let obj = self.expr_any(obj);
                let index = self.types.field(e);
                self.emit(Op::GetField { dst, obj, index }, span);
            }
            Expression::Match(scrutinee, arms) => self.match_(scrutinee, arms, dst, span),
            Expression::Error(_) => {
                unreachable!("the checker reports errors before anything is compiled")
            }
            Expression::Try(inner) => {
                // `Ok(v)` or `Some(v)` gives `v`. Anything else (`Err(e)` or
                // `None`) is returned as it is: a value's representation
                // doesn't depend on its type arguments, so it is also the
                // function's `Err(e)` or `None`.
                let mark = self.mark();
                let value = self.expr_any(inner);
                let (enum_name, variant) = match self.types.try_on(e) {
                    TryOn::Option => ("Option", "Some"),
                    TryOn::Result => ("Result", "Ok"),
                };
                let tag = self.variant_tag(&enum_name.into(), &variant.into());
                let ok = self.alloc();
                self.emit(
                    Op::TestTag {
                        dst: ok,
                        obj: value,
                        tag,
                    },
                    span,
                );
                let jump = self.emit(
                    Op::JumpIfTrue {
                        cond: ok,
                        target: 0,
                    },
                    span,
                );
                self.emit(Op::Return { src: value }, span);
                self.patch(jump);
                self.emit(
                    Op::GetField {
                        dst,
                        obj: value,
                        index: 0,
                    },
                    span,
                );
                self.release(mark);
            }
            Expression::Call(callee, args) => {
                let base = self.alloc();
                let method = self.types.function(callee).cloned();
                let mut argc = args.len() as u8;
                match (&callee.kind, &method) {
                    // `obj.name(args)` calls `Type::name(obj, args)`.
                    (Expression::Field(obj, ..), Some(global)) => {
                        let index = self.compiler.globals[global];
                        self.emit(Op::GetGlobal { dst: base, index }, callee.span);
                        let r = self.alloc();
                        self.expr(obj, r);
                        argc += 1;
                    }
                    _ => self.expr(callee, base),
                }
                for arg in args {
                    let r = self.alloc();
                    self.expr(arg, r);
                }
                let at = self.emit(
                    Op::Call {
                        dst,
                        callee: base,
                        argc,
                    },
                    span,
                );
                match (&callee.kind, method) {
                    (_, Some(global)) => {
                        self.cur().call_names.insert(at, global);
                    }
                    (Expression::Var(name), None) => {
                        self.cur().call_names.insert(at, name.clone());
                    }
                    _ => {}
                }
            }
            Expression::List(items) => {
                let start = self.mark();
                for x in items {
                    let r = self.alloc();
                    self.expr(x, r);
                }
                let count =
                    u32::try_from(items.len()).expect("a list literal this long fits in memory");
                self.emit(Op::NewList { dst, start, count }, span);
            }
            Expression::Tuple(items) => {
                let start = self.alloc_n(items.len());
                for (i, x) in items.iter().enumerate() {
                    self.expr(x, start + i as Reg);
                }
                let layout = self.compiler.tuple_layout(items.len());
                self.new_data(dst, layout, start, items.len(), span);
            }
            Expression::Index(list, index) => {
                // Use the list variable's own register only if the index
                // can't assign to it first.
                let l = if is_simple(index) {
                    self.expr_any(list)
                } else {
                    let r = self.alloc();
                    self.expr(list, r);
                    r
                };
                let i = self.expr_any(index);
                self.emit(
                    Op::Index {
                        dst,
                        list: l,
                        index: i,
                    },
                    span,
                );
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
        let int = t == OperandType::Int;
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
            Op2::LessThan if int => Op::LtI { dst, a, b },
            Op2::LessThan => Op::LtD { dst, a, b },
            Op2::LessEqual if float => Op::LeF { dst, a, b },
            Op2::LessEqual if int => Op::LeI { dst, a, b },
            Op2::LessEqual => Op::LeD { dst, a, b },
            Op2::GreaterThan if float => Op::LtF { dst, a: b, b: a },
            Op2::GreaterThan if int => Op::LtI { dst, a: b, b: a },
            Op2::GreaterThan => Op::LtD { dst, a: b, b: a },
            Op2::GreaterEqual if float => Op::LeF { dst, a: b, b: a },
            Op2::GreaterEqual if int => Op::LeI { dst, a: b, b: a },
            Op2::GreaterEqual => Op::LeD { dst, a: b, b: a },
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
            // `void` is its type's only value, so it always matches.
            PatternKind::Wildcard | PatternKind::Void => {}
            PatternKind::Or(alternatives) => self.or_pattern(alternatives, value, fails),
            PatternKind::Error(_) => {
                unreachable!("the checker reports errors before anything is compiled")
            }
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
            PatternKind::Tuple(items) => {
                for (i, sub) in items.iter().enumerate() {
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

    /// `a | b | c`: try each alternative in turn. Each binds the same names,
    /// so every name gets one register, and whichever alternative matches
    /// moves its values there.
    fn or_pattern(&mut self, alternatives: &[Pattern], value: Reg, fails: &mut Vec<usize>) {
        let mut names = Vec::new();
        pattern_bindings(&alternatives[0], &mut names);
        let slots: Vec<(Id, Reg)> = names.into_iter().map(|(n, _)| (n, self.alloc())).collect();
        let mut matched = Vec::new();
        for (i, alt) in alternatives.iter().enumerate() {
            let mark = self.mark();
            self.cur().scopes.push(Vec::new());
            let mut alt_fails = Vec::new();
            self.pattern(alt, value, &mut alt_fails);
            let locals = self.cur().scopes.pop().expect("the alternative's scope");
            for (name, slot) in &slots {
                let local = locals
                    .iter()
                    .find(|l| l.name == *name)
                    .expect("the checker makes every alternative bind the same names");
                self.emit(
                    Op::Move {
                        dst: *slot,
                        src: local.reg,
                    },
                    alt.span,
                );
            }
            self.release(mark);
            if i + 1 == alternatives.len() {
                fails.extend(alt_fails);
            } else {
                matched.push(self.emit(Op::Jump { target: 0 }, alt.span));
                for f in alt_fails {
                    self.patch(f);
                }
            }
        }
        for m in matched {
            self.patch(m);
        }
        for (name, slot) in slots {
            self.declare(name, slot, false);
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
            PatternKind::Error(_) => {
                unreachable!("the checker reports errors before anything is compiled")
            }
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

    /// Declare the variable a pattern binds, whose value is in `reg`. It
    /// can't be assigned to, so it never needs a cell.
    fn bind(&mut self, var: &Variable, reg: Reg) {
        self.declare(var.name.clone(), reg, false);
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
        | Expression::Void
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
                let repl_global = self.repl && self.depth == 0 && self.scopes.len() == 1;
                // A closure is in the scope of its own name (see `let_`).
                let declare_first = value.is_closure() && !repl_global;
                if declare_first {
                    let key = decl_key(var);
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .1
                        .push((var.name.clone(), key));
                }
                self.expr(value);
                if !repl_global && !declare_first {
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
            Statement::LetPattern { pattern, value, .. } => {
                self.expr(value);
                let mut vars = Vec::new();
                pattern_bindings(pattern, &mut vars);
                self.scopes.last_mut().unwrap().1.extend(vars);
            }
            Statement::For {
                pattern,
                iter,
                body,
            } => {
                self.expr(iter);
                let mut vars = Vec::new();
                pattern_bindings(pattern, &mut vars);
                self.scope(vars, |s| s.stmt(body));
            }
            Statement::Break(_) | Statement::Continue(_) => {}
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
            | Expression::Void
            | Expression::Path(..) => {}
            Expression::Grouping(e)
            | Expression::Unary(_, e)
            | Expression::Field(e, ..)
            | Expression::Try(e) => self.expr(e),
            Expression::Error(es) | Expression::List(es) | Expression::Tuple(es) => {
                es.iter().for_each(|e| self.expr(e))
            }
            Expression::Template(parts) => {
                for part in parts {
                    if let TemplatePart::Expr(e) = part {
                        self.expr(e);
                    }
                }
            }
            Expression::Index(list, index) => {
                self.expr(list);
                self.expr(index);
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
        PatternKind::Error(subs) | PatternKind::Tuple(subs) => {
            subs.iter().for_each(|f| pattern_bindings(f, out))
        }
        // Every alternative binds the same names; the first's stand for them.
        PatternKind::Or(alternatives) => pattern_bindings(&alternatives[0], out),
        PatternKind::Wildcard
        | PatternKind::Int(_)
        | PatternKind::Str(_)
        | PatternKind::Bool(_)
        | PatternKind::Void => {}
    }
}
