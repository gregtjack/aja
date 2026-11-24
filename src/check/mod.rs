//! Static type checking with local type inference.
//!
//! Aja infers types inside function bodies and requires them on function
//! signatures (DESIGN.md, rule 5). Inference works by unification, the core of
//! Hindley–Milner: every unknown type (an unannotated `let`, a closure
//! parameter, a closure's return type) starts as a type variable, and each use
//! adds an equation such as "the operands of `+` have the same type" or "this
//! argument has the parameter's type". Solving the equations as they arise
//! fills in the variables.
//!
//! What we leave out of Hindley–Milner is *generalization* (let-polymorphism):
//! a closure such as `let id = fn(x) { x };` gets one type, decided by its
//! uses, rather than a polymorphic type usable at many. Generic functions are
//! declared explicitly instead: `fn id<T>(x: T) -> T`. Inside its body, `T`
//! is a [`Ty::Param`], a type that is equal only to itself, so the body must
//! work for every `T`. Each use of the function elsewhere gets its own fresh
//! type variable for `T`, which the arguments (or the expected result) solve.
//! Generic structs and enums work the same way: `Option::Some(1)` makes an
//! `Option<?a>` and solves `?a` to `int`.
//!
//! Some operators work on more than one type (`+` on `int`, `float`, or
//! `str`, `==` on any of `int`, `float`, `bool`, `str`). A type variable can carry such a
//! restriction (a [`Class`]) until it is resolved, much like Rust's
//! `{integer}`. The same mechanism gives type parameters bounds: `T: Hash`
//! makes each use of a generic create a variable restricted to `Hash`
//! types, and lets the body use `==` on a `T`.
//!
//! Every binding must end up with a fully known type. If one doesn't, the
//! checker asks for an annotation rather than falling back to a dynamic type.

use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        types::Type, Definition, Expr, Expression, Id, Op1, Op2, PatternKind, Program, Source,
        Span, Statement, Variable,
    },
    builtins::{Params, BUILTINS},
    diagnostic::{closest_match, Diagnostic},
    prelude,
};

mod data;
mod methods;
#[cfg(test)]
mod tests;

use data::{DeferredBound, EnumInfo, StructInfo};
use methods::MethodInfo;

/// A type as the checker sees it: like [`Type`], plus inference variables
/// and two types for special cases.
#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Int,
    Float,
    Bool,
    Str,
    Void,
    Fn(Vec<Ty>, Box<Ty>),
    /// A tuple of two or more values.
    Tuple(Vec<Ty>),
    /// A struct or enum, by name, with its type arguments.
    Named(Id, Vec<Ty>),
    /// A type parameter, inside the generic definition that declares it.
    /// It stands for any type, so it is equal only to itself.
    Param(Id),
    /// An inference variable, indexing [`Checker::vars`].
    Var(usize),
    /// The type of something that never produces a value, such as a block
    /// that always `return`s. It fits wherever a value is expected.
    Never,
    /// The type of an expression whose error has already been reported.
    /// It fits anywhere, so one mistake doesn't cause a cascade of errors.
    Error,
}

/// A type parameter of a definition, with the traits its bounds require
/// (and those they imply): the `K` of `struct Map<K: Hash, V>`.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct Generic {
    pub name: Id,
    pub bounds: Class,
}

impl Generic {
    /// How it is declared: `K: Hash`, or `V`.
    pub fn declared(&self) -> String {
        match self.bounds.show() {
            b if b.is_empty() => self.name.to_string(),
            b => format!("{}: {b}", self.name),
        }
    }
}

/// What a type must support: the requirements that uses of an unresolved
/// type variable have put on it, or the bounds declared on a type parameter
/// (`T: Hash`). It is a set: the requirements of two uses together are both.
///
/// `Eq`, `Hash`, and `Ord` are the built-in traits, which a type has by its
/// shape (see [`Class::admits`] and [`Checker::has_traits`]) and which can be
/// written as bounds. `ADD` and `NUM` are what `+` and the other arithmetic
/// operators need; they can't be written, and no type parameter has them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(super) struct Class(u8);

impl Class {
    /// Any type.
    pub const ANY: Class = Class(0);
    /// Supports `==`: int, float, bool, str, and structs, enums, tuples, and
    /// lists made of such types.
    pub const EQ: Class = Class(1);
    /// Can be hashed, for a map's keys: `Eq` without float, since NaN isn't
    /// equal to itself.
    pub const HASH: Class = Class(2);
    /// Has an order: int, bool, str, and structs, enums, tuples, and lists
    /// made of such types (not float, for the same reason).
    pub const ORD: Class = Class(4);
    /// Supports `+`: int, float, or str (concatenation).
    pub const ADD: Class = Class(8);
    /// Supports arithmetic: int or float.
    pub const NUM: Class = Class(16);
    /// Supports `<` and the other comparisons: int, float, or an `Ord`
    /// type. (A float has an order, but not one that sorting can rely on.)
    pub const CMP: Class = Class(32);
    /// The traits, which bounds can name.
    const TRAITS: Class = Class(1 | 2 | 4);
    /// The traits, by the names bounds use for them.
    pub const NAMES: [(&'static str, Class); 3] = [
        ("Eq", Class::EQ),
        ("Hash", Class::HASH),
        ("Ord", Class::ORD),
    ];

    /// The types belonging to both `self` and `other`: both sets of
    /// requirements.
    pub fn meet(self, other: Class) -> Class {
        Class(self.0 | other.0)
    }

    /// Whether `self` includes every requirement of `other`.
    pub fn has(self, other: Class) -> bool {
        self.0 & other.0 == other.0
    }

    /// The requirements of `self` that `other` lacks.
    pub fn minus(self, other: Class) -> Class {
        Class(self.0 & !other.0)
    }

    /// Just the traits among the requirements.
    pub fn traits(self) -> Class {
        Class(self.0 & Class::TRAITS.0)
    }

    /// With the traits these imply: a type that is `Hash` or `Ord` is `Eq`.
    pub fn implied(self) -> Class {
        if self.0 & (Class::HASH.0 | Class::ORD.0) != 0 {
            self.meet(Class::EQ)
        } else {
            self
        }
    }

    /// The traits of `self`, as a bound is written: `Eq + Hash`. An implied
    /// `Eq` is left out, so `HASH | EQ` is `Hash`.
    pub fn show(self) -> String {
        let shown = if self.has(Class::HASH) || self.has(Class::ORD) {
            self.minus(Class::EQ)
        } else {
            self
        };
        let names: Vec<&str> = Class::NAMES
            .iter()
            .filter(|(_, c)| shown.has(*c))
            .map(|(n, _)| *n)
            .collect();
        names.join(" + ")
    }

    /// Whether the concrete type `t` belongs to this class, for types other
    /// than type parameters, structs, enums, and tuples (see
    /// [`Checker::admits`]).
    fn admits(self, t: &Ty) -> bool {
        let supports = match t {
            Ty::Never | Ty::Error => return true,
            Ty::Int => Class(u8::MAX),
            Ty::Float => Class(Class::EQ.0 | Class::ADD.0 | Class::NUM.0 | Class::CMP.0),
            Ty::Bool | Ty::Void => Class(Class::TRAITS.0 | Class::CMP.0),
            Ty::Str => Class(Class::TRAITS.0 | Class::ADD.0 | Class::CMP.0),
            _ => Class::ANY,
        };
        supports.has(self)
    }
}

/// A type error, with where it happened and how to fix it.
#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub span: Span,
    pub help: Option<String>,
    pub notes: Vec<String>,
}

impl TypeError {
    fn new(message: impl Into<String>, span: Span) -> Self {
        TypeError {
            message: message.into(),
            span,
            help: None,
            notes: Vec::new(),
        }
    }

    fn help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    fn note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        Diagnostic::new(self.message.clone())
            .with_span(self.span.start, self.span.end)
            .with_help(self.help.clone())
            .with_notes(self.notes.clone())
    }
}

/// The type an operator works on, which a backend needs in order to pick a
/// typed instruction (e.g. integer or float addition).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandType {
    Int,
    Float,
    Bool,
    Str,
    /// A struct, enum, or tuple, compared field by field.
    Data,
}

/// What a `?` is applied to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TryOn {
    Option,
    Result,
}

/// What the checker learned that the backends need: the operand type of
/// every arithmetic, comparison, and negation expression, which field each
/// field access reads, what each `?` is applied to, and which function each
/// method call and associated function refers to.
#[derive(Debug, Default)]
pub struct Types {
    operands: HashMap<SpanKey, OperandType>,
    fields: HashMap<SpanKey, u16>,
    tries: HashMap<SpanKey, TryOn>,
    functions: HashMap<SpanKey, Id>,
    for_iters: HashMap<SpanKey, Id>,
    templates: HashMap<SpanKey, Id>,
}

/// Identifies an expression by where it is in the source. Two operator
/// expressions never cover exactly the same text.
type SpanKey = (Source, u32, u32);

fn key(span: Span) -> SpanKey {
    (span.start.source, span.start.pos, span.end.pos)
}

impl Types {
    /// The operand type of the operator expression `e` (a binary operator
    /// other than `&&` / `||`, or a negation).
    pub fn operand(&self, e: &Expr) -> OperandType {
        *self
            .operands
            .get(&key(e.span))
            .expect("the type checker records the operand type of every operator")
    }

    /// The index, in declaration order, of the field that the field access
    /// `e` reads.
    pub fn field(&self, e: &Expr) -> u16 {
        *self
            .fields
            .get(&key(e.span))
            .expect("the type checker records the field of every field access")
    }

    /// Whether the `?` expression `e` is applied to an `Option` or a `Result`.
    pub fn try_on(&self, e: &Expr) -> TryOn {
        *self
            .tries
            .get(&key(e.span))
            .expect("the type checker records what every '?' is applied to")
    }

    /// The function, by the name it is compiled under (`Point::norm`), that
    /// `e` refers to: the callee of a method call (`p.norm` in `p.norm()`),
    /// or an associated function (`Point::origin`). `None` for anything else,
    /// such as a variant or a field.
    pub fn function(&self, e: &Expr) -> Option<&Id> {
        self.functions.get(&key(e.span))
    }

    /// The `iter` method, by the name it is compiled under, that a `for`
    /// loop over `e` calls to get an `Iter`, or `None` if `e` is one.
    pub fn for_iter(&self, e: &Expr) -> Option<&Id> {
        self.for_iters.get(&key(e.span))
    }

    /// The `to_str` method, by the name it is compiled under, that turns
    /// `e`, an expression in a template string, into a string, or `None` if
    /// `e` is a string already.
    pub fn template_to_str(&self, e: &Expr) -> Option<&Id> {
        self.templates.get(&key(e.span))
    }
}

/// Type-check a program that has no standard library modules in it.
#[cfg(test)]
pub fn check_program(program: &Program) -> Result<Types, Vec<TypeError>> {
    check_modules(program, HashSet::new())
}

/// Type-check a program put together from modules, where `std_sources` are
/// the files of the standard library's.
pub fn check_modules(
    program: &Program,
    std_sources: HashSet<Source>,
) -> Result<Types, Vec<TypeError>> {
    let mut checker = Checker::new();
    checker.std_sources = std_sources;
    checker.program(program);
    checker.finish()
}

/// Type-check the prelude itself, for compiling its functions.
pub fn check_prelude() -> Types {
    let mut checker = Checker::bare();
    checker.program(&Program::new(prelude::definitions()));
    match checker.finish() {
        Ok(types) => types,
        Err(errors) => panic!("the prelude has type errors: {errors:?}"),
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum BindingKind {
    /// A `let` variable, which can't be assigned to.
    Let,
    /// A `var` variable, which can.
    Var,
    /// A function or closure parameter.
    Param,
    /// A name bound by a `match` pattern.
    Pattern,
    /// A `for` loop's variable.
    Loop,
    /// A top-level function.
    Function,
    /// A variadic built-in such as `print`, which can only be called directly.
    Variadic,
}

#[derive(Debug, Clone)]
struct Binding {
    ty: Ty,
    kind: BindingKind,
    /// The type parameters of a generic function, which appear in `ty` as
    /// [`Ty::Param`]s and are replaced by fresh variables at each use.
    generics: Vec<Generic>,
    /// Whether its value can reach a `var` of the function it is in, so
    /// it mustn't go to another task: a `let` bound to a closure that uses
    /// one, say. Only what is plain from the code; a value from a call
    /// could reach one too, which the runtime checks.
    reaches_var: bool,
}

/// A name that a closure uses from outside it, where.
#[derive(Debug, Clone)]
struct Capture {
    name: Id,
    span: Span,
    kind: BindingKind,
    reaches_var: bool,
}

/// The function whose body is being checked.
#[derive(Clone)]
struct FnContext {
    /// `None` for a closure.
    name: Option<Id>,
    ret: Ty,
    /// Whether the return type was written out.
    ret_declared: bool,
    /// The function's name, or the whole closure.
    span: Span,
    /// Whether a `?` in it has been reported for not fitting its return
    /// type, which is then not reported again for every other `?`.
    try_reported: bool,
}

/// A binding whose type must be fully known by the end of the enclosing
/// top-level function.
#[derive(Clone)]
struct Pending {
    name: Id,
    ty: Ty,
    span: Span,
    /// e.g. "variable 'x'" or "parameter 'x'".
    what: String,
}

#[derive(Clone)]
pub struct Checker {
    /// For each type variable: what it was solved to (if anything) and
    /// which types it may still become.
    vars: Vec<(Option<Ty>, Class)>,
    /// Lexical scopes, innermost last. `scopes[0]` holds the built-ins and
    /// top-level functions (and, in the REPL, top-level `let`s).
    scopes: Vec<HashMap<Id, Binding>>,
    /// Parameter names of top-level functions and built-ins, for messages.
    param_names: HashMap<Id, Vec<Id>>,
    /// Top-level functions defined by the program (as opposed to built-ins).
    user_fns: HashSet<Id>,
    fns: Vec<FnContext>,
    pending: Vec<Pending>,
    errors: Vec<TypeError>,
    /// Set when unification fails because a type would contain itself.
    infinite: bool,
    /// Set when unification fails because a type lacks what a variable
    /// requires of it: the type, and the requirements.
    unmet: Option<(Ty, Class)>,
    /// The operand type of each operator expression, solved once checking
    /// is done.
    operands: Vec<(SpanKey, Ty)>,
    /// The field index of each field access.
    fields: HashMap<SpanKey, u16>,
    /// What each `?` is applied to.
    tries: HashMap<SpanKey, TryOn>,
    /// The function each method call or associated function refers to.
    functions: HashMap<SpanKey, Id>,
    /// The functions defined in impls, by type and then by name.
    methods: HashMap<Id, HashMap<Id, MethodInfo>>,
    structs: HashMap<Id, StructInfo>,
    enums: HashMap<Id, EnumInfo>,
    /// The type parameters in scope: those of the definition being checked.
    type_params: Vec<Generic>,
    /// While struct and enum definitions are declared: the type arguments
    /// written in them, whose bounds are checked once all are declared.
    deferred_bounds: Option<Vec<DeferredBound>>,
    /// How many loops the statement being checked is in, within the
    /// function or closure being checked (or the `defer` being checked).
    loop_depth: usize,
    /// The `defer` keyword of the deferred code being checked, if any,
    /// within the function or closure being checked. Code there can't
    /// leave it with `return`, `?`, `break`, or `continue`.
    in_defer: Option<Span>,
    /// The `iter` method each `for` loop calls, if it calls one.
    for_iters: HashMap<SpanKey, Id>,
    /// The `to_str` method each expression in a template string calls.
    templates: HashMap<SpanKey, Id>,
    /// The files of the standard library's modules, which can use each
    /// other's private items.
    std_sources: HashSet<Source>,
    /// The type expected of the closure literal about to be checked, if
    /// known: the closure's unannotated parameters start out with its
    /// parameter types, so its body can use them (`xs.map(fn(p) { p.0 })`).
    closure_hint: Option<Ty>,
    /// The closures being checked, innermost last: how many scopes there
    /// were around each, and the names it has been found to use from them.
    open_closures: Vec<(usize, Vec<Capture>)>,
    /// The names each closure checked uses from outside it, by its span.
    captures: HashMap<SpanKey, Vec<Capture>>,
}

impl Checker {
    /// A checker that knows the built-ins and the prelude's types and
    /// functions, ready to check a program or REPL inputs.
    pub fn new() -> Self {
        let mut checker = Self::bare();
        let prelude = Program::new(prelude::definitions());
        checker.declare_types(&prelude);
        checker.declare_impls(&prelude);
        assert!(
            checker.errors.is_empty(),
            "the prelude has type errors: {:?}",
            checker.errors
        );
        checker
    }

    /// A checker that knows only the built-ins.
    fn bare() -> Self {
        let mut checker = Checker {
            vars: Vec::new(),
            scopes: vec![HashMap::new()],
            param_names: HashMap::new(),
            user_fns: HashSet::new(),
            fns: Vec::new(),
            pending: Vec::new(),
            errors: Vec::new(),
            infinite: false,
            unmet: None,
            operands: Vec::new(),
            fields: HashMap::new(),
            tries: HashMap::new(),
            functions: HashMap::new(),
            methods: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            type_params: Vec::new(),
            deferred_bounds: None,
            loop_depth: 0,
            in_defer: None,
            for_iters: HashMap::new(),
            templates: HashMap::new(),
            std_sources: HashSet::new(),
            closure_hint: None,
            open_closures: Vec::new(),
            captures: HashMap::new(),
        };
        for builtin in BUILTINS {
            let binding = match &builtin.params {
                Params::Variadic => Binding {
                    ty: Ty::Error,
                    kind: BindingKind::Variadic,
                    generics: Vec::new(),
                    reaches_var: false,
                },
                Params::Fixed(params) => {
                    let names = params.iter().map(|(n, _)| Id::from(*n)).collect();
                    checker.param_names.insert(Id::from(builtin.name), names);
                    let params = params.iter().map(|(_, t)| checker.from_ast(t)).collect();
                    let ty = Ty::Fn(params, Box::new(checker.from_ast(&builtin.returns)));
                    Binding {
                        ty,
                        kind: BindingKind::Function,
                        generics: Vec::new(),
                        reaches_var: false,
                    }
                }
            };
            checker.scopes[0].insert(Id::from(builtin.name), binding);
        }
        checker
    }

    /// The operand types recorded so far, or the errors found so far in
    /// source order.
    fn finish(&mut self) -> Result<Types, Vec<TypeError>> {
        let operands = std::mem::take(&mut self.operands);
        let mut errors = std::mem::take(&mut self.errors);
        if errors.is_empty() {
            let operands = operands
                .into_iter()
                .map(|(k, t)| {
                    let t = match self.zonk(&t) {
                        Ty::Float => OperandType::Float,
                        Ty::Bool => OperandType::Bool,
                        Ty::Str => OperandType::Str,
                        // A type parameter's values are compared as
                        // whatever they turn out to be, and `void`, which
                        // has one value, as the heap compares it.
                        Ty::Named(..) | Ty::Tuple(_) | Ty::Param(_) | Ty::Void => OperandType::Data,
                        // `never` operands are in code that can't run, and
                        // anything still unknown has been reported.
                        _ => OperandType::Int,
                    };
                    (k, t)
                })
                .collect();
            let fields = std::mem::take(&mut self.fields);
            let tries = std::mem::take(&mut self.tries);
            let functions = std::mem::take(&mut self.functions);
            let for_iters = std::mem::take(&mut self.for_iters);
            let templates = std::mem::take(&mut self.templates);
            return Ok(Types {
                operands,
                fields,
                tries,
                functions,
                for_iters,
                templates,
            });
        }
        errors.sort_by_key(|e| (e.span.start.source, e.span.start.line, e.span.start.col));
        Err(errors)
    }

    fn error(&mut self, err: TypeError) {
        self.errors.push(err);
    }

    // ---- Programs and functions -------------------------------------------

    fn program(&mut self, program: &Program) {
        self.declare_types(program);
        let method_bodies = self.declare_impls(program);

        // Declare every function first, so bodies can call functions defined
        // later in the file (including mutually recursive ones).
        let mut sigs = Vec::new();
        for def in &program.definitions {
            let Definition::Function {
                name,
                name_span,
                type_params,
                params,
                rtype,
                ..
            } = def
            else {
                continue;
            };
            let generics = self.declare_type_params(type_params);
            self.type_params = generics.clone();
            let param_tys: Vec<Ty> = params
                .iter()
                .map(|p| match &p.t {
                    Some(t) => self.from_ast(t),
                    None => self.fresh(Class::ANY),
                })
                .collect();
            let ret = rtype.as_ref().map_or(Ty::Void, |t| self.from_ast(t));
            self.type_params.clear();
            let sig = Ty::Fn(param_tys.clone(), Box::new(ret.clone()));
            self.require_used(type_params, &[&sig], &format!("function '{name}'"));
            sigs.push((param_tys.clone(), ret.clone(), generics.clone()));

            match self.scopes[0].get(name) {
                Some(_) if self.user_fns.contains(name) => {
                    self.error(
                        TypeError::new(
                            format!("function '{name}' is defined more than once"),
                            *name_span,
                        )
                        .help("rename one of the definitions"),
                    );
                }
                Some(_) => {
                    self.error(TypeError::new(
                        format!("function '{name}' conflicts with the built-in function of the same name"),
                        *name_span,
                    ).help("choose a different name"));
                }
                None => {
                    self.scopes[0].insert(
                        name.clone(),
                        Binding {
                            ty: sig,
                            kind: BindingKind::Function,
                            generics,
                            reaches_var: false,
                        },
                    );
                    self.param_names.insert(
                        name.clone(),
                        params.iter().map(|p| p.name.clone()).collect(),
                    );
                    self.user_fns.insert(name.clone());
                }
            }
        }

        let fns = program.definitions.iter().filter_map(|d| match d {
            Definition::Function {
                name,
                name_span,
                params,
                body,
                rtype,
                native,
                ..
            } => Some((name, name_span, params, body, rtype, *native)),
            _ => None,
        });
        for ((name, name_span, params, body, rtype, native), (param_tys, ret, generics)) in
            fns.clone().zip(&sigs)
        {
            // A native function has only a signature to check.
            if native {
                continue;
            }
            let ret = ret.clone();
            let ctx = FnContext {
                name: Some(name.clone()),
                ret,
                ret_declared: rtype.is_some(),
                span: *name_span,
                try_reported: false,
            };
            // Annotations inside the body can name the type parameters.
            self.type_params = generics.clone();
            self.function(ctx, params, param_tys, body);
            self.type_params.clear();
            self.report_unresolved();
        }
        self.check_method_bodies(method_bodies);

        // Signatures must be written out. Report missing parameter types last,
        // so the suggested annotation can use what the bodies revealed.
        for ((name, _, params, _, _, _), (param_tys, _, _)) in fns.zip(&sigs) {
            for (p, t) in params.iter().zip(param_tys) {
                if p.t.is_some() {
                    continue;
                }
                let t = self.zonk(t);
                let help = if Self::free_vars(&t).is_empty() && t != Ty::Error {
                    format!("add a type: `{}: {}`", p.name, self.show(&t))
                } else {
                    format!("add a type, for example `{}: int`", p.name)
                };
                self.error(
                    TypeError::new(
                        format!("parameter '{}' of function '{name}' needs a type annotation", p.name),
                        p.span,
                    )
                    .help(help)
                    .note("function signatures are always written out; types are inferred inside function bodies"),
                );
            }
        }
    }

    /// Check a function or closure body against its signature.
    fn function(
        &mut self,
        ctx: FnContext,
        params: &[Variable],
        param_tys: &[Ty],
        body: &Statement,
    ) {
        self.scopes.push(HashMap::new());
        for (p, t) in params.iter().zip(param_tys) {
            self.define(p.name.clone(), t.clone(), BindingKind::Param);
        }
        let fallback = ctx.span;
        self.fns.push(ctx);
        // A loop around a closure isn't one its body can leave, and its
        // `return` leaves it rather than any `defer` around it.
        let loops = std::mem::replace(&mut self.loop_depth, 0);
        let in_defer = self.in_defer.take();
        let found = self.stmt(body);
        self.loop_depth = loops;
        self.in_defer = in_defer;
        let span = Self::tail_span(body).unwrap_or(fallback);
        self.check_return(&found, span);
        self.fns.pop();
        self.scopes.pop();
    }

    /// Check that a value of type `found`, produced at `span`, can be
    /// returned from the current function.
    fn check_return(&mut self, found: &Ty, span: Span) {
        let Some(ctx) = self.fns.last() else {
            self.error(TypeError::new(
                "'return' can only be used inside a function",
                span,
            ));
            return;
        };
        let expected = ctx.ret.clone();
        let (e, f) = (self.show(&expected), self.show(found));
        if self.unify(&expected, found) {
            return;
        }
        let ctx = self.fns.last().unwrap();
        let err = match (&ctx.name, ctx.ret_declared) {
            (Some(name), false) => {
                let mut err = TypeError::new(
                    format!(
                        "mismatched types: function '{name}' has no return type, so it returns void, \
                         but this is {f}"
                    ),
                    span,
                );
                if !f.contains('_') && !f.contains('{') {
                    err = err.help(format!("add a return type after the parameters: `-> {f}`"));
                }
                err
            }
            (Some(name), true) => TypeError::new(
                format!("mismatched types: function '{name}' is declared to return {e}, but this is {f}"),
                span,
            ),
            (None, true) => TypeError::new(
                format!("mismatched types: this closure is declared to return {e}, but this is {f}"),
                span,
            ),
            (None, false) => TypeError::new(
                format!("mismatched types: this closure returns {e} elsewhere, but this is {f}"),
                span,
            ),
        };
        let err = Self::with_conversion_note(err, &e, &f);
        let err = self.with_param_note(err, &expected, found);
        self.error(err);
        self.poison(&[&expected, found]);
    }

    /// Ask for annotations on bindings whose types are still unknown.
    fn report_unresolved(&mut self) {
        let mut reported = HashSet::new();
        for p in std::mem::take(&mut self.pending) {
            let t = self.zonk(&p.ty);
            let free = Self::free_vars(&t);
            if free.is_empty() || free.iter().all(|v| reported.contains(v)) {
                continue;
            }
            reported.extend(free);
            let numeric = matches!(t, Ty::Var(v) if self.vars[v].1.has(Class::NUM) || self.vars[v].1.has(Class::CMP));
            let mut err = TypeError::new(
                format!(
                    "type annotations needed: cannot infer the type of {}",
                    p.what
                ),
                p.span,
            )
            .help(if numeric {
                format!(
                    "add a type, for example `{}: int` or `{}: float`",
                    p.name, p.name
                )
            } else {
                format!(
                    "add a type, for example `{}: {}`",
                    p.name,
                    self.show_example(&t)
                )
            });
            if !matches!(t, Ty::Var(_)) {
                err = err.note(format!("its type is only partly known: {}", self.show(&t)));
            }
            self.error(err);
        }
    }

    // ---- Statements --------------------------------------------------------

    /// Check a statement, returning the type of the value it produces.
    fn stmt(&mut self, stmt: &Statement) -> Ty {
        match stmt {
            Statement::Block(stmts, tail) => {
                self.scopes.push(HashMap::new());
                let mut diverges = false;
                for s in stmts {
                    let t = self.stmt(s);
                    diverges |= self.resolve(&t) == Ty::Never;
                }
                let t = match tail {
                    Some(tail) => self.expr(tail),
                    None if diverges => Ty::Never,
                    None => Ty::Void,
                };
                self.scopes.pop();
                t
            }
            Statement::Expr(e) => self.expr(e),
            Statement::Let {
                var,
                value,
                mutable,
            } => {
                let kind = if *mutable {
                    BindingKind::Var
                } else {
                    BindingKind::Let
                };
                // A closure can refer to the variable it is being bound to,
                // so the name is defined before its body is checked, with a
                // placeholder type that the closure's type must then match.
                let placeholder = value.is_closure().then(|| {
                    let t = match &var.t {
                        Some(annotation) => self.from_ast(annotation),
                        None => self.fresh(Class::ANY),
                    };
                    self.define(var.name.clone(), t.clone(), kind);
                    t
                });
                let found = match &placeholder {
                    Some(p) if var.t.is_some() => self.expr_expecting(value, &p.clone()),
                    _ => self.expr(value),
                };
                if let Some(p) = &placeholder {
                    if var.t.is_none() {
                        self.expect(p, &found, value.span, |e, f| {
                            format!(
                                "mismatched types: '{}' is used as {e} inside its own definition, but it is {f}",
                                var.name
                            )
                        });
                    }
                }
                let ty = match &var.t {
                    Some(annotation) => {
                        let declared = self.from_ast(annotation);
                        self.expect(&declared, &found, value.span, |e, f| {
                            format!(
                                "mismatched types: '{}' is declared as {e} but was given {f}",
                                var.name
                            )
                        });
                        declared
                    }
                    None => {
                        self.pending.push(Pending {
                            name: var.name.clone(),
                            ty: found.clone(),
                            span: var.span,
                            what: format!("variable '{}'", var.name),
                        });
                        found
                    }
                };
                self.define(var.name.clone(), ty, kind);
                if self.reaches_var(value).is_some() {
                    if let Some(b) = self.scopes.last_mut().unwrap().get_mut(&var.name) {
                        b.reaches_var = true;
                    }
                }
                Ty::Void
            }
            Statement::If(cond, body) => {
                self.condition(cond, "if");
                self.stmt(body);
                Ty::Void
            }
            Statement::While(cond, body) => {
                self.condition(cond, "while");
                self.loop_depth += 1;
                self.stmt(body);
                self.loop_depth -= 1;
                Ty::Void
            }
            Statement::LetPattern { pattern, t, value } => {
                let found = self.expr(value);
                let ty = match t {
                    Some(annotation) => {
                        let declared = self.from_ast(annotation);
                        self.expect(&declared, &found, value.span, |e, f| {
                            format!("mismatched types: this pattern is declared as {e} but was given {f}")
                        });
                        declared
                    }
                    None => {
                        self.pending.push(Pending {
                            name: data::pattern_text(pattern).into(),
                            ty: found.clone(),
                            span: pattern.span,
                            what: "the value of this pattern".to_string(),
                        });
                        found
                    }
                };
                self.irrefutable(pattern, &ty, "let");
                Ty::Void
            }
            Statement::For {
                pattern,
                iter,
                body,
            } => {
                let t = self.expr(iter);
                let item = self.iter_item(&t, iter.span);
                self.scopes.push(HashMap::new());
                match &pattern.kind {
                    PatternKind::Binding(var) => {
                        self.define(var.name.clone(), item, BindingKind::Loop)
                    }
                    _ => self.irrefutable(pattern, &item, "for"),
                }
                self.loop_depth += 1;
                self.stmt(body);
                self.loop_depth -= 1;
                self.scopes.pop();
                Ty::Void
            }
            Statement::Defer(body, span) => {
                // A loop around the `defer` isn't one its code can leave.
                let loops = std::mem::replace(&mut self.loop_depth, 0);
                let outer = self.in_defer.replace(*span);
                self.stmt(body);
                self.loop_depth = loops;
                self.in_defer = outer;
                Ty::Void
            }
            Statement::Break(span) | Statement::Continue(span) => {
                let word = if matches!(stmt, Statement::Break(_)) {
                    "break"
                } else {
                    "continue"
                };
                if let (0, Some(at)) = (self.loop_depth, self.in_defer) {
                    self.error(self.leaves_defer(word, *span, at));
                } else if self.loop_depth == 0 {
                    let mut err = TypeError::new(format!("'{word}' outside a loop"), *span).help(
                        format!("'{word}' can only be used in a 'while' or 'for' loop"),
                    );
                    if self.fns.last().is_some_and(|f| f.name.is_none()) {
                        err = err.note(
                            "a closure's body is a function of its own, so it can't leave a loop around it",
                        );
                    }
                    self.error(err);
                }
                Ty::Never
            }
            Statement::Return(value) => {
                let (found, span) = match value {
                    Some(e) => (self.expr(e), e.span),
                    None => (
                        Ty::Void,
                        self.fns.last().map_or(Self::dummy_span(), |f| f.span),
                    ),
                };
                if let Some(at) = self.in_defer {
                    self.error(self.leaves_defer("return", span, at));
                }
                self.check_return(&found, span);
                Ty::Never
            }
        }
    }

    /// The error for `what` (`return`, `?`, `break`, or `continue`) at
    /// `span`, in code deferred at `at`, which it would leave.
    fn leaves_defer(&self, what: &str, span: Span, at: Span) -> TypeError {
        TypeError::new(format!("'{what}' can't leave deferred code"), span)
            .help("deferred code runs while its block is being left, so it must run to its end; handle the case inside it")
            .note(format!(
                "the 'defer' is at {}:{}",
                at.start.line, at.start.col
            ))
    }

    /// `list[index]`
    fn index(&mut self, list: &Expr, index: &Expr) -> Ty {
        let t = self.expr(list);
        let i = self.expr(index);
        self.expect(&Ty::Int, &i, index.span, |_, f| {
            format!("mismatched types: a list's index is an int, found {f}")
        });
        match self.resolve(&t) {
            Ty::Named(n, args) if n == "List" => args[0].clone(),
            Ty::Error | Ty::Never => Ty::Error,
            Ty::Var(_) => {
                self.error(
                    TypeError::new(
                        "type annotations needed: the type of this value must be known to index it",
                        list.span,
                    )
                    .help("annotate the variable or parameter it comes from, e.g. `xs: List<int>`"),
                );
                self.poison(&[&t]);
                Ty::Error
            }
            other => {
                let shown = self.show(&other);
                self.error(
                    TypeError::new(format!("{shown} can't be indexed"), list.span)
                        .help("only lists can be indexed, as in `xs[0]`"),
                );
                Ty::Error
            }
        }
    }

    fn condition(&mut self, cond: &Expr, keyword: &str) {
        let t = self.expr(cond);
        self.expect(&Ty::Bool, &t, cond.span, |_, f| {
            format!("'{keyword}' condition must be a bool, found {f}")
        });
    }

    // ---- Expressions -------------------------------------------------------

    /// Check `expr`, whose type should be `expected`. Only a closure
    /// literal makes use of it, for its parameters' types; the caller
    /// still checks that the types match.
    fn expr_expecting(&mut self, expr: &Expr, expected: &Ty) -> Ty {
        if expr.is_closure() {
            self.closure_hint = Some(expected.clone());
        }
        self.expr(expr)
    }

    fn expr(&mut self, expr: &Expr) -> Ty {
        let span = expr.span;
        match &expr.kind {
            Expression::Int(_) => Ty::Int,
            Expression::Float(_) => Ty::Float,
            Expression::String(_) => Ty::Str,
            Expression::True | Expression::False => Ty::Bool,
            Expression::Void => Ty::Void,
            Expression::Grouping(e) => self.expr(e),
            Expression::Unary(op, e) => {
                let t = self.expr(e);
                match op {
                    Op1::Not => {
                        self.expect(&Ty::Bool, &t, span, |_, f| {
                            format!("cannot apply '!' to {f}; it expects a bool")
                        });
                        Ty::Bool
                    }
                    Op1::Negate => {
                        if !self.constrain(&t, Class::NUM) {
                            let f = self.show(&t);
                            self.error(TypeError::new(
                                format!("cannot apply '-' to {f}; it expects an int or float"),
                                span,
                            ));
                            self.poison(&[&t]);
                            return Ty::Error;
                        }
                        self.operands.push((key(span), t.clone()));
                        t
                    }
                }
            }
            Expression::BinOp(l, op, r) => self.binop(l, op, r, span),
            Expression::If(cond, then, otherwise) => {
                self.condition(cond, "if");
                let t1 = self.stmt(then);
                let t2 = self.stmt(otherwise);
                match (self.resolve(&t1), self.resolve(&t2)) {
                    (Ty::Never, _) => t2,
                    (_, Ty::Never) => t1,
                    _ => {
                        let at = Self::tail_span(otherwise).unwrap_or(span);
                        let same = self.expect(&t1, &t2, at, |e, f| {
                            format!("'if' and 'else' have incompatible types: {e} and {f}")
                        });
                        // Once reported, the mismatch mustn't be reported
                        // again by an `if` this one is the `else if` of.
                        if same {
                            t1
                        } else {
                            Ty::Error
                        }
                    }
                }
            }
            Expression::Var(name) => {
                self.note_use(name, span);
                match self.lookup(name) {
                    Some(Binding {
                        kind: BindingKind::Variadic,
                        ..
                    }) => {
                        self.error(
                            TypeError::new(format!("'{name}' can only be called directly"), span)
                                .help(format!(
                                    "wrap it in a closure, e.g. `fn(x: str) {{ {name}(x) }}`"
                                )),
                        );
                        Ty::Error
                    }
                    Some(b) if b.generics.is_empty() => b.ty.clone(),
                    Some(b) => {
                        let b = b.clone();
                        self.instantiate(&b.generics, &b.ty)
                    }
                    None => {
                        self.undefined(name, span);
                        Ty::Error
                    }
                }
            }
            Expression::Assign(name, value) => {
                let found = self.expr(value);
                self.note_use(name, span);
                match self.lookup(name).cloned() {
                    Some(Binding {
                        ty,
                        kind: BindingKind::Var,
                        ..
                    }) => {
                        self.expect(&ty, &found, value.span, |e, f| {
                            format!("mismatched types: cannot assign {f} to '{name}', which has type {e}")
                        });
                        ty
                    }
                    Some(Binding {
                        ty,
                        kind:
                            kind @ (BindingKind::Let
                            | BindingKind::Param
                            | BindingKind::Pattern
                            | BindingKind::Loop),
                        ..
                    }) => {
                        let (why, help) = match kind {
                            BindingKind::Let => (
                                "it was declared with 'let'",
                                format!(
                                    "declare it with 'var' to make it mutable: `var {name} = ...;`"
                                ),
                            ),
                            BindingKind::Param => (
                                "it is a parameter",
                                format!(
                                    "copy it into a mutable variable first: `var {name} = {name};`"
                                ),
                            ),
                            BindingKind::Loop => (
                                "it is a loop variable",
                                format!(
                                    "copy it into a mutable variable first: `var {name} = {name};`"
                                ),
                            ),
                            _ => (
                                "it is bound by a pattern",
                                format!(
                                    "copy it into a mutable variable first: `var {name} = {name};`"
                                ),
                            ),
                        };
                        self.error(
                            TypeError::new(
                                format!("cannot assign to '{name}' because {why}"),
                                span,
                            )
                            .help(help),
                        );
                        ty
                    }
                    Some(_) => {
                        self.error(TypeError::new(
                            format!("cannot assign to '{name}' because it is a function"),
                            span,
                        ));
                        Ty::Error
                    }
                    None => {
                        self.undefined(name, span);
                        Ty::Error
                    }
                }
            }
            Expression::Call(callee, args) => self.call(callee, args, span),
            Expression::Closure(params, rtype, body) => {
                let hinted = match self.closure_hint.take().map(|h| self.resolve(&h)) {
                    Some(Ty::Fn(ps, _)) if ps.len() == params.len() => Some(ps),
                    _ => None,
                };
                let param_tys: Vec<Ty> = params
                    .iter()
                    .enumerate()
                    .map(|(i, p)| match &p.t {
                        Some(t) => self.from_ast(t),
                        None => {
                            let t = self.fresh(Class::ANY);
                            if let Some(ps) = &hinted {
                                // If they don't fit, the mismatch is reported
                                // where the closure is passed.
                                self.unify(&t, &ps[i]);
                            }
                            self.pending.push(Pending {
                                name: p.name.clone(),
                                ty: t.clone(),
                                span: p.span,
                                what: format!("parameter '{}'", p.name),
                            });
                            t
                        }
                    })
                    .collect();
                let ret = match rtype {
                    Some(t) => self.from_ast(t),
                    None => self.fresh(Class::ANY),
                };
                let ctx = FnContext {
                    name: None,
                    ret: ret.clone(),
                    ret_declared: rtype.is_some(),
                    span,
                    try_reported: false,
                };
                self.open_closures.push((self.scopes.len(), Vec::new()));
                self.function(ctx, params, &param_tys, body);
                let (_, uses) = self.open_closures.pop().expect("this closure is open");
                self.captures.insert(key(span), uses);
                Ty::Fn(param_tys, Box::new(ret))
            }
            Expression::Field(obj, name, name_span) => self.field(obj, name, *name_span, span),
            Expression::List(items) => {
                let item = self.fresh(Class::ANY);
                for x in items {
                    let t = self.expr(x);
                    self.expect(&item, &t, x.span, |e, f| {
                        format!("mismatched types: this list's values are {e}, but this one is {f}")
                    });
                }
                Ty::Named("List".into(), vec![item])
            }
            Expression::Tuple(items) => Ty::Tuple(items.iter().map(|x| self.expr(x)).collect()),
            Expression::Index(list, index) => self.index(list, index),
            Expression::Template(parts) => {
                for part in parts {
                    if let crate::ast::TemplatePart::Expr(e) = part {
                        self.template_part(e);
                    }
                }
                Ty::Str
            }
            Expression::Path(type_name, name) => match self.associated(type_name, name, span) {
                Some(t) => t,
                None => self.variant_value(type_name, name, span),
            },
            Expression::StructLit {
                name,
                name_span,
                fields,
                base,
            } => self.struct_lit(name, *name_span, fields, base.as_deref()),
            Expression::Match(scrutinee, arms) => self.match_(scrutinee, arms),
            Expression::Try(inner) => self.try_(inner, span),
            Expression::Error(inner) => {
                for e in inner {
                    self.expr(e);
                }
                Ty::Error
            }
        }
    }

    /// `inner?`: the value in an `Ok` or `Some`; an `Err` or `None` is
    /// returned from the enclosing function, which must return the same
    /// kind of value (with the same error type, for a `Result`).
    fn try_(&mut self, inner: &Expr, span: Span) -> Ty {
        if let Some(at) = self.in_defer {
            self.error(self.leaves_defer("?", span, at));
        }
        let t = self.expr(inner);
        let (on, value, error) = match self.resolve(&t) {
            Ty::Named(n, args) if n == "Option" => (TryOn::Option, args[0].clone(), None),
            Ty::Named(n, args) if n == "Result" => {
                (TryOn::Result, args[0].clone(), Some(args[1].clone()))
            }
            Ty::Error | Ty::Never => return Ty::Error,
            Ty::Var(_) => {
                self.error(
                    TypeError::new("type annotations needed: the type of this value must be known to use '?' on it", inner.span)
                        .help("annotate the variable or parameter it comes from, e.g. `r: Result<int, str>`"),
                );
                self.poison(&[&t]);
                return Ty::Error;
            }
            other => {
                let shown = self.show(&other);
                self.error(
                    TypeError::new(
                        format!(
                            "'?' can only be used on an Option or a Result, but this is {shown}"
                        ),
                        span,
                    )
                    .help("remove the '?'"),
                );
                return Ty::Error;
            }
        };
        self.tries.insert(key(span), on);

        let Some(ctx) = self.fns.last().cloned() else {
            self.error(TypeError::new(
                "'?' can only be used inside a function",
                span,
            ));
            return value;
        };
        let what = match &ctx.name {
            Some(name) => format!("function '{name}'"),
            None => "this closure".to_string(),
        };
        let kind = match on {
            TryOn::Option => "Option",
            TryOn::Result => "Result",
        };
        // What the function must return for the `?` to pass on what it got.
        let passes = match &error {
            None => Ty::Named("Option".into(), vec![self.fresh(Class::ANY)]),
            Some(e) => Ty::Named("Result".into(), vec![self.fresh(Class::ANY), e.clone()]),
        };
        let ret = self.resolve(&ctx.ret);
        match (&ret, &error) {
            // A closure whose return type is still unknown returns one.
            (Ty::Var(_), _) => {
                self.unify(&ret, &passes);
            }
            (Ty::Error | Ty::Never, _) => {}
            (Ty::Named(n, args), Some(e)) if n == "Result" => {
                let (found, returned) = (self.show(e), self.show(&args[1]));
                if !self.unify(e, &args[1]) {
                    self.error(
                        TypeError::new(
                            format!(
                                "mismatched types: this '?' passes on an error of type {found}, \
                                 but {what} returns errors of type {returned}"
                            ),
                            span,
                        )
                        .note("errors are not converted automatically; use `match` to turn one into the other"),
                    );
                }
            }
            (Ty::Named(n, _), None) if n == "Option" => {}
            _ if ctx.try_reported => {}
            _ => {
                self.fns.last_mut().unwrap().try_reported = true;
                let returns = self.show(&ret);
                let mut err = TypeError::new(
                    format!(
                        "'?' on {} {kind} can only be used in a function that returns {} {kind}, but {what} returns {returns}",
                        article(kind),
                        article(kind),
                    ),
                    span,
                );
                let other_kind = matches!(&ret, Ty::Named(n, _) if n == "Result" || n == "Option");
                err = match (&ctx.name, &error) {
                    (_, _) if other_kind => err.help("use `match` to handle it here"),
                    (Some(name), _) if name == "main" => err.help(format!(
                        "handle the {} with `match` instead",
                        if error.is_some() { "error" } else { "None" }
                    )),
                    (Some(_), _) if ctx.ret_declared || ret == Ty::Void => {
                        let suggested = match &error {
                            None => Ty::Named("Option".into(), vec![ret.clone()]),
                            Some(e) => Ty::Named("Result".into(), vec![ret.clone(), e.clone()]),
                        };
                        err.help(format!(
                            "change the return type to `{}`, or use `match`",
                            self.show_example(&suggested)
                        ))
                    }
                    _ => err.help("use `match` instead"),
                };
                if other_kind {
                    err = err.note(
                        "a None is not converted to an error, or an error to a None, automatically",
                    );
                }
                self.error(err);
            }
        }
        value
    }

    fn binop(&mut self, l: &Expr, op: &Op2, r: &Expr, span: Span) -> Ty {
        let t1 = self.expr(l);
        let t2 = self.expr(r);

        if matches!(op, Op2::And | Op2::Or) {
            for (t, side, at) in [(&t1, "left", l.span), (&t2, "right", r.span)] {
                self.expect(&Ty::Bool, t, at, |_, f| {
                    format!("'{op}' expects bool operands, but the {side} operand is {f}")
                });
            }
            return Ty::Bool;
        }

        let (class, result) = match op {
            Op2::Addition => (Class::ADD, None),
            Op2::Subtraction | Op2::Multiplication | Op2::Division | Op2::Modulo => {
                (Class::NUM, None)
            }
            Op2::LessThan | Op2::LessEqual | Op2::GreaterThan | Op2::GreaterEqual => {
                (Class::CMP, Some(Ty::Bool))
            }
            Op2::Equal | Op2::NotEqual => (Class::EQ, Some(Ty::Bool)),
            Op2::And | Op2::Or => unreachable!(),
        };

        let (s1, s2) = (self.show(&t1), self.show(&t2));
        let ok = self.unify(&t1, &t2) && self.constrain(&t1, class);
        if !ok {
            let mut err = TypeError::new(format!("cannot apply '{op}' to {s1} and {s2}"), span);
            err = Self::with_conversion_note(err, &s1, &s2);
            let param = [&t1, &t2].into_iter().find_map(|t| match self.zonk(t) {
                Ty::Param(p) => Some(p),
                _ => None,
            });
            if let Some(p) = param {
                let bound = match class {
                    Class::EQ => Some("Eq"),
                    Class::CMP => Some("Ord"),
                    _ => None,
                };
                err = match bound {
                    Some(b) => err
                        .help(format!("add a bound where '{p}' is declared: `{p}: {b}`"))
                        .note(format!(
                            "'{p}' is a type parameter, which could be any type, so its values \
                             can only be compared if its bounds say they can"
                        )),
                    None => err.note(format!(
                        "'{p}' is a type parameter, which could be any type, so its values \
                         can't be used with arithmetic operators"
                    )),
                };
            } else if s1 == s2 && class == Class::EQ {
                err = err.note(
                    "only int, float, bool, and str values, and structs, enums, and tuples made of them, \
                     can be compared with '==' and '!='",
                );
            } else if s1 == s2 && class == Class::CMP {
                let t = self.zonk(&t1);
                if matches!(t, Ty::Named(..) | Ty::Tuple(_)) {
                    err = err.note(self.unmet_note(&t, Class::ORD));
                }
                err = err.note(
                    "'<', '<=', '>', and '>=' work on int and float, and on Ord types: bool, str, \
                     and structs, enums, tuples, and lists made of them (without float)",
                );
            }
            self.error(err);
            self.poison(&[&t1, &t2]);
            return result.unwrap_or(Ty::Error);
        }
        self.operands.push((key(span), t1.clone()));
        result.unwrap_or(t1)
    }

    fn call(&mut self, callee: &Expr, args: &[Expr], span: Span) -> Ty {
        if let Expression::Path(enum_name, variant) = &callee.kind {
            // `Point::origin()` calls a function; anything else is a variant.
            if self.method(enum_name, variant).is_none() {
                return self.construct_variant(enum_name, variant, args, callee.span, span);
            }
        }
        let name = match &callee.kind {
            Expression::Var(n) => Some(n.clone()),
            _ => None,
        };

        if let Some(n) = &name {
            if matches!(
                self.lookup(n),
                Some(Binding {
                    kind: BindingKind::Variadic,
                    ..
                })
            ) {
                // Variadic built-ins take any number of arguments of any type.
                for a in args {
                    self.expr(a);
                }
                return Ty::Void;
            }
        }

        let describe = match (&name, &callee.kind) {
            (Some(n), _) => format!("function '{n}'"),
            (None, Expression::Path(t, f)) => format!("function '{t}::{f}'"),
            _ => "this function".to_string(),
        };
        let callee_ty = match &callee.kind {
            Expression::Field(obj, field, field_span) => {
                match self.method_call(obj, field, *field_span, callee, args, span) {
                    Ok(t) => return t,
                    // A field holding a function, called like any other.
                    Err(t) => t,
                }
            }
            _ => self.expr(callee),
        };
        let callee_ty = self.resolve(&callee_ty);
        let (params, ret) = match &callee_ty {
            Ty::Fn(params, ret) => (params.clone(), (**ret).clone()),
            Ty::Var(_) => {
                // Calling something of unknown type tells us it's a function.
                let params: Vec<Ty> = args.iter().map(|_| self.fresh(Class::ANY)).collect();
                let ret = self.fresh(Class::ANY);
                let f = Ty::Fn(params.clone(), Box::new(ret.clone()));
                if !self.unify(&callee_ty, &f) {
                    self.not_callable(&callee_ty, &name, callee.span, args);
                    return Ty::Error;
                }
                (params, ret)
            }
            Ty::Error | Ty::Never => {
                for a in args {
                    self.expr(a);
                }
                return Ty::Error;
            }
            _ => {
                self.not_callable(&callee_ty, &name, callee.span, args);
                return Ty::Error;
            }
        };

        if params.len() != args.len() {
            self.error(TypeError::new(
                format!(
                    "{describe} takes {} but {} {} given",
                    plural(params.len(), "argument"),
                    args.len(),
                    if args.len() == 1 { "was" } else { "were" },
                ),
                span,
            ));
            for a in args {
                self.expr(a);
            }
            return ret;
        }

        // Name the parameters when calling a top-level function directly.
        let param_names = name
            .as_ref()
            .filter(|n| matches!(self.lookup(n), Some(b) if b.kind == BindingKind::Function))
            .and_then(|n| self.param_names.get(n).cloned());

        for (i, (arg, param)) in args.iter().zip(&params).enumerate() {
            let found = self.expr_expecting(arg, param);
            let which = match &param_names {
                Some(names) => format!("parameter '{}'", names[i]),
                None => format!("argument {}", i + 1),
            };
            self.expect(param, &found, arg.span, |e, f| {
                format!("mismatched types: {which} of {describe} expects {e}, found {f}")
            });
        }
        ret
    }

    fn not_callable(&mut self, t: &Ty, name: &Option<Id>, span: Span, args: &[Expr]) {
        let t = self.show(t);
        let what = match name {
            Some(n) => format!(" ('{n}' is not a function)"),
            None => String::new(),
        };
        self.error(TypeError::new(
            format!("cannot call a value of type {t}{what}"),
            span,
        ));
        for a in args {
            self.expr(a);
        }
    }

    fn undefined(&mut self, name: &Id, span: Span) {
        let names: Vec<Id> = self.scopes.iter().flat_map(|s| s.keys().cloned()).collect();
        let help = match (
            self.type_used_as_value(name),
            closest_match(name, names.iter().map(|n| n.as_str())),
        ) {
            (Some(help), _) => help,
            (None, Some(s)) => format!("a variable with a similar name exists: '{s}'"),
            (None, None) => format!("declare it first with `let {name} = ...;`"),
        };
        self.error(
            TypeError::new(format!("cannot find variable '{name}' in this scope"), span).help(help),
        );
    }

    // ---- Scopes ------------------------------------------------------------

    fn define(&mut self, name: Id, ty: Ty, kind: BindingKind) {
        self.scopes.last_mut().unwrap().insert(
            name,
            Binding {
                ty,
                kind,
                generics: Vec::new(),
                reaches_var: false,
            },
        );
    }

    fn lookup(&self, name: &Id) -> Option<&Binding> {
        self.scopes.iter().rev().find_map(|s| s.get(name))
    }

    /// Note that `name` is used at `span`, for each closure being checked
    /// that it comes from outside of.
    fn note_use(&mut self, name: &Id, span: Span) {
        if self.open_closures.is_empty() {
            return;
        }
        let Some(at) = self.scopes.iter().rposition(|s| s.contains_key(name)) else {
            return;
        };
        let b = &self.scopes[at][name];
        if matches!(b.kind, BindingKind::Function | BindingKind::Variadic) {
            return;
        }
        let capture = Capture {
            name: name.clone(),
            span,
            kind: b.kind,
            reaches_var: b.reaches_var,
        };
        for (depth, uses) in &mut self.open_closures {
            if at < *depth {
                uses.push(capture.clone());
            }
        }
    }

    /// Where, if anywhere, the value of `e` plainly reaches a `var` of the
    /// function it is in, how to say so, and the `var`'s name if it is
    /// used directly: a closure that uses one, or a list, tuple, struct, or
    /// variant holding one, or a `let` bound to one.
    fn reaches_var(&self, e: &Expr) -> Option<(Span, String, Option<Id>)> {
        let holds = |name: &Id| format!("'{name}' holds a function that uses a `var`");
        match &e.kind {
            Expression::Closure(..) => self.captures.get(&key(e.span))?.iter().find_map(|c| {
                if c.kind == BindingKind::Var {
                    Some((
                        c.span,
                        format!("'{}' is a `var`", c.name),
                        Some(c.name.clone()),
                    ))
                } else if c.reaches_var {
                    Some((c.span, holds(&c.name), None))
                } else {
                    None
                }
            }),
            Expression::Var(name) => match self.lookup(name) {
                Some(b) if b.reaches_var => Some((e.span, holds(name), None)),
                _ => None,
            },
            Expression::Grouping(x) => self.reaches_var(x),
            Expression::List(items) | Expression::Tuple(items) => {
                items.iter().find_map(|x| self.reaches_var(x))
            }
            Expression::StructLit { fields, base, .. } => fields
                .iter()
                .map(|f| &f.value)
                .chain(base.as_deref())
                .find_map(|x| self.reaches_var(x)),
            // A variant that holds values, such as `Some(f)`.
            Expression::Call(callee, args) if matches!(callee.kind, Expression::Path(..)) => {
                args.iter().find_map(|x| self.reaches_var(x))
            }
            _ => None,
        }
    }

    /// Report `arg`, the function given to `Scope::spawn`, if it plainly
    /// uses a `var` of the task spawning it.
    pub(super) fn check_spawned(&mut self, arg: &Expr) {
        if let Some((span, why, var)) = self.reaches_var(arg) {
            let help = match var {
                Some(n) => format!(
                    "give the task a copy made with `let` before spawning it (`let {n} = {n};`), and get what it works out back from `join`"
                ),
                None => "have the task work out what it needs itself, and get what it works out back from `join`".to_string(),
            };
            self.error(
                TypeError::new(
                    format!("a task can't use a `var` of the task that spawns it: {why}"),
                    span,
                )
                .help(help)
                .note("a `var` belongs to one task, so that no two tasks can change it at once"),
            );
        }
    }

    // ---- Unification -------------------------------------------------------

    fn fresh(&mut self, class: Class) -> Ty {
        self.vars.push((None, class));
        Ty::Var(self.vars.len() - 1)
    }

    /// Follow solved variables until reaching a type that isn't one.
    fn resolve(&self, t: &Ty) -> Ty {
        let mut t = t.clone();
        while let Ty::Var(v) = t {
            match &self.vars[v].0 {
                Some(next) => t = next.clone(),
                None => break,
            }
        }
        t
    }

    /// Substitute every solved variable inside `t`.
    fn zonk(&self, t: &Ty) -> Ty {
        match self.resolve(t) {
            Ty::Fn(params, ret) => Ty::Fn(
                params.iter().map(|p| self.zonk(p)).collect(),
                Box::new(self.zonk(&ret)),
            ),
            Ty::Named(name, args) => Ty::Named(name, args.iter().map(|a| self.zonk(a)).collect()),
            Ty::Tuple(items) => Ty::Tuple(items.iter().map(|t| self.zonk(t)).collect()),
            t => t,
        }
    }

    fn free_vars(t: &Ty) -> Vec<usize> {
        match t {
            Ty::Var(v) => vec![*v],
            Ty::Fn(params, ret) => params
                .iter()
                .chain(std::iter::once(&**ret))
                .flat_map(Self::free_vars)
                .collect(),
            Ty::Named(_, args) | Ty::Tuple(args) => args.iter().flat_map(Self::free_vars).collect(),
            _ => vec![],
        }
    }

    /// `t` with each type parameter in `map` replaced by its type.
    fn subst(t: &Ty, map: &HashMap<Id, Ty>) -> Ty {
        match t {
            Ty::Param(p) => map.get(p).cloned().unwrap_or_else(|| t.clone()),
            Ty::Fn(params, ret) => Ty::Fn(
                params.iter().map(|p| Self::subst(p, map)).collect(),
                Box::new(Self::subst(ret, map)),
            ),
            Ty::Named(name, args) => Ty::Named(
                name.clone(),
                args.iter().map(|a| Self::subst(a, map)).collect(),
            ),
            Ty::Tuple(items) => Ty::Tuple(items.iter().map(|t| Self::subst(t, map)).collect()),
            t => t.clone(),
        }
    }

    /// A fresh type variable for each of the type parameters `params`,
    /// required to meet the parameter's bounds.
    fn fresh_args(&mut self, params: &[Generic]) -> (Vec<Ty>, HashMap<Id, Ty>) {
        let args: Vec<Ty> = params.iter().map(|p| self.fresh(p.bounds)).collect();
        let map = params
            .iter()
            .map(|p| p.name.clone())
            .zip(args.iter().cloned())
            .collect();
        (args, map)
    }

    /// The type `t` of a generic function, with its type parameters
    /// `generics` replaced by fresh variables for one use of it.
    fn instantiate(&mut self, generics: &[Generic], t: &Ty) -> Ty {
        let (_, map) = self.fresh_args(generics);
        Self::subst(t, &map)
    }

    /// The type parameters a type mentions.
    fn params_in(t: &Ty, out: &mut HashSet<Id>) {
        match t {
            Ty::Param(p) => {
                out.insert(p.clone());
            }
            Ty::Fn(params, ret) => {
                for p in params {
                    Self::params_in(p, out);
                }
                Self::params_in(ret, out);
            }
            Ty::Named(_, args) | Ty::Tuple(args) => {
                for a in args {
                    Self::params_in(a, out);
                }
            }
            _ => {}
        }
    }

    /// Make `a` and `b` the same type, solving variables as needed. Returns
    /// false if they can't be.
    fn unify(&mut self, a: &Ty, b: &Ty) -> bool {
        let (a, b) = (self.resolve(a), self.resolve(b));
        match (&a, &b) {
            (Ty::Never, _) | (_, Ty::Never) => true,
            (Ty::Var(x), Ty::Var(y)) if x == y => true,
            (Ty::Var(x), _) => self.bind(*x, &b),
            (_, Ty::Var(y)) => self.bind(*y, &a),
            (Ty::Error, _) | (_, Ty::Error) => true,
            (Ty::Fn(p1, r1), Ty::Fn(p2, r2)) => {
                p1.len() == p2.len()
                    && p1.iter().zip(p2).all(|(x, y)| self.unify(x, y))
                    && self.unify(r1, r2)
            }
            (Ty::Named(n1, a1), Ty::Named(n2, a2)) => {
                n1 == n2 && a1.len() == a2.len() && a1.iter().zip(a2).all(|(x, y)| self.unify(x, y))
            }
            (Ty::Tuple(t1), Ty::Tuple(t2)) => {
                t1.len() == t2.len() && t1.iter().zip(t2).all(|(x, y)| self.unify(x, y))
            }
            _ => a == b,
        }
    }

    /// Solve variable `v` (which is unsolved) to `t` (already resolved).
    fn bind(&mut self, v: usize, t: &Ty) -> bool {
        let class = self.vars[v].1;
        if let Ty::Var(w) = t {
            self.vars[*w].1 = self.vars[*w].1.meet(class);
        } else if !self.admits(class, t) {
            self.unmet = Some((t.clone(), class));
            return false;
        } else if Self::free_vars(&self.zonk(t)).contains(&v) {
            // `t` contains `v` itself, e.g. `f(f)`: no finite type fits.
            self.infinite = true;
            return false;
        }
        self.vars[v].0 = Some(t.clone());
        true
    }

    /// Require that `t` belongs to `class`.
    fn constrain(&mut self, t: &Ty, class: Class) -> bool {
        match self.resolve(t) {
            Ty::Var(v) => {
                self.vars[v].1 = self.vars[v].1.meet(class);
                true
            }
            t => self.admits(class, &t),
        }
    }

    /// Whether the concrete type `t` belongs to `class`. A struct or enum
    /// has a trait if its definition allows it and its type arguments have
    /// it too, so this may restrict variables among the arguments. A type
    /// parameter has the traits its bounds name.
    fn admits(&mut self, class: Class, t: &Ty) -> bool {
        // Only int and float can be compared without being `Ord`.
        let class = match t {
            Ty::Named(..) | Ty::Tuple(_) | Ty::Param(_) if class.has(Class::CMP) => {
                class.minus(Class::CMP).meet(Class::ORD.implied())
            }
            _ => class,
        };
        match t {
            _ if class == Class::ANY => true,
            Ty::Named(n, args) => {
                class == class.traits()
                    && self.has_traits(n, class, &mut HashSet::new())
                    && args.clone().iter().all(|a| self.constrain(a, class))
            }
            // A tuple has a trait if each of its types does.
            Ty::Tuple(items) => {
                class == class.traits() && items.clone().iter().all(|t| self.constrain(t, class))
            }
            Ty::Param(p) => self.bounds_of(p).has(class),
            _ => class.admits(t),
        }
    }

    /// The bounds of the type parameter `name`, which is in scope.
    pub(super) fn bounds_of(&self, name: &Id) -> Class {
        self.type_params
            .iter()
            .find(|g| g.name == *name)
            .map_or(Class::ANY, |g| g.bounds)
    }

    /// A note on why `t` doesn't have the traits in `needed`.
    pub(super) fn unmet_note(&self, t: &Ty, needed: Class) -> String {
        let needed = needed.traits();
        match self.zonk(t) {
            Ty::Float => format!(
                "float isn't {}, because NaN isn't equal to itself; \
                 convert floats to something that is first",
                needed.minus(Class::EQ).show()
            ),
            Ty::Fn(..) => format!("functions aren't {}: they can't be compared", needed.show()),
            Ty::Param(p) => {
                let missing = needed.minus(self.bounds_of(&p)).show();
                format!("'{p}' is a type parameter that could be any type, unless a bound like `{p}: {missing}` says otherwise")
            }
            t => {
                let base = format!("{} isn't {}", self.show(&t), needed.show());
                match &t {
                    Ty::Named(n, _) => match self.lacking_field(n, needed) {
                        Some(why) => format!("{base}, because {why}"),
                        None => base,
                    },
                    _ => base,
                }
            }
        }
    }

    /// Require `found` to be `expected`, reporting `message(expected, found)`
    /// at `span` if it isn't.
    fn expect(
        &mut self,
        expected: &Ty,
        found: &Ty,
        span: Span,
        message: impl FnOnce(&str, &str) -> String,
    ) -> bool {
        let (e, f) = (self.show(expected), self.show(found));
        self.infinite = false;
        self.unmet = None;
        if self.unify(expected, found) {
            return true;
        }
        let mut err = Self::with_conversion_note(TypeError::new(message(&e, &f), span), &e, &f);
        if let Some((t, class)) = self.unmet.take() {
            if class.traits() != Class::ANY {
                err = err.note(self.unmet_note(&t, class));
            }
        } else {
            err = self.with_param_note(err, expected, found);
        }
        if self.infinite {
            err = err.note("this would need an infinite type, such as a function that takes itself as an argument");
        }
        self.error(err);
        self.poison(&[expected, found]);
        false
    }

    /// After reporting an error about `types`, treat their unknown parts as
    /// errors too, so they aren't also reported as needing annotations.
    fn poison(&mut self, types: &[&Ty]) {
        for t in types {
            for v in Self::free_vars(&self.zonk(t)) {
                self.vars[v].0 = Some(Ty::Error);
            }
        }
    }

    /// Explain a mismatch involving a type parameter, which only matches itself.
    fn with_param_note(&self, err: TypeError, a: &Ty, b: &Ty) -> TypeError {
        match (self.zonk(a), self.zonk(b)) {
            (Ty::Param(p), Ty::Param(q)) if p != q => err.note(format!(
                "'{p}' and '{q}' are different type parameters, so they may be different types"
            )),
            (Ty::Param(p), other) | (other, Ty::Param(p))
                if !matches!(other, Ty::Param(_) | Ty::Var(_)) =>
            {
                err.note(format!(
                    "'{p}' is a type parameter: it stands for whatever type the caller chooses, \
                     which might not be {}",
                    self.show(&other)
                ))
            }
            _ => err,
        }
    }

    fn with_conversion_note(err: TypeError, a: &str, b: &str) -> TypeError {
        if matches!((a, b), ("int", "float") | ("float", "int")) {
            err.note("int and float are not converted automatically")
        } else {
            err
        }
    }

    // ---- Conversions and display ---------------------------------------------

    fn from_ast(&mut self, t: &Type) -> Ty {
        match t {
            Type::Int => Ty::Int,
            Type::Float => Ty::Float,
            Type::Bool => Ty::Bool,
            Type::String => Ty::Str,
            Type::Void => Ty::Void,
            Type::Fn {
                param_types,
                ret_type,
            } => Ty::Fn(
                param_types.iter().map(|t| self.from_ast(t)).collect(),
                Box::new(self.from_ast(ret_type)),
            ),
            Type::Tuple(items) => Ty::Tuple(items.iter().map(|t| self.from_ast(t)).collect()),
            Type::Named(name, args, span) => self.named_type(name, args, *span),
            Type::Error => Ty::Error,
        }
    }

    /// A type as written in Aja source; unknown parts show as `_`, or
    /// `{number}` when only int or float would fit (`{number or str}` when
    /// a str would too).
    pub fn show(&self, t: &Ty) -> String {
        match self.zonk(t) {
            Ty::Int => "int".into(),
            Ty::Float => "float".into(),
            Ty::Bool => "bool".into(),
            Ty::Str => "str".into(),
            Ty::Void => "void".into(),
            Ty::Never => "never".into(),
            Ty::Error => "{unknown}".into(),
            Ty::Named(name, args) if args.is_empty() => name.to_string(),
            Ty::Named(name, args) => {
                let args: Vec<String> = args.iter().map(|a| self.show(a)).collect();
                format!("{name}<{}>", args.join(", "))
            }
            Ty::Param(name) => name.to_string(),
            Ty::Var(v) => match self.vars[v].1 {
                c if c.has(Class::NUM) => "{number}".into(),
                c if c.has(Class::ADD) => "{number or str}".into(),
                c if c.traits() != Class::ANY => format!("{{{}}}", c.show()),
                _ => "_".into(),
            },
            Ty::Fn(params, ret) => {
                let params: Vec<String> = params.iter().map(|p| self.show(p)).collect();
                format!("({}) -> {}", params.join(", "), self.show(&ret))
            }
            Ty::Tuple(items) => {
                let items: Vec<String> = items.iter().map(|t| self.show(t)).collect();
                format!("({})", items.join(", "))
            }
        }
    }

    /// A complete type to suggest as an annotation for `t`, with `int` for
    /// its unknown parts: `Option<int>` for `Option<_>`.
    fn show_example(&self, t: &Ty) -> String {
        let mut t = self.zonk(t);
        fn fill(t: &mut Ty) {
            match t {
                Ty::Var(_) | Ty::Error => *t = Ty::Int,
                Ty::Fn(params, ret) => {
                    params.iter_mut().for_each(fill);
                    fill(ret);
                }
                Ty::Named(_, args) | Ty::Tuple(args) => args.iter_mut().for_each(fill),
                _ => {}
            }
        }
        fill(&mut t);
        self.show(&t)
    }

    /// The span of a block's final expression, if it has one.
    fn tail_span(stmt: &Statement) -> Option<Span> {
        match stmt {
            Statement::Block(_, Some(tail)) => Some(tail.span),
            _ => None,
        }
    }

    fn dummy_span() -> Span {
        let loc = crate::ast::Location::new(0, 1, 1);
        Span::new(loc, loc)
    }

    // ---- REPL ----------------------------------------------------------------

    /// Check one REPL input. Top-level `let`s stay in scope for later inputs;
    /// an input with errors leaves the scope as it was.
    pub fn check_repl_stmt(&mut self, stmt: &Statement) -> Result<Types, Vec<TypeError>> {
        let saved = self.scopes[0].clone();
        if let Statement::Defer(_, span) = stmt {
            self.error(TypeError::new("'defer' outside a block", *span).help(
                "put it in a block, `{ defer ...; ... }`, which runs it when the block is left",
            ));
        }
        self.stmt(stmt);
        self.report_unresolved();
        let result = self.finish();
        if result.is_err() {
            self.scopes.truncate(1);
            self.scopes[0] = saved;
        }
        result
    }
}

/// "an" before "Option", "a" before "Result".
fn article(word: &str) -> &'static str {
    if word.starts_with(['A', 'E', 'I', 'O', 'U']) {
        "an"
    } else {
        "a"
    }
}

fn plural(n: usize, word: &str) -> String {
    if n == 1 {
        format!("1 {word}")
    } else {
        format!("{n} {word}s")
    }
}
