use std::{
    error::Error,
    fmt::Display,
    sync::{Arc, Mutex},
};

use crate::{
    builtins::BUILTINS,
    ast::{types::Type, Definition, Expr, Expression, Id, Op1, Op2, Program, Span, Statement, Variable},
    diagnostic::{closest_match, Diagnostic},
};
use color_eyre::Result;
use env::RuntimeEnv;
use value::{Function, Value};

mod env;
mod native;
pub mod value;

#[derive(Debug, Clone)]
pub enum ErrorKind {
    Adhoc(String),
    UndefinedVariable { var: Id, suggestion: Option<Id> },
    TypeError { msg: String },
    /// Not a real error: unwinds a `return` to the enclosing call, carrying
    /// the returned value and the span of the returned expression.
    Return(Value, Option<Span>),
}

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub kind: ErrorKind,
    /// Where the error happened, when known.
    pub span: Option<Span>,
    /// Function calls the error unwound through, innermost first:
    /// (description of the function, where it was called).
    pub trace: Vec<(String, Span)>,
}

impl RuntimeError {
    fn new(kind: ErrorKind) -> Self {
        RuntimeError {
            kind,
            span: None,
            trace: Vec::new(),
        }
    }

    pub fn adhoc(msg: impl Into<String>) -> Self {
        Self::new(ErrorKind::Adhoc(msg.into()))
    }

    pub fn type_error(msg: impl Into<String>) -> Self {
        Self::new(ErrorKind::TypeError { msg: msg.into() })
    }

    pub fn undefined(var: Id) -> Self {
        Self::new(ErrorKind::UndefinedVariable {
            var,
            suggestion: None,
        })
    }

    fn ret(value: Value, span: Option<Span>) -> Self {
        Self::new(ErrorKind::Return(value, span))
    }

    /// Attach `span`, unless the error already has a more precise location.
    pub fn at(mut self, span: Span) -> Self {
        if self.span.is_none() && !matches!(self.kind, ErrorKind::Return(..)) {
            self.span = Some(span);
        }
        self
    }

    /// Record that this error unwound out of a call to `function` at `call_site`.
    fn called_from(mut self, function: String, call_site: Span) -> Self {
        if self.span.is_some() && !matches!(self.kind, ErrorKind::Return(..)) {
            self.trace.push((function, call_site));
        }
        self
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        let help = match &self.kind {
            ErrorKind::UndefinedVariable {
                suggestion: Some(s),
                ..
            } => Some(format!("a variable with a similar name exists: '{s}'")),
            ErrorKind::UndefinedVariable { var, .. } => {
                Some(format!("declare it first with `let {var} = ...;`"))
            }
            _ => None,
        };
        // Collapse runs of identical frames (e.g. deep recursion) into one note.
        let mut frames: Vec<(&str, Span, usize)> = Vec::new();
        for (function, site) in &self.trace {
            match frames.last_mut() {
                Some((f, s, count)) if *f == function.as_str() && *s == *site => *count += 1,
                _ => frames.push((function, *site, 1)),
            }
        }
        const MAX_FRAMES: usize = 8;
        let mut notes: Vec<String> = frames
            .iter()
            .take(MAX_FRAMES)
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
        if frames.len() > MAX_FRAMES {
            notes.push(format!("... and {} more calls", frames.len() - MAX_FRAMES));
        }

        let d = Diagnostic::new(self.to_string())
            .with_help(help)
            .with_notes(notes);
        match self.span {
            Some(span) => d.with_span(span.start, span.end),
            None => d,
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ErrorKind::UndefinedVariable { var, .. } => {
                write!(f, "cannot find variable '{var}' in this scope")
            }
            ErrorKind::TypeError { msg } => write!(f, "{msg}"),
            ErrorKind::Return(..) => write!(f, "'return' used outside of a function"),
            ErrorKind::Adhoc(msg) => write!(f, "{msg}"),
        }
    }
}

impl Error for RuntimeError {}

type Answer = Result<Value, RuntimeError>;

pub struct Interpreter {
    pub program: Program,
    pub env: Arc<Mutex<RuntimeEnv>>,
}

impl Interpreter {
    pub fn new(prog: Program) -> Self {
        Interpreter {
            program: prog,
            env: Arc::new(Mutex::new(RuntimeEnv::new())),
        }
    }

    /// Define builtins and top-level functions, then run `main`.
    pub fn run(mut self) -> Answer {
        self.defines()?;
        let main = self.env.lock().unwrap().values.get("main").cloned();
        let Some(Value::Fn(Function { body, ret_type, .. })) = main else {
            return Err(RuntimeError::adhoc(
                "no 'main' function found; add `fn main() { ... }` as the entry point",
            ));
        };

        let (res, ret_span) = self.run_body(body)?;
        Self::check_return(&res, ret_type.as_ref(), Some(&"main".into()), ret_span)?;
        Ok(res)
    }

    /// Run a function body, catching `return`. Also yields the span of the
    /// expression that produced the value (the tail or `return` expression).
    fn run_body(&mut self, body: Statement) -> Result<(Value, Option<Span>), RuntimeError> {
        let tail_span = match &body {
            Statement::Block(_, Some(tail)) => Some(tail.span),
            _ => None,
        };
        match self.interp_stmt(body) {
            Ok(v) => Ok((v, tail_span)),
            Err(RuntimeError {
                kind: ErrorKind::Return(v, span),
                ..
            }) => Ok((v, span)),
            Err(err) => Err(err),
        }
    }

    /// The interpreter's implementation of a built-in. Every built-in must
    /// have one; `native::tests` checks this too.
    fn native(name: &str) -> native::NativeFn {
        native::implementation(name)
            .unwrap_or_else(|| panic!("built-in '{name}' has no implementation in the interpreter"))
    }

    pub fn defines(&mut self) -> Result<(), RuntimeError> {
        for builtin in BUILTINS {
            self.env
                .lock()
                .unwrap()
                .define(builtin.name.into(), Value::NativeFn(builtin, Self::native(builtin.name)))?;
        }

        for def in &self.program.definitions {
            match def {
                Definition::Function {
                    name,
                    params,
                    body,
                    rtype,
                    ..
                } => {
                    let existing = self.env.lock().unwrap().values.get(name).cloned();
                    match existing {
                        Some(Value::NativeFn(..)) => {
                            return Err(RuntimeError::adhoc(format!(
                                "function '{name}' conflicts with the built-in function of the same name"
                            )))
                        }
                        Some(_) => {
                            return Err(RuntimeError::adhoc(format!(
                                "function '{name}' is defined more than once"
                            )))
                        }
                        None => {}
                    }
                    self.env.lock().unwrap().define(
                        name.clone(),
                        Value::Fn(Function {
                            params: params.to_vec(),
                            body: body.clone(),
                            ret_type: rtype.clone(),
                            env: self.env.clone(),
                        }),
                    )?;
                }
                Definition::Struct { name, .. } => {
                    return Err(RuntimeError::adhoc(format!(
                        "struct '{name}': structs are not supported yet"
                    )));
                }
            }
        }

        Ok(())
    }

    pub fn interp_stmt(&mut self, stmt: Statement) -> Answer {
        match stmt {
            Statement::Block(stmts, tail) => {
                let prev = self.env.clone();
                self.env = Arc::new(Mutex::new(RuntimeEnv::from_enclosing(prev.clone())));
                let result = (|| {
                    for stmt in stmts {
                        self.interp_stmt(stmt)?;
                    }
                    match tail {
                        Some(tail) => self.interp_expr(tail),
                        None => Ok(Value::Void),
                    }
                })();
                // Leave the block's scope even when it exits early with an error or `return`.
                self.env = prev;
                result
            }
            Statement::Expr(e) => self.interp_expr(e),
            Statement::Let { var, value } => self.interp_let(var, value),
            Statement::If(e1, s1) => self.interp_if(e1, *s1, None),
            Statement::While(e, s) => self.interp_while(e, *s),
            Statement::Return(maybe_e) => self.interp_return(maybe_e),
        }
    }

    pub fn interp_expr(&mut self, expr: Expr) -> Answer {
        let span = expr.span;
        let result = match expr.kind {
            Expression::Int(i) => Ok(Value::Int(i)),
            Expression::Float(f) => Ok(Value::Float(f)),
            Expression::True => Ok(Value::Bool(true)),
            Expression::False => Ok(Value::Bool(false)),
            Expression::String(s) => Ok(Value::String(s.into())),
            Expression::Unary(op, e1) => self.interp_unary(op, *e1),
            Expression::BinOp(e1, op, e2) => self.interp_binop(*e1, op, *e2),
            Expression::Grouping(e) => self.interp_expr(*e),
            Expression::If(e1, s1, s2) => self.interp_if(*e1, *s1, Some(*s2)),
            Expression::Var(id) => {
                let res = self.env.lock().unwrap().get(id);
                res.map_err(|e| self.with_suggestion(e))
            }
            Expression::Call(callee, args) => self.interp_call(*callee, args, span),
            Expression::Assign(lhs, rhs) => {
                let value = self.interp_expr(*rhs)?;
                let res = self.env.lock().unwrap().assign(lhs, value);
                res.map_err(|e| self.with_suggestion(e))
            }
            Expression::Closure(ps, ret_type, stmt) => Ok(Value::Fn(Function {
                params: ps,
                body: *stmt,
                ret_type,
                env: self.env.clone(),
            })),
        };
        // The innermost expression that fails supplies the error's location.
        result.map_err(|e| e.at(span))
    }

    /// Attach a "did you mean" suggestion to an undefined-variable error.
    fn with_suggestion(&self, err: RuntimeError) -> RuntimeError {
        match err.kind {
            ErrorKind::UndefinedVariable { var, .. } => {
                let names = self.env.lock().unwrap().names();
                let suggestion = closest_match(&var, names.iter().map(|n| n.as_str())).map(Id::from);
                RuntimeError {
                    kind: ErrorKind::UndefinedVariable { var, suggestion },
                    ..err
                }
            }
            _ => err,
        }
    }

    fn interp_call(&mut self, callee: Expr, args: Vec<Expr>, call_site: Span) -> Answer {
        let name: Option<Id> = match &callee.kind {
            Expression::Var(id) => Some(id.clone()),
            _ => None,
        };
        let describe = |name: &Option<Id>| match name {
            Some(n) => format!("function '{n}'"),
            None => "an anonymous function".to_string(),
        };

        let callee = self.interp_expr(callee)?;

        let arg_spans: Vec<Span> = args.iter().map(|a| a.span).collect();
        let args = args
            .into_iter()
            .map(|e| self.interp_expr(e))
            .collect::<Result<Vec<Value>, RuntimeError>>()?;

        match callee {
            Value::Fn(Function {
                params,
                body,
                ret_type,
                env,
            }) => {
                if args.len() != params.len() {
                    return Err(RuntimeError::adhoc(format!(
                        "{} takes {} but {} {} given",
                        describe(&name),
                        plural(params.len(), "argument"),
                        args.len(),
                        if args.len() == 1 { "was" } else { "were" },
                    )));
                }

                // Each call gets a fresh scope whose parent is the
                // function's captured environment, so parameters never
                // clobber other calls' bindings (e.g. during recursion).
                let mut call_env = RuntimeEnv::from_enclosing(env);
                for ((var, value), arg_span) in params.iter().zip(args).zip(arg_spans) {
                    if !Self::matches_type(&value, var.t.as_ref()) {
                        return Err(RuntimeError::type_error(format!(
                            "mismatched types: parameter '{}' of {} expects {}, found {}",
                            var.name,
                            describe(&name),
                            var.t.as_ref().unwrap(),
                            value.type_name()
                        ))
                        .at(arg_span));
                    }
                    call_env.define(var.name.clone(), value)?;
                }

                let prev_env = std::mem::replace(&mut self.env, Arc::new(Mutex::new(call_env)));
                let result = self.run_body(body);
                // Restore the caller's environment even if the body failed.
                self.env = prev_env;

                // Errors located inside the callee get a note saying where it was called.
                let frame = |e: RuntimeError| e.called_from(describe(&name), call_site);
                let (ret, ret_span) = result.map_err(frame)?;
                Self::check_return(&ret, ret_type.as_ref(), name.as_ref(), ret_span).map_err(frame)?;
                Ok(ret)
            }
            Value::NativeFn(_, run) => run(&args).map_err(RuntimeError::adhoc),
            other => Err(RuntimeError::type_error(format!(
                "cannot call a value of type {}{}",
                other.type_name(),
                match &name {
                    Some(n) => format!(" ('{n}' is not a function)"),
                    None => String::new(),
                }
            ))),
        }
    }

    fn interp_let(&mut self, var: Variable, e: Expr) -> Answer {
        let span = e.span;
        let r = self.interp_expr(e)?;
        if !Self::matches_type(&r, var.t.as_ref()) {
            return Err(RuntimeError::type_error(format!(
                "mismatched types: '{}' is declared as {} but was given {}",
                var.name,
                var.t.as_ref().unwrap(),
                r.type_name()
            ))
            .at(span));
        }
        self.env.lock().unwrap().define(var.name, r)?;
        Ok(Value::Void)
    }

    fn interp_return(&mut self, e: Option<Expr>) -> Answer {
        if let Some(expr) = e {
            let span = expr.span;
            let value = self.interp_expr(expr)?;
            Err(RuntimeError::ret(value, Some(span)))
        } else {
            Err(RuntimeError::ret(Value::Void, None))
        }
    }

    fn interp_if(&mut self, e1: Expr, s1: Statement, s2: Option<Statement>) -> Answer {
        let cond_span = e1.span;
        match self.interp_expr(e1)? {
            Value::Bool(true) => self.interp_stmt(s1),
            Value::Bool(false) => match s2 {
                Some(s) => self.interp_stmt(s),
                None => Ok(Value::Void),
            },
            other => Err(RuntimeError::type_error(format!(
                "'if' condition must be a bool, found {}",
                other.type_name()
            ))
            .at(cond_span)),
        }
    }

    fn interp_while(&mut self, e: Expr, s: Statement) -> Answer {
        let cond_span = e.span;
        loop {
            match self.interp_expr(e.clone())? {
                Value::Bool(true) => self.interp_stmt(s.clone())?,
                Value::Bool(false) => return Ok(Value::Void),
                other => {
                    return Err(RuntimeError::type_error(format!(
                        "'while' condition must be a bool, found {}",
                        other.type_name()
                    ))
                    .at(cond_span))
                }
            };
        }
    }

    /// Evaluate `&&` / `||`, only evaluating the right operand when needed.
    fn interp_logical(&mut self, e1: Expr, op: Op2, e2: Expr) -> Answer {
        let type_err = |side: &str, v: &Value, span: Span| {
            RuntimeError::type_error(format!(
                "'{op}' expects bool operands, but the {side} operand is {}",
                v.type_name()
            ))
            .at(span)
        };

        let (left_span, right_span) = (e1.span, e2.span);
        let left = match self.interp_expr(e1)? {
            Value::Bool(b) => b,
            other => return Err(type_err("left", &other, left_span)),
        };
        match (&op, left) {
            (Op2::And, false) => return Ok(Value::Bool(false)),
            (Op2::Or, true) => return Ok(Value::Bool(true)),
            _ => {}
        }
        match self.interp_expr(e2)? {
            Value::Bool(right) => Ok(Value::Bool(right)),
            other => Err(type_err("right", &other, right_span)),
        }
    }

    fn interp_binop(&mut self, e1: Expr, op: Op2, e2: Expr) -> Answer {
        use Value::{Bool, Float, Int, String as Str};

        if matches!(op, Op2::And | Op2::Or) {
            return self.interp_logical(e1, op, e2);
        }

        let r1 = self.interp_expr(e1)?;
        let r2 = self.interp_expr(e2)?;

        let overflow = || RuntimeError::adhoc(format!("integer overflow in '{op}'"));
        let int_op = |r: Option<i32>| r.map(Int).ok_or_else(overflow);

        let result = match (&op, &r1, &r2) {
            (Op2::Addition, Int(a), Int(b)) => int_op(a.checked_add(*b)),
            (Op2::Subtraction, Int(a), Int(b)) => int_op(a.checked_sub(*b)),
            (Op2::Multiplication, Int(a), Int(b)) => int_op(a.checked_mul(*b)),
            (Op2::Division | Op2::Modulo, Int(_), Int(0)) => Err(RuntimeError::adhoc(
                if matches!(op, Op2::Division) {
                    "division by zero".to_string()
                } else {
                    "modulo by zero".to_string()
                },
            )),
            (Op2::Division, Int(a), Int(b)) => int_op(a.checked_div(*b)),
            (Op2::Modulo, Int(a), Int(b)) => int_op(a.checked_rem(*b)),

            (Op2::Addition, Float(a), Float(b)) => Ok(Float(a + b)),
            (Op2::Subtraction, Float(a), Float(b)) => Ok(Float(a - b)),
            (Op2::Multiplication, Float(a), Float(b)) => Ok(Float(a * b)),
            (Op2::Division, Float(a), Float(b)) => Ok(Float(a / b)),
            (Op2::Modulo, Float(a), Float(b)) => Ok(Float(a % b)),

            (Op2::Equal, _, _) | (Op2::NotEqual, _, _) => {
                let eq = match (&r1, &r2) {
                    (Bool(a), Bool(b)) => Some(a == b),
                    (Int(a), Int(b)) => Some(a == b),
                    (Float(a), Float(b)) => Some(a == b),
                    (Str(a), Str(b)) => Some(a == b),
                    _ => None,
                };
                match eq {
                    Some(eq) => Ok(Bool(if matches!(op, Op2::Equal) { eq } else { !eq })),
                    None => Err(Self::operand_error(&op, &r1, &r2)),
                }
            }

            (Op2::LessThan, Int(a), Int(b)) => Ok(Bool(a < b)),
            (Op2::LessEqual, Int(a), Int(b)) => Ok(Bool(a <= b)),
            (Op2::GreaterThan, Int(a), Int(b)) => Ok(Bool(a > b)),
            (Op2::GreaterEqual, Int(a), Int(b)) => Ok(Bool(a >= b)),
            (Op2::LessThan, Float(a), Float(b)) => Ok(Bool(a < b)),
            (Op2::LessEqual, Float(a), Float(b)) => Ok(Bool(a <= b)),
            (Op2::GreaterThan, Float(a), Float(b)) => Ok(Bool(a > b)),
            (Op2::GreaterEqual, Float(a), Float(b)) => Ok(Bool(a >= b)),

            _ => Err(Self::operand_error(&op, &r1, &r2)),
        };
        result
    }

    fn operand_error(op: &Op2, r1: &Value, r2: &Value) -> RuntimeError {
        let (t1, t2) = (r1.type_name(), r2.type_name());
        let mut msg = format!("cannot apply '{op}' to {t1} and {t2}");
        if t1 != t2 && matches!((t1.as_str(), t2.as_str()), ("int", "float") | ("float", "int")) {
            msg += " (int and float are not converted automatically)";
        }
        RuntimeError::type_error(msg)
    }

    fn interp_unary(&mut self, op: Op1, expr: Expr) -> Answer {
        let e = self.interp_expr(expr)?;
        match (op, e) {
            (Op1::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
            (Op1::Negate, Value::Int(i)) => i
                .checked_neg()
                .map(Value::Int)
                .ok_or_else(|| RuntimeError::adhoc("integer overflow in '-'".to_string())),
            (Op1::Negate, Value::Float(f)) => Ok(Value::Float(-f)),
            (Op1::Not, other) => Err(RuntimeError::type_error(format!(
                "cannot apply '!' to {}; it expects a bool",
                other.type_name()
            ))),
            (Op1::Negate, other) => Err(RuntimeError::type_error(format!(
                "cannot apply '-' to {}; it expects an int or float",
                other.type_name()
            ))),
        }
    }

    /// Whether `v` fits annotation `t` (no annotation accepts anything).
    /// Function values only need to be functions; their signatures are
    /// checked statically.
    fn matches_type(v: &Value, t: Option<&Type>) -> bool {
        match t {
            None => true,
            Some(Type::Fn { .. }) => matches!(v, Value::Fn(_) | Value::NativeFn(..)),
            Some(t) => v.to_type() == *t,
        }
    }

    fn check_return(
        v: &Value,
        t: Option<&Type>,
        name: Option<&Id>,
        span: Option<Span>,
    ) -> Result<(), RuntimeError> {
        if Self::matches_type(v, t) {
            return Ok(());
        }
        let who = match name {
            Some(n) => format!("function '{n}'"),
            None => "function".to_string(),
        };
        let err = RuntimeError::type_error(format!(
            "mismatched types: {who} is declared to return {} but returned {}",
            t.unwrap(),
            v.type_name()
        ));
        // Point at the returned expression when we know it; otherwise the
        // caller's span (the call site) is attached as the error propagates.
        Err(match span {
            Some(span) => err.at(span),
            None => err,
        })
    }
}

fn plural(n: usize, word: &str) -> String {
    if n == 1 {
        format!("1 {word}")
    } else {
        format!("{n} {word}s")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::tokenize, parse::Parser};

    fn run(src: &str) -> Answer {
        let ast = Parser::new(tokenize(src).unwrap().into_iter())
            .parse()
            .unwrap();
        Interpreter::new(ast).run()
    }

    /// View a result's error as its kind, for matching in assertions.
    fn kind(v: &Answer) -> Result<&Value, &ErrorKind> {
        v.as_ref().map_err(|e| &e.kind)
    }

    fn run_main(body: &str) -> Answer {
        run(&format!("fn main() {{ {body} }}"))
    }

    #[test]
    fn recursion_uses_per_call_scope() {
        let v = run(
            "fn fib(n: int) -> int { if n < 2 { n } else { fib(n - 1) + fib(n - 2) } }
             fn main() -> int { fib(10) }",
        );
        assert!(matches!(kind(&v), Ok(Value::Int(55))), "got {:?}", v);
    }

    #[test]
    fn params_do_not_leak_into_caller_scope() {
        let v = run("fn g(x: int) -> int { x } fn main() -> int { g(4); x }");
        assert!(
            matches!(kind(&v), Err(ErrorKind::UndefinedVariable { .. })),
            "got {:?}",
            v
        );
    }

    #[test]
    fn caller_env_restored_after_error_in_callee() {
        // After a failed call, the caller's locals must still be visible.
        let v = run(
            "fn bad(x: int) -> int { assert(false); x }
             fn main() -> int { let a = 7; bad(1); a }",
        );
        assert!(matches!(kind(&v), Err(ErrorKind::Adhoc(_))), "got {:?}", v);
    }

    #[test]
    fn closures_still_capture_and_mutate() {
        let v = run(
            "fn mk() { let c = 0; fn(x) { c = c + x; c } }
             fn main() { let k = mk(); k(1); k(1); k(5) }",
        );
        assert!(matches!(kind(&v), Ok(Value::Int(7))), "got {:?}", v);
    }

    #[test]
    fn modulo() {
        assert!(matches!(kind(&run_main("7 % 3")), Ok(Value::Int(1))));
        assert!(matches!(kind(&run_main("2 + 7 % 3 * 2")), Ok(Value::Int(4))));
        assert!(matches!(kind(&run_main("7 % 0")), Err(ErrorKind::Adhoc(_))));
        assert!(matches!(kind(&run_main("7 / 0")), Err(ErrorKind::Adhoc(_))));
    }

    #[test]
    fn minus_is_an_operator() {
        assert!(matches!(kind(&run_main("let a = 5; let b = 2; a-b")), Ok(Value::Int(3))));
        assert!(matches!(kind(&run_main("let a = 5; a -1")), Ok(Value::Int(4))));
        assert!(matches!(kind(&run_main("5-1")), Ok(Value::Int(4))));
        assert!(matches!(kind(&run_main("-3 * -2")), Ok(Value::Int(6))));
    }

    #[test]
    fn logical_operators_short_circuit() {
        let boom = "fn boom() -> bool { assert(false); true }";
        let v = run(&format!("{boom} fn main() -> bool {{ false && boom() }}"));
        assert!(matches!(kind(&v), Ok(Value::Bool(false))), "got {:?}", v);
        let v = run(&format!("{boom} fn main() -> bool {{ true || boom() }}"));
        assert!(matches!(kind(&v), Ok(Value::Bool(true))), "got {:?}", v);
        let v = run(&format!("{boom} fn main() -> bool {{ true && boom() }}"));
        assert!(matches!(kind(&v), Err(ErrorKind::Adhoc(_))), "got {:?}", v);
        assert!(matches!(kind(&run_main("true && 1")), Err(ErrorKind::TypeError { .. })));
    }

    #[test]
    fn let_shadowing_in_same_scope() {
        assert!(matches!(kind(&run_main("let a = 1; let a = a + 1; a")), Ok(Value::Int(2))));
        assert!(matches!(kind(&run_main("let a: int = 1; let a: str = \"s\"; a")),
            Ok(Value::String(_))
        ));
    }

    #[test]
    fn duplicate_function_definitions_error() {
        let v = run("fn f() { 1 } fn f() { 2 } fn main() { f() }");
        assert!(matches!(kind(&v), Err(ErrorKind::Adhoc(m)) if m.contains("defined more than once")));
    }

    #[test]
    fn negate_float() {
        let v = run_main("let x = 1.5; -x");
        assert!(matches!(kind(&v), Ok(Value::Float(f)) if *f == -1.5), "got {:?}", v);
    }

    #[test]
    fn call_errors_are_reported_not_panics() {
        let v = run("fn g(x: int) -> int { x } fn main() -> int { g(1, 2) }");
        assert!(matches!(kind(&v), Err(ErrorKind::Adhoc(_))), "got {:?}", v);
        let v = run("fn g(x: int) -> int { x } fn main() -> int { g(\"s\") }");
        assert!(matches!(kind(&v), Err(ErrorKind::TypeError { .. })), "got {:?}", v);
        assert!(matches!(kind(&run_main("print(nope)")),
            Err(ErrorKind::UndefinedVariable { .. })
        ));
    }

    /// The source text an error points at, plus its call-trace notes.
    fn located(src: &str) -> (String, Vec<String>) {
        let err = run(src).expect_err("program should fail");
        let span = err.span.expect("error should have a location");
        let line = src.lines().nth(span.start.line as usize - 1).unwrap();
        let text: String = line
            .chars()
            .skip(span.start.col as usize - 1)
            .take((span.end.col - span.start.col) as usize)
            .collect();
        (text, err.to_diagnostic().notes)
    }

    #[test]
    fn runtime_errors_point_at_the_failing_expression() {
        assert_eq!(located("fn main() { let a = 1; a + \"s\" }").0, "a + \"s\"");
        assert_eq!(located("fn main() { let count = 1; cout }").0, "cout");
        assert_eq!(located("fn main() { if 1 { 2 } else { 3 } }").0, "1");
        assert_eq!(located("fn main() { let x: str = 5; x }").0, "5");
        assert_eq!(located("fn f(n: int) { n } fn main() { f(true) }").0, "true");
        assert_eq!(located("fn f() -> int { \"s\" } fn main() { f() }").0, "\"s\"");
        assert_eq!(located("fn main() { assert(1 == 2) }").0, "assert(1 == 2)");
        assert_eq!(located("fn main() { true && 5 }").0, "5");
    }

    #[test]
    fn runtime_errors_carry_a_call_trace() {
        let (text, notes) = located(
            "fn c(x) { x / 0 }\nfn b(x) { c(x) }\nfn main() { b(1) }",
        );
        assert_eq!(text, "x / 0");
        assert_eq!(
            notes,
            vec![
                "in function 'c', called at line 2, column 11",
                "in function 'b', called at line 3, column 13",
            ]
        );

        // Recursive frames collapse into a single note.
        let (_, notes) = located(
            "fn down(n: int) -> int { if n == 0 { 1 / n } else { down(n - 1) } }\nfn main() { down(5) }",
        );
        assert_eq!(
            notes,
            vec![
                "in function 'down', called at line 1, column 53 (5 times)",
                "in function 'down', called at line 2, column 13",
            ]
        );
    }
}
