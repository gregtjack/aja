//! Methods: `impl` blocks, method calls, and associated functions.
//!
//! An `impl` gives a struct or enum functions. One that takes `self` is a
//! method, called on a value (`p.norm()`); one that doesn't is an associated
//! function, called through the type (`Point::origin()`). Methods are always
//! called with `.`: there is no second way to call one, and a method isn't a
//! value until it is wrapped in a closure.
//!
//! An `impl` must be in the same program as its type (the prelude's types get
//! theirs in the prelude), so there is one place to look for a type's methods.
//! A generic type's `impl` declares the type's parameters and gives them as
//! its arguments, `impl<T> Option<T>`; a method can add its own, as in
//! `fn map<U>(self, f: (T) -> U) -> Option<U>`. Each method is checked once,
//! like a generic function, and compiled once, as a function whose first
//! parameter is `self`.

use std::collections::{HashMap, HashSet};

use super::{key, plural, Checker, Class, FnContext, Ty, TypeError};
use crate::{
    ast::{types::Type, Definition, Expr, Id, Method, Program, Source, Span},
    diagnostic::closest_match,
};

/// A function defined in an `impl`.
#[derive(Debug, Clone)]
pub(super) struct MethodInfo {
    /// The name it is compiled under, e.g. `Point::norm`.
    pub global: Id,
    /// The impl's type parameters, then the method's own.
    pub generics: Vec<Id>,
    /// How many of `generics` are the impl's.
    pub impl_params: usize,
    /// Its type, a [`Ty::Fn`] whose first parameter is `self` for a method.
    pub ty: Ty,
    pub has_self: bool,
    pub param_names: Vec<Id>,
    /// Whether it is marked `pub`; if not, only code in `source`, the file
    /// it is defined in, can call it.
    pub public: bool,
    pub source: Source,
}

impl MethodInfo {
    /// Whether code at `at` can call it.
    fn visible(&self, at: Span) -> bool {
        self.public || at.start.source == self.source
    }
}

/// A method whose body is still to be checked.
pub(super) struct MethodBody<'p> {
    method: &'p Method,
    global: Id,
    params: Vec<Ty>,
    ret: Ty,
    generics: Vec<Id>,
}

impl Checker {
    /// Check the impls of `program` and declare their functions, returning
    /// the bodies to check once every signature is known.
    pub(super) fn declare_impls<'p>(&mut self, program: &'p Program) -> Vec<MethodBody<'p>> {
        let local: HashSet<&Id> = program
            .definitions
            .iter()
            .filter_map(|d| match d {
                Definition::Struct { name, .. } | Definition::Enum { name, .. } => Some(name),
                _ => None,
            })
            .collect();
        let mut bodies = Vec::new();
        for def in &program.definitions {
            let Definition::Impl {
                type_params,
                target,
                methods,
            } = def
            else {
                continue;
            };
            let Some((type_name, impl_params)) = self.impl_target(type_params, target, &local)
            else {
                continue;
            };
            for m in methods {
                if let Some(body) = self.declare_method(&type_name, &impl_params, m) {
                    bodies.push(body);
                }
            }
        }
        bodies
    }

    /// The type an impl is for, and the names of its type parameters, or
    /// `None` (having reported why) if it can't have one.
    fn impl_target(
        &mut self,
        type_params: &[crate::ast::TypeParam],
        target: &Type,
        local: &HashSet<&Id>,
    ) -> Option<(Id, Vec<Id>)> {
        let Type::Named(name, args, span) = target else {
            let shown = target.to_string();
            let span = type_params.first().map_or(Self::dummy_span(), |p| p.span);
            self.error(
                TypeError::new(format!("methods can't be defined for {shown}"), span)
                    .note("an impl is for a struct or enum; built-in types get their methods from the standard library"),
            );
            return None;
        };
        let params = match (self.structs.get(name), self.enums.get(name)) {
            (Some(s), _) => s.params.clone(),
            (_, Some(e)) => e.params.clone(),
            _ => {
                self.named_type(name, args, *span);
                return None;
            }
        };
        if !local.contains(name) {
            self.error(
                TypeError::new(format!("methods for '{name}' can only be defined where '{name}' is defined, in the prelude"), *span)
                    .help("write a function that takes it instead")
                    .note("a type's methods are all defined next to it, so there is one place to look for them"),
            );
            return None;
        }

        let declared = self.declare_type_params(type_params);
        let matches = args.len() == params.len()
            && declared.len() == params.len()
            && args
                .iter()
                .zip(&declared)
                .all(|(a, p)| matches!(a, Type::Named(n, xs, _) if n == p && xs.is_empty()));
        if !matches {
            let example = if params.is_empty() {
                format!("impl {name}")
            } else {
                format!("impl<{0}> {name}<{0}>", params.join(", "))
            };
            self.error(
                TypeError::new(format!("an impl for '{name}' must be for all of it: `{example}`"), *span)
                    .help(format!("write `{example} {{ ... }}`"))
                    .note("an impl declares the type's own type parameters and gives them, in order, as its arguments"),
            );
            return None;
        }
        Some((name.clone(), declared))
    }

    fn declare_method<'p>(
        &mut self,
        type_name: &Id,
        impl_params: &[Id],
        m: &'p Method,
    ) -> Option<MethodBody<'p>> {
        let global: Id = format!("{type_name}::{}", m.name).into();
        let clash = match (self.structs.get(type_name), self.enums.get(type_name)) {
            (Some(s), _) if s.fields.iter().any(|(f, _)| *f == m.name) => {
                Some(format!("struct '{type_name}' has a field named '{}', so it can't also have a method with that name", m.name))
            }
            (_, Some(e)) if e.variants.iter().any(|(v, _)| *v == m.name) => {
                Some(format!("enum '{type_name}' has a variant named '{}', so it can't also have a method with that name", m.name))
            }
            _ if self.methods.get(type_name).is_some_and(|ms| ms.contains_key(&m.name)) => {
                Some(format!("'{type_name}' already has a method named '{}'", m.name))
            }
            _ => None,
        };
        if let Some(message) = clash {
            self.error(TypeError::new(message, m.name_span).help("choose a different name"));
            return None;
        }

        let own: Vec<Id> = self
            .declare_type_params(&m.type_params)
            .into_iter()
            .filter(|p| {
                let dup = impl_params.contains(p);
                if dup {
                    let span = m.type_params.iter().find(|t| t.name == *p).unwrap().span;
                    self.error(
                        TypeError::new(
                            format!("type parameter '{p}' is already declared by the impl"),
                            span,
                        )
                        .help("use the impl's, or choose a different name"),
                    );
                }
                !dup
            })
            .collect();
        let generics: Vec<Id> = impl_params.iter().chain(&own).cloned().collect();
        self.type_params = generics.clone();
        let this = Ty::Named(
            type_name.clone(),
            impl_params.iter().map(|p| Ty::Param(p.clone())).collect(),
        );
        let params: Vec<Ty> = m
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| match &p.t {
                _ if i == 0 && m.has_self => this.clone(),
                Some(t) => self.from_ast(t),
                None => {
                    self.error(
                        TypeError::new(format!("parameter '{}' of '{global}' needs a type annotation", p.name), p.span)
                            .help(format!("add a type, for example `{}: int`", p.name))
                            .note("function signatures are always written out; types are inferred inside function bodies"),
                    );
                    Ty::Error
                }
            })
            .collect();
        let ret = m.rtype.as_ref().map_or(Ty::Void, |t| self.from_ast(t));
        self.type_params.clear();
        let ty = Ty::Fn(params.clone(), Box::new(ret.clone()));
        self.require_used(&m.type_params, &[&ty], &format!("function '{global}'"));

        let info = MethodInfo {
            global: global.clone(),
            generics: generics.clone(),
            impl_params: impl_params.len(),
            ty,
            has_self: m.has_self,
            param_names: m.params.iter().map(|p| p.name.clone()).collect(),
            public: m.public,
            source: m.name_span.start.source,
        };
        self.methods
            .entry(type_name.clone())
            .or_default()
            .insert(m.name.clone(), info);
        Some(MethodBody {
            method: m,
            global,
            params,
            ret,
            generics,
        })
    }

    /// Check the bodies of methods declared by [`Checker::declare_impls`].
    pub(super) fn check_method_bodies(&mut self, bodies: Vec<MethodBody>) {
        for b in bodies {
            let ctx = FnContext {
                name: Some(b.global.clone()),
                ret: b.ret,
                ret_declared: b.method.rtype.is_some(),
                span: b.method.name_span,
                try_reported: false,
            };
            self.type_params = b.generics;
            self.function(ctx, &b.method.params, &b.params, &b.method.body);
            self.type_params.clear();
            self.report_unresolved();
        }
    }

    pub(super) fn method(&self, type_name: &Id, name: &Id) -> Option<&MethodInfo> {
        self.methods.get(type_name)?.get(name)
    }

    /// The names of a type's methods, sorted, for messages.
    fn method_names(&self, type_name: &Id) -> Vec<Id> {
        let mut names: Vec<Id> = self
            .methods
            .get(type_name)
            .map_or(Vec::new(), |ms| ms.keys().cloned().collect());
        names.sort();
        names
    }

    /// `obj.name(args)`: a method call, checked here (`Ok`, its result), or
    /// a call of a struct's field (`Err`, the field's type, to be called).
    pub(super) fn method_call(
        &mut self,
        obj: &Expr,
        name: &Id,
        name_span: Span,
        callee: &Expr,
        args: &[Expr],
        span: Span,
    ) -> Result<Ty, Ty> {
        let t = self.expr(obj);
        match self.resolve(&t) {
            Ty::Named(n, targs) => {
                if let Some(info) = self.method(&n, name).cloned() {
                    return Ok(self.call_method(&n, name, info, &targs, callee, args, span));
                }
                let is_field = self
                    .structs
                    .get(&n)
                    .is_some_and(|s| s.fields.iter().any(|(f, _)| f == name));
                if is_field {
                    return Err(self.field_on(&t, obj.span, name, name_span, callee.span));
                }
                let kind = if self.structs.contains_key(&n) {
                    "struct"
                } else {
                    "enum"
                };
                let methods = self.method_names(&n);
                let fields: Vec<Id> = self.structs.get(&n).map_or(Vec::new(), |s| {
                    s.fields.iter().map(|(f, _)| f.clone()).collect()
                });
                let help =
                    match closest_match(name, methods.iter().chain(&fields).map(|m| m.as_str())) {
                        Some(s) if methods.iter().any(|m| m == s) => {
                            format!("a method with a similar name exists: '{s}'")
                        }
                        Some(s) => format!("a field with a similar name exists: '{s}'"),
                        None if methods.is_empty() => format!(
                            "'{n}' has no methods; define them in an `impl {n} {{ ... }}` block"
                        ),
                        None => format!("its methods are: {}", methods.join(", ")),
                    };
                self.error(
                    TypeError::new(format!("{kind} '{n}' has no method '{name}'"), name_span)
                        .help(help),
                );
            }
            Ty::Error | Ty::Never => {}
            Ty::Var(_) => {
                self.error(
                    TypeError::new(
                        format!("type annotations needed: the type of this value must be known to call its method '{name}'"),
                        obj.span,
                    )
                    .help("annotate the variable or parameter it comes from, e.g. `p: Point`"),
                );
                self.poison(&[&t]);
            }
            other => {
                let shown = self.show(&other);
                self.error(
                    TypeError::new(format!("{shown} has no method '{name}'"), name_span)
                        .note("built-in types have no methods yet; they will come with the standard library"),
                );
            }
        }
        for a in args {
            self.expr(a);
        }
        Ok(Ty::Error)
    }

    #[allow(clippy::too_many_arguments)]
    fn call_method(
        &mut self,
        type_name: &Id,
        name: &Id,
        info: MethodInfo,
        targs: &[Ty],
        callee: &Expr,
        args: &[Expr],
        span: Span,
    ) -> Ty {
        if !info.has_self {
            self.error(
                TypeError::new(
                    format!("'{name}' is an associated function of '{type_name}', not a method, so it isn't called on a value"),
                    callee.span,
                )
                .help(format!("call it as `{type_name}::{name}(...)`")),
            );
            for a in args {
                self.expr(a);
            }
            return Ty::Error;
        }
        if !info.visible(callee.span) {
            self.private_method(type_name, name, &info, callee.span);
        }
        let map: HashMap<Id, Ty> = info
            .generics
            .iter()
            .enumerate()
            .map(|(i, g)| {
                (
                    g.clone(),
                    if i < info.impl_params {
                        targs[i].clone()
                    } else {
                        self.fresh(Class::Any)
                    },
                )
            })
            .collect();
        let Ty::Fn(params, ret) = Self::subst(&info.ty, &map) else {
            unreachable!()
        };
        let (params, names) = (&params[1..], &info.param_names[1..]);
        let what = format!("method '{}'", info.global);
        if params.len() != args.len() {
            self.error(TypeError::new(
                format!(
                    "{what} takes {} but {} {} given",
                    plural(params.len(), "argument"),
                    args.len(),
                    if args.len() == 1 { "was" } else { "were" },
                ),
                span,
            ));
            for a in args {
                self.expr(a);
            }
            return *ret;
        }
        for ((arg, param), pname) in args.iter().zip(params).zip(names) {
            let found = self.expr(arg);
            self.expect(param, &found, arg.span, |e, f| {
                format!("mismatched types: parameter '{pname}' of {what} expects {e}, found {f}")
            });
        }
        self.functions.insert(key(callee.span), info.global);
        *ret
    }

    /// `Type::name` for an associated function: its type, for this use. `None`
    /// if `Type` has no function `name`, so the path is a variant.
    pub(super) fn associated(&mut self, type_name: &Id, name: &Id, span: Span) -> Option<Ty> {
        let info = self.method(type_name, name)?.clone();
        if info.has_self {
            self.error(
                TypeError::new(
                    format!("'{name}' is a method of '{type_name}', so it is called on a value"),
                    span,
                )
                .help(format!("call it as `value.{name}(...)`, on a {type_name}")),
            );
            return Some(Ty::Error);
        }
        if !info.visible(span) {
            self.private_method(type_name, name, &info, span);
        }
        self.functions.insert(key(span), info.global.clone());
        Some(self.instantiate(&info.generics, &info.ty))
    }

    fn private_method(&mut self, type_name: &Id, name: &Id, info: &MethodInfo, span: Span) {
        let what = if info.has_self {
            "method"
        } else {
            "associated function"
        };
        self.error(
            TypeError::new(format!("{what} '{name}' of '{type_name}' is private"), span).help(
                format!(
                    "only code in module '{}' can call it; mark it 'pub' there to call it here",
                    super::data::module_of(type_name)
                ),
            ),
        );
    }

    /// A help message for `obj.name` without a call when `name` is a method.
    pub(super) fn method_not_called(&self, type_name: &Id, name: &Id) -> Option<String> {
        let info = self.method(type_name, name)?;
        Some(if info.has_self {
            format!("'{name}' is a method, so it must be called: `.{name}(...)`; to pass it around, wrap it in a closure")
        } else {
            format!("'{name}' is an associated function, called as `{type_name}::{name}(...)`")
        })
    }
}
