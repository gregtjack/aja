//! Checking structs, enums, and `match`.
//!
//! Structs and enums are nominal: a type is its name and its type arguments,
//! and two types are the same only if both match (`Option<int>` is not
//! `Option<str>`). Their definitions are collected before any function is
//! checked, so types can refer to each other (and to themselves) in any order.
//! A generic definition's fields are stored in terms of its type parameters
//! ([`Ty::Param`]), which each use substitutes with that use's arguments.
//!
//! A `match` must be exhaustive, and every arm must be reachable. Both are
//! decided with the "usefulness" algorithm from Maranget's *Warnings for
//! pattern matching* (2007), the one Rust and OCaml use: a pattern is useful
//! after some rows of patterns if some value matches it but none of the rows.
//! An arm is unreachable if its pattern isn't useful after the arms before it,
//! and the match is exhaustive if `_` isn't useful after all of its arms. When
//! `_` is useful, the algorithm also produces a value it matches, which is
//! what the error message lists as not covered.

use std::collections::{HashMap, HashSet};

use ecow::EcoString;

use super::{key, plural, BindingKind, Checker, Class, Ty, TypeError};
use crate::{
    ast::{
        types::Type, Arm, Definition, Expr, FieldInit, Id, Pattern, PatternKind, Program, Span,
        Statement, TypeParam,
    },
    diagnostic::closest_match,
    prelude::{self, variant_display},
};

/// A struct definition: its type parameters, and its fields in declaration
/// order, whose types may mention the parameters.
#[derive(Debug, Clone)]
pub(super) struct StructInfo {
    pub params: Vec<Id>,
    pub fields: Vec<(Id, Ty)>,
}

/// An enum definition: its type parameters, and its variants in declaration
/// order with the types of the values each holds.
#[derive(Debug, Clone)]
pub(super) struct EnumInfo {
    pub params: Vec<Id>,
    pub variants: Vec<(Id, Vec<Ty>)>,
}

/// A pattern reduced to what exhaustiveness checking needs: bindings are
/// wildcards, and struct patterns list every field in declaration order.
#[derive(Debug, Clone, PartialEq)]
enum Pat {
    Wild,
    Ctor(Ctor, Vec<Pat>),
}

/// What a pattern requires a value to be built from. Which type it belongs to
/// comes from the column the pattern is in.
#[derive(Debug, Clone, PartialEq)]
enum Ctor {
    /// The variant with this index.
    Variant(usize),
    Struct,
    Bool(bool),
    Int(i64),
    Str(EcoString),
}

/// A pattern of an arm, as checked.
struct Row {
    pat: Pat,
    guarded: bool,
    span: Span,
}

impl Checker {
    // ---- Definitions -----------------------------------------------------------

    /// Collect every struct and enum definition. Names are declared before
    /// any field type is resolved, so definitions can refer to each other.
    pub(super) fn declare_types(&mut self, program: &Program) {
        let mut accepted = Vec::new();
        for def in &program.definitions {
            let (name, span, type_params, is_struct) = match def {
                Definition::Struct {
                    name,
                    name_span,
                    type_params,
                    ..
                } => (name, *name_span, type_params, true),
                Definition::Enum {
                    name,
                    name_span,
                    type_params,
                    ..
                } => (name, *name_span, type_params, false),
                Definition::Function { .. } | Definition::Impl { .. } => continue,
            };
            if Type::try_from(name.to_string()).is_ok() {
                self.error(
                    TypeError::new(
                        format!("'{name}' is a built-in type, so it can't be redefined"),
                        span,
                    )
                    .help("choose a different name"),
                );
                continue;
            }
            if prelude::is_prelude_type(name)
                && (self.structs.contains_key(name) || self.enums.contains_key(name))
            {
                self.error(
                    TypeError::new(
                        format!("'{name}' is defined in the prelude, so it can't be redefined"),
                        span,
                    )
                    .help(format!(
                        "use the prelude's '{name}', or choose a different name"
                    )),
                );
                continue;
            }
            if self.structs.contains_key(name) || self.enums.contains_key(name) {
                self.error(
                    TypeError::new(format!("type '{name}' is defined more than once"), span)
                        .help("rename one of the definitions"),
                );
                continue;
            }
            // The parameters are checked below, once every type name is
            // known; their number is needed now, to check uses of the type.
            let params: Vec<Id> = type_params.iter().map(|p| p.name.clone()).collect();
            if is_struct {
                self.structs.insert(
                    name.clone(),
                    StructInfo {
                        params,
                        fields: Vec::new(),
                    },
                );
            } else {
                self.enums.insert(
                    name.clone(),
                    EnumInfo {
                        params,
                        variants: Vec::new(),
                    },
                );
            }
            accepted.push(def);
        }

        for def in accepted {
            let (Definition::Struct { type_params, .. } | Definition::Enum { type_params, .. }) =
                def
            else {
                unreachable!()
            };
            self.type_params = self.declare_type_params(type_params);
            match def {
                Definition::Struct { name, fields, .. } => {
                    let mut seen = HashSet::new();
                    let mut tys = Vec::new();
                    for f in fields {
                        let t = f.t.as_ref().map_or(Ty::Error, |t| self.from_ast(t));
                        if !seen.insert(f.name.clone()) {
                            self.error(TypeError::new(
                                format!(
                                    "struct '{name}' has more than one field named '{}'",
                                    f.name
                                ),
                                f.span,
                            ));
                            continue;
                        }
                        tys.push((f.name.clone(), t));
                    }
                    let used: Vec<&Ty> = tys.iter().map(|(_, t)| t).collect();
                    self.require_used(type_params, &used, &format!("struct '{name}'"));
                    self.structs.get_mut(name).unwrap().fields = tys;
                }
                Definition::Enum { name, variants, .. } => {
                    let mut seen = HashSet::new();
                    let mut vs = Vec::new();
                    for v in variants {
                        let tys: Vec<Ty> = v.fields.iter().map(|t| self.from_ast(t)).collect();
                        if !seen.insert(v.name.clone()) {
                            self.error(TypeError::new(
                                format!(
                                    "enum '{name}' has more than one variant named '{}'",
                                    v.name
                                ),
                                v.span,
                            ));
                            continue;
                        }
                        vs.push((v.name.clone(), tys));
                    }
                    let used: Vec<&Ty> = vs.iter().flat_map(|(_, ts)| ts).collect();
                    self.require_used(type_params, &used, &format!("enum '{name}'"));
                    self.enums.get_mut(name).unwrap().variants = vs;
                }
                Definition::Function { .. } | Definition::Impl { .. } => unreachable!(),
            }
            self.type_params.clear();
        }
    }

    /// Check a definition's type parameters, returning their names. A
    /// parameter can't share its name with another parameter or a type.
    pub(super) fn declare_type_params(&mut self, params: &[TypeParam]) -> Vec<Id> {
        let mut names: Vec<Id> = Vec::new();
        for p in params {
            let err = if names.contains(&p.name) {
                Some(TypeError::new(
                    format!("type parameter '{}' is declared more than once", p.name),
                    p.span,
                ))
            } else if Type::try_from(p.name.to_string()).is_ok() {
                Some(
                    TypeError::new(
                        format!(
                            "'{}' is a built-in type, so it can't be a type parameter",
                            p.name
                        ),
                        p.span,
                    )
                    .help("type parameters are usually single capital letters, e.g. `T`"),
                )
            } else if self.structs.contains_key(&p.name) || self.enums.contains_key(&p.name) {
                Some(
                    TypeError::new(
                        format!("type parameter '{}' has the same name as a type", p.name),
                        p.span,
                    )
                    .help("choose a different name, e.g. `T`"),
                )
            } else {
                None
            };
            let clashes_with_type =
                self.structs.contains_key(&p.name) || self.enums.contains_key(&p.name);
            match err {
                // Keep a parameter that only clashes with a type's name, so
                // its uses refer to it and don't cause more errors.
                Some(err) if clashes_with_type && !names.contains(&p.name) => {
                    self.error(err);
                    names.push(p.name.clone());
                }
                Some(err) => self.error(err),
                None => names.push(p.name.clone()),
            }
        }
        names
    }

    /// Every type parameter must appear in `tys` (a function's signature, or
    /// a type's fields), since that is the only way to know what it is.
    pub(super) fn require_used(&mut self, params: &[TypeParam], tys: &[&Ty], owner: &str) {
        let mut used = HashSet::new();
        for t in tys {
            Self::params_in(t, &mut used);
        }
        for p in params {
            // A built-in type's name isn't a parameter, and has been reported.
            if !used.contains(&p.name) && Type::try_from(p.name.to_string()).is_err() {
                let where_ = if owner.starts_with("function") {
                    "its signature"
                } else {
                    "its definition"
                };
                self.error(
                    TypeError::new(
                        format!("type parameter '{}' of {owner} is never used", p.name),
                        p.span,
                    )
                    .help(format!("remove it, or use it in {where_}")),
                );
            }
        }
    }

    /// The type a name, with type arguments, refers to: a type parameter in
    /// scope, or a struct or enum. Reports it if unknown or if the number of
    /// arguments is wrong.
    pub(super) fn named_type(&mut self, name: &Id, args: &[Type], span: Span) -> Ty {
        if self.type_params.contains(name) {
            if !args.is_empty() {
                self.error(
                    TypeError::new(
                        format!("type parameter '{name}' takes no type arguments"),
                        span,
                    )
                    .help(format!("write `{name}`")),
                );
            }
            return Ty::Param(name.clone());
        }
        let params = match (self.structs.get(name), self.enums.get(name)) {
            (Some(s), _) => Some(s.params.clone()),
            (_, Some(e)) => Some(e.params.clone()),
            _ => None,
        };
        if let Some(params) = params {
            let args: Vec<Ty> = args.iter().map(|a| self.from_ast(a)).collect();
            if args.len() == params.len() {
                return Ty::Named(name.clone(), args);
            }
            let example = if params.is_empty() {
                name.to_string()
            } else {
                let shown: Vec<String> = (0..params.len())
                    .map(|i| args.get(i).map_or("int".to_string(), |a| self.show(a)))
                    .collect();
                format!("{name}<{}>", shown.join(", "))
            };
            let message = if params.is_empty() {
                format!("type '{name}' takes no type arguments")
            } else {
                format!(
                    "type '{name}' takes {} but {} {} given",
                    plural(params.len(), "type argument"),
                    args.len(),
                    if args.len() == 1 { "was" } else { "were" },
                )
            };
            let mut err = TypeError::new(message, span).help(format!("write `{example}`"));
            if !params.is_empty() {
                err = err.note(format!(
                    "'{name}' is declared as `{name}<{}>`",
                    params.join(", ")
                ));
            }
            self.error(err);
            return Ty::Error;
        }
        let builtin = ["int", "float", "bool", "str", "void"];
        let names: Vec<&str> = self
            .structs
            .keys()
            .chain(self.enums.keys())
            .chain(self.type_params.iter())
            .map(|n| n.as_str())
            .chain(builtin)
            .collect();
        let help = match closest_match(name, names.into_iter()) {
            Some(s) => format!("a type with a similar name exists: '{s}'"),
            None => format!("define it with `struct {name} {{ ... }}` or `enum {name} {{ ... }}`"),
        };
        self.error(TypeError::new(format!("cannot find type '{name}'"), span).help(help));
        Ty::Error
    }

    /// Whether values of the struct or enum `name` can be compared with `==`,
    /// provided its type arguments can be: everything else it holds can be.
    pub(super) fn comparable(&self, name: &Id, visiting: &mut HashSet<Id>) -> bool {
        if !visiting.insert(name.clone()) {
            return true; // A recursive type is comparable if the rest of it is.
        }
        let tys: Vec<&Ty> = match (self.structs.get(name), self.enums.get(name)) {
            (Some(s), _) => s.fields.iter().map(|(_, t)| t).collect(),
            (_, Some(e)) => e.variants.iter().flat_map(|(_, ts)| ts).collect(),
            _ => return true,
        };
        tys.into_iter().all(|t| self.comparable_field(t, visiting))
    }

    /// Whether a field of type `t` (in a definition) can be compared.
    fn comparable_field(&self, t: &Ty, visiting: &mut HashSet<Id>) -> bool {
        match t {
            Ty::Named(n, args) => {
                self.comparable(n, visiting)
                    && args.iter().all(|a| self.comparable_field(a, visiting))
            }
            // Checked against the type argument wherever the type is used.
            Ty::Param(_) => true,
            t => Class::Eq.admits(t),
        }
    }

    /// A help message for using the type name `name` as a variable, if it is one.
    pub(super) fn type_used_as_value(&self, name: &Id) -> Option<String> {
        if self.structs.contains_key(name) {
            return Some(format!(
                "'{name}' is a struct; make one with `{name} {{ ... }}`"
            ));
        }
        let first = self.enums.get(name)?.variants.first();
        Some(match first {
            Some((v, _)) => format!(
                "'{name}' is an enum; make a value with one of its variants, e.g. `{name}::{v}`"
            ),
            None => format!("'{name}' is an enum with no variants, so it has no values"),
        })
    }

    // ---- Expressions -------------------------------------------------------------

    /// `obj.name`
    pub(super) fn field(&mut self, obj: &Expr, name: &Id, name_span: Span, span: Span) -> Ty {
        let t = self.expr(obj);
        self.field_on(&t, obj.span, name, name_span, span)
    }

    /// `obj.name`, where `obj` (at `obj_span`) has type `t`.
    pub(super) fn field_on(
        &mut self,
        t: &Ty,
        obj_span: Span,
        name: &Id,
        name_span: Span,
        span: Span,
    ) -> Ty {
        let t = t.clone();
        if let Ty::Named(n, _) = self.resolve(&t) {
            if let Some(help) = self.method_not_called(&n, name) {
                self.error(
                    TypeError::new(
                        format!("'{name}' is a function of '{n}', not a field"),
                        name_span,
                    )
                    .help(help),
                );
                return Ty::Error;
            }
        }
        match self.resolve(&t) {
            Ty::Named(n, args) if self.structs.contains_key(&n) => {
                let info = &self.structs[&n];
                match info.fields.iter().position(|(f, _)| f == name) {
                    Some(i) => {
                        let map = info.params.iter().cloned().zip(args).collect();
                        let t = Self::subst(&info.fields[i].1, &map);
                        self.fields.insert(key(span), i as u16);
                        t
                    }
                    None => {
                        let help = self.field_help(&n);
                        self.no_field(&n, name, name_span, help);
                        Ty::Error
                    }
                }
            }
            Ty::Named(n, _) => {
                self.error(
                    TypeError::new(format!("'{n}' is an enum, so it has no fields"), name_span)
                        .help("use `match` to get at the values its variants hold"),
                );
                Ty::Error
            }
            Ty::Error | Ty::Never => Ty::Error,
            Ty::Var(_) => {
                self.error(
                    TypeError::new(
                        format!("type annotations needed: the type of this value must be known to access its field '{name}'"),
                        obj_span,
                    )
                    .help("annotate the variable or parameter it comes from, e.g. `p: Point`"),
                );
                self.poison(&[&t]);
                Ty::Error
            }
            other => {
                let shown = self.show(&other);
                self.error(TypeError::new(
                    format!("{shown} has no field '{name}'"),
                    name_span,
                ));
                Ty::Error
            }
        }
    }

    /// Suggestions for a field name that `struct_name` doesn't have.
    fn field_help(&self, struct_name: &Id) -> Vec<Id> {
        self.structs[struct_name]
            .fields
            .iter()
            .map(|(f, _)| f.clone())
            .collect()
    }

    fn no_field(&mut self, struct_name: &Id, name: &Id, span: Span, fields: Vec<Id>) {
        let help = match closest_match(name, fields.iter().map(|f| f.as_str())) {
            Some(s) => format!("a field with a similar name exists: '{s}'"),
            None if fields.is_empty() => format!("'{struct_name}' has no fields"),
            None => format!("its fields are: {}", fields.join(", ")),
        };
        self.error(
            TypeError::new(
                format!("struct '{struct_name}' has no field '{name}'"),
                span,
            )
            .help(help),
        );
    }

    /// `Name { field: value, ..base }`
    pub(super) fn struct_lit(
        &mut self,
        name: &Id,
        fields: &[FieldInit],
        base: Option<&Expr>,
        span: Span,
    ) -> Ty {
        let name_span = token_span(span, name);
        let Some(info) = self.structs.get(name).cloned() else {
            for f in fields {
                self.expr(&f.value);
            }
            if let Some(b) = base {
                self.expr(b);
            }
            self.not_a_struct(name, name_span);
            return Ty::Error;
        };

        let (args, map) = self.fresh_args(&info.params);
        let mut seen = HashSet::new();
        for f in fields {
            let found = self.expr(&f.value);
            match info.fields.iter().position(|(n, _)| *n == f.name) {
                None => {
                    let help = self.field_help(name);
                    self.no_field(name, &f.name, f.span, help);
                }
                Some(i) if !seen.insert(i) => {
                    self.error(TypeError::new(
                        format!("field '{}' is given more than once", f.name),
                        f.span,
                    ));
                }
                Some(i) => {
                    let expected = Self::subst(&info.fields[i].1, &map);
                    self.expect(&expected, &found, f.value.span, |e, fd| {
                        format!(
                            "mismatched types: field '{}' of '{name}' is {e}, but was given {fd}",
                            f.name
                        )
                    });
                }
            }
        }

        let this = Ty::Named(name.clone(), args);
        match base {
            Some(b) => {
                let t = self.expr(b);
                self.expect(&this, &t, b.span, |e, f| {
                    format!("mismatched types: the value after '..' must be a {e}, found {f}")
                });
            }
            None => {
                let missing: Vec<String> = info
                    .fields
                    .iter()
                    .enumerate()
                    .filter(|(i, _)| !seen.contains(i))
                    .map(|(_, (n, _))| format!("'{n}'"))
                    .collect();
                if !missing.is_empty() {
                    let what = if missing.len() == 1 {
                        "field"
                    } else {
                        "fields"
                    };
                    self.error(
                        TypeError::new(
                            format!("missing {what} {} in '{name}'", join_and(&missing)),
                            name_span,
                        )
                        .help("give every field a value, or copy the rest from another value with `..other`"),
                    );
                }
            }
        }
        this
    }

    fn not_a_struct(&mut self, name: &Id, span: Span) {
        let err = if let Some(info) = self.enums.get(name) {
            let example = info
                .variants
                .first()
                .map_or(String::new(), |(v, _)| format!(", e.g. `{name}::{v}`"));
            TypeError::new(format!("'{name}' is an enum, not a struct"), span)
                .help(format!("make a value with one of its variants{example}"))
        } else {
            let help = match closest_match(name, self.structs.keys().map(|n| n.as_str())) {
                Some(s) => format!("a struct with a similar name exists: '{s}'"),
                None => format!("define it with `struct {name} {{ ... }}`"),
            };
            TypeError::new(format!("cannot find struct '{name}'"), span).help(help)
        };
        self.error(err);
    }

    /// The index of variant `Enum::Variant` and the types of the values it
    /// holds, for a new use of the enum with fresh type arguments, which make
    /// up the returned type. Reports it if there is no such variant.
    fn lookup_variant(
        &mut self,
        enum_name: &Id,
        variant: &Id,
        span: Span,
    ) -> Option<(usize, Vec<Ty>, Ty)> {
        let Some(info) = self.enums.get(enum_name) else {
            let err = if self.structs.contains_key(enum_name) {
                TypeError::new(format!("'{enum_name}' is a struct, not an enum"), span)
                    .help(format!("make one with `{enum_name} {{ ... }}`"))
            } else {
                let help = match closest_match(enum_name, self.enums.keys().map(|n| n.as_str())) {
                    Some(s) => format!("an enum with a similar name exists: '{s}'"),
                    None => format!("define it with `enum {enum_name} {{ ... }}`"),
                };
                TypeError::new(format!("cannot find enum '{enum_name}'"), span).help(help)
            };
            self.error(err);
            return None;
        };
        match info.variants.iter().position(|(v, _)| v == variant) {
            Some(i) => {
                let (params, tys) = (info.params.clone(), info.variants[i].1.clone());
                let (args, map) = self.fresh_args(&params);
                let tys = tys.iter().map(|t| Self::subst(t, &map)).collect();
                Some((i, tys, Ty::Named(enum_name.clone(), args)))
            }
            None => {
                let names: Vec<&str> = info.variants.iter().map(|(v, _)| v.as_str()).collect();
                let help = match closest_match(variant, names.iter().copied()) {
                    Some(s) => format!("a variant with a similar name exists: '{enum_name}::{s}'"),
                    None if names.is_empty() => format!("'{enum_name}' has no variants"),
                    None => format!("its variants are: {}", names.join(", ")),
                };
                self.error(
                    TypeError::new(
                        format!("enum '{enum_name}' has no variant '{variant}'"),
                        span,
                    )
                    .help(help),
                );
                None
            }
        }
    }

    /// `Enum::Variant`, not called.
    pub(super) fn variant_value(&mut self, enum_name: &Id, variant: &Id, span: Span) -> Ty {
        let Some((_, tys, this)) = self.lookup_variant(enum_name, variant, span) else {
            return Ty::Error;
        };
        if !tys.is_empty() {
            let blanks = vec!["..."; tys.len()].join(", ");
            let name = variant_display(enum_name, variant);
            self.error(
                TypeError::new(
                    format!(
                        "variant '{name}' holds {}, so it must be given them",
                        plural(tys.len(), "value")
                    ),
                    span,
                )
                .help(format!("construct it with `{name}({blanks})`")),
            );
        }
        this
    }

    /// `Enum::Variant(args)`
    pub(super) fn construct_variant(
        &mut self,
        enum_name: &Id,
        variant: &Id,
        args: &[Expr],
        callee: Span,
        span: Span,
    ) -> Ty {
        let Some((_, tys, this)) = self.lookup_variant(enum_name, variant, callee) else {
            for a in args {
                self.expr(a);
            }
            return Ty::Error;
        };
        let name = variant_display(enum_name, variant);
        if tys.len() != args.len() || tys.is_empty() {
            let err = if tys.is_empty() {
                TypeError::new(
                    format!("variant '{name}' holds no values, so it can't be called"),
                    span,
                )
                .help(format!("write `{name}` without parentheses"))
            } else {
                TypeError::new(
                    format!(
                        "variant '{name}' holds {} but {} {} given",
                        plural(tys.len(), "value"),
                        args.len(),
                        if args.len() == 1 { "was" } else { "were" },
                    ),
                    span,
                )
            };
            self.error(err);
            for a in args {
                self.expr(a);
            }
            return this;
        }
        for (i, (arg, t)) in args.iter().zip(&tys).enumerate() {
            let found = self.expr(arg);
            self.expect(t, &found, arg.span, |e, f| {
                format!(
                    "mismatched types: value {} of '{name}' is {e}, found {f}",
                    i + 1
                )
            });
        }
        this
    }

    // ---- Match -----------------------------------------------------------------

    pub(super) fn match_(&mut self, scrutinee: &Expr, arms: &[Arm]) -> Ty {
        let t = self.expr(scrutinee);
        let mut patterns_ok = true;
        let mut rows = Vec::new();
        let mut result: Option<Ty> = None;

        for arm in arms {
            self.scopes.push(HashMap::new());
            let before = self.errors.len();
            let pat = self.pattern(&arm.pattern, &t, &mut HashSet::new());
            patterns_ok &= self.errors.len() == before;
            if let Some(guard) = &arm.guard {
                let g = self.expr(guard);
                self.expect(&Ty::Bool, &g, guard.span, |_, f| {
                    format!("a match guard must be a bool, found {f}")
                });
            }
            let found = self.stmt(&arm.body);
            self.scopes.pop();

            if self.resolve(&found) != Ty::Never {
                match &result {
                    None => result = Some(found),
                    Some(expected) => {
                        let at = value_span(&arm.body).unwrap_or(arm.pattern.span);
                        self.expect(&expected.clone(), &found, at, |e, f| {
                            format!("match arms have incompatible types: {e} and {f}")
                        });
                    }
                }
            }
            rows.push(Row {
                pat,
                guarded: arm.guard.is_some(),
                span: arm.pattern.span,
            });
        }

        if patterns_ok {
            self.check_exhaustive(&t, &rows, scrutinee.span);
        }
        // A match whose arms all diverge (or that has none) diverges.
        result.unwrap_or(Ty::Never)
    }

    /// Check a pattern against the type of the value it matches, defining its
    /// bindings in the current scope. `names` are the names bound so far.
    fn pattern(&mut self, p: &Pattern, expected: &Ty, names: &mut HashSet<Id>) -> Pat {
        match &p.kind {
            PatternKind::Wildcard => Pat::Wild,
            PatternKind::Binding(var) => {
                if let Ty::Named(n, _) = self.resolve(expected) {
                    let is_variant = self
                        .enums
                        .get(&n)
                        .is_some_and(|e| e.variants.iter().any(|(v, _)| *v == var.name));
                    if is_variant {
                        self.error(
                            TypeError::new(
                                format!("this pattern binds a new variable '{}', which matches anything", var.name),
                                p.span,
                            )
                            .help(format!("to match the variant, write `{n}::{}`", var.name)),
                        );
                    }
                }
                if !names.insert(var.name.clone()) {
                    self.error(TypeError::new(
                        format!("'{}' is bound more than once in this pattern", var.name),
                        p.span,
                    ));
                }
                self.define(var.name.clone(), expected.clone(), BindingKind::Pattern);
                Pat::Wild
            }
            PatternKind::Int(i) => {
                self.pattern_type(&Ty::Int, expected, p.span);
                Pat::Ctor(Ctor::Int(*i), vec![])
            }
            PatternKind::Str(s) => {
                self.pattern_type(&Ty::Str, expected, p.span);
                Pat::Ctor(Ctor::Str(s.clone()), vec![])
            }
            PatternKind::Bool(b) => {
                self.pattern_type(&Ty::Bool, expected, p.span);
                Pat::Ctor(Ctor::Bool(*b), vec![])
            }
            PatternKind::Variant {
                enum_name,
                variant,
                fields,
                parens,
            } => {
                let Some((index, tys, this)) = self.lookup_variant(enum_name, variant, p.span)
                else {
                    self.bind_unchecked(fields.iter(), names);
                    return Pat::Wild;
                };
                self.pattern_type(&this, expected, p.span);
                if fields.len() != tys.len() {
                    let name = variant_display(enum_name, variant);
                    let err = if !parens {
                        let blanks = vec!["_"; tys.len()].join(", ");
                        TypeError::new(
                            format!("variant '{name}' holds {}", plural(tys.len(), "value")),
                            p.span,
                        )
                        .help(format!("match its values too: `{name}({blanks})`"))
                    } else if tys.is_empty() {
                        TypeError::new(format!("variant '{name}' holds no values"), p.span)
                            .help(format!("write `{name}` without parentheses"))
                    } else {
                        TypeError::new(
                            format!(
                                "this pattern has {} but variant '{name}' holds {}",
                                plural(fields.len(), "field"),
                                plural(tys.len(), "value"),
                            ),
                            p.span,
                        )
                    };
                    self.error(err);
                    self.bind_unchecked(fields.iter(), names);
                    return Pat::Wild;
                }
                let subs = fields
                    .iter()
                    .zip(&tys)
                    .map(|(f, t)| self.pattern(f, t, names))
                    .collect();
                Pat::Ctor(Ctor::Variant(index), subs)
            }
            PatternKind::Struct { name, fields, rest } => {
                let Some(info) = self.structs.get(name).cloned() else {
                    self.not_a_struct(name, token_span(p.span, name));
                    self.bind_unchecked(fields.iter().map(|(_, _, f)| f), names);
                    return Pat::Wild;
                };
                let (args, map) = self.fresh_args(&info.params);
                self.pattern_type(&Ty::Named(name.clone(), args), expected, p.span);
                let mut subs = vec![Pat::Wild; info.fields.len()];
                let mut seen = HashSet::new();
                for (field, span, sub) in fields {
                    match info.fields.iter().position(|(n, _)| n == field) {
                        None => {
                            let help = self.field_help(name);
                            self.no_field(name, field, *span, help);
                            self.bind_unchecked(std::iter::once(sub), names);
                        }
                        Some(i) => {
                            if !seen.insert(i) {
                                self.error(TypeError::new(
                                    format!(
                                        "field '{field}' appears more than once in this pattern"
                                    ),
                                    *span,
                                ));
                            }
                            let t = Self::subst(&info.fields[i].1, &map);
                            subs[i] = self.pattern(sub, &t, names);
                        }
                    }
                }
                let missing: Vec<String> = info
                    .fields
                    .iter()
                    .enumerate()
                    .filter(|(i, _)| !seen.contains(i))
                    .map(|(_, (n, _))| format!("'{n}'"))
                    .collect();
                if !rest && !missing.is_empty() {
                    let what = if missing.len() == 1 {
                        "field"
                    } else {
                        "fields"
                    };
                    self.error(
                        TypeError::new(
                            format!("pattern does not mention {what} {}", join_and(&missing)),
                            p.span,
                        )
                        .help("list every field, or end the pattern with `..` to ignore the rest"),
                    );
                }
                Pat::Ctor(Ctor::Struct, subs)
            }
        }
    }

    /// Define the bindings in patterns that couldn't be checked, so their
    /// uses don't cause more errors.
    fn bind_unchecked<'p>(
        &mut self,
        patterns: impl Iterator<Item = &'p Pattern>,
        names: &mut HashSet<Id>,
    ) {
        for p in patterns {
            self.pattern(p, &Ty::Error, names);
        }
    }

    /// Require a pattern of type `pat` to match a value of type `expected`.
    fn pattern_type(&mut self, pat: &Ty, expected: &Ty, span: Span) {
        self.expect(expected, pat, span, |e, f| {
            format!("mismatched types: this pattern matches {f}, but the value is {e}")
        });
    }

    // ---- Exhaustiveness --------------------------------------------------------

    fn check_exhaustive(&mut self, ty: &Ty, rows: &[Row], span: Span) {
        let ty = self.zonk(ty);
        if matches!(ty, Ty::Error | Ty::Never) {
            return;
        }
        let tys = [ty.clone()];

        let mut matrix: Vec<Vec<Pat>> = Vec::new();
        for row in rows {
            if self.useful(&matrix, &[row.pat.clone()], &tys).is_none() {
                self.error(
                    TypeError::new("unreachable pattern", row.span)
                        .help("remove this arm")
                        .note("the arms before it already match every value this one does"),
                );
            }
            // A guarded arm might not match, so it doesn't cover anything.
            if !row.guarded {
                matrix.push(vec![row.pat.clone()]);
            }
        }

        // Find up to a few values no arm matches, adding each one to the
        // matrix to find the next.
        const SHOWN: usize = 3;
        let mut missing = Vec::new();
        let mut guard_matters = false;
        while missing.len() <= SHOWN {
            let Some(w) = self.useful(&matrix, &[Pat::Wild], &tys) else {
                break;
            };
            // Would a guarded arm's pattern have matched this value?
            guard_matters |= rows
                .iter()
                .any(|r| r.guarded && self.useful(&[vec![r.pat.clone()]], &w, &tys).is_none());
            let w = w.into_iter().next().unwrap();
            missing.push(self.show_pat(&w, &ty));
            matrix.push(vec![w]);
        }
        if missing.is_empty() {
            return;
        }

        let more = missing.len() > SHOWN;
        missing.truncate(SHOWN);
        let quoted: Vec<String> = missing.iter().map(|m| format!("`{m}`")).collect();
        let list = if more {
            format!("{} and more", quoted.join(", "))
        } else {
            join_and(&quoted)
        };
        let help = if missing.len() == 1 && !more {
            format!("add an arm for it: `{} => ...`", missing[0])
        } else {
            "add arms for the missing cases, or a `_ => ...` arm to match everything else"
                .to_string()
        };
        let mut err = TypeError::new(format!("non-exhaustive match: {list} not covered"), span)
            .help(help)
            .note(format!("the value matched on has type {}", self.show(&ty)));
        if guard_matters {
            err = err.note("arms with an `if` guard don't count toward covering every case");
        }
        self.error(err);
    }

    /// Whether some value of types `tys` matches the patterns `q` but no
    /// row of `rows`; if so, such a value, as one pattern per column.
    fn useful(&self, rows: &[Vec<Pat>], q: &[Pat], tys: &[Ty]) -> Option<Vec<Pat>> {
        let Some((head, rest)) = q.split_first() else {
            return rows.is_empty().then(Vec::new);
        };
        let ty = &tys[0];
        match head {
            Pat::Ctor(c, args) => {
                let fields = self.ctor_fields(ty, c);
                let q: Vec<Pat> = args.iter().chain(rest).cloned().collect();
                let w = self.useful(
                    &specialize(rows, c, args.len()),
                    &q,
                    &concat(&fields, &tys[1..]),
                )?;
                Some(rebuild(c, args.len(), w))
            }
            Pat::Wild => {
                let heads: Vec<&Ctor> = rows
                    .iter()
                    .filter_map(|r| match &r[0] {
                        Pat::Ctor(c, _) => Some(c),
                        Pat::Wild => None,
                    })
                    .collect();
                let all = self.ctors(ty);
                match &all {
                    // Every constructor of the type appears: `_` is useful
                    // exactly if it is for one of them.
                    Some(all) if all.iter().all(|c| heads.contains(&c)) => {
                        all.iter().find_map(|c| {
                            let fields = self.ctor_fields(ty, c);
                            let q: Vec<Pat> = vec![Pat::Wild; fields.len()]
                                .into_iter()
                                .chain(rest.iter().cloned())
                                .collect();
                            let w = self.useful(
                                &specialize(rows, c, fields.len()),
                                &q,
                                &concat(&fields, &tys[1..]),
                            )?;
                            Some(rebuild(c, fields.len(), w))
                        })
                    }
                    // Some constructor doesn't appear, so only the rows that
                    // start with `_` can stop a value built with it.
                    _ => {
                        let default: Vec<Vec<Pat>> = rows
                            .iter()
                            .filter(|r| r[0] == Pat::Wild)
                            .map(|r| r[1..].to_vec())
                            .collect();
                        let mut w = self.useful(&default, rest, &tys[1..])?;
                        let witness = match &all {
                            Some(all) if !heads.is_empty() => {
                                let c = all.iter().find(|c| !heads.contains(c)).unwrap();
                                Pat::Ctor(c.clone(), vec![Pat::Wild; self.ctor_fields(ty, c).len()])
                            }
                            _ => Pat::Wild,
                        };
                        w.insert(0, witness);
                        Some(w)
                    }
                }
            }
        }
    }

    /// Every constructor of `ty`, or `None` if there are too many to list
    /// (ints, strings).
    fn ctors(&self, ty: &Ty) -> Option<Vec<Ctor>> {
        match ty {
            Ty::Named(n, _) => match self.enums.get(n) {
                Some(e) => Some((0..e.variants.len()).map(Ctor::Variant).collect()),
                None => Some(vec![Ctor::Struct]),
            },
            Ty::Bool => Some(vec![Ctor::Bool(false), Ctor::Bool(true)]),
            _ => None,
        }
    }

    /// The types of the values a constructor of `ty` holds.
    fn ctor_fields(&self, ty: &Ty, c: &Ctor) -> Vec<Ty> {
        let (params, tys): (&[Id], Vec<&Ty>) = match (ty, c) {
            (Ty::Named(n, _), Ctor::Variant(i)) => (
                &self.enums[n].params,
                self.enums[n].variants[*i].1.iter().collect(),
            ),
            (Ty::Named(n, _), Ctor::Struct) => (
                &self.structs[n].params,
                self.structs[n].fields.iter().map(|(_, t)| t).collect(),
            ),
            _ => return Vec::new(),
        };
        let Ty::Named(_, args) = ty else {
            unreachable!()
        };
        let map = params
            .iter()
            .cloned()
            .zip(args.iter().map(|a| self.zonk(a)))
            .collect();
        tys.into_iter().map(|t| Self::subst(t, &map)).collect()
    }

    /// A pattern of type `ty`, as it would be written in Aja.
    fn show_pat(&self, p: &Pat, ty: &Ty) -> String {
        let Pat::Ctor(c, args) = p else {
            return "_".to_string();
        };
        let fields = self.ctor_fields(ty, c);
        let args: Vec<String> = args
            .iter()
            .zip(&fields)
            .map(|(a, t)| self.show_pat(a, t))
            .collect();
        match (ty, c) {
            (Ty::Named(n, _), Ctor::Variant(i)) => {
                let v = variant_display(n, &self.enums[n].variants[*i].0);
                if args.is_empty() {
                    v
                } else {
                    format!("{v}({})", args.join(", "))
                }
            }
            (Ty::Named(n, _), Ctor::Struct) => {
                let names = self.structs[n].fields.iter().map(|(f, _)| f);
                let shown: Vec<String> = names
                    .zip(&args)
                    .filter(|(_, a)| *a != "_")
                    .map(|(f, a)| format!("{f}: {a}"))
                    .collect();
                if shown.is_empty() {
                    format!("{n} {{ .. }}")
                } else if shown.len() == args.len() {
                    format!("{n} {{ {} }}", shown.join(", "))
                } else {
                    format!("{n} {{ {}, .. }}", shown.join(", "))
                }
            }
            (_, Ctor::Bool(b)) => b.to_string(),
            (_, Ctor::Int(i)) => i.to_string(),
            (_, Ctor::Str(s)) => format!("{s:?}"),
            _ => "_".to_string(),
        }
    }
}

/// The rows of `rows` that a value built with `c` (holding `arity` values)
/// could match, with their first pattern replaced by the patterns for those
/// values.
fn specialize(rows: &[Vec<Pat>], c: &Ctor, arity: usize) -> Vec<Vec<Pat>> {
    rows.iter()
        .filter_map(|r| {
            let head: Vec<Pat> = match &r[0] {
                Pat::Ctor(c2, args) if c2 == c => args.clone(),
                Pat::Ctor(..) => return None,
                Pat::Wild => vec![Pat::Wild; arity],
            };
            Some(head.into_iter().chain(r[1..].iter().cloned()).collect())
        })
        .collect()
}

/// Undo [`specialize`] on a witness: fold its first `arity` patterns back
/// into one built with `c`.
fn rebuild(c: &Ctor, arity: usize, mut w: Vec<Pat>) -> Vec<Pat> {
    let rest = w.split_off(arity);
    std::iter::once(Pat::Ctor(c.clone(), w))
        .chain(rest)
        .collect()
}

fn concat(a: &[Ty], b: &[Ty]) -> Vec<Ty> {
    a.iter().chain(b).cloned().collect()
}

/// "a", "a and b", "a, b and c".
fn join_and(items: &[String]) -> String {
    match items {
        [] => String::new(),
        [one] => one.clone(),
        [init @ .., last] => format!("{} and {last}", init.join(", ")),
    }
}

/// The span of the name at the start of `span`, which begins with the
/// single token `name`.
fn token_span(span: Span, name: &str) -> Span {
    let mut end = span.start;
    let n = name.chars().count() as u32;
    end.col += n;
    end.pos += n;
    Span::new(span.start, end)
}

/// The span of the value a statement produces: a block's final expression,
/// or the expression itself.
fn value_span(stmt: &Statement) -> Option<Span> {
    match stmt {
        Statement::Block(_, Some(tail)) => Some(tail.span),
        Statement::Expr(e) => Some(e.span),
        _ => None,
    }
}
