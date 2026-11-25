use color_eyre::eyre::{bail, Result};
use ecow::EcoString;
use std::fmt::Display;
use types::Type;

use crate::token::Token;

pub mod types;

pub use aja_diagnostic::source::{Id, Location, Source, Span};

/// An expression together with where it appears in the source.
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: Expression,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: Expression, span: Span) -> Self {
        Expr { kind, span }
    }

    /// Whether this is a closure literal. A `let` or `var` whose value is
    /// one is in scope inside the closure's body, so the closure can call
    /// itself.
    pub fn is_closure(&self) -> bool {
        matches!(self.kind, Expression::Closure(..))
    }
}

/// A binding introduced by a parameter, `let`, or `var`, with its optional annotation.
#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Id,
    /// The annotated type, or `None` when the type is left to inference.
    pub t: Option<Type>,
    /// Where the name appears.
    pub span: Span,
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.t {
            Some(t) => write!(f, "{}: {}", self.name, t),
            None => write!(f, "{}", self.name),
        }
    }
}

impl Variable {
    pub fn new(name: Id, t: Option<Type>, span: Span) -> Self {
        Self { name, t, span }
    }
}

/// A type parameter of a generic function, struct, enum, or impl: the `T`
/// in `fn id<T>(x: T) -> T`, or the `K` in `struct Map<K: Hash, V>`.
#[derive(Debug, Clone)]
pub struct TypeParam {
    pub name: Id,
    pub span: Span,
    /// The traits its bounds name (`Hash` in `K: Hash`), with where each
    /// appears. The checker decides whether they are traits.
    pub bounds: Vec<(Id, Span)>,
}

/// A file: its imports, then its definitions.
#[derive(Debug, Clone, Default)]
pub struct Program {
    pub imports: Vec<Import>,
    pub definitions: Vec<Definition>,
}

impl Program {
    /// A program with no imports.
    pub fn new(definitions: Vec<Definition>) -> Self {
        Program {
            imports: Vec::new(),
            definitions,
        }
    }
}

/// `import geo::shapes;`, which makes `shapes::area` available, or
/// `import geo::shapes::{Shape, area};`, which brings in those names
/// directly. A path without braces names a module if there is one, and
/// otherwise an item of its parent module: `import geo::shapes::area;`,
/// which the loader rewrites to `import geo::shapes::{area};`.
#[derive(Debug, Clone)]
pub struct Import {
    /// The module's path, `geo::shapes`, with where each segment appears.
    pub path: Vec<(Id, Span)>,
    /// The names in braces, if there are any.
    pub items: Option<Vec<(Id, Span)>>,
}

impl Import {
    /// Where the module's path appears.
    pub fn path_span(&self) -> Span {
        let first = self.path.first().expect("an import names a module").1;
        let last = self.path.last().expect("an import names a module").1;
        first.to(last)
    }
}

#[derive(Debug, Clone)]
pub enum Definition {
    /// A function, which is `public` when marked `pub`; so are the others.
    /// A `native` function has no body (`body` is an empty block): each
    /// backend implements it. Only the prelude and the standard library
    /// can declare one.
    Function {
        public: bool,
        native: bool,
        name: Id,
        /// Where the function's name appears.
        name_span: Span,
        type_params: Vec<TypeParam>,
        params: Vec<Variable>,
        body: Statement,
        /// The declared return type; `None` means `void`.
        rtype: Option<Type>,
    },
    /// `struct Point { x: int, y: int }`, or `struct Pair<A, B> { ... }`.
    /// Every field's `t` is `Some`.
    /// A `native` struct (`native struct List<T>;`) has no fields: its
    /// values are made and used only by native functions, and each backend
    /// decides how they are represented.
    Struct {
        public: bool,
        native: bool,
        name: Id,
        name_span: Span,
        type_params: Vec<TypeParam>,
        fields: Vec<FieldDef>,
    },
    /// `enum Shape { Circle(float), Rect(float, float), Empty }`, or
    /// `enum Option<T> { Some(T), None }`.
    Enum {
        public: bool,
        name: Id,
        name_span: Span,
        type_params: Vec<TypeParam>,
        variants: Vec<VariantDef>,
    },
    /// `impl Point { fn norm(self) -> int { ... } }`, or
    /// `impl<T> Option<T> { ... }`: methods of the struct or enum `target`
    /// names, which must give exactly the impl's type parameters as its
    /// arguments.
    Impl {
        type_params: Vec<TypeParam>,
        target: Type,
        methods: Vec<Method>,
    },
}

/// A function in an `impl` block. A method takes `self`, its first
/// parameter (named `self`, with no annotation); an associated function,
/// called as `Point::origin()`, doesn't.
#[derive(Debug, Clone)]
pub struct Method {
    /// Whether it is marked `pub`, so code in other modules can call it.
    pub public: bool,
    /// Whether it is `native`, with no body (see [`Definition::Function`]).
    pub native: bool,
    pub name: Id,
    pub name_span: Span,
    pub type_params: Vec<TypeParam>,
    /// Every parameter, starting with `self` for a method.
    pub params: Vec<Variable>,
    pub has_self: bool,
    pub body: Statement,
    /// The declared return type; `None` means `void`.
    pub rtype: Option<Type>,
}

/// A field of a struct definition, which code in other modules can use
/// only when it is `public` (marked `pub`).
#[derive(Debug, Clone)]
pub struct FieldDef {
    pub public: bool,
    pub name: Id,
    pub t: Type,
    pub span: Span,
}

/// One variant of an enum definition, with the types of the values it holds.
#[derive(Debug, Clone)]
pub struct VariantDef {
    pub name: Id,
    pub span: Span,
    pub fields: Vec<Type>,
}

/// `name: value` in a struct literal. Shorthand `name` means `name: name`.
#[derive(Debug, Clone)]
pub struct FieldInit {
    pub name: Id,
    /// Where the field's name appears.
    pub span: Span,
    pub value: Expr,
}

/// One arm of a `match`: `pattern if guard => body`.
#[derive(Debug, Clone)]
pub struct Arm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    /// A block, or a `Statement::Expr`.
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    /// `_`
    Wildcard,
    /// A name, which matches anything and binds it.
    Binding(Variable),
    Int(i64),
    Str(EcoString),
    Bool(bool),
    /// `void`, which matches the one value of type `void`.
    Void,
    /// `Shape::Circle(r)`, or `Shape::Empty` (with `fields` empty and
    /// `parens` false).
    Variant {
        enum_name: Id,
        variant: Id,
        fields: Vec<Pattern>,
        parens: bool,
    },
    /// `Point { x, y: 0 }`, with `rest` set when it ends in `..`.
    Struct {
        name: Id,
        /// Where the struct's name (or path) appears.
        name_span: Span,
        fields: Vec<(Id, Span, Pattern)>,
        rest: bool,
    },
    /// `(a, b)`: a tuple with as many values as there are patterns (two
    /// or more), each matching its pattern.
    Tuple(Vec<Pattern>),
    /// `a | b | c`: matches a value any of the alternatives does. Each
    /// binds the same names, to values of the same types.
    Or(Vec<Pattern>),
    /// A pattern whose error has already been reported (by the module
    /// resolver), holding the patterns inside it, whose names it binds.
    /// It is treated like `_`.
    Error(Vec<Pattern>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Statement>, Option<Expr>),
    Expr(Expr),
    /// `let x = value;`, or `var x = value;` when `mutable`. Only a `var`
    /// can be assigned to. `x` is in scope after the statement, and also
    /// inside `value` when it is a closure (see [`Expr::is_closure`]).
    Let {
        var: Variable,
        value: Expr,
        mutable: bool,
    },
    /// `let (a, b) = value;`, or `let Point { x, y }: Point = value;`: a
    /// `let` whose pattern is more than a name. The pattern must match
    /// every value of its type, and the names it binds are immutable.
    /// They are in scope after the statement.
    LetPattern {
        pattern: Pattern,
        t: Option<Type>,
        value: Expr,
    },
    If(Expr, Box<Statement>),
    While(Expr, Box<Statement>),
    /// `for pattern in iter { body }`: `body` once for each value of `iter`,
    /// an `Iter<T>` or a value whose type has a method
    /// `iter(self) -> Iter<T>`, matched against `pattern`, which must match
    /// every value (usually a name, or a tuple such as `(k, v)`). The names
    /// it binds are immutable, and new for each value.
    For {
        pattern: Pattern,
        iter: Expr,
        body: Box<Statement>,
    },
    /// `break`, which leaves the innermost loop, at the given span.
    Break(Span),
    /// `continue`, which starts the innermost loop's next iteration.
    Continue(Span),
    Return(Option<Expr>),
    /// `defer body;` (or `defer { ... }`), where `span` is the `defer`
    /// keyword: `body` runs when the block the `defer` is in is left, by
    /// any path but a runtime error, after the code that follows it and
    /// before the defers above it. Its value is thrown away, and it can't
    /// leave by itself: no `return`, `?`, or `break` and `continue` that
    /// reach outside it.
    Defer(Box<Statement>, Span),
}

/// A piece of a template string.
#[derive(Debug, Clone)]
pub enum TemplatePart {
    Text(EcoString),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Int(i64),
    Float(f64),
    String(EcoString),
    True,
    False,
    /// `void`, the one value of type `void`.
    Void,
    Unary(Op1, Box<Expr>),
    BinOp(Box<Expr>, Op2, Box<Expr>),
    Grouping(Box<Expr>),
    If(Box<Expr>, Box<Statement>, Box<Statement>),
    Var(Id),
    Call(Box<Expr>, Vec<Expr>),
    Assign(Id, Box<Expr>),
    /// Parameters, optional return type (inferred when absent), and body.
    Closure(Vec<Variable>, Option<Type>, Box<Statement>),
    /// `value.field`, with where the field's name appears. A tuple's
    /// values are fields named by their position: `pair.0`.
    Field(Box<Expr>, Id, Span),
    /// `Enum::Variant`, `Type::function`, or `module::item`: everything
    /// before the last `::` (which may itself hold `::`, as in
    /// `shapes::Shape::Circle`), and the last name. Called like a function
    /// when the variant holds values. Once modules are resolved, the first
    /// is always a type and the second its variant or associated function.
    Path(Id, Id),
    /// `Point { x: 1, y: 2 }`, or `Point { x: 1, ..base }`.
    StructLit {
        name: Id,
        /// Where the struct's name (or path) appears.
        name_span: Span,
        fields: Vec<FieldInit>,
        base: Option<Box<Expr>>,
    },
    Match(Box<Expr>, Vec<Arm>),
    /// `value?`: the value inside an `Ok` or `Some`, or else return the
    /// `Err` or `None` from the enclosing function.
    Try(Box<Expr>),
    /// `[a, b, c]`: a `List`.
    List(Vec<Expr>),
    /// `(a, b)`: a tuple of two or more values.
    Tuple(Vec<Expr>),
    /// `list[index]`, which stops the program if `index` is out of range.
    Index(Box<Expr>, Box<Expr>),
    /// A string with expressions in it, `"x is {x}"`: its text and its
    /// expressions in order, each expression's value turned into a string.
    Template(Vec<TemplatePart>),
    /// An expression whose error has already been reported (by the module
    /// resolver), holding the expressions inside it that can still be
    /// checked. The checker gives it a type that fits anywhere; it never
    /// reaches a backend.
    Error(Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum Op1 {
    Not,
    Negate,
}

impl TryFrom<Token> for Op1 {
    type Error = color_eyre::eyre::Error;
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Bang => Ok(Self::Not),
            Token::Minus => Ok(Self::Negate),
            _ => bail!("Token {:?} is not a valid op1", value),
        }
    }
}

/// Binary operation
#[derive(Debug, Clone)]
pub enum Op2 {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

impl TryFrom<Token> for Op2 {
    type Error = color_eyre::eyre::Error;
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Minus => Ok(Self::Subtraction),
            Token::Plus => Ok(Self::Addition),
            Token::Mult => Ok(Self::Multiplication),
            Token::Div => Ok(Self::Division),
            Token::Modulo => Ok(Self::Modulo),
            Token::EqualEqual => Ok(Self::Equal),
            Token::BangEqual => Ok(Self::NotEqual),
            Token::GreaterThan => Ok(Self::GreaterThan),
            Token::GreaterEqual => Ok(Self::GreaterEqual),
            Token::LessThan => Ok(Self::LessThan),
            Token::LessEqual => Ok(Self::LessEqual),
            Token::And => Ok(Self::And),
            Token::Or => Ok(Self::Or),
            _ => bail!("Token {:?} is not a valid op2", value),
        }
    }
}

impl Display for Op2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sym = match self {
            Op2::Addition => "+",
            Op2::Subtraction => "-",
            Op2::Multiplication => "*",
            Op2::Division => "/",
            Op2::Modulo => "%",
            Op2::LessThan => "<",
            Op2::LessEqual => "<=",
            Op2::GreaterThan => ">",
            Op2::GreaterEqual => ">=",
            Op2::Equal => "==",
            Op2::NotEqual => "!=",
            Op2::And => "&&",
            Op2::Or => "||",
        };
        write!(f, "{sym}")
    }
}
