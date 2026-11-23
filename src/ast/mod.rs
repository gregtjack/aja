use color_eyre::eyre::{bail, Result};
use ecow::EcoString;
use std::fmt::Display;
use types::Type;

use crate::token::Token;

pub(crate) mod types;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub line: u32,
    pub col: u32,
    pub pos: u32,
    /// Which source the location is in: one of the program's files, or the
    /// prelude.
    pub source: Source,
}

/// A source of code, so that a location in one file isn't mistaken for one
/// in another. `File(0)` is the file being run (or the REPL); the files of
/// the modules it imports come after it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Source {
    File(u16),
    Prelude,
}

impl Source {
    /// The file being run.
    pub const MAIN: Source = Source::File(0);
}

impl Default for Source {
    fn default() -> Self {
        Source::MAIN
    }
}

impl Location {
    pub fn new(pos: u32, line: u32, col: u32) -> Self {
        Location {
            line,
            col,
            pos,
            source: Source::MAIN,
        }
    }
}

/// A region of source code; `end` is exclusive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Location,
    pub end: Location,
}

impl Span {
    pub fn new(start: Location, end: Location) -> Self {
        Span { start, end }
    }

    /// The span from the start of `self` to the end of `other`.
    pub fn to(self, other: Span) -> Span {
        Span::new(self.start, other.end)
    }
}

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

pub type Id = EcoString;

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

/// A type parameter of a generic function, struct, or enum: the `T` in
/// `fn id<T>(x: T) -> T`.
#[derive(Debug, Clone)]
pub struct TypeParam {
    pub name: Id,
    pub span: Span,
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
/// directly. A path without braces always names a module.
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
    Function {
        public: bool,
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
    Struct {
        public: bool,
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
    If(Expr, Box<Statement>),
    While(Expr, Box<Statement>),
    Return(Option<Expr>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Int(i64),
    Float(f64),
    String(EcoString),
    True,
    False,
    Unary(Op1, Box<Expr>),
    BinOp(Box<Expr>, Op2, Box<Expr>),
    Grouping(Box<Expr>),
    If(Box<Expr>, Box<Statement>, Box<Statement>),
    Var(Id),
    Call(Box<Expr>, Vec<Expr>),
    Assign(Id, Box<Expr>),
    /// Parameters, optional return type (inferred when absent), and body.
    Closure(Vec<Variable>, Option<Type>, Box<Statement>),
    /// `value.field`, with where the field's name appears.
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
