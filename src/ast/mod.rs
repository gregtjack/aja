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
    /// Which source the location is in: [`Source::Program`] for the program
    /// being run, or [`Source::Prelude`].
    pub source: Source,
}

/// A source of code, so that a location in the prelude isn't mistaken for
/// one in the program.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Source {
    #[default]
    Program,
    Prelude,
}

impl Location {
    pub fn new(pos: u32, line: u32, col: u32) -> Self {
        Location { line, col, pos, source: Source::Program }
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

#[derive(Debug, Clone)]
pub struct Program {
    pub definitions: Vec<Definition>,
}

#[derive(Debug, Clone)]
pub enum Definition {
    Function {
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
        name: Id,
        name_span: Span,
        type_params: Vec<TypeParam>,
        fields: Vec<Variable>,
    },
    /// `enum Shape { Circle(float), Rect(float, float), Empty }`, or
    /// `enum Option<T> { Some(T), None }`.
    Enum {
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
        fields: Vec<(Id, Span, Pattern)>,
        rest: bool,
    },
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
    /// `Enum::Variant`. Called like a function when the variant holds values.
    Path(Id, Id),
    /// `Point { x: 1, y: 2 }`, or `Point { x: 1, ..base }`.
    StructLit {
        name: Id,
        fields: Vec<FieldInit>,
        base: Option<Box<Expr>>,
    },
    Match(Box<Expr>, Vec<Arm>),
    /// `value?`: the value inside an `Ok` or `Some`, or else return the
    /// `Err` or `None` from the enclosing function.
    Try(Box<Expr>),
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
