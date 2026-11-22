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
}

impl Location {
    pub fn new(pos: u32, line: u32, col: u32) -> Self {
        Location { line, col, pos }
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
        params: Vec<Variable>,
        body: Statement,
        /// The declared return type; `None` means `void`.
        rtype: Option<Type>,
    },
    /// `struct Point { x: int, y: int }`. Every field's `t` is `Some`.
    Struct {
        name: Id,
        name_span: Span,
        fields: Vec<Variable>,
    },
    /// `enum Shape { Circle(float), Rect(float, float), Empty }`.
    Enum {
        name: Id,
        name_span: Span,
        variants: Vec<VariantDef>,
    },
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
    Int(i32),
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
    /// can be assigned to.
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
    Int(i32),
    Float(f32),
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
