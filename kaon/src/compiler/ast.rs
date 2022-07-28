//! AST (abstract-syntax tree).

use crate::common::Span;
use std::{fmt::{self, Display}};

use super::query::{Id, Query};

#[derive(Debug)]
pub struct AST {
    pub nodes: Vec<ASTNode>,
    pub query: Query,
    pub span: Span,
}

impl AST {
    pub fn new(nodes: Vec<ASTNode>, span: Span) -> Self {
        AST {
            nodes,
            span,
            query: Query::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ASTNode {
    Stmt(Stmt),
    Expr(Expr),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Fun {
    /// Function name.
    pub name: Ident,
    /// Function parameters.
    pub params: Vec<Ident>,
    /// Body of the function.
    pub body: Stmt,
    /// Function parameters' type information.
    pub params_typ: Vec<Option<Expr>>,
    /// Function return type.
    pub return_typ: Option<Expr>,
    /// Function access modifier (e.g. `public`).
    pub access: Visibility,
}

impl Fun {
    pub fn new(
        name: Ident,
        params: Vec<Ident>,
        body: Stmt,
        params_typ: Vec<Option<Expr>>,
        return_typ: Option<Expr>,
        access: Visibility,
    ) -> Self {
        Fun {
            name,
            params,
            body,
            params_typ,
            return_typ,
            access,
        }
    }

    pub fn name(&self, query: &Query) -> String {
        self.name.name_of(query)
    }
}

/// A class declaration.
#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    /// The name of the class.
    pub name: Ident,
    /// Name of parent class, if any.
    pub parent: Option<Ident>,
    /// List of fields.
    pub fields: Vec<Field>,
    /// List of methods.
    pub methods: Vec<Stmt>,
    /// List of constructors.
    pub constructors: Vec<Constructor>,
    pub visibility: Visibility,
}

impl Class {
    pub fn new(
        name: Ident,
        parent: Option<Ident>,
        fields: Vec<Field>,
        methods: Vec<Stmt>,
        constructors: Vec<Constructor>,
    ) -> Self {
        Self {
            name,
            parent,
            fields,
            methods,
            constructors,
            visibility: Visibility::Public,
        }
    }

    pub fn name(&self, query: &Query) -> String {
        self.name.name_of(query)
    }

    pub fn id(&self) -> Option<Id> {
        if let Ident::Id { id, .. } = self.name {
            Some(id)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub name: Ident,
    pub visibility: Visibility,
    pub default: Option<Expr>,
    pub typ: Option<Expr>,
    pub kind: FieldKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FieldKind {
    Var,
    // add other types of fields, e.g. `const`, later.
}

#[derive(Clone, Debug, PartialEq)]
pub struct Constructor {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub body: Stmt,
    pub class: Ident,
    pub visibility: Visibility,
}

impl Constructor {
    pub fn new(name: Ident, params: Vec<Ident>, body: Stmt, class: Ident) -> Self {
        Self {
            name,
            params,
            body,
            class,
            visibility: Visibility::Public,
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Signature {
    pub name: Ident,
    pub params: (Vec<Ident>, Vec<Option<Expr>>),
    pub return_typ: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Trait {
    pub methods: Vec<TraitMethod>,
    pub span: Span,
}

/// A `trait` method, including the function's signature and an
/// optional default block.
#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethod {
    pub sig: Signature,
    pub default: Option<Stmt>,
    pub span: Span,
}

/// A statment
#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    /// `if` expr `{` body `}` `else` `{`body `}`
    IfStatement(Expr, Box<(Stmt, Option<Stmt>)>, Span),
    /// `while` expr `{` body `}`
    WhileStatement(Expr, Box<Stmt>, Span),
    /// `loop` `{` body `}`
    LoopStatement(Box<Stmt>, Span),
    /// import statement
    ImportStatement(Expr, Span),
    /// `{` body `}`
    Block(Box<Vec<Stmt>>, Span),
    /// `var` id [`:` type] `=` expr
    VarDeclaration(Ident, Option<Expr>, Option<Expr>, Span),
    /// `con` id [`:` type] `=` expr
    ConDeclaration(Ident, Expr, Option<Expr>, Span),
    /// expr `=` expr
    AssignStatement(Expr, Expr, Span),
    /// `fun` id `(` ...args `)` `{` body `}`
    Function(Box<Fun>, Span),
    /// `class` id `{` method | field `}`
    Class(Class, Span),
    Trait(Trait),
    /// `return` expr
    Return(Option<Expr>, Span),
    /// `break`
    Break(Span),
    /// `continue`
    Continue(Span),
    /// [expression][Expr]
    Expr(Expr),
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self.clone() {
            Self::IfStatement(_, _, span) => span,
            Self::WhileStatement(_, _, span) => span,
            Self::LoopStatement(_, span) => span,
            Self::ImportStatement(_, span) => span,
            Self::Block(_, span) => span,
            Self::VarDeclaration(_, _, _, span) => span,
            Self::ConDeclaration(_, _, _, span) => span,
            Self::AssignStatement(_, _, span) => span,
            Self::Function(_, span) => span,
            Self::Class(_, span) => span,
            Self::Trait(trait_) => trait_.span,
            Self::Return(_, span) => span,
            Self::Break(span) => span,
            Self::Continue(span) => span,
            Self::Expr(expr) => expr.span(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinExpr {
    /// The binary operator.
    pub op: Op,
    /// The left-hand side of the expression.
    pub lhs: Expr,
    /// The right-hand side of the expression.
    pub rhs: Expr,
}

impl BinExpr {
    pub fn new(op: Op, lhs: Expr, rhs: Expr) -> Self {
        BinExpr { op, lhs, rhs }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    /// Addition a + b
    Add,
    /// Subtraction a - b
    Subtract,
    /// Multiplication a * b
    Multiply,
    /// Division a / b
    Divide,
    /// Remainder a % b
    Remainder,
    /// Greater-than check a > b
    GreaterThan,
    /// Greater-than or equal check a >= b
    GreaterThanEquals,
    /// Less-than check a < b
    LessThan,
    /// Less-than or equal check a <= b
    LessThanEquals,
    /// Equality check a == b
    EqualTo,
    /// Inequality check a != b
    NotEqual,
    /// Bitwise and a & b
    BitwiseAnd,
    /// Bitwise or a | b
    BitwiseOr,
    /// Bitwise xor a ^ b
    BitwiseXor,
    /// Falsy check !a
    Bang,
}

impl From<&str> for Op {
    fn from(op: &str) -> Self {
        match op {
            "+" => Op::Add,
            "-" => Op::Subtract,
            "*" => Op::Multiply,
            "/" => Op::Divide,
            "%" => Op::Remainder,
            ">" => Op::GreaterThan,
            ">=" => Op::GreaterThanEquals,
            "<" => Op::LessThan,
            "<=" => Op::LessThanEquals,
            "==" => Op::EqualTo,
            "!=" => Op::NotEqual,
            "&" => Op::BitwiseAnd,
            "|" => Op::BitwiseOr,
            "^" => Op::BitwiseXor,
            "!" => Op::Bang,
            _ => unreachable!(),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Add => f.write_str("+"),
            Op::Subtract => f.write_str("-"),
            Op::Multiply => f.write_str("*"),
            Op::Divide => f.write_str("/"),
            Op::Remainder => f.write_str("%"),
            Op::GreaterThan => f.write_str(">"),
            Op::GreaterThanEquals => f.write_str(">="),
            Op::LessThan => f.write_str("<"),
            Op::LessThanEquals => f.write_str("<="),
            Op::EqualTo => f.write_str("=="),
            Op::NotEqual => f.write_str("!="),
            Op::BitwiseAnd => f.write_str("&"),
            Op::BitwiseOr => f.write_str("|"),
            Op::BitwiseXor => f.write_str("^"),
            Op::Bang => f.write_str("!"),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Ident {
    Name { name: String, span: Span },
    Id { id: Id, span: Span },
}

impl Ident {
    pub fn span(&self) -> Span {
        match self {
            Ident::Name { span, .. } | Ident::Id { span, .. } => span.clone(),
        }
    }

    pub fn name_of(&self, query: &Query) -> String {
        match self {
            Ident::Name { name, .. } => name.to_string(),
            Ident::Id { id, .. } => query.get_item(*id).expect("invalid ID").name.clone(),
        }
    }

    pub fn name(&self) -> Option<&str> {
        if let Ident::Name { name, .. } = self {
            Some(name)
        } else {
            None
        }
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ident::Name { name, .. } => f.write_fmt(format_args!("Name({name})")),
            Ident::Id { id, .. } => f.write_fmt(format_args!("Id({})", id.0)),
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct TypePath {
    pub ident: Ident,
    pub arguments: Option<Box<TypePath>>,
}

/// An expression
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    /// f64
    Number(f64, Span),
    /// String
    String(String, Span),
    /// true|false
    Boolean(bool, Span),
    /// ()
    Unit(Span),
    /// nil
    Nil(Span),
    /// self
    SelfExpr(Span),
    /// id
    Identifier(Ident),
    /// expr `op` expr
    BinExpr(Box<BinExpr>, Span),
    /// `-`|`+`|`!` expr
    UnaryExpr(Op, Box<Expr>, Span),
    /// expr `[` expr `]`
    Index(Box<Expr>, Box<Expr>, Span),
    /// `(` [Expr] `)`
    ParenExpr(Box<Expr>, Span),
    /// [ expr, ... ]
    List(Box<Vec<Expr>>, Span),
    /// `(` expr, ... `)`
    Tuple(Box<Vec<Expr>>, Span),
    /// `{` expr : expr, ... `}`
    Map(Box<Vec<(Expr, Expr)>>, Span),
    /// expr `or` expr
    Or(Box<Expr>, Box<Expr>, Span),
    /// expr `and` expr
    And(Box<Expr>, Box<Expr>, Span),
    /// expr `(` expr, ... `)`
    FunCall(Box<Expr>, Box<Vec<Expr>>, Span),
    /// expr `.` expr
    MemberExpr(Box<Expr>, Box<Expr>, Span),
    /// expr `:` expr
    AssocExpr(Box<Expr>, Box<Expr>, Span),
    /// type
    Type(TypePath, Span),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self.clone() {
            Self::Number(_, span)
            | Self::String(_, span)
            | Self::Boolean(_, span)
            | Self::Unit(span)
            | Self::Nil(span)
            | Self::SelfExpr(span)
            | Self::BinExpr(_, span)
            | Self::UnaryExpr(_, _, span)
            | Self::ParenExpr(_, span)
            | Self::Index(_, _, span)
            | Self::List(_, span)
            | Self::Tuple(_, span)
            | Self::Map(_, span)
            | Self::Or(_, _, span)
            | Self::And(_, _, span)
            | Self::FunCall(_, _, span)
            | Self::MemberExpr(_, _, span)
            | Self::AssocExpr(_, _, span)
            | Self::Type(_, span) => span,
            Self::Identifier(x) => x.span(),
        }
    }
}
