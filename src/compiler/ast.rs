use crate::common::Span;

#[derive(Debug)]
pub struct AST {
    pub nodes: Vec<ASTNode>,
    pub span: Span,
}

impl AST {
    pub fn new(nodes: Vec<ASTNode>, span: Span) -> Self {
        AST { nodes, span }
    }
}

#[derive(Debug, PartialEq)]
pub enum ASTNode {
    Stmt(Stmt),
    Expr(Expr),
}

impl From<Stmt> for ASTNode {
    fn from(stmt: Stmt) -> ASTNode {
        ASTNode::Stmt(stmt)
    }
}

impl From<&Stmt> for ASTNode {
    fn from(stmt: &Stmt) -> ASTNode {
        ASTNode::Stmt(stmt.clone())
    }
}

impl From<Expr> for ASTNode {
    fn from(expr: Expr) -> ASTNode {
        ASTNode::Expr(expr)
    }
}

impl From<&Expr> for ASTNode {
    fn from(expr: &Expr) -> ASTNode {
        ASTNode::Expr(expr.clone())
    }
}

pub struct StmtBlock(Vec<Stmt>, Span);

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    IfStatement(Expr, Box<(Stmt, Option<Stmt>)>, Span),
    WhileStatement(Expr, Box<Stmt>, Span),
    LoopStatement(Box<Stmt>, Span),
    Block(Box<Vec<Stmt>>, Span),
    VarDeclaration(Ident, Expr, Span),
    ConDeclaration(Ident, Expr, Span),
    AssignStatement(Ident, Expr, Span),
    ScriptFun(Box<ScriptFun>, Span),
    Return(Expr, Span),
    Expr(Expr),
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self.clone() {
            Self::IfStatement(_, _, span) => span,
            Self::WhileStatement(_, _, span) => span,
            Self::LoopStatement(_, span) => span,
            Self::Block(_, span) => span,
            Self::VarDeclaration(_, _, span) => span,
            Self::ConDeclaration(_, _, span) => span,
            Self::AssignStatement(_, _, span) => span,
            Self::ScriptFun(_, span) => span,
            Self::Return(_, span) => span,
            Self::Expr(expr) => expr.span(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ScriptFun {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub body: Stmt,
    pub access: FunAccess,
}

impl ScriptFun {
    pub fn new(name: Ident, params: Vec<Ident>, body: Stmt, access: FunAccess) -> Self {
        ScriptFun { name, params, body, access }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum FunAccess {
    Public,
    Private,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinExpr {
    pub op: Op,
    pub lhs: Expr,
    pub rhs: Expr,
}

impl BinExpr {
    pub fn new(op: Op, lhs: Expr, rhs: Expr) -> Self {
        BinExpr { op, lhs, rhs }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    GreaterThan,
    GreaterThanEquals,
    LessThan,
    LessThanEquals,
    EqualTo,
    NotEqual,
    Bang,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl Ident {
    pub fn span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Number(f64, Span),
    String(String, Span),
    Boolean(bool, Span),
    Unit(Span),
    Identifier(Ident),
    BinExpr(Box<BinExpr>, Span),
    UnaryExpr(Op, Box<Expr>, Span),
    List(Box<Vec<Expr>>, Span),
    Or(Box<Expr>, Box<Expr>, Span),
    And(Box<Expr>, Box<Expr>, Span),
    FunCall(Box<Expr>, Box<Vec<Expr>>, Span),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self.clone() {
            Self::Number(_, span) => span,
            Self::String(_, span) => span,
            Self::Boolean(_, span) => span,
            Self::Unit(span) => span,
            Self::Identifier(x) => x.span(),
            Self::BinExpr(_, span) => span,
            Self::UnaryExpr(_, _, span) => span,
            Self::List(_, span) => span,
            Self::Or(_, _, span) => span,
            Self::And(_, _, span) => span,
            Self::FunCall(_, _, span) => span,
        }
    }
}
