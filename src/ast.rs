use crate::span::Span;

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
    AssignStatement(Ident, Expr, Span),
    Expr(Expr),
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

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Number(f64, Span),
    String(String, Span),
    Boolean(bool, Span),
    Identifier(Ident),
    BinExpr(Box<BinExpr>, Span),
    UnaryExpr(Op, Box<Expr>, Span),
    List(Box<Vec<Expr>>, Span),
    Or(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    FunCall(Box<Expr>, Box<Vec<Expr>>, Span),
}
