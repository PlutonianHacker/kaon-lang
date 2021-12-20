use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Ident(pub String);

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

impl From<Literal> for String {
    fn from(ast: Literal) -> String {
        match ast {
            Literal::String(val) => val,
            Literal::Number(val) => val.to_string(),
            Literal::Boolean(val) => val.to_string(),
            Literal::Nil => "nil".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    GToEq,
    LToEq,
    Gt,
    Lt,
    Not,
    Equals,
    NotEqual,
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr {
    pub op: Op,
    pub rhs: Expr,
}

#[derive(Debug, PartialEq)]
pub struct BinExpr {
    pub op: Op,
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub id: Ident,
    pub val: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignStmt {
    pub id: Ident,
    pub val: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Op(Op),
    Literal(Literal),
    BinExpr(Rc<BinExpr>),
    UnaryExpr(Rc<UnaryExpr>),
    Id(Ident),
    VarDecl(Rc<VarDecl>),
    AssignStmt(Rc<AssignStmt>),
}

#[derive(Debug)]
pub struct File {
    pub nodes: Vec<Expr>,
}
