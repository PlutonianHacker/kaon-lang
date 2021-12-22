use std::rc::Rc;
use std::fmt;

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
    Modulo,
    Gte,
    Lte,
    Gt,
    Lt,
    Not,
    Equals,
    NotEqual,
}

impl Op {
    pub fn get_symbol(&self) -> &'static str {
        match *self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Modulo => "%",
            Op::Gte => ">=",
            Op::Lte => "<=",
            Op::Gt => ">",
            Op::Lt => "<",
            Op::Not => "!",
            Op::Equals => "==",
            Op::NotEqual => "!=",
        }
    } 
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Op::Add => write!(f, "add"),
            Op::Sub => write!(f, "subtract"),
            Op::Mul => write!(f, "multiply"),
            Op::Div => write!(f, "divide"),
            Op::Modulo => write!(f, "mod"),
            _ => write!(f, "compare"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignOp {
    Assign,
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr {
    pub op: Op,
    pub rhs: Expr,
}

#[derive(Debug, Clone, PartialEq)]
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
    pub op: AssignOp,
    pub val: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub test: Expr,
    pub body: Vec<Expr>,
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
    IfStmt(Rc<IfStmt>),
}

#[derive(Debug)]
pub struct File {
    pub nodes: Vec<Expr>,
}
