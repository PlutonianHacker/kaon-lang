use std::ops::Neg;
use std::rc::Rc;
use std::borrow::Borrow;

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
pub enum Expr {
    Op(Op),
    Literal(Literal),
    BinExpr(Rc<BinExpr>),
    UnaryExpr(Rc<UnaryExpr>),
}

impl Neg for Expr {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Expr::UnaryExpr(ref val) => {
                let expr: &UnaryExpr = val.borrow();
                -expr.rhs.clone()
            }  
            Expr::Literal(Literal::Number(ref val)) => {
                Expr::Literal(Literal::Number(-val))
            } 
            _ => {
                println!("{:?}", self);
                unimplemented!()
            }
        }
    }
}

#[derive(Debug)]
pub struct File {
    pub nodes: Vec<Expr>,
}
