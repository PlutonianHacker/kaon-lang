use crate::ast::Expr;

pub trait Visitor {
    fn visit(&mut self, expr: Expr);
}