use crate::compiler::{ASTNode, BinExpr, Expr, Ident, Op, ScriptFun, Stmt, Class};

use super::ast::Constructor;

/// A trait used for each pass the compiler makes.
///
/// Applies transformations to an [AST].
pub trait Pass<T, E> {

    fn visit(&mut self, node: &ASTNode) {
        let _ = match node {
            ASTNode::Stmt(stmt) => self.statment(stmt),
            ASTNode::Expr(expr) => self.expression(expr),
        };
    }

    fn statment(&mut self, stmt: &Stmt) -> Result<T, E> {
        match stmt {
            Stmt::Block(stmts, _) => self.block(stmts),
            Stmt::IfStatement(expr, body, _) => self.if_statement(expr, body),
            Stmt::WhileStatement(expr, body, _) => self.while_statement(expr, body),
            Stmt::LoopStatement(body, _) => self.loop_statement(body),
            Stmt::ImportStatement(import, _) => self.import_statement(import),
            Stmt::VarDeclaration(ident, expr, _, _) => self.var_decl(ident, expr),
            Stmt::ConDeclaration(ident, expr, _, _) => self.con_decl(ident, expr),
            Stmt::AssignStatement(ident, expr, _) => self.assign_stmt(ident, expr),
            Stmt::ScriptFun(fun, _) => self.fun(fun),
            Stmt::Class(class, _) => self.class(class),
            Stmt::Constructor(constructor, _) => self.constructor(constructor),
            Stmt::Return(expr, _) => self.return_stmt(expr),
            Stmt::Break(_) => self.break_stmt(),
            Stmt::Continue(_) => self.continue_stmt(),
            Stmt::Expr(expr) => self.expression(expr),
        }
    }

    fn if_statement(&mut self, expr: &Expr, body: &(Stmt, Option<Stmt>)) -> Result<T, E>;

    fn while_statement(&mut self, expr: &Expr, body: &Stmt) -> Result<T, E>;

    fn loop_statement(&mut self, body: &Stmt) -> Result<T, E>;

    fn import_statement(&mut self, import: &Expr) -> Result<T, E>;

    fn block(&mut self, stmts: &[Stmt]) -> Result<T, E>;

    fn var_decl(&mut self, _ident: &Ident, init: &Option<Expr>) -> Result<T, E>;

    fn con_decl(&mut self, _ident: &Ident, expr: &Expr) -> Result<T, E>;

    fn assign_stmt(&mut self, _ident: &Expr, expr: &Expr) -> Result<T, E>;

    fn class(&mut self, _class: &Class) -> Result<T, E>;

    fn constructor(&mut self, _constructor: &Constructor) -> Result<T, E>;

    fn fun(&mut self, _fun: &ScriptFun) -> Result<T, E>;

    fn return_stmt(&mut self, expr: &Expr) -> Result<T, E>;

    fn break_stmt(&mut self) -> Result<T, E>;

    fn continue_stmt(&mut self) -> Result<T, E>;

    fn expression(&mut self, expr: &Expr) -> Result<T, E> {
        match expr {
            Expr::Number(val, _) => self.number(val),
            Expr::String(val, _) => self.string(val),
            Expr::Boolean(val, _) => self.boolean(val),
            Expr::Unit(_) | Expr::Nil(_) => self.nil(),
            Expr::Identifier(ident) => self.identifier(ident),
            Expr::SelfExpr(_) => self.self_expr(),
            Expr::BinExpr(bin_expr, _) => self.binary_expr(bin_expr),
            Expr::UnaryExpr(op, unary_expr, _) => self.unary_expr(op, unary_expr),
            Expr::Index(expr, index, _) => self.index(expr, index),
            Expr::List(list, _) => self.list((list).to_vec()),
            Expr::Tuple(tuple, _) => self.tuple(tuple),
            Expr::Map(map, _) => self.map(map),
            Expr::Or(lhs, rhs, _) => self.or(lhs, rhs),
            Expr::And(lhs, rhs, _) => self.and(lhs, rhs),
            Expr::FunCall(callee, args, _) => self.fun_call(callee, args),
            Expr::MemberExpr(obj, prop, _) => self.member_expr(obj, prop),
            Expr::Type(typ) => self.type_spec(typ),
        }
    }

    fn type_spec(&mut self, typ: &Ident) -> Result<T, E>;

    fn and(&mut self, lhs: &Expr, rhs: &Expr) -> Result<T, E>;

    fn or(&mut self, lhs: &Expr, rhs: &Expr) -> Result<T, E>;

    fn binary_expr(&mut self, bin_expr: &BinExpr) -> Result<T, E>;

    fn unary_expr(&mut self, _op: &Op, expr: &Expr) -> Result<T, E>;

    fn index(&mut self, expr: &Expr, index: &Expr) -> Result<T, E>;

    fn tuple(&mut self, tuple: &[Expr]) -> Result<T, E>;

    fn list(&mut self, list: Vec<Expr>) -> Result<T, E>;

    fn map(&mut self, map: &[(Expr, Expr)]) -> Result<T, E>;

    fn fun_call(&mut self, callee: &Expr, args: &[Expr]) -> Result<T, E>;

    fn member_expr(&mut self, obj: &Expr, prop: &Expr) -> Result<T, E>;

    fn self_expr(&mut self) -> Result<T, E>;

    fn identifier(&mut self, _ident: &Ident) -> Result<T, E>;

    fn number(&mut self, _val: &f64) -> Result<T, E>;

    fn string(&mut self, _val: &str) -> Result<T, E>;

    fn boolean(&mut self, _val: &bool) -> Result<T, E>;

    fn nil(&mut self) -> Result<T, E>;
}
