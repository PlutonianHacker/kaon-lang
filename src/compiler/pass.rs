use crate::compiler::{ASTNode, BinExpr, Expr, Ident, Op, ScriptFun, Stmt};

/// A trait used for each pass the compiler makes.
trait Pass {
    fn visit(&self, node: &ASTNode) {
        match node {
            ASTNode::Stmt(stmt) => self.statment(&stmt),
            ASTNode::Expr(expr) => self.expression(&expr),
        }
    }

    fn statment(&self, stmt: &Stmt) {
        match stmt {
            Stmt::IfStatement(expr, body, _) => self.if_statement(expr, body),
            Stmt::WhileStatement(expr, body, _) => self.while_statement(expr, body),
            Stmt::LoopStatement(body, _) => self.loop_statement(body),
            Stmt::Block(stmts, _) => self.block(&stmts),
            Stmt::VarDeclaration(ident, expr, _) => self.var_decl(ident, expr),
            Stmt::ConDeclaration(ident, expr, _) => self.con_decl(ident, expr),
            Stmt::AssignStatement(ident, expr, _) => self.assign_stmt(ident, expr),
            Stmt::ScriptFun(fun, _) => self.fun(fun),
            Stmt::Return(expr, _) => self.return_stmt(expr),
            Stmt::Break(_) => self.break_stmt(),
            Stmt::Continue(_) => self.continue_stmt(),
            Stmt::Expr(expr) => self.expression(expr),
        }
    }

    fn if_statement(&self, expr: &Expr, body: &(Stmt, Option<Stmt>)) {
        self.expression(&expr);
        self.statment(&body.0);
        if body.1.is_some() {
            self.statment(&body.1.as_ref().unwrap());
        }
    }

    fn while_statement(&self, expr: &Expr, body: &Stmt) {
        self.expression(&expr);
        self.statment(&body);
    }

    fn loop_statement(&self, body: &Stmt) {
        self.statment(&body);
    }

    fn block(&self, stmts: &Vec<Stmt>) {
        for node in stmts {
            self.statment(&node);
        }
    }

    fn var_decl(&self, _ident: &Ident, expr: &Expr) {
        self.expression(&expr);
    }

    fn con_decl(&self, _ident: &Ident, expr: &Expr) {
        self.expression(&expr);
    }

    fn assign_stmt(&self, _ident: &Ident, expr: &Expr) {
        self.expression(&expr);
    }

    fn fun(&self, _fun: &ScriptFun) {}

    fn return_stmt(&self, expr: &Expr) {
        self.expression(&expr);
    }

    fn break_stmt(&self) {}

    fn continue_stmt(&self) {}

    fn expression(&self, expr: &Expr) {
        match expr {
            Expr::Number(val, _) => self.number(&val),
            Expr::String(val, _) => self.string(&val),
            Expr::Boolean(val, _) => self.boolean(&val),
            Expr::Unit(_) => self.nil(),
            Expr::Identifier(ident) => self.identifier(&ident),
            Expr::BinExpr(bin_expr, _) => self.binary_expr(&bin_expr),
            Expr::UnaryExpr(op, unary_expr, _) => self.unary_expr(&op, &unary_expr),
            Expr::Index(expr, index, _) => self.index(&expr, &index),
            Expr::List(list, _) => self.list((&list).to_vec()),
            Expr::Or(lhs, rhs, _) => self.or(&lhs, &rhs),
            Expr::And(lhs, rhs, _) => self.and(&lhs, &rhs),
            Expr::FunCall(callee, args, _) => self.fun_call(&callee, &args),
            Expr::MemberExpr(obj, prop, _) => self.member_expr(&obj, &prop),
        }
    }

    fn and(&self, lhs: &Expr, rhs: &Expr) {
        self.expression(&lhs);
        self.expression(&rhs);
    }

    fn or(&self, lhs: &Expr, rhs: &Expr) {
        self.expression(&lhs);
        self.expression(&rhs);
    }

    fn binary_expr(&self, bin_expr: &BinExpr) {
        self.expression(&bin_expr.lhs);
        self.expression(&bin_expr.rhs);
    }

    fn unary_expr(&self, _op: &Op, expr: &Expr) {
        self.expression(&expr);
    }

    fn index(&self, expr: &Expr, index: &Expr) {
        self.expression(&expr);
        self.expression(&index);
    }

    fn list(&self, list: Vec<Expr>) {
        for item in list {
            self.expression(&item);
        }
    }

    fn fun_call(&self, callee: &Expr, args: &Vec<Expr>) {
        self.expression(&callee);
        for arg in args {
            self.expression(&arg);
        }
    }

    fn member_expr(&self, obj: &Expr, prop: &Expr) {
        self.expression(&obj);
        self.expression(&prop);
    }

    fn identifier(&self, _ident: &Ident) {}
    fn number(&self, _val: &f64) {}
    fn string(&self, _val: &String) {}
    fn boolean(&self, _val: &bool) {}
    fn nil(&self) {}
}
