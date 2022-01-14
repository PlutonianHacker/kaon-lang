use std::collections::HashMap;
use std::fmt;

use crate::compiler::{ASTNode, BinExpr, Expr, Ident, Op, Stmt};
use crate::core::{ffi_core, FFI};
use crate::common::Span;

pub type SymbolTable = HashMap<String, Symbol>;

pub struct SemanticError(pub String);

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    Boolean,
    Nil,
    String,
    Unit,
    List(Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Number => write!(f, "number"),
            Type::Boolean => write!(f, "boolean"),
            Type::Nil => write!(f, "nil"),
            Type::String => write!(f, "string"),
            Type::Unit => write!(f, "()"),
            Type::List(ref list_type) => write!(f, "[{}]", list_type),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Symbol {
    VarSymbol(Type),
    //BuiltinFunc(Vec<Type>, Type),
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub symbols: SymbolTable,
    pub outer: Option<Box<Scope>>,
}

impl Scope {
    pub fn new(outer: Option<Box<Scope>>) -> Scope {
        Scope {
            symbols: SymbolTable::new(),
            outer,
        }
    }

    pub fn insert(&mut self, key: String, symbol: Symbol) {
        self.symbols.insert(key, symbol);
    }

    pub fn find(&mut self, key: &String, current_scope_only: bool) -> Option<&mut Scope> {
        if self.symbols.contains_key(key) {
            Some(self)
        } else if self.outer.is_some() && !current_scope_only {
            self.outer.as_mut().unwrap().find(key, current_scope_only)
        } else {
            None
        }
    }

    pub fn get(&mut self, key: &String, current_scope_only: bool) -> Option<&Symbol> {
        let scope = self.find(key, current_scope_only);
        match scope {
            Some(_) => scope.unwrap().symbols.get(key),
            None => None,
        }
    }
}

pub struct SemanticAnalyzer {
    pub current_scope: Scope,
    pub ffi: FFI,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let current_scope = Scope::new(None);

        SemanticAnalyzer {
            current_scope,
            ffi: ffi_core(),
        }
    }

    fn enter_scope(&mut self) {
        let old_scope = self.current_scope.clone();
        let new_scope = Scope::new(Some(Box::new(old_scope)));
        self.current_scope = new_scope;
    }

    fn exit_scope(&mut self) {
        let old_scope = self.current_scope.outer.clone();
        self.current_scope = *old_scope.unwrap();
    }

    pub fn visit(&mut self, node: &ASTNode) -> Result<Type, SemanticError> {
        match &node.clone() {
            &ASTNode::Stmt(stmt) => match stmt.clone() {
                Stmt::IfStatement(ref expr, block, ref span) => self.if_stmt(expr, block, span),
                Stmt::WhileStatement(expr, block, _) => self.while_stmt(expr, block),
                Stmt::LoopStatement(block, _) => self.loop_stmt(block),
                Stmt::Block(stmts, span) => self.block(stmts, &span),
                Stmt::VarDeclaration(ident, expr, span) => self.var_decl(ident, expr, &span),
                Stmt::AssignStatement(ident, expr, span) => self.assign_stmt(ident, expr, &span),
                Stmt::Expr(expr) => self.expression(expr),
            },
            &ASTNode::Expr(expr) => match expr.clone() {
                Expr::Number(ref val, ref span) => self.number(val, span),
                Expr::String(ref val, ref span) => self.string(val, span),
                Expr::Boolean(ref val, ref span) => self.boolean(val, span),
                Expr::Identifier(ident) => self.identifier(ident),
                Expr::BinExpr(expr, span) => self.binary(expr, &span),
                Expr::UnaryExpr(op, expr, span) => self.unary(op, expr, &span),
                Expr::List(list, span) => self.list(list, &span),
                Expr::FunCall(id, args, span) => self.fun_call(id, args, &span),
                Expr::Or(lhs, rhs) => self.or(lhs, rhs),
                Expr::And(lhs, rhs) => self.and(lhs, rhs),
            },
        }
    }

    fn block(&mut self, stmts: Box<Vec<Stmt>>, _: &Span) -> Result<Type, SemanticError> {
        self.enter_scope();
        for stmt in *stmts {
            self.visit(&ASTNode::from(stmt))?;
        }

        self.exit_scope();

        Ok(Type::Unit)
    }

    fn if_stmt(
        &mut self,
        condition: &Expr,
        block: Box<(Stmt, Option<Stmt>)>,
        _span: &Span,
    ) -> Result<Type, SemanticError> {
        self.visit(&ASTNode::from(condition))?;

        if block.1.is_some() {
            self.visit(&ASTNode::from(block.1.unwrap()))?;
        }

        self.visit(&ASTNode::from(block.0))
    }

    fn loop_stmt(&mut self, block: Box<Stmt>) -> Result<Type, SemanticError> {
        self.visit(&ASTNode::from(*block))
    }

    fn while_stmt(&mut self, condition: Expr, block: Box<Stmt>) -> Result<Type, SemanticError> {
        self.visit(&ASTNode::from(condition))?;
        self.visit(&ASTNode::from(*block))
    }

    fn fun_call(
        &mut self,
        _: Box<Expr>,
        args: Box<Vec<Expr>>,
        _: &Span,
    ) -> Result<Type, SemanticError> {
        for arg in args.iter() {
            self.visit(&ASTNode::from(arg))?;
        }
        return Ok(Type::Unit);
    }

    fn var_decl(&mut self, ident: Ident, init: Expr, _span: &Span) -> Result<Type, SemanticError> {
        match self.current_scope.get(&ident.name, true) {
            Some(_) => Err(SemanticError(format!(
                "Semantic Error: variable {} has already been declared",
                &ident.name
            ))),
            None => {
                let sym_type = self.visit(&ASTNode::from(init))?;
                let sym = Symbol::VarSymbol(sym_type.clone());
                self.current_scope.insert(ident.name.clone(), sym);
                Ok(sym_type)
            }
        }
    }

    fn assign_stmt(
        &mut self,
        ident: Ident,
        expr: Expr,
        _span: &Span,
    ) -> Result<Type, SemanticError> {
        self.identifier(ident)?;
        Ok(self.visit(&ASTNode::from(expr))?)
    }

    fn expression(&mut self, expr: Expr) -> Result<Type, SemanticError> {
        self.visit(&ASTNode::from(expr))
    }

    fn identifier(&mut self, id: Ident) -> Result<Type, SemanticError> {
        match self.current_scope.get(&id.name, false) {
            None => match self.ffi.get(&id.name) {
                None => Err(SemanticError(format!(
                    "Semantic Error: cannot find variable `{}` in this scope",
                    &id.name
                ))),
                Some(_) => Ok(Type::Unit),
            },
            Some(Symbol::VarSymbol(sym)) => return Ok(sym.clone()),
        }
    }

    fn or(&mut self, lhs: Box<Expr>, rhs: Box<Expr>) -> Result<Type, SemanticError> {
        self.visit(&ASTNode::from(*lhs))?;
        Ok(self.visit(&ASTNode::from(*rhs))?)
    }

    fn and(&mut self, lhs: Box<Expr>, rhs: Box<Expr>) -> Result<Type, SemanticError> {
        self.visit(&ASTNode::from(*lhs))?;
        Ok(self.visit(&ASTNode::from(*rhs))?)
    }

    fn binary(&mut self, expr: Box<BinExpr>, _span: &Span) -> Result<Type, SemanticError> {
        let _lhs_type = self.visit(&ASTNode::from(expr.lhs))?;
        let rhs_type = self.visit(&ASTNode::from(expr.rhs))?;

        Ok(rhs_type)

        /*match (&node.op, &lhs_type, &rhs_type) {
            (&Op::And, &Type::Boolean, &Type::Boolean)
            | (&Op::Or, &Type::Boolean, &Type::Boolean) => return Ok(rhs_type),
            (&Op::And, _, _) | (&Op::Or, _, _) => return Ok(rhs_type),
            (_, &Type::Number, &Type::Number) => return Ok(rhs_type),
            (&Op::NotEqual, _, _) | (&Op::Equals, _, _) => return Ok(rhs_type),
            _ => {
                return Err(SemanticError(format!(
                    "Semantic Error: {}",
                    node.op.display(lhs_type, rhs_type),
                )))
            }
        }*/
    }

    fn unary(&mut self, op: Op, expr: Box<Expr>, _span: &Span) -> Result<Type, SemanticError> {
        let unary_type = self.visit(&ASTNode::from(*expr))?;
        match (&op, &unary_type) {
            (&Op::Subtract, Type::Number) | (&Op::Add, Type::Number) => Ok(unary_type),
            (&Op::Bang, Type::Boolean) => Ok(unary_type),
            _ => {
                return Err(SemanticError(format!(
                    "Semantic Error: cannot apply unary operator '+' to `{}`",
                    &unary_type,
                )))
            }
        }
    }

    fn list(&mut self, list: Box<Vec<Expr>>, _span: &Span) -> Result<Type, SemanticError> {
        if list.len() == 0 {
            Ok(Type::List(Box::new(Type::Nil)))
        } else {
            let list_type = self.visit(&ASTNode::from(list[0].clone()))?;

            for item in &list[1..] {
                let item_type = self.visit(&ASTNode::from(item.clone()))?;
                if item_type != list_type {
                    return Err(SemanticError(format!(
                        "Semantic Error: expected `{}` found {}",
                        list_type, item_type
                    )));
                }
            }

            Ok(Type::List(Box::new(list_type)))
        }
    }

    fn number(&mut self, _val: &f64, _span: &Span) -> Result<Type, SemanticError> {
        return Ok(Type::Number);
    }

    fn string(&mut self, _val: &String, _span: &Span) -> Result<Type, SemanticError> {
        return Ok(Type::String);
    }

    fn boolean(&mut self, _val: &bool, _span: &Span) -> Result<Type, SemanticError> {
        return Ok(Type::Boolean);
    }
}
