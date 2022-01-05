use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::ast::{
    AssignStmt, BinExpr, FuncCall, Ident, IfStmt, Literal, Op, Print, UnaryExpr, VarDecl,
};
use crate::ast::{ErrorMessage, AST};
use crate::core::{ffi_core, FFI};

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

    pub fn visit(&mut self, node: &AST) -> Result<Type, SemanticError> {
        match *node {
            AST::IfStmt(ref stmt) => self.if_stmt(stmt),
            AST::Block(_) => self.block(&node),
            AST::FuncCall(ref func) => self.func_call(func),
            AST::Print(ref expr) => self.print_expr(expr),
            AST::VarDecl(ref expr) => self.var_decl(expr),
            AST::AssignStmt(ref expr) => self.assign_stmt(expr),
            AST::BinExpr(ref expr) => self.binary(expr),
            AST::UnaryExpr(ref expr) => self.unary(expr),
            AST::List(ref list) => self.list(list),
            AST::Literal(ref val) => self.literal(val),
            AST::Id(ref id) => Ok(self.ident(id)?),
            _ => Err(SemanticError("SadBad Error: not implemented".to_string())),
        }
    }

    fn block(&mut self, nodes: &AST) -> Result<Type, SemanticError> {
        if let AST::Block(nodes) = nodes.clone() {
            let block: &Vec<AST> = nodes.borrow();
            self.enter_scope();
            match block.len() {
                0 => {
                    self.exit_scope();
                    return Ok(Type::Unit);
                }
                _ => {
                    for i in 0..block.len() - 1 {
                        self.visit(&nodes[i])?;
                    }
                    let ret_type = Ok(self.visit(&nodes[nodes.len() - 1])?);
                    self.exit_scope();
                    return ret_type;
                }
            }
        }
        Ok(Type::Unit)
    }

    fn if_stmt(&mut self, node: &IfStmt) -> Result<Type, SemanticError> {
        self.visit(&node.test)?;
        let ret_type = self.block(&node.body)?;

        Ok(ret_type)
    }

    fn print_expr(&mut self, node: &Print) -> Result<Type, SemanticError> {
        Ok(self.visit(&node.expr)?)
    }

    fn func_call(&mut self, func: &FuncCall) -> Result<Type, SemanticError> {
        //match self.current_scope.get(&func.callee, false) {
        //None => self.ffi_call(func),
        //Some(_) => Ok(Type::Unit),
        //}
        Ok(Type::Unit)
    }

    fn ffi_call(&mut self, func: &FuncCall) -> Result<Type, SemanticError> {
        /*match self.ffi.get(&func.callee) {
            Some(_) => {
                Ok(Type::Unit)
            }
            None => Err(SemanticError(format!(
                "Semantic Error: cannot find function `{}` in this scope",
                &func.callee
            ))),
        }*/
        Ok(Type::Unit)
    }

    fn var_decl(&mut self, node: &VarDecl) -> Result<Type, SemanticError> {
        match self.current_scope.get(&node.id.0, true) {
            Some(_) => Err(SemanticError(format!(
                "Semantic Error: variable {} has already been declared",
                &node.id.0
            ))),
            None => {
                let sym_type = self.visit(&node.val)?;
                let sym = Symbol::VarSymbol(sym_type.clone());
                self.current_scope.insert(node.id.0.clone(), sym);
                Ok(sym_type)
            }
        }
    }

    fn assign_stmt(&mut self, stmt: &AssignStmt) -> Result<Type, SemanticError> {
        self.ident(&stmt.id)?;
        Ok(self.visit(&stmt.val)?)
    }

    fn ident(&mut self, id: &Ident) -> Result<Type, SemanticError> {
        match self.current_scope.get(&id.0, false) {
            None => Err(SemanticError(format!(
                "Semantic Error: cannot find variable `{}` in this scope",
                &id.0
            ))),
            Some(Symbol::VarSymbol(sym)) => return Ok(sym.clone()),
        }
    }

    fn binary(&mut self, node: &Rc<BinExpr>) -> Result<Type, SemanticError> {
        let bin_expr: &BinExpr = node.borrow();
        let lhs_type = self.visit(&bin_expr.lhs)?;
        let rhs_type = self.visit(&bin_expr.rhs)?;

        match (&node.op, &lhs_type, &rhs_type) {
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
        }
    }

    fn unary(&mut self, node: &UnaryExpr) -> Result<Type, SemanticError> {
        let unary_type = self.visit(&node.rhs)?;
        match (&node.op, &unary_type) {
            (&Op::Sub, Type::Number) | (&Op::Add, Type::Number) => Ok(unary_type),
            (&Op::Not, Type::Boolean) => Ok(unary_type),
            _ => {
                return Err(SemanticError(format!(
                    "Semantic Error: cannot apply unary operator '{}' to `{}`",
                    &node.op.get_symbol(),
                    &unary_type,
                )))
            }
        }
    }

    fn list(&mut self, list: &Vec<AST>) -> Result<Type, SemanticError> {
        if list.len() == 0 {
            Ok(Type::List(Box::new(Type::Nil)))
        } else {
            let list_type = self.visit(&list[0])?;

            for item in &list[1..] {
                let item_type = self.visit(item)?;
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

    fn literal(&mut self, val: &Literal) -> Result<Type, SemanticError> {
        match *val {
            Literal::Number(_) => Ok(Type::Number),
            Literal::Boolean(_) => Ok(Type::Boolean),
            Literal::String(_) => Ok(Type::String),
            Literal::Nil => Ok(Type::Nil),
        }
    }
}
