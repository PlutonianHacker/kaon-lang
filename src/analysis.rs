use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::ast::AST;
use crate::ast::{AssignStmt, BinExpr, Ident, IfStmt, Literal, Op, Print, UnaryExpr, VarDecl};

type SymbolTable = HashMap<String, Symbol>;

pub struct SemanticErr(pub String);

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    Boolean,
    Nil,
    String,
    Unit,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Number => write!(f, "number"),
            Type::Boolean => write!(f, "boolean"),
            Type::Nil => write!(f, "nil"),
            Type::String => write!(f, "string"),
            Type::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug)]
enum Symbol {
    VarSymbol(Type),
}

pub struct SemanticAnalyzer {
    symbols: SymbolTable,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            symbols: SymbolTable::new(),
        }
    }

    pub fn visit(&mut self, node: &AST) -> Result<Type, SemanticErr> {
        match *node {
            AST::IfStmt(ref stmt) => self.if_stmt(stmt),
            AST::Print(ref expr) => self.print_expr(expr),
            AST::VarDecl(ref expr) => self.var_decl(expr),
            AST::AssignStmt(ref expr) => self.assign_stmt(expr),
            AST::BinExpr(ref expr) => self.binary(expr),
            AST::UnaryExpr(ref expr) => self.unary(expr),
            AST::Literal(ref val) => self.literal(val),
            AST::Id(ref id) => Ok(self.ident(id)?),
            _ => unimplemented!(),
        }
    }

    fn visit_block(&mut self, nodes: &Vec<AST>) -> Result<Type, SemanticErr> {
        if nodes.len() == 0 {
            Ok(Type::Unit)
        } else {
            for i in 0..nodes.len() - 1 {
                self.visit(&nodes[i])?;
            }
            Ok(self.visit(&nodes[nodes.len() - 1])?)
        }
    }

    fn if_stmt(&mut self, node: &IfStmt) -> Result<Type, SemanticErr> {
        self.visit(&node.test)?;
        let ret_type = self.visit_block(&node.body)?;

        Ok(ret_type)
    }

    fn print_expr(&mut self, node: &Print) -> Result<Type, SemanticErr> {
        Ok(self.visit(&node.expr)?)
    }

    fn var_decl(&mut self, node: &VarDecl) -> Result<Type, SemanticErr> {
        match self.symbols.get(&node.id.0) {
            Some(_) => Err(SemanticErr(format!(
                "Semantic Error: variable {} has already been declared",
                &node.id.0
            ))),
            None => {
                let sym_type = self.visit(&node.val)?;
                let sym = Symbol::VarSymbol(sym_type.clone());
                self.symbols.insert(node.id.0.clone(), sym);
                Ok(sym_type)
            }
        }
    }

    fn assign_stmt(&mut self, stmt: &AssignStmt) -> Result<Type, SemanticErr> {
        self.ident(&stmt.id)?;
        Ok(self.visit(&stmt.val)?)
    }

    fn ident(&mut self, id: &Ident) -> Result<Type, SemanticErr> {
        match self.symbols.get(&id.0) {
            None => Err(SemanticErr(format!(
                "Semantic Error: variable '{}' cannot be found",
                &id.0
            ))),
            Some(Symbol::VarSymbol(sym)) => return Ok(sym.clone()),
        }
    }

    fn binary(&mut self, node: &Rc<BinExpr>) -> Result<Type, SemanticErr> {
        let bin_expr: &BinExpr = node.borrow();
        let lhs_type = self.visit(&bin_expr.lhs)?;
        let rhs_type = self.visit(&bin_expr.rhs)?;

        match (&node.op, &lhs_type, &rhs_type) {
            (_, &Type::Number, &Type::Number) => return Ok(rhs_type),
            (&Op::NotEqual, _, _) | (&Op::Equals, _, _) => return Ok(rhs_type),
            _ => {
                return Err(SemanticErr(format!(
                    "Semantic Error: cannot {} {{{}}} to {{{}}}",
                    node.op, &lhs_type, &rhs_type
                )))
            }
        }
    }

    fn unary(&mut self, node: &UnaryExpr) -> Result<Type, SemanticErr> {
        let unary_type = self.visit(&node.rhs)?;
        match (&node.op, &unary_type) {
            (&Op::Sub, Type::Number) | (&Op::Add, Type::Number) => Ok(unary_type),
            (&Op::Not, Type::Boolean) => Ok(unary_type),
            _ => {
                return Err(SemanticErr(format!(
                    "Semantic Error: cannot apply unary operator '{}' to `{}`",
                    &node.op.get_symbol(),
                    &unary_type,
                )))
            }
        }
    }

    fn literal(&mut self, val: &Literal) -> Result<Type, SemanticErr> {
        match *val {
            Literal::Number(_) => Ok(Type::Number),
            Literal::Boolean(_) => Ok(Type::Boolean),
            Literal::String(_) => Ok(Type::String),
            Literal::Nil => Ok(Type::Nil),
        }
    }
}
