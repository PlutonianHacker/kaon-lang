use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::Expr;
use crate::ast::{AssignStmt, BinExpr, Ident, Literal, UnaryExpr, VarDecl};

type SymbolTable = HashMap<String, Symbol>;

pub struct SemanticErr(pub String);

#[derive(Debug)]
enum Symbol {
    VarSymbol { val: Expr },
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

    pub fn visit(&mut self, node: &Expr) -> Result<(), SemanticErr> {
        match *node {
            Expr::VarDecl(ref expr) => self.var_decl(expr),
            Expr::AssignStmt(ref expr) => self.assign_stmt(expr),
            Expr::BinExpr(ref expr) => self.binary(expr),
            Expr::UnaryExpr(ref expr) => self.unary(expr),
            Expr::Literal(ref val) => Ok(self.literal(val)),
            Expr::Id(ref id) => Ok(self.ident(id)?),
            _ => unimplemented!(),
        }
    }

    fn var_decl(&mut self, node: &VarDecl) -> Result<(), SemanticErr> {
        match self.symbols.get(&node.id.0) {
            Some(_) => Err(SemanticErr(format!(
                "Semantic Error: variable {} has already been declared",
                &node.id.0
            ))),
            None => {
                let sym = Symbol::VarSymbol {
                    val: node.val.clone(),
                };
                self.symbols.insert(node.id.0.clone(), sym);
                self.visit(&node.val)?;
                Ok(())
            }
        }
    }

    fn assign_stmt(&mut self, stmt: &AssignStmt) -> Result<(), SemanticErr> {
        self.ident(&stmt.id)?;
        self.visit(&stmt.val)?;

        Ok(())
    }

    fn ident(&mut self, id: &Ident) -> Result<(), SemanticErr> {
        match self.symbols.get(&id.0) {
            None => Err(SemanticErr(format!(
                "Semantic Error: variable '{}' cannot be found",
                &id.0
            ))),
            Some(_) => Ok(()),
        }
    }

    fn binary(&mut self, node: &Rc<BinExpr>) -> Result<(), SemanticErr> {
        let bin_expr: &BinExpr = node.borrow();
        self.visit(&bin_expr.lhs)?;
        self.visit(&bin_expr.rhs)?;
        Ok(())
    }

    fn unary(&mut self, node: &UnaryExpr) -> Result<(), SemanticErr> {
        self.visit(&node.rhs)?;
        Ok(())
    }

    fn literal(&mut self, _: &Literal) {
        return;
    }
}
