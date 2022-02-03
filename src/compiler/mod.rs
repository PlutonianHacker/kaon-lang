//! Compiler for the Kaon language.

pub mod analysis;
pub mod ast;
pub mod compiler;
pub mod lexer;
pub mod parser;
pub mod pass;
pub mod token;
pub mod typecheck;

pub use analysis::{Scope, SemanticAnalyzer};
pub use ast::{ASTNode, BinExpr, Expr, FunAccess, Ident, Op, ScriptFun, Stmt, AST};
pub use compiler::Compiler;
pub use lexer::Lexer;
pub use parser::Parser;
pub use pass::Pass;
pub use token::{Token, TokenType};
pub use typecheck::{Type, TypeChecker};
