//! Compiler for the Kaon language.

pub mod ast;
#[allow(clippy::module_inception)]
pub mod compiler;
pub mod lexer;
pub mod parser;
pub mod pass;
pub mod resolve;
pub mod token;
pub mod typecheck;

pub use ast::{ASTNode, BinExpr, Class, Expr, FunAccess, Ident, Op, ScriptFun, Stmt, AST, Constructor, TypePath};
pub use compiler::Compiler;
pub use lexer::Lexer;
pub use parser::Parser;
pub use pass::Pass;
pub use resolve::{Resolver, Scope, ScopedMap, Symbol};
pub use token::{Token, TokenType};
pub use typecheck::{Type, TypeChecker};
