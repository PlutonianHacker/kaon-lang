//! Compiler for the Kaon language.

pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod pass;
pub mod resolve;
pub mod token;
pub mod typecheck;
//pub mod typ;

pub use ast::{ASTNode, BinExpr, Class, Expr, FunAccess, Ident, Op, ScriptFun, Stmt, AST, Constructor, TypePath};
pub use codegen::Compiler;
pub use lexer::Lexer;
pub use parser::Parser;
pub use pass::Pass;
pub use resolve::{Resolver, Scope, ScopedMap, Symbol};
pub use token::{Token, TokenType};
pub use typecheck::{Type, TypeChecker};
