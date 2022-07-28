//! The `Kaon` compiler pipeline. 
//! 
//! This module contains the entire compilation process, including lexing,
//! parsing, name resolution, typechecking and compiling into bytecode.
//! 
//! # The Compiler Pipeline
//! 
//! The following is a high level overview of how the compiler works.
//! 
//! ## Lexing
//! 
//! To begin with, user defined source code is transformed into a stream
//! of tokens. This is done through [`Lexer::tokenize`]. Any unrecongized tokens 
//! are reported as syntax errors.
//! 
//! ## Parsing
//! 
//! The _parser_ generates an [`AST`] (abstract-syntax tree), from the token stream created
//! by the lexer. Parsing is done with a _recursive decent_ (top-down) parser.
//! 
//! Any syntax errors are handled at this point. 
//! 
//! ## Name Resolution
//! 
//! Names are resolved during this pass. The [`AST`] is transversed top-down and
//! any names it encounters (type names, variable name, etc.) are resolved.
//! 
//! Duplicate names, undeclared variables, and other errors are reported during this pass.
//! 
//! ## Typechecking
//! 
//! During this pass, the _typechecker_ performs type inference and typechecking on the [`AST`].
//! 
//! ## Code Generation
//! 
//! This phase of the pipeline handles the actual process of coverting the high-level [`AST`] into
//! low-level bytecode, which resembles simplified machine code.

pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod pass;
pub mod resolve;
pub mod token;
pub mod typecheck;
pub mod hir;
pub mod query;
mod typ;

pub use ast::{ASTNode, BinExpr, Class, Expr, Ident, Op, Fun, Stmt, AST, Constructor, TypePath};
pub use codegen::Compiler;
pub use lexer::Lexer;
pub use parser::Parser;
pub use pass::Pass;
pub use resolve::{Resolver, Scope, ScopedMap, Symbol};
pub use token::{Token, TokenType};
pub use typecheck::{Type, TypeChecker};
