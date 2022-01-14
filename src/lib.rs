//! # Kaon
//! This respitory contains the Kaon programming language, including the parser, compiler and vm.

pub mod analysis;
pub mod ast;
pub mod compiler;
pub mod core;
pub mod data;
//pub mod error;
pub mod lexer;
pub mod opcode;
pub mod parser;
pub mod repl;
pub mod source;
pub mod span;
pub mod stack;
pub mod token;
pub mod vm;
pub mod error;