//! # Kaon
//!
//! This respitory contains the Kaon programming language, including the parser, compiler and vm.

pub mod common;
pub mod compiler;
pub mod core;
pub mod error;
pub mod repl;
pub mod vm;

use common::{DataMap, Function, KaonFile, Source, Spanned};
use compiler::{Scope, Token, AST};
use std::{fmt, fmt::Debug, fmt::Display, rc::Rc};
use vm::{Vm, VmSettings};

#[derive(Debug)]
pub enum KaonError {
    CompilerError(error::Error),
    RuntimeError(error::Error),
    InvalidScriptPath(String),
}

impl Display for KaonError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CompilerError(error) | Self::RuntimeError(error) => write!(f, "{}", error),
            Self::InvalidScriptPath(path) => {
                write!(f, "the path '{}' could not be found", path)
            }
        }
    }
}

pub struct KaonSettings {
    stdin: Rc<dyn KaonFile>,
    stdout: Rc<dyn KaonFile>,
    stderr: Rc<dyn KaonFile>,
}

impl Default for KaonSettings {
    fn default() -> Self {
        let settings = VmSettings::default();
        KaonSettings {
            stdin: settings.stdin,
            stdout: settings.stdout,
            stderr: settings.stderr,
        }
    }
}

/// The main interface for the Kaon langauge.
pub struct Kaon {
    vm: Vm,
    chunk: Function,
}

impl Kaon {
    pub fn new() -> Self {
        Self::with_settings(KaonSettings::default())
    }

    pub fn with_settings(settings: KaonSettings) -> Self {
        Kaon {
            vm: Vm::with_settings(VmSettings {
                stdin: settings.stdin,
                stdout: settings.stdout,
                stderr: settings.stderr,
            }),
            chunk: Function::empty(),
        }
    }

    /// Compile bytecode from a string.
    pub fn compile(&mut self, script: &str) -> Result<Function, KaonError> {
        let source = Source::contents(script);
        self.compile_from_source(source)
    }

    /// Compile bytecode from a source.
    pub fn compile_from_source(&mut self, source: Rc<Source>) -> Result<Function, KaonError> {
        let tokens = self.tokenize(source)?;
        let ast = self.parse(tokens)?;

        let mut compiler = compiler::Compiler::build();
        let bytecode = compiler.run(&ast, Scope::new(None));

        match bytecode {
            Ok(bytecode) => {
                self.chunk = bytecode.clone();
                Ok(bytecode)
            }
            Err(err) => panic!("{:?}", err),
        }
    }

    /// Run compiled bytecode.
    pub fn run(&mut self) -> Result<(), KaonError> {
        self.vm.interpret(Rc::new(self.chunk.clone()));
        Ok(())
    }

    /// Compile and run from a script.
    pub fn run_from_script(&mut self, script: &str) -> Result<(), KaonError> {
        self.compile(script)?;
        self.run()?;
        Ok(())
    }

    /// Run from a [Source]
    pub fn run_from_source(&mut self, source: Rc<Source>) -> Result<(), KaonError> {
        self.compile_from_source(source)?;
        self.run()?;
        Ok(())
    }

    /// Parse a stream of [Token]s into an [AST].
    pub fn parse(&self, tokens: Spanned<Vec<Token>>) -> Result<AST, KaonError> {
        let mut parser = compiler::Parser::new(tokens);
        let ast = parser.parse();
        match ast {
            Ok(ast) => Ok(ast),
            Err(err) => panic!("{}", err),
        }
    }

    /// Tokenize a script.
    pub fn tokenize(&self, source: Rc<Source>) -> Result<Spanned<Vec<Token>>, KaonError> {
        let mut lexer = compiler::Lexer::new(source);
        let tokens = lexer.tokenize();
        match tokens {
            Ok(token_stream) => Ok(token_stream),
            Err(err) => panic!("{}", err),
        }
    }

    /// Read a file from provided path.
    pub fn read_file(&self, path: &str) -> Result<Rc<Source>, KaonError> {
        Source::from_file(path).map_err(|_| KaonError::InvalidScriptPath(path.to_string()))
    }

    /// Access the VM's prelude
    pub fn prelude(&self) -> DataMap {
        self.vm.prelude()
    }
}
