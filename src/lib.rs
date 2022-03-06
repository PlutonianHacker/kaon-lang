//! # Kaon
//!
//! The Kaon programming language, including the parser, compiler and vm.

pub mod cli;
pub mod common;
pub mod compiler;
pub mod core;
pub mod error;
pub mod vm;

extern crate fnv;

use common::{Function, KaonFile, Spanned, ValueMap};
use compiler::{Resolver, Token, TypeChecker, AST};
use error::{Error, Errors};
use vm::{Vm, VmSettings};

use std::{fmt, fmt::Debug, fmt::Display, path::PathBuf, rc::Rc};

pub use {common::Source, common::Value, compiler::Scope};

pub enum KaonError {
    ParserError(Error),
    CompilerError(String),
    RuntimeError(String),
    InvalidScriptPath(String),
    MultipleErrors(Errors),
}

impl Display for KaonError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CompilerError(error) => write!(f, "{}", error),
            Self::ParserError(error) => write!(f, "{}", error),
            Self::RuntimeError(error) => write!(f, "{}", error),
            Self::InvalidScriptPath(path) => {
                write!(f, "the path '{}' could not be found", path)
            }
            Self::MultipleErrors(errors) => write!(f, "{}", errors),
        }
    }
}

impl Debug for KaonError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CompilerError(error) => write!(f, "{error}"),
            Self::ParserError(error) => write!(f, "{error}"),
            Self::RuntimeError(error) => write!(f, "{error}"),
            Self::InvalidScriptPath(path) => {
                write!(f, "the path '{path}' could not be found")
            }
            Self::MultipleErrors(errors) => write!(f, "{errors}"),
        }
    }
}

pub struct KaonSettings {
    pub stdin: Rc<dyn KaonFile>,
    pub stdout: Rc<dyn KaonFile>,
    pub stderr: Rc<dyn KaonFile>,
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
/// ## Examples
/// ```rust
/// use kaon_lang::{Kaon, KaonError, Value};
///
/// fn main() -> Result<(), KaonError> {
///     let mut kaon = Kaon::new();
///
///     let script = "io.println(1 + 2)";
///     
///     match kaon.run_from_script(script) {
///         Ok(value) => println!("{}", value), // 3
///         Err(err) => println!("{}", err),
///     }
///     
///     Ok(())
/// }
/// ```
pub struct Kaon {
    pub vm: Vm,
    chunk: Function,
}

impl Default for Kaon {
    fn default() -> Self {
        Kaon::new()
    }
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

        let scope = self.type_check(&ast)?;

        let mut compiler = compiler::Compiler::default();
        let bytecode = compiler.run(&ast, scope);

        match bytecode {
            Ok(bytecode) => {
                self.chunk = bytecode.clone();
                Ok(bytecode)
            }
            Err(err) => panic!("{:?}", err),
        }
    }

    /// Compile script with provided scope.
    pub fn compile_with_scope(
        &mut self,
        scope: &mut Scope,
        script: &str,
    ) -> Result<(Function, Scope), KaonError> {
        let source = Source::contents(script);
        let tokens = self.tokenize(source)?;
        let ast = self.parse(tokens)?;

        let mut resolver = Resolver::with_scope(scope);
        resolver.resolve_ast(&ast);

        let globals = resolver.global_scope();

        if !resolver.errors.is_empty() {
            return Err(KaonError::MultipleErrors(Errors::from(resolver.errors)));
        }

        let mut compiler = compiler::Compiler::default();
        let bytecode = compiler.run(&ast, resolver.global_scope());

        match bytecode {
            Ok(bytecode) => {
                self.chunk = bytecode.clone();
                Ok((bytecode, globals))
            }
            Err(err) => panic!("{:?}", err),
        }
    }

    /// Compiles a [Function] from an [AST] with the supplied [Scope].
    ///
    /// # Examples
    ///
    /// ```
    /// use kaon_lang::{Kaon, Source};
    ///
    /// let mut kaon = Kaon::new();
    ///
    /// let tokens = kaon.tokenize(Source::contents("1 + 2")).unwrap();
    /// let ast = kaon.parse(tokens).unwrap();
    ///
    /// assert!(kaon.compile_ast(ast).is_ok());
    /// ```
    ///
    /// # Errors
    ///
    /// This function will return an error if the [AST] is not semantically correct.
    pub fn compile_ast(
        &mut self,
        ast: AST,
        scope: &mut Scope,
    ) -> Result<(Function, Scope), KaonError> {
        let mut resolver = Resolver::with_scope(scope);
        resolver.resolve_ast(&ast);

        let globals = resolver.global_scope();

        if !resolver.errors.is_empty() {
            return Err(KaonError::MultipleErrors(Errors::from(resolver.errors)));
        }

        let mut compiler = compiler::Compiler::default();
        let bytecode = compiler.run(&ast, resolver.global_scope());

        match bytecode {
            Ok(bytecode) => {
                self.chunk = bytecode.clone();
                Ok((bytecode, globals))
            }
            Err(err) => Err(KaonError::CompilerError(err.0)),
        }
    }

    /// Run a chunk of bytecode.
    pub fn run(&mut self) -> Result<Value, KaonError> {
        self.vm
            .interpret(Rc::new(self.chunk.clone()))
            .map_err(KaonError::RuntimeError)
    }

    /// Compile and run from a script.
    pub fn run_from_script(&mut self, script: &str) -> Result<Value, KaonError> {
        self.compile(script)?;
        self.run()
    }

    /// Run from a [Source]
    pub fn run_from_source(&mut self, source: Rc<Source>) -> Result<Value, KaonError> {
        self.compile_from_source(source)?;
        self.run()
    }

    /// Compile and run a script with the provided scope.
    pub fn run_with_scope(
        &mut self,
        scope: &mut Scope,
        script: &str,
    ) -> Result<(Value, Scope), KaonError> {
        let scope = self.compile_with_scope(scope, script)?.1;
        let value = self.run()?;

        Ok((value, scope))
    }

    /// Generate an [AST] from a script.
    pub fn parse_from_script(&self, script: &str) -> Result<AST, KaonError> {
        let source = Source::contents(script);
        let tokens = self.tokenize(source)?;
        self.parse(tokens)
    }

    /// Parse a stream of [Token]s into an [AST].
    pub fn parse(&self, tokens: Spanned<Vec<Token>>) -> Result<AST, KaonError> {
        let mut parser = compiler::Parser::new(tokens);
        let ast = parser.parse();
        match ast {
            Ok(ast) => Ok(ast),
            Err(err) => Err(KaonError::ParserError(err)),
        }
    }

    /// Tokenize a script.
    pub fn tokenize(&self, source: Rc<Source>) -> Result<Spanned<Vec<Token>>, KaonError> {
        let mut lexer = compiler::Lexer::new(source);
        let tokens = lexer.tokenize();
        match tokens {
            Ok(token_stream) => Ok(token_stream),
            Err(err) => Err(KaonError::ParserError(err)),
        }
    }

    /// Type check an [AST].  
    pub fn type_check(&mut self, ast: &AST) -> Result<Scope, KaonError> {
        let mut resolver = Resolver::default();
        resolver.resolve_ast(ast);

        if !resolver.errors.is_empty() {
            return Err(KaonError::MultipleErrors(Errors::from(resolver.errors)));
        }

        let mut typechecker = TypeChecker::new();
        typechecker.check_ast(ast);

        if !typechecker.errors.is_empty() {
            return Err(KaonError::MultipleErrors(Errors::from(typechecker.errors)));
        }

        Ok(resolver.global_scope())
    }

    /// Read a file from provided path.
    pub fn read_file(&self, path: PathBuf) -> Result<Rc<Source>, KaonError> {
        Source::from_file(path.to_str().unwrap())
            .map_err(|_| KaonError::InvalidScriptPath(path.to_str().unwrap().to_string()))
    }

    /// Access the VM's prelude
    pub fn prelude(&self) -> ValueMap {
        self.vm.prelude()
    }
}
