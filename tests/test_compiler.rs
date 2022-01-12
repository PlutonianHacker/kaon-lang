use kaon_lang::analysis::SemanticAnalyzer;
use kaon_lang::ast::AST;
use kaon_lang::compiler::Compiler;
use kaon_lang::data::Data;
use kaon_lang::error::SyntaxError;
use kaon_lang::{lexer::Lexer, parser::Parser, source::Source};

use std::path::PathBuf;

fn new_parser(src: &str) -> Result<AST, SyntaxError> {
    let source = Source::new(src, &PathBuf::from("./main"));
    let tokens = Lexer::new(source).tokenize().unwrap();

    let mut analyzer = SemanticAnalyzer::new();
    let ast = Parser::new(tokens).parse(&mut analyzer);
    return ast;
}

fn new_compiler() -> Compiler {
    Compiler::build()
}

#[test]
fn test_compiler() {
    let src = r#"
        1 + 2
        5 * (10 - 3)
        -4 - -8
    "#;
    let ast = new_parser(src).unwrap();
    let mut compiler = new_compiler();
    assert_eq!(compiler.run(&ast).is_ok(), true);
}

#[test]
fn compile_number() {
    let ast = new_parser("123").unwrap();
    let mut compiler = new_compiler();
    let chunk = compiler.run(&ast).unwrap();
    assert_eq!(chunk.opcodes, vec![0, 0, 24, 27]);
    assert_eq!(chunk.constants, vec![Data::Number(123.0)]);
}

#[test]
fn compile_binary() {
    let ast = new_parser("1 + 2").unwrap();
    let mut compiler = new_compiler();
    let chunk = compiler.run(&ast).unwrap();
    assert_eq!(chunk.opcodes, vec![0, 0, 0, 1, 1, 24, 27]);
    assert_eq!(chunk.constants, vec![Data::Number(2.0), Data::Number(1.0)]);
}

#[test]
fn compile_unary() {
    let ast = new_parser("-8").unwrap();
    let mut compiler = new_compiler();
    let chunk = compiler.run(&ast).unwrap();
    assert_eq!(chunk.opcodes, vec![0, 0, 6, 24, 27]);
    assert_eq!(chunk.constants, vec![Data::Number(8.0)]);
}
