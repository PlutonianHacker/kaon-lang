use kaon_lang::common::{Value, Source};
use kaon_lang::compiler::{Compiler, Lexer, Parser, SemanticAnalyzer, AST, Scope};

/*fn new_parser(src: &str) -> (AST, Scope) {
    let source = Source::new(src, "./main");
    let tokens = Lexer::new(source).tokenize().unwrap();

    //let mut analyzer = SemanticAnalyzer::new();
    let ast = Parser::new(tokens).parse().unwrap();
    return (ast, analyzer.current_scope);
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
    let (ast, globals) = new_parser(src);
    let mut compiler = new_compiler();
    assert_eq!(compiler.run(&ast, globals).is_ok(), true);
}

#[test]
fn compile_number() {
    let (ast, scope) = new_parser("123");
    let mut compiler = new_compiler();
    let chunk = compiler.run(&ast, scope).unwrap().chunk;
    assert_eq!(chunk.opcodes, vec![0, 0, 29, 33]);
    assert_eq!(chunk.constants, vec![Data::Number(123.0)]);
}

#[test]
fn compile_binary() {
    let (ast, scope) = new_parser("1 + 2");
    let mut compiler = new_compiler();
    let chunk = compiler.run(&ast, scope).unwrap().chunk;
    assert_eq!(chunk.opcodes, vec![0, 0, 0, 1, 3, 29, 33]);
    assert_eq!(chunk.constants, vec![Data::Number(2.0), Data::Number(1.0)]);
}

#[test]
fn compile_unary() {
    let (ast, scope) = new_parser("-8");
    let mut compiler = new_compiler();
    let chunk = compiler.run(&ast, scope).unwrap().chunk;
    assert_eq!(chunk.opcodes, vec![0, 0, 8, 29, 33]);
    assert_eq!(chunk.constants, vec![Data::Number(8.0)]);
}
*/