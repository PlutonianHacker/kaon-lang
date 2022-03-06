use kaon_lang::common::{Source, Span};
use kaon_lang::compiler::token::Literal;
use kaon_lang::compiler::{Lexer, TokenType};

use std::rc::Rc;

fn new_lexer(input: &str) -> (Lexer, Rc<Source>) {
    let source = Source::new(input, "./main");
    (Lexer::new(source.clone()), source)
}

#[test]
fn tokenize_number() {
    let (mut lexer, source) = new_lexer("7");
    let token = lexer.tokenize().unwrap();
    assert_eq!(
        token.node[0],
        (
            TokenType::Literal(Literal::NumberLiteral("7".to_string())),
            Span::new(0, 1, &source)
        )
    );
}

#[test]
fn tokenize_numbers() {
    let (mut lexer, source) = new_lexer("543");
    let token = lexer.tokenize().unwrap();
    assert_eq!(
        token.node[0],
        (
            TokenType::Literal(Literal::NumberLiteral("543".to_string())),
            Span::new(0, 3, &source)
        )
    );
}

#[test]
fn tokenize_whitespace() {
    let (mut lexer, source) = new_lexer(&" ".repeat(4));
    let token = lexer.tokenize().unwrap();
    assert_eq!(token.node[0], (TokenType::eof(), Span::new(4, 0, &source)));
}

#[test]
fn tokenize_sym() {
    let (mut lexer, source) = new_lexer("+-*/");
    let tokens = lexer.tokenize().unwrap().node;
    assert_eq!(
        tokens[0],
        (TokenType::symbol("+"), Span::new(0, 1, &source))
    );
    assert_eq!(
        tokens[1],
        (TokenType::symbol("-"), Span::new(1, 1, &source))
    );
    assert_eq!(
        tokens[2],
        (TokenType::symbol("*"), Span::new(3, 1, &source))
    );
    assert_eq!(
        tokens[3],
        (TokenType::symbol("/"), Span::new(4, 1, &source))
    );
}

#[test]
fn tokenize_eof() {
    let (mut lexer, source) = new_lexer(" ");
    assert_eq!(
        lexer.tokenize().unwrap().node[0],
        (TokenType::eof(), Span::new(1, 0, &source))
    );
}

#[test]
fn tokenize_bin_op() {
    let (mut lexer, source) = new_lexer("1 + 2");
    let tokens = lexer.tokenize().unwrap().node;
    assert_eq!(
        tokens[0],
        (
            TokenType::Literal(Literal::NumberLiteral("1".to_string())),
            Span::new(0, 1, &source)
        )
    );
    assert_eq!(
        tokens[1],
        (TokenType::symbol("+"), Span::new(2, 1, &source))
    );
    assert_eq!(
        tokens[2],
        (
            TokenType::Literal(Literal::NumberLiteral("2".to_string())),
            Span::new(4, 1, &source)
        )
    );
    assert_eq!(tokens[3], (TokenType::eof(), Span::new(5, 0, &source)));
}
