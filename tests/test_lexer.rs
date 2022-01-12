use kaon_lang::lexer::Lexer;
use kaon_lang::source::Source;
use kaon_lang::span::Span;
use kaon_lang::token::{Token, TokenType};

use std::path::PathBuf;
use std::rc::Rc;

fn new_lexer(input: &str) -> (Lexer, Rc<Source>) {
    let source = Source::new(input, &PathBuf::from("./main"));
    (Lexer::new(source.clone()), source)
}

#[test]
fn tokenize_number() {
    let (mut lexer, source) = new_lexer("7");
    let token = lexer.tokenize().unwrap();
    assert_eq!(
        token.node[0],
        Token::new("7".to_string(), TokenType::Number, Span::new(0, 1, &source))
    );
}

#[test]
fn tokenize_numbers() {
    let (mut lexer, source) = new_lexer("543");
    let token = lexer.tokenize().unwrap();
    assert_eq!(
        token.node[0],
        Token::new(
            "543".to_string(),
            TokenType::Number,
            Span::new(0, 3, &source)
        )
    );
}

#[test]
fn tokenize_whitespace() {
    let (mut lexer, source) = new_lexer(&" ".repeat(4));
    let token = lexer.tokenize().unwrap();
    assert_eq!(
        token.node[0],
        Token::new("<eof>".to_string(), TokenType::Eof, Span::new(4, 1, &source))
    );
}

#[test]
fn tokenize_sym() {
    let (mut lexer, source) = new_lexer("+-*/()");
    let tokens = lexer.tokenize().unwrap().node;
    assert_eq!(
        tokens[0],
        Token::new(
            "+".to_string(),
            TokenType::symbol("+"),
            Span::new(0, 1, &source)
        )
    );
    assert_eq!(
        tokens[1],
        Token::new(
            "-".to_string(),
            TokenType::symbol("-"),
            Span::new(1, 1, &source)
        )
    );
    assert_eq!(
        tokens[2],
        Token::new(
            "*".to_string(),
            TokenType::symbol("*"),
            Span::new(2, 1, &source)
        )
    );
    assert_eq!(
        tokens[3],
        Token::new(
            "/".to_string(),
            TokenType::symbol("/"),
            Span::new(3, 1, &source)
        )
    );
    assert_eq!(
        tokens[4],
        Token::new(
            "(".to_string(),
            TokenType::symbol("("),
            Span::new(4, 1, &source)
        )
    );
    assert_eq!(
        tokens[5],
        Token::new(
            ")".to_string(),
            TokenType::symbol(")"),
            Span::new(5, 1, &source)
        )
    );
}

#[test]
fn tokenize_eof() {
    let (mut lexer, source) = new_lexer(" ");
    assert_eq!(
        lexer.tokenize().unwrap().node[0],
        Token::new(
            "<eof>".to_string(),
            TokenType::Eof,
            Span::new(1, 1, &source)
        )
    );
}

#[test]
fn tokenize_bin_op() {
    let (mut lexer, source) = new_lexer("1 + 2");
    let tokens = lexer.tokenize().unwrap().node;
    assert_eq!(
        tokens[0],
        Token::new(
            "1".to_string(),
            TokenType::Number,
            Span::new(0, 1, &source)
        )
    );
    assert_eq!(
        tokens[1],
        Token::new(
            "+".to_string(),
            TokenType::symbol("+"),
            Span::new(2, 1, &source)
        )
    );
    assert_eq!(
        tokens[2],
        Token::new(
            "2".to_string(),
            TokenType::Number,
            Span::new(4, 1, &source)
        )
    );
    assert_eq!(
        tokens[3],
        Token::new(
            "<eof>".to_string(),
            TokenType::Eof,
            Span::new(5, 1, &source)
        )
    );
}
