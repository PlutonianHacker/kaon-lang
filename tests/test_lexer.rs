use kaon_lang::lexer::Lexer;
use kaon_lang::token::{Token, TokenType};

fn new_lexer(input: Vec<char>) -> Lexer {
    Lexer::new(input)
}

#[test]
fn tokenize_number() {
    let mut lexer = new_lexer(['7'].to_vec());
    let token = lexer.tokenize();
    assert_eq!(token, Ok(Token::new("7".to_string(), TokenType::Number)));
}

#[test]
fn tokenize_numbers() {
    let mut lexer = new_lexer(vec!['5', '4', '3']);
    let token = lexer.tokenize();
    assert_eq!(token, Ok(Token::new("543".to_string(), TokenType::Number)));
}

#[test]
fn tokenize_whitespace() {
    let mut lexer = new_lexer(vec![' ', ' ', ' ', ' ']);
    let token = lexer.tokenize();
    assert_eq!(token, Ok(Token::new("eof".to_string(), TokenType::Eof)));
}

#[test]
fn tokenize_sym() {
    let mut lexer = new_lexer(vec!['+', '-', '*', '/', '(', ')']);
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("+".to_string(), TokenType::Add))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("-".to_string(), TokenType::Sub))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("*".to_string(), TokenType::Mul))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("/".to_string(), TokenType::Div))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("(".to_string(), TokenType::LParen))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new(")".to_string(), TokenType::RParen))
    );
}

#[test]
fn tokenize_eof() {
    let mut lexer = new_lexer(vec![' ']);
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("eof".to_string(), TokenType::Eof))
    );
}

#[test]
fn tokenize_bin_op() {
    let mut lexer = new_lexer(vec!['1', ' ', '+', ' ', '2']);
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("1".to_string(), TokenType::Number))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("+".to_string(), TokenType::Add))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("2".to_string(), TokenType::Number))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("eof".to_string(), TokenType::Eof))
    );
}
