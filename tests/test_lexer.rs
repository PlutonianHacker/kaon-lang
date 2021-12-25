use kaon_lang::lexer::Lexer;
use kaon_lang::token::{Token, TokenType};

fn new_lexer(input: Vec<char>) -> Lexer {
    Lexer::new(input)
}

#[test]
fn tokenize_number() {
    let mut lexer = new_lexer(['7'].to_vec());
    let token = lexer.tokenize();
    assert_eq!(token, Ok(Token::new("7".to_string(), TokenType::Number, 0, 0)));
}

#[test]
fn tokenize_numbers() {
    let mut lexer = new_lexer(vec!['5', '4', '3']);
    let token = lexer.tokenize();
    assert_eq!(token, Ok(Token::new("543".to_string(), TokenType::Number, 0, 2)));
}

#[test]
fn tokenize_whitespace() {
    let mut lexer = new_lexer(vec![' ', ' ', ' ', ' ']);
    let token = lexer.tokenize();
    assert_eq!(token, Ok(Token::new("eof".to_string(), TokenType::Eof, 0, 0)));
}

#[test]
fn tokenize_sym() {
    let mut lexer = new_lexer(vec!['+', '-', '*', '/', '(', ')']);
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("+".to_string(), TokenType::Add, 0, 0))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("-".to_string(), TokenType::Sub, 1, 1))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("*".to_string(), TokenType::Mul, 2, 2))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("/".to_string(), TokenType::Div, 3, 3))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("(".to_string(), TokenType::LParen, 4, 4))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new(")".to_string(), TokenType::RParen, 5, 5))
    );
}

#[test]
fn tokenize_eof() {
    let mut lexer = new_lexer(vec![' ']);
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("eof".to_string(), TokenType::Eof, 0, 0))
    );
}

#[test]
fn tokenize_bin_op() {
    let mut lexer = new_lexer(vec!['1', ' ', '+', ' ', '2']);
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("1".to_string(), TokenType::Number, 0, 0))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("+".to_string(), TokenType::Add, 2, 2))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("2".to_string(), TokenType::Number, 4, 4))
    );
    assert_eq!(
        lexer.tokenize(),
        Ok(Token::new("eof".to_string(), TokenType::Eof, 5, 5))
    );
}
