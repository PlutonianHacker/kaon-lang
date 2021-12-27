#[derive(Debug, PartialEq)]
pub struct Span {
    start: usize,
    length: usize,
}

impl Span {
    pub fn new(start: usize, length: usize) -> Self {
        Span { start, length }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Keyword(String),
    Symbol(String),
    Number,
    String,
    Id,
    Newline,
    Eof,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    token_type: TokenType,
    token_val: String,
    span: Span,
}

impl Token {
    pub fn new(token_val: String, token_type: TokenType, start: usize, length: usize) -> Self {
        Token {
            token_val,
            token_type,
            span: Span::new(start, length),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::token::{Span, Token, TokenType};

    #[test]
    fn test_token() {
        let token = Token::new("+".to_string(), TokenType::Symbol("+".to_string()), 0, 1);
        assert_eq!(
            token,
            Token {
                token_val: "+".to_string(),
                token_type: TokenType::Symbol("+".to_string()),
                span: Span::new(0, 1)
            }
        );
    }
}
