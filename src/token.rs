#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    start: usize,
    length: usize,
}

impl Span {
    pub fn new(start: usize, length: usize) -> Self {
        Span { start, length }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Keyword(String),
    Symbol(String),
    Number,
    String,
    Id,
    Newline,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub token_val: String,
    pub span: Span,
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
