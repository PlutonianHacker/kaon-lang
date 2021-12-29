use crate::source::Source;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    source: Rc<Source>,
    start: usize,
    length: usize,
}

impl Span {
    pub fn new(start: usize, length: usize, source: &Rc<Source>) -> Self {
        Span {
            start,
            length,
            source: source.clone(),
        }
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
    pub fn new(token_val: String, token_type: TokenType, span: Span) -> Self {
        Token {
            token_val,
            token_type,
            span,
        }
    }

    /// helper function for end-of-file token
    pub fn eof(start: usize, source: &Rc<Source>) -> Token {
        Token {
            token_val: "<eof>".to_string(),
            token_type: TokenType::Eof,
            span: Span::new(start, 0, source),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::token::Source;
    use crate::token::{Span, Token, TokenType};

    #[test]
    fn test_token() {
        let token = Token::new(
            "+".to_string(),
            TokenType::Symbol("+".to_string()),
            Span::new(0, 1, &Source::contents("+")),
        );
        assert_eq!(
            token,
            Token {
                token_val: "+".to_string(),
                token_type: TokenType::Symbol("+".to_string()),
                span: Span::new(0, 1, &Source::contents("+"))
            }
        );
    }
}
