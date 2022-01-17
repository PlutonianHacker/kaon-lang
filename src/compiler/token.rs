use crate::common::{Source, Span};

use std::fmt;
use std::fmt::Display;
use std::rc::Rc;

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

impl TokenType {
    pub fn symbol(sym: &str) -> TokenType {
        TokenType::Symbol(sym.to_string())
    }

    pub fn keyword(keyword: &str) -> TokenType {
        TokenType::Keyword(keyword.to_string())
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match &*self {
            TokenType::Keyword(keyword) => write!(f, "{}", keyword),
            TokenType::Symbol(keyword) => write!(f, "{}", keyword),
            TokenType::Number => write!(f, "{{number}}"),
            TokenType::String => write!(f, "{{string}}"),
            TokenType::Id => write!(f, "{{identifier}}"),
            TokenType::Newline => write!(f, "<newline>"),
            TokenType::Eof => write!(f, "<eof>"),
        }
    }
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

    pub fn empty() -> Self {
        Token {
            token_val: "".to_string(),
            token_type: TokenType::Number,
            span: Span::empty(),
        }
    }

    /// helper function for end-of-file token
    pub fn eof(start: usize, source: &Rc<Source>) -> Token {
        Token {
            token_val: "<eof>".to_string(),
            token_type: TokenType::Eof,
            span: Span::new(start, 1, source),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::common::{Source, Span};
    use crate::compiler::{Token, TokenType};

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