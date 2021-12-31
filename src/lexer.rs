use std::rc::Rc;

use crate::error::SyntaxError;
use crate::source::Source;
use crate::span::{Span, Spanned};
use crate::token::Token;
use crate::token::TokenType;

pub struct Lexer {
    source: Rc<Source>,
    current: usize,
}

impl Lexer {
    pub fn new(source: Rc<Source>) -> Self {
        Lexer { source, current: 0 }
    }

    fn remaining(&mut self) -> &str {
        &self.source.contents[self.current..]
    }

    fn advance(&mut self) -> Option<&str> {
        if self.remaining().is_empty() {
            None
        } else {
            let source = &self.source.contents[self.current..];
            let mut end = 1;
            while !source.is_char_boundary(end) {
                end += 1;
            }

            self.current += end;

            Some(&source[0..end])
        }
    }

    fn peek(&mut self) -> Option<&str> {
        if self.remaining().is_empty() {
            None
        } else {
            let source = &self.source.contents[self.current..];
            let mut end = 1;
            while !source.is_char_boundary(end) {
                end += 1;
            }

            Some(&source[0..end])
        }
    }

    fn is_alpha(string: &str) -> bool {
        string
            .bytes()
            .all(|c| matches!(c, b'a'..=b'z'|b'A'..=b'Z'|b'_' ))
    }

    fn is_number(string: &str) -> bool {
        string.as_bytes()[0].is_ascii_digit()
    }

    fn is_whitespace(string: &str) -> bool {
        string.contains(char::is_whitespace)
    }

    fn ident(&mut self) -> Result<Token, SyntaxError> {
        let mut res: String = String::new();
        let mut c = self.advance();
        while c.is_some() && Lexer::is_alpha(c.unwrap()) {
            res.push_str(c.unwrap());
            c = self.advance();
        }
        match &res[..] {
            "true" | "false" => Ok(Token::new(
                res.to_string(),
                TokenType::Keyword(res.to_string()),
                Span::new(self.current - &res.len(), res.len(), &self.source),
            )),
            _ => Ok(Token::new(
                res.to_string(),
                TokenType::Id,
                Span::new(self.current - &res.len(), res.len(), &self.source),
            )),
        }
    }

    fn number(&mut self) -> Result<Token, SyntaxError> {
        let mut res: String = String::new();

        let mut c = self.advance();
        while c.is_some() && Lexer::is_number(c.unwrap()) {
            res.push_str(c.unwrap());
            c = self.advance();
        }

        Ok(Token::new(
            res.to_string(),
            TokenType::Number,
            Span::new(self.current - &res.len(), res.len(), &self.source),
        ))
    }

    fn string(&mut self) -> Result<Token, SyntaxError> {
        let mut res: String = String::new();
        let mut c = self.advance();
        while c != Some("\"") {
            if c.is_none() {
                return Err(SyntaxError::error(
                    "Syntax Error: unterminated string",
                    &Span::new(0, self.source.contents.len(), &self.source),
                ));
            }
            res.push_str(c.unwrap());
            c = self.advance();
        }
        let start = self.current - &res.len();
        let length = &res.len();

        Ok(Token::new(
            res,
            TokenType::String,
            Span::new(start, *length, &self.source),
        ))
    }

    fn make_token(&mut self, token_val: &str, token_type: TokenType) -> Token {
        for _ in 0..token_val.len() {
            self.advance();
        }

        let token = Token::new(
            token_val.to_string(),
            token_type,
            Span::new(
                self.current - token_val.len(),
                token_val.len(),
                &self.source,
            ),
        );
        return token;
    }

    pub fn tokenize(&mut self) -> Result<Spanned<Vec<Token>>, SyntaxError> {
        let mut tokens = vec![];
        loop {
            tokens.push(match self.peek() {
                Some("+") => self.make_token("+", TokenType::Symbol("+".to_string())),
                Some("-") => self.make_token("-", TokenType::Symbol("-".to_string())),
                Some("*") => self.make_token("*", TokenType::Symbol("*".to_string())),
                Some("/") => self.make_token("/", TokenType::Symbol("/".to_string())),
                Some("\n") => self.make_token("\\n", TokenType::Newline),
                Some("\"") => self.string()?,
                None => {
                    tokens.push(Token::eof(self.current, &self.source));
                    break;
                }
                c if Lexer::is_alpha(c.unwrap()) => self.ident()?,
                c if Lexer::is_number(c.unwrap()) => self.number()?,
                c if Lexer::is_whitespace(c.unwrap()) => {
                    self.advance();
                    continue;
                }
                c => {
                    return Err(SyntaxError::error(
                        &format!("Syntax Error: unexpected token `{}`", c.unwrap(),),
                        &Span::new(0, self.source.contents.len(), &self.source),
                    ))
                }
            });
        }
        Ok(Spanned::new(
            tokens,
            Span::new(0, self.source.contents.len(), &self.source),
        ))
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::{Lexer, Token, TokenType};
    use crate::source::Source;
    use crate::span::Span;
    use std::path::PathBuf;

    #[test]
    fn test_lexer() {
        let source = Source::new("123 + 456", &PathBuf::from("./hello.kaon"));
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            [
                Token::new(
                    "123".to_string(),
                    TokenType::Number,
                    Span::new(
                        1,
                        3,
                        &Source::new("123 + 456", &PathBuf::from("./hello.kaon"))
                    )
                ),
                Token::new(
                    "+".to_string(),
                    TokenType::Symbol("+".to_string()),
                    Span::new(
                        4,
                        1,
                        &Source::new("123 + 456", &PathBuf::from("./hello.kaon"))
                    )
                ),
                Token::new(
                    "456".to_string(),
                    TokenType::Number,
                    Span::new(
                        6,
                        3,
                        &Source::new("123 + 456", &PathBuf::from("./hello.kaon"))
                    )
                ),
                Token::new(
                    "<eof>".to_string(),
                    TokenType::Eof,
                    Span::new(
                        9,
                        0,
                        &Source::new("123 + 456", &PathBuf::from("./hello.kaon"))
                    )
                ),
            ]
        )
    }
}
