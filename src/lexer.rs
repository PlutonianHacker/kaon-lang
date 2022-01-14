use std::rc::Rc;

use crate::error::error::SyntaxError;
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
        let mut c = self.peek();
        while c.is_some() && Lexer::is_alpha(c.unwrap()) {
            res.push_str(self.advance().unwrap());
            c = self.peek();
        }
        match &res[..] {
            "true" | "false" | "is" | "isnt" | "and" | "or" | "if" | "else" | "var" | "loop"
            | "while" | "break" => Ok(Token::new(
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

        let mut c = self.peek();
        while c.is_some() && Lexer::is_number(c.unwrap()) {
            res.push_str(self.advance().unwrap());
            c = self.peek();
        }

        Ok(Token::new(
            res.to_string(),
            TokenType::Number,
            Span::new(self.current - &res.len(), res.len(), &self.source),
        ))
    }

    fn string(&mut self) -> Result<Token, SyntaxError> {
        let mut res: String = String::new();
        self.advance();
        let mut c = self.peek();
        while c != Some("\"") {
            if c.is_none() {
                return Err(SyntaxError::error(
                    "Syntax Error: unterminated string",
                    &Span::new(0, self.source.contents.len(), &self.source),
                ));
            }
            res.push_str(self.advance().unwrap());
            c = self.peek();
        }
        self.advance();

        let start = self.current - &res.len();
        let length = &res.len();

        Ok(Token::new(
            res,
            TokenType::String,
            Span::new(start, *length, &self.source),
        ))
    }

    fn newline(&mut self) -> Token {
        let mut c = self.peek();
        while c.is_some() && c == Some("\n") {
            self.advance();
            c = self.peek();
        }

        Token::new(
            "\\n".to_string(),
            TokenType::Newline,
            Span::new(self.current, 1, &self.source),
        )
    }

    fn make_token(&mut self, token_val: &str, token_type: TokenType) -> Token {
        self.advance();

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
                Some("+") => self.make_token("+", TokenType::symbol("+")),
                Some("-") => self.make_token("-", TokenType::symbol("-")),
                Some("*") => self.make_token("*", TokenType::symbol("*")),
                Some("/") => self.make_token("/", TokenType::symbol("/")),
                Some("(") => self.make_token("(", TokenType::symbol("(")),
                Some(")") => self.make_token(")", TokenType::symbol(")")),
                Some("{") => self.make_token("{", TokenType::symbol("{")),
                Some("}") => self.make_token("}", TokenType::symbol("}")),
                Some("[") => self.make_token("[", TokenType::symbol("[")),
                Some("]") => self.make_token("]", TokenType::symbol("]")),
                Some(",") => self.make_token(",", TokenType::symbol(",")),
                Some(".") => self.make_token(".", TokenType::symbol(".")),
                Some("=") => self.make_token("=", TokenType::symbol("=")),
                Some(">") => {
                    self.advance();
                    if self.peek() == Some("=") {
                        self.advance();
                        Token::new(
                            ">=".to_string(),
                            TokenType::symbol(">="),
                            Span::new(self.current - 1, 2, &self.source),
                        )
                    } else {
                        self.make_token(">", TokenType::symbol(">"))
                    }
                }
                Some("<") => {
                    self.advance();
                    if self.peek() == Some("=") {
                        self.advance();
                        Token::new(
                            "<=".to_string(),
                            TokenType::symbol("<="),
                            Span::new(self.current - 1, 2, &self.source),
                        )
                    } else {
                        self.make_token("<", TokenType::symbol("<"))
                    }
                }
                Some("%") => self.make_token("%", TokenType::symbol("%")),
                Some("!") => self.make_token("!", TokenType::symbol("!")),
                Some("\n") => self.newline(),
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

    #[test]
    fn test_lexer() {
        let source = Source::new("123 + 456", "./hello.kaon");
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens.node,
            [
                Token::new(
                    "123".to_string(),
                    TokenType::Number,
                    Span::new(0, 3, &Source::new("123 + 456", "./hello.kaon"))
                ),
                Token::new(
                    "+".to_string(),
                    TokenType::Symbol("+".to_string()),
                    Span::new(4, 1, &Source::new("123 + 456", "./hello.kaon"))
                ),
                Token::new(
                    "456".to_string(),
                    TokenType::Number,
                    Span::new(6, 3, &Source::new("123 + 456", "./hello.kaon"))
                ),
                Token::new(
                    "<eof>".to_string(),
                    TokenType::Eof,
                    Span::new(9, 1, &Source::new("123 + 456", "./hello.kaon"))
                ),
            ]
        )
    }
}
