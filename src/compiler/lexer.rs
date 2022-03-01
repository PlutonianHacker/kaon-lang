use std::char;
use std::rc::Rc;

use crate::common::Source;
use crate::common::{Span, Spanned};
use crate::compiler::{Token, TokenType, token::KEYWORDS};
use crate::error::{Error, Item};

pub struct Lexer {
    source: Rc<Source>,
    previous: usize,
    current: usize,
}

impl Lexer {
    pub fn new(source: Rc<Source>) -> Self {
        Lexer {
            source,
            previous: 0,
            current: 0,
        }
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

    fn match_(&mut self, lexeme: &str) -> bool {
        if self.peek().is_some() && self.peek().unwrap() == lexeme {
            self.advance();
            true
        } else {
            false
        }
    }

    fn ident(&mut self) -> Result<Token, Error> {
        while self.peek().is_some() && Lexer::is_alpha(self.peek().unwrap()) || Lexer::is_number(self.peek().unwrap()) {
            self.advance();
        }

        let (name, typ) = self.keyword();
        let token = self.make_token(&name, typ);

        Ok(token)
    }

    fn keyword(&mut self) -> (String, TokenType) {
        let value = &self.source.contents[self.previous..self.current];
        match KEYWORDS.iter().find(|k| *k == &value) {
            Some(value) => (value.to_string(), TokenType::keyword(value)),
            None => (value.to_string(), TokenType::Id),
        }
    }

    fn number(&mut self) -> Result<Token, Error> {
        while self.peek().is_some() && Lexer::is_number(self.peek().unwrap()) {
            self.advance();
        }

        if self.peek() == Some(".") {
            self.advance();
            while self.peek().is_some() && Lexer::is_number(self.peek().unwrap()) {
                self.advance();
            }
        }

        if self.peek() == Some("e") {
            self.advance();
            if self.peek() == Some("-") || self.peek() == Some("+") {
                self.advance();
            }
            while self.peek().is_some() && Lexer::is_number(self.peek().unwrap()) {
                self.advance();
            }
        }

        let value = self.source.contents[self.previous..self.current].to_string();

        let token = self.make_token(&value, TokenType::Number);

        Ok(token)
    }

    fn string(&mut self) -> Result<Token, Error> {
        let mut escape = false;
        let mut string = "".to_string();
        let mut len = 0;

        let mut source = self.source.contents[self.current..].chars();

        while let Some(c) = source.next() {
            len += c.len_utf8();
            if escape {
                escape = false;
                string.push(match c {
                    '"' => '"',
                    '\\' => '\\',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'u' => {
                        let mut sub_string =
                            self.source.contents[self.previous + len + 1..].chars();

                        if sub_string.next() != Some('{') {
                            break;
                        }

                        // TODO: replace unwrap with error handling
                        let mut value: u32 = sub_string.next().unwrap().to_digit(16).unwrap();

                        let mut digits = 1;
                        loop {
                            match sub_string.next().unwrap() {
                                '}' => break,
                                c => {
                                    let digit = c.to_digit(16).unwrap();
                                    digits += 1;
                                    if digits > 6 {
                                        continue;
                                    }
                                    let digit = digit as u32;
                                    value = value * 16 + digit;
                                }
                            }
                        }

                        for _ in 0..digits + 2 {
                            len += 1;
                            source.next();
                        }

                        char::from_u32(value).unwrap()
                    }
                    o => {
                        return Err(Error::UnknownEscapeCode(Item::new(
                            &o.to_string(),
                            Span::new(
                                self.previous + len,
                                self.current - self.previous,
                                &self.source,
                            ),
                        )))
                    }
                });
            } else {
                match c {
                    '\\' => escape = true,
                    '"' => {
                        for _ in 0..len {
                            self.advance();
                        }

                        return Ok(self.make_token(&string, TokenType::String));
                    }
                    c => string.push(c),
                }
            }
        }

        Err(Error::UnterminatedString(Item::new(
            &string,
            Span::new(self.current_span().start, string.len() + 1, &self.source),
        )))
    }

    fn single_line_comment(&mut self) -> Token {
        while self.peek().is_some() && self.peek() != Some("\n") {
            self.advance();
        }

        let value = self.source.contents[self.previous + 2..self.current].to_string();
        self.make_token(&value, TokenType::comment("//"))
    }

    fn newline(&mut self) -> Token {
        while self.peek().is_some() && self.peek() == Some("\n") {
            self.advance();
        }

        self.make_token("\\n", TokenType::Newline)
    }

    fn make_token(&mut self, token_val: &str, token_type: TokenType) -> Token {
        let token = Token::new(token_val.to_string(), token_type, self.current_span());
        self.previous = self.current;
        token
    }

    fn current_span(&mut self) -> Span {
        Span::new(self.previous, self.current - self.previous, &self.source)
    }

    pub fn tokenize(&mut self) -> Result<Spanned<Vec<Token>>, Error> {
        let mut tokens = vec![];
        loop {
            let c = self.advance();
            tokens.push(match c {
                Some("+") => self.make_token("+", TokenType::symbol("+")),
                Some("-") => self.make_token("-", TokenType::symbol("-")),
                Some("*") => self.make_token("*", TokenType::symbol("*")),
                Some("%") => self.make_token("%", TokenType::symbol("%")),
                Some("~") => self.make_token("~", TokenType::symbol("~")),
                Some(":") => self.make_token(":", TokenType::symbol(":")),
                Some("(") => self.make_token("(", TokenType::symbol("(")),
                Some(")") => self.make_token(")", TokenType::symbol(")")),
                Some("{") => self.make_token("{", TokenType::symbol("{")),
                Some("}") => self.make_token("}", TokenType::symbol("}")),
                Some("[") => self.make_token("[", TokenType::symbol("[")),
                Some("]") => self.make_token("]", TokenType::symbol("]")),
                Some(",") => self.make_token(",", TokenType::symbol(",")),
                Some(".") => self.make_token(".", TokenType::symbol(".")),
                Some("=") => {
                    if self.match_("=") {
                        self.make_token("==", TokenType::symbol("=="))
                    } else {
                        self.make_token("=", TokenType::symbol("="))
                    }
                }
                Some("!") => {
                    if self.match_("=") {
                        self.make_token("!=", TokenType::symbol("!="))
                    } else {
                        self.make_token("!", TokenType::symbol("!"))
                    }
                }
                Some("/") => {
                    if self.match_("/") {
                        self.single_line_comment()
                    } else {
                        self.make_token("/", TokenType::symbol("/"))
                    }
                }
                Some(">") => {
                    if self.match_("=") {
                        self.make_token(">=", TokenType::symbol(">="))
                    } else {
                        self.make_token(">", TokenType::symbol(">"))
                    }
                }
                Some("<") => {
                    if self.match_("=") {
                        self.make_token("<=", TokenType::symbol("<="))
                    } else {
                        self.make_token("<", TokenType::symbol("<"))
                    }
                }
                Some("\n") => self.newline(),
                Some("\"") => self.string()?,
                None => {
                    tokens.push(Token::eof(self.current, &self.source));
                    break;
                }
                c if Lexer::is_alpha(c.unwrap()) => self.ident()?,
                c if Lexer::is_number(c.unwrap()) => self.number()?,
                c if Lexer::is_whitespace(c.unwrap()) => {
                    self.previous = self.current;
                    continue;
                }
                c => {
                    return Err(Error::UnexpectedToken(Item::new(
                        &c.unwrap().to_string(),
                        self.current_span(),
                    )))
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
    use crate::common::Source;
    use crate::common::Span;
    use crate::compiler::{Lexer, Token, TokenType};

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
