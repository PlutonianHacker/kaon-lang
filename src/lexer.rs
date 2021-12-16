use crate::token::Token;
use crate::token::TokenType;

pub struct Lexer {
    pos: usize,
    src: Vec<char>,
    pub eof: bool,
}

impl Lexer {
    pub fn new(src: Vec<char>) -> Self {
        Lexer {
            pos: 0,
            src,
            eof: false,
        }
    }

    fn advance(&mut self) {
        let char = self.src.get(self.pos + 1);
        match char {
            Some(_) => self.pos += 1,
            None => self.eof = true,
        }
    }

    pub fn tokenize_number(&mut self) -> String {
        let mut res = String::new();
        while !self.eof && self.src[self.pos].is_numeric() {
            res.push(self.src[self.pos]);
            self.advance();
        }

        res
    }

    fn make_token(&mut self, val: &str, token_type: TokenType) -> Result<Token, String> {
        self.advance();
        Ok(Token::new(val.to_string(), token_type))
    }

    pub fn tokenize(&mut self) -> Result<Token, String> {
        if self.eof {
            return Ok(Token::new("eof".to_string(), TokenType::Eof));
        }

        match self.src[self.pos] {
            val if val == '\n' => self.make_token("\n", TokenType::NewLn),
            val if val.is_whitespace() => {
                self.advance();
                self.tokenize()
            }
            val if val.is_numeric() => Ok(Token::new(self.tokenize_number(), TokenType::Number)),
            val => {
                let sym = TokenType::from(val.to_string());
                self.advance();
                Ok(Token::new(val.to_string(), sym))
            }
        }
    }
}
