use crate::token::Token;
use crate::token::TokenType;

#[derive(Debug, PartialEq)]
pub struct SyntaxErr(pub String);

#[derive(Clone)]
pub struct Lexer {
    pos: usize,
    src: Vec<char>,
    tokens: Vec<Token>,
    pub eof: bool,
}

impl Lexer {
    pub fn new(src: Vec<char>) -> Self {
        Lexer {
            pos: 0,
            src,
            tokens: vec![],
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

    fn error(&mut self, lexeme: char) -> SyntaxErr {
        SyntaxErr(format!("Syntax Error: unexpected token '{}'", lexeme))
    }

    fn peek(&mut self) -> char {
        return *self.src.get(self.pos + 1).or_else(|| Some(&' ')).unwrap();
    }

    fn tokenize_number(&mut self) -> String {
        let mut res = String::new();
        while !self.eof && self.src[self.pos].is_numeric() {
            res.push(self.src[self.pos]);
            self.advance();
        }

        if self.src[self.pos] == '.' && self.peek().is_numeric() {
            self.advance();
            res.push('.');
            res.push_str(&self.tokenize_number()[..]);
        }
        return res;
    }

    fn tokenize_id(&mut self) -> Result<Token, SyntaxErr> {
        let mut res = String::new();
        while !self.eof && self.src[self.pos].is_alphabetic() {
            res.push(self.src[self.pos]);
            self.advance();
        }

        match &res[..] {
            "var" => Ok(Token::new(res, TokenType::Var)),
            _ => Ok(Token::new(res, TokenType::Id)),
        }
    }

    fn make_token(&mut self, val: &str, token_type: TokenType) -> Result<Token, SyntaxErr> {
        self.advance();
        Ok(Token::new(val.to_string(), token_type))
    }

    pub fn tokenize(&mut self) -> Result<Token, SyntaxErr> {
        if self.eof {
            return Ok(Token::new("eof".to_string(), TokenType::Eof));
        }

        match self.src[self.pos] {
            val if val == '\n' => self.make_token("\n", TokenType::NewLn),
            '+' => self.make_token("+", TokenType::Add),
            '-' => self.make_token("-", TokenType::Sub),
            '*' => self.make_token("*", TokenType::Mul),
            '/' => self.make_token("/", TokenType::Div),
            '(' => self.make_token("(", TokenType::LParen),
            ')' => self.make_token(")", TokenType::RParen),
            '=' => self.make_token("=", TokenType::Assign),
            val if val.is_whitespace() => {
                self.advance();
                self.tokenize()
            }
            val if val.is_alphabetic() => self.tokenize_id(),
            val if val.is_numeric() => Ok(Token::new(self.tokenize_number(), TokenType::Number)),
            val => Err(self.error(val)),
        }
    }
}
