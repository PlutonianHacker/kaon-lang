use crate::token::Token;
use crate::token::TokenType;

#[derive(Debug, PartialEq)]
pub struct SyntaxErr(pub String);

#[derive(Clone)]
pub struct Lexer {
    src: Vec<char>,
    tokens: Vec<Token>,
    current: usize,
    previous: usize,
    pub eof: bool,
}

impl Lexer {
    pub fn new(src: Vec<char>) -> Self {
        Lexer {
            src,
            tokens: vec![],
            current: 0,
            previous: 0,
            eof: false,
        }
    }

    fn advance(&mut self) {
        let char = self.src.get(self.current + 1);
        match char {
            Some(_) => self.current += 1,
            None => self.eof = true,
        }
    }

    fn error(&mut self, lexeme: char) -> SyntaxErr {
        SyntaxErr(format!("Syntax Error: unexpected token '{}'", lexeme))
    }

    pub fn peek(&mut self) -> char {
        return *self
            .src
            .get(self.current + 1)
            .or_else(|| Some(&' '))
            .unwrap();
    }

    fn tokenize_string(&mut self) -> Result<Token, SyntaxErr> {
        let start = self.current;
        self.advance();
        let mut res = String::new();
        loop {
            match self.src[self.current] {
                '"' => {
                    self.advance();
                    break;
                }
                _ if self.eof => {
                    return Err(SyntaxErr(
                        "Syntax Error: unterminated double quote string".to_string(),
                    ))
                }
                c => {
                    res.push(c);
                    self.advance();
                }
            }
        }
        Ok(Token::new(res, TokenType::String, start, self.current))
    }

    fn tokenize_number(&mut self) -> String {
        let mut res = String::new();
        while !self.eof && self.src[self.current].is_numeric() {
            res.push(self.src[self.current]);
            self.advance();
        }

        if self.src[self.current] == '.' && self.peek().is_numeric() {
            self.advance();
            res.push('.');
            res.push_str(&self.tokenize_number()[..]);
        }
        return res;
    }

    fn tokenize_id(&mut self) -> Result<Token, SyntaxErr> {
        let start = self.current;
        let mut res = String::new();
        while !self.eof && self.src[self.current].is_alphabetic() || self.src[self.current] == '_' {
            res.push(self.src[self.current]);
            self.advance();
        }
        let end = self.current;

        match &res[..] {
            "if" => Ok(Token::new(res, TokenType::If, start, end)),
            "else" => Ok(Token::new(res, TokenType::Else, start, end)),
            "while" => Ok(Token::new(res, TokenType::While, start, end)),
            "var" => Ok(Token::new(res, TokenType::Var, start, end)),
            "true" | "false" => Ok(Token::new(res, TokenType::Bool, start, end)),
            "nil" => Ok(Token::new(res, TokenType::Nil, start, end)),
            "is" => Ok(Token::new(res, TokenType::Is, start, end)),
            "isnt" => Ok(Token::new(res, TokenType::Isnt, start, end)),
            "print" => Ok(Token::new(res, TokenType::Print, start, end)),
            _ => Ok(Token::new(res, TokenType::Id, start, end)),
        }
    }

    fn make_token(&mut self, val: &str, token_type: TokenType) -> Result<Token, SyntaxErr> {
        self.previous = self.current;
        self.advance();
        Ok(Token::new(
            val.to_string(),
            token_type,
            self.previous,
            self.current,
        ))
    }

    pub fn tokenize(&mut self) -> Result<Token, SyntaxErr> {
        if self.eof {
            return Ok(Token::new(
                "eof".to_string(),
                TokenType::Eof,
                self.previous,
                self.current,
            ));
        }

        match self.src[self.current] {
            val if val == '\n' => self.make_token("\n", TokenType::NewLn),
            '+' => self.make_token("+", TokenType::Add),
            '-' => self.make_token("-", TokenType::Sub),
            '*' => self.make_token("*", TokenType::Mul),
            '/' => self.make_token("/", TokenType::Div),
            '%' => self.make_token("%", TokenType::Modulo),
            '!' => self.make_token("!", TokenType::Bang),
            '(' => self.make_token("(", TokenType::LParen),
            ')' => self.make_token(")", TokenType::RParen),
            '{' => self.make_token("{", TokenType::LBrace),
            '}' => self.make_token("}", TokenType::RBrace),
            '=' => self.make_token("=", TokenType::Assign),
            ',' => self.make_token(",", TokenType::Comma),
            '<' => {
                if self.peek() == '=' {
                    let token = Ok(Token::new(
                        "<=".to_string(),
                        TokenType::Lte,
                        self.current,
                        self.current + 2,
                    ));
                    self.advance();
                    self.advance();
                    return token;
                } else {
                    self.make_token("<", TokenType::Lt)
                }
            }
            '>' => {
                if self.peek() == '=' {
                    let token = Ok(Token::new(
                        ">=".to_string(),
                        TokenType::Gte,
                        self.current,
                        self.current + 2,
                    ));
                    self.advance();
                    self.advance();
                    return token;
                } else {
                    self.make_token(">", TokenType::Gt)
                }
            }
            '"' => return Ok(self.tokenize_string()?),
            val if val.is_whitespace() => {
                self.advance();
                self.tokenize()
            }
            val if val.is_alphabetic() => self.tokenize_id(),
            val if val.is_numeric() => Ok(Token::new(
                self.tokenize_number(),
                TokenType::Number,
                self.current,
                self.current,
            )),
            val => Err(self.error(val)),
        }
    }
}
