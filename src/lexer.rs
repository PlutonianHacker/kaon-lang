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

    pub fn peek(&mut self) -> char {
        return *self.src.get(self.pos + 1).or_else(|| Some(&' ')).unwrap();
    }

    fn tokenize_string(&mut self) -> Result<Token, SyntaxErr> {
        self.advance();
        let mut res = String::new();
        loop {
            match self.src[self.pos] {
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
        Ok(Token::new(res, TokenType::String))
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
        while !self.eof && self.src[self.pos].is_alphabetic() || self.src[self.pos] == '_' {
            res.push(self.src[self.pos]);
            self.advance();
        }

        match &res[..] {
            "if" => Ok(Token::new(res, TokenType::If)),
            "else" => Ok(Token::new(res, TokenType::Else)),
            "while" => Ok(Token::new(res, TokenType::While)),
            "var" => Ok(Token::new(res, TokenType::Var)),
            "true" | "false" => Ok(Token::new(res, TokenType::Bool)),
            "nil" => Ok(Token::new(res, TokenType::Nil)),
            "is" => Ok(Token::new(res, TokenType::Is)),
            "isnt" => Ok(Token::new(res, TokenType::Isnt)),
            "print" => Ok(Token::new(res, TokenType::Print)),
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
            '%' => self.make_token("%", TokenType::Modulo),
            '!' => self.make_token("!", TokenType::Bang),
            '(' => self.make_token("(", TokenType::LParen),
            ')' => self.make_token(")", TokenType::RParen),
            '{' => self.make_token("{", TokenType::LBrace),
            '}' => self.make_token("}", TokenType::RBrace),
            '=' => self.make_token("=", TokenType::Assign),
            '<' => {
                if self.peek() == '=' {
                    let token = Ok(Token::new("<=".to_string(), TokenType::Lte));
                    self.advance();
                    self.advance();
                    return token;
                } else {
                    self.make_token("<", TokenType::Lt)
                }
            }
            '>' => {
                if self.peek() == '=' {
                    let token = Ok(Token::new(">=".to_string(), TokenType::Gte));
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
            val if val.is_numeric() => Ok(Token::new(self.tokenize_number(), TokenType::Number)),
            val => Err(self.error(val)),
        }
    }
}
