use crate::token::Token;
use crate::token::TokenType;

#[derive(Debug)]
pub struct SyntaxError(pub String);

pub struct Source {
    source: Vec<char>,
}

impl Source {
    pub fn new(source: &str) -> Self {
        Source {
            source: source.chars().collect::<Vec<char>>(),
        }
    }

    pub fn get(&mut self, idx: usize) -> Option<&char> {
        self.source.get(idx)
    }

    pub fn len(&mut self) -> usize {
        self.source.len()
    }

    pub fn end(&mut self, idx: usize) -> bool {
        idx > self.len() - 1
    }
}

pub struct Lexer {
    source: Source,
    pos: usize,
}

impl Lexer {
    pub fn new(source: Source) -> Self {
        Lexer { source, pos: 0 }
    }

    fn current_char(&mut self) -> Result<char, SyntaxError> {
        match self.source.get(self.pos) {
            Some(char) => Ok(*char),
            None => Err(SyntaxError("Unexpected end of input".to_string())),
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn ident(&mut self) -> Result<Token, SyntaxError> {
        let mut res: String = String::new();
        while !self.source.end(self.pos) && self.current_char()?.is_alphabetic() {
            res.push(self.current_char()?);
            self.advance();
        }
        match &res[..] {
            "true" | "false" => Ok(Token::new(
                res.to_string(),
                TokenType::Keyword(res.to_string()),
                self.pos - &res.len(),
                res.len(),
            )),
            _ => Ok(Token::new(
                res.to_string(),
                TokenType::Id,
                self.pos - &res.len(),
                res.len(),
            )),
        }
    }

    fn number(&mut self) -> Result<Token, SyntaxError> {
        let mut res: String = String::new();
        while !self.source.end(self.pos) && self.current_char()?.is_numeric() {
            res.push(self.current_char()?);
            self.advance();
        }
        Ok(Token::new(
            res.to_string(),
            TokenType::Number,
            self.pos - &res.len(),
            res.len(),
        ))
    }

    fn string(&mut self) -> Result<Token, SyntaxError> {
        let mut res: String = String::new();
        while self.current_char()? != '"' {
            if self.source.end(self.pos) {
                return Err(SyntaxError("Syntax Error: unterminated string".to_string()));
            }
            res.push(self.current_char()?);
            self.advance();
        }
        let start = self.pos - &res.len();
        let length = &res.len();

        Ok(Token::new(res, TokenType::String, start, *length))
    }

    fn make_token(&mut self, token_val: &str, token_type: TokenType) -> Token {
        for _ in 0..token_val.len() {
            self.advance();
        }
        let token = Token::new(
            token_val.to_string(),
            token_type,
            self.pos - token_val.len(),
            token_val.len(),
        );
        return token;
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, SyntaxError> {
        let mut tokens: Vec<Token> = vec![];
        loop {
            if self.source.end(self.pos) {
                tokens.push(self.make_token("eof", TokenType::Eof));
                break;
            }

            tokens.push(match self.current_char()? {
                '+' => self.make_token("+", TokenType::Symbol("+".to_string())),
                '-' => self.make_token("-", TokenType::Symbol("-".to_string())),
                '*' => self.make_token("*", TokenType::Symbol("*".to_string())),
                '/' => self.make_token("/", TokenType::Symbol("/".to_string())),
                '\n' => self.make_token("\n", TokenType::Newline),
                '"' => self.string()?,
                c if c.is_alphabetic() => self.ident()?,
                c if c.is_numeric() => self.number()?,
                c if c.is_whitespace() => {
                    self.advance();
                    continue;
                }
                _ => {
                    return Err(SyntaxError(format!(
                        "Syntax Error: unexpected token `{}`",
                        self.current_char()?
                    )))
                }
            });
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::*;

    #[test]
    fn test_lexer() {
        let source = Source::new("123 + 456");
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            [
                Token::new("123".to_string(), TokenType::Number, 0, 3),
                Token::new("+".to_string(), TokenType::Symbol("+".to_string()), 4, 1),
                Token::new("456".to_string(), TokenType::Number, 6, 3),
                Token::new("eof".to_string(), TokenType::Eof, 9, 3),
            ]
        )
    }
}
