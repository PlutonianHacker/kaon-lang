#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // symbols
    Add,
    Sub,
    Div,
    Mul,
    RParen,
    LParen,

    // types
    Number,
    String,
    Bool,
    Nil,
    // keywords

    // misc.
    NewLn,
    Eof,
    Id,
    Bad,
}

impl From<String> for TokenType {
    fn from(val: String) -> TokenType {
        match &val[..] {
            "+" => TokenType::Add,
            "-" => TokenType::Sub,
            "*" => TokenType::Mul,
            "/" => TokenType::Div,
            _ => TokenType::Id,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_val: String,
    pub token_type: TokenType,
}

impl Token {
    pub fn new(token_val: String, token_type: TokenType) -> Token {
        Token {
            token_val,
            token_type,
        }
    }
}
