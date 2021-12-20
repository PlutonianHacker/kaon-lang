#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Add,
    Sub,
    Div,
    Mul,
    RParen,
    LParen,
    Assign,
    Number,
    String,
    Bool,
    Nil,
    Var,
    NewLn,
    Eof,
    Id,
    Bad,
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
