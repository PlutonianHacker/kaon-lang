#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }

    pub fn empty() -> Span {
        Span { start: 0, end: 0 }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Add,
    Sub,
    Div,
    Mul,
    Modulo,
    Bang,
    RParen,
    LParen,
    RBrace,
    LBrace,
    Assign,
    Is,
    Isnt,
    Gte,
    Lte,
    Gt,
    Lt,
    Comma,
    Number,
    String,
    Bool,
    If,
    Else,
    While,
    Return,
    Def,
    Nil,
    Var,
    Print,
    NewLn,
    Eof,
    Id,
    Bad,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_val: String,
    pub token_type: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(token_val: String, token_type: TokenType, start: usize, end: usize) -> Token {
        Token {
            token_val,
            token_type,
            span: Span::new(start, end),
        }
    }

    pub fn empty() -> Token {
        Token {
            token_val: String::new(),
            token_type: TokenType::Nil,
            span: Span::empty(),
        }
    }
}
