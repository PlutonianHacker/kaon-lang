#![allow(dead_code)]

use std::rc::Rc;

use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

use crate::ast::{BinExpr, Expr, File, Literal, Op, UnaryExpr};

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        let token = lexer.tokenize().unwrap();

        Parser {
            lexer: lexer,
            curr_token: token,
        }
    }

    fn consume(&mut self, token_type: TokenType) {
        match token_type {
            token if token == self.curr_token.token_type => {
                self.curr_token = self.lexer.tokenize().unwrap();
            }
            _ => {
                panic!(
                    "Unexpected token: expected {:?} found {:?}",
                    self.curr_token.token_type, token_type
                )
            }
        }
    }

    fn parse_file(&mut self) -> File {
        let mut nodes: Vec<Expr> = vec![];
        loop {
            match self.curr_token.token_type {
                TokenType::Eof => {
                    break;
                }
                TokenType::NewLn => {
                    self.consume(TokenType::NewLn);
                    continue;
                }
                _ => {
                    nodes.push(self.parse_term());
                }
            }
        }
        return File { nodes };
    }

    fn parse_term(&mut self) -> Expr {
        let mut node = self.parse_sum();
        loop {
            match self.curr_token.token_type {
                TokenType::Add => {
                    self.consume(TokenType::Add);
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::Add,
                        lhs: node,
                        rhs: self.parse_sum(),
                    }));
                }
                TokenType::Sub => {
                    self.consume(TokenType::Sub);
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::Sub,
                        lhs: node,
                        rhs: self.parse_sum(),
                    }));
                }
                _ => {
                    break;
                }
            }
        }
        return node;
    }

    fn parse_sum(&mut self) -> Expr {
        let mut node = self.parse_literal();
        loop {
            match self.curr_token.token_type {
                TokenType::Mul => {
                    self.consume(TokenType::Mul);
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::Mul,
                        lhs: node,
                        rhs: self.parse_literal(),
                    }));
                }
                TokenType::Div => {
                    self.consume(TokenType::Div);
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::Div,
                        lhs: node,
                        rhs: self.parse_literal(),
                    }));
                }
                _ => {
                    break;
                }
            }
        }
        return node;
    }

    pub fn parse_literal(&mut self) -> Expr {
        match self.curr_token.token_type {
            TokenType::Number => {
                let node = Expr::Literal(Literal::Number(
                    self.curr_token.token_val.parse::<f64>().unwrap(),
                ));
                self.consume(TokenType::Number);
                return node;
            }
            _ => {
                println!("{:?}", self.curr_token);
                unimplemented!()
            }
        }
    }

    pub fn parse(&mut self) -> File {
        self.parse_file()
    }
}
