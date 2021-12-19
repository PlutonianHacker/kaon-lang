#![allow(dead_code)]

use std::rc::Rc;

use crate::analysis::SemanticAnalyzer;
use crate::analysis::SemanticErr;
use crate::lexer::Lexer;
use crate::lexer::SyntaxErr;
use crate::token::{Token, TokenType};

use crate::ast::{BinExpr, Expr, File, Ident, Literal, Op, UnaryExpr, VarDecl};

#[derive(Debug)]
pub struct ParserErr(pub String);

pub type ParserRes = Result<File, ParserErr>;

impl From<SemanticErr> for ParserErr {
    fn from(err: SemanticErr) -> Self {
        ParserErr(err.0)
    }
}

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
}

impl Parser {
    pub fn new(input: String) -> Parser {
        let mut lexer = Lexer::new(input.chars().collect());

        let token = match lexer.tokenize() {
            Err(SyntaxErr(str)) => panic!("{}", str),
            Ok(val) => val,
        };

        Parser {
            lexer: lexer,
            curr_token: token,
        }
    }

    fn consume(&mut self, token_type: TokenType) -> Result<(), ParserErr> {
        match token_type {
            token if token == self.curr_token.token_type => match self.lexer.tokenize() {
                Ok(token) => self.curr_token = token,
                Err(SyntaxErr(err)) => {
                    panic!("{}", err);
                }
            },
            _ => {
                return Err(ParserErr(format!(
                    "Parser Error: unexpected token {}",
                    self.curr_token.token_val
                )));
            }
        }
        Ok(())
    }

    fn parse_file(&mut self) -> Result<File, ParserErr> {
        let mut nodes: Vec<Expr> = vec![];
        loop {
            match self.curr_token.token_type {
                TokenType::Eof => {
                    break;
                }
                TokenType::NewLn => {
                    self.consume(TokenType::NewLn)?;
                    continue;
                }
                TokenType::Var => {
                    nodes.push(self.var_decl()?);
                }
                _ => {
                    nodes.push(self.parse_sum()?);
                }
            }
        }
        return Ok(File { nodes });
    }

    fn var_decl(&mut self) -> Result<Expr, ParserErr> {
        self.consume(TokenType::Var)?;
        let id = Ident(self.curr_token.token_val.clone());
        self.consume(TokenType::Id)?;
        self.consume(TokenType::Assign)?;
        let val = self.parse_sum()?;
        let node = Expr::VarDecl(Rc::new(VarDecl { id, val }));
        return Ok(node);
    }

    fn parse_sum(&mut self) -> Result<Expr, ParserErr> {
        let mut node = self.parse_term()?;
        loop {
            match self.curr_token.token_type {
                TokenType::Add => {
                    self.consume(TokenType::Add)?;
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::Add,
                        lhs: node,
                        rhs: self.parse_term()?,
                    }));
                }
                TokenType::Sub => {
                    self.consume(TokenType::Sub)?;
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::Sub,
                        lhs: node,
                        rhs: self.parse_term()?,
                    }));
                }
                _ => {
                    break;
                }
            }
        }
        return Ok(node);
    }

    fn parse_term(&mut self) -> Result<Expr, ParserErr> {
        let mut node = self.parse_factor()?;
        loop {
            match self.curr_token.token_type {
                TokenType::Mul => {
                    self.consume(TokenType::Mul)?;
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::Mul,
                        lhs: node,
                        rhs: self.parse_factor()?,
                    }));
                }
                TokenType::Div => {
                    self.consume(TokenType::Div)?;
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::Div,
                        lhs: node,
                        rhs: self.parse_factor()?,
                    }));
                }
                _ => {
                    break;
                }
            }
        }
        return Ok(node);
    }

    pub fn parse_factor(&mut self) -> Result<Expr, ParserErr> {
        match self.curr_token.token_type {
            TokenType::Number => {
                let node = Expr::Literal(Literal::Number(
                    self.curr_token.token_val.parse::<f64>().unwrap(),
                ));
                self.consume(TokenType::Number)?;
                return Ok(node);
            }
            TokenType::Add => {
                self.consume(TokenType::Add)?;
                return Ok(Expr::UnaryExpr(Rc::new(UnaryExpr {
                    op: Op::Add,
                    rhs: self.parse_factor()?,
                })));
            }
            TokenType::Sub => {
                self.consume(TokenType::Sub)?;
                return Ok(Expr::UnaryExpr(Rc::new(UnaryExpr {
                    op: Op::Sub,
                    rhs: self.parse_factor()?,
                })));
            }
            TokenType::LParen => {
                self.consume(TokenType::LParen)?;
                let node = self.parse_sum()?;
                self.consume(TokenType::RParen)?;
                return Ok(node);
            }
            TokenType::Id => return Ok(self.parse_id()?),
            _ => {
                return Err(ParserErr(format!(
                    "Parser Error: Unexpected token '{}'.",
                    self.curr_token.token_val
                )))
            }
        }
    }

    fn parse_id(&mut self) -> Result<Expr, ParserErr> {
        let id = self.curr_token.token_val.clone();
        self.consume(TokenType::Id)?;
        return Ok(Expr::Id(Ident(id)));
    }

    pub fn parse(&mut self, analyzer: &mut SemanticAnalyzer) -> ParserRes {
        let ast = self.parse_file()?;

        for node in &ast.nodes {
            analyzer.visit(node)?;
        }

        return Ok(ast);
    }
}
