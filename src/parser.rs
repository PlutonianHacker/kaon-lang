#![allow(dead_code)]

use std::num::ParseFloatError;
use std::rc::Rc;

use crate::analysis::SemanticAnalyzer;
use crate::analysis::SemanticErr;
use crate::lexer::Lexer;
use crate::lexer::SyntaxErr;
use crate::token::{Token, TokenType};

use crate::ast::{
    AssignOp, AssignStmt, BinExpr, Expr, File, Ident, IfStmt, Literal, Op, UnaryExpr, VarDecl,
};

#[derive(Debug)]
pub struct ParserErr(pub String);

pub type ParserRes = Result<File, ParserErr>;

impl From<SemanticErr> for ParserErr {
    fn from(err: SemanticErr) -> Self {
        ParserErr(err.0)
    }
}

impl From<ParseFloatError> for ParserErr {
    fn from(_: ParseFloatError) -> Self {
        ParserErr("Parser Error: Unexpected token '.'".to_string())
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
            Err(SyntaxErr(err)) => panic!("{}", err),
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
                Err(SyntaxErr(err)) => return Err(ParserErr(err)),
            },
            _ => {
                return Err(ParserErr(format!(
                    "Parser Error: unexpected token '{}'",
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
                TokenType::If => {
                    nodes.push(self.parse_if_stmt()?);
                }
                TokenType::Id => {
                    nodes.push(self.parse_assign_stmt()?);
                }
                _ => {
                    nodes.push(self.parse_comparison()?);
                }
            }
        }
        return Ok(File { nodes });
    }

    fn parse_if_stmt(&mut self) -> Result<Expr, ParserErr> {
        self.consume(TokenType::If)?;
        let condition = self.parse_comparison()?;
        let block = self.parse_block()?;

        let node = Expr::IfStmt(Rc::new(IfStmt {
            test: condition,
            body: block,
        }));

        return Ok(node);
    }

    fn parse_block(&mut self) -> Result<Vec<Expr>, ParserErr> {
        self.consume(TokenType::LBrace)?;
        self.consume(TokenType::NewLn)?;
        let mut nodes: Vec<Expr> = vec![];
        loop {
            match self.curr_token.token_type {
                TokenType::RBrace => {
                    self.consume(TokenType::RBrace)?;
                    break;
                }
                TokenType::NewLn => {
                    self.consume(TokenType::NewLn)?;
                    continue;
                }
                TokenType::Var => {
                    nodes.push(self.var_decl()?);
                }
                TokenType::If => {
                    nodes.push(self.parse_if_stmt()?);
                }
                TokenType::Id => {
                    nodes.push(self.parse_assign_stmt()?);
                }
                _ => {
                    nodes.push(self.parse_comparison()?);
                }
            }
        }
        return Ok(nodes);
    }

    fn var_decl(&mut self) -> Result<Expr, ParserErr> {
        self.consume(TokenType::Var)?;
        let id = Ident(self.curr_token.token_val.clone());
        self.consume(TokenType::Id)?;
        self.consume(TokenType::Assign)?;
        let val = self.parse_comparison()?;
        let node = Expr::VarDecl(Rc::new(VarDecl { id, val }));
        return Ok(node);
    }

    fn parse_assign_stmt(&mut self) -> Result<Expr, ParserErr> {
        let node = self.parse_comparison()?;
        match (self.curr_token.token_type.clone(), node.clone()) {
            (TokenType::Assign, Expr::Id(Ident(val))) => {
                let op = AssignOp::Assign;
                self.consume(TokenType::Assign)?;
                let node = Expr::AssignStmt(Rc::new(AssignStmt {
                    id: Ident(val),
                    op,
                    val: self.parse_comparison()?,
                }));
                return Ok(node);
            }
            _ => {}
        };
        return Ok(node);
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParserErr> {
        let mut node = self.parse_sum()?;
        loop {
            match self.curr_token.token_type {
                TokenType::Is => {
                    self.consume(TokenType::Is)?;
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::Equals,
                        lhs: node,
                        rhs: self.parse_sum()?,
                    }));
                }
                TokenType::Isnt => {
                    self.consume(TokenType::Isnt)?;
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::NotEqual,
                        lhs: node,
                        rhs: self.parse_sum()?,
                    }));
                }
                TokenType::Gte => {
                    self.consume(TokenType::Gte)?;
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::Gte,
                        lhs: node,
                        rhs: self.parse_sum()?,
                    }));
                }
                TokenType::Lte => {
                    self.consume(TokenType::Lte)?;
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::Lte,
                        lhs: node,
                        rhs: self.parse_sum()?,
                    }));
                }
                TokenType::Gt => {
                    self.consume(TokenType::Gt)?;
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::Gt,
                        lhs: node,
                        rhs: self.parse_sum()?,
                    }));
                }
                TokenType::Lt => {
                    self.consume(TokenType::Lt)?;
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::Lt,
                        lhs: node,
                        rhs: self.parse_sum()?,
                    }));
                }
                _ => break,
            }
        }
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
                TokenType::Modulo => {
                    self.consume(TokenType::Modulo)?;
                    node = Expr::BinExpr(Rc::new(BinExpr {
                        op: Op::Modulo,
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
                let node =
                    Expr::Literal(Literal::Number(self.curr_token.token_val.parse::<f64>()?));
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
            TokenType::Bang => {
                self.consume(TokenType::Bang)?;
                return Ok(Expr::UnaryExpr(Rc::new(UnaryExpr {
                    op: Op::Not,
                    rhs: self.parse_factor()?,
                })));
            }
            TokenType::Bool => {
                let node = Expr::Literal(Literal::Boolean(
                    self.curr_token.token_val.parse::<bool>().unwrap(),
                ));
                self.consume(TokenType::Bool)?;
                return Ok(node);
            }
            TokenType::LParen => {
                self.consume(TokenType::LParen)?;
                let node = self.parse_sum()?;
                self.consume(TokenType::RParen)?;
                return Ok(node);
            }
            TokenType::Id => return Ok(Expr::Id(self.parse_id()?)),
            _ => {
                return Err(ParserErr(format!(
                    "Parser Error: Unexpected token '{}'.",
                    self.curr_token.token_val
                )))
            }
        }
    }

    fn parse_id(&mut self) -> Result<Ident, ParserErr> {
        let id = self.curr_token.token_val.clone();
        self.consume(TokenType::Id)?;
        return Ok(Ident(id));
    }

    pub fn parse(&mut self, analyzer: &mut SemanticAnalyzer) -> ParserRes {
        let ast = self.parse_file()?;

        for node in &ast.nodes {
            analyzer.visit(node)?;
        }

        return Ok(ast);
    }
}
