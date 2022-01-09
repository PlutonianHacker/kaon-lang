use std::rc::Rc;

use crate::analysis::{SemanticAnalyzer, SemanticError};
use crate::error::SyntaxError;
use crate::span::Spanned;
use crate::token::{Token, TokenType};

use crate::ast::{
    AssignOp, AssignStmt, BinExpr, File, FuncCall, Ident, IfStmt, Literal, MemberExpr, Op,
    UnaryExpr, VarDecl, AST,
};

pub struct Parser {
    tokens: Spanned<Vec<Token>>,
    current_token: Token,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Spanned<Vec<Token>>) -> Parser {
        Parser {
            tokens,
            current_token: Token::empty(),
            pos: 0,
        }
    }

    fn consume(&mut self, token_type: TokenType) -> Result<(), SyntaxError> {
        match token_type {
            token_type if token_type == self.current_token.token_type => {
                self.pos += 1;
                self.current_token = self.tokens.node[self.pos].clone();
                Ok(())
            }
            _ => Err(SyntaxError::error(
                &format!("Unexpected token `{}`", &self.current_token.token_val),
                &self.current_token.span,
            )),
        }
    }

    fn parse_file(&mut self) -> Result<File, SyntaxError> {
        let mut nodes = vec![];
        loop {
            match self.current_token.token_type.clone() {
                TokenType::Keyword(x) if x == "if" => {
                    nodes.push(self.if_statement()?);
                }
                TokenType::Keyword(x) if x == "loop" => {
                    nodes.push(self.loop_statement()?);
                }
                TokenType::Keyword(x) if x == "while" => {
                    nodes.push(self.while_statement()?);
                }
                TokenType::Symbol(sym) if sym == "{" => {
                    nodes.push(self.block()?);
                }
                TokenType::Newline => {
                    self.consume(TokenType::Newline)?;
                    continue;
                }
                TokenType::Eof => {
                    break;
                }
                _ => {
                    nodes.push(self.statement()?);
                }
            }
        }
        return Ok(File { nodes });
    }

    fn block(&mut self) -> Result<AST, SyntaxError> {
        self.consume(TokenType::symbol("{"))?;
        let mut nodes: Vec<AST> = vec![];
        loop {
            match self.current_token.token_type.clone() {
                TokenType::Symbol(sym) if sym == "}" => {
                    self.consume(TokenType::symbol("}"))?;
                    break;
                }
                TokenType::Newline => {
                    self.consume(TokenType::Newline)?;
                    continue;
                }
                TokenType::Keyword(x) if x == "if" => {
                    nodes.push(self.if_statement()?);
                }
                TokenType::Symbol(sym) if sym == "{" => {
                    nodes.push(self.block()?);
                }
                _ => {
                    nodes.push(self.statement()?);
                }
            }
        }
        return Ok(AST::Block(Rc::new(nodes)));
    }

    fn statement(&mut self) -> Result<AST, SyntaxError> {
        loop {
            match self.current_token.token_type.clone() {
                TokenType::Keyword(x) if x == "var" => return self.var_decl(),
                TokenType::Id => return self.assignment_stmt(),
                _ => return self.disjunction(),
            }
        }
    }

    fn loop_statement(&mut self) -> Result<AST, SyntaxError> {
        self.consume(TokenType::keyword("loop"))?;
        return Ok(AST::Loop(Rc::new(self.block()?)));
    }

    fn while_statement(&mut self) -> Result<AST, SyntaxError> {
        self.consume(TokenType::keyword("while"))?;
        return Ok(AST::while_stmt(self.disjunction()?, self.block()?))
    }

    fn if_statement(&mut self) -> Result<AST, SyntaxError> {
        self.consume(TokenType::keyword("if"))?;

        let condition = self.disjunction()?;

        let block = self.block()?;
        let alternate = match self.current_token.token_type.clone() {
            TokenType::Keyword(x) if x == "else" => Some(self.parse_else_block()?),
            _ => None,
        };

        let node = AST::IfStmt(Rc::new(IfStmt {
            test: condition,
            body: block,
            alternate,
        }));

        return Ok(node);
    }

    fn parse_else_block(&mut self) -> Result<AST, SyntaxError> {
        self.consume(TokenType::keyword("else"))?;

        if self.current_token.token_type == TokenType::keyword("if") {
            return self.if_statement();
        } else {
            return self.block();
        }
    }

    fn var_decl(&mut self) -> Result<AST, SyntaxError> {
        self.consume(TokenType::keyword("var"))?;
        let id = Ident(self.current_token.token_val.clone());
        self.consume(TokenType::Id)?;
        self.consume(TokenType::symbol("="))?;
        let val = self.parse_comparison()?;
        let node = AST::VarDecl(Rc::new(VarDecl { id, val }));
        return Ok(node);
    }

    fn args(&mut self) -> Result<Vec<AST>, SyntaxError> {
        self.consume(TokenType::symbol("("))?;

        let mut args = vec![];

        if self.current_token.token_type != TokenType::symbol(")") {
            args.push(self.disjunction()?);
        }

        loop {
            match self.current_token.token_type.clone() {
                TokenType::Symbol(sym) if sym == ")" => {
                    break;
                }
                TokenType::Symbol(sym) if sym == "," => {
                    self.consume(TokenType::symbol(","))?;
                    args.push(self.disjunction()?);
                }
                _ => {
                    return Err(SyntaxError::error(
                        "This expression is broken in ways I can't explain",
                        &self.current_token.span,
                    ))
                }
            }
        }

        self.consume(TokenType::symbol(")"))?;

        return Ok(args);
    }

    fn assignment_stmt(&mut self) -> Result<AST, SyntaxError> {
        let node = self.parse_comparison()?;
        match (self.current_token.token_type.clone(), node.clone()) {
            (TokenType::Symbol(x), AST::Id(Ident(val))) if x == "=" => {
                let op = AssignOp::Assign;
                self.consume(TokenType::symbol("="))?;
                let node = AST::AssignStmt(Rc::new(AssignStmt {
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

    fn disjunction(&mut self) -> Result<AST, SyntaxError> {
        let mut node = self.conjunction()?;
        loop {
            match &self.current_token.token_type {
                TokenType::Keyword(x) if x == "or" => {
                    self.consume(TokenType::keyword("or"))?;
                    node = AST::BinExpr(Rc::new(BinExpr {
                        op: Op::Or,
                        lhs: node,
                        rhs: self.conjunction()?,
                    }));
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn conjunction(&mut self) -> Result<AST, SyntaxError> {
        let mut node = self.parse_comparison()?;
        loop {
            match &self.current_token.token_type {
                TokenType::Keyword(x) if x == "and" => {
                    self.consume(TokenType::keyword("and"))?;
                    node = AST::BinExpr(Rc::new(BinExpr {
                        op: Op::And,
                        lhs: node,
                        rhs: self.parse_comparison()?,
                    }));
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_comparison(&mut self) -> Result<AST, SyntaxError> {
        let mut node = self.parse_sum()?;
        loop {
            match self.current_token.token_type.clone() {
                TokenType::Keyword(sym) if sym == "is" => {
                    self.consume(TokenType::keyword("is"))?;
                    node = AST::BinExpr(Rc::new(BinExpr {
                        op: Op::Equals,
                        lhs: node,
                        rhs: self.parse_sum()?,
                    }));
                }
                TokenType::Keyword(sym) if sym == "isnt" => {
                    self.consume(TokenType::keyword("isnt"))?;
                    node = AST::BinExpr(Rc::new(BinExpr {
                        op: Op::NotEqual,
                        lhs: node,
                        rhs: self.parse_sum()?,
                    }));
                }
                TokenType::Symbol(sym) if sym == ">=" => {
                    self.consume(TokenType::symbol(">="))?;
                    node = AST::BinExpr(Rc::new(BinExpr {
                        op: Op::Gte,
                        lhs: node,
                        rhs: self.parse_sum()?,
                    }));
                }
                TokenType::Symbol(sym) if sym == "<=" => {
                    self.consume(TokenType::symbol("<="))?;
                    node = AST::BinExpr(Rc::new(BinExpr {
                        op: Op::Lte,
                        lhs: node,
                        rhs: self.parse_sum()?,
                    }));
                }
                TokenType::Symbol(sym) if sym == ">" => {
                    self.consume(TokenType::symbol(">"))?;
                    node = AST::BinExpr(Rc::new(BinExpr {
                        op: Op::Gt,
                        lhs: node,
                        rhs: self.parse_sum()?,
                    }));
                }
                TokenType::Symbol(sym) if sym == "<" => {
                    self.consume(TokenType::symbol("<"))?;
                    node = AST::BinExpr(Rc::new(BinExpr {
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

    fn parse_sum(&mut self) -> Result<AST, SyntaxError> {
        let mut node = self.parse_term()?;
        loop {
            match self.current_token.token_type.clone() {
                TokenType::Symbol(sym) if sym == "+" => {
                    self.consume(TokenType::symbol("+"))?;
                    node = AST::BinExpr(Rc::new(BinExpr {
                        op: Op::Add,
                        lhs: node,
                        rhs: self.parse_term()?,
                    }));
                }
                TokenType::Symbol(sym) if sym == "-" => {
                    self.consume(TokenType::symbol("-"))?;
                    node = AST::BinExpr(Rc::new(BinExpr {
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

    fn parse_term(&mut self) -> Result<AST, SyntaxError> {
        let mut node = self.member_expr()?;
        loop {
            match self.current_token.token_type.clone() {
                TokenType::Symbol(sym) if sym == "*" => {
                    self.consume(TokenType::symbol("*"))?;
                    node = AST::BinExpr(Rc::new(BinExpr {
                        op: Op::Mul,
                        lhs: node,
                        rhs: self.member_expr()?,
                    }));
                }
                TokenType::Symbol(sym) if sym == "/" => {
                    self.consume(TokenType::symbol("/"))?;
                    node = AST::BinExpr(Rc::new(BinExpr {
                        op: Op::Div,
                        lhs: node,
                        rhs: self.member_expr()?,
                    }));
                }
                TokenType::Symbol(sym) if sym == "%" => {
                    self.consume(TokenType::symbol("%"))?;
                    node = AST::BinExpr(Rc::new(BinExpr {
                        op: Op::Modulo,
                        lhs: node,
                        rhs: self.member_expr()?,
                    }));
                }
                _ => {
                    break;
                }
            }
        }
        return Ok(node);
    }

    fn member_expr(&mut self) -> Result<AST, SyntaxError> {
        let mut node = self.parse_factor()?;

        loop {
            match self.current_token.token_type.clone() {
                TokenType::Symbol(sym) if sym == "[" => {
                    node = AST::MemberExpr(Rc::new(MemberExpr::new(node, self.slice()?)))
                }
                TokenType::Symbol(sym) if sym == "(" => {
                    node = AST::FuncCall(Rc::new(FuncCall::new(node, self.args()?)))
                }
                _ => break,
            }
        }

        return Ok(node);
    }

    fn parse_factor(&mut self) -> Result<AST, SyntaxError> {
        match self.current_token.token_type.clone() {
            TokenType::Number => {
                let node = AST::Literal(Literal::Number(
                    self.current_token.token_val.parse::<f64>().unwrap(),
                ));
                self.consume(TokenType::Number)?;
                return Ok(node);
            }
            TokenType::String => {
                let node =
                    AST::Literal(Literal::String((&self.current_token.token_val).to_string()));
                self.consume(TokenType::String)?;
                return Ok(node);
            }
            TokenType::Symbol(sym) if sym == "+" => {
                self.consume(TokenType::symbol("+"))?;
                return Ok(AST::UnaryExpr(Rc::new(UnaryExpr {
                    op: Op::Add,
                    rhs: self.parse_factor()?,
                })));
            }
            TokenType::Symbol(sym) if sym == "-" => {
                self.consume(TokenType::symbol("-"))?;
                return Ok(AST::UnaryExpr(Rc::new(UnaryExpr {
                    op: Op::Sub,
                    rhs: self.parse_factor()?,
                })));
            }
            TokenType::Symbol(sym) if sym == "!" => {
                self.consume(TokenType::symbol("!"))?;
                return Ok(AST::UnaryExpr(Rc::new(UnaryExpr {
                    op: Op::Not,
                    rhs: self.parse_factor()?,
                })));
            }
            TokenType::Keyword(x) if x == "true" || x == "false" => {
                let node = AST::Literal(Literal::Boolean(
                    self.current_token.token_val.parse::<bool>().unwrap(),
                ));
                self.consume(TokenType::Keyword(x))?;
                return Ok(node);
            }
            TokenType::Symbol(sym) if sym == "(" => {
                self.consume(TokenType::symbol("("))?;
                let node = self.parse_sum()?;
                self.consume(TokenType::symbol(")"))?;
                return Ok(node);
            }
            TokenType::Symbol(sym) if sym == "[" => return self.list(),
            TokenType::Id => {
                return Ok(AST::Id(self.parse_id()?));
            }
            _ => {
                return Err(SyntaxError::error(
                    &format!(
                        "Parser Error: Unexpected token `{}`",
                        self.current_token.token_val
                    ),
                    &self.current_token.span,
                ))
            }
        }
    }

    fn list(&mut self) -> Result<AST, SyntaxError> {
        self.consume(TokenType::symbol("["))?;

        let mut nodes = vec![];

        loop {
            match self.current_token.token_type.clone() {
                TokenType::Symbol(sym) if sym == "," => {
                    self.consume(TokenType::symbol(","))?;
                }
                TokenType::Symbol(sym) if sym == "]" => {
                    break;
                }
                _ => nodes.push(self.disjunction()?),
            }
        }

        self.consume(TokenType::symbol("]"))?;

        return Ok(AST::List(nodes));
    }

    fn slice(&mut self) -> Result<AST, SyntaxError> {
        self.consume(TokenType::symbol("["))?;

        let slice = self.parse_sum();

        self.consume(TokenType::symbol("]"))?;

        return slice;
    }

    fn parse_id(&mut self) -> Result<Ident, SyntaxError> {
        let id = self.current_token.token_val.clone();
        self.consume(TokenType::Id)?;
        return Ok(Ident(id));
    }

    pub fn parse(&mut self, analyzer: &mut SemanticAnalyzer) -> Result<File, SyntaxError> {
        self.current_token = self.tokens.node[self.pos].clone();

        let ast = self.parse_file()?;

        //println!("{:#?}", ast);

        for node in &ast.nodes {
            match analyzer.visit(node) {
                Err(SemanticError(err)) => {
                    return Err(SyntaxError::error(&err, &self.tokens.source))
                }
                Ok(_) => continue,
            }
        }

        return Ok(ast);
    }
}
