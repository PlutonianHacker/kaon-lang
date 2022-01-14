use crate::common::Spanned;
use crate::compiler::{SemanticAnalyzer, SemanticError};
use crate::compiler::{Token, TokenType};
use crate::error::error::SyntaxError;

use crate::compiler::{ASTNode, BinExpr, Expr, Ident, Op, Stmt, AST};

pub struct Parser {
    tokens: Spanned<Vec<Token>>,
    current: Token,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Spanned<Vec<Token>>) -> Parser {
        Parser {
            tokens,
            current: Token::empty(),
            pos: 0,
        }
    }

    fn consume(&mut self, token_type: TokenType) -> Result<(), SyntaxError> {
        match token_type {
            token_type if token_type == self.current.token_type => {
                self.pos += 1;
                self.current = self.tokens.node[self.pos].clone();
                Ok(())
            }
            _ => Err(SyntaxError::error(
                &format!("Unexpected token `{}`", &self.current.token_val),
                &self.current.span,
            )),
        }
    }

    fn compound_statement(&mut self) -> Result<Stmt, SyntaxError> {
        match self.current.token_type.clone() {
            TokenType::Keyword(x) if x == "if" => self.if_statement(),
            TokenType::Keyword(x) if x == "loop" => self.loop_statement(),
            TokenType::Keyword(x) if x == "while" => self.while_statement(),
            TokenType::Symbol(sym) if sym == "{" => self.block(),
            _ => self.statement(),
        }
    }

    fn statement(&mut self) -> Result<Stmt, SyntaxError> {
        let node = match self.current.token_type.clone() {
            TokenType::Keyword(x) if x == "var" => self.var_decl(),
            TokenType::Id => self.assignment_stmt(),
            _ => Ok(Stmt::Expr(self.disjunction()?)),
        };
        if self.current.token_type == TokenType::Eof {
            return node;
        } else {
            self.consume(TokenType::Newline)?;
        }

        return node;
    }

    fn block(&mut self) -> Result<Stmt, SyntaxError> {
        self.consume(TokenType::symbol("{"))?;
        let mut nodes = vec![];
        loop {
            match self.current.token_type.clone() {
                TokenType::Symbol(sym) if sym == "}" => {
                    self.consume(TokenType::symbol("}"))?;
                    break;
                }
                TokenType::Newline => {
                    self.consume(TokenType::Newline)?;
                    continue;
                }
                _ => {
                    nodes.push(self.compound_statement()?);
                }
            }
        }
        return Ok(Stmt::Block(Box::new(nodes), self.tokens.source.clone()));
    }

    fn loop_statement(&mut self) -> Result<Stmt, SyntaxError> {
        self.consume(TokenType::keyword("loop"))?;
        return Ok(Stmt::LoopStatement(
            Box::new(self.block()?),
            self.current.span.clone(),
        ));
    }

    fn while_statement(&mut self) -> Result<Stmt, SyntaxError> {
        self.consume(TokenType::keyword("while"))?;
        return Ok(Stmt::WhileStatement(
            self.disjunction()?,
            Box::new(self.block()?),
            self.current.span.clone(),
        ));
    }

    fn if_statement(&mut self) -> Result<Stmt, SyntaxError> {
        self.consume(TokenType::keyword("if"))?;

        let condition = self.disjunction()?;

        let block = self.block()?;
        let alternate = match self.current.token_type.clone() {
            TokenType::Keyword(x) if x == "else" => Some(self.parse_else_block()?),
            _ => None,
        };

        return Ok(Stmt::IfStatement(
            condition,
            Box::new((block, alternate)),
            self.current.span.clone(),
        ));
    }

    fn parse_else_block(&mut self) -> Result<Stmt, SyntaxError> {
        self.consume(TokenType::keyword("else"))?;

        if self.current.token_type == TokenType::keyword("if") {
            return self.if_statement();
        } else {
            return self.block();
        }
    }

    fn var_decl(&mut self) -> Result<Stmt, SyntaxError> {
        self.consume(TokenType::keyword("var"))?;
        let id = self.identifier()?;

        self.consume(TokenType::symbol("="))?;
        let init = self.disjunction()?;

        return Ok(Stmt::VarDeclaration(id, init, self.current.span.clone()));
    }

    fn args(&mut self) -> Result<Vec<Expr>, SyntaxError> {
        self.consume(TokenType::symbol("("))?;

        let mut args = vec![];

        if self.current.token_type != TokenType::symbol(")") {
            args.push(self.disjunction()?);
        }

        loop {
            match self.current.token_type.clone() {
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
                        &self.current.span,
                    ))
                }
            }
        }

        self.consume(TokenType::symbol(")"))?;

        return Ok(args);
    }

    fn assignment_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let node = self.expression()?;

        match self.current.token_type.clone() {
            TokenType::Symbol(sym) => match &sym[..] {
                "=" => {
                    self.consume(TokenType::symbol("="))?;

                    let id = match node.clone() {
                        Stmt::Expr(Expr::Identifier(id)) => id,
                        _ => {
                            return Err(SyntaxError::error(
                                "Expected indentifier",
                                &self.current.span,
                            ))
                        }
                    };

                    let node =
                        Stmt::AssignStatement(id, self.disjunction()?, self.current.span.clone());
                    return Ok(node);
                }
                _ => {}
            },
            _ => {}
        }

        return Ok(node);
    }

    fn expression(&mut self) -> Result<Stmt, SyntaxError> {
        Ok(Stmt::Expr(self.disjunction()?))
    }

    fn disjunction(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.conjunction()?;
        loop {
            match &self.current.token_type {
                TokenType::Keyword(x) if x == "or" => {
                    self.consume(TokenType::keyword("or"))?;
                    node = Expr::Or(Box::new(node), Box::new(self.conjunction()?));
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn conjunction(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.comparison()?;
        loop {
            match &self.current.token_type {
                TokenType::Keyword(x) if x == "and" => {
                    self.consume(TokenType::keyword("and"))?;
                    node = Expr::And(Box::new(node), Box::new(self.comparison()?));
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn comparison(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.parse_sum()?;
        loop {
            match self.current.token_type.clone() {
                TokenType::Keyword(sym) if sym == "is" => {
                    self.consume(TokenType::keyword("is"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::EqualTo, node, self.parse_sum()?)),
                        self.current.span.clone(),
                    );
                }
                TokenType::Keyword(sym) if sym == "isnt" => {
                    self.consume(TokenType::keyword("isnt"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::NotEqual, node, self.parse_sum()?)),
                        self.current.span.clone(),
                    );
                }
                TokenType::Symbol(sym) if sym == ">=" => {
                    self.consume(TokenType::symbol(">="))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::GreaterThanEquals, node, self.parse_sum()?)),
                        self.current.span.clone(),
                    );
                }
                TokenType::Symbol(sym) if sym == "<=" => {
                    self.consume(TokenType::symbol("<="))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::LessThanEquals, node, self.parse_sum()?)),
                        self.current.span.clone(),
                    );
                }
                TokenType::Symbol(sym) if sym == ">" => {
                    self.consume(TokenType::symbol(">"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::GreaterThan, node, self.parse_sum()?)),
                        self.current.span.clone(),
                    );
                }
                TokenType::Symbol(sym) if sym == "<" => {
                    self.consume(TokenType::symbol("<"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::LessThan, node, self.parse_sum()?)),
                        self.current.span.clone(),
                    );
                }
                _ => break,
            }
        }
        return Ok(node);
    }

    fn parse_sum(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.parse_term()?;
        loop {
            match self.current.token_type.clone() {
                TokenType::Symbol(sym) if sym == "+" => {
                    self.consume(TokenType::symbol("+"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Add, node, self.parse_term()?)),
                        self.current.span.clone(),
                    );
                }
                TokenType::Symbol(sym) if sym == "-" => {
                    self.consume(TokenType::symbol("-"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Subtract, node, self.parse_term()?)),
                        self.current.span.clone(),
                    );
                }
                _ => {
                    break;
                }
            }
        }
        return Ok(node);
    }

    fn parse_term(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.member_expr()?;
        loop {
            match self.current.token_type.clone() {
                TokenType::Symbol(sym) if sym == "*" => {
                    self.consume(TokenType::symbol("*"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Multiply, node, self.member_expr()?)),
                        self.current.span.clone(),
                    );
                }
                TokenType::Symbol(sym) if sym == "/" => {
                    self.consume(TokenType::symbol("/"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Divide, node, self.member_expr()?)),
                        self.current.span.clone(),
                    );
                }
                TokenType::Symbol(sym) if sym == "%" => {
                    self.consume(TokenType::symbol("%"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Remainder, node, self.member_expr()?)),
                        self.current.span.clone(),
                    );
                }
                _ => {
                    break;
                }
            }
        }
        return Ok(node);
    }

    fn member_expr(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.factor()?;

        loop {
            match self.current.token_type.clone() {
                TokenType::Symbol(sym) if sym == "[" => {
                    //node = AST::MemberExpr(Rc::new(MemberExpr::new(node, self.slice()?)))
                    continue;
                }
                TokenType::Symbol(sym) if sym == "(" => {
                    node = Expr::FunCall(
                        Box::new(node),
                        Box::new(self.args()?),
                        self.current.span.clone(),
                    );
                }
                _ => break,
            }
        }

        return Ok(node);
    }

    fn factor(&mut self) -> Result<Expr, SyntaxError> {
        let node;
        match self.current.token_type.clone() {
            TokenType::Number => {
                node = Expr::Number(
                    self.current.token_val.parse::<f64>().unwrap(),
                    self.current.span.clone(),
                );
                self.consume(TokenType::Number)?;
            }
            TokenType::String => {
                node = Expr::String(self.current.token_val.clone(), self.current.span.clone());
                self.consume(TokenType::String)?;
            }
            TokenType::Keyword(x) if x == "true" || x == "false" => {
                node = Expr::Boolean(x.parse::<bool>().unwrap(), self.current.span.clone());
                self.consume(TokenType::Keyword(x))?;
            }
            TokenType::Symbol(sym) => match &sym[..] {
                "+" => {
                    self.consume(TokenType::symbol("+"))?;
                    node = Expr::UnaryExpr(
                        Op::Add,
                        Box::new(self.factor()?),
                        self.current.span.clone(),
                    );
                }
                "-" => {
                    self.consume(TokenType::symbol("-"))?;
                    node = Expr::UnaryExpr(
                        Op::Subtract,
                        Box::new(self.factor()?),
                        self.current.span.clone(),
                    );
                }
                "!" => {
                    self.consume(TokenType::symbol("!"))?;
                    node = Expr::UnaryExpr(
                        Op::Bang,
                        Box::new(self.factor()?),
                        self.current.span.clone(),
                    );
                }
                "(" => {
                    self.consume(TokenType::symbol("("))?;
                    node = self.parse_sum()?;
                    self.consume(TokenType::symbol(")"))?;
                }
                "[" => return self.list(),
                sym => {
                    return Err(SyntaxError::error(
                        &format!("Parser Error: Unexpected symbol `{}`", sym),
                        &self.current.span,
                    ))
                }
            },
            TokenType::Id => {
                return Ok(Expr::Identifier(self.identifier()?));
            }
            _ => {
                return Err(SyntaxError::error(
                    &format!(
                        "Parser Error: Unexpected token `{}`",
                        self.current.token_val
                    ),
                    &self.current.span.clone(),
                ))
            }
        }
        return Ok(node);
    }

    fn list(&mut self) -> Result<Expr, SyntaxError> {
        self.consume(TokenType::symbol("["))?;

        let mut nodes = vec![];

        loop {
            match self.current.token_type.clone() {
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

        return Ok(Expr::List(Box::new(nodes), self.current.span.clone()));
    }

    fn _slice(&mut self) -> Result<Expr, SyntaxError> {
        self.consume(TokenType::symbol("["))?;

        let slice = self.parse_sum();

        self.consume(TokenType::symbol("]"))?;

        return slice;
    }

    fn identifier(&mut self) -> Result<Ident, SyntaxError> {
        let name = self.current.token_val.clone();
        let span = self.current.span.clone();
        self.consume(TokenType::Id)?;
        return Ok(Ident { name, span });
    }

    pub fn parse_file(&mut self) -> Result<AST, SyntaxError> {
        let mut nodes = vec![];

        loop {
            match self.current.token_type {
                TokenType::Eof => break,
                TokenType::Newline => {
                    self.consume(TokenType::Newline)?;
                    continue;
                }
                _ => nodes.push(ASTNode::from(self.compound_statement()?)),
            }
        }

        return Ok(AST::new(nodes, self.tokens.source.clone()));
    }

    pub fn parse(&mut self, analyzer: &mut SemanticAnalyzer) -> Result<AST, SyntaxError> {
        self.current = self.tokens.node[self.pos].clone();

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
