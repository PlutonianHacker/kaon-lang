use crate::common::{Span, Spanned};
use crate::compiler::SemanticAnalyzer;
use crate::compiler::{Token, TokenType};
use crate::error::{Emitter, ErrorKind, SyntaxError};

use crate::compiler::{ASTNode, BinExpr, Expr, FunAccess, Ident, Op, ScriptFun, Stmt, AST};

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
            _ => Err(self.error()),
        }
    }

    fn error(&self) -> SyntaxError {
        SyntaxError::error(
            ErrorKind::UnexpectedToken,
            &format!("Unexpected token `{}`", &self.current.token_val),
            &self.current.span,
        )
    }

    fn compound_statement(&mut self) -> Result<Stmt, SyntaxError> {
        match self.current.token_type.clone() {
            TokenType::Keyword(x) if x == "if" => self.if_statement(),
            TokenType::Keyword(x) if x == "loop" => self.loop_statement(),
            TokenType::Keyword(x) if x == "while" => self.while_statement(),
            TokenType::Keyword(x) if x == "fun" => self.fun(),
            TokenType::Symbol(sym) if sym == "{" => self.block(),
            _ => self.statement(),
        }
    }

    fn statement(&mut self) -> Result<Stmt, SyntaxError> {
        let node = match self.current.token_type.clone() {
            TokenType::Keyword(x) if x == "var" => self.var_decl(),
            TokenType::Keyword(x) if x == "con" => self.const_decl(),
            TokenType::Keyword(x) if x == "break" => self.break_stmt(),
            TokenType::Keyword(x) if x == "continue" => self.continue_stmt(),
            TokenType::Keyword(x) if x == "return" => self.return_stmt(),
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
                TokenType::Comment(typ) => {
                    self.consume(TokenType::Comment(typ))?;
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

    fn break_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let start = self.current.span.clone();
        self.consume(TokenType::keyword("break"))?;
        return Ok(Stmt::Break(start));
    }

    fn continue_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let start = self.current.span.clone();
        self.consume(TokenType::keyword("continue"))?;
        return Ok(Stmt::Continue(start));
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

    fn fun(&mut self) -> Result<Stmt, SyntaxError> {
        let start = &self.current.span.clone();
        self.consume(TokenType::keyword("fun"))?;

        let name = self.identifier()?;
        let params = self.params()?;
        let body = self.block()?;

        let end = &body.span();

        let access = FunAccess::Public;

        let fun = ScriptFun::new(name, params, body, access);

        Ok(Stmt::ScriptFun(Box::new(fun), Span::combine(start, end)))
    }

    fn params(&mut self) -> Result<Vec<Ident>, SyntaxError> {
        self.consume(TokenType::symbol("("))?;
        let mut params = vec![];

        if self.current.token_type != TokenType::symbol(")") {
            params.push(self.identifier()?);
        }

        loop {
            match self.current.token_type.clone() {
                TokenType::Symbol(sym) if sym == "," => {
                    self.consume(TokenType::symbol(","))?;
                    params.push(self.identifier()?);
                }
                TokenType::Symbol(sym) if sym == ")" => {
                    break;
                }
                _ => return Err(self.error()),
            }
        }

        self.consume(TokenType::symbol(")"))?;
        Ok(params)
    }

    fn return_stmt(&mut self) -> Result<Stmt, SyntaxError> {
        let start = &self.current.span.clone();

        self.consume(TokenType::keyword("return"))?;

        let expr = self.disjunction()?;

        let end = &expr.span();

        let stmt = Stmt::Return(expr, Span::combine(start, end));

        Ok(stmt)
    }

    fn var_decl(&mut self) -> Result<Stmt, SyntaxError> {
        let start = &self.current.span.clone();

        self.consume(TokenType::keyword("var"))?;
        let id = self.identifier()?;

        self.consume(TokenType::symbol("="))?;
        let init = self.disjunction()?;

        let end = &init.span();

        return Ok(Stmt::VarDeclaration(id, init, Span::combine(start, end)));
    }

    fn const_decl(&mut self) -> Result<Stmt, SyntaxError> {
        let start = &self.current.span.clone();

        self.consume(TokenType::keyword("con"))?;
        let id = self.identifier()?;

        self.consume(TokenType::symbol("="))?;
        let init = self.disjunction()?;

        let end = &init.span();

        return Ok(Stmt::ConDeclaration(id, init, Span::combine(start, end)));
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
                        ErrorKind::UnexpectedToken,
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
        let start = &node.span();

        match self.current.token_type.clone() {
            TokenType::Symbol(sym) => match &sym[..] {
                "=" => {
                    self.consume(TokenType::symbol("="))?;

                    let id = match node.clone() {
                        Stmt::Expr(Expr::Identifier(id)) => id,
                        _ => {
                            return Err(SyntaxError::error(
                                ErrorKind::ExpectedIdentifier,
                                "expected indentifier",
                                &Span::combine(start, &self.current.span),
                            ))
                        }
                    };

                    let val = self.disjunction()?;
                    let end = &val.span();

                    let node = Stmt::AssignStatement(id, val, Span::combine(start, end));
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
        let start = &node.span();
        loop {
            match &self.current.token_type {
                TokenType::Keyword(x) if x == "or" => {
                    self.consume(TokenType::keyword("or"))?;
                    node = Expr::Or(
                        Box::new(node),
                        Box::new(self.conjunction()?),
                        Span::combine(start, &self.current.span),
                    );
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn conjunction(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.comparison()?;
        let start = &node.span();
        loop {
            match &self.current.token_type {
                TokenType::Keyword(x) if x == "and" => {
                    self.consume(TokenType::keyword("and"))?;
                    node = Expr::And(
                        Box::new(node),
                        Box::new(self.comparison()?),
                        Span::combine(start, &self.current.span),
                    );
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn comparison(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.parse_sum()?;
        let start = &node.span();
        loop {
            match self.current.token_type.clone() {
                TokenType::Symbol(sym) if sym == "==" => {
                    self.consume(TokenType::symbol("=="))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::EqualTo, node, self.parse_sum()?)),
                        Span::combine(start, &self.current.span),
                    );
                }
                TokenType::Symbol(sym) if sym == "!=" => {
                    self.consume(TokenType::symbol("!="))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::NotEqual, node, self.parse_sum()?)),
                        Span::combine(start, &self.current.span),
                    );
                }
                TokenType::Symbol(sym) if sym == ">=" => {
                    self.consume(TokenType::symbol(">="))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::GreaterThanEquals, node, self.parse_sum()?)),
                        Span::combine(start, &self.current.span),
                    );
                }
                TokenType::Symbol(sym) if sym == "<=" => {
                    self.consume(TokenType::symbol("<="))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::LessThanEquals, node, self.parse_sum()?)),
                        Span::combine(start, &self.current.span),
                    );
                }
                TokenType::Symbol(sym) if sym == ">" => {
                    self.consume(TokenType::symbol(">"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::GreaterThan, node, self.parse_sum()?)),
                        Span::combine(start, &self.current.span),
                    );
                }
                TokenType::Symbol(sym) if sym == "<" => {
                    self.consume(TokenType::symbol("<"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::LessThan, node, self.parse_sum()?)),
                        Span::combine(start, &self.current.span),
                    );
                }
                _ => break,
            }
        }
        return Ok(node);
    }

    fn parse_sum(&mut self) -> Result<Expr, SyntaxError> {
        let mut node = self.parse_term()?;
        let start = &node.span();
        loop {
            match self.current.token_type.clone() {
                TokenType::Symbol(sym) if sym == "+" => {
                    self.consume(TokenType::symbol("+"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Add, node, self.parse_term()?)),
                        Span::combine(start, &self.current.span),
                    );
                }
                TokenType::Symbol(sym) if sym == "-" => {
                    self.consume(TokenType::symbol("-"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Subtract, node, self.parse_term()?)),
                        Span::combine(start, &self.current.span),
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
        let start = &node.span();
        loop {
            match self.current.token_type.clone() {
                TokenType::Symbol(sym) if sym == "*" => {
                    self.consume(TokenType::symbol("*"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Multiply, node, self.member_expr()?)),
                        Span::combine(start, &self.current.span),
                    );
                }
                TokenType::Symbol(sym) if sym == "/" => {
                    self.consume(TokenType::symbol("/"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Divide, node, self.member_expr()?)),
                        Span::combine(start, &self.current.span),
                    );
                }
                TokenType::Symbol(sym) if sym == "%" => {
                    self.consume(TokenType::symbol("%"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Remainder, node, self.member_expr()?)),
                        Span::combine(start, &self.current.span),
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
        let start = node.span();

        loop {
            match self.current.token_type.clone() {
                TokenType::Symbol(sym) if sym == "[" => {
                    self.consume(TokenType::symbol("["))?;
                    node = Expr::Index(
                        Box::new(node),
                        Box::new(self.disjunction()?),
                        Span::combine(&start, &self.current.span.clone()),
                    );
                    self.consume(TokenType::symbol("]"))?;
                }
                TokenType::Symbol(sym) if sym == "(" => {
                    node = Expr::FunCall(
                        Box::new(node),
                        Box::new(self.args()?),
                        Span::combine(&start, &self.current.span.clone()),
                    );
                }
                TokenType::Symbol(sym) if sym == "." => {
                    self.consume(TokenType::symbol("."))?;

                    let mut args = vec![node];
                    node = Expr::FunCall(
                        Box::new(Expr::Identifier(self.identifier()?)),
                        Box::new({
                            args.append(&mut self.args()?);
                            args
                        }),
                        Span::combine(&start, &self.current.span.clone()),
                    )
                }
                TokenType::Comment(typ) => {
                    self.consume(TokenType::Comment(typ))?;
                    continue;
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
                        ErrorKind::UnexpectedToken,
                        &format!("unexpected symbol `{}`", sym),
                        &self.current.span,
                    ))
                }
            },
            TokenType::Id => {
                return Ok(Expr::Identifier(self.identifier()?));
            }
            _ => {
                return Err(SyntaxError::error(
                    ErrorKind::UnexpectedToken,
                    &format!("unexpected token `{}`", self.current.token_val),
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
            match self.current.token_type.clone() {
                TokenType::Eof => break,
                TokenType::Newline => {
                    self.consume(TokenType::Newline)?;
                    continue;
                }
                TokenType::Comment(ref typ) => {
                    self.consume(TokenType::comment(typ))?;
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

        analyzer.run(&ast.nodes);

        if !analyzer.errors.is_empty() {
            let mut diagnostics = vec![];

            for error in &analyzer.errors {
                diagnostics.push(error.report());
            }

            Emitter::emit(diagnostics);

            let length = &analyzer.errors.len();

            return Err(SyntaxError::error(
                ErrorKind::CompileFail,
                &format!(
                    "aborting due to {} previous error{}",
                    length,
                    if *length == 1 { "" } else { "s" }
                ),
                &self.current.span,
            ));
        }

        return Ok(ast);
    }
}
