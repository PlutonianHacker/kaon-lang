use std::rc::Rc;

use crate::{
    common::{Span, Spanned},
    compiler::{
        ASTNode, BinExpr, Class, Constructor, Expr, Ident, Op, Fun, Stmt, Token,
        TokenType, TypePath, AST,
    },
    error::{Error, Item},
    Source,
};

use super::{
    ast::{Signature, Trait, TraitMethod, Field, FieldKind, Visibility},
    token::{Delimiter, Keyword, Literal, Symbol},
    Lexer,
};

/// Recursive descent parser for the Kaon language.
/// Takes a stream of [Token]s created by the [Lexer] and generates an [AST] from it.
///
/// # Errors
/// If the input has any syntactic errors, the parser will return them.
pub struct Parser {
    tokens: Spanned<Vec<Token>>,
    current: Token,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Spanned<Vec<Token>>) -> Parser {
        Parser {
            tokens,
            current: (TokenType::eof(), Span::empty()),
            pos: 0,
        }
    }

    pub fn parse_str(src: &str) -> Result<AST, Error> {
        let source = Source::new(src, "main");
        let tokens = Lexer::new(source).tokenize()?;

        let ast = Parser::new(tokens).parse()?;
        Ok(ast)
    }

    pub fn parse_source(source: Rc<Source>) -> Result<AST, Error> {
        let token_stream = Lexer::new(source).tokenize()?;
        let mut parser = Parser::new(token_stream);

        Ok(parser.parse()?)
    }

    /// Consume the next token.
    fn consume(&mut self, token_type: TokenType) -> Result<Span, Error> {
        match token_type {
            token_type if token_type == self.current.0 => {
                self.pos += 1;
                let old_token =
                    std::mem::replace(&mut self.current, self.tokens.node[self.pos].clone());
                Ok(old_token.1)
            }
            _ if self.current.0 == TokenType::Delimiter(Delimiter::Newline) => {
                self.expect_delimiter(Delimiter::Newline)?;
                self.consume(token_type)
            }
            _ => Err(self.error()),
        }
    }

    /// Consume a delimiter.
    fn expect_delimiter(&mut self, delimiter: Delimiter) -> Result<Span, Error> {
        self.consume(TokenType::Delimiter(delimiter))
    }

    /// Consume a keyword.
    fn expect_keyword(&mut self, keyword: Keyword) -> Result<Span, Error> {
        self.consume(TokenType::Keyword(keyword))
    }

    /// Consume a symbol.
    fn symbol(&mut self, symbol: Symbol) -> Result<Span, Error> {
        self.consume(TokenType::Symbol(symbol))
    }

    fn _expect(&mut self, token_type: TokenType) -> Result<Span, Error> {
        match token_type {
            token_type if token_type == self.current.0 => {
                self.pos += 1;
                let old_token =
                    std::mem::replace(&mut self.current, self.tokens.node[self.pos].clone());
                Ok(old_token.1)
            }
            TokenType::Delimiter(Delimiter::Eof) => Err(Error::UnexpectedEOF(Item::new(
                "<eof>",
                self.current.1.clone(),
            ))),
            _ => Err(Error::ExpectedToken(
                Item::new(&self.current.0.to_string(), self.current.1.clone()),
                Item::new(&self.current.0.to_string(), self.current.1.clone()),
            )),
        }
    }

    fn last(&self) -> &Token {
        &self.tokens.node[self.pos - 1]
    }

    /// Return an unexpected token error.
    fn error(&self) -> Error {
        Error::UnexpectedToken(Item::new(
            &self.current.0.to_string(),
            self.current.1.clone(),
        ))
    }

    /// Get the next token without checking it.
    fn next(&mut self) {
        self.pos += 1;
        self.current = self.tokens.node[self.pos].clone();
    }

    /// Parse a compound statement.
    fn compound_statement(&mut self) -> Result<Stmt, Error> {
        match &self.current.0 {
            TokenType::Keyword(Keyword::If) => self.if_statement(),
            TokenType::Keyword(Keyword::Loop) => self.loop_statement(),
            TokenType::Keyword(Keyword::While) => self.while_statement(),
            TokenType::Keyword(Keyword::Class) => self.class(),
            TokenType::Keyword(Keyword::Trait) => self.parse_trait(),
            TokenType::Keyword(Keyword::Fun) => self.fun(),
            TokenType::Keyword(Keyword::Public) => self.modifier(),
            TokenType::Delimiter(Delimiter::OpenBrace) => self.block(),
            _ => self.statement(),
        }
    }

    /// Parse a statement and consume a delimiter.
    fn statement(&mut self) -> Result<Stmt, Error> {
        let node = self.simple_statement();

        match &self.current.0 {
            TokenType::Delimiter(Delimiter::Newline) => {
                self.expect_delimiter(Delimiter::Newline)?;
                node
            }
            TokenType::Symbol(Symbol::SemiColon) => {
                self.symbol(Symbol::SemiColon)?;
                node
            }
            TokenType::Delimiter(Delimiter::Eof) | TokenType::Delimiter(Delimiter::CloseBrace) => {
                node
            }
            node => Err(Error::ExpectedNewline(Item::new(
                &node.to_string(),
                self.current.1.clone(),
            ))),
        }
    }

    /// Parse a statement.
    fn simple_statement(&mut self) -> Result<Stmt, Error> {
        match &self.current.0 {
            TokenType::Keyword(Keyword::Var) => self.var_decl(),
            TokenType::Keyword(Keyword::Final) => self.const_decl(),
            TokenType::Keyword(Keyword::Break) => self.break_stmt(),
            TokenType::Keyword(Keyword::Continue) => self.continue_stmt(),
            TokenType::Keyword(Keyword::Return) => self.return_stmt(),
            TokenType::Keyword(Keyword::Import) => self.import_stmt(),
            TokenType::Keyword(Keyword::Trait) => self.parse_trait(),
            _ => Ok(self.assignment_stmt()?),
        }
    }

    /// Parse a block.
    fn block(&mut self) -> Result<Stmt, Error> {
        self.expect_delimiter(Delimiter::OpenBrace)?;
        let mut nodes = vec![];
        loop {
            match &self.current.0 {
                TokenType::Delimiter(Delimiter::CloseBrace) => {
                    self.expect_delimiter(Delimiter::CloseBrace)?;
                    break;
                }
                TokenType::Delimiter(Delimiter::Newline) => {
                    self.expect_delimiter(Delimiter::Newline)?;
                    continue;
                }
                TokenType::Delimiter(Delimiter::Eof) => {
                    return Err(Error::UnexpectedEOF(Item::new(
                        "<eof>",
                        self.current.1.clone(),
                    )));
                }
                TokenType::Comment(_) => {
                    self.comment()?;
                    continue;
                }
                _ => {
                    nodes.push(self.compound_statement()?);
                }
            }
        }

        Ok(Stmt::Block(Box::new(nodes), self.tokens.source.clone()))
    }

    fn loop_statement(&mut self) -> Result<Stmt, Error> {
        self.expect_keyword(Keyword::Loop)?;
        Ok(Stmt::LoopStatement(
            Box::new(self.block()?),
            self.current.1.clone(),
        ))
    }

    fn while_statement(&mut self) -> Result<Stmt, Error> {
        self.expect_keyword(Keyword::While)?;
        Ok(Stmt::WhileStatement(
            self.disjunction()?,
            Box::new(self.block()?),
            self.current.1.clone(),
        ))
    }

    fn break_stmt(&mut self) -> Result<Stmt, Error> {
        let start = self.expect_keyword(Keyword::Break)?;
        Ok(Stmt::Break(start))
    }

    fn continue_stmt(&mut self) -> Result<Stmt, Error> {
        let start = self.expect_keyword(Keyword::Continue)?;
        Ok(Stmt::Continue(start))
    }

    fn if_statement(&mut self) -> Result<Stmt, Error> {
        self.expect_keyword(Keyword::If)?;

        let condition = self.disjunction()?;

        let block = self.block()?;
        let alternate = match &self.current.0 {
            TokenType::Keyword(Keyword::Else) => Some(self.parse_else_block()?),
            _ => None,
        };

        Ok(Stmt::IfStatement(
            condition,
            Box::new((block, alternate)),
            self.current.1.clone(),
        ))
    }

    fn parse_else_block(&mut self) -> Result<Stmt, Error> {
        self.expect_keyword(Keyword::Else)?;

        if let TokenType::Keyword(Keyword::If) = self.current.0 {
            self.if_statement()
        } else {
            self.block()
        }
    }

    fn modifier(&mut self) -> Result<Stmt, Error> {
        self.expect_keyword(Keyword::Public)?;

        match &self.current.0 {
            TokenType::Keyword(Keyword::Fun) => {
                let mut fun = self.fun_()?;
                fun.0.access = Visibility::Public;

                Ok(Stmt::Function(Box::new(fun.0), fun.1))
            }
            _ => Err(self.error()),
        }
    }

    fn class(&mut self) -> Result<Stmt, Error> {
        let start = self.expect_keyword(Keyword::Class)?;

        let name = self.identifier()?;

        let mut fields = vec![];
        let mut methods: Vec<Stmt> = vec![];
        let mut constructors = vec![];

        self.consume(TokenType::delimiter("{"))?;

        loop {
            match &self.current.0 {
                TokenType::Delimiter(Delimiter::CloseBrace) => {
                    break;
                }
                TokenType::Keyword(keyword) => match keyword {
                    Keyword::Create => {
                        constructors.push(self.constructor(name.clone())?);
                    }
                    Keyword::Fun => {
                        methods.push(self.fun()?);
                    }
                    Keyword::Var => {
                        fields.push(self.parse_field()?);
                    }
                    _ => return Err(self.error()),
                },
                TokenType::Delimiter(Delimiter::Newline) => {
                    self.expect_delimiter(Delimiter::Newline)?;
                    continue;
                }
                TokenType::Comment(_) => {
                    self.next();
                    continue;
                }
                TokenType::Delimiter(Delimiter::Eof) => {
                    return Err(Error::UnexpectedEOF(Item::new(
                        "<eof>",
                        self.current.1.clone(),
                    )));
                }
                _ => return Err(self.error()),
            }
        }

        let end = self.consume(TokenType::delimiter("}"))?;
        let class = Class::new(name, None, fields, methods, constructors);

        Ok(Stmt::Class(class, Span::combine(&start, &end)))
    }

    fn parse_field(&mut self) -> Result<Field, Error> {
        self.expect_keyword(Keyword::Var)?;

        let name = self.identifier()?;
        let kind = FieldKind::Var;
        let vis = Visibility::Private;
        let mut default = None;

        let typ = self.type_spec()?;

        match &self.current.0 {
            TokenType::Symbol(Symbol::Equal) => {
                self.next();

                default = Some(self.disjunction()?);
            },
            TokenType::Delimiter(..) => {},
            _ => return Err(Error::ExpectedToken(
                Item::new("=", self.current.1.clone()), 
                Item::new(&self.current.0.to_string(), self.current.1.clone()))),
        };

        let field = Field { name, visibility: vis, default, typ, kind };

        Ok(field)
    } 

    fn parse_trait(&mut self) -> Result<Stmt, Error> {
        self.expect_keyword(Keyword::Trait)?;

        let name = self.identifier()?;

        match &self.current.0 {
            TokenType::Symbol(Symbol::Colon) => self.trait_sum(name),
            TokenType::Delimiter(Delimiter::OpenBrace) => self.trait_decl(name),
            token => Err(Error::UnexpectedToken(Item::new(
                &token.to_string(),
                self.current.1.clone(),
            ))),
        }
    }

    fn trait_decl(&mut self, name: Ident) -> Result<Stmt, Error> {
        self.expect_delimiter(Delimiter::OpenBrace)?;

        let mut methods = Vec::new();

        while self.current.0 != TokenType::Delimiter(Delimiter::CloseBrace) {
            match &self.current.0 {
                TokenType::Delimiter(Delimiter::Newline) => {
                    self.next();
                    continue;
                }
                TokenType::Keyword(Keyword::Fun) => {
                    methods.push(self.trait_method()?);
                }
                token => {
                    return Err(Error::UnexpectedToken(Item::new(
                        &token.to_string(),
                        self.current.1.clone(),
                    )))
                }
            };
        }

        let end = self.expect_delimiter(Delimiter::CloseBrace)?;
        let span = Span::combine(&name.span(), &end);

        let trait_ = Trait { methods, span };

        Ok(Stmt::Trait(trait_))
    }

    fn trait_method(&mut self) -> Result<TraitMethod, Error> {
        let sig = self.fun_signature()?;
        let start = &sig.span.clone();

        let mut default = None;

        if let TokenType::Delimiter(Delimiter::OpenBrace) = self.current.0 {
            default = Some(self.block()?);
        }

        Ok(TraitMethod {
            sig,
            default,
            span: Span::combine(start, &self.last().1),
        })
    }

    fn fun_signature(&mut self) -> Result<Signature, Error> {
        let start = self.expect_keyword(Keyword::Fun)?;

        let name = self.identifier()?;
        let params = self.params()?;
        let return_typ = self.type_spec()?;

        let end = self.current.1.clone();

        Ok(Signature {
            name,
            params,
            return_typ,
            span: Span::combine(&start, &end),
        })
    }

    fn trait_sum(&mut self, _name: Ident) -> Result<Stmt, Error> {
        todo!()
    }

    fn constructor(&mut self, class: Ident) -> Result<Constructor, Error> {
        let _start = &self.consume(TokenType::keyword("create"))?;
        let name = self.identifier()?;
        let params = self.params()?;
        let body = self.block()?;
        let _end = &body.span();

        let constructor = Constructor::new(name, params.0, body, class);

        Ok(constructor)
    }

    fn fun(&mut self) -> Result<Stmt, Error> {
        let fun = self.fun_()?;
        Ok(Stmt::Function(Box::new(fun.0), fun.1))
    }

    fn fun_(&mut self) -> Result<(Fun, Span), Error> {
        let start = self.consume(TokenType::keyword("fun"))?;

        let name = self.identifier()?;
        let (params, types) = self.params()?;
        let return_typ = self.type_spec()?;
        let body = self.block()?;

        let end = &body.span();

        let access = Visibility::Public;

        let fun = Fun::new(name, params, body, types, return_typ, access);

        Ok((fun, Span::combine(&start, end)))
    }

    fn params(&mut self) -> Result<(Vec<Ident>, Vec<Option<Expr>>), Error> {
        self.expect_delimiter(Delimiter::OpenParen)?;
        let mut params = vec![];
        let mut typs = vec![];

        if TokenType::Delimiter(Delimiter::CloseParen) != self.current.0 {
            params.push(self.identifier()?);
            typs.push(self.type_spec()?);
        }

        loop {
            match &self.current.0 {
                TokenType::Symbol(Symbol::Comma) => {
                    self.consume(TokenType::symbol(","))?;
                    params.push(self.identifier()?);
                    typs.push(self.type_spec()?);
                }
                TokenType::Delimiter(Delimiter::CloseParen) => {
                    break;
                }
                _ => return Err(self.error()),
            }
        }

        self.expect_delimiter(Delimiter::CloseParen)?;
        Ok((params, typs))
    }

    fn return_stmt(&mut self) -> Result<Stmt, Error> {
        let start = &self.consume(TokenType::keyword("return"))?;

        let expr = if let TokenType::Delimiter(_) = self.current.0 {
            None
        } else {
            Some(self.disjunction()?)
        };

        let end = if let Some(expr) = &expr {
            expr.span()
        } else {
            start.clone()
        };

        let stmt = Stmt::Return(expr, Span::combine(start, &end));

        Ok(stmt)
    }

    fn import_stmt(&mut self) -> Result<Stmt, Error> {
        // import ...
        let start = &self.consume(TokenType::keyword("import"))?;

        // import name ...
        let import_name = self.dot_expr()?;

        let end = &import_name.span();

        Ok(Stmt::ImportStatement(
            import_name,
            Span::combine(start, end),
        ))
    }

    fn var_decl(&mut self) -> Result<Stmt, Error> {
        let start = &self.current.1.clone();

        self.consume(TokenType::keyword("var"))?;
        let id = self.identifier()?;

        let end = &id.span();

        let typ = self.type_spec()?;

        let init = if let TokenType::Symbol(Symbol::Equal) = self.current.0 {
            self.consume(TokenType::symbol("="))?;
            Some(self.disjunction()?)
        } else {
            None
        };

        Ok(Stmt::VarDeclaration(
            id,
            init,
            typ,
            Span::combine(start, end),
        ))
    }

    fn const_decl(&mut self) -> Result<Stmt, Error> {
        let start = &self.current.1.clone();

        self.consume(TokenType::keyword("con"))?;
        let id = self.identifier()?;

        let typ = self.type_spec()?;

        self.consume(TokenType::symbol("="))?;
        let init = self.disjunction()?;

        let end = &init.span();

        Ok(Stmt::ConDeclaration(
            id,
            init,
            typ,
            Span::combine(start, end),
        ))
    }

    fn type_spec(&mut self) -> Result<Option<Expr>, Error> {
        if let TokenType::Symbol(Symbol::Colon) = self.current.0 {
            self.consume(TokenType::symbol(":"))?;

            let start = self.current.1.clone();
            let typ_path = self.type_path()?;

            Ok(Some(Expr::Type(typ_path, start)))
        } else {
            Ok(None)
        }
    }

    fn type_path(&mut self) -> Result<TypePath, Error> {
        let ident = self.identifier()?;

        let arguments = match &self.current.0 {
            TokenType::Symbol(Symbol::LeftAngleBracket) => {
                self.consume(TokenType::symbol("<"))?;
                let argument = Some(Box::new(self.type_path()?));
                self._expect(TokenType::symbol(">"))?;
                argument
            }
            _ => None,
        };

        Ok(TypePath { ident, arguments })
    }

    fn args(&mut self) -> Result<Vec<Expr>, Error> {
        self.expect_delimiter(Delimiter::OpenParen)?;

        let mut args = vec![];

        if self.current.0 != TokenType::delimiter(")") {
            args.push(self.disjunction()?);
        }

        loop {
            match &self.current.0 {
                TokenType::Delimiter(Delimiter::CloseParen) => {
                    break;
                }
                TokenType::Symbol(Symbol::Comma) => {
                    self.consume(TokenType::symbol(","))?;
                    args.push(self.disjunction()?);
                }
                _ => break,
            }
        }

        self.expect_delimiter(Delimiter::CloseParen)?;

        Ok(args)
    }

    fn assignment_stmt(&mut self) -> Result<Stmt, Error> {
        let node = self.expression()?;
        let start = &node.span();

        if let TokenType::Symbol(Symbol::Equal) = &self.current.0 {
            self.consume(TokenType::symbol("="))?;

            let id = match node {
                Stmt::Expr(expr) => expr,
                _ => {
                    return Err(Error::ExpectedToken(
                        Item::new("identifer", self.current.1.clone()),
                        Item::new(&self.current.0.to_string(), self.current.1.clone()),
                    ))
                }
            };

            let val = self.disjunction()?;
            let end = &val.span();

            let node = Stmt::AssignStatement(id, val, Span::combine(start, end));
            return Ok(node);
        }

        Ok(node)
    }

    fn expression(&mut self) -> Result<Stmt, Error> {
        Ok(Stmt::Expr(self.disjunction()?))
    }

    fn disjunction(&mut self) -> Result<Expr, Error> {
        let mut node = self.conjunction()?;
        let start = &node.span();
        while let TokenType::Keyword(Keyword::Or) = &self.current.0 {
            self.consume(TokenType::keyword("or"))?;
            node = Expr::Or(
                Box::new(node),
                Box::new(self.conjunction()?),
                Span::combine(start, &self.current.1),
            );
        }

        Ok(node)
    }

    fn conjunction(&mut self) -> Result<Expr, Error> {
        let mut node = self.bitwise_or()?;
        let start = &node.span();
        loop {
            if let TokenType::Keyword(Keyword::And) = &self.current.0 {
                self.consume(TokenType::keyword("and"))?;
                node = Expr::And(
                    Box::new(node),
                    Box::new(self.bitwise_or()?),
                    Span::combine(start, &self.current.1),
                );
            } else {
                break;
            }
        }
        Ok(node)
    }

    fn bitwise_or(&mut self) -> Result<Expr, Error> {
        let mut node = self.bitwise_xor()?;

        loop {
            if let TokenType::Symbol(Symbol::Or) = &self.current.0 {
                self.next();
                let start = &node.span();

                node = Expr::BinExpr(
                    Box::new(BinExpr::new(Op::BitwiseOr, node, self.bitwise_xor()?)),
                    Span::combine(start, &self.current.1),
                );
            } else {
                break;
            }
        }

        Ok(node)
    }

    fn bitwise_xor(&mut self) -> Result<Expr, Error> {
        let mut node = self.bitwise_and()?;

        loop {
            if let TokenType::Symbol(Symbol::Xor) = &self.current.0 {
                self.next();
                let start = &node.span();

                node = Expr::BinExpr(
                    Box::new(BinExpr::new(Op::BitwiseXor, node, self.bitwise_and()?)),
                    Span::combine(start, &self.current.1),
                );
            } else {
                break;
            }
        }

        Ok(node)
    }

    fn bitwise_and(&mut self) -> Result<Expr, Error> {
        let mut node = self.comparison()?;

        loop {
            if let TokenType::Symbol(Symbol::And) = &self.current.0 {
                self.next();
                let start = &node.span();

                node = Expr::BinExpr(
                    Box::new(BinExpr::new(Op::BitwiseAnd, node, self.comparison()?)),
                    Span::combine(start, &self.current.1),
                );
            } else {
                break;
            }
        }

        Ok(node)
    }

    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut node = self.parse_sum()?;
        let start = &node.span();

        loop {
            match &self.current.0 {
                TokenType::Symbol(Symbol::EqualsEquals) => {
                    self.consume(TokenType::symbol("=="))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::EqualTo, node, self.parse_sum()?)),
                        Span::combine(start, &self.current.1),
                    );
                }
                TokenType::Symbol(Symbol::NotEqual) => {
                    self.consume(TokenType::symbol("!="))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::NotEqual, node, self.parse_sum()?)),
                        Span::combine(start, &self.current.1),
                    );
                }
                TokenType::Symbol(Symbol::GreaterThanEqual) => {
                    self.consume(TokenType::symbol(">="))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::GreaterThanEquals, node, self.parse_sum()?)),
                        Span::combine(start, &self.current.1),
                    );
                }
                TokenType::Symbol(Symbol::LessThanEqual) => {
                    self.consume(TokenType::symbol("<="))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::LessThanEquals, node, self.parse_sum()?)),
                        Span::combine(start, &self.current.1),
                    );
                }
                TokenType::Symbol(Symbol::RightAngleBracket) => {
                    self.consume(TokenType::symbol(">"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::GreaterThan, node, self.parse_sum()?)),
                        Span::combine(start, &self.current.1),
                    );
                }
                TokenType::Symbol(Symbol::LeftAngleBracket) => {
                    self.consume(TokenType::symbol("<"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::LessThan, node, self.parse_sum()?)),
                        Span::combine(start, &self.current.1),
                    );
                }
                _ => break,
            }
        }
        Ok(node)
    }

    /// Parse bitwise `and`, `or` operators.
    fn _bitwise_and_or(&mut self) -> Result<Expr, Error> {
        let node = self.parse_sum()?;

        Ok(node)
    }

    fn parse_sum(&mut self) -> Result<Expr, Error> {
        let mut node = self.parse_term()?;
        let start = &node.span();
        loop {
            match &self.current.0 {
                TokenType::Symbol(Symbol::Plus) => {
                    self.consume(TokenType::symbol("+"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Add, node, self.parse_term()?)),
                        Span::combine(start, &self.current.1),
                    );
                }
                TokenType::Symbol(Symbol::Hypen) => {
                    self.consume(TokenType::symbol("-"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Subtract, node, self.parse_term()?)),
                        Span::combine(start, &self.current.1),
                    );
                }
                _ => {
                    break;
                }
            }
        }
        Ok(node)
    }

    fn parse_term(&mut self) -> Result<Expr, Error> {
        let mut node = self.member_expr()?;
        let start = &node.span();
        loop {
            match &self.current.0 {
                TokenType::Symbol(Symbol::Star) => {
                    self.consume(TokenType::symbol("*"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Multiply, node, self.member_expr()?)),
                        Span::combine(start, &self.current.1),
                    );
                }
                TokenType::Symbol(Symbol::Slash) => {
                    self.consume(TokenType::symbol("/"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Divide, node, self.member_expr()?)),
                        Span::combine(start, &self.current.1),
                    );
                }
                TokenType::Symbol(Symbol::Modulo) => {
                    self.consume(TokenType::symbol("%"))?;
                    node = Expr::BinExpr(
                        Box::new(BinExpr::new(Op::Remainder, node, self.member_expr()?)),
                        Span::combine(start, &self.current.1),
                    );
                }
                _ => {
                    break;
                }
            }
        }
        Ok(node)
    }

    fn member_expr(&mut self) -> Result<Expr, Error> {
        let mut node = self.dot_expr()?;
        let start = node.span();

        loop {
            match self.current.0.clone() {
                TokenType::Delimiter(Delimiter::OpenBracket) => {
                    self.expect_delimiter(Delimiter::OpenBracket)?;
                    node = Expr::Index(
                        Box::new(node),
                        Box::new(self.disjunction()?),
                        Span::combine(&start, &self.current.1.clone()),
                    );
                    self.expect_delimiter(Delimiter::CloseBracket)?;
                }
                TokenType::Delimiter(Delimiter::OpenParen) => {
                    node = Expr::FunCall(
                        Box::new(node),
                        Box::new(self.args()?),
                        Span::combine(&start, &self.current.1.clone()),
                    );
                }
                TokenType::Comment(typ) => {
                    self.consume(TokenType::Comment(typ))?;
                    continue;
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn dot_expr(&mut self) -> Result<Expr, Error> {
        let mut node = self.paren_expr()?;
        let start = node.span();

        loop {
            match &self.current.0 {
                TokenType::Symbol(Symbol::Dot) => {
                    self.consume(TokenType::symbol("."))?;

                    node = Expr::MemberExpr(
                        Box::new(node),
                        Box::new(self.paren_expr()?),
                        Span::combine(&start, &self.current.1.clone()),
                    );
                }
                TokenType::Symbol(Symbol::Colon) => {
                    self.consume(TokenType::symbol(":"))?;

                    node = Expr::AssocExpr(
                        Box::new(node),
                        Box::new(self.paren_expr()?),
                        Span::combine(&start, &self.current.1),
                    );
                }
                TokenType::Comment(_) => {
                    self.comment()?;
                    continue;
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn paren_expr(&mut self) -> Result<Expr, Error> {
        if let TokenType::Delimiter(Delimiter::OpenParen) = self.current.0 {
            let start = &self.expect_delimiter(Delimiter::OpenParen)?;
            let node = self.disjunction()?;

            if let TokenType::Symbol(Symbol::Comma) = self.current.0 {
                todo!()
            }

            let end = &self.expect_delimiter(Delimiter::CloseParen)?;
            return Ok(Expr::ParenExpr(Box::new(node), Span::combine(start, end)));
        }

        self.factor()
    }

    fn factor(&mut self) -> Result<Expr, Error> {
        let node;
        match &self.current.0 {
            TokenType::Literal(literal) => match literal {
                Literal::NumberLiteral(number) => {
                    node = Expr::Number(number.parse::<f64>().unwrap(), self.current.1.clone());
                    self.next();
                }
                Literal::StringLiteral(string) => {
                    node = Expr::String(string.to_owned(), self.current.1.clone());
                    self.next();
                }
                Literal::CharLiteral(_) => {
                    unimplemented!()
                }
                Literal::False => {
                    node = Expr::Boolean(false, self.current.1.clone());
                    self.next();
                }
                Literal::True => {
                    node = Expr::Boolean(true, self.current.1.clone());
                    self.next();
                }
                Literal::Nil => {
                    node = Expr::Nil(self.current.1.clone());
                    self.next();
                }
                Literal::Id(_) => {
                    node = Expr::Identifier(self.identifier()?);
                }
            },
            TokenType::Symbol(sym) => match sym {
                Symbol::Plus => {
                    self.consume(TokenType::symbol("+"))?;
                    node = Expr::UnaryExpr(
                        Op::Add,
                        Box::new(self.paren_expr()?),
                        self.current.1.clone(),
                    );
                }
                Symbol::Hypen => {
                    self.consume(TokenType::symbol("-"))?;
                    node = Expr::UnaryExpr(
                        Op::Subtract,
                        Box::new(self.paren_expr()?),
                        self.current.1.clone(),
                    );
                }
                Symbol::Bang => {
                    self.consume(TokenType::symbol("!"))?;
                    node = Expr::UnaryExpr(
                        Op::Bang,
                        Box::new(self.paren_expr()?),
                        self.current.1.clone(),
                    );
                }
                sym => {
                    return Err(Error::UnexpectedToken(Item::new(
                        &sym.to_string(),
                        self.current.1.clone(),
                    )))
                }
            },
            TokenType::Delimiter(delimiter) => match delimiter {
                Delimiter::OpenParen => node = self.tuple()?,
                Delimiter::OpenBracket => node = self.list()?,
                Delimiter::OpenBrace => node = self.map()?,
                Delimiter::Newline => {
                    self.next();
                    node = self.factor()?;
                }
                sym => {
                    return Err(Error::UnexpectedToken(Item::new(
                        &sym.to_string(),
                        self.current.1.clone(),
                    )))
                }
            },
            TokenType::Keyword(Keyword::Self_) => {
                node = Expr::SelfExpr(self.current.1.clone());
                self.next();
            }
            _ => {
                return Err(Error::UnexpectedToken(Item::new(
                    &self.current.0.to_string(),
                    self.current.1.clone(),
                )))
            }
        }
        Ok(node)
    }

    fn list(&mut self) -> Result<Expr, Error> {
        self.expect_delimiter(Delimiter::OpenBracket)?;

        let mut nodes = vec![];

        loop {
            match &self.current.0 {
                TokenType::Symbol(Symbol::Comma) => {
                    self.next();
                }
                TokenType::Delimiter(Delimiter::CloseBracket) => {
                    break;
                }
                _ => nodes.push(self.disjunction()?),
            }
        }

        self.expect_delimiter(Delimiter::CloseBracket)?;

        Ok(Expr::List(Box::new(nodes), self.current.1.clone()))
    }

    fn tuple(&mut self) -> Result<Expr, Error> {
        let start = self.expect_delimiter(Delimiter::OpenParen)?;

        let mut tuple = Vec::new();
        let node = self.disjunction()?;

        if let TokenType::Delimiter(Delimiter::CloseParen) = self.current.0 {
            self.expect_delimiter(Delimiter::CloseParen)?;
            return Ok(node);
        }

        tuple.push(node);

        while let TokenType::Symbol(Symbol::Comma) = &self.current.0 {
            self.consume(TokenType::symbol(","))?;
            tuple.push(self.disjunction()?);
        }

        let end = &self.expect_delimiter(Delimiter::CloseParen)?;

        Ok(Expr::Tuple(Box::new(tuple), Span::combine(&start, end)))
    }

    fn map(&mut self) -> Result<Expr, Error> {
        let start = &self.expect_delimiter(Delimiter::OpenBrace)?;

        if let TokenType::Delimiter(Delimiter::CloseBrace) = self.current.0 {
            let end = &self.expect_delimiter(Delimiter::CloseBrace)?;

            return Ok(Expr::Map(Box::new(vec![]), Span::combine(start, end)));
        }

        let mut map = vec![];
        loop {
            let key = self.factor()?;
            self.consume(TokenType::symbol(":"))?;
            let value = self.disjunction()?;

            map.push((key, value));

            match &self.current.0 {
                TokenType::Delimiter(Delimiter::CloseBrace) => break,
                _ => {
                    self.consume(TokenType::symbol(","))?;
                    continue;
                }
            }
        }

        let end = &self.expect_delimiter(Delimiter::CloseBrace)?;

        Ok(Expr::Map(Box::new(map), Span::combine(start, end)))
    }

    /// Consumes an identifier from the token stream and returns an [`Ident`].
    fn identifier(&mut self) -> Result<Ident, Error> {
        let name = self.current.0.to_string();
        let span = self.current.1.clone();
        self.consume(TokenType::Literal(Literal::Id(name.clone())))?;
        Ok(Ident::Name { name, span })
    }

    fn comment(&mut self) -> Result<(), Error> {
        self.next();

        Ok(())
    }

    pub fn parse_file(&mut self) -> Result<AST, Error> {
        let mut nodes = vec![];

        loop {
            match &self.current.0 {
                TokenType::Delimiter(Delimiter::Eof) => break,
                TokenType::Delimiter(Delimiter::Newline) => {
                    self.expect_delimiter(Delimiter::Newline)?;
                    continue;
                }
                TokenType::Comment(_) => {
                    self.comment()?;
                    continue;
                }
                _ => nodes.push(ASTNode::Stmt(self.compound_statement()?)),
            }
        }

        Ok(AST::new(nodes, self.tokens.source.clone()))
    }

    pub fn parse(&mut self) -> Result<AST, Error> {
        self.current = self.tokens.node[self.pos].clone();

        let ast = self.parse_file()?;

        Ok(ast)
    }
}
