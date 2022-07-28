use crate::{
    common::Span,
    compiler::{ASTNode, BinExpr, Class, Expr, Fun, Ident, Op, Stmt, TypePath, AST},
    error::{Error, Item},
};

use super::{
    ast::{Field, Visibility},
    query::{Id, MetaItem, Query},
    typ::Typ,
};

#[derive(Clone, Debug)]
pub struct Scope {
    symbols: Vec<Symbol>,
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            symbols: Vec::new(),
        }
    }

    pub fn insert(&mut self, symbol: Symbol) {
        self.symbols.push(symbol);
    }

    pub fn has_symbol(&mut self, name: &str) -> bool {
        self.find(name).is_some()
    }

    pub fn find(&mut self, name: &str) -> Option<&Symbol> {
        self.symbols.iter().filter(|sym| sym.0 == name).last()
    }
}

#[derive(Clone, Debug)]
pub struct Symbol(pub String, pub Id, pub Span);

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl std::hash::Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
        self.2.hash(state);
    }
}

impl Eq for Symbol {}

#[derive(Debug)]
pub struct ScopedMap {
    scopes: Vec<Scope>,
}

impl Default for ScopedMap {
    fn default() -> Self {
        Self::new()
    }
}

impl ScopedMap {
    pub fn new() -> Self {
        ScopedMap {
            scopes: vec![Scope::new()],
        }
    }

    pub fn with_global_scope(scope: Scope) -> Self {
        Self {
            scopes: vec![scope],
        }
    }

    pub fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn insert(&mut self, symbol: Symbol) {
        self.current_scope().insert(symbol);
    }

    pub fn find(&mut self, symbol: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter_mut() {
            match scope.find(symbol) {
                Some(symbol) => return Some(symbol),
                None => continue,
            }
        }

        None
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }
}

#[derive(Default)]
pub struct Resolver {
    symbols: ScopedMap,
    unresolved_symbols: Vec<(String, Span)>,
    pub errors: Vec<Error>,
}

impl Resolver {
    pub fn with_scope(scope: &mut Scope) -> Self {
        Self {
            symbols: ScopedMap::with_global_scope(scope.to_owned()),
            ..Self::default()
        }
    }

    pub fn resolve_ast(&mut self, ast: &mut AST) {
        for node in &mut ast.nodes {
            let result = match node {
                ASTNode::Stmt(stmt) => self.statment(stmt, &mut ast.query),
                ASTNode::Expr(expr) => self.expression(expr, &mut ast.query),
            };

            if let Err(error) = result {
                self.errors.push(error);
            }
        }

        for unresolved_symbol in &self.unresolved_symbols {
            self.errors.push(Error::UnresolvedIdentifier(Item::new(
                &unresolved_symbol.0,
                unresolved_symbol.1.clone(),
            )))
        }
    }

    fn declare_symbol(&mut self, symbol: Symbol) {
        let name = &symbol.0;
        self.unresolved_symbols.retain(|s| &s.0 != name);
        self.symbols.insert(symbol);
    }

    pub fn global_scope(&mut self) -> Scope {
        self.symbols.scopes.first().unwrap().clone()
    }

    pub fn resolve_fun(&mut self, name: &Ident, query: &mut Query) -> Result<(), Error> {
        if self
            .symbols
            .current_scope()
            .has_symbol(&name.name_of(query))
        {
            let duplicate = self
                .symbols
                .current_scope()
                .find(&name.name_of(query))
                .unwrap();

            return Err(Error::DuplicateFun(
                Item::new(&name.name_of(query), name.span()),
                Item::new(&duplicate.0.clone(), duplicate.2.clone()),
            ));
        } else {
            Ok(())
        }
    }

    pub fn resolve_id(&mut self, name: &Ident, query: &mut Query) -> Result<(), Error> {
        if self
            .symbols
            .current_scope()
            .has_symbol(&name.name_of(query))
        {
            let duplicate = self
                .symbols
                .current_scope()
                .find(&name.name_of(query))
                .unwrap();

            return Err(Error::DuplicateIdentifier(
                Item::new(&name.name_of(query), name.span()),
                Item::new(&duplicate.0.clone(), duplicate.2.clone()),
            ));
        } else {
            Ok(())
        }
    }

    pub fn declare_id(
        &mut self,
        ident: &mut Ident,
        vis: Visibility,
        query: &mut Query,
    ) -> Result<(), Error> {
        let name = ident.name_of(query);
        let span = ident.span();

        let id = query.add_item(MetaItem::new(name.clone(), Typ::unknown(), vis));

        *ident = Ident::Id { id, span };

        let symbol = Symbol(name, id, ident.span());
        self.declare_symbol(symbol);

        Ok(())
    }
}

impl Resolver {
    fn statment(&mut self, stmt: &mut Stmt, query: &mut Query) -> Result<(), Error> {
        match stmt {
            Stmt::Block(stmts, _) => self.block(stmts, query),
            Stmt::IfStatement(expr, body, _) => self.if_statement(expr, body, query),
            Stmt::WhileStatement(expr, body, _) => self.while_statement(expr, body, query),
            Stmt::LoopStatement(body, _) => self.loop_statement(body, query),
            Stmt::ImportStatement(import, _) => self.import_statement(import, query),
            Stmt::VarDeclaration(ident, expr, _, _) => self.var_decl(ident, expr, query),
            Stmt::ConDeclaration(ident, expr, _, _) => self.con_decl(ident, expr, query),
            Stmt::AssignStatement(ident, expr, _) => self.assign_stmt(ident, expr, query),
            Stmt::Function(fun, _) => self.fun(fun, query),
            Stmt::Class(class, _) => self.class(class, query),
            Stmt::Return(expr, _) => self.return_stmt(expr, query),
            Stmt::Break(_) => self.break_stmt(),
            Stmt::Continue(_) => self.continue_stmt(),
            Stmt::Expr(expr) => self.expression(expr, query),
            Stmt::Trait(_trait_) => todo!(), //self.trait_decl(trait_),
        }
    }

    fn block(&mut self, stmts: &mut [Stmt], query: &mut Query) -> Result<(), Error> {
        self.symbols.enter_scope();
        for node in stmts {
            self.statment(node, query)?;
        }

        self.symbols.exit_scope();
        Ok(())
    }

    fn if_statement(
        &mut self,
        expr: &mut Expr,
        body: &mut (Stmt, Option<Stmt>),
        query: &mut Query,
    ) -> Result<(), Error> {
        self.expression(expr, query)?;
        self.statment(&mut body.0, query)?;
        if let Some(stmt) = &mut body.1 {
            self.statment(stmt, query)?;
        }

        Ok(())
    }

    fn while_statement(
        &mut self,
        expr: &mut Expr,
        body: &mut Stmt,
        query: &mut Query,
    ) -> Result<(), Error> {
        self.expression(expr, query)?;
        self.statment(body, query)
    }

    fn loop_statement(&mut self, body: &mut Stmt, query: &mut Query) -> Result<(), Error> {
        self.statment(body, query)
    }

    fn import_statement(&mut self, _import: &Expr, _query: &mut Query) -> Result<(), Error> {
        todo!();
    }

    fn class(&mut self, class: &mut Class, query: &mut Query) -> Result<(), Error> {
        if self.symbols.current_scope().has_symbol(&class.name(query)) {
            let duplicate = self
                .symbols
                .current_scope()
                .find(&class.name(query))
                .unwrap();

            return Err(Error::DuplicateClass(
                Item::new(&class.name.name_of(query), class.name.span()),
                Item::new(&duplicate.0.clone(), duplicate.2.clone()),
            ));
        } else {
            self.declare_id(&mut class.name, class.visibility, query)?;

            self.symbols.enter_scope();

            self.fields(&mut class.fields, query)?;

            for ctor in &mut class.constructors {
                self.constructor(ctor, &class.name, query)?;
            }

            for method in &mut class.methods {
                self.statment(method, query)?;
            }

            self.symbols.exit_scope();
        }

        Ok(())
    }

    fn fields(&mut self, fields: &mut [Field], query: &mut Query) -> Result<(), Error> {
        for field in fields {
            self.resolve_id(&field.name, query)?;
            self.declare_id(&mut field.name, field.visibility, query)?;
        }

        Ok(())
    }

    fn constructor(
        &mut self,
        ctor: &mut super::ast::Constructor,
        class_id: &Ident,
        query: &mut Query,
    ) -> Result<(), Error> {
        self.resolve_fun(&ctor.name, query)?;

        self.declare_id(&mut ctor.name, ctor.visibility, query)?;

        ctor.class = class_id.clone();

        for param in &mut ctor.params {
            self.resolve_id(param, query)?;
            self.declare_id(param, Visibility::Private, query)?;
        }

        self.statment(&mut ctor.body, query)?;

        Ok(())
    }

    fn fun(&mut self, fun: &mut Fun, query: &mut Query) -> Result<(), Error> {
        self.resolve_fun(&fun.name, query)?;

        self.declare_id(&mut fun.name, fun.access, query)?;

        self.symbols.enter_scope();

        for param in fun.params.iter_mut() {
            self.resolve_id(&param, query)?;

            self.declare_id(param, Visibility::Public, query)?;
        }

        if let Stmt::Block(stmts, _) = &mut fun.body {
            for stmt in (*stmts).iter_mut() {
                if let Err(err) = self.statment(stmt, query) {
                    self.symbols.exit_scope();
                    return Err(err);
                }
            }
        }

        self.symbols.exit_scope();

        Ok(())
    }

    fn var_decl(
        &mut self,
        ident: &mut Ident,
        init: &mut Option<Expr>,
        query: &mut Query,
    ) -> Result<(), Error> {
        self.resolve_id(ident, query)?;

        if let Some(expr) = init {
            self.expression(expr, query)?;
        }

        self.declare_id(ident, Visibility::Private, query)?;

        Ok(())
    }

    fn con_decl(
        &mut self,
        ident: &mut Ident,
        init: &mut Expr,
        query: &mut Query,
    ) -> Result<(), Error> {
        self.resolve_id(ident, query)?;

        self.expression(init, query)?;

        self.declare_id(ident, Visibility::Private, query)?;

        Ok(())
    }

    fn assign_stmt(
        &mut self,
        ident: &mut Expr,
        expr: &mut Expr,
        query: &mut Query,
    ) -> Result<(), Error> {
        self.expression(ident, query)?;

        self.expression(expr, query)
    }

    fn return_stmt(&mut self, expr: &mut Option<Expr>, query: &mut Query) -> Result<(), Error> {
        match expr {
            Some(expr) => self.expression(expr, query),
            None => Ok(()),
        }
    }

    fn break_stmt(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn continue_stmt(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn expression(&mut self, expr: &mut Expr, query: &mut Query) -> Result<(), Error> {
        match expr {
            Expr::Number(val, _) => self.number(val),
            Expr::String(val, _) => self.string(val),
            Expr::Boolean(val, _) => self.boolean(val),
            Expr::Unit(_) | Expr::Nil(_) => self.nil(),
            Expr::Identifier(ident) => self.identifier(ident, query),
            Expr::SelfExpr(_) => self.self_expr(query),
            Expr::BinExpr(bin_expr, _) => self.binary_expr(bin_expr, query),
            Expr::UnaryExpr(op, unary_expr, _) => self.unary_expr(op, unary_expr, query),
            Expr::ParenExpr(expr, _) => self.expression(&mut *expr, query),
            Expr::Index(expr, index, _) => self.index(expr, index, query),
            Expr::List(list, _) => self.list((list).to_vec(), query),
            Expr::Tuple(tuple, _) => self.tuple(tuple, query),
            Expr::Map(map, _) => self.map(map, query),
            Expr::Or(lhs, rhs, _) => self.or(lhs, rhs, query),
            Expr::And(lhs, rhs, _) => self.and(lhs, rhs, query),
            Expr::FunCall(callee, args, _) => self.fun_call(callee, args, query),
            Expr::MemberExpr(obj, prop, _) => self.member_expr(obj, prop, query),
            Expr::AssocExpr(obj, prop, _) => self.assoc_expr(obj, prop, query),
            Expr::Type(typ, _) => self.type_spec(typ, query),
        }
    }

    fn type_spec(&mut self, _typ: &TypePath, _query: &mut Query) -> Result<(), Error> {
        Ok(())
    }

    fn and(&mut self, lhs: &mut Expr, rhs: &mut Expr, query: &mut Query) -> Result<(), Error> {
        self.expression(lhs, query)?;
        self.expression(rhs, query)
    }

    fn or(&mut self, lhs: &mut Expr, rhs: &mut Expr, query: &mut Query) -> Result<(), Error> {
        self.expression(lhs, query)?;
        self.expression(rhs, query)
    }

    fn binary_expr(&mut self, bin_expr: &mut BinExpr, query: &mut Query) -> Result<(), Error> {
        self.expression(&mut bin_expr.rhs, query)?;
        self.expression(&mut bin_expr.lhs, query)?;

        Ok(())
    }

    fn unary_expr(&mut self, _op: &Op, expr: &mut Expr, query: &mut Query) -> Result<(), Error> {
        self.expression(expr, query)
    }

    fn index(&mut self, expr: &mut Expr, index: &mut Expr, query: &mut Query) -> Result<(), Error> {
        self.expression(expr, query)?;
        self.expression(index, query)
    }

    fn list(&mut self, list: Vec<Expr>, query: &mut Query) -> Result<(), Error> {
        for mut item in list {
            self.expression(&mut item, query)?;
        }

        Ok(())
    }

    fn tuple(&mut self, tuple: &mut [Expr], query: &mut Query) -> Result<(), Error> {
        for item in tuple {
            self.expression(item, query)?;
        }
        Ok(())
    }

    fn map(&mut self, map: &mut [(Expr, Expr)], query: &mut Query) -> Result<(), Error> {
        let mut values = Vec::new();

        for (key, value) in map {
            if values.contains(&key) {
                return Err(Error::DuplicateKey(Item::new("key", key.span())));
            }

            values.push(key);
            self.expression(value, query)?;
        }

        Ok(())
    }

    fn fun_call(
        &mut self,
        callee: &mut Expr,
        args: &mut [Expr],
        query: &mut Query,
    ) -> Result<(), Error> {
        self.expression(callee, query)?;
        for arg in args {
            self.expression(arg, query)?;
        }

        Ok(())
    }

    fn member_expr(
        &mut self,
        obj: &mut Expr,
        _prop: &mut Expr,
        query: &mut Query,
    ) -> Result<(), Error> {
        // TODO: resolve names in core library

        self.expression(obj, query)?;

        Ok(())
    }

    fn assoc_expr(&mut self, obj: &mut Expr, _prop: &Expr, query: &mut Query) -> Result<(), Error> {
        self.expression(obj, query)
    }

    fn self_expr(&mut self, _query: &mut Query) -> Result<(), Error> {
        Ok(())
    }

    fn identifier(&mut self, ident: &mut Ident, query: &mut Query) -> Result<(), Error> {
        let symbol = self.symbols.find(&ident.name_of(query));

        if let Some(symbol) = symbol {
            *ident = Ident::Id {
                id: symbol.1,
                span: symbol.2.clone(),
            };
        } else {
            self.unresolved_symbols
                .push((ident.name_of(query).clone(), ident.span()));
        }

        Ok(())
    }

    fn number(&mut self, _val: &f64) -> Result<(), Error> {
        Ok(())
    }

    fn string(&mut self, _val: &str) -> Result<(), Error> {
        Ok(())
    }

    fn boolean(&mut self, _val: &bool) -> Result<(), Error> {
        Ok(())
    }

    fn nil(&mut self) -> Result<(), Error> {
        Ok(())
    }
}
