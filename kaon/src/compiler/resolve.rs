use crate::{
    common::Span,
    compiler::{ASTNode, BinExpr, Class, Expr, Ident, Op, Pass, ScriptFun, Stmt, AST, TypePath},
    error::{Error, Item},
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
pub struct Symbol(pub String, pub Span);

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl std::hash::Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
        self.1.hash(state);
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
            scopes: vec![scope]
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
    unresolved_symbols: Vec<Symbol>,
    pub errors: Vec<Error>,
}

impl Resolver {
    pub fn with_scope(scope: &mut Scope) -> Self {
        Self {
            symbols: ScopedMap::with_global_scope(scope.to_owned()),
            ..Self::default()
        }
    }

    pub fn resolve_ast(&mut self, ast: &AST) {
        for node in &ast.nodes {
            let result = match node {
                ASTNode::Stmt(stmt) => self.statment(stmt),
                ASTNode::Expr(expr) => self.expression(expr),
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
}

impl Pass<(), Error> for Resolver {
    fn block(&mut self, stmts: &[Stmt]) -> Result<(), Error> {
        self.symbols.enter_scope();
        for node in stmts {
            self.statment(node)?;
        }

        self.symbols.exit_scope();
        Ok(())
    }

    fn if_statement(&mut self, expr: &Expr, body: &(Stmt, Option<Stmt>)) -> Result<(), Error> {
        self.expression(expr)?;
        self.statment(&body.0)?;
        if let Some(stmt) = &body.1 {
            self.statment(stmt)?;
        }

        Ok(())
    }

    fn while_statement(&mut self, expr: &Expr, body: &Stmt) -> Result<(), Error> {
        self.expression(expr)?;
        self.statment(body)
    }

    fn loop_statement(&mut self, body: &Stmt) -> Result<(), Error> {
        self.statment(body)
    }

    fn import_statement(&mut self, import: &Expr) -> Result<(), Error> {
        // The following is mostly a hack to appease the name checker until I can make
        // a proper import resolver.
        match import {
            Expr::Identifier(id) => self.symbols.insert(Symbol(id.name.to_owned(), id.span())),
            Expr::MemberExpr(_obj, prop, _) => {
                if let Expr::Identifier(id) = &**prop {
                    self.symbols.insert(Symbol(id.name.to_owned(), id.span()))
                }
            }
            // TODO: replace with proper error message
            _ => panic!("invaild import")
        };

        Ok(())
    }

    fn class(&mut self, class: &Class) -> Result<(), Error> {
        if self.symbols.current_scope().has_symbol(&class.name.name) {
            let duplicate = self.symbols.current_scope().find(&class.name.name).unwrap();

            return Err(Error::DuplicateIdentifier(
                Item::new(&class.name.name, class.name.span()),
                Item::new(&duplicate.0.clone(), duplicate.1.clone()),
            ));
        } else {
            let symbol = Symbol(class.name.name.clone(), class.name.span());
            self.declare_symbol(symbol);
        }

        Ok(())
    }

    fn constructor(&mut self, _constructor: &super::ast::Constructor) -> Result<(), Error> {
        todo!()
    }

    fn fun(&mut self, fun: &ScriptFun) -> Result<(), Error> {
        if self.symbols.current_scope().has_symbol(&fun.name.name) {
            let duplicate = self.symbols.current_scope().find(&fun.name.name).unwrap();

            return Err(Error::DuplicateFun(
                Item::new(&fun.name.name, fun.name.span()),
                Item::new(&duplicate.0.clone(), duplicate.1.clone()),
            ));
        } else {
            let symbol = Symbol(fun.name.name.clone(), fun.name.span());
            self.declare_symbol(symbol);
        }

        self.symbols.enter_scope();

        for param in &fun.params {
            if self.symbols.current_scope().has_symbol(&param.name) {
                self.symbols.exit_scope();

                let original = self.symbols.current_scope().find(&param.name).unwrap();

                return Err(Error::DuplicateIdentifier(
                    Item::new(&original.0, original.1.clone()),
                    Item::new(&param.name, param.span()),
                ));
            } else {
                self.symbols
                    .insert(Symbol(param.name.clone(), param.span()));
            }
        }

        if let Stmt::Block(stmts, _) = &fun.body {
            for stmt in (*stmts).iter() {
                if let Err(err) = self.statment(stmt) {
                    self.symbols.exit_scope();
                    return Err(err);
                }
            }
        }

        self.symbols.exit_scope();

        Ok(())
    }

    fn var_decl(&mut self, ident: &Ident, init: &Option<Expr>) -> Result<(), Error> {
        // check for duplicate variable.
        if self.symbols.current_scope().has_symbol(&ident.name) {
            let original = self.symbols.current_scope().find(&ident.name).unwrap();

            return Err(Error::DuplicateIdentifier(
                Item::new(&original.0, original.1.clone()),
                Item::new(&ident.name, ident.span()),
            ));
        }
        // evaluate init.
        if let Some(expr) = init {
            self.expression(expr)?;
        }
        // insert new symbol.
        let symbol = Symbol(ident.name.clone(), ident.span());
        self.declare_symbol(symbol);

        Ok(())
    }

    fn con_decl(&mut self, ident: &Ident, init: &Expr) -> Result<(), Error> {
        if self.symbols.current_scope().has_symbol(&ident.name) {
            let original = self.symbols.current_scope().find(&ident.name).unwrap();

            return Err(Error::DuplicateIdentifier(
                Item::new(&original.0, original.1.clone()),
                Item::new(&ident.name, ident.span()),
            ));
        }

        self.expression(init)?;

        let symbol = Symbol(ident.name.clone(), ident.span());
        self.declare_symbol(symbol);

        Ok(())
    }

    fn assign_stmt(&mut self, _ident: &Expr, expr: &Expr) -> Result<(), Error> {
        /*if self.symbols.find(&ident.name).is_none() {
            return Err(Error::UnresolvedIdentifier(Item::new(
                &ident.name,
                ident.span(),
            )));
        }*/

        self.expression(expr)
    }

    fn return_stmt(&mut self, expr: &Option<Expr>) -> Result<(), Error> {
        match expr {
            Some(expr) => self.expression(expr),
            None => Ok(())
        }
    }

    fn break_stmt(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn continue_stmt(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn type_spec(&mut self, _typ: &TypePath) -> Result<(), Error> {
        Ok(())
    }

    fn and(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expression(lhs)?;
        self.expression(rhs)
    }

    fn or(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expression(lhs)?;
        self.expression(rhs)
    }

    fn binary_expr(&mut self, bin_expr: &BinExpr) -> Result<(), Error> {
        self.expression(&bin_expr.rhs)?;
        self.expression(&bin_expr.lhs)?;

        Ok(())
    }

    fn unary_expr(&mut self, _op: &Op, expr: &Expr) -> Result<(), Error> {
        self.expression(expr)
    }

    fn index(&mut self, expr: &Expr, index: &Expr) -> Result<(), Error> {
        self.expression(expr)?;
        self.expression(index)
    }

    fn list(&mut self, list: Vec<Expr>) -> Result<(), Error> {
        for item in list {
            self.expression(&item)?;
        }

        Ok(())
    }

    fn tuple(&mut self, tuple: &[Expr]) -> Result<(), Error> {
        for item in tuple {
            self.expression(item)?;
        }
        Ok(())
    }

    fn map(&mut self, _map: &[(Expr, Expr)]) -> Result<(), Error> {
        Ok(())
    }

    fn fun_call(&mut self, callee: &Expr, args: &[Expr]) -> Result<(), Error> {
        self.expression(callee)?;
        for arg in args {
            self.expression(arg)?;
        }

        Ok(())
    }

    fn member_expr(&mut self, _obj: &Expr, _prop: &Expr) -> Result<(), Error> {
        // TODO: resolve names in core library
        Ok(())
    }

    fn assoc_expr(&mut self, obj: &Expr, _prop: &Expr) -> Result<(), Error> {
        self.expression(obj)
    }

    fn self_expr(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn identifier(&mut self, ident: &Ident) -> Result<(), Error> {
        if self.symbols.find(&ident.name).is_none() {
            self.unresolved_symbols
                .push(Symbol(ident.name.clone(), ident.span()));
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
