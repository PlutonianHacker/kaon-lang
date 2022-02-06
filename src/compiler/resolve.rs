use crate::{
    common::Span,
    compiler::{ASTNode, BinExpr, Expr, Ident, Op, Pass, ScriptFun, Stmt, AST},
    error::{Error, Item},
};

#[derive(Clone, Debug)]
pub struct Scope {
    symbols: Vec<Symbol>,
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
        match self.find(name) {
            Some(_) => true,
            None => false,
        }
    }

    pub fn find(&mut self, name: &str) -> Option<&Symbol> {
        self.symbols.iter().filter(|sym| sym.0 == name).last()
    }
}

#[derive(Hash, Clone, Debug)]
pub struct Symbol(pub String, pub Span);

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
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

pub struct Resolver {
    symbols: ScopedMap,
    unresolved_symbols: Vec<Symbol>,
    pub errors: Vec<Error>,
}

impl Default for Resolver {
    fn default() -> Self {
        Resolver {
            symbols: ScopedMap::default(),
            unresolved_symbols: Vec::default(),
            errors: Vec::default(),
        }
    }
}

impl Resolver {
    pub fn resolve_ast(&mut self, ast: &AST) {
        for node in &ast.nodes {
            let result = match node {
                ASTNode::Stmt(stmt) => self.statment(&stmt),
                ASTNode::Expr(expr) => self.expression(&expr),
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
    fn block(&mut self, stmts: &Vec<Stmt>) -> Result<(), Error> {
        self.symbols.enter_scope();
        for node in stmts {
            self.statment(&node)?;
        }

        self.symbols.exit_scope();
        Ok(())
    }

    fn if_statement(&mut self, expr: &Expr, body: &(Stmt, Option<Stmt>)) -> Result<(), Error> {
        self.expression(&expr)?;
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

    fn fun(&mut self, fun: &ScriptFun) -> Result<(), Error> {
        if self.symbols.current_scope().has_symbol(&fun.name.name) {
            let duplicate = self.symbols.current_scope().find(&fun.name.name).unwrap();

            return Err(Error::DuplicateFun(
                Item::new(&fun.name.name, fun.name.span()),
                Item::new(&duplicate.0.clone(), duplicate.1.clone()),
            ));
        } else {
            let symbol = Symbol(fun.name.name.clone(), fun.name.span());
            self.declare_symbol(symbol); //symbols.insert(symbol);
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
            self.expression(&expr)?;
        }
        // insert new symbol.
        let symbol = Symbol(ident.name.clone(), ident.span());
        self.declare_symbol(symbol); //symbols.insert(symbol);

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

        self.expression(&init)?;

        let symbol = Symbol(ident.name.clone(), ident.span());
        self.declare_symbol(symbol); //symbols.insert(symbol);

        Ok(())
    }

    fn assign_stmt(&mut self, ident: &Ident, expr: &Expr) -> Result<(), Error> {
        if let None = self.symbols.find(&ident.name) {
            return Err(Error::UnresolvedIdentifier(Item::new(
                &ident.name,
                ident.span(),
            )));
        }

        self.expression(&expr)
    }

    fn return_stmt(&mut self, expr: &Expr) -> Result<(), Error> {
        self.expression(expr)
    }

    fn break_stmt(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn continue_stmt(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn type_spec(&mut self, _typ: &Ident) -> Result<(), Error> {
        Ok(())
    }

    fn and(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expression(&lhs)?;
        self.expression(&rhs)
    }

    fn or(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), Error> {
        self.expression(&lhs)?;
        self.expression(&rhs)
    }

    fn binary_expr(&mut self, bin_expr: &BinExpr) -> Result<(), Error> {
        self.expression(&bin_expr.rhs)?;
        self.expression(&bin_expr.lhs)?;

        Ok(())
    }

    fn unary_expr(&mut self, _op: &Op, expr: &Expr) -> Result<(), Error> {
        self.expression(&expr)
    }

    fn index(&mut self, expr: &Expr, index: &Expr) -> Result<(), Error> {
        self.expression(&expr)?;
        self.expression(&index)
    }

    fn list(&mut self, list: Vec<Expr>) -> Result<(), Error> {
        for item in list {
            self.expression(&item)?;
        }

        Ok(())
    }

    fn tuple(&mut self, tuple: &Vec<Expr>) -> Result<(), Error> {
        for item in tuple {
            self.expression(&item)?;
        }
        Ok(())
    }

    fn fun_call(&mut self, callee: &Expr, args: &Vec<Expr>) -> Result<(), Error> {
        self.expression(&callee)?;
        for arg in args {
            self.expression(&arg)?;
        }

        Ok(())
    }

    fn member_expr(&mut self, _obj: &Expr, _prop: &Expr) -> Result<(), Error> {
        // TODO: resolve names in core library
        Ok(())
    }

    fn identifier(&mut self, ident: &Ident) -> Result<(), Error> {
        if let None = self.symbols.find(&ident.name) {
            self.unresolved_symbols
                .push(Symbol(ident.name.clone(), ident.span()));
        }

        Ok(())
    }

    fn number(&mut self, _val: &f64) -> Result<(), Error> {
        Ok(())
    }

    fn string(&mut self, _val: &String) -> Result<(), Error> {
        Ok(())
    }

    fn boolean(&mut self, _val: &bool) -> Result<(), Error> {
        Ok(())
    }

    fn nil(&mut self) -> Result<(), Error> {
        Ok(())
    }
}
