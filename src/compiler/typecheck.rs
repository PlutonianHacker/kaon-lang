use crate::{
    common::Span,
    compiler::{ASTNode, BinExpr, Constructor, Expr, Ident, Op, ScriptFun, Stmt, AST},
    error::{Error, Item},
};
use std::{collections::HashMap, fmt, fmt::Display};

/// enum containing all possible data types for the Kaon langauge
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// integer
    Int,
    /// f64
    Float,
    /// string
    String,
    /// `true` | `false`
    Bool,
    /// ()
    Void,
    /// any type
    Any,
    /// Vec of matching types
    List(Box<Type>),
    /// Vec of different types
    Tuple(Box<Vec<Type>>),
    /// Vec of key, value pairs
    Map(Box<Vec<(Type, Type)>>),
    /// Array of argument types and the function's return type
    Fun(Box<Vec<Type>>, Box<Type>),
    /// A type alias
    Alias(String, Box<Type>),
    /// A type error
    ///
    /// note: for internal use only
    Error,
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => f.write_str("int"),
            Type::Float => f.write_str("float"),
            Type::String => f.write_str("string"),
            Type::Bool => f.write_str("bool"),
            Type::Void => f.write_str("()"),
            Type::Any => f.write_str("any"),
            Type::List(list) => f.write_fmt(format_args!("List<{}>", list)),
            Type::Tuple(tuple) => {
                let fmt_items = tuple
                    .as_ref()
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ");
                f.write_fmt(format_args!("({})", fmt_items))
            }
            Type::Map(map) => {
                let fmt_items = map
                    .as_ref()
                    .iter()
                    .map(|arg| format!("{}: {}", arg.0, arg.1))
                    .collect::<Vec<String>>()
                    .join(", ");
                f.write_fmt(format_args!("{{{}}}", fmt_items))
            }
            Type::Fun(args, return_typ) => {
                let fmt_args = args
                    .as_ref()
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ");
                f.write_fmt(format_args!("Fun({}): {}", fmt_args, return_typ))
            }
            Type::Alias(alias, typ) => f.write_fmt(format_args!("alias {} = {}", alias, typ)),
            Type::Error => f.write_str("[type error]"),
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
/// A unique symbol
pub struct Symbol {
    name: String,
}

impl Symbol {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

#[derive(Default)]
pub struct TypeEnv {
    symbols: HashMap<Symbol, Type>,
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            symbols: HashMap::new(),
        }
    }

    pub fn find(&mut self, name: &str) -> Option<(&Symbol, &Type)> {
        self.symbols.iter().find(|symbol| symbol.0.name == *name)
    }

    pub fn insert(&mut self, symbol: Symbol, typ: Type) {
        self.symbols.insert(symbol, typ);
    }
}

/// Typechecker for the kaon langauge.
///
/// Recursively walks an [AST], generating a typed symbol table for it.
#[derive(Default)]
pub struct TypeChecker {
    pub env: Vec<TypeEnv>,
    pub errors: Vec<Error>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            env: vec![TypeEnv::new()],
            errors: Vec::new(),
        }
    }

    pub fn check_ast(&mut self, ast: &AST) {
        //self.current_env().insert(Symbol::new("io".to_string()), typ)

        for node in &ast.nodes {
            let result = match node {
                ASTNode::Stmt(stmt) => self.check_stmt(stmt),
                ASTNode::Expr(expr) => self.check_expr(expr),
            };
            if let Err(err) = result {
                self.errors.push(err);
            }
        }
    }

    pub fn check_stmt(&mut self, stmt: &Stmt) -> Result<Type, Error> {
        match stmt {
            Stmt::IfStatement(expr, body, _) => self.if_statement(expr, body),
            Stmt::WhileStatement(expr, body, _) => self.while_statement(expr, body),
            Stmt::LoopStatement(body, _) => self.loop_statement(body),
            Stmt::ImportStatement(import, _) => self.import_statement(import),
            Stmt::Block(stmts, _) => self.block(stmts),
            Stmt::VarDeclaration(ident, expr, typ, _) => self.var_decl(ident, expr, typ),
            Stmt::ConDeclaration(ident, expr, typ, _) => self.con_decl(ident, expr, typ),
            Stmt::AssignStatement(ident, expr, _) => self.assign_stmt(ident, expr),
            Stmt::ScriptFun(fun, _) => self.fun(fun),
            Stmt::Class(_class, _) => todo!(),
            Stmt::Constructor(constructor, _) => self.constructor(constructor),
            Stmt::Return(expr, _) => self.return_stmt(expr),
            Stmt::Break(_) => self.break_stmt(),
            Stmt::Continue(_) => self.continue_stmt(),
            Stmt::Expr(expr) => self.check_expr(expr),
        }
    }

    fn if_statement(&mut self, expr: &Expr, body: &(Stmt, Option<Stmt>)) -> Result<Type, Error> {
        self.check_expr(expr)?;
        let typ = self.check_stmt(&body.0);
        if let Some(stmt) = &body.1 {
            self.check_stmt(stmt)
        } else {
            typ
        }
    }

    fn while_statement(&mut self, expr: &Expr, body: &Stmt) -> Result<Type, Error> {
        self.check_expr(expr)?;
        self.check_stmt(body)
    }

    fn loop_statement(&mut self, body: &Stmt) -> Result<Type, Error> {
        self.check_stmt(body)
    }

    fn import_statement(&mut self, _import: &Expr) -> Result<Type, Error> {
        unimplemented!()
    }

    fn block(&mut self, stmts: &[Stmt]) -> Result<Type, Error> {
        let mut return_typ = Type::Void;
        for stmt in stmts {
            return_typ = self.check_stmt(stmt)?;
        }

        Ok(return_typ)
    }

    fn constructor(&mut self, _constructor: &Constructor) -> Result<Type, Error> {
        todo!()
    }

    fn var_decl(
        &mut self,
        ident: &Ident,
        init: &Option<Expr>,
        typ: &Option<Expr>,
    ) -> Result<Type, Error> {
        let typ = match typ {
            Some(typ_name) => self.check_expr(typ_name)?,
            None => Type::Any,
        };

        let inferred_typ = match init {
            Some(expr) => self.check_expr(expr)?,
            None => Type::Any,
        };

        let typ = match (typ, inferred_typ) {
            (Type::Any, typ) => typ,
            (typ, Type::Any) => typ,
            (rhs_typ, lhs_typ) if rhs_typ == lhs_typ => rhs_typ,
            (typ, inferred_typ) => {
                return Err(Error::MismatchType(
                    Item::new(&format!("{}", typ)[..], ident.span.clone()),
                    Item::new(&format!("{}", inferred_typ)[..], ident.span.clone()),
                ))
            }
        };

        self.current_env()
            .insert(Symbol::new(ident.name.to_owned()), typ.clone());

        Ok(typ)
    }

    fn con_decl(
        &mut self,
        _ident: &Ident,
        expr: &Expr,
        _typ: &Option<Expr>,
    ) -> Result<Type, Error> {
        self.check_expr(expr)
    }

    fn assign_stmt(&mut self, _ident: &Expr, expr: &Expr) -> Result<Type, Error> {
        self.check_expr(expr)?;
        self.check_expr(expr)
    }

    fn fun(&mut self, fun: &ScriptFun) -> Result<Type, Error> {
        let mut return_typ = Type::Any;

        self.enter_scope();

        let mut params_typs = Vec::with_capacity(fun.params.len());

        for param in &fun.params {
            self.current_env()
                .insert(Symbol::new(param.name.to_owned()), Type::Any);
            params_typs.push(Type::Any);
        }

        if let Stmt::Block(stmts, _) = &fun.body {
            for node in stmts.iter() {
                if let Stmt::Return(expr, _) = node {
                    return_typ = self.check_expr(expr)?;
                } else {
                    self.check_stmt(node)?;
                }
            }
        }

        self.exit_scope();

        let signature = Type::Fun(Box::new(params_typs), Box::new(return_typ));

        self.current_env()
            .insert(Symbol::new(fun.name.name.to_owned()), signature.clone());

        Ok(signature)
    }

    fn return_stmt(&mut self, expr: &Expr) -> Result<Type, Error> {
        self.check_expr(expr)
    }

    fn break_stmt(&mut self) -> Result<Type, Error> {
        Ok(Type::Void)
    }

    fn continue_stmt(&mut self) -> Result<Type, Error> {
        Ok(Type::Void)
    }

    pub fn check_expr(&mut self, expr: &Expr) -> Result<Type, Error> {
        match expr {
            Expr::Number(val, _) => self.number(val),
            Expr::String(val, _) => self.string(val),
            Expr::Boolean(val, _) => self.boolean(val),
            Expr::Unit(_) | Expr::Nil(_) => self.nil(),
            Expr::Identifier(ident) => self.identifier(ident),
            Expr::SelfExpr(_) => self.self_expr(),
            Expr::BinExpr(bin_expr, _) => self.binary_expr(bin_expr),
            Expr::UnaryExpr(op, unary_expr, _) => self.unary_expr(op, unary_expr),
            Expr::Index(expr, index, _) => self.index(expr, index),
            Expr::List(list, _) => self.list((list).to_vec()),
            Expr::Tuple(tuple, _) => self.tuple(tuple),
            Expr::Map(map, _) => self.map(map),
            Expr::Or(lhs, rhs, _) => self.or(lhs, rhs),
            Expr::And(lhs, rhs, _) => self.and(lhs, rhs),
            Expr::FunCall(callee, args, _) => self.fun_call(callee, args),
            Expr::MemberExpr(obj, prop, _) => self.member_expr(obj, prop),
            Expr::Type(typ_name) => self.type_spec(typ_name),
        }
    }

    fn type_spec(&mut self, type_name: &Ident) -> Result<Type, Error> {
        Ok(match &type_name.name[..] {
            "float" => Type::Float,
            "int" => Type::Int,
            "string" => Type::String,
            "bool" => Type::Bool,
            _ => {
                return Err(Error::UnknownType(Item::new(
                    &type_name.name.clone(),
                    type_name.span(),
                )))
            }
        })
    }

    fn and(&mut self, lhs: &Expr, rhs: &Expr) -> Result<Type, Error> {
        self.check_expr(lhs)?;
        self.check_expr(rhs)
    }

    fn or(&mut self, lhs: &Expr, rhs: &Expr) -> Result<Type, Error> {
        self.check_expr(lhs)?;
        self.check_expr(rhs)
    }

    fn binary_expr(&mut self, bin_expr: &BinExpr) -> Result<Type, Error> {
        let lhs_typ = self.check_expr(&bin_expr.lhs)?;
        let rhs_typ = self.check_expr(&bin_expr.rhs)?;
        if lhs_typ != rhs_typ {
            return Err(Error::MismatchBinOp(
                Item::new("+", bin_expr.rhs.span()),
                Item::new(&lhs_typ.to_string()[..], bin_expr.lhs.span()),
                Item::new(&rhs_typ.to_string()[..], bin_expr.rhs.span()),
            ));
        }

        Ok(lhs_typ)
    }

    fn unary_expr(&mut self, _op: &Op, expr: &Expr) -> Result<Type, Error> {
        self.check_expr(expr)
    }

    fn index(&mut self, expr: &Expr, index: &Expr) -> Result<Type, Error> {
        self.check_expr(expr)?;
        self.check_expr(index)
    }

    fn list(&mut self, list: Vec<Expr>) -> Result<Type, Error> {
        let typ = self.check_expr(&list[0]);

        for item in &list[1..] {
            self.check_expr(item)?;
        }

        typ
    }

    fn tuple(&mut self, tuple: &[Expr]) -> Result<Type, Error> {
        let mut tuple_typ = Vec::new();
        for item in tuple {
            tuple_typ.push(self.check_expr(item)?);
        }

        Ok(Type::Tuple(Box::new(tuple_typ)))
    }

    fn map(&mut self, _map: &[(Expr, Expr)]) -> Result<Type, Error> {
        Ok(Type::Any)
    }

    fn fun_call(&mut self, callee: &Expr, args: &[Expr]) -> Result<Type, Error> {
        let typ = self.check_expr(callee)?;

        for arg in args {
            self.check_expr(arg)?;
        }

        if let Type::Fun(params, res) = typ {
            if params.len() != args.len() {
                return Err(Error::MismatchArgCount(
                    Item::new(&params.len().to_string(), Span::empty()),
                    Item::new(&args.len().to_string(), callee.span()),
                    args.iter()
                        .map(|arg| Item::new("", arg.span()))
                        .collect::<Vec<Item>>(),
                ));
            }

            Ok(*res)
        } else if let Type::Any = typ {
            Ok(Type::Any)
        } else {
            Err(Error::ExpectedFunction(Item::new(
                &typ.to_string(),
                callee.span(),
            )))
        }
    }

    fn member_expr(&mut self, _obj: &Expr, _prop: &Expr) -> Result<Type, Error> {
        //self.check_expr(obj)?;
        //self.check_expr(prop)
        Ok(Type::Any)
    }

    fn self_expr(&mut self) -> Result<Type, Error> {
        Ok(Type::Any)
    }

    fn identifier(&mut self, ident: &Ident) -> Result<Type, Error> {
        match self.lookup_symbol(&ident.name[..]) {
            Some((_, typ)) => Ok(typ.clone()),
            None => Err(Error::NotInScope(Item {
                content: ident.name.clone(),
                span: ident.span.clone(),
            })),
        }
    }

    fn number(&mut self, _val: &f64) -> Result<Type, Error> {
        Ok(Type::Float)
    }
    fn string(&mut self, _val: &str) -> Result<Type, Error> {
        Ok(Type::String)
    }
    fn boolean(&mut self, _val: &bool) -> Result<Type, Error> {
        Ok(Type::Bool)
    }
    fn nil(&mut self) -> Result<Type, Error> {
        Ok(Type::Any)
    }

    fn enter_scope(&mut self) {
        self.env.push(TypeEnv::new());
    }

    fn exit_scope(&mut self) {
        self.env.pop();
    }

    fn lookup_symbol(&mut self, name: &str) -> Option<(&Symbol, &Type)> {
        for env in self.env.iter_mut().rev() {
            if let Some(sym) = env.find(name) {
                return Some(sym);
            }
        }

        None
    }

    fn current_env(&mut self) -> &mut TypeEnv {
        let len = *&self.env.len();
        &mut self.env[len - 1]
    }
}
