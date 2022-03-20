//! Typechecker for the Kaon language.

use crate::{
    common::Span,
    compiler::{
        ASTNode, BinExpr, Class, Constructor, Expr, Ident, Op, ScriptFun, Stmt, TypePath, AST,
    },
    error::{Error, Item},
};
use std::{collections::HashMap, fmt, fmt::Debug, fmt::Display};

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
    Class,
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
            Type::List(arg) => f.write_fmt(format_args!("List[{arg}]")),
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
            Type::Class => f.write_str("class"),
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
    pub fn new<T: Into<String>>(name: T) -> Self {
        Self { name: name.into() }
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", &self.name))
    }
}

#[derive(Default, Debug)]
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
    pub frames: Vec<Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut typ_env = TypeEnv::new();

        typ_env.insert(Symbol::new("f64"), Type::Float);
        typ_env.insert(Symbol::new("i64"), Type::Int);
        typ_env.insert(Symbol::new("string"), Type::String);
        typ_env.insert(Symbol::new("bool"), Type::Bool);
        typ_env.insert(Symbol::new("void"), Type::Void);
        typ_env.insert(Symbol::new("any"), Type::Any);
        typ_env.insert(Symbol::new("List"), Type::Any);
        typ_env.insert(Symbol::new("Range"), Type::Any);

        let env = vec![typ_env];

        TypeChecker {
            env,
            errors: Vec::new(),
            frames: Vec::new(),
        }
    }

    pub fn check_ast(&mut self, ast: &AST) {
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
            Stmt::Class(class, _) => self.class(class),
            Stmt::Constructor(constructor, _) => self.constructor(constructor),
            Stmt::Return(expr, span) => self.return_stmt(expr, span),
            Stmt::Break(_) => self.break_stmt(),
            Stmt::Continue(_) => self.continue_stmt(),
            Stmt::Expr(expr) => self.check_expr(expr),
        }
    }

    fn if_statement(&mut self, expr: &Expr, body: &(Stmt, Option<Stmt>)) -> Result<Type, Error> {
        self.check_expr(expr)?;
        let typ = self.check_stmt(&body.0)?;
        let return_typ = if let Some(stmt) = body.1.as_ref() {
            self.check_stmt(stmt)?
        } else {
            return Ok(typ);
        };

        if return_typ != typ {
            Err(Error::MismatchType(
                Item::new(&typ.to_string(), expr.span()),
                Item::new(&return_typ.to_string(), body.1.as_ref().unwrap().span()),
            ))
        } else {
            Ok(typ)
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

    fn class(&mut self, class: &Class) -> Result<Type, Error> {
        self.current_env()
            .insert(Symbol::new(class.name()), Type::Class);

        Ok(Type::Any)
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
                    Item::new(&typ.to_string(), ident.span.clone()),
                    Item::new(&inferred_typ.to_string(), ident.span.clone()),
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

    fn assign_stmt(&mut self, ident: &Expr, expr: &Expr) -> Result<Type, Error> {
        let lhs = self.check_expr(ident)?;
        let rhs = self.check_expr(expr)?;

        let typ = match (lhs, rhs) {
            (Type::Any, typ) => typ,
            (typ, Type::Any) => typ,
            (rhs_typ, lhs_typ) if rhs_typ == lhs_typ => lhs_typ,
            (Type::List(typ), lhs) if *typ == lhs => *typ,
            (typ, inferred_typ) => {
                return Err(Error::MismatchType(
                    Item::new(&typ.to_string(), ident.span()),
                    Item::new(&inferred_typ.to_string(), ident.span()),
                ))
            }
        };

        Ok(typ)
    }

    fn fun(&mut self, fun: &ScriptFun) -> Result<Type, Error> {
        self.enter_scope();

        let mut params_typs = Vec::with_capacity(fun.params.len());

        for (pos, param) in fun.params.iter().enumerate() {
            let typ = match &fun.params_typ[pos] {
                Some(expr) => self.check_expr(expr)?,
                None => Type::Any,
            };

            self.current_env()
                .insert(Symbol::new(param.name.to_owned()), typ.clone());
            params_typs.push(typ);
        }

        let return_typ = match &fun.return_typ {
            Some(typ) => self.check_expr(typ)?,
            None => Type::Void,
        };

        self.frames.push(return_typ.clone());

        self.check_stmt(&fun.body)?;

        self.frames.pop();
        self.exit_scope();

        let signature = Type::Fun(Box::new(params_typs), Box::new(return_typ));

        self.current_env()
            .insert(Symbol::new(fun.name.name.to_owned()), signature.clone());

        Ok(signature)
    }

    fn return_stmt(&mut self, expr: &Option<Expr>, span: &Span) -> Result<Type, Error> {
        let inferred_typ = match expr {
            Some(expr) => self.check_expr(expr)?,
            None => Type::Void,
        };

        let span = match expr {
            Some(expr) => expr.span(),
            None => span.clone(),
        };

        let actual_typ = &self.frames[self.frames.len() - 1];

        let typ = match (actual_typ, &inferred_typ) {
            (Type::Any, typ) => typ,
            (typ, Type::Any) => typ,
            (rhs_typ, lhs_typ) if rhs_typ == lhs_typ => rhs_typ,
            (typ, inferred_typ) => {
                return Err(Error::MismatchType(
                    Item::new(&typ.to_string(), span.clone()),
                    Item::new(&inferred_typ.to_string(), span),
                ))
            }
        };

        Ok(typ.clone())
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
            Expr::ParenExpr(expr, _) => self.check_expr(&*expr),
            Expr::Index(expr, index, _) => self.index(expr, index),
            Expr::List(list, _) => self.list((list).to_vec()),
            Expr::Tuple(tuple, _) => self.tuple(tuple),
            Expr::Map(map, _) => self.map(map),
            Expr::Or(lhs, rhs, _) => self.or(lhs, rhs),
            Expr::And(lhs, rhs, _) => self.and(lhs, rhs),
            Expr::FunCall(callee, args, _) => self.fun_call(callee, args),
            Expr::MemberExpr(obj, prop, _) => self.member_expr(obj, prop),
            Expr::AssocExpr(obj, prop, _) => self.assoc_expr(obj, prop),
            Expr::Type(typ_name, _) => self.type_spec(typ_name),
        }
    }

    fn type_spec(&mut self, type_name: &TypePath) -> Result<Type, Error> {
        let name = type_name.ident.clone();

        let typ = if let Some(typ) = self.lookup_symbol(&name.name) {
            typ.1.clone()
        } else {
            return Err(Error::UnknownType(Item::new(
                &name.name.clone(),
                name.span(),
            )));
        };

        if let Some(arg) = &type_name.arguments {
            let arg_typ = self.type_spec(&**arg)?;
            Ok(Type::List(Box::new(arg_typ)))
        } else {
            Ok(typ)
        }
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
        match (&lhs_typ, &rhs_typ) {
            (lhs, rhs) if lhs == rhs => {}
            (Type::Any, _) | (_, Type::Any) => {}
            _ => {
                return Err(Error::MismatchBinOp(
                    Item::new("+", bin_expr.rhs.span()),
                    Item::new(&lhs_typ.to_string()[..], bin_expr.lhs.span()),
                    Item::new(&rhs_typ.to_string()[..], bin_expr.rhs.span()),
                ))
            }
        }

        match bin_expr.op {
            Op::Add
            | Op::Subtract
            | Op::Multiply
            | Op::Divide
            | Op::Remainder
            | Op::BitwiseAnd
            | Op::BitwiseOr
            | Op::BitwiseXor => Ok(lhs_typ),
            Op::GreaterThan
            | Op::GreaterThanEquals
            | Op::LessThan
            | Op::LessThanEquals
            | Op::EqualTo
            | Op::NotEqual
            | Op::Bang => Ok(Type::Bool),
        }
    }

    fn unary_expr(&mut self, _op: &Op, expr: &Expr) -> Result<Type, Error> {
        self.check_expr(expr)
    }

    fn index(&mut self, expr: &Expr, index: &Expr) -> Result<Type, Error> {
        let typ = self.check_expr(expr)?;
        let _ = self.check_expr(index)?;

        if let Type::List(typ) = typ {
            Ok(*typ)
        } else {
            Ok(typ)
        }
    }

    fn list(&mut self, list: Vec<Expr>) -> Result<Type, Error> {
        if list.is_empty() {
            return Ok(Type::Any);
        }

        let arg_typ = self.check_expr(&list[0])?;

        for item in &list[1..] {
            let typ = self.check_expr(item)?;
            if typ != arg_typ {
                return Err(Error::MismatchType(
                    Item::new(&arg_typ.to_string(), list[0].span()),
                    Item::new(&typ.to_string(), item.span()),
                ));
            }
        }

        Ok(Type::List(Box::new(arg_typ)))
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

            for (pos, arg) in args.iter().enumerate() {
                let arg_typ = &self.check_expr(arg)?;
                let param_typ = &params[pos];
                match (arg_typ, param_typ) {
                    (arg_typ, param_typ) if arg_typ == param_typ => {}
                    (Type::Any, _typ) | (_typ, Type::Any) => {}
                    _ => {
                        return Err(Error::MismatchType(
                            Item::new(&param_typ.to_string(), arg.span()),
                            Item::new(&arg_typ.to_string(), arg.span()),
                        ))
                    }
                }
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
        Ok(Type::Any)
    }

    fn assoc_expr(&mut self, obj: &Expr, _prop: &Expr) -> Result<Type, Error> {
        match self.check_expr(obj)? {
            Type::Class => {}
            _ => panic!("expected a class"),
        }

        Ok(Type::Any)
    }

    fn self_expr(&mut self) -> Result<Type, Error> {
        Ok(Type::Any)
    }

    fn identifier(&mut self, ident: &Ident) -> Result<Type, Error> {
        match self.lookup_symbol(&ident.name[..]) {
            Some((_, typ)) => Ok(typ.clone()),
            None => Ok(Type::Any), /*Err(Error::NotInScope(Item {
                                       content: ident.name.clone(),
                                       span: ident.span.clone(),
                                   })),*/
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
        let len = self.env.len();
        &mut self.env[len - 1]
    }
}
