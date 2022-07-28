//! Typechecker for the Kaon language.

use crate::{
    common::Span,
    compiler::{
        ASTNode, BinExpr, Class, Constructor, Expr, Ident, Op, Fun, Stmt, TypePath, AST,
    },
    error::{Error, Item},
};
use std::{collections::HashMap, fmt, fmt::Debug, fmt::Display};

use super::{ast::Trait, typ::Typ};

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
/*
impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => f.write_str("int"),
            Type::Float => f.write_str("float"),
            Type::String => f.write_str("string"),
            Type::Bool => f.write_str("bool"),
            Typ::Void => f.write_str("()"),
            Typ::Any => f.write_str("any"),
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
}*/

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
    symbols: HashMap<Symbol, Typ>,
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            symbols: HashMap::new(),
        }
    }

    pub fn find(&mut self, name: &str) -> Option<(&Symbol, &Typ)> {
        self.symbols.iter().find(|symbol| symbol.0.name == *name)
    }

    pub fn insert(&mut self, symbol: Symbol, typ: Typ) {
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
    pub frames: Vec<Typ>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut typ_env = TypeEnv::new();

        typ_env.insert(Symbol::new("Float"), Typ::class("Float"));
        typ_env.insert(Symbol::new("Int"), Typ::class("Int"));
        typ_env.insert(Symbol::new("String"), Typ::class("String"));
        typ_env.insert(Symbol::new("Bool"), Typ::class("Bool"));
        typ_env.insert(Symbol::new("void"), Typ::class("void"));
        typ_env.insert(Symbol::new("any"), Typ::class("Any"));
        typ_env.insert(Symbol::new("List"), Typ::class("List"));

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

    pub fn check_stmt(&mut self, stmt: &Stmt) -> Result<Typ, Error> {
        match stmt {
            Stmt::IfStatement(expr, body, _) => self.if_statement(expr, body),
            Stmt::WhileStatement(expr, body, _) => self.while_statement(expr, body),
            Stmt::LoopStatement(body, _) => self.loop_statement(body),
            Stmt::ImportStatement(import, _) => self.import_statement(import),
            Stmt::Block(stmts, _) => self.block(stmts),
            Stmt::VarDeclaration(ident, expr, typ, _) => self.var_decl(ident, expr, typ),
            Stmt::ConDeclaration(ident, expr, typ, _) => self.con_decl(ident, expr, typ),
            Stmt::AssignStatement(ident, expr, _) => self.assign_stmt(ident, expr),
            Stmt::Function(fun, _) => self.fun(fun),
            Stmt::Class(class, _) => self.class(class),
            Stmt::Trait(t) => self.trait_decl(t),
            //Stmt::Constructor(constructor, _) => self.constructor(constructor),
            Stmt::Return(expr, span) => self.return_stmt(expr, span),
            Stmt::Break(_) => self.break_stmt(),
            Stmt::Continue(_) => self.continue_stmt(),
            Stmt::Expr(expr) => self.check_expr(expr),
        }
    }

    fn if_statement(&mut self, expr: &Expr, body: &(Stmt, Option<Stmt>)) -> Result<Typ, Error> {
        self.check_expr(expr)?;
        let typ = self.check_stmt(&body.0)?;
        let return_typ = if let Some(stmt) = body.1.as_ref() {
            self.check_stmt(stmt)?
        } else {
            return Ok(typ);
        };

        if return_typ != typ {
            Err(Error::MismatchType(
                Item::new(&typ.name(), expr.span()),
                Item::new(&return_typ.name(), body.1.as_ref().unwrap().span()),
            ))
        } else {
            Ok(typ)
        }
    }

    fn while_statement(&mut self, expr: &Expr, body: &Stmt) -> Result<Typ, Error> {
        self.check_expr(expr)?;
        self.check_stmt(body)
    }

    fn loop_statement(&mut self, body: &Stmt) -> Result<Typ, Error> {
        self.check_stmt(body)
    }

    fn import_statement(&mut self, _import: &Expr) -> Result<Typ, Error> {
        unimplemented!()
    }

    fn block(&mut self, stmts: &[Stmt]) -> Result<Typ, Error> {
        let mut return_typ = Typ::Void;
        for stmt in stmts {
            return_typ = self.check_stmt(stmt)?;
        }

        Ok(return_typ)
    }

    fn class(&mut self, class: &Class) -> Result<Typ, Error> {
        /*self.current_env()
            .insert(Symbol::new(class.name()), Type::Class);
        */
        Ok(Typ::Any)
    }

    fn constructor(&mut self, _constructor: &Constructor) -> Result<Typ, Error> {
        todo!()
    }

    fn trait_decl(&mut self, _t: &Trait) -> Result<Typ, Error> {
        todo!()
    }

    fn var_decl(
        &mut self,
        ident: &Ident,
        init: &Option<Expr>,
        typ: &Option<Expr>,
    ) -> Result<Typ, Error> {
        let typ = match typ {
            Some(typ_name) => self.check_expr(typ_name)?,
            None => Typ::Any,
        };

        let inferred_typ = match init {
            Some(expr) => self.check_expr(expr)?,
            None => Typ::Any,
        };

        let typ = match (typ, inferred_typ) {
            (Typ::Any, typ) => typ,
            (typ, Typ::Any) => typ,
            (rhs_typ, lhs_typ) if rhs_typ == lhs_typ => rhs_typ,
            (typ, inferred_typ) => {
                return Err(Error::MismatchType(
                    Item::new(&typ.name(), ident.span()),
                    Item::new(&inferred_typ.name(), ident.span()),
                ))
            }
        };

        //self.current_env()
        //    .insert(Symbol::new(ident.id.to_owned()), typ.clone());

        Ok(typ)
    }

    fn con_decl(
        &mut self,
        _ident: &Ident,
        expr: &Expr,
        _typ: &Option<Expr>,
    ) -> Result<Typ, Error> {
        self.check_expr(expr)
    }

    fn assign_stmt(&mut self, ident: &Expr, expr: &Expr) -> Result<Typ, Error> {
        let lhs = self.check_expr(ident)?;
        let rhs = self.check_expr(expr)?;

        let typ = match (lhs, rhs) {
            (Typ::Any, typ) => typ,
            (typ, Typ::Any) => typ,
            (rhs_typ, lhs_typ) if rhs_typ == lhs_typ => lhs_typ,
            //(Type::List(typ), lhs) if *typ == lhs => *typ,
            (typ, inferred_typ) => {
                return Err(Error::MismatchType(
                    Item::new(&typ.name(), ident.span()),
                    Item::new(&inferred_typ.name(), ident.span()),
                ))
            }
        };

        Ok(typ)
    }

    fn fun(&mut self, fun: &Fun) -> Result<Typ, Error> {
        self.enter_scope();

        //let mut params_typs = Vec::with_capacity(fun.params.len());

        for (pos, param) in fun.params.iter().enumerate() {
            let typ = match &fun.params_typ[pos] {
                Some(expr) => self.check_expr(expr)?,
                None => Typ::Any,
            };

            /*self.current_env()
                .insert(Symbol::new(param.id.to_owned()), typ.clone());
            params_typs.push(typ);*/
        }

        let return_typ = match &fun.return_typ {
            Some(typ) => self.check_expr(typ)?,
            None => Typ::Void,
        };

        self.frames.push(return_typ.clone());

        self.check_stmt(&fun.body)?;

        self.frames.pop();
        self.exit_scope();

        let signature = Typ::Any;//Typ::Fun(Box::new(params_typs), Box::new(return_typ));

        //self.current_env().insert(Symbol::new(fun.name.id.to_owned()), signature.clone());

        Ok(signature)
    }

    fn return_stmt(&mut self, expr: &Option<Expr>, span: &Span) -> Result<Typ, Error> {
        let inferred_typ = match expr {
            Some(expr) => self.check_expr(expr)?,
            None => Typ::Void,
        };

        let span = match expr {
            Some(expr) => expr.span(),
            None => span.clone(),
        };

        let actual_typ = &self.frames[self.frames.len() - 1];

        let typ = match (actual_typ, &inferred_typ) {
            (Typ::Any, typ) => typ,
            (typ, Typ::Any) => typ,
            (rhs_typ, lhs_typ) if rhs_typ == lhs_typ => rhs_typ,
            (typ, inferred_typ) => {
                return Err(Error::MismatchType(
                    Item::new(&typ.name(), span.clone()),
                    Item::new(&inferred_typ.name(), span),
                ))
            }
        };

        Ok(typ.clone())
    }

    fn break_stmt(&mut self) -> Result<Typ, Error> {
        Ok(Typ::Void)
    }

    fn continue_stmt(&mut self) -> Result<Typ, Error> {
        Ok(Typ::Void)
    }

    pub fn check_expr(&mut self, expr: &Expr) -> Result<Typ, Error> {
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

    fn type_spec(&mut self, type_name: &TypePath) -> Result<Typ, Error> {
        let name = type_name.ident.clone();

        /*let typ = if let Some(typ) = self.lookup_symbol(&name.id) {
            typ.1.clone()
        } else {
            /*return Err(Error::UnknownType(Item::new(
                &name.,
                name.span(),
            )));*/
            todo!()
        };*/

        let typ = Typ::Any;

        if let Some(arg) = &type_name.arguments {
            let arg_typ = self.type_spec(&**arg)?;
            Ok(Typ::Any)
        } else {
            Ok(typ)
        }
    }

    fn and(&mut self, lhs: &Expr, rhs: &Expr) -> Result<Typ, Error> {
        self.check_expr(lhs)?;
        self.check_expr(rhs)
    }

    fn or(&mut self, lhs: &Expr, rhs: &Expr) -> Result<Typ, Error> {
        self.check_expr(lhs)?;
        self.check_expr(rhs)
    }

    fn binary_expr(&mut self, bin_expr: &BinExpr) -> Result<Typ, Error> {
        let lhs_typ = self.check_expr(&bin_expr.lhs)?;
        let rhs_typ = self.check_expr(&bin_expr.rhs)?;
        match (&lhs_typ, &rhs_typ) {
            (lhs, rhs) if lhs == rhs => {}
            (Typ::Any, _) | (_, Typ::Any) => {}
            _ => {
                return Err(Error::MismatchBinOp(
                    Item::new("+", bin_expr.rhs.span()),
                    Item::new(&lhs_typ.name()[..], bin_expr.lhs.span()),
                    Item::new(&rhs_typ.name()[..], bin_expr.rhs.span()),
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
            | Op::Bang => Ok(Typ::class("Bool")),
        }
    }

    fn unary_expr(&mut self, _op: &Op, expr: &Expr) -> Result<Typ, Error> {
        self.check_expr(expr)
    }

    fn index(&mut self, expr: &Expr, index: &Expr) -> Result<Typ, Error> {
        let typ = self.check_expr(expr)?;
        let _ = self.check_expr(index)?;

        /*if let Typ::Class { name } = typ {
            Ok(*typ)
        } else {
            Ok(typ)
        }*/

        Ok(Typ::Any)
    }

    fn list(&mut self, list: Vec<Expr>) -> Result<Typ, Error> {
        if list.is_empty() {
            return Ok(Typ::Any);
        }

        let arg_typ = self.check_expr(&list[0])?;

        for item in &list[1..] {
            let typ = self.check_expr(item)?;
            if typ != arg_typ {
                return Err(Error::MismatchType(
                    Item::new(&arg_typ.name(), list[0].span()),
                    Item::new(&typ.name(), item.span()),
                ));
            }
        }

        Ok(Typ::Any)
    }

    fn tuple(&mut self, tuple: &[Expr]) -> Result<Typ, Error> {
        let mut tuple_typ = Vec::new();
        for item in tuple {
            tuple_typ.push(self.check_expr(item)?);
        }
        todo!()

        //Ok(Type::Tuple(Box::new(tuple_typ)))
    }

    fn map(&mut self, _map: &[(Expr, Expr)]) -> Result<Typ, Error> {
        Ok(Typ::Any)
    }

    fn fun_call(&mut self, callee: &Expr, args: &[Expr]) -> Result<Typ, Error> {
        let typ = self.check_expr(callee)?;

        if let Typ::Fun { params, return_typ } = typ {
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
                let param_typ = &*params[pos];
                match (arg_typ, param_typ) {
                    (arg_typ, param_typ) if arg_typ == param_typ => {}
                    (Typ::Any, _typ) | (_typ, Typ::Any) => {}
                    _ => {
                        return Err(Error::MismatchType(
                            Item::new(&param_typ.name(), arg.span()),
                            Item::new(&arg_typ.name(), arg.span()),
                        ))
                    }
                }
            }

            Ok(*return_typ)
        } else if let Typ::Any = typ {
            Ok(Typ::Any)
        } else {
            Err(Error::ExpectedFunction(Item::new(
                &typ.name(),
                callee.span(),
            )))
        }
    }

    fn member_expr(&mut self, _obj: &Expr, _prop: &Expr) -> Result<Typ, Error> {
        Ok(Typ::Any)
    }

    fn assoc_expr(&mut self, obj: &Expr, _prop: &Expr) -> Result<Typ, Error> {
        /*match self.check_expr(obj)? {
            Type::Class => {}
            _ => panic!("expected a class"),
        }

        Ok(Typ::Any)*/
        todo!()
    }

    fn self_expr(&mut self) -> Result<Typ, Error> {
        Ok(Typ::Any)
    }

    fn identifier(&mut self, ident: &Ident) -> Result<Typ, Error> {
        /*match self.lookup_symbol(&ident.id[..]) {
            Some((_, typ)) => Ok(typ.clone()),
            None => Ok(Typ::Any), /*Err(Error::NotInScope(Item {
                                       content: ident.name.clone(),
                                       span: ident.span.clone(),
                                   })),*/
        }*/
        Ok(Typ::Any)
    }

    fn number(&mut self, _val: &f64) -> Result<Typ, Error> {
        Ok(Typ::class("Float"))
    }
    fn string(&mut self, _val: &str) -> Result<Typ, Error> {
        Ok(Typ::class("String"))
    }
    fn boolean(&mut self, _val: &bool) -> Result<Typ, Error> {
        Ok(Typ::class("Bool"))
    }
    fn nil(&mut self) -> Result<Typ, Error> {
        Ok(Typ::Any)
    }

    fn enter_scope(&mut self) {
        self.env.push(TypeEnv::new());
    }

    fn exit_scope(&mut self) {
        self.env.pop();
    }

    fn lookup_symbol(&mut self, name: &str) -> Option<(&Symbol, &Typ)> {
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
