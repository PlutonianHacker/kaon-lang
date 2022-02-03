use crate::{
    compiler::{ASTNode, BinExpr, Expr, Ident, Op, ScriptFun, Stmt},
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
            Type::Fun(args, return_typ) => {
                let fmt_args = args
                    .as_ref()
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ");
                f.write_fmt(format_args!("Fun ({}) ~ {}", fmt_args, return_typ))
            }
            Type::Alias(alias, typ) => f.write_fmt(format_args!("alias {} = {}", alias, typ)),
            Type::Error => f.write_str("[type error]"),
        }
    }
}

/// A unique symbol
pub struct Symbol {
    name: String,
}

pub struct TypeEnv {
    symbols: HashMap<Symbol, Type>,
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            symbols: HashMap::new(),
        }
    }

    pub fn find(&self, name: &str) -> Option<(&Symbol, &Type)> {
        self.symbols
            .iter()
            .find(|symbol| symbol.0.name == name.to_string())
    }
}

/// Typechecker for the kaon langauge.
/// 
/// Recursively walks an [AST], generating a typed symbol table for it. 
pub struct TypeChecker {
    pub env: TypeEnv,
    pub error: Vec<Error>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            env: TypeEnv::new(),
            error: Vec::new(),
        }
    }

    pub fn check(&mut self, ast: &Vec<ASTNode>) {
        for node in ast {
            let result = match node {
                ASTNode::Stmt(stmt) => self.check_stmt(&stmt),
                ASTNode::Expr(expr) => self.check_expr(&expr),
            };
            if let Err(err) = result {
                self.error.push(err);
            }
        }
    }

    pub fn check_stmt(&self, stmt: &Stmt) -> Result<Type, Error> {
        match stmt {
            Stmt::IfStatement(expr, body, _) => self.if_statement(expr, body),
            Stmt::WhileStatement(expr, body, _) => self.while_statement(expr, body),
            Stmt::LoopStatement(body, _) => self.loop_statement(body),
            Stmt::Block(stmts, _) => self.block(&stmts),
            Stmt::VarDeclaration(ident, expr, typ, _) => self.var_decl(ident, expr, typ),
            Stmt::ConDeclaration(ident, expr, typ, _) => self.con_decl(ident, expr, typ),
            Stmt::AssignStatement(ident, expr, _) => self.assign_stmt(ident, expr),
            Stmt::ScriptFun(fun, _) => self.fun(fun),
            Stmt::Return(expr, _) => self.return_stmt(expr),
            Stmt::Break(_) => self.break_stmt(),
            Stmt::Continue(_) => self.continue_stmt(),
            Stmt::Expr(expr) => self.check_expr(expr),
        }
    }

    fn if_statement(&self, expr: &Expr, body: &(Stmt, Option<Stmt>)) -> Result<Type, Error> {
        self.check_expr(&expr)?;
        let typ = self.check_stmt(&body.0);
        if let Some(stmt) = &body.1 {
            self.check_stmt(&stmt)
        } else {
            typ
        }
    }

    fn while_statement(&self, expr: &Expr, body: &Stmt) -> Result<Type, Error> {
        self.check_expr(&expr)?;
        self.check_stmt(&body)
    }

    fn loop_statement(&self, body: &Stmt) -> Result<Type, Error> {
        self.check_stmt(&body)
    }

    fn block(&self, stmts: &Vec<Stmt>) -> Result<Type, Error> {
        let mut return_typ = Type::Void;
        for stmt in stmts {
            return_typ = self.check_stmt(stmt)?;
        }

        Ok(return_typ)
    }

    fn var_decl(
        &self,
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
                    Item::new(format!("{}", typ), ident.span.clone()),
                    Item::new(format!("{}", inferred_typ), ident.span.clone()),
                ))
            }
        };

        println!("{}", typ);

        Ok(Type::Any)
    }

    fn con_decl(&self, _ident: &Ident, expr: &Expr, _typ: &Option<Expr>) -> Result<Type, Error> {
        self.check_expr(&expr)
    }

    fn assign_stmt(&self, _ident: &Ident, expr: &Expr) -> Result<Type, Error> {
        self.check_expr(&expr)
    }

    fn fun(&self, fun: &ScriptFun) -> Result<Type, Error> {
        let mut return_typ = Type::Void;

        if let Stmt::Block(stmts, _) = &fun.body {
            for node in stmts.iter() {
                if let Stmt::Return(expr, _) = node {
                    return_typ = self.check_expr(&expr)?;
                } else {
                    self.check_stmt(&node)?;
                }
            }
        }

        Ok(return_typ)
    }

    fn return_stmt(&self, expr: &Expr) -> Result<Type, Error> {
        self.check_expr(&expr)
    }

    fn break_stmt(&self) -> Result<Type, Error> {
        Ok(Type::Void)
    }

    fn continue_stmt(&self) -> Result<Type, Error> {
        Ok(Type::Void)
    }

    pub fn check_expr(&self, expr: &Expr) -> Result<Type, Error> {
        match expr {
            Expr::Number(val, _) => self.number(&val),
            Expr::String(val, _) => self.string(&val),
            Expr::Boolean(val, _) => self.boolean(&val),
            Expr::Unit(_) => self.nil(),
            Expr::Identifier(ident) => self.identifier(&ident),
            Expr::BinExpr(bin_expr, _) => self.binary_expr(&bin_expr),
            Expr::UnaryExpr(op, unary_expr, _) => self.unary_expr(&op, &unary_expr),
            Expr::Index(expr, index, _) => self.index(&expr, &index),
            Expr::List(list, _) => self.list((&list).to_vec()),
            Expr::Or(lhs, rhs, _) => self.or(&lhs, &rhs),
            Expr::And(lhs, rhs, _) => self.and(&lhs, &rhs),
            Expr::FunCall(callee, args, _) => self.fun_call(&callee, &args),
            Expr::MemberExpr(obj, prop, _) => self.member_expr(&obj, &prop),
            Expr::Type(typ_name) => self.type_spec(&typ_name),
        }
    }

    fn type_spec(&self, type_name: &Ident) -> Result<Type, Error> {
        Ok(match &type_name.name[..] {
            "float" => Type::Float,
            "int" => Type::Int,
            "string" => Type::String,
            "bool" => Type::Bool,
            _ => {
                return Err(Error::UnknownType(Item::new(
                    type_name.name.clone(),
                    type_name.span(),
                )))
            }
        })
    }

    fn and(&self, lhs: &Expr, rhs: &Expr) -> Result<Type, Error> {
        self.check_expr(&lhs)?;
        self.check_expr(&rhs)
    }

    fn or(&self, lhs: &Expr, rhs: &Expr) -> Result<Type, Error> {
        self.check_expr(&lhs)?;
        self.check_expr(&rhs)
    }

    fn binary_expr(&self, bin_expr: &BinExpr) -> Result<Type, Error> {
        let lhs_typ = self.check_expr(&bin_expr.lhs)?;
        let rhs_typ = self.check_expr(&bin_expr.rhs)?;
        if lhs_typ != rhs_typ {
            return Err(Error::MismatchBinOp(
                Item::new("+".to_string(), bin_expr.rhs.span()),
                Item::new(lhs_typ.to_string(), bin_expr.lhs.span()),
                Item::new(rhs_typ.to_string(), bin_expr.rhs.span()),
            ));
        }

        Ok(lhs_typ)
    }

    fn unary_expr(&self, _op: &Op, expr: &Expr) -> Result<Type, Error> {
        self.check_expr(&expr)
    }

    fn index(&self, expr: &Expr, index: &Expr) -> Result<Type, Error> {
        self.check_expr(&expr)?;
        self.check_expr(&index)
    }

    fn list(&self, list: Vec<Expr>) -> Result<Type, Error> {
        let typ = self.check_expr(&list[0]);

        for item in &list[1..] {
            self.check_expr(&item)?;
        }

        typ
    }

    fn fun_call(&self, callee: &Expr, args: &Vec<Expr>) -> Result<Type, Error> {
        self.check_expr(&callee)?;
        for arg in args {
            self.check_expr(&arg)?;
        }

        Ok(Type::Error)
    }

    fn member_expr(&self, obj: &Expr, prop: &Expr) -> Result<Type, Error> {
        self.check_expr(&obj)?;
        self.check_expr(&prop)
    }

    fn identifier(&self, ident: &Ident) -> Result<Type, Error> {
        match self.env.find(&ident.name[..]) {
            Some((_, typ)) => Ok(typ.clone()),
            None => Err(Error::NotInScope(Item {
                content: ident.name.clone(),
                span: ident.span.clone(),
            })),
        }
    }

    fn number(&self, _val: &f64) -> Result<Type, Error> {
        Ok(Type::Float)
    }
    fn string(&self, _val: &String) -> Result<Type, Error> {
        Ok(Type::String)
    }
    fn boolean(&self, _val: &bool) -> Result<Type, Error> {
        Ok(Type::Bool)
    }
    fn nil(&self) -> Result<Type, Error> {
        Ok(Type::Any)
    }
}
