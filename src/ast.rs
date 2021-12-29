#[derive(Debug, PartialEq)]
pub enum AST {
    FuncCall(String, ArgList),
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl AST {
    pub fn func_call(ident: &str, args: ArgList) -> AST {
        AST::FuncCall(ident.to_string(), args)
    }

    pub fn number(val: f64) -> AST {
        AST::Number(val)
    }

    pub fn string(val: &str) -> AST {
        AST::String(val.to_string())
    }

    pub fn boolean(val: bool) -> AST {
        AST::Boolean(val)
    }
}

pub type ArgList = Vec<AST>;
