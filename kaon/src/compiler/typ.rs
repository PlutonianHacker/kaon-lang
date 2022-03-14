use std::rc::Rc;

pub enum Type {
    App { name: String, args: Vec<Type> },
    Fun { retrn: Rc<Type>, args: Vec<Type> },
    Tuple { elems: Vec<Type> },
    Class { name: String, args: Vec<Type> },
}

pub fn float() -> Type {
    Type::App {
        name: "float".to_string(),
        args: vec![],
    }
}

pub fn string() -> Type {
    Type::App {
        name: "string".to_string(),
        args: vec![],
    }
}

pub fn bool() -> Type {
    Type::App {
        name: "bool".to_string(),
        args: vec![],
    }
}

pub fn dynamic() -> Type {
    Type::App {
        name: "dynamic".to_string(),
        args: vec![],
    }
}

pub fn void() -> Type {
    Type::App {
        name: "void".to_string(),
        args: vec![],
    }
}

pub fn list(t: Type) -> Type {
    Type::App {
        name: "List".to_string(),
        args: vec![t],
    }
}

pub fn tuple(elems: Vec<Type>) -> Type {
    Type::Tuple { elems }
}

pub fn map(k: Type, v: Type) -> Type {
    Type::App {
        name: "map".to_string(),
        args: vec![k, v],
    }
}

pub fn fun(retrn: Rc<Type>, args: Vec<Type>) -> Type {
    Type::Fun { retrn, args }
}
