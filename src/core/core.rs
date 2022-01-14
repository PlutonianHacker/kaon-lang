use crate::common::Data;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct NativeFun(pub Rc<dyn Fn(Vec<Data>) -> Data>);

impl NativeFun {
    pub fn new(fun: Box<fn(Vec<Data>) -> Data>) -> Self {
        NativeFun(Rc::new(fun))
    }

    pub fn call(self, args: Vec<Data>) -> Data {
        (self.0)(args)
    }
}

fn println(args: Vec<Data>) -> Data {
    println!(
        "{}",
        args.iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>()
            .join(" ")
    );
    return Data::Unit;
}

fn to_string(args: Vec<Data>) -> Data {
    Data::String(format!("{}", args[0]))
}

fn sqrt(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.sqrt()),
        _ => return Data::Unit,
    }
}

fn pow(args: Vec<Data>) -> Data {
    match (args[0].clone(), args[1].clone()) {
        (Data::Number(lhs), Data::Number(rhs)) => return Data::Number(lhs.powf(rhs)),
        _ => return Data::Unit,
    }
}

pub struct FFI(HashMap<String, NativeFun>);

impl FFI {
    pub fn new() -> Self {
        FFI(HashMap::new())
    }

    pub fn add(&mut self, ident: &str, fun: NativeFun) {
        self.0.insert(ident.to_string(), fun);
    }

    pub fn get(&mut self, ident: &str) -> Option<&NativeFun> {
        self.0.get(ident)
    }
}

pub fn ffi_core() -> FFI {
    let mut ffi = FFI::new();

    // io
    ffi.add("println", NativeFun::new(Box::new(println)));
    ffi.add("to_string", NativeFun::new(Box::new(to_string)));

    // maths
    ffi.add("sqrt", NativeFun::new(Box::new(sqrt)));
    ffi.add("pow", NativeFun::new(Box::new(pow)));

    return ffi;
}
