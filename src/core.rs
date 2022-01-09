use crate::data::Data;
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

pub fn println(args: Vec<Data>) -> Data {
    println!("{}", &args[0]);
    return args[0].clone();
}

fn sqrt(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.sqrt()),
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
    ffi.add("println", NativeFun::new(Box::new(println)));
    ffi.add("sqrt", NativeFun::new(Box::new(sqrt)));

    return ffi;
}
