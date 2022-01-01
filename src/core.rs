use crate::data::Data;
use std::collections::HashMap;
use std::rc::Rc;

pub struct NativeFunction(pub Rc<dyn Fn(Vec<Data>) -> Data>);

impl NativeFunction {
    pub fn new(fun: Box<fn(Vec<Data>) -> Data>) -> Self {
        NativeFunction(Rc::new(fun))
    }

    pub fn call(self, args: Vec<Data>) -> Data {
        (self.0)(args)
    }
}

pub fn println(args: Vec<Data>) -> Data {
    println!("{}", &args[0]);
    return args[0].clone()
}

pub struct FFI(HashMap<String, NativeFunction>);

impl FFI {
    pub fn new() -> Self {
        FFI(HashMap::new())
    }

    pub fn add(&mut self, ident: &str, fun: NativeFunction) {
        self.0.insert(ident.to_string(), fun);
    }

    pub fn get(&mut self, ident: &str) -> Option<&NativeFunction> {
        self.0.get(ident)
    }
}

pub fn ffi_core() -> FFI {
    let mut ffi = FFI::new();
    
    ffi.add("println", NativeFunction::new(Box::new(println)));

    return ffi;
}

#[cfg(test)]
mod test {
    use crate::data::Data;
    use crate::core::ffi_core;

    #[test]
    fn test_core() {
        let mut ffi = ffi_core();
        let add = ffi.get("add").unwrap();
        let res = add.0(vec![Data::Number(2.0), Data::Number(1.0)]);
        assert_eq!(res, Data::Number(3.0))
    }
}
