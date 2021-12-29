use crate::data::Data;
use std::collections::HashMap;
use std::rc::Rc;

pub struct NativeFunction(Rc<dyn Fn(Vec<Data>) -> Data>);

impl NativeFunction {
    pub fn new(fun: Box<fn(Vec<Data>) -> Data>) -> Self {
        NativeFunction(Rc::new(fun))
    }

    pub fn call(self, args: Vec<Data>) -> Data {
        (self.0)(args)
    }
}

pub fn add(args: Vec<Data>) -> Data {
    match (args[0].clone(), args[1].clone()) {
        (Data::Number(l), Data::Number(r)) => Data::Number(l + r),
        _ => panic!("Cannot add non-numbers"),
    }
}

pub fn sub(args: Vec<Data>) -> Data {
    match (args[0].clone(), args[1].clone()) {
        (Data::Number(l), Data::Number(r)) => Data::Number(l - r),
        _ => panic!("Cannot subtract non-numbers"),
    }
}

pub fn mul(args: Vec<Data>) -> Data {
    match (args[0].clone(), args[1].clone()) {
        (Data::Number(l), Data::Number(r)) => Data::Number(l * r),
        _ => panic!("Cannot multiply non-numbers"),
    }
}

pub fn div(args: Vec<Data>) -> Data {
    match (args[0].clone(), args[1].clone()) {
        (Data::Number(l), Data::Number(r)) => Data::Number(l / r),
        _ => panic!("Cannot divide non-numbers"),
    }
}

pub struct FFI(HashMap<String, NativeFunction>);

impl FFI {
    pub fn new() -> Self {
        FFI(HashMap::new())
    }

    pub fn add(&mut self, ident: &str, fun: NativeFunction) {
        self.0.insert(ident.to_string(), fun);
    }

    pub fn get(&mut self, ident: &str) -> &NativeFunction {
        self.0.get(ident).unwrap()
    }
}

pub fn core() -> FFI {
    let mut ffi = FFI::new();

    ffi.add("add", NativeFunction::new(Box::new(add)));
    ffi.add("sub", NativeFunction::new(Box::new(sub)));
    ffi.add("mul", NativeFunction::new(Box::new(mul)));
    ffi.add("div", NativeFunction::new(Box::new(div)));

    return ffi;
}

#[cfg(test)]
mod test {
    use crate::data::Data;
    use crate::ffi::core;

    #[test]
    fn test_core() {
        let mut ffi = core();
        let add = ffi.get("add");
        let res = add.0(vec![Data::Number(2.0), Data::Number(1.0)]);
        assert_eq!(res, Data::Number(3.0))
    }
}
