use crate::common::Data;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

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

fn clock(_: Vec<Data>) -> Data {
    let start = SystemTime::now();
    let time_since_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("time is went backwards");
    Data::Number(time_since_epoch.as_secs_f64())
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

fn round(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.round()),
        _ => panic!("value must be a number"),
    }
}

fn sin(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.sin()),
        _ => panic!("value must be a number"),
    }
}

fn asin(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.asin()),
        _ => panic!("value must be a number"),
    }
}

fn tan(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.tan()),
        _ => panic!("value must be a number"),
    }
}

fn cos(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.cos()),
        _ => panic!("value must be a number"),
    }
}

fn to_radians(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.to_radians()),
        _ => panic!("value must be a number"),
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
    ffi.add("clock", NativeFun::new(Box::new(clock)));

    // maths
    ffi.add("sqrt", NativeFun::new(Box::new(sqrt)));
    ffi.add("pow", NativeFun::new(Box::new(pow)));
    ffi.add("round", NativeFun::new(Box::new(round)));
    ffi.add("sin", NativeFun::new(Box::new(sin)));
    ffi.add("tan", NativeFun::new(Box::new(tan)));
    ffi.add("cos", NativeFun::new(Box::new(cos)));
    ffi.add("to_radians", NativeFun::new(Box::new(to_radians)));
    ffi.add("asin", NativeFun::new(Box::new(asin)));

    return ffi;
}
