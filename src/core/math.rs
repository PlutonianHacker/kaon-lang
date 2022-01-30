use crate::common::{Data, DataMap, NativeFun};
use crate::core::NativeFun as CoreFun;
use crate::core::SharedContext;

pub fn sqrt(_vm: SharedContext, args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.sqrt()),
        _ => panic!("value must be a number"),
    }
}

pub fn pow(_vm: SharedContext, args: Vec<Data>) -> Data {
    match (args[0].clone(), args[1].clone()) {
        (Data::Number(lhs), Data::Number(rhs)) => return Data::Number(lhs.powf(rhs)),
        _ => panic!("value must be a number"),
    }
}

pub fn abs(_vm: SharedContext, args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.abs()),
        _ => panic!("value must be a number"),
    }
}

pub fn round(_vm: SharedContext, args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.round()),
        _ => panic!("value must be a number"),
    }
}

pub fn sin(_vm: SharedContext, args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.sin()),
        _ => panic!("value must be a number"),
    }
}

pub fn tan(_vm: SharedContext, args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.tan()),
        _ => panic!("value must be a number"),
    }
}

pub fn cos(_vm: SharedContext, args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.cos()),
        _ => panic!("value must be a number"),
    }
}

pub fn asin(_vm: SharedContext, args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.asin()),
        _ => panic!("value must be a number"),
    }
}

pub fn atan(_vm: SharedContext, args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.atan()),
        _ => panic!("value must be a number"),
    }
}

pub fn acos(_vm: SharedContext, args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.acos()),
        _ => panic!("value must be a number"),
    }
}

pub fn to_radians(_vm: SharedContext, args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.to_radians()),
        _ => panic!("value must be a number"),
    }
}

pub fn make_module() -> DataMap {
    let mut math = DataMap::new();

    math.insert_fun(
        "sqrt",
        NativeFun::new("sqrt", 1, CoreFun::new(Box::new(sqrt))),
    );
    math.insert_fun("pow", NativeFun::new("pow", 2, CoreFun::new(Box::new(pow))));
    math.insert_fun("abs", NativeFun::new("abs", 1, CoreFun::new(Box::new(abs))));
    math.insert_fun(
        "round",
        NativeFun::new("round", 1, CoreFun::new(Box::new(round))),
    );
    math.insert_fun("sin", NativeFun::new("sin", 1, CoreFun::new(Box::new(sin))));
    math.insert_fun("cos", NativeFun::new("cos", 1, CoreFun::new(Box::new(cos))));
    math.insert_fun("tan", NativeFun::new("tan", 1, CoreFun::new(Box::new(tan))));
    math.insert_fun(
        "asin",
        NativeFun::new("asin", 1, CoreFun::new(Box::new(asin))),
    );
    math.insert_fun(
        "acos",
        NativeFun::new("acos", 1, CoreFun::new(Box::new(acos))),
    );
    math.insert_fun(
        "atan",
        NativeFun::new("atan", 1, CoreFun::new(Box::new(atan))),
    );
    math.insert_fun(
        "to_radians",
        NativeFun::new("to_radians", 1, CoreFun::new(Box::new(to_radians))),
    );

    math.insert_constant("PI", Data::Number(std::f64::consts::PI));
    math.insert_constant("E", Data::Number(std::f64::consts::E));
    math.insert_constant("INFINITY", Data::Number(std::f64::INFINITY));
    math.insert_constant("NAN", Data::Number(std::f64::NAN));

    math
}
