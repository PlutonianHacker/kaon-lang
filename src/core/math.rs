use crate::common::{Value, ValueMap, NativeFun};
use crate::core::NativeFun as CoreFun;
use crate::core::SharedContext;

pub fn sqrt(_vm: SharedContext, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => return Value::Number(val.sqrt()),
        _ => panic!("value must be a number"),
    }
}

pub fn pow(_vm: SharedContext, args: Vec<Value>) -> Value {
    match (args[0].clone(), args[1].clone()) {
        (Value::Number(lhs), Value::Number(rhs)) => return Value::Number(lhs.powf(rhs)),
        _ => panic!("value must be a number"),
    }
}

pub fn abs(_vm: SharedContext, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => return Value::Number(val.abs()),
        _ => panic!("value must be a number"),
    }
}

pub fn round(_vm: SharedContext, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => return Value::Number(val.round()),
        _ => panic!("value must be a number"),
    }
}

pub fn sin(_vm: SharedContext, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => return Value::Number(val.sin()),
        _ => panic!("value must be a number"),
    }
}

pub fn tan(_vm: SharedContext, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => return Value::Number(val.tan()),
        _ => panic!("value must be a number"),
    }
}

pub fn cos(_vm: SharedContext, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => return Value::Number(val.cos()),
        _ => panic!("value must be a number"),
    }
}

pub fn asin(_vm: SharedContext, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => return Value::Number(val.asin()),
        _ => panic!("value must be a number"),
    }
}

pub fn atan(_vm: SharedContext, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => return Value::Number(val.atan()),
        _ => panic!("value must be a number"),
    }
}

pub fn acos(_vm: SharedContext, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => return Value::Number(val.acos()),
        _ => panic!("value must be a number"),
    }
}

pub fn to_radians(_vm: SharedContext, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => return Value::Number(val.to_radians()),
        _ => panic!("value must be a number"),
    }
}

pub fn make_module() -> ValueMap {
    let mut math = ValueMap::new();

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

    math.insert_constant("PI", Value::Number(std::f64::consts::PI));
    math.insert_constant("E", Value::Number(std::f64::consts::E));
    math.insert_constant("INFINITY", Value::Number(std::f64::INFINITY));
    math.insert_constant("NAN", Value::Number(std::f64::NAN));

    math
}
