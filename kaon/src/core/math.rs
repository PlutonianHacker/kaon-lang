use crate::common::{NativeFun, Value, ValueMap};
use crate::runtime::Vm;

fn sqrt(_vm: &mut Vm, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => Value::Number(val.sqrt()),
        _ => panic!("value must be a number"),
    }
}

fn pow(_vm: &mut Vm, args: Vec<Value>) -> Value {
    match (args[0].clone(), args[1].clone()) {
        (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs.powf(rhs)),
        _ => panic!("value must be a number"),
    }
}

fn abs(_vm: &mut Vm, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => Value::Number(val.abs()),
        _ => panic!("value must be a number"),
    }
}

fn round(_vm: &mut Vm, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => Value::Number(val.round()),
        _ => panic!("value must be a number"),
    }
}

fn sin(_vm: &mut Vm, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => Value::Number(val.sin()),
        _ => panic!("value must be a number"),
    }
}

fn tan(_vm: &mut Vm, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => Value::Number(val.tan()),
        _ => panic!("value must be a number"),
    }
}

fn cos(_vm: &mut Vm, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => Value::Number(val.cos()),
        _ => panic!("value must be a number"),
    }
}

fn asin(_vm: &mut Vm, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => Value::Number(val.asin()),
        _ => panic!("value must be a number"),
    }
}

fn atan(_vm: &mut Vm, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => Value::Number(val.atan()),
        _ => panic!("value must be a number"),
    }
}

fn acos(_vm: &mut Vm, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => Value::Number(val.acos()),
        _ => panic!("value must be a number"),
    }
}

fn to_radians(_vm: &mut Vm, args: Vec<Value>) -> Value {
    match args[0] {
        Value::Number(val) => Value::Number(val.to_radians()),
        _ => panic!("value must be a number"),
    }
}

pub fn make_module() -> ValueMap {
    let mut math = ValueMap::new();

    math.insert_fun("pow", NativeFun::new("pow", 2, pow));
    math.insert_fun("abs", NativeFun::new("abs", 1, abs));
    math.insert_fun("sin", NativeFun::new("sin", 1, sin));
    math.insert_fun("cos", NativeFun::new("cos", 1, cos));
    math.insert_fun("tan", NativeFun::new("tan", 1, tan));
    math.insert_fun("asin", NativeFun::new("asin", 1, asin));
    math.insert_fun("acos", NativeFun::new("acos", 1, acos));
    math.insert_fun("atan", NativeFun::new("atan", 1, atan));
    math.insert_fun("sqrt", NativeFun::new("sqrt", 1, sqrt));
    math.insert_fun("round", NativeFun::new("round", 1, round));
    math.insert_fun("to_radians", NativeFun::new("to_radians", 1, to_radians));

    math.insert_constant("PI", Value::Number(std::f64::consts::PI));
    math.insert_constant("E", Value::Number(std::f64::consts::E));
    math.insert_constant("INFINITY", Value::Number(std::f64::INFINITY));
    math.insert_constant("NAN", Value::Number(std::f64::NAN));

    math
}
