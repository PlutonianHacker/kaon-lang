use crate::common::Data;

pub fn sqrt(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.sqrt()),
        _ => panic!("value must be a number"),
    }
}

pub fn pow(args: Vec<Data>) -> Data {
    match (args[0].clone(), args[1].clone()) {
        (Data::Number(lhs), Data::Number(rhs)) => return Data::Number(lhs.powf(rhs)),
        _ => panic!("value must be a number"),
    }
}

pub fn abs(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.abs()),
        _ => panic!("value must be a number"),
    }
}

pub fn round(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.round()),
        _ => panic!("value must be a number"),
    }
}

pub fn sin(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.sin()),
        _ => panic!("value must be a number"),
    }
}

pub fn tan(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.tan()),
        _ => panic!("value must be a number"),
    }
}

pub fn cos(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.cos()),
        _ => panic!("value must be a number"),
    }
}

pub fn asin(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.asin()),
        _ => panic!("value must be a number"),
    }
}

pub fn atan(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.atan()),
        _ => panic!("value must be a number"),
    }
}

pub fn acos(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.acos()),
        _ => panic!("value must be a number"),
    }
}

pub fn to_radians(args: Vec<Data>) -> Data {
    match args[0] {
        Data::Number(val) => return Data::Number(val.to_radians()),
        _ => panic!("value must be a number"),
    }
}