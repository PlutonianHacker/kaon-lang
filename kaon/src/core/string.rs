use std::rc::Rc;

use crate::{
    common,
    common::{Value, ValueMap},
    core::{NativeFun, SharedContext},
};

fn length(_vm: SharedContext, args: Vec<Value>) -> Value {
    match &args[0] {
        Value::String(val) => Value::Number(val.len() as f64),
        _ => panic!("expected a string"),
    }
}

fn format_str(_vm: SharedContext, args: Vec<Value>) -> Value {
    match &args[0] {
        Value::String(string) => {
            let slices = string.split("{}");
            let mut result = "".to_string();
            for (pos, slice) in slices.enumerate() {
                result += &(slice.to_string() + &args[pos + 1].to_string());
            }

            Value::String(Rc::new(result))
        }
        _ => panic!("expected a string"),
    }
}

pub fn make_module() -> ValueMap {
    let mut string = ValueMap::new();

    string.insert_fun(
        "length",
        common::NativeFun::new("length", 1, NativeFun::new(Box::new(length)), false),
    );

    string.insert_fun(
        "format",
        common::NativeFun::new("format", 1, NativeFun::new(Box::new(format_str)), true),
    );

    string
}
