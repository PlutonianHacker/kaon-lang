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

pub fn make_module() -> ValueMap {
    let mut string = ValueMap::new();

    string.insert_fun(
        "length",
        common::NativeFun::new("length", 1, NativeFun::new(Box::new(length))),
    );

    string
}
