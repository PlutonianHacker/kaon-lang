use crate::{
    common,
    common::{Value, ValueMap},
    core::{NativeFun, SharedContext},
};

fn length(_vm: SharedContext, args: Vec<Value>) -> Value {
    match &args[0] {
        Value::List(list) => Value::Number(list.len() as f64),
        _ => panic!("expected a list"),
    }
}

pub fn make_module() -> ValueMap {
    let mut list = ValueMap::new();

    list.insert_fun(
        "length",
        common::NativeFun::new("length", 1, NativeFun::new(Box::new(length))),
    );

    list
}
