use crate::common::{self, Value, ValueMap};
use crate::core::{NativeFun, SharedContext};

fn first(_vm: SharedContext, args: Vec<Value>) -> Value {
    match &args[0] {
        Value::Tuple(tuple) => {
            if let [first, ..] = &tuple[..] {
                first.clone()
            } else {
                Value::Nil
            }
        }
        _ => panic!("expected tuple"),
    }
}

fn last(_vm: SharedContext, args: Vec<Value>) -> Value {
    match &args[0] {
        Value::Tuple(tuple) => {
            if let [.., last] = &tuple[..] {
                last.clone()
            } else {
                Value::Nil
            }
        }
        _ => panic!("expected tuple"),
    }
}

pub fn make_module() -> ValueMap {
    let mut tuple = ValueMap::new();

    tuple.insert_fun(
        "first",
        common::NativeFun::new("first", 1, NativeFun::new(Box::new(first))),
    );

    tuple.insert_fun(
        "last",
        common::NativeFun::new("last", 1, NativeFun::new(Box::new(last))),
    );

    tuple
}
