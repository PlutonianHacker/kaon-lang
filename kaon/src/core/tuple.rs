use crate::common::{Value, ValueMap, NativeFun};
use crate::runtime::Vm;

fn first(_vm: &mut Vm, args: Vec<Value>) -> Value {
    match &args[0] {
        Value::Tuple(tuple) => {
            if let [first, ..] = &tuple.0[..] {
                first.clone()
            } else {
                Value::Nil
            }
        }
        _ => panic!("expected tuple"),
    }
}

fn last(_vm: &mut Vm, args: Vec<Value>) -> Value {
    match &args[0] {
        Value::Tuple(tuple) => {
            if let [.., last] = &tuple.0[..] {
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

    tuple.insert_fun("first", NativeFun::new("first", 1, first));

    tuple.insert_fun("last", NativeFun::new("last", 1, last));

    tuple
}
