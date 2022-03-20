use std::rc::Rc;

use crate::{
    common::{NativeFun, Value, ValueMap},
    runtime::Vm,
};

fn length(vm: &mut Vm, _args: Vec<Value>) -> Value {
    vm.stack.pop();
    match vm.stack.peek() {
        Value::String(val) => Value::Number(val.len() as f64),
        _ => panic!("expected a string"),
    }
}

fn format_str(_vm: &mut Vm, args: Vec<Value>) -> Value {
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

    string.insert_fun("len", NativeFun::new("len", 0, length));

    string.insert_fun("format", NativeFun::varidic("format", 1, format_str));

    string
}
