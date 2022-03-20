use crate::{
    common::{NativeFun, Value, ValueMap},
    runtime::Vm,
};

fn length(vm: &mut Vm, _args: Vec<Value>) -> Value {
    vm.stack.pop();
    match &vm.stack.peek() {
        Value::List(list) => Value::Number(list.len() as f64),
        _ => panic!("expected a list"),
    }
}

pub fn make_module() -> ValueMap {
    let mut list = ValueMap::new();

    list.insert_fun("len", NativeFun::new("len", 0, length));

    list
}
