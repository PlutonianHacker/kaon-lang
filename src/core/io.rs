use crate::common::{NativeFun, Value, ValueMap};
use crate::core::{NativeFun as Fun, SharedContext};

pub fn println(vm: SharedContext, args: Vec<Value>) -> Value {
    vm.as_ref()
        .borrow_mut()
        .settings
        .stdout
        .writeln(
            &args
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<String>>()
                .join(" "),
        )
        .unwrap();
    Value::Unit
}

pub fn to_string(_: SharedContext, args: Vec<Value>) -> Value {
    Value::String(format!("{}", args[0]))
}

pub fn make_module() -> ValueMap {
    let mut io = ValueMap::new();

    io.insert_fun(
        "println",
        NativeFun::new("println", 1, Fun::new(Box::new(println)), true),
    );
    io.insert_fun(
        "to_string",
        NativeFun::new("to_string", 1, Fun::new(Box::new(to_string)), false),
    );

    io
}
