use crate::common::{Value, ValueMap, NativeFun};
use crate::core::{NativeFun as Fun, SharedContext};

pub fn println(vm: SharedContext, args: Vec<Value>) -> Value {
    for arg in args.iter() {
        vm.as_ref()
            .borrow_mut()
            .settings
            .stdout
            .writeln(&arg.to_string())
            .unwrap();
    }
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
