use crate::common::{NativeFun, Value, ValueMap, ImmutableString};
use crate::runtime::Vm;

pub fn println(vm: &mut Vm, args: Vec<Value>) -> Value {
    vm.context
        .as_ref()
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

pub fn to_string(_: &mut Vm, args: Vec<Value>) -> Value {
    Value::String(ImmutableString::from(format!("{}", args[0])))
}

pub fn assert_eq(vm: &mut Vm, args: Vec<Value>) -> Value {
    if args[0] != args[1] {
        vm.exception_handler("left does not equal right");
    }

    Value::Unit
}

pub fn make_module() -> ValueMap {
    let mut io = ValueMap::new();

    io.insert_fun("println", NativeFun::varidic("println", 1, println));
    io.insert_fun("to_string", NativeFun::new("to_string", 1, to_string));

    io
}
