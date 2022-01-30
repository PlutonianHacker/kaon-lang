use crate::common::{Data, DataMap, NativeFun};
use crate::core::{NativeFun as Fun, SharedContext};

pub fn println(vm: SharedContext, args: Vec<Data>) -> Data {
    for arg in args.iter() {
        vm.as_ref()
            .borrow_mut()
            .settings
            .stdout
            .writeln(&arg.to_string())
            .unwrap();
    }
    return Data::Unit;
}

pub fn to_string(_: SharedContext, args: Vec<Data>) -> Data {
    Data::String(format!("{}", args[0]))
}

pub fn make_module() -> DataMap {
    let mut io = DataMap::new();

    io.insert_fun(
        "println",
        NativeFun::new("println", 1, Fun::new(Box::new(println))),
    );
    io.insert_fun(
        "to_string",
        NativeFun::new("to_string", 1, Fun::new(Box::new(to_string))),
    );

    io
}
