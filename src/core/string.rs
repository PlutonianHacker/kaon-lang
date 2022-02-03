use crate::{
    common,
    common::{Data, DataMap},
    core::{NativeFun, SharedContext},
};

fn length(_vm: SharedContext, args: Vec<Data>) -> Data {
    match &args[0] {
        Data::String(val) => Data::Number(val.len() as f64),
        _ => panic!("expected a string"),
    }
}

pub fn make_module() -> DataMap {
    let mut string = DataMap::new();

    string.insert_fun(
        "length",
        common::NativeFun::new("length", 1, NativeFun::new(Box::new(length))),
    );

    /*string.insert_fun(
        "test",
        common::NativeFun::new("test", 2, NativeFun::new(Box::new(test))),
    );*/

    string
}
