use crate::{
    common,
    common::{Data, DataMap},
    core::{NativeFun, SharedContext},
};

fn length(_vm: SharedContext, args: Vec<Data>) -> Data {
    match &args[0] {
        Data::List(list) => Data::Number(list.len() as f64),
        _ => panic!("expected a list"),
    }
}

pub fn make_module() -> DataMap {
    let mut list = DataMap::new();

    list.insert_fun(
        "length",
        common::NativeFun::new("length", 1, NativeFun::new(Box::new(length))),
    );

    list
}
