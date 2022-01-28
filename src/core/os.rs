use crate::{
    common::{Data, DataMap, NativeFun},
    core::{NativeFun as Fun, SharedContext},
};
use std::time::{SystemTime, UNIX_EPOCH};

pub fn clock(_vm: SharedContext, _args: Vec<Data>) -> Data {
    let start = SystemTime::now();
    let time_since_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("time is went backwards");
    Data::Number(time_since_epoch.as_secs_f64())
}

pub fn make_module() -> DataMap {
    let mut os = DataMap::new();

    os.insert_fun(
        "clock",
        NativeFun::new("clock", 0, Fun::new(Box::new(clock))),
    );

    os
}
