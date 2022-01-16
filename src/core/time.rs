use crate::common::Data;
use std::time::{SystemTime, UNIX_EPOCH};

pub fn clock(_: Vec<Data>) -> Data {
    let start = SystemTime::now();
    let time_since_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("time is went backwards");
    Data::Number(time_since_epoch.as_secs_f64())
}