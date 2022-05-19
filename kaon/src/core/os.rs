use crate::{
    common::{NativeFun, Value, ValueMap, Class, Instance},
    runtime::Vm,
};
use std::{time::{SystemTime, UNIX_EPOCH, Instant}, rc::Rc};

pub fn clock() -> f64 {
    let start = SystemTime::now();
    let time_since_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("time went backwards");
    time_since_epoch.as_secs_f64()
}

pub fn make_module() -> ValueMap {
    let mut os = ValueMap::new();

    //os.insert_fun("clock", NativeFun::new("clock", 0, clock));

    os
}

fn now(_clock: &mut Rc<Instance>) {}

fn elapsed(time: &mut Rc<Instance>) -> f64 {
    let fields = time.fields.borrow();

    let elapsed = match fields.get("start").unwrap() {
        Value::Float(f) => f,
        _ => unimplemented!()
    };

    clock() - elapsed
}

pub fn make_class() -> Rc<Class> {
    let class = Class::new("Time");

    class.add_field("start", clock());
    class.register_init("now", now);
    class.register_method("elapsed", elapsed);

    class
}