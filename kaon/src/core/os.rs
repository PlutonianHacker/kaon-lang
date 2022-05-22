use crate::common::{Class, Instance, Value};
use std::{
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

pub fn clock() -> f64 {
    let start = SystemTime::now();
    let time_since_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("time went backwards");
    time_since_epoch.as_secs_f64()
}

fn now() -> f64 {
    let start = SystemTime::now();
    let time_since_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("time went backwards");
    time_since_epoch.as_secs_f64()
}

fn elapsed(time: &mut Rc<Instance>) -> f64 {
    let fields = time.fields.borrow();

    let elapsed = match fields.get("start").unwrap() {
        Value::Float(f) => f,
        _ => unimplemented!(),
    };

    clock() - elapsed
}

pub fn make_class() -> Rc<Class> {
    let class = Class::new("Os");

    class.add_field("start", clock());
    class.register_static("now", now);
    class.register_method("elapsed", elapsed);

    class
}
