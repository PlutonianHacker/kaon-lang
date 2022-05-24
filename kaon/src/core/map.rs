use std::rc::Rc;

use crate::{common::{Class, ImmutableString, Map, Named}, Value};

fn contains_key(map: &mut Map, key: ImmutableString) -> bool {
    map.contains_key(&key.into_owned())
}

fn remove(map: &mut Map, key: ImmutableString) -> Value {
    map.remove(&key.into_owned()).unwrap_or(Value::Nil)
}

fn insert(map: &mut Map, key: ImmutableString, value: Value) {
    map.insert(key.into_owned(), value);
}

fn len(map: &mut Map) -> usize {
    map.len()
}

fn is_empty(map: &mut Map) -> bool {
    map.is_empty()
}

pub fn make_class() -> Rc<Class> {
    let class = Class::new(Map::NAME);

    class.register_method("contains_key", contains_key);
    class.register_method("remove", remove);
    class.register_method("insert", insert);
    class.register_method("len", len);
    class.register_method("is_empty", is_empty);

    class
}
