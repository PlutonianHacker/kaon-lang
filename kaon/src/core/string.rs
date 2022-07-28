use std::rc::Rc;
use crate::common::{ImmutableString, Class};

fn init_string(_str: ImmutableString, raw_str: String) -> ImmutableString {
    ImmutableString::from(raw_str)
} 

fn len(str: &mut ImmutableString) -> usize {
    str.len()
}

fn is_empty(str: &mut ImmutableString) -> bool {
    str.is_empty()
}

fn contains(str: ImmutableString, other: String) -> bool {
    let s = str.clone().into_owned();

    s.contains(&other)
}

pub fn make_class() -> Rc<Class> {
    let class = Class::new("String");

    class.register_init("new", init_string);

    class.register_method("len", len);
    class.register_method("is_empty", is_empty);
    class.register_method("contains", contains);

    class
}