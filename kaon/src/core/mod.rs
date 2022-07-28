//! The core library for the Kaon language

pub mod float;
pub mod io;
pub mod map;
pub mod os;
pub mod string;
pub mod tuple;

use crate::{common::{ImmutableString}, Value};

pub fn str(v: &mut Value) -> ImmutableString {
    ImmutableString::from(v.to_string())
}
/*
pub fn prelude() -> State {
    let mut prelude = State::new();

    prelude.add::<Rc<Class>>("String", string::make_class());
    prelude.add::<Rc<Class>>("Float", float::make_class());
    prelude.add::<Rc<Class>>("System", io::make_class());
    prelude.add::<Rc<Class>>("Os", os::make_class());
    prelude.add::<Rc<Class>>("Map", map::make_class());

    prelude.register_function("print", io::print);
    prelude.register_function("str", str);

    prelude
}
*/