//! The core library for the Kaon language

pub mod io;
pub mod list;
pub mod os;
pub mod string;
pub mod tuple;
mod float;

use std::{rc::Rc};

use crate::{
    common::{state::State, Class, ValueMap},
    runtime::Vm,
    Value,
};

#[derive(Default)]
pub struct CoreLib {
    pub io: ValueMap,
    pub os: ValueMap,
    pub string: ValueMap,
    pub list: ValueMap,
    pub tuple: ValueMap,
}

impl CoreLib {
    pub fn new() -> Self {
        CoreLib {
            io: io::make_module(),
            os: os::make_module(),
            string: string::make_module(),
            list: list::make_module(),
            tuple: tuple::make_module(),
        }
    }
}

fn print(vm: &mut Vm, value: Value) {
    vm.context
        .as_ref()
        .borrow()
        .settings
        .stdout
        .writeln(&value.to_string()).unwrap();
}

pub fn prelude() -> State {
    let mut prelude = State::new();

    prelude.add::<Rc<Class>>("String", string::make_class());
    prelude.add::<Rc<Class>>("Float", float::make_class());
    prelude.register_function("print", print);

    prelude
}

