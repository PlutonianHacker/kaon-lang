//! The core library for the Kaon language

pub mod ffi;
pub mod io;
pub mod list;
pub mod math;
pub mod os;
pub mod string;
pub mod tuple;

pub use self::ffi::{NativeFun, SharedContext};

use crate::common::ValueMap;

#[derive(Default)]
pub struct CoreLib {
    pub io: ValueMap,
    pub os: ValueMap,
    pub math: ValueMap,
    pub string: ValueMap,
    pub list: ValueMap,
    pub tuple: ValueMap,
}

impl CoreLib {
    pub fn new() -> Self {
        CoreLib {
            io: io::make_module(),
            os: os::make_module(),
            math: math::make_module(),
            string: string::make_module(),
            list: list::make_module(),
            tuple: tuple::make_module(),
        }
    }
}