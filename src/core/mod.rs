//! The core library for the Kaon language

pub mod ffi;
pub mod io;
pub mod list;
pub mod math;
pub mod os;
pub mod string;

pub use self::ffi::{NativeFun, SharedContext, FFI};

use crate::common::DataMap;

pub struct CoreLib {
    pub io: DataMap,
    pub os: DataMap,
    pub math: DataMap,
    pub string: DataMap,
    pub list: DataMap,
}

impl CoreLib {
    pub fn new() -> Self {
        CoreLib {
            io: io::make_module(),
            os: os::make_module(),
            math: math::make_module(),
            string: string::make_module(),
            list: list::make_module(),
        }
    }
}
