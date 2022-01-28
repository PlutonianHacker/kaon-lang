//! The core library for the Kaon language

pub mod ffi;
pub mod io;
pub mod math;
pub mod os;

pub use self::ffi::{NativeFun, SharedContext, FFI};

use crate::common::DataMap;

pub struct CoreLib {
    pub io: DataMap,
    pub os: DataMap,
    pub math: DataMap,
}

impl CoreLib {
    pub fn new() -> Self {
        CoreLib {
            io: io::make_module(),
            os: os::make_module(),
            math: math::make_module(),
        }
    }
}

pub fn ffi_core() -> FFI {
    let ffi = FFI::new();

    // io
    //ffi.add("println", NativeFun::new(Box::new(io::println)));
    //ffi.add("to_string", NativeFun::new(Box::new(io::to_string)));

    // time
    /*ffi.add("clock", NativeFun::new(Box::new(time::clock)));

    // math
    ffi.add("sqrt", NativeFun::new(Box::new(math::sqrt)));
    ffi.add("pow", NativeFun::new(Box::new(math::pow)));
    ffi.add("abs", NativeFun::new(Box::new(math::abs)));
    ffi.add("sin", NativeFun::new(Box::new(math::sin)));
    ffi.add("tan", NativeFun::new(Box::new(math::tan)));
    ffi.add("cos", NativeFun::new(Box::new(math::cos)));
    ffi.add("asin", NativeFun::new(Box::new(math::asin)));
    ffi.add("atan", NativeFun::new(Box::new(math::atan)));
    ffi.add("acos", NativeFun::new(Box::new(math::acos)));
    ffi.add("round", NativeFun::new(Box::new(math::round)));
    ffi.add("to_radians", NativeFun::new(Box::new(math::to_radians)));

    // list
    ffi.add("len", NativeFun::new(Box::new(math::len)));*/

    return ffi;
}
