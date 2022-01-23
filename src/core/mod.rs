pub mod ffi;
pub mod io;
pub mod math;
pub mod time;

pub use self::ffi::{NativeFun, FFI};

pub fn ffi_core() -> FFI {
    let mut ffi = FFI::new();

    // io
    ffi.add("println", NativeFun::new(Box::new(io::println)));
    ffi.add("to_string", NativeFun::new(Box::new(io::to_string)));

    // time
    ffi.add("clock", NativeFun::new(Box::new(time::clock)));

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
    ffi.add("len", NativeFun::new(Box::new(math::len)));

    return ffi;
}
