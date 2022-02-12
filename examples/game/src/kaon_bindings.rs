use kaon_lang::common::{self, External, ExternalData, MetaMap, Value, ValueMap};
use kaon_lang::core::{NativeFun, SharedContext};
use std::{cell::RefCell, rc::Rc};

extern crate sdl2;

use std::any::Any;

use sdl2::{Sdl, VideoSubsystem};

pub fn sdl2_new(_vm: SharedContext, _args: Vec<Value>) -> Value {
    let sdl2_ctx = sdl2::init().unwrap();
    Value::External(KaonSdl::new(sdl2_ctx))
}

pub fn sdl2_video(_vm: SharedContext, args: Vec<Value>) -> Value {
    match &args[0] {
        Value::External(external) => {
            Value::Nil
        }
        _ => panic!("expected sdl2 context"),
    }
}

pub fn make_module() -> ValueMap {
    let mut sdl = ValueMap::new();

    sdl.insert_fun(
        "new",
        common::NativeFun::new("new", 0, NativeFun::new(Box::new(sdl2_new))),
    );

    /*sdl.insert_fun(
        "video",
        common::NativeFun::new("video", 1, NativeFun::new(Box::new(sdl2_video))),
    );*/

    sdl
}

pub fn make_meta_map() -> Rc<RefCell<MetaMap>> {
    let mut meta_map = MetaMap::new();

    meta_map.insert(
        "video",
        Value::NativeFun(Box::new(common::NativeFun::new(
            "video",
            1,
            NativeFun::new(Box::new(sdl2_video)),
        ))),
    );

    Rc::new(RefCell::new(meta_map))
}

pub struct KaonSdl(Sdl);

impl ExternalData for KaonSdl {}

impl KaonSdl {
    pub fn new(ctx: Sdl) -> External {
        External::new(Rc::new(RefCell::new(KaonSdl(ctx))), make_meta_map())
    }
}

use std::fmt;

impl fmt::Debug for KaonSdl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Sdl2")
    }
}
