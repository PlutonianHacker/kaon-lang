//! The core library for the Kaon language

pub mod io;
pub mod list;
pub mod math;
pub mod os;
pub mod string;
pub mod tuple;

use crate::{
    common::{NativeFun as Fun, ValueMap},
    error::Error,
    Scope, compiler::Resolver,
};

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

pub fn defaults(prelude: &mut ValueMap) {
    prelude.insert_fun("print", Fun::new("print", 1, io::println));

    prelude.insert_fun("assert_eq", Fun::new("assert_eq", 2, io::assert_eq));
}

pub fn prelude() -> Result<Scope, Error> {
    let source = crate::common::Source::from_file("kaon/src/core/core.kaon").unwrap();
    let ast = crate::compiler::Parser::parse_source(source)?;

    let mut resolver = Resolver::default();
    resolver.resolve_ast(&ast);

    Ok(resolver.global_scope())
}
