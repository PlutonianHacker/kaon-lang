use super::module::Module;
use crate::error::Error;

#[derive(Default)]
pub struct Loader {
    pub cache: Vec<Module>,
}

impl Loader {
    pub fn new() -> Self {
        Self {
            cache: Vec::new(),
        }
    }

    pub fn compile_module(&mut self, _name: &str) -> Result<Module, Error> {
        todo!()
    }
}