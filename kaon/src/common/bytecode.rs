use std::rc::Rc;

use crate::common::{Span, Value};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct DebugInfo {
    pub source_map: Vec<(usize, Span)>,
}

impl DebugInfo {
    pub fn push(&mut self, ip: usize, span: Span) {
        self.source_map.push((ip, span));
    }

    pub fn get_source(&self, ip: usize) -> Option<Span> {
        let mut result = None;

        for entry in self.source_map.iter() {
            if ip <= entry.0 {
                result = Some(entry.1.clone());
            }
        }

        result
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ByteCode {
    pub opcodes: Vec<u8>,
    pub constants: Vec<Value>,
    pub debug_info: DebugInfo,
}

impl ByteCode {
    pub fn empty() -> Self {
        ByteCode {
            opcodes: vec![],
            constants: vec![],
            debug_info: DebugInfo::default(),
        }
    }

    pub fn add_constant(&mut self, constant: Value) -> usize {
        let index = self.constants.len();
        self.constants.push(constant);
        index
    }

    pub fn identifier(&mut self, value: String) -> usize {
        self.add_constant(Value::String(Rc::new(value)))
    }

    pub fn emit_span(&mut self, span: Span) {
        self.debug_info.push(self.opcodes.len(), span)
    }
}
