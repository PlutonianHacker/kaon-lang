use crate::common::{Span, Value};

use super::Function;

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

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Chunk {
    pub opcodes: Vec<u8>,
    pub constants: Vec<Box<Value>>,
    /// A vec storing static strings.
    pub variables: Vec<Box<str>>,
    /// A vec of function declaration.
    pub functions: Vec<Function>,
    pub debug_info: DebugInfo,
}

impl Chunk {
    pub fn empty() -> Self {
        Chunk {
            opcodes: Vec::new(),
            constants: Vec::new(),
            variables: Vec::new(),
            functions: Vec::new(),
            debug_info: DebugInfo::default(),
        }
    }

    pub fn add_constant(&mut self, constant: Value) -> usize {
        let index = self.constants.len();
        self.constants.push(Box::new(constant));
        index
    }

    pub fn identifier<S: Into<Box<str>>>(&mut self, value: S) -> usize {
        self.variables.push(value.into());

        self.variables.len() - 1
    }

    pub fn emit_span(&mut self, span: Span) {
        self.debug_info.push(self.opcodes.len(), span)
    }
}

pub enum ChunkConstant {
    Bool(bool),
    Float(f64),
    Integer(f64),
}
