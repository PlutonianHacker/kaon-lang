use crate::common::{ByteCode, Opcode};

pub struct Disassembler<'a> {
    name: &'a str,
    chunk: &'a ByteCode,
}

impl<'a> Disassembler<'a> {
    pub fn new(name: &'a str, chunk: &'a ByteCode) -> Self {
        Disassembler { name, chunk }
    }

    pub fn disassemble(&self) {
        println!("Disassembling {}", self.name);
        let mut offset = 0;

        println!("Code");
        while offset < self.chunk.opcodes.len() {
            offset = self.disassemble_instruction(offset);
        }

        println!("Constants");
        for con in &self.chunk.constants {
            print!("[ {} ]", con);
        }

        println!();
    }

    fn disassemble_instruction(&self, offset: usize) -> usize {
        let byte = self.chunk.opcodes[offset];
        match Opcode::from(byte) {
            Opcode::Const => self.byte_instruction("Const", offset),
            Opcode::True => self.simple_instruction("True", offset),
            Opcode::False => self.simple_instruction("False", offset),
            Opcode::Nil => self.simple_instruction("Nil", offset),
            Opcode::Add => self.simple_instruction("Add", offset),
            Opcode::Sub => self.simple_instruction("Subtract", offset),
            Opcode::Mul => self.simple_instruction("Multiply", offset),
            Opcode::Div => self.simple_instruction("Divide", offset),
            Opcode::Mod => self.simple_instruction("Modulo", offset),
            Opcode::Negate => self.simple_instruction("Negate", offset),
            Opcode::Equal => self.simple_instruction("EqualsTo", offset),
            Opcode::NotEqual => self.simple_instruction("NotEqual", offset),
            Opcode::Gte => self.simple_instruction("GreaterThanEquals", offset),
            Opcode::Lte => self.simple_instruction("LessThanEquals", offset),
            Opcode::Gt => self.simple_instruction("GreaterThan", offset),
            Opcode::Lt => self.simple_instruction("LessThan", offset),
            Opcode::Not => self.simple_instruction("Not", offset),
            Opcode::DefGlobal => self.byte_instruction("DefGlobal", offset),
            Opcode::SetGlobal => self.byte_instruction("SetGlobal", offset),
            Opcode::GetGlobal => self.byte_instruction("GetGlobal", offset),
            Opcode::LoadLocal => self.byte_instruction("LoadLocal", offset),
            Opcode::SaveLocal => self.byte_instruction("SaveLocal", offset),
            Opcode::LoadUpValue => self.byte_instruction("LoadUpValue", offset),
            Opcode::SaveUpValue => self.byte_instruction("SaveUpValue", offset),
            Opcode::CloseUpValue => self.simple_instruction("CloseUpValue", offset),
            Opcode::Jump => self.short_instruction("Jump", offset),
            Opcode::JumpIfFalse => self.short_instruction("JumpIfFalse", offset),
            Opcode::JumpIfTrue => self.short_instruction("JumpIfTrue", offset),
            Opcode::Print => self.simple_instruction("[Deprecated] Print", offset),
            Opcode::Call => self.simple_instruction("Call", offset),
            Opcode::Closure => self.closure(offset),
            Opcode::Return => self.simple_instruction("Return", offset),
            Opcode::Del => self.simple_instruction("Del", offset),
            Opcode::Class => self.byte_instruction("Class", offset),
            Opcode::Constructor => self.simple_instruction("Constructor", offset),
            Opcode::Method => self.byte_instruction("Method", offset),
            Opcode::Instance => self.byte_instruction("Instance", offset),
            Opcode::List => self.byte_instruction("List", offset),
            Opcode::BuildTuple => self.byte_instruction("Tuple", offset),
            Opcode::BuildMap => self.byte_instruction("Map", offset),
            Opcode::Index => self.byte_instruction("Index", offset),
            Opcode::Get => self.byte_instruction("Get", offset),
            Opcode::Loop => self.short_instruction("Loop", offset),
            Opcode::Halt => self.simple_instruction("Halt", offset),
        }
    }

    fn simple_instruction(&self, name: &str, offset: usize) -> usize {
        self.write_instruction(name, offset);
        println!();
        offset + 1
    }

    fn byte_instruction(&self, name: &str, offset: usize) -> usize {
        self.write_instruction(name, offset);
        let index = &self.chunk.opcodes[offset + 1];
        let i_padding =
            " ".repeat(self.chunk.constants.len().to_string().len() - index.to_string().len());
        print!("{}{}", i_padding, index);

        self.write_value(*index as usize);

        offset + 2
    }

    fn short_instruction(&self, name: &str, offset: usize) -> usize {
        self.write_instruction(name, offset);
        let index = &self.chunk.opcodes[offset + 1];
        let i_padding =
            " ".repeat(self.chunk.constants.len().to_string().len() - index.to_string().len());
        print!("{}{}", i_padding, index);

        self.write_value(*index as usize);

        offset + 3
    }

    fn closure(&self, offset: usize) -> usize {
        let constant = self.chunk.opcodes[offset + 2];
        println!("{}", constant);

        self.write_value(constant as usize);

        offset + 2
    }

    fn write_instruction(&self, name: &str, offset: usize) {
        let padding =
            " ".repeat(self.chunk.opcodes.len().to_string().len() - offset.to_string().len());
        print!("{}{}: {:<14}", padding, offset, name);
    }

    fn write_value(&self, index: usize) {
        println!(" ; {}", self.chunk.constants[index]);
    }
}
