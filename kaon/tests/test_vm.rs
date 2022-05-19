use kaon::common::{Chunk, Function, Opcode, Value};
use kaon::runtime::Vm;

use std::rc::Rc;

fn new_chunk(opcodes: Vec<u8>, constants: Vec<Value>) -> Rc<Function> {
    let chunk = Chunk {
        opcodes,
        constants: constants
            .iter()
            .map(|v| Box::new(v.clone()))
            .collect::<Vec<Box<Value>>>(),
        ..Default::default()
    };

    Rc::new(Function::new("script".to_string(), 0, chunk, vec![]))
}

#[test]
fn opcode_load() {
    let chunk = new_chunk(
        vec![0_u8, 0_u8, Opcode::Halt as u8],
        vec![Value::Float(567.0)],
    );
    let mut vm = Vm::new();
    vm.execute(chunk).unwrap();
    assert_eq!(vm.stack.peek(), Value::Float(567.0));
}

#[test]
fn opcode_add() {
    let chunk = new_chunk(
        vec![0, 0, 0, 1, Opcode::Add as u8, Opcode::Halt as u8],
        vec![Value::Float(1.0), Value::Float(2.0)],
    );
    let mut vm = Vm::new();
    vm.execute(chunk).unwrap();
    assert_eq!(vm.stack.peek(), Value::Float(3.0));
}

#[test]
fn opcode_sub() {
    let chunk = new_chunk(
        vec![0, 0, 0, 1, Opcode::Sub as u8, Opcode::Halt as u8],
        vec![Value::Float(2.0), Value::Float(3.0)],
    );
    let mut vm = Vm::new();
    vm.execute(chunk).unwrap();
    assert_eq!(vm.stack.peek(), Value::Float(1.0));
}

#[test]
fn opcode_mul() {
    let chunk = new_chunk(
        vec![0, 0, 0, 1, Opcode::Mul as u8, Opcode::Halt as u8],
        vec![Value::Float(2.0), Value::Float(3.0)],
    );
    let mut vm = Vm::new();
    vm.execute(chunk).unwrap();
    assert_eq!(vm.stack.peek(), Value::Float(6.0));
}

#[test]
fn opcode_div() {
    let chunk = new_chunk(
        vec![0, 0, 0, 1, Opcode::Div as u8, Opcode::Halt as u8],
        vec![Value::Float(2.0), Value::Float(6.0)],
    );
    let mut vm = Vm::new();
    vm.execute(chunk).unwrap();
    assert_eq!(vm.stack.peek(), Value::Float(3.0));
}

#[test]
fn opcode_neg() {
    let chunk = new_chunk(
        vec![0, 0, Opcode::Negate as u8, Opcode::Halt as u8],
        vec![Value::Float(2.0)],
    );
    let mut vm = Vm::new();
    vm.execute(chunk).unwrap();
    assert_eq!(vm.stack.peek(), Value::Float(-2.0));
}
