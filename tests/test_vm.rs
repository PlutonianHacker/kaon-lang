use kaon_lang::common::{ByteCode, Function, Opcode, Value};
use kaon_lang::vm::Vm;

use std::collections::HashMap;
use std::rc::Rc;

fn new_chunk(opcodes: Vec<u8>, constants: Vec<Value>) -> Rc<Function> {
    let chunk = ByteCode {
        opcodes,
        constants,
        identifiers: HashMap::new(),
    };

    Rc::new(Function::new("script".to_string(), 0, chunk, vec![]))
}

#[test]
fn opcode_load() {
    let chunk = new_chunk(
        vec![0_u8, 0_u8, Opcode::Halt as u8],
        vec![Value::Number(567.0)],
    );
    let mut vm = Vm::new();
    vm.interpret(chunk);
    assert_eq!(vm.stack.peek(), Value::Number(567.0));
}

#[test]
fn opcode_add() {
    let chunk = new_chunk(
        vec![0, 0, 0, 1, Opcode::Add as u8, Opcode::Halt as u8],
        vec![Value::Number(1.0), Value::Number(2.0)],
    );
    let mut vm = Vm::new();
    vm.interpret(chunk);
    assert_eq!(vm.stack.peek(), Value::Number(3.0));
}

#[test]
fn opcode_sub() {
    let chunk = new_chunk(
        vec![0, 0, 0, 1, Opcode::Sub as u8, Opcode::Halt as u8],
        vec![Value::Number(2.0), Value::Number(3.0)],
    );
    let mut vm = Vm::new();
    vm.interpret(chunk);
    assert_eq!(vm.stack.peek(), Value::Number(1.0));
}

#[test]
fn opcode_mul() {
    let chunk = new_chunk(
        vec![0, 0, 0, 1, Opcode::Mul as u8, Opcode::Halt as u8],
        vec![Value::Number(2.0), Value::Number(3.0)],
    );
    let mut vm = Vm::new();
    vm.interpret(chunk);
    assert_eq!(vm.stack.peek(), Value::Number(6.0));
}

#[test]
fn opcode_div() {
    let chunk = new_chunk(
        vec![0, 0, 0, 1, Opcode::Div as u8, Opcode::Halt as u8],
        vec![Value::Number(2.0), Value::Number(6.0)],
    );
    let mut vm = Vm::new();
    vm.interpret(chunk);
    assert_eq!(vm.stack.peek(), Value::Number(3.0));
}

#[test]
fn opcode_neg() {
    let chunk = new_chunk(vec![0, 0, Opcode::Negate as u8, Opcode::Halt as u8], vec![Value::Number(2.0)]);
    let mut vm = Vm::new();
    vm.interpret(chunk);
    assert_eq!(vm.stack.peek(), Value::Number(-2.0));
}
