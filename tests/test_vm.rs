use kaon_lang::common::{ByteCode, Data};
use kaon_lang::vm::Vm;

fn new_chunk(opcodes: Vec<u8>, constants: Vec<Data>) -> ByteCode {
    ByteCode {
        opcodes,
        constants,
    }
}

#[test]
fn opcode_load() {
    let chunk = new_chunk(vec![0, 0, 28], vec![Data::Number(567.0)]);
    let mut vm = Vm::new();
    vm.interpret(chunk);
    assert_eq!(vm.stack.peek(), Data::Number(567.0));
}

#[test]
fn opcode_add() {
    let chunk = new_chunk(vec![0, 0, 0, 1, 1, 28], vec![Data::Number(1.0), Data::Number(2.0)]);
    let mut vm = Vm::new();
    vm.interpret(chunk);
    assert_eq!(vm.stack.peek(), Data::Number(3.0));
}

#[test]
fn opcode_sub() {
    let chunk = new_chunk(vec![0, 0, 0, 1, 2, 28], vec![Data::Number(2.0), Data::Number(3.0)]);
    let mut vm = Vm::new();
    vm.interpret(chunk);
    assert_eq!(vm.stack.peek(), Data::Number(1.0));
}

#[test]
fn opcode_mul() {
    let chunk = new_chunk(vec![0, 0, 0, 1, 3, 28], vec![Data::Number(2.0), Data::Number(3.0)]);
    let mut vm = Vm::new();
    vm.interpret(chunk);
    assert_eq!(vm.stack.peek(), Data::Number(6.0));
}

#[test]
fn opcode_div() {
    let chunk = new_chunk(vec![0, 0, 0, 1, 4, 28], vec![Data::Number(2.0), Data::Number(6.0)]);
    let mut vm = Vm::new();
    vm.interpret(chunk);
    assert_eq!(vm.stack.peek(), Data::Number(3.0));
}

#[test]
fn opcode_neg() {
    let chunk = new_chunk(vec![0, 0, 6, 28], vec![Data::Number(2.0)]);
    let mut vm = Vm::new();
    vm.interpret(chunk);
    assert_eq!(vm.stack.peek(), Data::Number(-2.0));
}