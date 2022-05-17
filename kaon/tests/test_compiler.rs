use kaon::{
    common::{Opcode, Value},
    Kaon,
};

#[test]
fn test_compiler() {
    let src = r#"
        1 + 2
        5 * (10 - 3)
        -4 - -8
    "#;
    let mut kaon = Kaon::new();

    assert_eq!(kaon.compile(src).is_ok(), true);
}

#[test]
fn compile_number() {
    let mut kaon = Kaon::new();
    let function = kaon.compile("123").unwrap();
    assert_eq!(
        function.chunk.opcodes,
        vec![
            Opcode::Const as u8,
            0,
            Opcode::Del as u8,
            Opcode::Halt as u8
        ]
    );
    assert_eq!(*function.chunk.constants[0], Value::Number(123.0));
}

#[test]
fn compile_binary() {
    let mut kaon = Kaon::new();
    let function = kaon.compile("1 + 2").unwrap();
    assert_eq!(
        function.chunk.opcodes,
        vec![
            Opcode::Const as u8,
            0,
            Opcode::Const as u8,
            1,
            Opcode::Add as u8,
            Opcode::Del as u8,
            Opcode::Halt as u8,
        ]
    );
    assert_eq!(
        function.chunk.constants,
        vec![Box::new(Value::Number(2.0)), Box::new(Value::Number(1.0))]
    );
}

#[test]
fn compile_unary() {
    let mut kaon = Kaon::new();
    let function = kaon.compile("-7").unwrap();
    assert_eq!(
        function.chunk.opcodes,
        vec![
            Opcode::Const as u8,
            0,
            Opcode::Negate as u8,
            Opcode::Del as u8,
            Opcode::Halt as u8,
        ]
    );
    assert_eq!(function.chunk.constants, vec![Box::new(Value::Number(7.0))]);
}
