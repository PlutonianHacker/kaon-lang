use kaon::{Kaon, Value};

#[test]
fn test() {
    let mut kaon = Kaon::new();
    let state = kaon.globals();

    state.add("x", 12u32);
    state.add("y", true);

    assert_eq!(state.get("x").unwrap(), Value::Number(12.0));
    assert_eq!(state.get("y").unwrap(), Value::Boolean(true));
}