use kaon::Kaon;

#[test]
fn test() {
    let mut kaon = Kaon::new();
    let state = kaon.globals();

    state.add("x", 12.0_f64);
    state.add("y", true);

    assert_eq!(state.get::<f64>("x").unwrap(), 12.0);
    assert_eq!(state.get::<bool>("y").unwrap(), true);
}