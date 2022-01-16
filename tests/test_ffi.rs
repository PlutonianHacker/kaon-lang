use kaon_lang::common::Data;
use kaon_lang::core;

#[test]
fn test_io() {
    let mut ffi = core::ffi_core();

    let fun = ffi.get("to_string").unwrap();
    assert_eq!(
        fun.0(vec![Data::Number(123_f64)]),
        Data::String("123".to_string())
    );
}

#[test]
fn test_time() {
    let mut ffi = core::ffi_core();

    let clock = ffi.get("clock").unwrap();
    let start = clock.0(vec![]);
    while clock.0(vec![]) - start.clone() != Data::Number(1.0) {}

    let end = clock.0(vec![]) - start;
    if let Data::Number(val) = end {
        assert_eq!(val.round(), 1.0);
    }
}

#[test]
fn test_math() {
    let mut ffi = core::ffi_core();

    let sqrt = ffi.get("sqrt").unwrap();
    assert_eq!(sqrt.0(vec![Data::Number(25.0)]), Data::Number(5.0));

    let pow = ffi.get("pow").unwrap();
    assert_eq!(
        pow.0(vec![Data::Number(5.0), Data::Number(2.0)]),
        Data::Number(25.0)
    );

    let abs = ffi.get("abs").unwrap();
    assert_eq!(abs.0(vec![Data::Number(-2.0)]), Data::Number(2.0));

    let sin = ffi.get("sin").unwrap();
    let x = 45.0;
    assert_eq!(sin.0(vec![Data::Number(x)]), Data::Number(x.sin()));

    let tan = ffi.get("tan").unwrap();
    let x = 45.0;
    assert_eq!(tan.0(vec![Data::Number(x)]), Data::Number(x.tan()));

    let cos = ffi.get("cos").unwrap();
    let x = 45.0;
    assert_eq!(cos.0(vec![Data::Number(x)]), Data::Number(x.cos()));

    let asin = ffi.get("asin").unwrap();
    let x = -1.0;
    assert_eq!(asin.0(vec![Data::Number(x)]), Data::Number(x.asin()));

    let atan = ffi.get("atan").unwrap();
    let x = -1.0;
    assert_eq!(atan.0(vec![Data::Number(x)]), Data::Number(x.atan()));

    let acos = ffi.get("acos").unwrap();
    let x = -1.0;
    assert_eq!(acos.0(vec![Data::Number(x)]), Data::Number(x.acos()));

    let round = ffi.get("round").unwrap();
    assert_eq!(round.0(vec![Data::Number(99.99)]), Data::Number(100.0));

    let to_radians = ffi.get("to_radians").unwrap();
    assert_eq!(
        to_radians.0(vec![Data::Number(90.0)]),
        Data::Number((90_f64).to_radians())
    );
}
