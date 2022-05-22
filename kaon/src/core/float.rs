use std::rc::Rc;

use crate::common::Class;

fn sqrt(f: &mut f64) -> f64 {
    f.sqrt()
}

fn floor(f: &mut f64) -> f64 {
    f.floor()
}

fn ceil(f: &mut f64) -> f64 {
    f.ceil()
}

fn max(f: &mut f64, other: f64) -> f64 {
    f.max(other)
}

fn pow(f: &mut f64, n: f64) -> f64 {
    f.powf(n)
}

fn abs(f: &mut f64) -> f64 {
    f.abs()
}

fn round(f: &mut f64) -> f64 {
    f.round()
}

fn sin(f: &mut f64) -> f64 {
    f.sin()
}

fn tan(f: &mut f64) -> f64 {
    f.tan()
}

fn cos(f: &mut f64) -> f64 {
    f.cos()
}

fn asin(f: &mut f64) -> f64 {
    f.asin()
}

fn atan(f: &mut f64) -> f64 {
    f.atan()
}

fn acos(f: &mut f64) -> f64 {
    f.acos()
}

fn to_radians(f: &mut f64) -> f64 {
    f.to_radians()
}

pub(crate) fn make_class() -> Rc<Class> {
    let class = Class::new("Float");  

    class.register_method("max", max);
    class.register_method("sqrt", sqrt);
    class.register_method("ceil", ceil);
    class.register_method("floor", floor);
    class.register_method("round", round);
    class.register_method("pow", pow);
    class.register_method("abs", abs);
    class.register_method("tan", tan);
    class.register_method("sin", sin);
    class.register_method("cos", cos);
    class.register_method("atan", atan);
    class.register_method("asin", asin);
    class.register_method("acos", acos);
    class.register_method("to_radians", to_radians);

    class
} 