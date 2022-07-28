use std::rc::Rc;
use crate::common::Class;

pub(crate) fn make_class() -> Rc<Class> {
    let class = Class::new("Float");  

    class.register_method("max", f64::max);
    class.register_method("sqrt", f64::sqrt);
    class.register_method("ceil", f64::ceil);
    class.register_method("floor", f64::floor);
    class.register_method("round", f64::round);
    class.register_method("pow", f64::powf);
    class.register_method("abs", f64::abs);
    class.register_method("tan", f64::tan);
    class.register_method("sin", f64::sin);
    class.register_method("cos", f64::cos);
    class.register_method("atan", f64::atan);
    class.register_method("asin", f64::asin);
    class.register_method("acos", f64::acos);
    class.register_method("clamp", f64::clamp);
    class.register_method("to_radians", f64::to_radians);

    class
} 