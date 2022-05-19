use std::rc::Rc;

use crate::{
    common::{NativeFun, Value, ValueMap, ImmutableString, Class},
    runtime::Vm,
};

fn length(vm: &mut Vm, _args: Vec<Value>) -> Value {
    vm.stack.pop();
    match vm.stack.peek() {
        Value::String(val) => Value::Float(val.len() as f64),
        _ => panic!("expected a string"),
    }
}

fn format_str(_vm: &mut Vm, args: Vec<Value>) -> Value {
    match &args[0] {
        Value::String(string) => {
            let slices = string.split("{}");
            let mut result = "".to_string();
            for (pos, slice) in slices.enumerate() {
                result += &(slice.to_string() + &args[pos + 1].to_string());
            }

            Value::String(ImmutableString::from(result))
        }
        _ => panic!("expected a string"),
    }
}

pub fn str(_vm: &mut Vm, args: Vec<Value>) -> Value {
    Value::String(ImmutableString::from(format!("{}", args[0])))
}

pub fn make_module() -> ValueMap {
    let mut string = ValueMap::new();

   // string.insert_fun("len", NativeFun::new("len", 0, length));
    //string.insert_fun("format", NativeFun::varidic("format", 1, format_str));

    string
}

fn init_string(_str: &mut ImmutableString, raw_str: String) -> ImmutableString {
    ImmutableString::from(raw_str)
} 

fn len(str: &mut ImmutableString) -> usize {
    str.len()
}

fn is_empty(str: &mut ImmutableString) -> bool {
    str.is_empty()
}

fn contains(str: &mut ImmutableString, other: String) -> bool {
    let s = str.clone().into_owned();

    s.contains(&other)
}

pub fn make_class() -> Rc<Class> {
    let class = Class::new("String");

    class.register_init("new", init_string);

    class.register_method("len", len);
    class.register_method("is_empty", is_empty);
    class.register_method("contains", contains);

    class
}