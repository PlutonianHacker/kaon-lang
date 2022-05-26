use std::rc::Rc;

use smallvec::SmallVec;

use crate::Value;

use super::{value::{ToValue, RegisterFunction}, NativeFun, FromValue};

#[derive(Debug, Default)]
pub struct State {
    pub values: SmallVec<[Value; 8]>,
    pub names: SmallVec<[Box<str>; 8]>,
}

impl State {
    pub fn new() -> Self {
        Self {
            values: SmallVec::new(),
            names: SmallVec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty() && self.names.is_empty()
    }

    pub fn add<T: ToValue>(&mut self, name: &str, value: T) {
        self.names.push(name.into());
        self.values.push(value.to_value());
    }

    pub fn get<T: FromValue>(&self, name: &str) -> Option<T> {
        let value = self.names
            .iter()
            .enumerate()
            .find(|(_, n)| &***n == name)
            .and_then(|(pos, _)| self.values.get(pos).cloned());
        
        if let Some(value) = value {
            Some(FromValue::from_value(value).unwrap())
        } else {
            None
        }
    }

    pub fn register_function<S: Into<Box<str>> + Copy, A, R, F: RegisterFunction<A, R> + Copy>(&mut self, name: S, fun: F) {
        let fun = NativeFun::new(name, fun.arity(), fun.to_native_function(), fun.is_varidic());   

        self.names.push(name.into());
        self.values.push(Value::NativeFun(Rc::new(fun)));
    }
}

#[cfg(test)]
mod test {
    use super::State;

    #[test]
    fn test_state() {
        let mut state = State::new();

        state.add("x", 23u32);
        state.add("y", true);

        assert_eq!(state.get::<u32>("x").unwrap(), 23);
        assert_eq!(state.get::<bool>("y").unwrap(), true);

        assert_eq!(state.get::<String>("nothing"), None);

        println!("{:#?}", state);
    }
}
