use smallvec::SmallVec;

use crate::Value;

use super::value::ToValue;

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

    pub fn get(&self, name: &str) -> Option<Value> {
        self.names
            .iter()
            .enumerate()
            .find(|(_, n)| &***n == name)
            .and_then(|(pos, _)| self.values.get(pos).cloned())
    }
}

#[cfg(test)]
mod test {
    use crate::Value;

    use super::State;

    #[test]
    fn test_state() {
        let mut state = State::new();

        state.add("x", 23u32);
        state.add("y", true);

        assert_eq!(state.get("x").unwrap(), Value::Number(23.0));
        assert_eq!(state.get("y").unwrap(), Value::Boolean(true));

        assert_eq!(state.get("nothing"), None);

        println!("{:#?}", state);
    }
}
