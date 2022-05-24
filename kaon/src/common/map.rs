use core::{borrow, fmt};
use std::{
    collections::HashMap,
    hash::{self, BuildHasherDefault},
    rc::Rc,
};

use super::{FromValue, Named, ToValue};
use crate::Value;

pub type Keys<'a> = std::collections::hash_map::Keys<'a, String, Value>;

pub type Values<'a> = std::collections::hash_map::Values<'a, String, Value>;

/// A struct representing a map at runtime.
///
/// # Examples
///
/// ```rust
/// # fn main() -> kaon::Result<()> {
/// let mut map = kaon::common::Map::new();
/// assert_eq!(map.len(), 0);
///
/// map.insert_value::<i32>(String::from("x"), 32);
/// map.insert_value::<&str>(String::from("y"), "hello");
/// assert_eq!(map.len(), 2);
///
/// assert_eq!(Some(32), map.get_value("x"));
/// assert_eq!(Some(String::from("hello")), map.get_value("y"));
///
/// # Ok(())
/// # }
/// ```
#[derive(Default, Clone)]
pub struct Map {
    inner: Rc<HashMap<String, Value, BuildHasherDefault<ahash::AHasher>>>,
}

impl Map {
    /// Create a new empty [`Map`].
    pub fn new() -> Self {
        Self {
            inner: Rc::new(HashMap::default()),
        }
    }

    /// Create a new empty [`Map`] with the specified capacity.
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            inner: Rc::new(HashMap::with_capacity_and_hasher(
                cap,
                BuildHasherDefault::default(),
            )),
        }
    }

    pub fn make_mut(&mut self) -> &mut HashMap<String, Value, BuildHasherDefault<ahash::AHasher>> {
        Rc::make_mut(&mut self.inner)
    }

    /// Return a reference to the value corresponding to the key.
    pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&Value>
    where
        String: borrow::Borrow<Q>,
        Q: hash::Hash + std::cmp::Eq + std::cmp::Ord,
    {
        self.inner.get(k)
    }

    /// Return a mutable reference to the value corresponding to the key
    pub fn get_mut<Q: ?Sized>(&mut self, k: &Q) -> Option<&mut Value>
    where
        String: borrow::Borrow<Q>,
        Q: hash::Hash + std::cmp::Eq + std::cmp::Ord,
    {
        self.make_mut().get_mut(k)
    }

    /// Return a reference to the value corresponding to the key, converting the
    /// value as needed with the [`FromValue`] trait.
    pub fn get_value<Q: ?Sized, T>(&self, k: &Q) -> Option<T>
    where
        String: borrow::Borrow<Q>,
        Q: hash::Hash + std::cmp::Eq + std::cmp::Ord,
        T: FromValue,
    {
        let v = match self.inner.get(k) {
            Some(v) => v.clone(),
            None => return None,
        };

        Some(T::from_value(v).unwrap())
    }

    /// Insert a key-value pair into the [`Map`].
    pub fn insert(&mut self, k: String, v: Value) {
        self.make_mut().insert(k, v);
    }

    /// Insert a key-value pair into the map, converting the value
    /// using the [`ToValue`] trait.
    pub fn insert_value<V>(&mut self, k: String, v: V)
    where
        V: ToValue,
    {
        self.make_mut().insert(k, v.to_value());
    }

    /// Removes a key from the [`Map`], returning the value at the key.
    pub fn remove<Q: ?Sized>(&mut self, k: &Q) -> Option<Value>
    where
        String: borrow::Borrow<Q>,
        Q: hash::Hash + std::cmp::Eq + std::cmp::Ord,
    {
        self.make_mut().remove(k)
    }

    /// Return the number of key-value pairs in the [`Map`].
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Returns `true` if the inner [`HashMap`] is empty, otherwise `false`.
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// An iterator visiting all keys in an arbitrary order.
    pub fn keys(&self) -> Keys<'_> {
        self.inner.keys()
    }

    /// An iterator visiting all values in an arbitrary order.
    pub fn values(&self) -> Values<'_> {
        self.inner.values()
    }

    pub fn contains_key<Q: ?Sized>(&self, k: &Q) -> bool
    where
        String: borrow::Borrow<Q>,
        Q: hash::Hash + std::cmp::Eq + std::cmp::Ord,
    {
        self.inner.contains_key(k)
    }
}

impl FromValue for Map {
    fn from_value(value: Value) -> Result<Self, String> {
        match value {
            Value::Map(map) => Ok(map),
            _ => {
                Err("cannot coerce map from value".into())
            }
        }
    }
}

impl ToValue for Map {
    fn to_value(self) -> Value {
        Value::Map(self)
    }
}

impl fmt::Debug for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.inner.iter()).finish()
    }
}

impl fmt::Display for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("{")?;

        for (pos, (key, value)) in self.inner.iter().enumerate() {
            f.write_fmt(format_args!("{key}: {value}"))?;

            if pos != self.inner.len() {
                f.write_str(", ")?;
            }
        }
        f.write_str("}")
    }
}

impl PartialEq for Map {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.inner == other.inner
    }
}

impl PartialOrd for Map {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.values().partial_cmp(other.values())
    }
}

impl Named for Map {
    const NAME: &'static str = "Map";
}
