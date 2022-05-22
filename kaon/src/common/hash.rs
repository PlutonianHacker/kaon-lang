use std::hash::{Hasher, Hash};

/// Marks a function as a method.
pub const METHOD: &str = "method";
/// Marks a function as a static method.
pub const STATIC: &str = "static";
/// Marks a function as a class constructor.
pub const INIT: &str = "init";

/// Calculate a key hash form a given name and modifier.
pub fn calculate_hash(fun_name: &str, modifier: &str) -> u64 {
    let s = &mut ahash::AHasher::default();

    fun_name.hash(s);
    modifier.hash(s);

    s.finish()
}