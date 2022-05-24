use std::{
    fmt::{self, Display},
    ops::{Add, AddAssign, Deref},
    rc::Rc,
};

use super::Named;

/// An immutable string type.
///
/// An [ImmutableString] wraps a `Rc<String>` (or `Arc<String>` when Send/Sync is added in the future)
/// to ensure the string isn't cloned when unnessary.
///
/// # Example
/// ``` rust
/// use kaon::common::ImmutableString;
///
/// let s1: ImmutableString = "Hello".into();
///
/// // Cloning here doesn't actually do anything
/// let s2 = s1.clone();
/// let s3 = s1.clone();
/// 
/// // Consumes the ImmutableString and returns the underlying string
/// let mut s: String = s1.into_owned();
/// 
/// // Changing the sting won't effect the clones
/// s.push_str(", World!");
/// 
/// assert_ne!(s, s2.as_str());
/// assert_eq!(s, "Hello, World!");
///
/// assert_eq!(s2, s3);
/// ```
#[derive(Debug, Clone, Default, Hash, PartialOrd, Ord, Eq)]
pub struct ImmutableString(Rc<String>);

impl ImmutableString {
    /// Create a new [ImmutableString].
    pub fn new() -> Self {
        Self(Rc::new(String::new()))
    }

    /// Returns `true` if this [ImmutableString] has a length of zero, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns the length of this [ImmutableString].
    pub fn length(&self) -> usize {
        self.0.len()
    }

    /// Makes a mutable reference from this [ImmutableString].
    pub fn make_mut(&mut self) -> &mut String {
        Rc::make_mut(&mut self.0)
    }

    /// Consumes the [ImmutableString] and returns the inner string.
    /// 
    /// If there are more than one strong reference, the inner value is cloned instead.
    pub fn into_owned(mut self) -> String {
        self.make_mut();

        Rc::try_unwrap(self.0).unwrap_or_else(|v| v.as_ref().to_string())
    }
}

impl From<String> for ImmutableString {
    fn from(str: String) -> Self {
        Self(Rc::new(str))
    }
}

impl From<&String> for ImmutableString {
    fn from(str: &String) -> Self {
        Self(Rc::new(str.to_string()))
    }
}

impl From<&str> for ImmutableString {
    fn from(str: &str) -> Self {
        Self(Rc::new(str.to_string()))
    }
}

impl From<&mut str> for ImmutableString {
    fn from(str: &mut str) -> Self {
        Self(Rc::new(str.to_string()))
    }
}

impl From<Box<str>> for ImmutableString {
    fn from(str: Box<str>) -> Self {
        Self(Rc::new(str.to_string()))
    }
}

impl Add<&str> for ImmutableString {
    type Output = Self;

    fn add(mut self, rhs: &str) -> Self::Output {
        if rhs.is_empty() {
            self
        } else if self.is_empty() {
            Self::from(rhs)
        } else {
            self.make_mut().push_str(rhs);

            self
        }
    }
}

impl Add<&ImmutableString> for ImmutableString {
    type Output = Self;

    fn add(mut self, rhs: &ImmutableString) -> Self::Output {
        if rhs.is_empty() {
            self
        } else if self.is_empty() {
            rhs.clone()
        } else {
            self.make_mut().push_str(rhs.0.as_str());

            self
        }
    }
}

impl Add<ImmutableString> for ImmutableString {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        if rhs.is_empty() {
            self
        } else if self.is_empty() {
            rhs
        } else {
            self.make_mut().push_str(rhs.0.as_str());

            self
        }
    }
}

impl AddAssign<&str> for ImmutableString {
    fn add_assign(&mut self, rhs: &str) {
        self.make_mut().push_str(rhs);
    }
}

impl AddAssign<&ImmutableString> for ImmutableString {
    fn add_assign(&mut self, rhs: &ImmutableString) {
        self.make_mut().push_str(rhs);
    }
}

impl PartialEq for ImmutableString {
    fn eq(&self, other: &Self) -> bool {
        self.0.len() == other.0.len() && self.0 == other.0
    }
}

impl Deref for ImmutableString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl Display for ImmutableString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

impl Named for ImmutableString {
    const NAME: &'static str = "String";    
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_add() {
        let str = ImmutableString::from("Hello, ");

        assert_eq!(str + "World!", ImmutableString::from("Hello, World!"));
    }

    #[test]
    fn test_add_assign() {
        let mut str = ImmutableString::from("ab");
        str += "c";

        assert_eq!(*str, "abc");
    }

    #[test]
    fn test_deref() {
        let str = ImmutableString::from("Hello");

        assert_eq!(*str, "Hello");
    }

    #[test]
    fn test_immutable_str() {
        let s1: ImmutableString = "Hello, World".into();

        // Cloning here doesn't actually do anything.
        let s2 = s1.clone();
        let s3 = s1.clone();

        assert_eq!(s1, s2);

        let mut s: String = s1.into_owned();

        s.push_str("!");

        assert_eq!(s3, s2);

        assert_eq!(s, "Hello, World!");
    }
}
