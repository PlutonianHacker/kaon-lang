use crate::{common::value::ToValue, common::value::Value};

use super::FromValue;

pub type Result<T> = std::result::Result<T, String>;

/// Represents a varidic arg type.
///
/// When used in rust functions, this must be the last parameter
/// passed into the function.
pub struct Varidic<T>(Vec<T>);

impl<T: FromValue> Varidic<T> {
    pub fn new_from_iter<V: FromValue>(iter: std::slice::Iter<'_, Value>) -> Self {
        let mut v = Vec::<T>::new();

        for i in iter {
            v.push(FromValue::from_value(i).unwrap());
        }

        Varidic(v)
    }
}

impl<T> From<Vec<T>> for Varidic<T> {
    fn from(v: Vec<T>) -> Self {
        Varidic(v)
    }
}

impl<T: Clone> From<&[T]> for Varidic<T> {
    fn from(v: &[T]) -> Self {
        Varidic(v.to_vec())
    }
}

impl<T: Clone> From<Box<[T]>> for Varidic<T> {
    fn from(v: Box<[T]>) -> Self {
        Varidic(v.to_vec())
    }
}

impl<T: Clone> From<std::slice::Iter<'_, T>> for Varidic<T> {
    fn from(iter: std::slice::Iter<'_, T>) -> Self {
        let mut v = Vec::<T>::new();

        for i in iter {
            v.push(i.clone())
        }

        Varidic(v)
    }
}

impl<T> IntoIterator for Varidic<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

macro_rules! impl_varidic_args {
    ($num:literal) => {
        impl<T: Clone> From<[T; $num]> for Varidic<T> {
            fn from(v: [T; $num]) -> Self {
                Self(v.to_vec())
            }
        }

        impl<T: Clone> From<&[T; $num]> for Varidic<T> {
            fn from(v: &[T; $num]) -> Self {
                Self(v.to_vec())
            }
        }
    };
    ($num:literal $($nums:literal)*) => {
        impl_varidic_args!($($nums)*);

        impl<T: Clone> From<[T; $num]> for Varidic<T> {
            fn from(v: [T; $num]) -> Self {
                Self(v.to_vec())
            }
        }

        impl<T: Clone> From<&[T; $num]> for Varidic<T> {
            fn from(v: &[T; $num]) -> Self {
                Self(v.to_vec())
            }
        }
    };
}

impl_varidic_args!(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16);

/// A struct used to represent a function's arguments.
#[derive(Debug, PartialEq)]
pub struct Args(pub Vec<Value>);

impl Args {
    pub fn new() -> Self {
        Args(Vec::new())
    }

    pub fn from_vec(values: Vec<Value>) -> Self {
        Args(values)
    }

    pub fn push(&mut self, value: Value) {
        self.0.push(value);
    }
}

/// Trait for converting an arbitrary type into a vec of thrush values.
pub trait ToArgs {
    fn to_args(self) -> Result<Args>;
}

/// Trait for turning a vec of thrush values into some other type.
pub trait FromArgs {
    fn from_args(values: Args) -> Result<Self>
    where
        Self: Sized;
}

impl<T> ToArgs for T
where
    T: ToValue,
{
    fn to_args(self) -> Result<Args> {
        Ok(Args::from_vec(vec![self.to_value()]))
    }
}

impl ToArgs for Args {
    fn to_args(self) -> Result<Args> {
        Ok(self)
    }
}

macro_rules! impl_tuple {
    ($($name:ident)*) => {
        impl<$($name,)*> ToArgs for ($($name,)*) where $($name: ToValue,)* {
            #[allow(non_snake_case)]
            fn to_args(self) -> Result<Args> {
                let ($($name,)*) = self;

                Ok(Args::from_vec(vec![$($name.to_value(),)*]))
            }
        }
    };
}

impl_tuple!(A);
impl_tuple!(A B);
impl_tuple!(A B C);
impl_tuple!(A B C D);
impl_tuple!(A B C D E);
impl_tuple!(A B C D E F);
impl_tuple!(A B C D E F G);
impl_tuple!(A B C D E F G H);
impl_tuple!(A B C D E F G H I);
impl_tuple!(A B C D E F G H I J);
impl_tuple!(A B C D E F G H I J K);
impl_tuple!(A B C D E F G H I J K L);
impl_tuple!(A B C D E F G H I J K L M);
impl_tuple!(A B C D E F G H I J K L M N);
impl_tuple!(A B C D E F G H I J K L M N O);
impl_tuple!(A B C D E F G H I J K L M N O P);

#[cfg(test)]
mod test {
    use super::ToArgs;
    use super::*;

    #[test]
    fn test_to_args() -> Result<()> {
        assert_eq!((1f32).to_args()?, Args::from_vec(vec![Value::Float(1.0)]));
        assert_eq!(
            (1f32, "Hello").to_args()?,
            Args::from_vec(vec![Value::Float(1.0), Value::String("Hello".into())])
        );
        assert_eq!(
            (1f32, "Hello", true).to_args()?,
            Args::from_vec(vec![
                Value::Float(1.0),
                Value::String("Hello".into()),
                Value::TRUE
            ])
        );

        Ok(())
    }

    #[test]
    fn test_varidic_args() -> Result<()> {
        fn test(args: Varidic<i32>) -> i32 {
            let mut sum = 0;
            for arg in args {
                sum += arg;
            }

            sum
        }

        assert_eq!(15, test(Varidic::from([1, 2, 3, 4, 5])));
        assert_eq!(15, test(Varidic::from(vec![1, 2, 3, 4, 5])));
        assert_eq!(15, test(Varidic::from(vec![1, 2, 3, 4, 5].iter())));

        Ok(())
    }

    #[test]
    fn test() {
        struct DefaultArg<T>(Option<T>);

        impl<T: Clone> DefaultArg<T> {
            fn or(&self, default: T) -> T {
                if let Some(t) = &self.0 {
                    t.clone()
                } else {
                    default
                }
            }
        }

        fn hello(x: DefaultArg<i32>, y: DefaultArg<i32>) -> i32 {
            let x = x.or(30);
            let y = y.or(2);

            x + y
        }

        assert_eq!(8, hello(DefaultArg(Some(5)), DefaultArg(Some(3))));
        assert_eq!(12, hello(DefaultArg(Some(10)), DefaultArg(None)));
        assert_eq!(32, hello(DefaultArg(None), DefaultArg(None)));
    }
}
