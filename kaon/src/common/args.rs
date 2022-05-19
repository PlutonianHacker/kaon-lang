use crate::{common::value::ToValue, common::value::Value};

pub type Result<T> = std::result::Result<T, String>;

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
    use super::*;
    use super::ToArgs;

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
}
