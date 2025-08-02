use std::fmt::Display;

use crate::call::{Call, Class, Function, NativeFunction};

#[derive(Debug, Clone, PartialEq)]
pub enum Object<'src> {
    String(String),
    Function(Function<'src>),
    NativeFunction(NativeFunction),
    Class(Class),
    Instance(Instance),
}

impl<'src> Object<'src> {
    pub fn as_call(&self) -> Option<&dyn Call<'src>> {
        let f: &dyn Call = match self {
            Object::Function(f) => f,
            Object::NativeFunction(f) => f,
            Object::Class(c) => c,
            Object::String(_) | Object::Instance(_) => return None,
        };

        Some(f)
    }
}

impl<'src> Display for Object<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "{s}"),
            Object::Function(fun) => write!(f, "{fun}"),
            Object::NativeFunction(fun) => write!(f, "{fun}"),
            Object::Class(class) => write!(f, "{class}"),
            Object::Instance(instance) => write!(f, "{instance}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    pub class: Class,
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class)
    }
}
