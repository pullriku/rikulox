use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    call::{Class, Function, NativeFunction},
    value::Value,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Object<'src> {
    String(String),
    Function(Function<'src>),
    NativeFunction(NativeFunction),
    Class(Class<'src>),
    Instance(Instance<'src>),
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
pub struct Instance<'src> {
    pub class: Rc<RefCell<Object<'src>>>,
    pub fields: HashMap<String, Value<'src>>,
}

impl<'src> Instance<'src> {
    pub fn get(
        &self,
        name: &str,
        self_ref: Value<'src>,
    ) -> Option<Value<'src>> {
        let Object::Class(class) = &*self.class.borrow() else {
            unreachable!()
        };
        let result = self.fields.get(name);
        if result.is_none() {
            class.find_method(name).cloned().map(|mut m| {
                m.bind(self_ref.clone());
                Value::Object(Rc::new(RefCell::new(Object::Function(m))))
            })
        } else {
            result.cloned()
        }
    }

    pub fn set(&mut self, name: String, value: Value<'src>) {
        self.fields.insert(name, value);
    }
}

impl<'src> Display for Instance<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class.borrow())
    }
}
