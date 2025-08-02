use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::obj::Object;

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'src> {
    Nil,
    Bool(bool),
    Number(f64),
    Object(Rc<RefCell<Object<'src>>>),
}

impl<'src> Value<'src> {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }
}

impl<'src> Display for Value<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Value::Nil => "nil".to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Number(n) => n.to_string(),
            Value::Object(o) => o.borrow().to_string(),
        };
        write!(f, "{s}")
    }
}
