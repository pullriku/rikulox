use rikulox_gc::{gc::Heap, list::EntryRef};

use crate::obj::Object;

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'src> {
    Nil,
    Bool(bool),
    Number(f64),
    Object(EntryRef<Object<'src>>),
}

impl<'src> Value<'src> {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }

    pub fn to_string_with(&self, heap: &Heap<Object<'src>>) -> String {
        match self {
            Value::Nil => "nil".to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Number(n) => n.to_string(),
            Value::Object(o) => {
                heap.get(*o).expect("Object not found").to_string()
            }
        }
    }
}
