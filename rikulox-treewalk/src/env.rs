use std::{cell::RefCell, collections::HashMap, rc::Rc};

use rikulox_runtime::{error::RuntimeErrorKind, obj::Object};

#[derive(Debug, Clone)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    pub fn with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Self {
        Environment {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Object) {
        self.values.insert(name, value);
    }

    pub fn assign(&mut self, name: &str, value: Object) -> Result<(), RuntimeErrorKind> {
        match (self.values.get_mut(name), &self.enclosing) {
            (Some(var), _) => {
                *var = value;
                Ok(())
            }
            (None, Some(enclosing)) => enclosing.borrow_mut().assign(name, value),
            (None, None) => Err(RuntimeErrorKind::UndefinedVariable(name.to_string())),
        }
    }

    pub fn get(&self, name: &str) -> Result<Object, RuntimeErrorKind> {
        match (self.values.get(name), &self.enclosing) {
            (Some(var), _) => Ok(var.clone()),
            (None, Some(enclosing)) => enclosing.borrow().get(name),
            (None, None) => Err(RuntimeErrorKind::UndefinedVariable(name.to_string())),
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}
