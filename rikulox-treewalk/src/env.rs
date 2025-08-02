use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{error::RuntimeErrorKind, value::Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Environment<'src> {
    enclosing: Option<Rc<RefCell<Environment<'src>>>>,
    values: HashMap<&'src str, Value<'src>>,
}

impl<'src> Environment<'src> {
    pub fn new() -> Self {
        Environment {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    pub fn with_enclosing(enclosing: Rc<RefCell<Environment<'src>>>) -> Self {
        Environment {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &'src str, value: Value<'src>) {
        self.values.insert(name, value);
    }

    pub fn assign(
        &mut self,
        name: &str,
        value: Value<'src>,
    ) -> Result<(), RuntimeErrorKind<'src>> {
        match (self.values.get_mut(name), &self.enclosing) {
            (Some(var), _) => {
                *var = value;
                Ok(())
            }
            (None, Some(enclosing)) => {
                enclosing.borrow_mut().assign(name, value)
            }
            (None, None) => {
                Err(RuntimeErrorKind::UndefinedVariable(name.to_string()))
            }
        }
    }

    pub fn get(
        &self,
        name: &str,
    ) -> Result<Value<'src>, RuntimeErrorKind<'src>> {
        match (self.values.get(name), &self.enclosing) {
            (Some(var), _) => Ok(var.clone()),
            (None, Some(enclosing)) => enclosing.borrow().get(name),
            (None, None) => {
                Err(RuntimeErrorKind::UndefinedVariable(name.to_string()))
            }
        }
    }
}

impl<'src> Default for Environment<'src> {
    fn default() -> Self {
        Self::new()
    }
}
