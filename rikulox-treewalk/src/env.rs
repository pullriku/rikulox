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
        name: &'src str,
    ) -> Result<Value<'src>, RuntimeErrorKind<'src>> {
        match (self.values.get(name), &self.enclosing) {
            (Some(var), _) => Ok(var.clone()),
            (None, Some(enclosing)) => enclosing.borrow().get(name),
            (None, None) => {
                Err(RuntimeErrorKind::UndefinedVariable(name.to_string()))
            }
        }
    }

    pub fn get_at(&self, name: &'src str, distance: usize) -> Option<Value<'src>> {
        self.ancestor(distance)?.borrow().values.get(name).cloned()
    }

    pub fn assign_at(&mut self, name: &'src str, value: Value<'src>, distance: usize) -> Result<(), RuntimeErrorKind<'src>> {
        self.ancestor(distance).ok_or(RuntimeErrorKind::UndefinedVariable(name.to_string()))?.borrow_mut().values.insert(name, value);

        Ok(())
    }

    fn ancestor(&self, distance: usize) -> Option<Rc<RefCell<Environment<'src>>>> {
        if distance == 0 {
            return Some(Rc::new(RefCell::new(self.clone())));
        }

        let mut environment: Rc<RefCell<Environment>> = Rc::clone(self.enclosing.as_ref().unwrap());
        for _ in 1..distance {
            if let Some(enclosing) = environment.clone().borrow().enclosing.as_ref() {
                environment = Rc::clone(enclosing);
            } else {
                return None;
            }
        }

        Some(environment)
    }
}

impl<'src> Default for Environment<'src> {
    fn default() -> Self {
        Self::new()
    }
}
