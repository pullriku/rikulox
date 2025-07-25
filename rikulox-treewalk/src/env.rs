use std::collections::HashMap;

use rikulox_runtime::obj::Object;

#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Object) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        self.values.get(name)
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}
