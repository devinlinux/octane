use std::collections::HashMap;
use super::Object;

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Environment {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }

    pub fn get(&self, name: String) -> Option<&Object> {
        match self.store.get(&name) {
            Some(obj) => Some(&obj),
            _ => None,
        }
    }
}
