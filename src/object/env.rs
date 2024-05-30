use std::collections::HashMap;
use super::Object;

pub struct Environment {
    lookup_table: HashMap<usize, String>,
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Environment {
        Self {
            lookup_table: HashMap::new(),
            store: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }

    pub fn get(&self, name: usize) -> Option<*const Object> {
        match self.lookup_table.get(&name) {
            Some(binding) => {
                self.store.get(binding).map(|obj| obj as *const Object)  //  get the raw pointer
            },
            None => None,
        }
    }

    pub fn lookup_ident(&self, id: usize) -> Option<&String> {
        self.lookup_table.get(&id)
    }

    pub fn set_lookup_table(&mut self, lookup_table: HashMap<usize, String>) {
        self.lookup_table = lookup_table;
    }
}
