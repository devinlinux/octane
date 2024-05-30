use std::collections::HashMap;
use super::Object;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    lookup_table: HashMap<usize, String>,
    store: HashMap<String, Object>,
    context: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Environment {
        Self {
            lookup_table: HashMap::new(),
            store: HashMap::new(),
            context: None,
        }
    }

    pub fn new_with_context(context: Environment) -> Environment {
        Self {
            lookup_table: HashMap::new(),
            store: HashMap::new(),
            context: Some(Box::new(context)),
        }
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }

    pub fn get(&self, name: usize) -> Option<Object> {
        match self.lookup_table.get(&name) {
            Some(binding) => match self.store.get(binding) {
                Some(obj) => Some(obj.clone()),
                None => match &self.context {
                    Some(ctx) => ctx.get(name),
                    None => None,
                }
            }
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
