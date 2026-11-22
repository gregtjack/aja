use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use crate::ast::Id;

use super::{Answer, RuntimeError, Value};

/// Runtime environment
#[derive(Debug, Clone)]
pub struct RuntimeEnv {
    pub values: HashMap<Id, Value>,
    pub enclosing: Option<Arc<Mutex<RuntimeEnv>>>,
}

impl RuntimeEnv {
    /// Create a new blank run environment
    pub fn new() -> Self {
        RuntimeEnv {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    /// Create a new run environment from an enclosing scope
    pub fn from_enclosing(env: Arc<Mutex<RuntimeEnv>>) -> Self {
        RuntimeEnv {
            values: HashMap::new(),
            enclosing: Some(env),
        }
    }

    pub fn get(&self, id: Id) -> Answer {
        if let Some(ans) = self.values.get(&id) {
            return Ok(ans.clone());
        }

        if let Some(env) = &self.enclosing {
            return env.lock().unwrap().get(id);
        }

        return Err(RuntimeError::undefined(id));
    }

    /// Bind `id` in this scope. Re-declaring a name in the same scope
    /// shadows the previous binding (like Rust's `let`).
    pub fn define(&mut self, id: Id, value: Value) -> Result<(), RuntimeError> {
        self.values.insert(id, value);
        Ok(())
    }

    pub fn assign(&mut self, id: Id, value: Value) -> Answer {
        if self.values.contains_key(&id) {
            self.values.insert(id, value.clone());
            return Ok(value);
        }

        if let Some(env) = &mut self.enclosing {
            return env.lock().unwrap().assign(id, value);
        }

        return Err(RuntimeError::undefined(id));
    }

    /// All names visible from this scope, innermost first.
    pub fn names(&self) -> Vec<Id> {
        let mut names: Vec<Id> = self.values.keys().cloned().collect();
        if let Some(env) = &self.enclosing {
            names.extend(env.lock().unwrap().names());
        }
        names
    }
}
