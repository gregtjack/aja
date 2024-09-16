use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use crate::ast::Id;

use super::{Answer, RuntimeError, Value};

/// Run environment
#[derive(Debug, Clone)]
pub struct REnvironment {
    pub values: HashMap<Id, Value>,
    pub enclosing: Option<Arc<Mutex<REnvironment>>>,
}

impl REnvironment {
    /// Create a new blank run environment
    pub fn new() -> Self {
        REnvironment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    /// Create a new run environment from an enclosing scope
    pub fn from_enclosing(env: Arc<Mutex<REnvironment>>) -> Self {
        REnvironment {
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

        return Err(RuntimeError::UndefinedVariable { var: id });
    }

    pub fn define(&mut self, id: Id, value: Value) -> Result<(), RuntimeError> {
        if let Some(_) = self.values.insert(id.clone(), value) {
            return Err(RuntimeError::UndefinedVariable { var: id });
        }

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

        return Err(RuntimeError::UndefinedVariable { var: id });
    }
}
