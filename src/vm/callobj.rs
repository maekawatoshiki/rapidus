use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

use super::error::RuntimeError;
use super::value::{ArrayValue, Value, ValueBase};
use gc;
use gc::GcType;

pub type CallObjectRef = GcType<CallObject>;

#[derive(Clone, Debug)]
pub struct CallObject {
    pub vals: GcType<FxHashMap<String, Value>>,
    pub params: Vec<(String, bool)>, // (name, rest param?)
    pub arg_rest_vals: Vec<Value>,
    pub this: Box<Value>,
    pub parent: Option<CallObjectRef>,
}

impl PartialEq for CallObject {
    fn eq(&self, other: &CallObject) -> bool {
        self.vals == other.vals
            && self.params == other.params
            && self.arg_rest_vals == other.arg_rest_vals
            && self.parent == other.parent
    }
}

impl CallObject {
    pub fn new(this: Value) -> CallObject {
        CallObject {
            vals: gc::new(FxHashMap::default()),
            params: vec![],
            arg_rest_vals: vec![],
            this: Box::new(this),
            parent: None,
        }
    }

    pub fn new_global() -> CallObjectRef {
        let vals = gc::new(FxHashMap::default());
        let callobj = gc::new(CallObject {
            vals: vals.clone(),
            params: vec![],
            arg_rest_vals: vec![],
            this: Box::new(Value::new(ValueBase::Undefined)),
            parent: None,
        });
        unsafe {
            *(*callobj).this = Value::new(ValueBase::Object(vals));
        }
        callobj
    }

    pub fn clear_args_vals(&mut self) {
        let params = self.params.clone();

        for (name, _) in params {
            self.set_value(name, Value::undefined());
        }

        self.arg_rest_vals.clear();
    }

    pub fn apply_arguments(&mut self, args: &Vec<Value>) {
        let mut rest_args = vec![];
        let mut rest_param_name = None;

        for (i, arg) in args.iter().enumerate() {
            if let Some(name) = self.get_parameter_nth_name(i) {
                // When rest parameter. TODO: More features of rest parameter
                if self.params[i].1 {
                    rest_param_name = Some(name);
                    rest_args.push(arg.clone());
                } else {
                    self.set_value(name, arg.clone());
                }
            } else {
                rest_args.push(arg.clone());
            }
        }

        if let Some(rest_param_name) = rest_param_name {
            self.set_value(
                rest_param_name,
                Value::array(gc::new(ArrayValue::new(rest_args))),
            );
        } else {
            for arg in rest_args {
                self.arg_rest_vals.push(arg);
            }
        }
    }

    pub fn set_value(&mut self, name: String, val: Value) {
        unsafe {
            (*self.vals).insert(name, val);
        }
    }

    pub fn set_value_if_exist(&mut self, name: String, val: Value) {
        unsafe {
            match (*self.vals).entry(name.clone()) {
                Entry::Occupied(ref mut v) => *v.get_mut() = val,
                Entry::Vacant(v) => {
                    match self.parent {
                        Some(ref parent) => return (**parent).set_value_if_exist(name, val),
                        None => v.insert(val),
                    };
                }
            }
        }
    }

    pub fn get_value(&self, name: &String) -> Result<Value, RuntimeError> {
        unsafe {
            if let Some(val) = (*self.vals).get(name) {
                return Ok(val.clone());
            }
            match self.parent {
                Some(ref parent) => (**parent).get_value(name),
                None => Err(RuntimeError::Reference(format!(
                    "reference error: '{}' is not defined",
                    name
                ))),
            }
        }
    }

    pub fn get_arguments_nth_value(&self, n: usize) -> Result<Value, RuntimeError> {
        if n < self.params.len() {
            let param_name = &self.params[n].0;
            return self.get_value(param_name);
        }

        let n = n - self.params.len();
        if n >= self.arg_rest_vals.len() {
            return Ok(Value::new(ValueBase::Undefined));
        }
        Ok(self.arg_rest_vals[n].clone())
    }

    pub fn set_arguments_nth_value(&mut self, n: usize, val: Value) {
        if n < self.params.len() {
            let param_name = self.params[n].0.clone();
            self.set_value(param_name, val);
            return;
        }

        let n = n - self.params.len();
        if n >= self.arg_rest_vals.len() {
            return;
        }
        self.arg_rest_vals[n] = val;
    }

    pub fn get_arguments_length(&self) -> usize {
        self.params.len() + self.arg_rest_vals.len()
    }

    pub fn get_parameter_nth_name(&self, n: usize) -> Option<String> {
        if n < self.params.len() {
            return Some(self.params[n].0.clone());
        }
        None
    }
}
