use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

use super::error::RuntimeError;
use super::value::*;
use gc;
use gc::GcType;

pub type CallObjectRef = GcType<CallObject>;

#[derive(Clone, Debug)]
pub struct CallObject {
    pub vals: PropMap,
    pub this: Box<Value>,
    pub func: Option<ObjectKind>,
    pub parent: Option<CallObjectRef>,
}

impl PartialEq for CallObject {
    fn eq(&self, other: &CallObject) -> bool {
        self.vals == other.vals && self.parent == other.parent
    }
}

impl CallObject {
    pub fn new(this: Value, func: Option<ObjectKind>) -> CallObject {
        CallObject {
            vals: gc::new(FxHashMap::default()),
            this: Box::new(this),
            func: func,
            parent: None,
        }
    }

    pub fn new_global() -> CallObjectRef {
        let vals = gc::new(FxHashMap::default());
        let callobj = gc::new(CallObject {
            vals: vals.clone(),
            this: Box::new(Value::Undefined),
            func: None,
            parent: None,
        });
        unsafe {
            *(*callobj).this = Value::Object(vals, ObjectKind::Ordinary);
        }
        callobj
    }

    pub fn clear_args_vals(&mut self, func_info: FuncInfo) {
        let params = func_info.clone().params.clone();
        for (name, _) in params {
            self.set_value(name, Value::Undefined);
        }
        func_info.clone().arg_rest_vals.clear();
    }

    pub fn apply_arguments(&mut self, func_info: FuncInfo, args: &Vec<Value>) {
        let mut rest_args = vec![];
        let mut rest_param_name = None;
        //if let Some(ObjectKind::Function(box (func_info, _))) = self.clone().func {
        for (i, arg) in args.iter().enumerate() {
            if let Some(name) = self.get_parameter_nth_name(func_info.clone(), i) {
                // When rest parameter. TODO: More features of rest parameter
                if func_info.params[i].1 {
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
            self.set_value(rest_param_name, Value::array_from_elems(rest_args));
        } else {
            for arg in rest_args {
                func_info.clone().arg_rest_vals.push(arg);
            }
        }
        //} else {
        //    unreachable!("can not apply arguments in non-function environment.");
        //}
    }

    pub fn set_value(&mut self, name: String, val: Value) {
        unsafe {
            (*self.vals).insert(name, Property::new(val));
        }
    }

    pub fn set_value_if_exist(&mut self, name: String, val: Value) {
        unsafe {
            match (*self.vals).entry(name.clone()) {
                Entry::Occupied(ref mut v) => *v.get_mut() = Property::new(val),
                Entry::Vacant(v) => {
                    match self.parent {
                        Some(ref parent) => return (**parent).set_value_if_exist(name, val),
                        None => v.insert(Property::new(val)),
                    };
                }
            }
        }
    }

    pub fn get_value(&self, name: &String) -> Result<Value, RuntimeError> {
        unsafe {
            if let Some(prop) = (*self.vals).get(name) {
                return Ok(prop.val.clone());
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
        if let Some(ObjectKind::Function(box (func_info, _))) = self.clone().func {
            if n < func_info.params.len() {
                let param_name = &func_info.params[n].0;
                return self.get_value(param_name);
            }

            let n = n - func_info.params.len();
            if n >= func_info.arg_rest_vals.len() {
                return Ok(Value::Undefined);
            }
            Ok(func_info.arg_rest_vals[n].clone())
        } else {
            unreachable!("can not get arguments in non-function environment.");
        }
    }

    pub fn set_arguments_nth_value(&mut self, n: usize, val: Value) {
        if let Some(ObjectKind::Function(box (func_info, _))) = self.clone().func {
            if n < func_info.params.len() {
                let param_name = func_info.params[n].0.clone();
                self.set_value(param_name, val);
                return;
            }

            let n = n - func_info.params.len();
            if n >= func_info.arg_rest_vals.len() {
                return;
            }
            func_info.clone().arg_rest_vals[n] = val;
        } else {
            unreachable!("can not set arguments in non-function environment.");
        }
    }

    pub fn get_arguments_length(&self) -> usize {
        if let Some(ObjectKind::Function(box (func_info, _))) = self.clone().func {
            func_info.params.len() + func_info.arg_rest_vals.len()
        } else {
            unreachable!("can not get arguments in non-function environment.");
        }
    }

    pub fn get_parameter_nth_name(&self, func_info: FuncInfo, n: usize) -> Option<String> {
        if n < func_info.params.len() {
            return Some(func_info.params[n].0.clone());
        }
        None
    }
}
