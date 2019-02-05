use super::error::RuntimeError;
use super::value::*;
use gc;
use rustc_hash::FxHashMap;

#[derive(Clone)]
/// 80 bytes
pub struct CallObject {
    /// map of variables belongs to the scope.
    pub vals: PropMapRef,
    /// this value.
    pub this: Box<Value>,
    /// reference to the outer scope.
    pub parent: Option<CallObjectRef>,
}

impl PartialEq for CallObject {
    fn eq(&self, other: &CallObject) -> bool {
        self.vals == other.vals && self.parent == other.parent
    }
}
/*
impl fmt::Debug for CallObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ptr_co: *const CallObject = &*self;
        let ptr_map: *const PropMapRef = *self.vals;
        write!(f, "CO:[{:?}] Map:[{:?}]", ptr_co, ptr_map)
    }
}

*/
impl CallObject {
    fn new(this: Value) -> CallObject {
        CallObject {
            vals: gc::new(FxHashMap::default()),
            this: Box::new(this),
            parent: None,
        }
    }

    pub fn new_with_this(this: Value) -> CallObjectRef {
        gc::new(CallObject::new(this))
    }

    /// create new CallObj for func invocation.
    /// this: None => use self.this.
    pub fn new_callobj_from_func(&self, this: Option<Value>) -> CallObjectRef {
        let mut callobj = match this {
            Some(this) => CallObject::new(this),
            None => CallObject::new(*self.this.clone()),
        };
        callobj.parent = self.parent.clone();
        gc::new(callobj)
    }

    pub fn new_global() -> CallObjectRef {
        let vals = gc::new(FxHashMap::default());
        gc::new(CallObject {
            vals: vals.clone(),
            this: Box::new(Value::Object(vals.clone(), ObjectKind::Ordinary)),
            parent: None,
        })
    }

    /// set the binding for the value and the name in the local environment.
    pub fn set_local_value(&mut self, name: String, val: Value) -> Result<(), RuntimeError> {
        let prop = self.vals.get(&name);
        match prop {
            None => {
                // if the binding does not exists
                self.vals.clone().insert(name, val.to_property());
                Ok(())
            }
            Some(prop) => {
                if !prop.writable && prop.val != Value::Uninitialized {
                    // if the binding in immutable and already initialized
                    Err(RuntimeError::Type(format!(
                        "Assignment to constant variable {}.",
                        name
                    )))
                } else {
                    let prop = Property { val: val, ..*prop };
                    self.vals.clone().insert(name, prop);
                    Ok(())
                }
            }
        }
    }

    pub fn set_mutability(&mut self, name: String, mutable: bool) {
        self.vals.entry(name).and_modify(|x| x.writable = mutable);
    }

    pub fn set_value(&mut self, name: String, val: Value) -> Result<(), RuntimeError> {
        if self.vals.contains_key(&name.clone()) {
            self.set_local_value(name.clone(), val)?;
            Ok(())
        } else {
            match self.parent {
                Some(ref mut parent) => parent.set_value(name, val),
                None => {
                    self.set_local_value(name, val)?;
                    Ok(())
                }
            }
        }
    }

    /// get the binding for the name tracing environments chain.
    pub fn get_value(&self, name: &String) -> Result<Value, RuntimeError> {
        if let Some(prop) = self.vals.get(name) {
            return Ok(prop.val.clone());
        }
        match self.parent {
            Some(ref parent) => parent.get_value(name),
            None => Err(RuntimeError::Reference(format!(
                "reference error: '{}' is not defined",
                name
            ))),
        }
    }

    /// get the binding for the name in the local environment.
    pub fn get_local_value(&self, name: &String) -> Result<Value, RuntimeError> {
        if let Some(prop) = self.vals.get(name) {
            Ok(prop.val.clone())
        } else {
            Err(RuntimeError::General(
                "get_local_value(): the argument did not found in local scope.".to_string(),
            ))
        }
    }
}
