use std::collections::HashMap;

pub enum Value {
    Number(f64),
    String(String),
    Function(Vec<i32>), // i32 will replaced with appropriate type.
    Object(HashMap<String, Object>),
}

pub struct Object {
    pub val: Value,
    pub name: Option<String>,
}

pub struct VM {
    pub global_objects: HashMap<String, Object>,
}
