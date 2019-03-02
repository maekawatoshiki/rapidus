use super::value::*;
pub use rustc_hash::FxHashMap;

#[derive(Clone, Debug)]
pub struct ObjectInfo {
    pub kind: ObjectKind2,
    pub property: FxHashMap<String, Property2>,
}

#[derive(Clone, Debug)]
pub enum ObjectKind2 {
    Function(FunctionObjectInfo),
    Ordinary,
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct Property2 {
    pub val: Value2,
    pub writable: bool,
    pub enumerable: bool,
    pub configurable: bool,
}

impl ObjectInfo {
    pub fn get_property(&self, key: &str) -> Value2 {
        match self.property.get(key) {
            Some(prop) => prop.val,
            None => match self.property.get("__proto__") {
                Some(Property2 {
                    val: Value2::Object(obj_info),
                    ..
                }) => unsafe { &**obj_info }.get_property(key),
                _ => Value2::undefined(),
            },
        }
    }
}
