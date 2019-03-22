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
    Array(ArrayObjectInfo),
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
    pub fn has_own_property(&self, key: &str) -> bool {
        self.property.contains_key(key)
    }

    pub fn get_property(&self, key: Value2) -> Value2 {
        match self.property.get(key.to_string().as_str()) {
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

    pub fn get_property_by_str_key(&self, key: &str) -> Value2 {
        match self.property.get(key) {
            Some(prop) => prop.val,
            None => match self.property.get("__proto__") {
                Some(Property2 {
                    val: Value2::Object(obj_info),
                    ..
                }) => unsafe { &**obj_info }.get_property_by_str_key(key),
                _ => Value2::undefined(),
            },
        }
    }

    pub fn set_property_by_string_key(&mut self, key: String, val: Value2) {
        let property = self.property.entry(key).or_insert_with(|| Property2 {
            val: Value2::undefined(),
            writable: true,
            enumerable: true,
            configurable: true,
        });
        property.val = val;
    }

    pub fn set_property(&mut self, key: Value2, val: Value2) {
        let property = self
            .property
            .entry(key.to_string())
            .or_insert_with(|| Property2 {
                val: Value2::undefined(),
                writable: true,
                enumerable: true,
                configurable: true,
            });
        property.val = val;
    }
}
