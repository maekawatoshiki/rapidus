use super::super::error;
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

// #[derive(Clone, PartialEq, Debug, Copy)]
// pub struct Property2 {
//     pub val: Value2,
//     pub writable: bool,
//     pub enumerable: bool,
//     pub configurable: bool,
// }

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum Property2 {
    Data(DataProperty),
    Accessor(AccessorProperty),
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct DataProperty {
    pub val: Value2,
    pub writable: bool,
    pub enumerable: bool,
    pub configurable: bool,
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct AccessorProperty {
    pub get: Value2,
    pub set: Value2,
    pub enumerable: bool,
    pub configurable: bool,
}

impl ObjectInfo {
    pub fn has_own_property(&self, key: &str) -> bool {
        self.property.contains_key(key)
    }

    pub fn get_property(&self, key: Value2) -> Result<Property2, error::RuntimeError> {
        match self.kind {
            ObjectKind2::Array(ref info) => match key {
                Value2::Number(idx) if is_integer(idx) => return Ok(info.elems[idx as usize]),
                Value2::String(x) if unsafe { &*x }.to_str().unwrap() == "length" => {
                    return Ok(Property2::new_data_simple(Value2::Number(
                        info.elems.len() as f64
                    )))
                }
                _ => {}
            },
            _ => {}
        }

        match self.property.get(key.to_string().as_str()) {
            Some(prop) => Ok(*prop),
            None => match self.property.get("__proto__") {
                Some(Property2::Data(DataProperty {
                    val: Value2::Object(obj_info),
                    ..
                })) => unsafe { &**obj_info }.get_property(key),
                _ => Ok(Property2::new_data_simple(Value2::undefined())),
            },
        }
    }

    pub fn get_property_by_str_key(&self, key: &str) -> Value2 {
        match self.property.get(key) {
            Some(prop) => prop.as_data().val,
            None => match self.property.get("__proto__") {
                Some(Property2::Data(DataProperty {
                    val: Value2::Object(obj_info),
                    ..
                })) => unsafe { &**obj_info }.get_property_by_str_key(key),
                _ => Value2::undefined(),
            },
        }
    }

    pub fn set_property_by_string_key(&mut self, key: String, val: Value2) {
        let property = self.property.entry(key).or_insert_with(|| {
            Property2::Data(DataProperty {
                val: Value2::undefined(),
                writable: true,
                enumerable: true,
                configurable: true,
            })
        });
        property.as_data_mut().val = val;
    }

    pub fn set_property(
        &mut self,
        key: Value2,
        val_: Value2,
    ) -> Result<Option<Value2>, error::RuntimeError> {
        match self.kind {
            ObjectKind2::Array(ref mut info) => match key {
                Value2::Number(idx) if is_integer(idx) && idx >= 0.0 => {
                    info.elems[idx as usize].as_data_mut().val = val_;
                    return Ok(None);
                }
                Value2::String(x) if unsafe { &*x }.to_str().unwrap() == "length" => match val_ {
                    Value2::Number(n) if is_integer(n) && n >= 0.0 => {
                        while info.elems.len() < n as usize {
                            info.elems.push(Property2::new_data_simple(Value2::empty()))
                        }
                        return Ok(None);
                    }
                    _ => {}
                },
                _ => {}
            },
            _ => {}
        }

        let property = self
            .property
            .entry(key.to_string())
            .or_insert_with(|| Property2::new_data_simple(Value2::undefined()));

        match property {
            Property2::Data(DataProperty { ref mut val, .. }) => {
                *val = val_;
                Ok(None)
            }
            Property2::Accessor(AccessorProperty { set, .. }) => {
                if set.is_undefined() {
                    Ok(None)
                } else {
                    Ok(Some(*set))
                }
            }
        }
    }
}

impl Property2 {
    pub fn new_data(data: DataProperty) -> Self {
        Property2::Data(data)
    }

    pub fn new_data_simple(val: Value2) -> Self {
        Property2::Data(DataProperty {
            val,
            writable: true,
            enumerable: true,
            configurable: true,
        })
    }

    pub fn as_data(self) -> DataProperty {
        match self {
            Property2::Data(data) => data,
            _ => panic!(),
        }
    }

    pub fn as_accessor(self) -> AccessorProperty {
        match self {
            Property2::Accessor(accessor) => accessor,
            _ => panic!(),
        }
    }

    pub fn as_data_mut(&mut self) -> &mut DataProperty {
        match self {
            Property2::Data(ref mut data) => data,
            _ => panic!(),
        }
    }

    pub fn as_accessor_mut(&mut self) -> &mut AccessorProperty {
        match self {
            Property2::Accessor(ref mut accessor) => accessor,
            _ => panic!(),
        }
    }
}
