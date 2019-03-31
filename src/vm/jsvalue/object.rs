use super::super::super::gc::MemoryAllocator;
use super::super::error;
use super::prototype::ObjectPrototypes;
use super::value::*;
pub use rustc_hash::FxHashMap;

#[derive(Clone, Debug)]
pub struct ObjectInfo {
    /// Kind
    pub kind: ObjectKind2,
    /// Internal slot \[\[Prototype\]\]
    pub prototype: Value2,
    /// Properties
    pub property: FxHashMap<String, Property2>,
}

#[derive(Clone, Debug)]
pub enum ObjectKind2 {
    Function(FunctionObjectInfo),
    Array(ArrayObjectInfo),
    Ordinary,
}

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

    #[inline]
    pub fn get_prototype(&self) -> Value2 {
        self.prototype
    }

    pub fn get_property(
        &self,
        allocator: &mut MemoryAllocator,
        object_prototypes: &ObjectPrototypes,
        key: Value2,
    ) -> Result<Property2, error::RuntimeError> {
        // Annoying
        if key.is_string() && key.into_str() == "__proto__" {
            return Ok(Property2::new_data_simple(self.get_prototype()));
        }

        match self.kind {
            ObjectKind2::Array(ref info) => {
                if let Some(idx) = key.is_array_index() {
                    return Ok(info.get_element(idx));
                }

                if let Some(idx) = key.is_canonical_numeric_index_string() {
                    return Ok(info.get_element(idx));
                }

                if key.is_string() && key.into_str() == "length" {
                    return Ok(Property2::new_data_simple(Value2::Number(
                        info.elems.len() as f64
                    )));
                }
            }
            _ => {}
        }

        match self.property.get(key.to_string().as_str()) {
            Some(prop) => Ok(*prop),
            None => self
                .prototype
                .get_property(allocator, object_prototypes, key),
        }
    }

    pub fn get_property_by_str_key(&self, key: &str) -> Value2 {
        match self.property.get(key) {
            Some(prop) => prop.as_data().val,
            None => self.prototype.get_property_by_str_key(key),
        }
    }

    pub fn set_property_by_string_key(&mut self, key: String, val: Value2) {
        let property = self
            .property
            .entry(key)
            .or_insert_with(|| Property2::new_data_simple(Value2::undefined()));
        let data = property.as_data_mut();
        if data.writable {
            data.val = val;
        }
    }

    pub fn set_property(
        &mut self,
        key: Value2,
        val_: Value2,
    ) -> Result<Option<Value2>, error::RuntimeError> {
        // Annoying
        if key.is_string() && key.into_str() == "__proto__" {
            self.prototype = val_;
            return Ok(None);
        }

        match self.kind {
            ObjectKind2::Array(ref mut info) => {
                if let Some(idx) = key.is_array_index() {
                    return Ok(info.set_element(idx, val_));
                }

                if let Some(idx) = key.is_canonical_numeric_index_string() {
                    return Ok(info.set_element(idx, val_));
                }

                if key.is_string() && key.into_str() == "length" {
                    if let Some(new_length) = val_.is_array_index() {
                        info.set_length(new_length);
                        return Ok(None);
                    }
                }
            }
            _ => {}
        }

        let property = self
            .property
            .entry(key.to_string())
            .or_insert_with(|| Property2::new_data_simple(Value2::undefined()));

        match property {
            Property2::Data(DataProperty {
                ref mut val,
                writable,
                ..
            }) => {
                if *writable {
                    *val = val_;
                }
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
