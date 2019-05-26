use super::super::super::gc::MemoryAllocator;
use super::super::error;
use super::prototype::ObjectPrototypes;
use super::value::*;
pub use rustc_hash::FxHashMap;

#[derive(Clone, Debug)]
pub struct ObjectInfo {
    /// Kind
    pub kind: ObjectKind,
    /// Internal slot \[\[Prototype\]\]
    pub prototype: Value,
    /// Properties
    pub property: FxHashMap<String, Property>,
    /// Symbol properties
    pub sym_property: FxHashMap<usize, Property>,
}

#[derive(Debug, Clone)]
pub struct ObjectRef(pub *mut ObjectInfo);

impl std::ops::Deref for ObjectRef {
    type Target = ObjectInfo;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0 }
    }
}

impl std::ops::DerefMut for ObjectRef {
    fn deref_mut(&mut self) -> &mut ObjectInfo {
        unsafe { &mut *self.0 }
    }
}

#[derive(Clone, Debug)]
pub enum ObjectKind {
    Function(FunctionObjectInfo),
    Array(ArrayObjectInfo),
    Symbol(SymbolInfo),
    Ordinary,
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum Property {
    Data(DataProperty),
    Accessor(AccessorProperty),
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct DataProperty {
    pub val: Value,
    pub writable: bool,
    pub enumerable: bool,
    pub configurable: bool,
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub struct AccessorProperty {
    pub get: Value,
    pub set: Value,
    pub enumerable: bool,
    pub configurable: bool,
}

impl ObjectInfo {
    pub fn has_own_property(&self, key: &str) -> bool {
        self.property.contains_key(key)
    }

    #[inline]
    pub fn get_prototype(&self) -> Value {
        self.prototype
    }

    pub fn get_property(
        &self,
        allocator: &mut MemoryAllocator,
        object_prototypes: &ObjectPrototypes,
        key: Value,
    ) -> Result<Property, error::RuntimeError> {
        // Annoying
        if key.is_string() && key.into_str() == "__proto__" {
            return Ok(Property::new_data_simple(self.get_prototype()));
        }

        if key.is_symbol() {
            let id = key.get_symbol_info().id;
            return match self.sym_property.get(&id) {
                Some(prop) => Ok(*prop),
                None => self
                    .prototype
                    .get_property(allocator, object_prototypes, key),
            };
        }

        match self.kind {
            ObjectKind::Array(ref info) => {
                if let Some(idx) = key.is_array_index() {
                    return Ok(info.get_element(idx));
                }

                if let Some(idx) = key.is_canonical_numeric_index_string(allocator) {
                    return Ok(info.get_element(idx));
                }

                if key.is_string() && key.into_str() == "length" {
                    return Ok(Property::new_data_simple(Value::Number(
                        info.elems.len() as f64
                    )));
                }
            }
            _ => {}
        }

        match self.property.get(key.to_string().as_str()) {
            Some(prop) => Ok(*prop),
            None => {
                let proto = self.prototype;
                if proto.is_null() {
                    return Ok(Property::new_data_simple(Value::undefined()));
                };
                proto.get_property(allocator, object_prototypes, key)
            }
        }
    }

    pub fn get_property_by_str_key(&self, key: &str) -> Value {
        match self.property.get(key) {
            Some(prop) => prop.as_data().val,
            None => self.prototype.get_property_by_str_key(key),
        }
    }

    pub fn set_property_by_string_key(&mut self, key: String, val: Value) {
        let property = self
            .property
            .entry(key)
            .or_insert_with(|| Property::new_data_simple(Value::undefined()));
        let data = property.as_data_mut();
        if data.writable {
            data.val = val;
        }
    }

    pub fn set_property(
        &mut self,
        allocator: &mut MemoryAllocator,
        key: Value,
        val_: Value,
    ) -> Result<Option<Value>, error::RuntimeError> {
        // Annoying
        if key.is_string() && key.into_str() == "__proto__" {
            self.prototype = val_;
            return Ok(None);
        }

        match self.kind {
            ObjectKind::Array(ref mut info) => {
                if let Some(idx) = key.is_array_index() {
                    return Ok(info.set_element(idx, val_));
                }

                if let Some(idx) = key.is_canonical_numeric_index_string(allocator) {
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

        let property = if key.is_symbol() {
            let id = key.get_symbol_info().id;
            self.sym_property
                .entry(id)
                .or_insert_with(|| Property::new_data_simple(Value::undefined()))
        } else {
            self.property
                .entry(key.to_string())
                .or_insert_with(|| Property::new_data_simple(Value::undefined()))
        };

        match property {
            Property::Data(DataProperty {
                ref mut val,
                writable,
                ..
            }) => {
                if *writable {
                    *val = val_;
                }
                Ok(None)
            }
            Property::Accessor(AccessorProperty { set, .. }) => {
                if set.is_undefined() {
                    Ok(None)
                } else {
                    Ok(Some(*set))
                }
            }
        }
    }
}

impl Property {
    pub fn new_data(data: DataProperty) -> Self {
        Property::Data(data)
    }

    pub fn new_data_simple(val: Value) -> Self {
        Property::Data(DataProperty {
            val,
            writable: true,
            enumerable: true,
            configurable: true,
        })
    }

    pub fn as_data(self) -> DataProperty {
        match self {
            Property::Data(data) => data,
            _ => panic!(),
        }
    }

    pub fn as_accessor(self) -> AccessorProperty {
        match self {
            Property::Accessor(accessor) => accessor,
            _ => panic!(),
        }
    }

    pub fn as_data_mut(&mut self) -> &mut DataProperty {
        match self {
            Property::Data(ref mut data) => data,
            _ => panic!(),
        }
    }

    pub fn as_accessor_mut(&mut self) -> &mut AccessorProperty {
        match self {
            Property::Accessor(ref mut accessor) => accessor,
            _ => panic!(),
        }
    }

    pub fn get_data(&self) -> Option<&DataProperty> {
        match self {
            Property::Data(data) => Some(data),
            _ => None,
        }
    }
}
