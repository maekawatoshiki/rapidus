use super::super::super::gc::MemoryAllocator;
use super::super::error;
use super::super::exec_context::ExecContext;
use super::value::*;
use crate::vm::vm::Factory;
pub use rustc_hash::FxHashMap;

#[derive(Clone, Debug)]
pub struct Object {
    /// Kind
    pub kind: ObjectKind,
    /// Internal slot \[\[Prototype\]\]
    pub prototype: Value,
    /// Properties
    pub property: FxHashMap<String, Property>,
    /// Own string property creation order, excluding array-index ordering rules.
    pub property_order: Vec<String>,
    /// Private fields and methods, keyed by PrivateIdentifier name.
    pub private_elements: FxHashMap<String, Property>,
    /// Symbol properties
    pub sym_property: FxHashMap<usize, Property>,
    /// Own symbol property creation order.
    pub sym_property_order: Vec<Value>,
    /// Internal slot [[Extensible]]
    pub extensible: bool,
}

#[derive(Debug, Clone)]
pub struct ObjectRef(pub *mut Object);

impl std::ops::Deref for ObjectRef {
    type Target = Object;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0 }
    }
}

impl std::ops::DerefMut for ObjectRef {
    fn deref_mut(&mut self) -> &mut Object {
        unsafe { &mut *self.0 }
    }
}

#[derive(Clone, Debug)]
pub enum ObjectKind {
    Function(FunctionObjectInfo),
    Array(ArrayObjectInfo),
    Date(DateObjectInfo),
    RegExp(RegExpObjectInfo),
    Map(MapObjectInfo),
    Set(SetObjectInfo),
    WeakMap(WeakMapObjectInfo),
    WeakSet(WeakSetObjectInfo),
    WeakRef(WeakRefObjectInfo),
    FinalizationRegistry(FinalizationRegistryObjectInfo),
    ShadowRealm(ShadowRealmObjectInfo),
    MapIterator(MapIteratorInfo),
    SetIterator(SetIteratorInfo),
    Generator(GeneratorObjectInfo),
    ArrayBuffer(ArrayBufferObjectInfo),
    DataView(DataViewObjectInfo),
    TypedArray(TypedArrayObjectInfo),
    Symbol(SymbolInfo),
    BigInt(BigIntInfo),
    Error(ErrorObjectInfo),
    Arguments(ArgumentsObjectInfo),
    Proxy(ProxyObjectInfo),
    Temporal(TemporalObjectInfo),
    Ordinary,
}

#[derive(Clone, Debug)]
pub struct RegExpObjectInfo {
    pub original_source: String,
    pub original_flags: String,
}

#[derive(Clone, Debug)]
pub struct MapObjectInfo {
    pub entries: Vec<(Value, Value)>,
}

#[derive(Clone, Debug)]
pub struct SetObjectInfo {
    pub entries: Vec<Value>,
}

#[derive(Clone, Debug)]
pub struct WeakMapObjectInfo {
    pub entries: Vec<(Value, Value)>,
}

#[derive(Clone, Debug)]
pub struct WeakSetObjectInfo {
    pub entries: Vec<Value>,
}

#[derive(Clone, Debug)]
pub struct WeakRefObjectInfo {
    pub target: Value,
}

#[derive(Clone, Debug)]
pub struct FinalizationRegistryCell {
    pub target: Value,
    pub holdings: Value,
    pub unregister_token: Option<Value>,
}

#[derive(Clone, Debug)]
pub struct FinalizationRegistryObjectInfo {
    pub cleanup_callback: Value,
    pub cells: Vec<FinalizationRegistryCell>,
}

#[derive(Clone, Debug)]
pub struct ShadowRealmObjectInfo {
    pub realm: *mut crate::vm::vm::VM,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CollectionIteratorKind {
    Key,
    Value,
    KeyValue,
}

#[derive(Clone, Debug)]
pub struct MapIteratorInfo {
    pub iterated_map: Value,
    pub next_index: usize,
    pub kind: CollectionIteratorKind,
}

#[derive(Clone, Debug)]
pub struct SetIteratorInfo {
    pub iterated_set: Value,
    pub next_index: usize,
    pub kind: CollectionIteratorKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum GeneratorState {
    SuspendedStart,
    SuspendedYield,
    Executing,
    Completed,
}

#[derive(Clone, Debug)]
pub struct GeneratorObjectInfo {
    pub context: Option<ExecContext>,
    pub state: GeneratorState,
}

#[derive(Clone, Debug)]
pub struct ArgumentsObjectInfo {
    pub parameter_map: FxHashMap<String, String>,
}

#[derive(Clone, Debug)]
pub struct ProxyObjectInfo {
    pub target: Value,
    pub handler: Value,
}

#[derive(Clone, Debug)]
pub struct TemporalObjectInfo {
    pub kind: TemporalObjectKind,
}

#[derive(Clone, Debug)]
pub enum TemporalObjectKind {
    Calendar(TemporalCalendarInfo),
    Duration(TemporalDurationInfo),
    Instant(TemporalInstantInfo),
    PlainDate(TemporalDateInfo),
    PlainDateTime(TemporalDateTimeInfo),
    PlainMonthDay(TemporalMonthDayInfo),
    PlainTime(TemporalTimeInfo),
    PlainYearMonth(TemporalYearMonthInfo),
    TimeZone(TemporalTimeZoneInfo),
    ZonedDateTime(TemporalZonedDateTimeInfo),
}

#[derive(Clone, Debug)]
pub struct TemporalCalendarInfo {
    pub identifier: String,
}

#[derive(Clone, Debug)]
pub struct TemporalDurationInfo {
    pub years: i64,
    pub months: i64,
    pub weeks: i64,
    pub days: i64,
    pub hours: i64,
    pub minutes: i64,
    pub seconds: i64,
    pub milliseconds: i64,
    pub microseconds: i64,
    pub nanoseconds: i64,
}

#[derive(Clone, Debug)]
pub struct TemporalInstantInfo {
    pub epoch_nanoseconds: i128,
}

#[derive(Clone, Debug)]
pub struct TemporalDateInfo {
    pub year: i32,
    pub month: u8,
    pub day: u8,
    pub calendar: String,
    pub calendar_object: Option<Value>,
}

#[derive(Clone, Debug)]
pub struct TemporalDateTimeInfo {
    pub date: TemporalDateInfo,
    pub time: TemporalTimeInfo,
}

#[derive(Clone, Debug)]
pub struct TemporalMonthDayInfo {
    pub month: u8,
    pub day: u8,
    pub calendar: String,
    pub calendar_object: Option<Value>,
}

#[derive(Clone, Debug)]
pub struct TemporalTimeInfo {
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
    pub millisecond: u16,
    pub microsecond: u16,
    pub nanosecond: u16,
}

#[derive(Clone, Debug)]
pub struct TemporalYearMonthInfo {
    pub year: i32,
    pub month: u8,
    pub calendar: String,
    pub calendar_object: Option<Value>,
}

#[derive(Clone, Debug)]
pub struct TemporalTimeZoneInfo {
    pub identifier: String,
    pub offset_nanoseconds: Option<i64>,
    pub object: Option<Value>,
}

#[derive(Clone, Debug)]
pub struct TemporalZonedDateTimeInfo {
    pub epoch_nanoseconds: i128,
    pub time_zone: TemporalTimeZoneInfo,
    pub calendar: String,
    pub calendar_object: Option<Value>,
}

#[derive(Clone, Debug)]
pub struct BigIntInfo {
    pub decimal: String,
}

#[derive(Clone, Debug)]
pub struct ArrayBufferObjectInfo {
    pub bytes: Vec<u8>,
    pub max_byte_length: Option<usize>,
    pub detached: bool,
    pub shared: bool,
}

#[derive(Clone, Debug)]
pub struct DataViewObjectInfo {
    pub buffer: Value,
    pub byte_offset: usize,
    pub byte_length: usize,
    pub length_tracking: bool,
}

#[derive(Clone, Debug)]
pub struct TypedArrayObjectInfo {
    pub buffer: Value,
    pub byte_offset: usize,
    pub length: usize,
    pub length_tracking: bool,
    pub element_size: usize,
    pub kind: crate::builtins::typed_array::TypedArrayElementKind,
    pub name: &'static str,
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

impl Object {
    pub fn insert_property(&mut self, key: String, prop: Property) -> Option<Property> {
        if !self.property.contains_key(&key) {
            self.property_order.push(key.clone());
        }
        self.property.insert(key, prop)
    }

    pub fn remove_property(&mut self, key: &str) -> Option<Property> {
        let removed = self.property.remove(key);
        if removed.is_some() {
            self.property_order.retain(|existing| existing != key);
        }
        removed
    }

    pub fn get_private_element(&self, key: &str) -> Option<Property> {
        self.private_elements.get(key).copied()
    }

    pub fn define_private_element(&mut self, key: String, prop: Property) -> Result<(), ()> {
        if self.private_elements.contains_key(&key) {
            return Err(());
        }
        self.private_elements.insert(key, prop);
        Ok(())
    }

    pub fn define_private_accessor(
        &mut self,
        key: String,
        func: Value,
        is_getter: bool,
    ) -> Result<(), ()> {
        match self.private_elements.get_mut(&key) {
            Some(Property::Accessor(accessor)) => {
                if is_getter {
                    if !accessor.get.is_undefined() {
                        return Err(());
                    }
                    accessor.get = func;
                } else {
                    if !accessor.set.is_undefined() {
                        return Err(());
                    }
                    accessor.set = func;
                }
                Ok(())
            }
            Some(Property::Data(_)) => Err(()),
            None => {
                let prop = if is_getter {
                    Property::Accessor(AccessorProperty {
                        get: func,
                        set: Value::undefined(),
                        enumerable: false,
                        configurable: false,
                    })
                } else {
                    Property::Accessor(AccessorProperty {
                        get: Value::undefined(),
                        set: func,
                        enumerable: false,
                        configurable: false,
                    })
                };
                self.private_elements.insert(key, prop);
                Ok(())
            }
        }
    }

    pub fn update_private_element(&mut self, key: &str, val: Value) -> Result<Option<Value>, ()> {
        let Some(prop) = self.private_elements.get_mut(key) else {
            return Err(());
        };
        let (setter, success) = set_existing_property(prop, val);
        if success {
            Ok(setter)
        } else {
            Err(())
        }
    }

    pub fn own_string_property_keys(&self) -> Vec<String> {
        self.string_property_keys(false)
    }

    pub fn enumerable_own_string_property_keys(&self) -> Vec<String> {
        self.string_property_keys(true)
    }

    fn string_property_keys(&self, enumerable_only: bool) -> Vec<String> {
        let mut index_keys = Vec::new();
        for (key, prop) in &self.property {
            if is_internal_slot_key(key) {
                continue;
            }
            if (!enumerable_only || property_is_enumerable(prop)) && array_index_key(key).is_some()
            {
                push_index_key(&mut index_keys, key);
            }
        }

        let mut has_array_length = false;
        if let ObjectKind::Array(ref array) = self.kind {
            has_array_length = true;
            for (index, prop) in array.elems.iter().enumerate() {
                if property_is_present(prop) && (!enumerable_only || property_is_enumerable(prop)) {
                    push_index_key(&mut index_keys, &index.to_string());
                }
            }
        }

        let string_length = string_object_data(self).map(|string| {
            for index in 0..string.chars().count() {
                push_index_key(&mut index_keys, &index.to_string());
            }
            string.chars().count()
        });

        index_keys.sort_unstable();
        index_keys.dedup();
        let mut keys = index_keys
            .into_iter()
            .map(|index| index.to_string())
            .collect::<Vec<_>>();

        if !enumerable_only && has_array_length {
            keys.push("length".to_string());
        }
        if !enumerable_only && string_length.is_some() {
            keys.push("length".to_string());
        }

        for key in &self.property_order {
            if is_internal_slot_key(key)
                || array_index_key(key).is_some()
                || keys.iter().any(|existing| existing == key)
            {
                continue;
            }
            if let Some(prop) = self.property.get(key) {
                if !enumerable_only || property_is_enumerable(prop) {
                    keys.push(key.clone());
                }
            }
        }

        let mut unordered = self
            .property
            .iter()
            .filter(|(key, prop)| {
                !is_internal_slot_key(key)
                    && array_index_key(key).is_none()
                    && !self.property_order.iter().any(|ordered| ordered == *key)
                    && (!enumerable_only || property_is_enumerable(prop))
            })
            .map(|(key, _)| key.clone())
            .collect::<Vec<_>>();
        unordered.sort();
        keys.extend(unordered);
        keys
    }

    pub fn has_own_property(&self, key: &str) -> bool {
        if let ObjectKind::Proxy(ref proxy) = self.kind {
            return proxy.target.has_own_property(key);
        }
        if let ObjectKind::Array(ref array) = self.kind {
            if key == "length" {
                return true;
            }
            if let Ok(idx) = key.parse::<usize>() {
                if idx < array.elems.len() {
                    return array.elems[idx]
                        .get_data()
                        .map(|data| !data.val.is_empty())
                        .unwrap_or(true);
                }
            }
        }
        if let ObjectKind::TypedArray(_) = self.kind {
            if let Some(index) = string_index(key) {
                let this = Value::Object(self as *const Object as *mut Object);
                return crate::builtins::typed_array::typed_array_length(this)
                    .map(|length| index < length)
                    .unwrap_or(false);
            }
        }

        if string_object_index_exists(self, key) {
            return true;
        }

        self.property.contains_key(key)
    }

    #[inline]
    pub fn get_prototype(&self) -> Value {
        if let ObjectKind::Proxy(ref proxy) = self.kind {
            return proxy.target.get_prototype();
        }
        self.prototype
    }

    pub fn get_property_by_value(
        &self,
        factory: &mut Factory,
        key: Value,
    ) -> Result<Property, error::RuntimeError> {
        if let ObjectKind::Proxy(ref proxy) = self.kind {
            return proxy.target.get_property_by_value(factory, key);
        }
        // Annoying
        if key.is_string() && key.into_str() == "__proto__" {
            if let Some(prop) = self.property.get("__proto__") {
                return Ok(*prop);
            }
            return Ok(Property::new_data_simple(self.get_prototype()));
        }

        if key.is_symbol() {
            let id = key.get_symbol_info().id;
            return match self.sym_property.get(&id) {
                Some(prop) => Ok(*prop),
                None if self.prototype.is_null() => {
                    Ok(Property::new_data_simple(Value::undefined()))
                }
                None => self.prototype.get_property_by_value(factory, key),
            };
        }

        match self.kind {
            ObjectKind::Array(ref info) => {
                let index = key.is_array_index().or_else(|| {
                    key.is_canonical_numeric_index_string(&mut factory.memory_allocator)
                });
                if let Some(idx) = index {
                    if idx < info.elems.len() {
                        if info.elems[idx]
                            .get_data()
                            .map(|data| !data.val.is_empty())
                            .unwrap_or(true)
                        {
                            return Ok(info.get_element(idx));
                        }
                    }
                }

                if key.is_string() && key.into_str() == "length" {
                    return Ok(Property::Data(DataProperty {
                        val: Value::Number(info.get_length() as f64),
                        writable: info.length_writable,
                        enumerable: false,
                        configurable: false,
                    }));
                }
            }
            ObjectKind::TypedArray(_) => {
                let index = key.is_array_index().or_else(|| {
                    key.is_canonical_numeric_index_string(&mut factory.memory_allocator)
                });
                if let Some(index) = index {
                    let this = Value::Object(self as *const Object as *mut Object);
                    if let Some(value) =
                        crate::builtins::typed_array::typed_array_get_index(factory, this, index)
                    {
                        return Ok(Property::new_data_simple(value));
                    }
                }
            }
            _ => {}
        }

        if !key.is_symbol() {
            let key_string = key.to_string();
            if let Some(prop) = string_object_index_property(factory, self, &key_string) {
                return Ok(prop);
            }
        }

        match self.property.get(key.to_string().as_str()) {
            Some(prop) => Ok(*prop),
            None => {
                let proto = self.prototype;
                if proto.is_null() {
                    return Ok(Property::new_data_simple(Value::undefined()));
                };
                proto.get_property_by_value(factory, key)
            }
        }
    }

    pub fn get_property(&self, key: &str) -> Value {
        if let ObjectKind::Proxy(ref proxy) = self.kind {
            return proxy.target.get_property(key);
        }
        match self.property.get(key) {
            Some(prop) => prop.as_data().val,
            None => self.prototype.get_property(key),
        }
    }

    pub fn set_property(&mut self, key: String, val: Value) {
        if !self.property.contains_key(&key) {
            self.property_order.push(key.clone());
        }
        let property = self
            .property
            .entry(key)
            .or_insert_with(|| Property::new_data_simple(Value::undefined()));
        let data = property.as_data_mut();
        if data.writable {
            data.val = val;
        }
    }

    pub fn set_property_by_value(
        &mut self,
        allocator: &mut MemoryAllocator,
        key: Value,
        val_: Value,
    ) -> Result<(Option<Value>, bool), error::RuntimeError> {
        if let ObjectKind::Proxy(ref proxy) = self.kind {
            return proxy
                .target
                .get_object_info()
                .set_property_by_value(allocator, key, val_);
        }
        // Annoying
        if key.is_string() && key.into_str() == "__proto__" {
            if let Some(prop) = self.property.get_mut("__proto__") {
                return Ok(set_existing_property(prop, val_));
            }
            if let Some((setter, success)) = inherited_setter(self.prototype, "__proto__") {
                return Ok((setter, success));
            }
        }

        let array_index = match self.kind {
            ObjectKind::Array(_) => key
                .is_array_index()
                .or_else(|| key.is_canonical_numeric_index_string(allocator)),
            ObjectKind::TypedArray(_) => key
                .is_array_index()
                .or_else(|| key.is_canonical_numeric_index_string(allocator)),
            _ => None,
        };
        let key_string = key.to_string();

        if !key.is_symbol() && string_object_index_exists(self, &key_string) {
            return Ok((None, false));
        }

        if let Some(idx) = array_index {
            if let ObjectKind::TypedArray(_) = self.kind {
                let this = Value::Object(self as *mut Object);
                if let Some(success) =
                    crate::builtins::typed_array::typed_array_set_index(this, idx, val_)
                {
                    return Ok((None, success));
                }
            }

            if let Some(prop) = self.property.get_mut(&key_string) {
                return Ok(set_existing_property(prop, val_));
            }

            if let ObjectKind::Array(ref mut info) = self.kind {
                if idx >= info.length && !info.length_writable {
                    return Ok((None, false));
                }
                let present = idx < info.elems.len()
                    && info.elems[idx]
                        .get_data()
                        .map(|data| !data.val.is_empty())
                        .unwrap_or(true);
                if !present && !self.extensible {
                    return Ok((None, false));
                }
                return Ok(info.set_element(idx, val_));
            }
        }

        match self.kind {
            ObjectKind::Array(ref mut info) => {
                if key.is_string() && key.into_str() == "length" {
                    if let Some(new_length) = array_length_from_value(allocator, val_) {
                        if !info.length_writable && new_length != info.length {
                            return Ok((None, false));
                        }
                        info.set_length(new_length);
                        return Ok((None, true));
                    }
                    return Err(error::RuntimeError::rangeerr("Invalid array length"));
                }
            }
            _ => {}
        }

        let property = if key.is_symbol() {
            let id = key.get_symbol_info().id;
            if !self.sym_property.contains_key(&id) && !self.extensible {
                return Ok((None, false));
            }
            if !self.sym_property.contains_key(&id) {
                self.sym_property_order.push(key);
            }
            self.sym_property
                .entry(id)
                .or_insert_with(|| Property::new_data_simple(Value::undefined()))
        } else {
            if !self.property.contains_key(&key_string) && !self.extensible {
                return Ok((None, false));
            }
            if !self.property.contains_key(&key_string) {
                self.property_order.push(key_string.clone());
            }
            self.property
                .entry(key_string)
                .or_insert_with(|| Property::new_data_simple(Value::undefined()))
        };

        Ok(set_existing_property(property, val_))
    }

    pub fn delete_property_by_value(
        &mut self,
        allocator: &mut MemoryAllocator,
        key: Value,
    ) -> Result<bool, error::RuntimeError> {
        if key.is_symbol() {
            let id = key.get_symbol_info().id;
            return match self.sym_property.get(&id) {
                Some(prop) if !prop.configurable() => Ok(false),
                Some(_) => {
                    self.sym_property_order
                        .retain(|symbol| symbol.get_symbol_info().id != id);
                    Ok(self.sym_property.remove(&id).is_some())
                }
                None => Ok(true),
            };
        }

        let key_string = key.to_string();
        if string_object_index_exists(self, &key_string) {
            return Ok(false);
        }

        match self.kind {
            ObjectKind::Array(ref mut info) => {
                if key.is_string() && key.into_str() == "length" {
                    return Ok(false);
                }

                let index = key
                    .is_array_index()
                    .or_else(|| key.is_canonical_numeric_index_string(allocator));
                if let Some(idx) = index {
                    let key_string = key.to_string();
                    if let Some(prop) = self.property.get(&key_string) {
                        if !prop.configurable() {
                            return Ok(false);
                        }
                        return Ok(self.remove_property(&key_string).is_some());
                    }
                    if idx >= info.elems.len() {
                        return Ok(true);
                    }
                    if !info.elems[idx].configurable() {
                        return Ok(false);
                    }
                    info.elems[idx] = Property::new_data_simple(Value::empty());
                    return Ok(true);
                }
            }
            _ => {}
        }

        match self.property.get(&key_string) {
            Some(prop) if !prop.configurable() => Ok(false),
            Some(_) => Ok(self.remove_property(&key_string).is_some()),
            None => Ok(true),
        }
    }
}

pub fn property_order_from_map(property: &FxHashMap<String, Property>) -> Vec<String> {
    let mut order = property
        .keys()
        .filter(|key| array_index_key(key).is_none())
        .cloned()
        .collect::<Vec<_>>();
    order.sort();
    order
}

pub fn array_index_key(key: &str) -> Option<u32> {
    let index = key.parse::<u32>().ok()?;
    if key == index.to_string() && index != u32::MAX {
        Some(index)
    } else {
        None
    }
}

pub fn property_is_enumerable(prop: &Property) -> bool {
    match prop {
        Property::Data(data) => data.enumerable,
        Property::Accessor(accessor) => accessor.enumerable,
    }
}

fn property_is_present(prop: &Property) -> bool {
    prop.get_data()
        .map(|data| !data.val.is_empty())
        .unwrap_or(true)
}

fn push_index_key(keys: &mut Vec<u32>, key: &str) {
    if let Some(index) = array_index_key(key) {
        keys.push(index);
    }
}

fn is_internal_slot_key(key: &str) -> bool {
    matches!(
        key,
        "__string_data" | "__number_data" | "__boolean_data" | "__symbol_data" | "__bigint_data"
    )
}

fn string_object_data(object: &Object) -> Option<String> {
    object
        .property
        .get("__string_data")
        .and_then(|prop| prop.get_data())
        .map(|data| data.val)
        .filter(|value| value.is_string())
        .map(|value| value.to_string())
}

fn string_index(key: &str) -> Option<usize> {
    let index = key.parse::<usize>().ok()?;
    if key == index.to_string() {
        Some(index)
    } else {
        None
    }
}

fn string_object_index_exists(object: &Object, key: &str) -> bool {
    let Some(index) = string_index(key) else {
        return false;
    };
    string_object_data(object)
        .map(|string| index < string.chars().count())
        .unwrap_or(false)
}

fn string_object_index_property(
    factory: &mut Factory,
    object: &Object,
    key: &str,
) -> Option<Property> {
    let index = string_index(key)?;
    let string = string_object_data(object)?;
    let value = string.chars().nth(index)?;
    Some(Property::Data(DataProperty {
        val: factory.string(value.to_string()),
        writable: false,
        enumerable: true,
        configurable: false,
    }))
}

fn inherited_setter(mut object: Value, key: &str) -> Option<(Option<Value>, bool)> {
    while let Value::Object(info) = object {
        let info = ObjectRef(info);
        if let Some(prop) = info.property.get(key) {
            return Some(match prop {
                Property::Accessor(AccessorProperty { set, .. }) => {
                    if set.is_undefined() {
                        (None, false)
                    } else {
                        (Some(*set), true)
                    }
                }
                Property::Data(data) if !data.writable => (None, false),
                Property::Data(_) => return None,
            });
        }
        object = info.prototype;
    }
    None
}

fn set_existing_property(prop: &mut Property, val_: Value) -> (Option<Value>, bool) {
    match prop {
        Property::Data(DataProperty {
            ref mut val,
            writable,
            ..
        }) => {
            if *writable {
                *val = val_;
                return (None, true);
            }
            (None, false)
        }
        Property::Accessor(AccessorProperty { set, .. }) => {
            if set.is_undefined() {
                (None, false)
            } else {
                (Some(*set), true)
            }
        }
    }
}

fn array_length_from_value(allocator: &mut MemoryAllocator, val: Value) -> Option<usize> {
    let num = val.to_number(allocator);
    if num.is_finite() && num >= 0.0 && num.trunc() == num && num <= u32::MAX as f64 {
        Some(num as usize)
    } else {
        None
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

    pub fn configurable(&self) -> bool {
        match self {
            Property::Data(data) => data.configurable,
            Property::Accessor(accessor) => accessor.configurable,
        }
    }
}

impl DataProperty {
    pub fn new(val: Value) -> Self {
        Self {
            val,
            writable: false,
            enumerable: false,
            configurable: false,
        }
    }

    pub fn set_writable(mut self) -> Self {
        self.writable = true;
        self
    }

    pub fn set_enumerate(mut self) -> Self {
        self.enumerable = true;
        self
    }

    pub fn set_configurable(mut self) -> Self {
        self.configurable = true;
        self
    }
}

impl From<DataProperty> for Property {
    fn from(data: DataProperty) -> Self {
        Property::Data(data)
    }
}
