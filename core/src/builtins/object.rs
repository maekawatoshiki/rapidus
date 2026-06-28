use super::helpers::{primitive_wrapper_object, to_object};
use crate::vm::{
    error::RuntimeError,
    jsvalue::symbol::{SYMBOL_ITERATOR_ID, SYMBOL_TO_PRIMITIVE_ID, SYMBOL_TO_STRING_TAG_ID},
    jsvalue::value::*,
    vm::{Factory, VMValueResult, VM},
};
use rustc_hash::FxHashMap;

pub fn object(factory: &mut Factory) -> Value {
    let obj = factory.generate_builtin_constructor(
        "Object",
        object_constructor,
        factory.object_prototypes.object,
    );
    obj.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
    );
    let get_own_property_descriptor = factory.builtin_function(
        "getOwnPropertyDescriptor",
        object_get_own_property_descriptor,
    );
    let get_own_property_descriptors = factory.builtin_function(
        "getOwnPropertyDescriptors",
        object_get_own_property_descriptors,
    );
    let define_property = factory.builtin_function("defineProperty", object_define_property);
    let define_properties = factory.builtin_function("defineProperties", object_define_properties);
    let create = factory.builtin_function("create", object_create);
    let assign = factory.builtin_function("assign", object_assign);
    let from_entries = factory.builtin_function("fromEntries", object_from_entries);
    let has_own = factory.builtin_function("hasOwn", object_has_own);
    let entries = factory.builtin_function("entries", object_entries);
    let keys = factory.builtin_function("keys", object_keys);
    let values = factory.builtin_function("values", object_values);
    let is = factory.builtin_function("is", object_is);
    let prevent_extensions =
        factory.builtin_function("preventExtensions", object_prevent_extensions);
    let is_extensible = factory.builtin_function("isExtensible", object_is_extensible);
    let set_prototype_of = factory.builtin_function("setPrototypeOf", object_set_prototype_of);
    let seal = factory.builtin_function("seal", object_seal);
    let freeze = factory.builtin_function("freeze", object_freeze);
    let is_sealed = factory.builtin_function("isSealed", object_is_sealed);
    let is_frozen = factory.builtin_function("isFrozen", object_is_frozen);
    let get_prototype_of = factory.builtin_function("getPrototypeOf", object_get_prototype_of);
    let get_own_property_names =
        factory.builtin_function("getOwnPropertyNames", object_get_own_property_names);
    let get_own_property_symbols =
        factory.builtin_function("getOwnPropertySymbols", object_get_own_property_symbols);
    set_builtin_length(get_own_property_descriptor, 2.0);
    set_builtin_length(get_own_property_descriptors, 1.0);
    set_builtin_length(define_property, 3.0);
    set_builtin_length(define_properties, 2.0);
    set_builtin_length(create, 2.0);
    set_builtin_length(assign, 2.0);
    set_builtin_length(from_entries, 1.0);
    set_builtin_length(has_own, 2.0);
    set_builtin_length(entries, 1.0);
    set_builtin_length(keys, 1.0);
    set_builtin_length(values, 1.0);
    set_builtin_length(is, 2.0);
    set_builtin_length(prevent_extensions, 1.0);
    set_builtin_length(is_extensible, 1.0);
    set_builtin_length(set_prototype_of, 2.0);
    set_builtin_length(seal, 1.0);
    set_builtin_length(freeze, 1.0);
    set_builtin_length(is_sealed, 1.0);
    set_builtin_length(is_frozen, 1.0);
    set_builtin_length(get_prototype_of, 1.0);
    set_builtin_length(get_own_property_names, 1.0);
    set_builtin_length(get_own_property_symbols, 1.0);
    set_builtin_property(obj, "getOwnPropertyDescriptor", get_own_property_descriptor);
    set_builtin_property(
        obj,
        "getOwnPropertyDescriptors",
        get_own_property_descriptors,
    );
    set_builtin_property(obj, "defineProperty", define_property);
    set_builtin_property(obj, "defineProperties", define_properties);
    set_builtin_property(obj, "create", create);
    set_builtin_property(obj, "assign", assign);
    set_builtin_property(obj, "fromEntries", from_entries);
    set_builtin_property(obj, "hasOwn", has_own);
    set_builtin_property(obj, "entries", entries);
    set_builtin_property(obj, "keys", keys);
    set_builtin_property(obj, "values", values);
    set_builtin_property(obj, "is", is);
    set_builtin_property(obj, "preventExtensions", prevent_extensions);
    set_builtin_property(obj, "isExtensible", is_extensible);
    set_builtin_property(obj, "setPrototypeOf", set_prototype_of);
    set_builtin_property(obj, "seal", seal);
    set_builtin_property(obj, "freeze", freeze);
    set_builtin_property(obj, "isSealed", is_sealed);
    set_builtin_property(obj, "isFrozen", is_frozen);
    set_builtin_property(obj, "getPrototypeOf", get_prototype_of);
    set_builtin_property(obj, "getOwnPropertyNames", get_own_property_names);
    set_builtin_property(obj, "getOwnPropertySymbols", get_own_property_symbols);
    obj
}

fn set_builtin_property(obj: Value, name: &str, func: Value) {
    obj.get_object_info().insert_property(
        name.to_string(),
        Property::new_data(DataProperty::new(func).set_writable().set_configurable()),
    );
}

fn set_builtin_length(func: Value, len: f64) {
    func.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(len)).set_configurable()),
    );
}

pub fn object_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    if args.len() == 0 {
        let empty_obj = vm.factory.object(FxHashMap::default());
        vm.current_context.stack.push(empty_obj.into());
        return Ok(empty_obj);
    }

    match &args[0] {
        Value::Other(NULL) | Value::Other(UNDEFINED) => {
            let empty_obj = vm.factory.object(FxHashMap::default());
            Ok(empty_obj)
        }
        Value::String(_) | Value::Number(_) | Value::Bool(_) => {
            Ok(primitive_wrapper_object(vm, args[0]).unwrap())
        }
        _ if args[0].is_symbol() || args[0].is_bigint() => {
            Ok(primitive_wrapper_object(vm, args[0]).unwrap())
        }
        Value::Object(_) => Ok(args[0]),
        Value::Other(EMPTY) => unreachable!(),
        _ => Ok(args[0]),
    }
}

pub fn object_from_entries(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let iterable = *args.get(0).unwrap_or(&Value::undefined());
    if iterable.is_null() || iterable.is_undefined() {
        return Err(vm.current_context.error_type("Object.fromEntries"));
    }

    let iterator_key = vm.factory.well_known_symbol(SYMBOL_ITERATOR_ID);
    let using_iterator = vm.get_property_by_value(iterable, iterator_key)?;
    if !using_iterator.is_function_object() {
        return Err(vm.current_context.error_type("Object.fromEntries"));
    }

    let iterator = vm.call_function(using_iterator, &[], iterable)?;
    if !iterator.is_object() {
        return Err(vm.current_context.error_type("Object.fromEntries"));
    }

    let next_key = vm.factory.string("next".to_string());
    let next_method = vm.get_property_by_value(iterator, next_key)?;
    if !next_method.is_function_object() {
        return Err(vm.current_context.error_type("Object.fromEntries"));
    }

    let result = vm.factory.object(FxHashMap::default());
    loop {
        let next = vm.call_function(next_method, &[], iterator)?;
        if !next.is_object() {
            return Err(vm.current_context.error_type("Object.fromEntries"));
        }

        let done_key = vm.factory.string("done".to_string());
        if vm.get_property_by_value(next, done_key)?.to_boolean() {
            return Ok(result);
        }

        let value_key = vm.factory.string("value".to_string());
        let next_item = vm.get_property_by_value(next, value_key)?;
        if !next_item.is_object() {
            let err = vm.current_context.error_type("Object.fromEntries");
            return Err(close_iterator_or_original(vm, iterator, err));
        }

        let key_key = vm.factory.string("0".to_string());
        let key = match vm.get_property_by_value(next_item, key_key) {
            Ok(key) => key,
            Err(err) => return Err(close_iterator_or_original(vm, iterator, err)),
        };
        let value_key = vm.factory.string("1".to_string());
        let value = match vm.get_property_by_value(next_item, value_key) {
            Ok(value) => value,
            Err(err) => return Err(close_iterator_or_original(vm, iterator, err)),
        };
        let key = match to_property_key(vm, key) {
            Ok(key) => key,
            Err(err) => return Err(close_iterator_or_original(vm, iterator, err)),
        };

        if let Err(err) = create_data_property(result, key, value, vm) {
            return Err(close_iterator_or_original(vm, iterator, err));
        }
    }
}

fn close_iterator_or_original(
    vm: &mut VM,
    iterator: Value,
    original: crate::vm::error::RuntimeError,
) -> crate::vm::error::RuntimeError {
    match iterator_close_for_error(vm, iterator) {
        Ok(()) => original,
        Err(close_err) => close_err,
    }
}

fn iterator_close_for_error(
    vm: &mut VM,
    iterator: Value,
) -> Result<(), crate::vm::error::RuntimeError> {
    let return_key = vm.factory.string("return".to_string());
    let return_method = vm.get_property_by_value(iterator, return_key)?;
    if return_method.is_null() || return_method.is_undefined() {
        return Ok(());
    }
    if !return_method.is_function_object() {
        return Err(vm.current_context.error_type("Iterator return"));
    }
    vm.call_function(return_method, &[], iterator)?;
    Ok(())
}

fn create_data_property(
    obj: Value,
    key: PropertyKey,
    value: Value,
    vm: &mut VM,
) -> Result<(), crate::vm::error::RuntimeError> {
    let mut info = obj.get_object_info();
    if !info.extensible {
        return Err(vm.current_context.error_type("Object.fromEntries"));
    }
    match key {
        PropertyKey::String(key) => {
            info.insert_property(key, Property::new_data_simple(value));
        }
        PropertyKey::Symbol(sym) => {
            super::helpers::define_symbol_property(obj, sym, Property::new_data_simple(value));
        }
    }
    Ok(())
}

pub fn object_get_own_property_descriptor(
    vm: &mut VM,
    args: &[Value],
    _this: Value,
) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    let key_value = *args.get(1).unwrap_or(&Value::undefined());
    if key_value.is_symbol() {
        if let Value::Object(info) = obj {
            let id = key_value.get_symbol_info().id;
            if let Some(prop) = ObjectRef(info).sym_property.get(&id) {
                return Ok(property_to_descriptor_object(vm, *prop));
            }
        }
        return Ok(Value::undefined());
    }

    let key = to_property_key_string(vm, key_value)?;
    if let Value::Object(info) = obj {
        if let Some(prop) = string_index_property(vm, obj, &key) {
            return Ok(property_to_descriptor_object(vm, prop));
        }
        if let ObjectKind::Array(ref array) = ObjectRef(info).kind {
            if key == "length" {
                return Ok(make_normal_object!(
                    vm.factory,
                    value => true, true, true: Value::Number(array.get_length() as f64),
                    writable => true, true, true: Value::bool(array.length_writable),
                    enumerable => true, true, true: Value::bool(false),
                    configurable => true, true, true: Value::bool(false)
                ));
            }
            if let Ok(idx) = key.parse::<usize>() {
                if idx < array.elems.len() {
                    match array.elems[idx] {
                        Property::Data(data) => {
                            if data.val.is_empty() {
                                return Ok(Value::undefined());
                            }
                            return Ok(make_normal_object!(
                                vm.factory,
                                value => true, true, true: data.val,
                                writable => true, true, true: Value::bool(data.writable),
                                enumerable => true, true, true: Value::bool(data.enumerable),
                                configurable => true, true, true: Value::bool(data.configurable)
                            ));
                        }
                        Property::Accessor(accessor) => {
                            return Ok(make_normal_object!(
                                vm.factory,
                                get => true, true, true: accessor.get,
                                set => true, true, true: accessor.set,
                                enumerable => true, true, true: Value::bool(accessor.enumerable),
                                configurable => true, true, true: Value::bool(accessor.configurable)
                            ));
                        }
                    }
                }
            }
        }
    }
    let prop = match obj
        .get_object_properties()
        .and_then(|props| props.get(&key))
    {
        Some(prop) => *prop,
        None => return Ok(Value::undefined()),
    };

    Ok(property_to_descriptor_object(vm, prop))
}

pub fn object_get_own_property_descriptors(
    vm: &mut VM,
    args: &[Value],
    _this: Value,
) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    if obj.is_null() || obj.is_undefined() {
        return Err(vm
            .current_context
            .error_type("Object.getOwnPropertyDescriptors"));
    }

    let descriptors = vm.factory.object(FxHashMap::default());
    if !obj.is_object() {
        if obj.is_string() {
            define_string_value_descriptors(vm, descriptors, obj.to_string());
        }
        return Ok(descriptors);
    }

    let string_keys = obj.get_object_info().own_string_property_keys();
    for key in string_keys {
        let key_value = vm.factory.string(key.clone());
        let desc = object_get_own_property_descriptor(vm, &[obj, key_value], Value::undefined())?;
        if !desc.is_undefined() {
            descriptors
                .get_object_info()
                .insert_property(key, Property::new_data_simple(desc));
        }
    }

    let symbol_keys = {
        let info = obj.get_object_info();
        info.sym_property_order
            .iter()
            .filter(|symbol| info.sym_property.contains_key(&symbol.get_symbol_info().id))
            .copied()
            .collect::<Vec<_>>()
    };
    for symbol in symbol_keys {
        let desc = object_get_own_property_descriptor(vm, &[obj, symbol], Value::undefined())?;
        if !desc.is_undefined() {
            let id = symbol.get_symbol_info().id;
            let mut info = descriptors.get_object_info();
            if !info.sym_property.contains_key(&id) {
                info.sym_property_order.push(symbol);
            }
            info.sym_property
                .insert(id, Property::new_data_simple(desc));
        }
    }

    Ok(descriptors)
}

fn define_string_value_descriptors(vm: &mut VM, descriptors: Value, string: String) {
    for (index, ch) in string.chars().enumerate() {
        let prop = Property::Data(DataProperty {
            val: vm.factory.string(ch.to_string()),
            writable: false,
            enumerable: true,
            configurable: false,
        });
        let desc = property_to_descriptor_object(vm, prop);
        descriptors
            .get_object_info()
            .insert_property(index.to_string(), Property::new_data_simple(desc));
    }

    let length = string.chars().count();
    let prop = Property::Data(DataProperty {
        val: Value::Number(length as f64),
        writable: false,
        enumerable: false,
        configurable: false,
    });
    let desc = property_to_descriptor_object(vm, prop);
    descriptors
        .get_object_info()
        .insert_property("length".to_string(), Property::new_data_simple(desc));
}

fn property_to_descriptor_object(vm: &mut VM, prop: Property) -> Value {
    match prop {
        Property::Data(data) => make_normal_object!(
            vm.factory,
            value => true, true, true: data.val,
            writable => true, true, true: Value::bool(data.writable),
            enumerable => true, true, true: Value::bool(data.enumerable),
            configurable => true, true, true: Value::bool(data.configurable)
        ),
        Property::Accessor(accessor) => make_normal_object!(
            vm.factory,
            get => true, true, true: accessor.get,
            set => true, true, true: accessor.set,
            enumerable => true, true, true: Value::bool(accessor.enumerable),
            configurable => true, true, true: Value::bool(accessor.configurable)
        ),
    }
}

fn descriptor_has(vm: &mut VM, desc: Value, name: &str) -> Result<bool, RuntimeError> {
    let key = vm.factory.string(name.to_string());
    Ok(vm.has_property(key, desc)?.to_boolean())
}

fn descriptor_get(vm: &mut VM, desc: Value, name: &str) -> Result<Value, RuntimeError> {
    let key = vm.factory.string(name.to_string());
    vm.get_property_by_value(desc, key)
}

pub fn object_define_property(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    let key_value = *args.get(1).unwrap_or(&Value::undefined());
    let desc = *args.get(2).unwrap_or(&Value::undefined());
    if !obj.is_object() || obj.is_symbol() || !desc.is_object() || desc.is_symbol() {
        return Err(vm.current_context.error_type("Object.defineProperty"));
    }
    if key_value.is_symbol() {
        define_symbol_property(vm, obj, key_value, desc)?;
        return Ok(obj);
    }
    let key = to_property_key_string(vm, key_value)?;

    let has_value = descriptor_has(vm, desc, "value")?;
    let has_get = descriptor_has(vm, desc, "get")?;
    let has_set = descriptor_has(vm, desc, "set")?;
    let has_writable = descriptor_has(vm, desc, "writable")?;
    let has_enumerable = descriptor_has(vm, desc, "enumerable")?;
    let has_configurable = descriptor_has(vm, desc, "configurable")?;
    if (has_get || has_set) && (has_value || has_writable) {
        return Err(vm.current_context.error_type("Object.defineProperty"));
    }
    let existing = own_string_property(obj, &key);
    if existing.is_none() && !obj.get_object_info().extensible {
        return Err(vm.current_context.error_type("Object is not extensible"));
    }
    let existing_data = existing.and_then(|prop| prop.get_data().copied());
    let existing_accessor = existing.and_then(|prop| match prop {
        Property::Accessor(accessor) => Some(accessor),
        _ => None,
    });
    let enumerable = if has_enumerable {
        descriptor_get(vm, desc, "enumerable")?.to_boolean()
    } else if let Some(data) = existing_data {
        data.enumerable
    } else {
        existing_accessor
            .map(|accessor| accessor.enumerable)
            .unwrap_or(false)
    };
    let configurable = if has_configurable {
        descriptor_get(vm, desc, "configurable")?.to_boolean()
    } else if let Some(data) = existing_data {
        data.configurable
    } else {
        existing_accessor
            .map(|accessor| accessor.configurable)
            .unwrap_or(false)
    };

    if let Some(existing) = existing {
        match existing {
            Property::Data(data) if !data.configurable => {
                if has_configurable && configurable {
                    return Err(vm
                        .current_context
                        .error_type("Cannot redefine non-configurable property"));
                }
                if has_enumerable && enumerable != data.enumerable {
                    return Err(vm
                        .current_context
                        .error_type("Cannot redefine non-configurable property"));
                }
                if has_get || has_set {
                    return Err(vm
                        .current_context
                        .error_type("Cannot redefine non-configurable property"));
                }
            }
            Property::Accessor(accessor) if !accessor.configurable => {
                if has_configurable && configurable {
                    return Err(vm
                        .current_context
                        .error_type("Cannot redefine non-configurable property"));
                }
                if has_enumerable && enumerable != accessor.enumerable {
                    return Err(vm
                        .current_context
                        .error_type("Cannot redefine non-configurable property"));
                }
                if has_value || has_writable {
                    return Err(vm
                        .current_context
                        .error_type("Cannot redefine non-configurable property"));
                }
            }
            _ => {}
        }
    }

    if has_get || has_set {
        let get = if has_get {
            descriptor_get(vm, desc, "get")?
        } else {
            existing_accessor
                .map(|accessor| accessor.get)
                .unwrap_or(Value::undefined())
        };
        let set = if has_set {
            descriptor_get(vm, desc, "set")?
        } else {
            existing_accessor
                .map(|accessor| accessor.set)
                .unwrap_or(Value::undefined())
        };
        if (!get.is_undefined() && !get.is_function_object())
            || (!set.is_undefined() && !set.is_function_object())
        {
            return Err(vm.current_context.error_type("Object.defineProperty"));
        }
        if let Some(accessor) = existing_accessor {
            if !accessor.configurable {
                if has_get && !get.strict_eq_bool(accessor.get) {
                    return Err(vm
                        .current_context
                        .error_type("Cannot redefine non-configurable property"));
                }
                if has_set && !set.strict_eq_bool(accessor.set) {
                    return Err(vm
                        .current_context
                        .error_type("Cannot redefine non-configurable property"));
                }
            }
        }
        define_own_string_property(
            vm,
            obj,
            &key,
            Property::Accessor(AccessorProperty {
                get,
                set,
                enumerable,
                configurable,
            }),
        )?;
        vm.delete_mapped_argument(obj, &key);
        return Ok(obj);
    }

    if !has_value && !has_writable {
        if let Some(accessor) = existing_accessor {
            define_own_string_property(
                vm,
                obj,
                &key,
                Property::Accessor(AccessorProperty {
                    get: accessor.get,
                    set: accessor.set,
                    enumerable,
                    configurable,
                }),
            )?;
            vm.delete_mapped_argument(obj, &key);
            return Ok(obj);
        }
    }

    let value = if has_value {
        descriptor_get(vm, desc, "value")?
    } else {
        existing_data
            .map(|data| data.val)
            .unwrap_or(Value::undefined())
    };
    let writable = if has_writable {
        descriptor_get(vm, desc, "writable")?.to_boolean()
    } else {
        existing_data.map(|data| data.writable).unwrap_or(false)
    };
    if let Some(data) = existing_data {
        if !data.configurable && !data.writable {
            if has_writable && writable {
                return Err(vm
                    .current_context
                    .error_type("Cannot redefine non-configurable property"));
            }
            if has_value && !same_value(value, data.val) {
                return Err(vm
                    .current_context
                    .error_type("Cannot redefine non-configurable property"));
            }
        }
    }
    define_own_string_property(
        vm,
        obj,
        &key,
        Property::Data(DataProperty {
            val: value,
            writable,
            enumerable,
            configurable,
        }),
    )?;
    if has_value {
        vm.set_mapped_argument(obj, &key, value)?;
    }
    if has_writable && !writable {
        vm.delete_mapped_argument(obj, &key);
    }
    Ok(obj)
}

fn define_own_string_property(
    vm: &mut VM,
    obj: Value,
    key: &str,
    property: Property,
) -> Result<(), crate::vm::error::RuntimeError> {
    let mut info = obj.get_object_info();
    let extensible = info.extensible;
    if let ObjectKind::Array(ref mut array) = info.kind {
        if key == "length" {
            return define_array_length_property(vm, array, property);
        }
        if let Some(index) = array_index_key(key).map(|index| index as usize) {
            if index >= array.length && !array.length_writable {
                return Err(vm
                    .current_context
                    .error_type("Cannot define array index past non-writable length"));
            }
            let present = index < array.elems.len()
                && array.elems[index]
                    .get_data()
                    .map(|data| !data.val.is_empty())
                    .unwrap_or(true);
            if !present && !extensible {
                return Err(vm.current_context.error_type("Object is not extensible"));
            }
            while array.elems.len() <= index {
                array.elems.push(Property::new_data_simple(Value::empty()));
            }
            array.length = array.length.max(index + 1);
            array.elems[index] = property;
            return Ok(());
        }
    }

    info.insert_property(key.to_string(), property);
    Ok(())
}

fn define_array_length_property(
    vm: &mut VM,
    array: &mut ArrayObjectInfo,
    property: Property,
) -> Result<(), RuntimeError> {
    let Some(data) = property.get_data() else {
        return Err(vm
            .current_context
            .error_type("Invalid array length descriptor"));
    };
    let Some(new_len) = array_length_from_value(vm, data.val) else {
        return Err(vm.current_context.error_range("Invalid array length"));
    };
    if !array.length_writable && new_len != array.length {
        return Err(vm
            .current_context
            .error_type("Cannot redefine non-writable array length"));
    }

    if new_len < array.length {
        let end = array.length.min(array.elems.len());
        for index in (new_len..end).rev() {
            if !array.elems[index].configurable() {
                array.length = index + 1;
                if !data.writable {
                    array.length_writable = false;
                }
                return Err(vm
                    .current_context
                    .error_type("Cannot delete non-configurable array element"));
            }
            array.elems[index] = Property::new_data_simple(Value::empty());
        }
        array.set_length(new_len);
    } else {
        array.length = new_len;
    }
    array.length_writable = data.writable;
    Ok(())
}

fn array_length_from_value(vm: &mut VM, val: Value) -> Option<usize> {
    let num = val.to_number(&mut vm.factory.memory_allocator);
    if num.is_finite() && num >= 0.0 && num.trunc() == num && num <= u32::MAX as f64 {
        Some(num as usize)
    } else {
        None
    }
}

fn define_symbol_property(
    vm: &mut VM,
    obj: Value,
    key: Value,
    desc: Value,
) -> Result<(), crate::vm::error::RuntimeError> {
    let id = key.get_symbol_info().id;
    let existing = obj.get_object_info().sym_property.get(&id).copied();
    if existing.is_none() && !obj.get_object_info().extensible {
        return Err(vm.current_context.error_type("Object is not extensible"));
    }

    let has_value = descriptor_has(vm, desc, "value")?;
    let has_get = descriptor_has(vm, desc, "get")?;
    let has_set = descriptor_has(vm, desc, "set")?;
    let has_writable = descriptor_has(vm, desc, "writable")?;
    let has_enumerable = descriptor_has(vm, desc, "enumerable")?;
    let has_configurable = descriptor_has(vm, desc, "configurable")?;
    if (has_get || has_set) && (has_value || has_writable) {
        return Err(vm.current_context.error_type("Object.defineProperty"));
    }
    let existing_data = existing.and_then(|prop| prop.get_data().copied());
    let existing_accessor = existing.and_then(|prop| match prop {
        Property::Accessor(accessor) => Some(accessor),
        _ => None,
    });

    let enumerable = if has_enumerable {
        descriptor_get(vm, desc, "enumerable")?.to_boolean()
    } else if let Some(data) = existing_data {
        data.enumerable
    } else {
        existing_accessor
            .map(|accessor| accessor.enumerable)
            .unwrap_or(false)
    };
    let configurable = if has_configurable {
        descriptor_get(vm, desc, "configurable")?.to_boolean()
    } else if let Some(data) = existing_data {
        data.configurable
    } else {
        existing_accessor
            .map(|accessor| accessor.configurable)
            .unwrap_or(false)
    };

    let property = if has_get || has_set {
        let get = if has_get {
            descriptor_get(vm, desc, "get")?
        } else {
            existing_accessor
                .map(|accessor| accessor.get)
                .unwrap_or(Value::undefined())
        };
        let set = if has_set {
            descriptor_get(vm, desc, "set")?
        } else {
            existing_accessor
                .map(|accessor| accessor.set)
                .unwrap_or(Value::undefined())
        };
        if (!get.is_undefined() && !get.is_function_object())
            || (!set.is_undefined() && !set.is_function_object())
        {
            return Err(vm.current_context.error_type("Object.defineProperty"));
        }
        Property::Accessor(AccessorProperty {
            get,
            set,
            enumerable,
            configurable,
        })
    } else if !has_value && !has_writable {
        if let Some(accessor) = existing_accessor {
            Property::Accessor(AccessorProperty {
                get: accessor.get,
                set: accessor.set,
                enumerable,
                configurable,
            })
        } else {
            Property::Data(DataProperty {
                val: existing_data
                    .map(|data| data.val)
                    .unwrap_or(Value::undefined()),
                writable: existing_data.map(|data| data.writable).unwrap_or(false),
                enumerable,
                configurable,
            })
        }
    } else {
        Property::Data(DataProperty {
            val: if has_value {
                descriptor_get(vm, desc, "value")?
            } else {
                existing_data
                    .map(|data| data.val)
                    .unwrap_or(Value::undefined())
            },
            writable: if has_writable {
                descriptor_get(vm, desc, "writable")?.to_boolean()
            } else {
                existing_data.map(|data| data.writable).unwrap_or(false)
            },
            enumerable,
            configurable,
        })
    };

    super::helpers::define_symbol_property(obj, key, property);
    Ok(())
}

pub fn object_create(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let proto = *args.get(0).unwrap_or(&Value::undefined());
    if !proto.is_object() && !proto.is_null() {
        return Err(vm.current_context.error_type("Object.create"));
    }

    let obj = vm.factory.object(FxHashMap::default());
    obj.get_object_info().prototype = proto;
    if let Some(properties) = args.get(1) {
        if !properties.is_undefined() {
            object_define_properties(vm, &[obj, *properties], Value::undefined())?;
        }
    }
    Ok(obj)
}

pub fn object_assign(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = *args.get(0).unwrap_or(&Value::undefined());
    if target.is_null() || target.is_undefined() {
        return Err(vm.current_context.error_type("Object.assign"));
    }
    let to = to_object(vm, target)?;

    for source in args.iter().skip(1) {
        if source.is_null() || source.is_undefined() {
            continue;
        }
        let source = to_object(vm, *source)?;
        for key in enumerable_own_string_keys(source) {
            let key_value = vm.factory.string(key.clone());
            let value = vm.get_property_by_value(source, key_value)?;
            let to_key = vm.factory.string(key);
            vm.set_property_by_value_or_throw(to, to_key, value)?;
        }
        let info = source.get_object_info();
        for symbol in info.sym_property_order.clone() {
            let id = symbol.get_symbol_info().id;
            let Some(prop) = info.sym_property.get(&id).copied() else {
                continue;
            };
            if !property_is_enumerable(&prop) {
                continue;
            }
            let value = vm.get_property_by_value(source, symbol)?;
            vm.set_property_by_value_or_throw(to, symbol, value)?;
        }
    }
    Ok(to)
}

pub fn object_define_properties(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    let props = *args.get(1).unwrap_or(&Value::undefined());
    if !obj.is_object()
        || obj.is_symbol()
        || props.is_null()
        || props.is_undefined()
        || props.is_symbol()
    {
        return Err(vm.current_context.error_type("Object.defineProperties"));
    }
    let props = if props.is_object() {
        props
    } else {
        object_constructor(vm, &[props], Value::undefined())?
    };

    let mut descriptors = Vec::new();
    for key in enumerable_own_string_keys(props) {
        let key_value = vm.factory.string(key.clone());
        let desc = vm.get_property_by_value(props, key_value)?;
        if !desc.is_object() {
            return Err(vm.current_context.error_type("Object.defineProperties"));
        }
        descriptors.push((key, desc));
    }

    for (key, desc) in descriptors {
        let key_value = vm.factory.string(key);
        object_define_property(vm, &[obj, key_value, desc], Value::undefined())?;
    }

    Ok(obj)
}

pub fn object_has_own(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    if obj.is_null() || obj.is_undefined() {
        return Err(vm.current_context.error_type("Object.hasOwn"));
    }
    let key_value = *args.get(1).unwrap_or(&Value::undefined());
    let key = match to_property_key(vm, key_value)? {
        PropertyKey::Symbol(sym) => {
            let id = sym.get_symbol_info().id;
            return Ok(Value::bool(
                obj.is_object() && obj.get_object_info().sym_property.contains_key(&id),
            ));
        }
        PropertyKey::String(key) => key,
    };
    if !obj.is_object() {
        if obj.is_string() {
            return Ok(Value::bool(string_value_index_exists(
                obj.to_string(),
                &key,
            )));
        }
        return Ok(Value::bool(false));
    }
    if obj
        .get_object_properties()
        .map(|props| props.contains_key(&key))
        .unwrap_or(false)
    {
        return Ok(Value::bool(true));
    }
    if let ObjectKind::Array(ref array) = obj.get_object_info().kind {
        if key == "length" {
            return Ok(Value::bool(true));
        }
        if let Ok(index) = key.parse::<usize>() {
            return Ok(Value::bool(
                index < array.elems.len()
                    && array.elems[index]
                        .get_data()
                        .map(|data| !data.val.is_empty())
                        .unwrap_or(true),
            ));
        }
    }
    if string_index_exists(obj, &key) {
        return Ok(Value::bool(true));
    }
    Ok(Value::bool(false))
}

fn to_property_key_string(
    vm: &mut VM,
    key: Value,
) -> Result<String, crate::vm::error::RuntimeError> {
    Ok(match to_property_key(vm, key)? {
        PropertyKey::String(key) => key,
        PropertyKey::Symbol(_) => key.to_string(),
    })
}

enum PropertyKey {
    String(String),
    /// Carries the symbol Value itself (not just its id) so property
    /// definition can keep sym_property_order in sync with the canonical
    /// symbol instance.
    Symbol(Value),
}

fn to_property_key(vm: &mut VM, key: Value) -> Result<PropertyKey, crate::vm::error::RuntimeError> {
    if key.is_symbol() {
        return Ok(PropertyKey::Symbol(key));
    }
    if key.is_object() {
        let to_primitive_key = vm.factory.symbol_with_id(
            SYMBOL_TO_PRIMITIVE_ID,
            Some("Symbol.toPrimitive".to_string()),
        );
        let to_primitive = vm.get_property_by_value(key, to_primitive_key)?;
        if to_primitive.is_function_object() {
            let hint = vm.factory.string("string".to_string());
            let primitive = vm.call_function(to_primitive, &[hint], key)?;
            if primitive.is_object() && !primitive.is_symbol() && !primitive.is_bigint() {
                return Err(vm
                    .current_context
                    .error_type("Cannot convert object to property key"));
            }
            return primitive_to_property_key(primitive);
        }

        for internal in [
            "__string_data",
            "__number_data",
            "__boolean_data",
            "__symbol_data",
            "__bigint_data",
        ] {
            let value = key.get_property(internal);
            if !value.is_undefined() {
                return primitive_to_property_key(value);
            }
        }

        let to_string_key = vm.factory.string("toString".to_string());
        let to_string = vm.get_property_by_value(key, to_string_key)?;
        if to_string.is_function_object() {
            let primitive = vm.call_function(to_string, &[], key)?;
            if !primitive.is_object() || primitive.is_symbol() || primitive.is_bigint() {
                return primitive_to_property_key(primitive);
            }
        }

        let value_of_key = vm.factory.string("valueOf".to_string());
        let value_of = vm.get_property_by_value(key, value_of_key)?;
        if value_of.is_function_object() {
            let primitive = vm.call_function(value_of, &[], key)?;
            if !primitive.is_object() || primitive.is_symbol() || primitive.is_bigint() {
                return primitive_to_property_key(primitive);
            }
        }
    }
    primitive_to_property_key(key)
}

fn primitive_to_property_key(key: Value) -> Result<PropertyKey, crate::vm::error::RuntimeError> {
    if key.is_symbol() {
        Ok(PropertyKey::Symbol(key))
    } else {
        Ok(PropertyKey::String(key.to_string()))
    }
}

pub fn object_entries(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    if !obj.is_object() {
        if obj.is_null() || obj.is_undefined() {
            return Err(vm.current_context.error_type("Object.entries"));
        }
        if obj.is_string() {
            let elems = obj
                .to_string()
                .chars()
                .enumerate()
                .map(|(index, ch)| {
                    let key = vm.factory.string(index.to_string());
                    let value = vm.factory.string(ch.to_string());
                    Property::new_data_simple(vm.factory.array(vec![
                        Property::new_data_simple(key),
                        Property::new_data_simple(value),
                    ]))
                })
                .collect();
            return Ok(vm.factory.array(elems));
        }
        return Ok(vm.factory.array(Vec::new()));
    }
    let mut entries = Vec::new();
    for key in enumerable_own_string_keys(obj) {
        if !is_own_enumerable_string_property(obj, &key) {
            continue;
        }
        let key_value = vm.factory.string(key.clone());
        let value = vm.get_property_by_value(obj, key_value)?;
        let key_string = vm.factory.string(key);
        let entry = vm.factory.array(vec![
            Property::new_data_simple(key_string),
            Property::new_data_simple(value),
        ]);
        entries.push(Property::new_data_simple(entry));
    }
    Ok(vm.factory.array(entries))
}

pub fn object_keys(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    if !obj.is_object() {
        if obj.is_null() || obj.is_undefined() {
            return Err(vm.current_context.error_type("Object.keys"));
        }
        if obj.is_string() {
            let elems = obj
                .to_string()
                .chars()
                .enumerate()
                .map(|(index, _)| Property::new_data_simple(vm.factory.string(index.to_string())))
                .collect();
            return Ok(vm.factory.array(elems));
        }
        return Ok(vm.factory.array(Vec::new()));
    }
    let elems = enumerable_own_string_keys(obj)
        .into_iter()
        .map(|key| Property::new_data_simple(vm.factory.string(key)))
        .collect();
    Ok(vm.factory.array(elems))
}

pub fn object_values(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    if !obj.is_object() {
        if obj.is_null() || obj.is_undefined() {
            return Err(vm.current_context.error_type("Object.values"));
        }
        if obj.is_string() {
            let elems = obj
                .to_string()
                .chars()
                .map(|ch| Property::new_data_simple(vm.factory.string(ch.to_string())))
                .collect();
            return Ok(vm.factory.array(elems));
        }
        return Ok(vm.factory.array(Vec::new()));
    }
    let mut elems = Vec::new();
    for key in enumerable_own_string_keys(obj) {
        if !is_own_enumerable_string_property(obj, &key) {
            continue;
        }
        let key_value = vm.factory.string(key);
        let value = vm.get_property_by_value(obj, key_value)?;
        elems.push(Property::new_data_simple(value));
    }
    Ok(vm.factory.array(elems))
}

pub fn object_is(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let x = *args.get(0).unwrap_or(&Value::undefined());
    let y = *args.get(1).unwrap_or(&Value::undefined());
    Ok(Value::bool(same_value(x, y)))
}

pub fn object_prevent_extensions(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    if obj.is_object() {
        obj.get_object_info().extensible = false;
    }
    Ok(obj)
}

pub fn object_is_extensible(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    if !obj.is_object() {
        return Ok(Value::bool(false));
    }
    Ok(Value::bool(obj.get_object_info().extensible))
}

pub fn object_seal(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    if obj.is_object() {
        set_integrity_level(obj, false);
    }
    Ok(obj)
}

pub fn object_freeze(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    if obj.is_object() {
        set_integrity_level(obj, true);
    }
    Ok(obj)
}

pub fn object_is_sealed(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    Ok(Value::bool(test_integrity_level(obj, false)))
}

pub fn object_is_frozen(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    Ok(Value::bool(test_integrity_level(obj, true)))
}

fn same_value(x: Value, y: Value) -> bool {
    match (x, y) {
        (Value::Number(a), Value::Number(b)) => {
            (a.is_nan() && b.is_nan()) || (a == b && a.is_sign_negative() == b.is_sign_negative())
        }
        _ => x.strict_eq_bool(y),
    }
}

fn own_string_property(obj: Value, key: &str) -> Option<Property> {
    if !obj.is_object() {
        return None;
    }
    let info = obj.get_object_info();
    if let ObjectKind::Array(ref array) = info.kind {
        if key == "length" {
            return Some(Property::Data(DataProperty {
                val: Value::Number(array.get_length() as f64),
                writable: array.length_writable,
                enumerable: false,
                configurable: false,
            }));
        }
        if let Ok(index) = key.parse::<usize>() {
            if index < array.elems.len() {
                let prop = array.elems[index];
                if prop
                    .get_data()
                    .map(|data| !data.val.is_empty())
                    .unwrap_or(true)
                {
                    return Some(prop);
                }
            }
        }
    }
    info.property.get(key).copied()
}

fn is_own_enumerable_string_property(obj: Value, key: &str) -> bool {
    own_string_property(obj, key)
        .map(|prop| property_is_enumerable(&prop))
        .unwrap_or(false)
}

fn string_object_data(obj: Value) -> Option<String> {
    if !obj.is_object() {
        return None;
    }
    obj.get_object_info()
        .property
        .get("__string_data")
        .and_then(|prop| prop.get_data())
        .map(|data| data.val)
        .filter(|value| value.is_string())
        .map(|value| value.to_string())
}

fn string_index_exists(obj: Value, key: &str) -> bool {
    let Ok(index) = key.parse::<usize>() else {
        return false;
    };
    if key != index.to_string() {
        return false;
    }
    string_object_data(obj)
        .map(|string| index < string.chars().count())
        .unwrap_or(false)
}

fn string_value_index_exists(string: String, key: &str) -> bool {
    let Ok(index) = key.parse::<usize>() else {
        return false;
    };
    key == index.to_string() && index < string.chars().count()
}

fn string_index_property(vm: &mut VM, obj: Value, key: &str) -> Option<Property> {
    let Ok(index) = key.parse::<usize>() else {
        return None;
    };
    if key != index.to_string() {
        return None;
    }
    let string = string_object_data(obj)?;
    let value = string.chars().nth(index)?;
    Some(Property::Data(DataProperty {
        val: vm.factory.string(value.to_string()),
        writable: false,
        enumerable: true,
        configurable: false,
    }))
}

fn set_integrity_level(obj: Value, frozen: bool) {
    let mut info = obj.get_object_info();
    info.extensible = false;
    for prop in info.property.values_mut() {
        set_property_integrity(prop, frozen);
    }
    for prop in info.sym_property.values_mut() {
        set_property_integrity(prop, frozen);
    }
    if let ObjectKind::Array(ref mut array) = info.kind {
        for prop in &mut array.elems {
            set_property_integrity(prop, frozen);
        }
        if frozen {
            array.length_writable = false;
        }
    }
}

fn set_property_integrity(prop: &mut Property, frozen: bool) {
    match prop {
        Property::Data(data) => {
            data.configurable = false;
            if frozen {
                data.writable = false;
            }
        }
        Property::Accessor(accessor) => {
            accessor.configurable = false;
        }
    }
}

fn test_integrity_level(obj: Value, frozen: bool) -> bool {
    if !obj.is_object() {
        return true;
    }
    let info = obj.get_object_info();
    if info.extensible {
        return false;
    }
    if info
        .property
        .values()
        .any(|prop| !property_has_integrity(prop, frozen))
    {
        return false;
    }
    if info
        .sym_property
        .values()
        .any(|prop| !property_has_integrity(prop, frozen))
    {
        return false;
    }
    if let ObjectKind::Array(ref array) = info.kind {
        if frozen && array.length_writable {
            return false;
        }
        if array
            .elems
            .iter()
            .any(|prop| !property_has_integrity(prop, frozen))
        {
            return false;
        }
    }
    true
}

fn property_has_integrity(prop: &Property, frozen: bool) -> bool {
    match prop {
        Property::Data(data) => {
            !data.configurable && (!frozen || !data.writable || data.val.is_empty())
        }
        Property::Accessor(accessor) => !accessor.configurable,
    }
}

fn enumerable_own_string_keys(obj: Value) -> Vec<String> {
    if !obj.is_object() {
        return Vec::new();
    }
    obj.get_object_info().enumerable_own_string_property_keys()
}

pub fn object_get_prototype_of(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    if !obj.is_object() {
        return Err(vm.current_context.error_type("Object.getPrototypeOf"));
    }
    if let ObjectKind::Proxy(proxy) = obj.get_object_info().kind.clone() {
        return super::proxy::proxy_get_prototype_of(vm, proxy);
    }
    Ok(obj.get_prototype())
}

pub fn object_set_prototype_of(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    if obj.is_null() || obj.is_undefined() {
        return Err(vm.current_context.error_type("Object.setPrototypeOf"));
    }

    let proto = *args.get(1).unwrap_or(&Value::undefined());
    let proto_is_object = proto.is_object() && !proto.is_symbol();
    if !proto_is_object && !proto.is_null() {
        return Err(vm.current_context.error_type("Object.setPrototypeOf"));
    }

    if !obj.is_object() || obj.is_symbol() {
        return Ok(obj);
    }

    if let ObjectKind::Proxy(proxy) = obj.get_object_info().kind.clone() {
        if !super::proxy::proxy_set_prototype_of(vm, proxy, proto)? {
            return Err(vm.current_context.error_type("Object.setPrototypeOf"));
        }
        return Ok(obj);
    }

    if !ordinary_set_prototype_of(
        obj,
        proto,
        obj.strict_eq_bool(vm.factory.object_prototypes.object),
    ) {
        return Err(vm.current_context.error_type("Object.setPrototypeOf"));
    }

    Ok(obj)
}

pub fn object_prototype_proto_get(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if this.is_null() || this.is_undefined() {
        return Err(vm.current_context.error_type("Object.prototype.__proto__"));
    }
    if this.is_string() {
        return Ok(vm.factory.object_prototypes.string);
    }
    if this.is_number() {
        return Ok(vm.factory.object_prototypes.number);
    }
    if matches!(this, Value::Bool(_)) {
        return Ok(vm.factory.object_prototypes.boolean);
    }
    if this.is_symbol() {
        return Ok(vm.factory.object_prototypes.symbol);
    }
    if this.is_bigint() {
        return Ok(vm.factory.object_prototypes.bigint);
    }
    if !this.is_object() {
        return Ok(Value::undefined());
    }
    Ok(this.get_prototype())
}

pub fn object_prototype_proto_set(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if this.is_null() || this.is_undefined() {
        return Err(vm.current_context.error_type("Object.prototype.__proto__"));
    }
    let proto = args.get(0).copied().unwrap_or(Value::undefined());
    let proto_is_object = proto.is_object() && !proto.is_symbol();
    if !proto_is_object && !proto.is_null() {
        return Ok(Value::undefined());
    }
    if !this.is_object() || this.is_symbol() {
        return Ok(Value::undefined());
    }
    if let ObjectKind::Proxy(proxy) = this.get_object_info().kind.clone() {
        if !super::proxy::proxy_set_prototype_of(vm, proxy, proto)? {
            return Err(vm.current_context.error_type("Object.prototype.__proto__"));
        }
        return Ok(Value::undefined());
    }
    if !ordinary_set_prototype_of(
        this,
        proto,
        this.strict_eq_bool(vm.factory.object_prototypes.object),
    ) {
        return Err(vm.current_context.error_type("Object.prototype.__proto__"));
    }
    Ok(Value::undefined())
}

pub fn ordinary_set_prototype_of(obj: Value, proto: Value, immutable_prototype: bool) -> bool {
    let current = obj.get_prototype();
    if current.strict_eq_bool(proto) {
        return true;
    }
    if immutable_prototype || !obj.get_object_info().extensible {
        return false;
    }
    if prototype_chain_contains(proto, obj) {
        return false;
    }

    obj.get_object_info().prototype = proto;
    true
}

fn prototype_chain_contains(mut proto: Value, target: Value) -> bool {
    while proto.is_object() && !proto.is_symbol() {
        if proto.strict_eq_bool(target) {
            return true;
        }
        proto = proto.get_prototype();
    }
    false
}

pub fn object_get_own_property_names(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    if !obj.is_object() {
        if obj.is_null() || obj.is_undefined() {
            return Err(vm.current_context.error_type("Object.getOwnPropertyNames"));
        }
        if obj.is_string() {
            let mut elems = obj
                .to_string()
                .chars()
                .enumerate()
                .map(|(index, _)| Property::new_data_simple(vm.factory.string(index.to_string())))
                .collect::<Vec<_>>();
            elems.push(Property::new_data_simple(vm.factory.string("length")));
            return Ok(vm.factory.array(elems));
        }
        return Ok(vm.factory.array(Vec::new()));
    }

    let elems = obj
        .get_object_info()
        .own_string_property_keys()
        .into_iter()
        .map(|name| Property::new_data_simple(vm.factory.string(name)))
        .collect();
    Ok(vm.factory.array(elems))
}

pub fn object_get_own_property_symbols(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let obj = *args.get(0).unwrap_or(&Value::undefined());
    if obj.is_null() || obj.is_undefined() {
        return Err(vm
            .current_context
            .error_type("Object.getOwnPropertySymbols"));
    }
    if !obj.is_object() || obj.is_symbol() {
        return Ok(vm.factory.array(Vec::new()));
    }

    let info = obj.get_object_info();
    let elems = info
        .sym_property_order
        .iter()
        .filter(|symbol| info.sym_property.contains_key(&symbol.get_symbol_info().id))
        .map(|symbol| Property::new_data_simple(*symbol))
        .collect();
    Ok(vm.factory.array(elems))
}

pub fn object_prototype_has_own_property(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    let key_value = *args.get(0).unwrap_or(&Value::undefined());
    let key = to_property_key(vm, key_value)?;
    let obj = to_object(vm, this)?;
    match key {
        PropertyKey::Symbol(symbol) => {
            if let Value::Object(info) = obj {
                return Ok(Value::bool(
                    ObjectRef(info)
                        .sym_property
                        .contains_key(&symbol.get_symbol_info().id),
                ));
            }
            Ok(Value::bool(false))
        }
        PropertyKey::String(key) => Ok(Value::bool(obj.has_own_property(&key))),
    }
}

pub fn object_prototype_is_prototype_of(
    _vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    let mut target = *args.get(0).unwrap_or(&Value::undefined());
    if !this.is_object() || !target.is_object() {
        return Ok(Value::bool(false));
    }

    loop {
        target = target.get_prototype();
        if target.is_null() {
            return Ok(Value::bool(false));
        }
        if target.strict_eq_bool(this) {
            return Ok(Value::bool(true));
        }
    }
}

pub fn object_prototype_value_of(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    to_object(vm, this)
}

pub fn object_prototype_to_string(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let tag = match this {
        Value::Other(UNDEFINED) => "Undefined",
        Value::Other(NULL) => "Null",
        Value::Bool(_) => "Boolean",
        Value::Number(_) => "Number",
        Value::String(_) => "String",
        Value::Object(info) => match ObjectRef(info).kind {
            ObjectKind::Array(_) => "Array",
            ObjectKind::Function(_) => "Function",
            ObjectKind::Date(_) => "Date",
            ObjectKind::RegExp(_) => "RegExp",
            ObjectKind::Map(_) => "Map",
            ObjectKind::Set(_) => "Set",
            ObjectKind::WeakMap(_) => "WeakMap",
            ObjectKind::WeakSet(_) => "WeakSet",
            ObjectKind::WeakRef(_) => "WeakRef",
            ObjectKind::FinalizationRegistry(_) => "FinalizationRegistry",
            ObjectKind::ShadowRealm(_) => "ShadowRealm",
            ObjectKind::MapIterator(_) => "Map Iterator",
            ObjectKind::SetIterator(_) => "Set Iterator",
            ObjectKind::Generator(_) => "Generator",
            ObjectKind::ArrayBuffer(ref info) if info.shared => "SharedArrayBuffer",
            ObjectKind::ArrayBuffer(_) => "ArrayBuffer",
            ObjectKind::DataView(_) => "DataView",
            ObjectKind::TypedArray(ref info) => info.name,
            ObjectKind::Symbol(_) => "Symbol",
            ObjectKind::BigInt(_) => "BigInt",
            ObjectKind::Error(_) => "Error",
            ObjectKind::Arguments(_) => "Arguments",
            ObjectKind::Proxy(_) => "Object",
            ObjectKind::Temporal(_) => "Object",
            ObjectKind::Ordinary => {
                if ObjectRef(info).property.contains_key("__number_data") {
                    "Number"
                } else if ObjectRef(info).property.contains_key("__string_data") {
                    "String"
                } else if ObjectRef(info).property.contains_key("__boolean_data") {
                    "Boolean"
                } else if ObjectRef(info).property.contains_key("__symbol_data") {
                    "Symbol"
                } else if ObjectRef(info).property.contains_key("__bigint_data") {
                    "BigInt"
                } else {
                    "Object"
                }
            }
        },
        _ => "Object",
    };
    let tag = if this.is_object() {
        let tag_key = vm.factory.symbol_with_id(
            SYMBOL_TO_STRING_TAG_ID,
            Some("Symbol.toStringTag".to_string()),
        );
        let tag_value = vm.get_property_by_value(this, tag_key)?;
        if tag_value.is_string() {
            tag_value.to_string()
        } else {
            tag.to_string()
        }
    } else {
        tag.to_string()
    };
    Ok(vm.factory.string(format!("[object {}]", tag)))
}

pub fn object_prototype_define_getter(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let getter = *args.get(1).unwrap_or(&Value::undefined());
    if !getter.is_function_object() {
        return Err(vm.current_context.error_type("__defineGetter__"));
    }
    define_getter_or_setter(
        vm,
        this,
        *args.get(0).unwrap_or(&Value::undefined()),
        getter,
        true,
    )
}

pub fn object_prototype_define_setter(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let setter = *args.get(1).unwrap_or(&Value::undefined());
    if !setter.is_function_object() {
        return Err(vm.current_context.error_type("__defineSetter__"));
    }
    define_getter_or_setter(
        vm,
        this,
        *args.get(0).unwrap_or(&Value::undefined()),
        setter,
        false,
    )
}

fn define_getter_or_setter(
    vm: &mut VM,
    this: Value,
    key_value: Value,
    func: Value,
    is_getter: bool,
) -> VMValueResult {
    if !this.is_object() {
        return Err(vm.current_context.error_type("Object.prototype accessor"));
    }
    let key = to_property_key(vm, key_value)?;
    let mut info = this.get_object_info();

    let existing = match key {
        PropertyKey::String(ref key) => info.property.get(key).copied(),
        PropertyKey::Symbol(sym) => info.sym_property.get(&sym.get_symbol_info().id).copied(),
    };
    if existing.is_none() && !info.extensible {
        return Err(vm.current_context.error_type("Object is not extensible"));
    }
    if existing.map(|prop| !prop.configurable()).unwrap_or(false) {
        return Err(vm
            .current_context
            .error_type("Cannot redefine non-configurable property"));
    }

    let (get, set) = match existing {
        Some(Property::Accessor(accessor)) => (accessor.get, accessor.set),
        _ => (Value::undefined(), Value::undefined()),
    };
    let prop = if is_getter {
        Property::Accessor(AccessorProperty {
            get: func,
            set,
            enumerable: true,
            configurable: true,
        })
    } else {
        Property::Accessor(AccessorProperty {
            get,
            set: func,
            enumerable: true,
            configurable: true,
        })
    };

    match key {
        PropertyKey::String(key) => {
            info.insert_property(key, prop);
        }
        PropertyKey::Symbol(sym) => {
            super::helpers::define_symbol_property(this, sym, prop);
        }
    }
    Ok(Value::undefined())
}

pub fn object_prototype_lookup_getter(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    lookup_getter_or_setter(vm, this, *args.get(0).unwrap_or(&Value::undefined()), true)
}

pub fn object_prototype_lookup_setter(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    lookup_getter_or_setter(vm, this, *args.get(0).unwrap_or(&Value::undefined()), false)
}

fn lookup_getter_or_setter(
    vm: &mut VM,
    mut object: Value,
    key_value: Value,
    is_getter: bool,
) -> VMValueResult {
    if !object.is_object() {
        return Err(vm.current_context.error_type("Object.prototype accessor"));
    }
    let key = to_property_key(vm, key_value)?;
    while object.is_object() {
        let info = object.get_object_info();
        let prop = match key {
            PropertyKey::String(ref key) => info.property.get(key).copied(),
            PropertyKey::Symbol(sym) => info.sym_property.get(&sym.get_symbol_info().id).copied(),
        };
        if let Some(Property::Accessor(accessor)) = prop {
            return Ok(if is_getter {
                accessor.get
            } else {
                accessor.set
            });
        }
        if prop.is_some() {
            return Ok(Value::undefined());
        }
        object = info.prototype;
    }
    Ok(Value::undefined())
}

pub fn object_prototype_property_is_enumerable(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    let key_value = *args.get(0).unwrap_or(&Value::undefined());
    let key = to_property_key(vm, key_value)?;
    let obj = to_object(vm, this)?;
    if let PropertyKey::Symbol(symbol) = key {
        if let Value::Object(info) = obj {
            let id = symbol.get_symbol_info().id;
            let enumerable = ObjectRef(info)
                .sym_property
                .get(&id)
                .map(|prop| match prop {
                    Property::Data(data) => data.enumerable,
                    Property::Accessor(accessor) => accessor.enumerable,
                })
                .unwrap_or(false);
            return Ok(Value::bool(enumerable));
        }
        return Ok(Value::bool(false));
    }

    let PropertyKey::String(key) = key else {
        unreachable!();
    };
    if let Value::Object(info) = obj {
        if let ObjectKind::Array(ref array) = ObjectRef(info).kind {
            if key == "length" {
                return Ok(Value::bool(false));
            }
            if let Ok(idx) = key.parse::<usize>() {
                if idx < array.elems.len() {
                    let enumerable = match array.elems[idx] {
                        Property::Data(data) => !data.val.is_empty() && data.enumerable,
                        Property::Accessor(accessor) => accessor.enumerable,
                    };
                    return Ok(Value::bool(enumerable));
                }
            }
        }
    }
    if string_index_exists(obj, &key) {
        return Ok(Value::bool(true));
    }
    let enumerable = obj
        .get_object_properties()
        .and_then(|props| props.get(&key))
        .map(|prop| match prop {
            Property::Data(data) => data.enumerable,
            Property::Accessor(accessor) => accessor.enumerable,
        })
        .unwrap_or(false);
    Ok(Value::bool(enumerable))
}
