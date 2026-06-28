use super::helpers::{define_species_getter as set_species, same_value_zero};
use crate::vm::{
    jsvalue::{
        object::{CollectionIteratorKind, DataProperty, ObjectKind, Property},
        value::Value,
    },
    vm::{Factory, VMValueResult, VM},
};

pub fn map(factory: &mut Factory) -> Value {
    let constructor =
        factory.generate_builtin_constructor("Map", map_constructor, factory.object_prototypes.map);
    constructor.get_object_info().insert_property(
        "prototype".to_string(),
        Property::new_data(DataProperty::new(factory.object_prototypes.map)),
    );
    let group_by = factory.builtin_function("groupBy", map_group_by);
    group_by.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(2.0)).set_configurable()),
    );
    constructor.get_object_info().insert_property(
        "groupBy".to_string(),
        Property::new_data(
            DataProperty::new(group_by)
                .set_writable()
                .set_configurable(),
        ),
    );
    set_species(factory, constructor);
    constructor
}

pub fn set(factory: &mut Factory) -> Value {
    let constructor =
        factory.generate_builtin_constructor("Set", set_constructor, factory.object_prototypes.set);
    constructor.get_object_info().insert_property(
        "prototype".to_string(),
        Property::new_data(DataProperty::new(factory.object_prototypes.set)),
    );
    set_species(factory, constructor);
    constructor
}

pub fn weak_map(factory: &mut Factory) -> Value {
    let constructor = factory.generate_builtin_constructor(
        "WeakMap",
        weak_map_constructor,
        factory.object_prototypes.weak_map,
    );
    constructor.get_object_info().insert_property(
        "prototype".to_string(),
        Property::new_data(DataProperty::new(factory.object_prototypes.weak_map)),
    );
    constructor
}

pub fn weak_set(factory: &mut Factory) -> Value {
    let constructor = factory.generate_builtin_constructor(
        "WeakSet",
        weak_set_constructor,
        factory.object_prototypes.weak_set,
    );
    constructor.get_object_info().insert_property(
        "prototype".to_string(),
        Property::new_data(DataProperty::new(factory.object_prototypes.weak_set)),
    );
    constructor
}

pub fn iterator_identity(_vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    Ok(this)
}

pub fn map_group_by(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let items = args.get(0).copied().unwrap_or(Value::undefined());
    let callback = args.get(1).copied().unwrap_or(Value::undefined());
    if !callback.is_function_object() {
        return Err(vm.current_context.error_type("Map.groupBy"));
    }

    let iterator = get_iterator(vm, items)?;
    let map = vm.factory.map();
    let mut index = 0usize;
    loop {
        let next = iterator_next(vm, iterator)?;
        if iterator_complete(vm, next)? {
            break;
        }
        let value = iterator_value(vm, next)?;
        let key = match vm.call_function(
            callback,
            &[value, Value::Number(index as f64)],
            Value::undefined(),
        ) {
            Ok(key) => normalize_zero(key),
            Err(err) => {
                let _ = iterator_close(vm, iterator);
                return Err(err);
            }
        };
        map_group_add(vm, map, key, value);
        index += 1;
    }
    Ok(map)
}

pub fn map_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    if !vm.builtin_constructor_call {
        return Err(vm.current_context.error_type("Map constructor"));
    }

    let map = vm.factory.map();
    let iterable = args.get(0).copied().unwrap_or(Value::undefined());
    if iterable.is_undefined() || iterable.is_null() {
        return Ok(map);
    }

    let set_key = vm.factory.string("set");
    let adder = vm.get_property_by_value(map, set_key)?;
    if !adder.is_function_object() {
        return Err(vm.current_context.error_type("Map set is not callable"));
    }
    let iterator = get_iterator(vm, iterable)?;
    loop {
        let next = iterator_next(vm, iterator)?;
        if iterator_complete(vm, next)? {
            break;
        }
        let entry = iterator_value(vm, next)?;
        if !is_object_type(entry) {
            let err = vm
                .current_context
                .error_type("Iterator value is not an object");
            let _ = iterator_close(vm, iterator);
            return Err(err);
        }
        let key = match vm.get_property_by_value(entry, Value::Number(0.0)) {
            Ok(key) => key,
            Err(err) => {
                let _ = iterator_close(vm, iterator);
                return Err(err);
            }
        };
        let value = match vm.get_property_by_value(entry, Value::Number(1.0)) {
            Ok(value) => value,
            Err(err) => {
                let _ = iterator_close(vm, iterator);
                return Err(err);
            }
        };
        if let Err(err) = vm.call_function(adder, &[key, value], map) {
            let _ = iterator_close(vm, iterator);
            return Err(err);
        }
    }
    Ok(map)
}

pub fn set_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    if !vm.builtin_constructor_call {
        return Err(vm.current_context.error_type("Set constructor"));
    }

    let set = vm.factory.set();
    let iterable = args.get(0).copied().unwrap_or(Value::undefined());
    if iterable.is_undefined() || iterable.is_null() {
        return Ok(set);
    }

    let add_key = vm.factory.string("add");
    let adder = vm.get_property_by_value(set, add_key)?;
    if !adder.is_function_object() {
        return Err(vm.current_context.error_type("Set add is not callable"));
    }
    let iterator = get_iterator(vm, iterable)?;
    loop {
        let next = iterator_next(vm, iterator)?;
        if iterator_complete(vm, next)? {
            break;
        }
        let value = iterator_value(vm, next)?;
        if let Err(err) = vm.call_function(adder, &[value], set) {
            let _ = iterator_close(vm, iterator);
            return Err(err);
        }
    }
    Ok(set)
}

pub fn weak_map_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    if !vm.builtin_constructor_call {
        return Err(vm.current_context.error_type("WeakMap constructor"));
    }

    let weak_map = vm.factory.weak_map();
    let iterable = args.get(0).copied().unwrap_or(Value::undefined());
    if iterable.is_undefined() || iterable.is_null() {
        return Ok(weak_map);
    }

    let set_key = vm.factory.string("set");
    let adder = vm.get_property_by_value(weak_map, set_key)?;
    if !adder.is_function_object() {
        return Err(vm.current_context.error_type("WeakMap set is not callable"));
    }
    let iterator = get_iterator(vm, iterable)?;
    loop {
        let next = iterator_next(vm, iterator)?;
        if iterator_complete(vm, next)? {
            break;
        }
        let entry = iterator_value(vm, next)?;
        if !is_object_type(entry) {
            let err = vm
                .current_context
                .error_type("Iterator value is not an object");
            let _ = iterator_close(vm, iterator);
            return Err(err);
        }
        let key = match vm.get_property_by_value(entry, Value::Number(0.0)) {
            Ok(key) => key,
            Err(err) => {
                let _ = iterator_close(vm, iterator);
                return Err(err);
            }
        };
        let value = match vm.get_property_by_value(entry, Value::Number(1.0)) {
            Ok(value) => value,
            Err(err) => {
                let _ = iterator_close(vm, iterator);
                return Err(err);
            }
        };
        if let Err(err) = vm.call_function(adder, &[key, value], weak_map) {
            let _ = iterator_close(vm, iterator);
            return Err(err);
        }
    }
    Ok(weak_map)
}

pub fn weak_set_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    if !vm.builtin_constructor_call {
        return Err(vm.current_context.error_type("WeakSet constructor"));
    }

    let weak_set = vm.factory.weak_set();
    let iterable = args.get(0).copied().unwrap_or(Value::undefined());
    if iterable.is_undefined() || iterable.is_null() {
        return Ok(weak_set);
    }

    let add_key = vm.factory.string("add");
    let adder = vm.get_property_by_value(weak_set, add_key)?;
    if !adder.is_function_object() {
        return Err(vm.current_context.error_type("WeakSet add is not callable"));
    }
    let iterator = get_iterator(vm, iterable)?;
    loop {
        let next = iterator_next(vm, iterator)?;
        if iterator_complete(vm, next)? {
            break;
        }
        let value = iterator_value(vm, next)?;
        if let Err(err) = vm.call_function(adder, &[value], weak_set) {
            let _ = iterator_close(vm, iterator);
            return Err(err);
        }
    }
    Ok(weak_set)
}

pub fn map_prototype_get(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let key = args.get(0).copied().unwrap_or(Value::undefined());
    require_map(vm, this)?;
    let obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::Map(ref info) => &info.entries,
        _ => return Err(vm.current_context.error_type("Map.prototype.get")),
    };
    for (entry_key, entry_value) in entries {
        if !entry_key.is_empty() && same_value_zero(*entry_key, key) {
            return Ok(*entry_value);
        }
    }
    Ok(Value::undefined())
}

pub fn map_prototype_set(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let key = normalize_zero(args.get(0).copied().unwrap_or(Value::undefined()));
    let value = args.get(1).copied().unwrap_or(Value::undefined());
    require_map(vm, this)?;
    let mut obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::Map(ref mut info) => &mut info.entries,
        _ => return Err(vm.current_context.error_type("Map.prototype.set")),
    };
    for (entry_key, entry_value) in entries.iter_mut() {
        if !entry_key.is_empty() && same_value_zero(*entry_key, key) {
            *entry_value = value;
            return Ok(this);
        }
    }
    entries.push((key, value));
    Ok(this)
}

pub fn map_prototype_has(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let key = args.get(0).copied().unwrap_or(Value::undefined());
    require_map(vm, this)?;
    let obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::Map(ref info) => &info.entries,
        _ => return Err(vm.current_context.error_type("Map.prototype.has")),
    };
    Ok(Value::bool(entries.iter().any(|(entry_key, _)| {
        !entry_key.is_empty() && same_value_zero(*entry_key, key)
    })))
}

pub fn map_prototype_delete(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let key = args.get(0).copied().unwrap_or(Value::undefined());
    require_map(vm, this)?;
    let mut obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::Map(ref mut info) => &mut info.entries,
        _ => return Err(vm.current_context.error_type("Map.prototype.delete")),
    };
    for (entry_key, entry_value) in entries.iter_mut() {
        if !entry_key.is_empty() && same_value_zero(*entry_key, key) {
            *entry_key = Value::empty();
            *entry_value = Value::empty();
            return Ok(Value::bool(true));
        }
    }
    Ok(Value::bool(false))
}

pub fn map_prototype_clear(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    require_map(vm, this)?;
    let mut obj = this.get_object_info();
    match obj.kind {
        ObjectKind::Map(ref mut info) => info.entries.clear(),
        _ => return Err(vm.current_context.error_type("Map.prototype.clear")),
    }
    Ok(Value::undefined())
}

pub fn map_prototype_size(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    require_map(vm, this)?;
    let obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::Map(ref info) => &info.entries,
        _ => return Err(vm.current_context.error_type("Map.prototype.size")),
    };
    Ok(Value::Number(
        entries.iter().filter(|(key, _)| !key.is_empty()).count() as f64,
    ))
}

pub fn map_prototype_keys(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    require_map(vm, this)?;
    Ok(vm.factory.map_iterator(this, CollectionIteratorKind::Key))
}

pub fn map_prototype_values(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    require_map(vm, this)?;
    Ok(vm.factory.map_iterator(this, CollectionIteratorKind::Value))
}

pub fn map_prototype_entries(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    require_map(vm, this)?;
    Ok(vm
        .factory
        .map_iterator(this, CollectionIteratorKind::KeyValue))
}

pub fn map_prototype_for_each(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    require_map(vm, this)?;
    let callback = args.get(0).copied().unwrap_or(Value::undefined());
    if !callback.is_function_object() {
        return Err(vm.current_context.error_type("Map.prototype.forEach"));
    }
    let this_arg = args.get(1).copied().unwrap_or(Value::undefined());
    let mut index = 0;
    loop {
        let next = {
            let obj = this.get_object_info();
            match obj.kind {
                ObjectKind::Map(ref info) => info
                    .entries
                    .iter()
                    .enumerate()
                    .skip(index)
                    .find(|(_, (key, _))| !key.is_empty())
                    .map(|(entry_index, (key, value))| (entry_index, *key, *value)),
                _ => unreachable!(),
            }
        };
        let Some((entry_index, key, value)) = next else {
            break;
        };
        index = entry_index + 1;
        vm.call_function(callback, &[value, key, this], this_arg)?;
    }
    Ok(Value::undefined())
}

pub fn set_prototype_add(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = normalize_zero(args.get(0).copied().unwrap_or(Value::undefined()));
    require_set(vm, this)?;
    let mut obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::Set(ref mut info) => &mut info.entries,
        _ => return Err(vm.current_context.error_type("Set.prototype.add")),
    };
    if !entries
        .iter()
        .any(|entry| !entry.is_empty() && same_value_zero(*entry, value))
    {
        entries.push(value);
    }
    Ok(this)
}

pub fn set_prototype_has(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = args.get(0).copied().unwrap_or(Value::undefined());
    require_set(vm, this)?;
    let obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::Set(ref info) => &info.entries,
        _ => return Err(vm.current_context.error_type("Set.prototype.has")),
    };
    Ok(Value::bool(entries.iter().any(|entry| {
        !entry.is_empty() && same_value_zero(*entry, value)
    })))
}

pub fn set_prototype_delete(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = args.get(0).copied().unwrap_or(Value::undefined());
    require_set(vm, this)?;
    let mut obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::Set(ref mut info) => &mut info.entries,
        _ => return Err(vm.current_context.error_type("Set.prototype.delete")),
    };
    for entry in entries.iter_mut() {
        if !entry.is_empty() && same_value_zero(*entry, value) {
            *entry = Value::empty();
            return Ok(Value::bool(true));
        }
    }
    Ok(Value::bool(false))
}

pub fn set_prototype_clear(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    require_set(vm, this)?;
    let mut obj = this.get_object_info();
    match obj.kind {
        ObjectKind::Set(ref mut info) => info.entries.clear(),
        _ => return Err(vm.current_context.error_type("Set.prototype.clear")),
    }
    Ok(Value::undefined())
}

pub fn set_prototype_size(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    require_set(vm, this)?;
    let obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::Set(ref info) => &info.entries,
        _ => return Err(vm.current_context.error_type("Set.prototype.size")),
    };
    Ok(Value::Number(
        entries.iter().filter(|value| !value.is_empty()).count() as f64,
    ))
}

pub fn set_prototype_values(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    require_set(vm, this)?;
    Ok(vm.factory.set_iterator(this, CollectionIteratorKind::Value))
}

pub fn set_prototype_entries(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    require_set(vm, this)?;
    Ok(vm
        .factory
        .set_iterator(this, CollectionIteratorKind::KeyValue))
}

pub fn set_prototype_for_each(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    require_set(vm, this)?;
    let callback = args.get(0).copied().unwrap_or(Value::undefined());
    if !callback.is_function_object() {
        return Err(vm.current_context.error_type("Set.prototype.forEach"));
    }
    let this_arg = args.get(1).copied().unwrap_or(Value::undefined());
    let mut index = 0;
    loop {
        let next = {
            let obj = this.get_object_info();
            match obj.kind {
                ObjectKind::Set(ref info) => info
                    .entries
                    .iter()
                    .enumerate()
                    .skip(index)
                    .find(|(_, value)| !value.is_empty())
                    .map(|(entry_index, value)| (entry_index, *value)),
                _ => unreachable!(),
            }
        };
        let Some((entry_index, value)) = next else {
            break;
        };
        index = entry_index + 1;
        vm.call_function(callback, &[value, value, this], this_arg)?;
    }
    Ok(Value::undefined())
}

pub fn weak_map_prototype_get(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let key = args.get(0).copied().unwrap_or(Value::undefined());
    require_weak_map(vm, this)?;
    if !can_be_held_weakly(key) {
        return Ok(Value::undefined());
    }
    let obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::WeakMap(ref info) => &info.entries,
        _ => return Err(vm.current_context.error_type("WeakMap.prototype.get")),
    };
    for (entry_key, entry_value) in entries {
        if same_value_zero(*entry_key, key) {
            return Ok(*entry_value);
        }
    }
    Ok(Value::undefined())
}

pub fn weak_map_prototype_set(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let key = args.get(0).copied().unwrap_or(Value::undefined());
    let value = args.get(1).copied().unwrap_or(Value::undefined());
    require_weak_map(vm, this)?;
    if !can_be_held_weakly(key) {
        return Err(vm
            .current_context
            .error_type("Invalid value used as weak map key"));
    }
    let mut obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::WeakMap(ref mut info) => &mut info.entries,
        _ => return Err(vm.current_context.error_type("WeakMap.prototype.set")),
    };
    for (entry_key, entry_value) in entries.iter_mut() {
        if same_value_zero(*entry_key, key) {
            *entry_value = value;
            return Ok(this);
        }
    }
    entries.push((key, value));
    Ok(this)
}

pub fn weak_map_prototype_has(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let key = args.get(0).copied().unwrap_or(Value::undefined());
    require_weak_map(vm, this)?;
    if !can_be_held_weakly(key) {
        return Ok(Value::bool(false));
    }
    let obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::WeakMap(ref info) => &info.entries,
        _ => return Err(vm.current_context.error_type("WeakMap.prototype.has")),
    };
    Ok(Value::bool(
        entries
            .iter()
            .any(|(entry_key, _)| same_value_zero(*entry_key, key)),
    ))
}

pub fn weak_map_prototype_delete(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let key = args.get(0).copied().unwrap_or(Value::undefined());
    require_weak_map(vm, this)?;
    if !can_be_held_weakly(key) {
        return Ok(Value::bool(false));
    }
    let mut obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::WeakMap(ref mut info) => &mut info.entries,
        _ => return Err(vm.current_context.error_type("WeakMap.prototype.delete")),
    };
    let old_len = entries.len();
    entries.retain(|(entry_key, _)| !same_value_zero(*entry_key, key));
    Ok(Value::bool(entries.len() != old_len))
}

pub fn weak_set_prototype_add(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = args.get(0).copied().unwrap_or(Value::undefined());
    require_weak_set(vm, this)?;
    if !can_be_held_weakly(value) {
        return Err(vm
            .current_context
            .error_type("Invalid value used in weak set"));
    }
    let mut obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::WeakSet(ref mut info) => &mut info.entries,
        _ => return Err(vm.current_context.error_type("WeakSet.prototype.add")),
    };
    if !entries.iter().any(|entry| same_value_zero(*entry, value)) {
        entries.push(value);
    }
    Ok(this)
}

pub fn weak_set_prototype_has(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = args.get(0).copied().unwrap_or(Value::undefined());
    require_weak_set(vm, this)?;
    if !can_be_held_weakly(value) {
        return Ok(Value::bool(false));
    }
    let obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::WeakSet(ref info) => &info.entries,
        _ => return Err(vm.current_context.error_type("WeakSet.prototype.has")),
    };
    Ok(Value::bool(
        entries.iter().any(|entry| same_value_zero(*entry, value)),
    ))
}

pub fn weak_set_prototype_delete(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = args.get(0).copied().unwrap_or(Value::undefined());
    require_weak_set(vm, this)?;
    if !can_be_held_weakly(value) {
        return Ok(Value::bool(false));
    }
    let mut obj = this.get_object_info();
    let entries = match obj.kind {
        ObjectKind::WeakSet(ref mut info) => &mut info.entries,
        _ => return Err(vm.current_context.error_type("WeakSet.prototype.delete")),
    };
    let old_len = entries.len();
    entries.retain(|entry| !same_value_zero(*entry, value));
    Ok(Value::bool(entries.len() != old_len))
}

pub fn map_iterator_next(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if !this.is_object() {
        return Err(vm.current_context.error_type("Map Iterator next"));
    }
    let (iterated_map, index, kind) = {
        let mut iterator = this.get_object_info();
        match iterator.kind {
            ObjectKind::MapIterator(ref mut info) => {
                (info.iterated_map, info.next_index, info.kind)
            }
            _ => return Err(vm.current_context.error_type("Map Iterator next")),
        }
    };
    if iterated_map.is_undefined() {
        return Ok(iterator_result(vm, Value::undefined(), true));
    }
    let next = {
        let map = iterated_map.get_object_info();
        match map.kind {
            ObjectKind::Map(ref info) => info
                .entries
                .iter()
                .enumerate()
                .skip(index)
                .find(|(_, (key, _))| !key.is_empty())
                .map(|(entry_index, (key, value))| (entry_index, *key, *value)),
            _ => None,
        }
    };
    if let Some((entry_index, key, value)) = next {
        let mut iterator = this.get_object_info();
        if let ObjectKind::MapIterator(ref mut info) = iterator.kind {
            info.next_index = entry_index + 1;
        }
        let result = match kind {
            CollectionIteratorKind::Key => key,
            CollectionIteratorKind::Value => value,
            CollectionIteratorKind::KeyValue => vm.factory.array(vec![
                Property::new_data_simple(key),
                Property::new_data_simple(value),
            ]),
        };
        Ok(iterator_result(vm, result, false))
    } else {
        let mut iterator = this.get_object_info();
        if let ObjectKind::MapIterator(ref mut info) = iterator.kind {
            info.iterated_map = Value::undefined();
        }
        Ok(iterator_result(vm, Value::undefined(), true))
    }
}

pub fn set_iterator_next(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if !this.is_object() {
        return Err(vm.current_context.error_type("Set Iterator next"));
    }
    let (iterated_set, index, kind) = {
        let mut iterator = this.get_object_info();
        match iterator.kind {
            ObjectKind::SetIterator(ref mut info) => {
                (info.iterated_set, info.next_index, info.kind)
            }
            _ => return Err(vm.current_context.error_type("Set Iterator next")),
        }
    };
    if iterated_set.is_undefined() {
        return Ok(iterator_result(vm, Value::undefined(), true));
    }
    let next = {
        let set = iterated_set.get_object_info();
        match set.kind {
            ObjectKind::Set(ref info) => info
                .entries
                .iter()
                .enumerate()
                .skip(index)
                .find(|(_, value)| !value.is_empty())
                .map(|(entry_index, value)| (entry_index, *value)),
            _ => None,
        }
    };
    if let Some((entry_index, value)) = next {
        let mut iterator = this.get_object_info();
        if let ObjectKind::SetIterator(ref mut info) = iterator.kind {
            info.next_index = entry_index + 1;
        }
        let result = match kind {
            CollectionIteratorKind::Key | CollectionIteratorKind::Value => value,
            CollectionIteratorKind::KeyValue => vm.factory.array(vec![
                Property::new_data_simple(value),
                Property::new_data_simple(value),
            ]),
        };
        Ok(iterator_result(vm, result, false))
    } else {
        let mut iterator = this.get_object_info();
        if let ObjectKind::SetIterator(ref mut info) = iterator.kind {
            info.iterated_set = Value::undefined();
        }
        Ok(iterator_result(vm, Value::undefined(), true))
    }
}

fn require_map(vm: &mut VM, value: Value) -> Result<(), crate::vm::error::RuntimeError> {
    if value.is_object() {
        let obj = value.get_object_info();
        if matches!(obj.kind, ObjectKind::Map(_)) {
            return Ok(());
        }
    }
    Err(vm.current_context.error_type("Map object expected"))
}

fn require_set(vm: &mut VM, value: Value) -> Result<(), crate::vm::error::RuntimeError> {
    if value.is_object() {
        let obj = value.get_object_info();
        if matches!(obj.kind, ObjectKind::Set(_)) {
            return Ok(());
        }
    }
    Err(vm.current_context.error_type("Set object expected"))
}

fn require_weak_map(vm: &mut VM, value: Value) -> Result<(), crate::vm::error::RuntimeError> {
    if value.is_object() {
        let obj = value.get_object_info();
        if matches!(obj.kind, ObjectKind::WeakMap(_)) {
            return Ok(());
        }
    }
    Err(vm.current_context.error_type("WeakMap object expected"))
}

fn require_weak_set(vm: &mut VM, value: Value) -> Result<(), crate::vm::error::RuntimeError> {
    if value.is_object() {
        let obj = value.get_object_info();
        if matches!(obj.kind, ObjectKind::WeakSet(_)) {
            return Ok(());
        }
    }
    Err(vm.current_context.error_type("WeakSet object expected"))
}

fn get_iterator(vm: &mut VM, iterable: Value) -> Result<Value, crate::vm::error::RuntimeError> {
    let iterator_key = vm.factory.symbol_with_id(
        crate::vm::jsvalue::symbol::SYMBOL_ITERATOR_ID,
        Some("Symbol.iterator".to_string()),
    );
    let method = vm.get_property_by_value(iterable, iterator_key)?;
    if !method.is_function_object() {
        return Err(vm.current_context.error_type("Object is not iterable"));
    }
    let iterator = vm.call_function(method, &[], iterable)?;
    if !iterator.is_object() {
        return Err(vm.current_context.error_type("Iterator is not an object"));
    }
    Ok(iterator)
}

fn iterator_next(vm: &mut VM, iterator: Value) -> Result<Value, crate::vm::error::RuntimeError> {
    let next_key = vm.factory.string("next");
    let next = vm.get_property_by_value(iterator, next_key)?;
    if !next.is_function_object() {
        return Err(vm
            .current_context
            .error_type("Iterator next is not callable"));
    }
    let result = vm.call_function(next, &[], iterator)?;
    if !result.is_object() {
        return Err(vm
            .current_context
            .error_type("Iterator result is not an object"));
    }
    Ok(result)
}

fn iterator_complete(vm: &mut VM, result: Value) -> Result<bool, crate::vm::error::RuntimeError> {
    let done_key = vm.factory.string("done");
    Ok(vm.get_property_by_value(result, done_key)?.into_bool())
}

fn iterator_value(vm: &mut VM, result: Value) -> Result<Value, crate::vm::error::RuntimeError> {
    let value_key = vm.factory.string("value");
    vm.get_property_by_value(result, value_key)
}

fn iterator_close(vm: &mut VM, iterator: Value) -> Result<(), crate::vm::error::RuntimeError> {
    let return_key = vm.factory.string("return");
    let return_method = vm.get_property_by_value(iterator, return_key)?;
    if return_method.is_undefined() || return_method.is_null() {
        return Ok(());
    }
    if !return_method.is_function_object() {
        return Ok(());
    }
    let result = vm.call_function(return_method, &[], iterator)?;
    if !result.is_object() {
        return Err(vm
            .current_context
            .error_type("Iterator return is not an object"));
    }
    Ok(())
}

fn is_object_type(value: Value) -> bool {
    if !value.is_object() {
        return false;
    }
    let obj = value.get_object_info();
    !matches!(obj.kind, ObjectKind::Symbol(_))
}

fn can_be_held_weakly(value: Value) -> bool {
    if !value.is_object() {
        return false;
    }
    let obj = value.get_object_info();
    match obj.kind {
        ObjectKind::Symbol(ref info) => !info.registered,
        _ => true,
    }
}

fn iterator_result(vm: &mut VM, value: Value, done: bool) -> Value {
    make_normal_object!(
        vm.factory,
        value => true, false, true: value,
        done => true, false, true: Value::bool(done)
    )
}

fn map_group_add(vm: &mut VM, map: Value, key: Value, value: Value) {
    let mut obj = map.get_object_info();
    let entries = match obj.kind {
        ObjectKind::Map(ref mut info) => &mut info.entries,
        _ => unreachable!(),
    };
    for (entry_key, entry_value) in entries.iter_mut() {
        if !entry_key.is_empty() && same_value_zero(*entry_key, key) {
            push_array_value(*entry_value, value);
            return;
        }
    }
    let array = vm.factory.array(vec![Property::new_data_simple(value)]);
    entries.push((key, array));
}

fn push_array_value(array: Value, value: Value) {
    let mut obj = array.get_object_info();
    if let ObjectKind::Array(ref mut info) = obj.kind {
        let index = info.get_length();
        info.set_element(index, value);
    }
}

fn normalize_zero(value: Value) -> Value {
    match value {
        Value::Number(number) if number == 0.0 => Value::Number(0.0),
        _ => value,
    }
}
