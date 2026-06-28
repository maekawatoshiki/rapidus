use super::helpers::{
    define_species_getter, relative_to_index, same_value_zero, to_integer_or_infinity, to_object,
};
use crate::vm::{
    jsvalue::{
        object::{DataProperty, ObjectKind, Property},
        symbol::{SYMBOL_IS_CONCAT_SPREADABLE_ID, SYMBOL_ITERATOR_ID},
        value::Value,
    },
    vm::{Factory, VMResult, VMValueResult, VM},
};
use rustc_hash::FxHashMap;

const ARRAY_ITERATOR_KIND_KEY: i32 = 0;
const ARRAY_ITERATOR_KIND_VALUE: i32 = 1;
const ARRAY_ITERATOR_KIND_KEY_VALUE: i32 = 2;

pub fn array(factory: &mut Factory) -> Value {
    let constructor = factory.generate_builtin_constructor(
        "Array",
        array_constructor,
        factory.object_prototypes.array,
    );
    let is_array = factory.builtin_function("isArray", array_is_array);
    let from = factory.builtin_function("from", array_from);
    let of = factory.builtin_function("of", array_of);
    from.get_object_info().property.insert(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
    );
    constructor.get_object_info().property.insert(
        "isArray".to_string(),
        Property::new_data(
            DataProperty::new(is_array)
                .set_writable()
                .set_configurable(),
        ),
    );
    constructor.get_object_info().property.insert(
        "from".to_string(),
        Property::new_data(DataProperty::new(from).set_writable().set_configurable()),
    );
    constructor.get_object_info().property.insert(
        "of".to_string(),
        Property::new_data(DataProperty::new(of).set_writable().set_configurable()),
    );
    define_species_getter(factory, constructor);
    constructor
}

pub fn array_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let arg_length = args.len();
    let props = {
        match arg_length {
            0 => vec![],
            1 => {
                let len = args[0];
                if len.is_number() {
                    let Some(len) = array_length_from_value(&mut vm.factory, len) else {
                        return Err(vm.current_context.error_range("Invalid array length"));
                    };
                    let val = vm.factory.array(vec![]);
                    val.as_array_mut().set_length(len);
                    return Ok(val);
                } else {
                    vec![Property::new_data_simple(args[0])]
                }
            }
            _ => {
                let mut ary = vec![];
                for i in 0..arg_length {
                    ary.push(Property::new_data_simple(args[i]));
                }
                ary
            }
        }
    };
    let val = vm.factory.array(props);
    Ok(val)
}

pub fn array_is_array(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    Ok(Value::bool(
        args.get(0).unwrap_or(&Value::undefined()).is_array_object(),
    ))
}

pub fn array_from(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let items = *args.get(0).unwrap_or(&Value::undefined());
    if items.is_null() || items.is_undefined() {
        return Err(vm.current_context.error_type("Array.from"));
    }

    let mapfn = *args.get(1).unwrap_or(&Value::undefined());
    let mapping = !mapfn.is_undefined();
    if mapping && !mapfn.is_function_object() {
        return Err(vm.current_context.error_type("Array.from"));
    }
    let this_arg = *args.get(2).unwrap_or(&Value::undefined());

    let iterator_key = vm.factory.well_known_symbol(SYMBOL_ITERATOR_ID);
    let using_iterator = vm.get_property_by_value(items, iterator_key)?;
    if !using_iterator.is_undefined() {
        if !using_iterator.is_function_object() {
            return Err(vm.current_context.error_type("Array.from"));
        }

        let result = if vm.is_constructor(this) {
            vm.construct_function(this, &[])?
        } else {
            vm.factory.array(vec![])
        };
        let iterator = vm.call_function(using_iterator, &[], items)?;
        if !iterator.is_object() {
            return Err(vm.current_context.error_type("Array.from"));
        }
        let next_key = vm.factory.string("next".to_string());
        let next_method = vm.get_property_by_value(iterator, next_key)?;
        if !next_method.is_function_object() {
            return Err(vm.current_context.error_type("Array.from"));
        }

        let mut k = 0usize;
        loop {
            let next = vm.call_function(next_method, &[], iterator)?;
            if !next.is_object() {
                return Err(vm.current_context.error_type("Array.from"));
            }
            let done_key = vm.factory.string("done".to_string());
            if vm.get_property_by_value(next, done_key)?.to_boolean() {
                break;
            }
            let value_key = vm.factory.string("value".to_string());
            let next_value = vm.get_property_by_value(next, value_key)?;
            let mapped_value = if mapping {
                vm.call_function(mapfn, &[next_value, Value::Number(k as f64)], this_arg)?
            } else {
                next_value
            };
            create_data_property_or_throw(vm, result, Value::Number(k as f64), mapped_value)?;
            k += 1;
        }

        if !result.is_array_object() {
            let length_key = vm.factory.string("length".to_string());
            vm.set_property_by_value(result, length_key, Value::Number(k as f64))?;
        }

        return Ok(result);
    }

    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(items, length_key)?;
    let len = to_length(&mut vm.factory, len_value);
    if len > u32::MAX as usize {
        return Err(vm.current_context.error_range("Invalid array length"));
    }

    let result = if vm.is_constructor(this) {
        vm.construct_function(this, &[Value::Number(len as f64)])?
    } else {
        vm.factory.array(vec![])
    };

    for k in 0..len {
        let key = Value::Number(k as f64);
        let k_value = vm.get_property_by_value(items, key)?;
        let mapped_value = if mapping {
            vm.call_function(mapfn, &[k_value, Value::Number(k as f64)], this_arg)?
        } else {
            k_value
        };
        create_data_property_or_throw(vm, result, Value::Number(k as f64), mapped_value)?;
    }

    let length_key = vm.factory.string("length".to_string());
    vm.set_property_by_value(result, length_key, Value::Number(len as f64))?;

    Ok(result)
}

pub fn array_of(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let len = args.len();
    if len > u32::MAX as usize {
        return Err(vm.current_context.error_range("Invalid array length"));
    }

    let result = if vm.is_constructor(this) {
        vm.construct_function(this, &[Value::Number(len as f64)])?
    } else {
        let elems = vec![Property::new_data_simple(Value::undefined()); len];
        vm.factory.array(elems)
    };

    for (k, val) in args.iter().enumerate() {
        create_data_property_or_throw(vm, result, Value::Number(k as f64), *val)?;
    }

    let length_key = vm.factory.string("length".to_string());
    vm.set_property_by_value(result, length_key, Value::Number(len as f64))?;

    Ok(result)
}

pub fn array_prototype_at(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if this.is_null() || this.is_undefined() {
        return Err(vm.current_context.error_type("Array.prototype.at"));
    }

    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(this, length_key)?;
    let len = to_length(&mut vm.factory, len_value);
    let relative_index = to_integer_or_infinity(vm, *args.get(0).unwrap_or(&Value::undefined()))?;
    let k = if relative_index >= 0.0 {
        relative_index
    } else {
        len as f64 + relative_index
    };

    if k < 0.0 || k >= len as f64 {
        return Ok(Value::undefined());
    }

    vm.get_property_by_value(this, Value::Number(k as usize as f64))
}

pub fn array_prototype_entries(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    Ok(create_array_iterator(
        vm,
        obj,
        ARRAY_ITERATOR_KIND_KEY_VALUE,
    ))
}

pub fn array_prototype_keys(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    Ok(create_array_iterator(vm, obj, ARRAY_ITERATOR_KIND_KEY))
}

pub fn array_prototype_values(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    Ok(create_array_iterator(vm, obj, ARRAY_ITERATOR_KIND_VALUE))
}

pub fn array_iterator_next(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if !this.is_object() {
        return Err(vm.current_context.error_type("Array Iterator next"));
    }

    let iterated_object = this.get_property("__array_iterator_object");
    if iterated_object.is_undefined() {
        return Ok(create_iter_result_object(vm, Value::undefined(), true));
    }

    let next_index = this
        .get_property("__array_iterator_next_index")
        .to_number(&mut vm.factory.memory_allocator) as usize;
    let kind = this
        .get_property("__array_iterator_kind")
        .to_number(&mut vm.factory.memory_allocator) as i32;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(iterated_object, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;

    if next_index >= len {
        this.set_property("__array_iterator_object", Value::undefined());
        return Ok(create_iter_result_object(vm, Value::undefined(), true));
    }

    this.set_property(
        "__array_iterator_next_index",
        Value::Number((next_index + 1) as f64),
    );

    let value = match kind {
        ARRAY_ITERATOR_KIND_KEY => Value::Number(next_index as f64),
        ARRAY_ITERATOR_KIND_VALUE => {
            vm.get_property_by_value(iterated_object, Value::Number(next_index as f64))?
        }
        ARRAY_ITERATOR_KIND_KEY_VALUE => {
            let entry_value =
                vm.get_property_by_value(iterated_object, Value::Number(next_index as f64))?;
            vm.factory.array(vec![
                Property::new_data_simple(Value::Number(next_index as f64)),
                Property::new_data_simple(entry_value),
            ])
        }
        _ => return Err(vm.current_context.error_type("Array Iterator kind")),
    };

    Ok(create_iter_result_object(vm, value, false))
}

fn create_array_iterator(vm: &mut VM, obj: Value, kind: i32) -> Value {
    let mut properties = FxHashMap::default();
    properties.insert(
        "__array_iterator_object".to_string(),
        Property::new_data_simple(obj),
    );
    properties.insert(
        "__array_iterator_next_index".to_string(),
        Property::new_data_simple(Value::Number(0.0)),
    );
    properties.insert(
        "__array_iterator_kind".to_string(),
        Property::new_data_simple(Value::Number(kind as f64)),
    );
    let iterator = vm.factory.object(properties);
    iterator.get_object_info().prototype = vm.factory.object_prototypes.array_iterator;
    iterator
}

fn create_iter_result_object(vm: &mut VM, value: Value, done: bool) -> Value {
    let mut properties = FxHashMap::default();
    properties.insert("value".to_string(), Property::new_data_simple(value));
    properties.insert(
        "done".to_string(),
        Property::new_data_simple(Value::bool(done)),
    );
    vm.factory.object(properties)
}

fn to_length(factory: &mut Factory, val: Value) -> usize {
    const MAX_SAFE_INTEGER: f64 = 9_007_199_254_740_991.0;

    let num = val.to_number(&mut factory.memory_allocator);
    if num.is_nan() || num <= 0.0 {
        return 0;
    }
    if num.is_infinite() {
        return MAX_SAFE_INTEGER as usize;
    }
    num.trunc().min(MAX_SAFE_INTEGER) as usize
}

fn to_length_with_abrupt(vm: &mut VM, val: Value) -> Result<usize, crate::vm::error::RuntimeError> {
    const MAX_SAFE_INTEGER: f64 = 9_007_199_254_740_991.0;

    let num = to_integer_or_infinity(vm, val)?;
    if num <= 0.0 {
        return Ok(0);
    }
    if num.is_infinite() {
        return Ok(MAX_SAFE_INTEGER as usize);
    }
    Ok(num.min(MAX_SAFE_INTEGER) as usize)
}

fn array_length_from_value(factory: &mut Factory, val: Value) -> Option<usize> {
    let num = val.to_number(&mut factory.memory_allocator);
    if num.is_finite() && num >= 0.0 && num.trunc() == num && num <= u32::MAX as f64 {
        Some(num as usize)
    } else {
        None
    }
}

fn create_data_property_or_throw(vm: &mut VM, obj: Value, key: Value, val: Value) -> VMResult {
    if !obj.is_object() {
        return Err(vm.current_context.error_type("CreateDataProperty"));
    }

    let array_index = key
        .is_array_index()
        .or_else(|| key.is_canonical_numeric_index_string(&mut vm.factory.memory_allocator));
    let mut info = obj.get_object_info();

    if let ObjectKind::Array(ref mut array) = info.kind {
        if let Some(idx) = array_index {
            if idx >= array.elems.len() {
                array.length = array.length.max(idx + 1);
                while array.elems.len() <= idx {
                    array.elems.push(Property::new_data_simple(Value::empty()));
                }
            }
            if !array.elems[idx].configurable() {
                return Err(vm.current_context.error_type("CreateDataProperty"));
            }
            array.elems[idx] = Property::new_data_simple(val);
            return Ok(());
        }
    }

    let key = key.to_string();
    if info
        .property
        .get(&key)
        .map(|prop| !prop.configurable())
        .unwrap_or(false)
    {
        return Err(vm.current_context.error_type("CreateDataProperty"));
    }
    info.property.insert(key, Property::new_data_simple(val));
    Ok(())
}

pub fn array_prototype_copy_within(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;

    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;

    let relative_target = to_integer_or_infinity(vm, *args.get(0).unwrap_or(&Value::undefined()))?;
    let mut to = relative_to_index(relative_target, len);

    let relative_start = to_integer_or_infinity(vm, *args.get(1).unwrap_or(&Value::undefined()))?;
    let mut from = relative_to_index(relative_start, len);

    let final_index = if args.get(2).unwrap_or(&Value::undefined()).is_undefined() {
        len
    } else {
        let relative_end = to_integer_or_infinity(vm, args[2])?;
        relative_to_index(relative_end, len)
    };

    let mut count = std::cmp::min(final_index.saturating_sub(from), len.saturating_sub(to));
    let direction = if from < to && to < from + count {
        from += count - 1;
        to += count - 1;
        -1isize
    } else {
        1isize
    };

    while count > 0 {
        let from_key = Value::Number(from as f64);
        let to_key = Value::Number(to as f64);
        if vm.has_property(from_key, obj)?.to_boolean() {
            let from_val = vm.get_property_by_value(obj, from_key)?;
            vm.set_property_by_value(obj, to_key, from_val)?;
        } else {
            let deleted = obj.delete_property_by_value(&mut vm.factory.memory_allocator, to_key)?;
            if !deleted {
                return Err(vm.current_context.error_type("Array.prototype.copyWithin"));
            }
        }

        from = ((from as isize) + direction) as usize;
        to = ((to as isize) + direction) as usize;
        count -= 1;
    }

    Ok(obj)
}

pub fn array_prototype_every(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let callback = *args.get(0).unwrap_or(&Value::undefined());
    if !callback.is_function_object() {
        return Err(vm.current_context.error_type("Array.prototype.every"));
    }
    let this_arg = *args.get(1).unwrap_or(&Value::undefined());

    for k in 0..len {
        let key = Value::Number(k as f64);
        if vm.has_property(key, obj)?.to_boolean() {
            let k_value = vm.get_property_by_value(obj, key)?;
            let test_result = vm.call_function(callback, &[k_value, key, obj], this_arg)?;
            if !test_result.to_boolean() {
                return Ok(Value::bool(false));
            }
        }
    }

    Ok(Value::bool(true))
}

pub fn array_prototype_some(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let callback = *args.get(0).unwrap_or(&Value::undefined());
    if !callback.is_function_object() {
        return Err(vm.current_context.error_type("Array.prototype.some"));
    }
    let this_arg = *args.get(1).unwrap_or(&Value::undefined());

    for k in 0..len {
        let key = Value::Number(k as f64);
        if vm.has_property(key, obj)?.to_boolean() {
            let k_value = vm.get_property_by_value(obj, key)?;
            let test_result = vm.call_function(callback, &[k_value, key, obj], this_arg)?;
            if test_result.to_boolean() {
                return Ok(Value::bool(true));
            }
        }
    }

    Ok(Value::bool(false))
}

pub fn array_prototype_fill(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let value = *args.get(0).unwrap_or(&Value::undefined());
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;

    let start = to_integer_or_infinity(vm, *args.get(1).unwrap_or(&Value::undefined()))?;
    let mut k = relative_to_index(start, len);
    let final_index = match args.get(2) {
        Some(end) if !end.is_undefined() => {
            let end = to_integer_or_infinity(vm, *end)?;
            relative_to_index(end, len)
        }
        _ => len,
    };

    while k < final_index {
        vm.set_property_by_value(obj, Value::Number(k as f64), value)?;
        k += 1;
    }

    Ok(obj)
}

pub fn array_prototype_includes(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let search_element = *args.get(0).unwrap_or(&Value::undefined());
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    if len == 0 {
        return Ok(Value::bool(false));
    }

    let n = to_integer_or_infinity(vm, *args.get(1).unwrap_or(&Value::undefined()))?;
    if n == f64::INFINITY {
        return Ok(Value::bool(false));
    }
    let mut k = if n >= 0.0 {
        n as isize
    } else {
        (len as isize + n as isize).max(0)
    };

    while (k as usize) < len {
        let element = vm.get_property_by_value(obj, Value::Number(k as f64))?;
        if same_value_zero(element, search_element) {
            return Ok(Value::bool(true));
        }
        k += 1;
    }

    Ok(Value::bool(false))
}

pub fn array_prototype_index_of(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let search_element = *args.get(0).unwrap_or(&Value::undefined());
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    if len == 0 {
        return Ok(Value::Number(-1.0));
    }

    let n = to_integer_or_infinity(vm, *args.get(1).unwrap_or(&Value::undefined()))?;
    if n >= len as f64 {
        return Ok(Value::Number(-1.0));
    }
    let mut k = if n >= 0.0 {
        n as isize
    } else {
        (len as isize + n as isize).max(0)
    };

    while (k as usize) < len {
        let key = Value::Number(k as f64);
        if vm.has_property(key, obj)?.to_boolean() {
            let element = vm.get_property_by_value(obj, key)?;
            if element.strict_eq_bool(search_element) {
                return Ok(Value::Number(k as f64));
            }
        }
        k += 1;
    }

    Ok(Value::Number(-1.0))
}

pub fn array_prototype_last_index_of(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let search_element = *args.get(0).unwrap_or(&Value::undefined());
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    if len == 0 {
        return Ok(Value::Number(-1.0));
    }

    let mut k = if args.len() >= 2 {
        let n = to_integer_or_infinity(vm, args[1])?;
        if n == f64::NEG_INFINITY {
            return Ok(Value::Number(-1.0));
        }
        if n >= 0.0 {
            n.min((len - 1) as f64) as usize
        } else {
            let candidate = len as f64 + n;
            if candidate < 0.0 {
                return Ok(Value::Number(-1.0));
            }
            candidate as usize
        }
    } else {
        len - 1
    };

    loop {
        let key = Value::Number(k as f64);
        if vm.has_property(key, obj)?.to_boolean() {
            let element = vm.get_property_by_value(obj, key)?;
            if element.strict_eq_bool(search_element) {
                return Ok(Value::Number(k as f64));
            }
        }
        if k == 0 {
            break;
        }
        k -= 1;
    }

    Ok(Value::Number(-1.0))
}

pub fn array_prototype_reduce(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let callback = *args.get(0).unwrap_or(&Value::undefined());
    if !callback.is_function_object() {
        return Err(vm.current_context.error_type("Array.prototype.reduce"));
    }

    let mut k = 0usize;
    let mut accumulator = if args.len() >= 2 {
        args[1]
    } else {
        let mut initial = None;
        while k < len {
            let key = Value::Number(k as f64);
            if vm.has_property(key, obj)?.to_boolean() {
                initial = Some(vm.get_property_by_value(obj, key)?);
                k += 1;
                break;
            }
            k += 1;
        }
        match initial {
            Some(value) => value,
            None => return Err(vm.current_context.error_type("Array.prototype.reduce")),
        }
    };

    while k < len {
        let key = Value::Number(k as f64);
        if vm.has_property(key, obj)?.to_boolean() {
            let value = vm.get_property_by_value(obj, key)?;
            accumulator = vm.call_function(
                callback,
                &[accumulator, value, key, obj],
                Value::undefined(),
            )?;
        }
        k += 1;
    }

    Ok(accumulator)
}

pub fn array_prototype_reduce_right(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let callback = *args.get(0).unwrap_or(&Value::undefined());
    if !callback.is_function_object() {
        return Err(vm.current_context.error_type("Array.prototype.reduceRight"));
    }

    let mut k = len;
    let mut accumulator = if args.len() >= 2 {
        args[1]
    } else {
        let mut initial = None;
        while k > 0 {
            k -= 1;
            let key = Value::Number(k as f64);
            if vm.has_property(key, obj)?.to_boolean() {
                initial = Some(vm.get_property_by_value(obj, key)?);
                break;
            }
        }
        match initial {
            Some(value) => value,
            None => return Err(vm.current_context.error_type("Array.prototype.reduceRight")),
        }
    };

    while k > 0 {
        k -= 1;
        let key = Value::Number(k as f64);
        if vm.has_property(key, obj)?.to_boolean() {
            let value = vm.get_property_by_value(obj, key)?;
            accumulator = vm.call_function(
                callback,
                &[accumulator, value, key, obj],
                Value::undefined(),
            )?;
        }
    }

    Ok(accumulator)
}

pub fn array_prototype_filter(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let callback = *args.get(0).unwrap_or(&Value::undefined());
    if !callback.is_function_object() {
        return Err(vm.current_context.error_type("Array.prototype.filter"));
    }
    let this_arg = *args.get(1).unwrap_or(&Value::undefined());

    let result = vm.factory.array(vec![]);
    let mut to = 0usize;

    for k in 0..len {
        let key = Value::Number(k as f64);
        if vm.has_property(key, obj)?.to_boolean() {
            let k_value = vm.get_property_by_value(obj, key)?;
            let selected = vm
                .call_function(callback, &[k_value, key, obj], this_arg)?
                .to_boolean();
            if selected {
                create_data_property_or_throw(vm, result, Value::Number(to as f64), k_value)?;
                to += 1;
            }
        }
    }

    Ok(result)
}

pub fn array_prototype_for_each(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let callback = *args.get(0).unwrap_or(&Value::undefined());
    if !callback.is_function_object() {
        return Err(vm.current_context.error_type("Array.prototype.forEach"));
    }
    let this_arg = *args.get(1).unwrap_or(&Value::undefined());

    for k in 0..len {
        let key = Value::Number(k as f64);
        if vm.has_property(key, obj)?.to_boolean() {
            let k_value = vm.get_property_by_value(obj, key)?;
            vm.call_function(callback, &[k_value, key, obj], this_arg)?;
        }
    }

    Ok(Value::undefined())
}

pub fn array_prototype_find(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let predicate = *args.get(0).unwrap_or(&Value::undefined());
    if !predicate.is_function_object() {
        return Err(vm.current_context.error_type("Array.prototype.find"));
    }
    let this_arg = *args.get(1).unwrap_or(&Value::undefined());

    for k in 0..len {
        let key = Value::Number(k as f64);
        let k_value = vm.get_property_by_value(obj, key)?;
        let test_result = vm
            .call_function(predicate, &[k_value, key, obj], this_arg)?
            .to_boolean();
        if test_result {
            return Ok(k_value);
        }
    }

    Ok(Value::undefined())
}

pub fn array_prototype_find_index(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let predicate = *args.get(0).unwrap_or(&Value::undefined());
    if !predicate.is_function_object() {
        return Err(vm.current_context.error_type("Array.prototype.findIndex"));
    }
    let this_arg = *args.get(1).unwrap_or(&Value::undefined());

    for k in 0..len {
        let key = Value::Number(k as f64);
        let k_value = vm.get_property_by_value(obj, key)?;
        let test_result = vm
            .call_function(predicate, &[k_value, key, obj], this_arg)?
            .to_boolean();
        if test_result {
            return Ok(key);
        }
    }

    Ok(Value::Number(-1.0))
}

pub fn array_prototype_find_last(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let predicate = *args.get(0).unwrap_or(&Value::undefined());
    if !predicate.is_function_object() {
        return Err(vm.current_context.error_type("Array.prototype.findLast"));
    }
    let this_arg = *args.get(1).unwrap_or(&Value::undefined());

    let mut k = len;
    while k > 0 {
        k -= 1;
        let key = Value::Number(k as f64);
        let k_value = vm.get_property_by_value(obj, key)?;
        let test_result = vm
            .call_function(predicate, &[k_value, key, obj], this_arg)?
            .to_boolean();
        if test_result {
            return Ok(k_value);
        }
    }

    Ok(Value::undefined())
}

pub fn array_prototype_find_last_index(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let predicate = *args.get(0).unwrap_or(&Value::undefined());
    if !predicate.is_function_object() {
        return Err(vm
            .current_context
            .error_type("Array.prototype.findLastIndex"));
    }
    let this_arg = *args.get(1).unwrap_or(&Value::undefined());

    let mut k = len;
    while k > 0 {
        k -= 1;
        let key = Value::Number(k as f64);
        let k_value = vm.get_property_by_value(obj, key)?;
        let test_result = vm
            .call_function(predicate, &[k_value, key, obj], this_arg)?
            .to_boolean();
        if test_result {
            return Ok(key);
        }
    }

    Ok(Value::Number(-1.0))
}

pub fn array_prototype_flat(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let depth = match args.get(0) {
        Some(depth) => to_depth(vm, *depth)?,
        None => 1,
    };
    let result = vm.factory.array(vec![]);
    flatten_into_array(vm, result, obj, len, 0, depth, None)?;
    Ok(result)
}

pub fn array_prototype_flat_map(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let mapper = *args.get(0).unwrap_or(&Value::undefined());
    if !mapper.is_function_object() {
        return Err(vm.current_context.error_type("Array.prototype.flatMap"));
    }
    let this_arg = *args.get(1).unwrap_or(&Value::undefined());
    let result = vm.factory.array(vec![]);
    flatten_into_array(vm, result, obj, len, 0, 1, Some((mapper, this_arg)))?;
    Ok(result)
}

fn to_depth(vm: &mut VM, val: Value) -> Result<usize, crate::vm::error::RuntimeError> {
    let depth = to_integer_or_infinity(vm, val)?;
    if depth <= 0.0 {
        Ok(0)
    } else if depth.is_infinite() {
        Ok(usize::MAX)
    } else {
        Ok(depth as usize)
    }
}

fn flatten_into_array(
    vm: &mut VM,
    target: Value,
    source: Value,
    source_len: usize,
    start: usize,
    depth: usize,
    mapper: Option<(Value, Value)>,
) -> Result<usize, crate::vm::error::RuntimeError> {
    let mut target_index = start;
    for source_index in 0..source_len {
        let source_key = Value::Number(source_index as f64);
        if !vm.has_property(source_key, source)?.to_boolean() {
            continue;
        }

        let mut element = vm.get_property_by_value(source, source_key)?;
        if let Some((mapper_function, this_arg)) = mapper {
            element =
                vm.call_function(mapper_function, &[element, source_key, source], this_arg)?;
        }

        if depth > 0 && element.is_array_object() {
            let length_key = vm.factory.string("length".to_string());
            let element_len_value = vm.get_property_by_value(element, length_key)?;
            let element_len = to_length_with_abrupt(vm, element_len_value)?;
            target_index = flatten_into_array(
                vm,
                target,
                element,
                element_len,
                target_index,
                depth - 1,
                None,
            )?;
        } else {
            create_data_property_or_throw(vm, target, Value::Number(target_index as f64), element)?;
            target_index += 1;
        }
    }
    Ok(target_index)
}

pub fn array_prototype_concat(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let this = to_object(vm, this)?;

    let result = vm.factory.array(vec![]);
    let mut next_index = 0usize;
    concat_append(vm, result, &mut next_index, this)?;
    for arg in args {
        concat_append(vm, result, &mut next_index, *arg)?;
    }

    Ok(result)
}

fn concat_append(vm: &mut VM, result: Value, next_index: &mut usize, item: Value) -> VMResult {
    if is_concat_spreadable(vm, item)? {
        let length_key = vm.factory.string("length".to_string());
        let len_value = vm.get_property_by_value(item, length_key)?;
        let len = to_length_with_abrupt(vm, len_value)?;
        for k in 0..len {
            if vm.has_property(Value::Number(k as f64), item)?.to_boolean() {
                let value = vm.get_property_by_value(item, Value::Number(k as f64))?;
                create_data_property_or_throw(
                    vm,
                    result,
                    Value::Number(*next_index as f64),
                    value,
                )?;
            }
            *next_index += 1;
        }
    } else {
        create_data_property_or_throw(vm, result, Value::Number(*next_index as f64), item)?;
        *next_index += 1;
    }

    Ok(())
}

fn is_concat_spreadable(vm: &mut VM, item: Value) -> Result<bool, crate::vm::error::RuntimeError> {
    if item.is_object() {
        let key = vm.factory.symbol_with_id(
            SYMBOL_IS_CONCAT_SPREADABLE_ID,
            Some("Symbol.isConcatSpreadable".to_string()),
        );
        let spreadable = vm.get_property_by_value(item, key)?;
        if !spreadable.is_undefined() {
            return Ok(spreadable.to_boolean());
        }
    }
    Ok(item.is_array_object())
}

pub fn array_prototype_join(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    if len == 0 {
        return Ok(vm.factory.string("".to_string()));
    }

    let separator = match args.get(0) {
        Some(separator) if !separator.is_undefined() => separator.to_string(),
        _ => ",".to_string(),
    };
    let mut result = String::new();
    for k in 0..len {
        if k > 0 {
            result += &separator;
        }
        let elem = vm.get_property_by_value(obj, Value::Number(k as f64))?;
        if !elem.is_null() && !elem.is_undefined() {
            result += &elem.to_string();
        }
    }

    Ok(vm.factory.string(result))
}

pub fn array_prototype_slice(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;

    let start = to_integer_or_infinity(vm, *args.get(0).unwrap_or(&Value::undefined()))?;
    let mut k = relative_to_index(start, len);
    let final_index = match args.get(1) {
        Some(end) if !end.is_undefined() => {
            let end = to_integer_or_infinity(vm, *end)?;
            relative_to_index(end, len)
        }
        _ => len,
    };
    let count = final_index.saturating_sub(k);
    let result = vm.factory.array(vec![]);
    result.as_array_mut().set_length(count);

    let mut n = 0usize;
    while k < final_index {
        let from_key = Value::Number(k as f64);
        if vm.has_property(from_key, obj)?.to_boolean() {
            let k_value = vm.get_property_by_value(obj, from_key)?;
            create_data_property_or_throw(vm, result, Value::Number(n as f64), k_value)?;
        }
        k += 1;
        n += 1;
    }

    Ok(result)
}

pub fn array_prototype_reverse(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;

    for lower in 0..(len / 2) {
        let upper = len - lower - 1;
        let lower_key = Value::Number(lower as f64);
        let upper_key = Value::Number(upper as f64);
        let lower_exists = vm.has_property(lower_key, obj)?.to_boolean();
        let lower_value = if lower_exists {
            Some(vm.get_property_by_value(obj, lower_key)?)
        } else {
            None
        };
        let upper_exists = vm.has_property(upper_key, obj)?.to_boolean();
        let upper_value = if upper_exists {
            Some(vm.get_property_by_value(obj, upper_key)?)
        } else {
            None
        };

        match (lower_value, upper_value) {
            (Some(lower_value), Some(upper_value)) => {
                vm.set_property_by_value(obj, lower_key, upper_value)?;
                vm.set_property_by_value(obj, upper_key, lower_value)?;
            }
            (Some(lower_value), None) => {
                vm.set_property_by_value(obj, upper_key, lower_value)?;
                if !obj.delete_property_by_value(&mut vm.factory.memory_allocator, lower_key)? {
                    return Err(vm.current_context.error_type("Array.prototype.reverse"));
                }
            }
            (None, Some(upper_value)) => {
                vm.set_property_by_value(obj, lower_key, upper_value)?;
                if !obj.delete_property_by_value(&mut vm.factory.memory_allocator, upper_key)? {
                    return Err(vm.current_context.error_type("Array.prototype.reverse"));
                }
            }
            (None, None) => {}
        }
    }

    Ok(obj)
}

pub fn array_prototype_shift(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;

    if len == 0 {
        let length_key = vm.factory.string("length".to_string());
        vm.set_property_by_value(obj, length_key, Value::Number(0.0))?;
        return Ok(Value::undefined());
    }

    let first = vm.get_property_by_value(obj, Value::Number(0.0))?;
    for k in 1..len {
        let from_key = Value::Number(k as f64);
        let to_key = Value::Number((k - 1) as f64);
        if vm.has_property(from_key, obj)?.to_boolean() {
            let from_value = vm.get_property_by_value(obj, from_key)?;
            vm.set_property_by_value(obj, to_key, from_value)?;
        } else if !obj.delete_property_by_value(&mut vm.factory.memory_allocator, to_key)? {
            return Err(vm.current_context.error_type("Array.prototype.shift"));
        }
    }

    let last_key = Value::Number((len - 1) as f64);
    if !obj.delete_property_by_value(&mut vm.factory.memory_allocator, last_key)? {
        return Err(vm.current_context.error_type("Array.prototype.shift"));
    }
    let length_key = vm.factory.string("length".to_string());
    vm.set_property_by_value(obj, length_key, Value::Number((len - 1) as f64))?;

    Ok(first)
}

pub fn array_prototype_unshift(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    const MAX_SAFE_INTEGER: usize = 9_007_199_254_740_991;

    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let arg_count = args.len();
    let new_len = len
        .checked_add(arg_count)
        .ok_or_else(|| vm.current_context.error_type("Array.prototype.unshift"))?;
    if new_len > MAX_SAFE_INTEGER {
        return Err(vm.current_context.error_type("Array.prototype.unshift"));
    }

    let mut k = len;
    while k > 0 {
        k -= 1;
        let from_key = Value::Number(k as f64);
        let to_key = Value::Number((k + arg_count) as f64);
        if vm.has_property(from_key, obj)?.to_boolean() {
            let from_value = vm.get_property_by_value(obj, from_key)?;
            vm.set_property_by_value(obj, to_key, from_value)?;
        } else if !obj.delete_property_by_value(&mut vm.factory.memory_allocator, to_key)? {
            return Err(vm.current_context.error_type("Array.prototype.unshift"));
        }
    }

    for (index, arg) in args.iter().enumerate() {
        vm.set_property_by_value(obj, Value::Number(index as f64), *arg)?;
    }

    let length_key = vm.factory.string("length".to_string());
    vm.set_property_by_value(obj, length_key, Value::Number(new_len as f64))?;

    Ok(Value::Number(new_len as f64))
}

pub fn array_prototype_splice(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    const MAX_SAFE_INTEGER: usize = 9_007_199_254_740_991;

    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let start = to_integer_or_infinity(vm, *args.get(0).unwrap_or(&Value::undefined()))?;
    let actual_start = relative_to_index(start, len);
    let insert_count = args.len().saturating_sub(2);
    let actual_delete_count = if args.is_empty() {
        0
    } else if args.len() == 1 {
        len - actual_start
    } else {
        let delete_count = to_integer_or_infinity(vm, args[1])?;
        if delete_count <= 0.0 {
            0
        } else {
            delete_count.min((len - actual_start) as f64) as usize
        }
    };
    let new_len = len
        .checked_sub(actual_delete_count)
        .and_then(|value| value.checked_add(insert_count))
        .ok_or_else(|| vm.current_context.error_type("Array.prototype.splice"))?;
    if new_len > MAX_SAFE_INTEGER {
        return Err(vm.current_context.error_type("Array.prototype.splice"));
    }

    let removed = vm.factory.array(vec![]);
    removed.as_array_mut().set_length(actual_delete_count);
    for k in 0..actual_delete_count {
        let from_key = Value::Number((actual_start + k) as f64);
        if vm.has_property(from_key, obj)?.to_boolean() {
            let from_value = vm.get_property_by_value(obj, from_key)?;
            create_data_property_or_throw(vm, removed, Value::Number(k as f64), from_value)?;
        }
    }

    if insert_count < actual_delete_count {
        for k in actual_start..(len - actual_delete_count) {
            let from_key = Value::Number((k + actual_delete_count) as f64);
            let to_key = Value::Number((k + insert_count) as f64);
            if vm.has_property(from_key, obj)?.to_boolean() {
                let from_value = vm.get_property_by_value(obj, from_key)?;
                vm.set_property_by_value(obj, to_key, from_value)?;
            } else if !obj.delete_property_by_value(&mut vm.factory.memory_allocator, to_key)? {
                return Err(vm.current_context.error_type("Array.prototype.splice"));
            }
        }

        let mut k = len;
        while k > len - actual_delete_count + insert_count {
            k -= 1;
            if !obj.delete_property_by_value(
                &mut vm.factory.memory_allocator,
                Value::Number(k as f64),
            )? {
                return Err(vm.current_context.error_type("Array.prototype.splice"));
            }
        }
    } else if insert_count > actual_delete_count {
        let mut k = len - actual_delete_count;
        while k > actual_start {
            let from_key = Value::Number((k + actual_delete_count - 1) as f64);
            let to_key = Value::Number((k + insert_count - 1) as f64);
            if vm.has_property(from_key, obj)?.to_boolean() {
                let from_value = vm.get_property_by_value(obj, from_key)?;
                vm.set_property_by_value(obj, to_key, from_value)?;
            } else if !obj.delete_property_by_value(&mut vm.factory.memory_allocator, to_key)? {
                return Err(vm.current_context.error_type("Array.prototype.splice"));
            }
            k -= 1;
        }
    }

    for (index, item) in args.iter().skip(2).enumerate() {
        vm.set_property_by_value(obj, Value::Number((actual_start + index) as f64), *item)?;
    }

    let length_key = vm.factory.string("length".to_string());
    vm.set_property_by_value(obj, length_key, Value::Number(new_len as f64))?;

    Ok(removed)
}

pub fn array_prototype_sort(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let comparefn = *args.get(0).unwrap_or(&Value::undefined());
    if !comparefn.is_undefined() && !comparefn.is_function_object() {
        return Err(vm.current_context.error_type("Array.prototype.sort"));
    }
    let comparefn = if comparefn.is_function_object() {
        Some(comparefn)
    } else {
        None
    };

    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let mut items = Vec::new();
    for k in 0..len {
        let key = Value::Number(k as f64);
        if vm.has_property(key, obj)?.to_boolean() {
            items.push(vm.get_property_by_value(obj, key)?);
        }
    }

    for i in 1..items.len() {
        let mut j = i;
        while j > 0 && sort_compare(vm, comparefn, items[j - 1], items[j])?.is_gt() {
            items.swap(j - 1, j);
            j -= 1;
        }
    }

    for (index, item) in items.iter().enumerate() {
        vm.set_property_by_value(obj, Value::Number(index as f64), *item)?;
    }
    for index in items.len()..len {
        if !obj.delete_property_by_value(
            &mut vm.factory.memory_allocator,
            Value::Number(index as f64),
        )? {
            return Err(vm.current_context.error_type("Array.prototype.sort"));
        }
    }

    Ok(obj)
}

pub fn array_prototype_to_reversed(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let result = vm.factory.array(vec![]);
    result.as_array_mut().set_length(len);

    for k in 0..len {
        let from = len - k - 1;
        let from_key = Value::Number(from as f64);
        let value = vm.get_property_by_value(obj, from_key)?;
        create_data_property_or_throw(vm, result, Value::Number(k as f64), value)?;
    }

    Ok(result)
}

pub fn array_prototype_to_sorted(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let comparefn = *args.get(0).unwrap_or(&Value::undefined());
    if !comparefn.is_undefined() && !comparefn.is_function_object() {
        return Err(vm.current_context.error_type("Array.prototype.toSorted"));
    }
    let comparefn = if comparefn.is_function_object() {
        Some(comparefn)
    } else {
        None
    };

    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let mut items = Vec::new();
    for k in 0..len {
        items.push(vm.get_property_by_value(obj, Value::Number(k as f64))?);
    }

    for i in 1..items.len() {
        let mut j = i;
        while j > 0 && sort_compare(vm, comparefn, items[j - 1], items[j])?.is_gt() {
            items.swap(j - 1, j);
            j -= 1;
        }
    }

    let result = vm.factory.array(vec![]);
    result.as_array_mut().set_length(len);
    for (index, item) in items.iter().enumerate() {
        create_data_property_or_throw(vm, result, Value::Number(index as f64), *item)?;
    }

    Ok(result)
}

pub fn array_prototype_to_spliced(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    const MAX_SAFE_INTEGER: usize = 9_007_199_254_740_991;

    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let start = to_integer_or_infinity(vm, *args.get(0).unwrap_or(&Value::undefined()))?;
    let actual_start = relative_to_index(start, len);
    let insert_count = args.len().saturating_sub(2);
    let actual_delete_count = if args.is_empty() {
        0
    } else if args.len() == 1 {
        len - actual_start
    } else {
        let delete_count = to_integer_or_infinity(vm, args[1])?;
        if delete_count <= 0.0 {
            0
        } else {
            delete_count.min((len - actual_start) as f64) as usize
        }
    };
    let new_len = len
        .checked_sub(actual_delete_count)
        .and_then(|value| value.checked_add(insert_count))
        .ok_or_else(|| vm.current_context.error_type("Array.prototype.toSpliced"))?;
    if new_len > MAX_SAFE_INTEGER {
        return Err(vm.current_context.error_type("Array.prototype.toSpliced"));
    }

    let result = vm.factory.array(vec![]);
    result.as_array_mut().set_length(new_len);
    for k in 0..actual_start {
        let key = Value::Number(k as f64);
        let value = vm.get_property_by_value(obj, key)?;
        create_data_property_or_throw(vm, result, key, value)?;
    }
    for (index, item) in args.iter().skip(2).enumerate() {
        create_data_property_or_throw(
            vm,
            result,
            Value::Number((actual_start + index) as f64),
            *item,
        )?;
    }
    let from_start = actual_start + actual_delete_count;
    let to_start = actual_start + insert_count;
    for k in from_start..len {
        let from_key = Value::Number(k as f64);
        let value = vm.get_property_by_value(obj, from_key)?;
        create_data_property_or_throw(
            vm,
            result,
            Value::Number((to_start + k - from_start) as f64),
            value,
        )?;
    }

    Ok(result)
}

pub fn array_prototype_with(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let index = to_integer_or_infinity(vm, *args.get(0).unwrap_or(&Value::undefined()))?;
    let actual_index = if index >= 0.0 {
        index
    } else {
        len as f64 + index
    };
    if actual_index < 0.0 || actual_index >= len as f64 {
        return Err(vm.current_context.error_range("Array.prototype.with"));
    }
    let actual_index = actual_index as usize;
    let value = *args.get(1).unwrap_or(&Value::undefined());
    let result = vm.factory.array(vec![]);
    result.as_array_mut().set_length(len);

    for k in 0..len {
        let new_value = if k == actual_index {
            value
        } else {
            vm.get_property_by_value(obj, Value::Number(k as f64))?
        };
        create_data_property_or_throw(vm, result, Value::Number(k as f64), new_value)?;
    }

    Ok(result)
}

fn sort_compare(
    vm: &mut VM,
    comparefn: Option<Value>,
    x: Value,
    y: Value,
) -> Result<std::cmp::Ordering, crate::vm::error::RuntimeError> {
    if x.is_undefined() && y.is_undefined() {
        return Ok(std::cmp::Ordering::Equal);
    }
    if x.is_undefined() {
        return Ok(std::cmp::Ordering::Greater);
    }
    if y.is_undefined() {
        return Ok(std::cmp::Ordering::Less);
    }

    if let Some(comparefn) = comparefn {
        let value = vm.call_function(comparefn, &[x, y], Value::undefined())?;
        let number = value.to_number(&mut vm.factory.memory_allocator);
        if number.is_nan() || number == 0.0 {
            Ok(std::cmp::Ordering::Equal)
        } else if number < 0.0 {
            Ok(std::cmp::Ordering::Less)
        } else {
            Ok(std::cmp::Ordering::Greater)
        }
    } else {
        Ok(x.to_string().cmp(&y.to_string()))
    }
}

pub fn array_prototype_to_string(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let join_key = vm.factory.string("join".to_string());
    let join = vm.get_property_by_value(obj, join_key)?;
    if join.is_function_object() {
        vm.call_function(join, &[], obj)
    } else {
        Ok(vm.factory.string("[object Array]".to_string()))
    }
}

pub fn array_prototype_to_locale_string(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    if len == 0 {
        return Ok(vm.factory.string(""));
    }

    let mut result = String::new();
    for k in 0..len {
        if k > 0 {
            result.push(',');
        }
        let elem = vm.get_property_by_value(obj, Value::Number(k as f64))?;
        if elem.is_null() || elem.is_undefined() {
            continue;
        }
        let method_key = vm.factory.string("toLocaleString");
        let method = vm.get_property_by_value(elem, method_key)?;
        if !method.is_function_object() {
            return Err(vm
                .current_context
                .error_type("Array.prototype.toLocaleString"));
        }
        let locale_args = [
            args.get(0).copied().unwrap_or(Value::undefined()),
            args.get(1).copied().unwrap_or(Value::undefined()),
        ];
        let formatted = vm.call_function(method, &locale_args, elem)?;
        result.push_str(&formatted.to_string());
    }

    Ok(vm.factory.string(result))
}

pub fn array_prototype_push(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    const MAX_SAFE_INTEGER: usize = 9_007_199_254_740_991;

    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let mut len = to_length_with_abrupt(vm, len_value)?;
    if len
        .checked_add(args.len())
        .map(|value| value > MAX_SAFE_INTEGER)
        .unwrap_or(true)
    {
        return Err(vm.current_context.error_type("Array.prototype.push"));
    }

    for arg in args {
        vm.set_property_by_value(obj, Value::Number(len as f64), *arg)?;
        len += 1;
    }

    let length_key = vm.factory.string("length".to_string());
    vm.set_property_by_value(obj, length_key, Value::Number(len as f64))?;

    Ok(Value::Number(len as f64))
}

pub fn array_prototype_pop(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;

    if len == 0 {
        let length_key = vm.factory.string("length".to_string());
        vm.set_property_by_value(obj, length_key, Value::Number(0.0))?;
        return Ok(Value::undefined());
    }

    let new_len = len - 1;
    let key = Value::Number(new_len as f64);
    let val = vm.get_property_by_value(obj, key)?;
    let deleted = obj.delete_property_by_value(&mut vm.factory.memory_allocator, key)?;
    if !deleted {
        return Err(vm.current_context.error_type("Array.prototype.pop"));
    }
    let length_key = vm.factory.string("length".to_string());
    vm.set_property_by_value(obj, length_key, Value::Number(new_len as f64))?;

    Ok(val)
}

pub fn array_prototype_map(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let obj = to_object(vm, this)?;
    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(obj, length_key)?;
    let len = to_length_with_abrupt(vm, len_value)?;
    let callback = *args.get(0).unwrap_or(&Value::undefined());
    if !callback.is_function_object() {
        return Err(vm.current_context.error_type("Array.prototype.map"));
    }
    let this_arg = *args.get(1).unwrap_or(&Value::undefined());

    let result = vm.factory.array(vec![]);
    result.as_array_mut().set_length(len);

    for k in 0..len {
        let key = Value::Number(k as f64);
        if vm.has_property(key, obj)?.to_boolean() {
            let k_value = vm.get_property_by_value(obj, key)?;
            let mapped = vm.call_function(callback, &[k_value, key, obj], this_arg)?;
            create_data_property_or_throw(vm, result, key, mapped)?;
        }
    }

    Ok(result)
}
