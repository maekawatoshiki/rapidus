use super::helpers::{
    define_species_getter, define_well_known_symbol_property, relative_to_index, same_value_zero,
    set_function_length, to_index, to_integer_or_infinity, to_number,
};
use crate::vm::{
    jsvalue::{
        object::{
            AccessorProperty, ArrayBufferObjectInfo, DataProperty, ObjectKind, Property,
            TypedArrayObjectInfo,
        },
        symbol::{SYMBOL_ITERATOR_ID, SYMBOL_TO_STRING_TAG_ID},
        value::Value,
    },
    vm::{Factory, VMValueResult, VM},
};
use rustc_hash::FxHashMap;

const MAX_SAFE_INTEGER: f64 = 9_007_199_254_740_991.0;
const MAX_ARRAY_BUFFER_BYTES: usize = 1 << 30;

#[derive(Clone, Copy)]
struct TypedArraySpec {
    name: &'static str,
    element_size: usize,
    kind: TypedArrayElementKind,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TypedArrayElementKind {
    Int8,
    Uint8,
    Uint8Clamped,
    Int16,
    Uint16,
    Int32,
    Uint32,
    Float32,
    Float64,
    BigInt64,
    BigUint64,
}

const SPECS: &[TypedArraySpec] = &[
    TypedArraySpec {
        name: "Int8Array",
        element_size: 1,
        kind: TypedArrayElementKind::Int8,
    },
    TypedArraySpec {
        name: "Uint8Array",
        element_size: 1,
        kind: TypedArrayElementKind::Uint8,
    },
    TypedArraySpec {
        name: "Uint8ClampedArray",
        element_size: 1,
        kind: TypedArrayElementKind::Uint8Clamped,
    },
    TypedArraySpec {
        name: "Int16Array",
        element_size: 2,
        kind: TypedArrayElementKind::Int16,
    },
    TypedArraySpec {
        name: "Uint16Array",
        element_size: 2,
        kind: TypedArrayElementKind::Uint16,
    },
    TypedArraySpec {
        name: "Int32Array",
        element_size: 4,
        kind: TypedArrayElementKind::Int32,
    },
    TypedArraySpec {
        name: "Uint32Array",
        element_size: 4,
        kind: TypedArrayElementKind::Uint32,
    },
    TypedArraySpec {
        name: "Float32Array",
        element_size: 4,
        kind: TypedArrayElementKind::Float32,
    },
    TypedArraySpec {
        name: "Float64Array",
        element_size: 8,
        kind: TypedArrayElementKind::Float64,
    },
    TypedArraySpec {
        name: "BigInt64Array",
        element_size: 8,
        kind: TypedArrayElementKind::BigInt64,
    },
    TypedArraySpec {
        name: "BigUint64Array",
        element_size: 8,
        kind: TypedArrayElementKind::BigUint64,
    },
];

pub fn typed_array_constructors(factory: &mut Factory) -> Vec<(&'static str, Value)> {
    let common_prototype = typed_array_prototype(
        factory,
        TypedArraySpec {
            name: "TypedArray",
            element_size: 1,
            kind: TypedArrayElementKind::Uint8,
        },
    );
    let common_constructor = factory.builtin_function("TypedArray", typed_array_intrinsic);
    common_constructor.get_object_info().insert_property(
        "prototype".to_string(),
        Property::new_data(DataProperty::new(common_prototype)),
    );
    let from = builtin_method(factory, "from", typed_array_from, 1.0);
    let of = builtin_method(factory, "of", typed_array_of, 0.0);
    common_constructor.get_object_info().insert_property(
        "from".to_string(),
        Property::new_data(DataProperty::new(from).set_writable().set_configurable()),
    );
    common_constructor.get_object_info().insert_property(
        "of".to_string(),
        Property::new_data(DataProperty::new(of).set_writable().set_configurable()),
    );
    common_prototype.set_constructor(common_constructor);
    define_species_getter(factory, common_constructor);

    SPECS
        .iter()
        .map(|spec| {
            (
                spec.name,
                typed_array_constructor_object(
                    factory,
                    *spec,
                    common_constructor,
                    common_prototype,
                ),
            )
        })
        .collect()
}

pub fn typed_array_intrinsic(_vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    Err(crate::vm::error::RuntimeError::typeerr("TypedArray"))
}

pub fn typed_array_from(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !vm.is_constructor(this) {
        return Err(vm.current_context.error_type("TypedArray.from"));
    }
    let items = args.get(0).copied().unwrap_or(Value::undefined());
    if items.is_null() || items.is_undefined() {
        return Err(vm.current_context.error_type("TypedArray.from"));
    }

    let mapfn = args.get(1).copied().unwrap_or(Value::undefined());
    let mapping = !mapfn.is_undefined();
    if mapping && !mapfn.is_function_object() {
        return Err(vm.current_context.error_type("TypedArray.from"));
    }
    let this_arg = args.get(2).copied().unwrap_or(Value::undefined());

    let mut values = Vec::new();
    let iterator_key = vm.factory.well_known_symbol(SYMBOL_ITERATOR_ID);
    let using_iterator = vm.get_property_by_value(items, iterator_key)?;
    if !using_iterator.is_undefined() {
        if !using_iterator.is_function_object() {
            return Err(vm.current_context.error_type("TypedArray.from"));
        }
        let iterator = vm.call_function(using_iterator, &[], items)?;
        if !iterator.is_object() {
            return Err(vm.current_context.error_type("TypedArray.from"));
        }
        let next_key = vm.factory.string("next".to_string());
        let next_method = vm.get_property_by_value(iterator, next_key)?;
        if !next_method.is_function_object() {
            return Err(vm.current_context.error_type("TypedArray.from"));
        }
        loop {
            let next = vm.call_function(next_method, &[], iterator)?;
            if !next.is_object() {
                return Err(vm.current_context.error_type("TypedArray.from"));
            }
            let done_key = vm.factory.string("done".to_string());
            if vm.get_property_by_value(next, done_key)?.to_boolean() {
                break;
            }
            let value_key = vm.factory.string("value".to_string());
            values.push(vm.get_property_by_value(next, value_key)?);
        }
    } else {
        let length_key = vm.factory.string("length".to_string());
        let len_value = vm.get_property_by_value(items, length_key)?;
        let len = to_length(vm, len_value)?;
        for k in 0..len {
            values.push(vm.get_property_by_value(items, Value::Number(k as f64))?);
        }
    }

    let result = vm.construct_function(this, &[Value::Number(values.len() as f64)])?;
    for (index, value) in values.into_iter().enumerate() {
        let value = if mapping {
            vm.call_function(mapfn, &[value, Value::Number(index as f64)], this_arg)?
        } else {
            value
        };
        write_typed_array_index(vm, result, index, value)?;
    }
    Ok(result)
}

pub fn typed_array_of(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !vm.is_constructor(this) {
        return Err(vm.current_context.error_type("TypedArray.of"));
    }
    let result = vm.construct_function(this, &[Value::Number(args.len() as f64)])?;
    for (index, value) in args.iter().copied().enumerate() {
        write_typed_array_index(vm, result, index, value)?;
    }
    Ok(result)
}

pub fn is_typed_array(value: Value) -> bool {
    value.is_object() && matches!(value.get_object_info().kind, ObjectKind::TypedArray(_))
}

pub fn typed_array_get_index(
    factory: &mut crate::vm::vm::Factory,
    value: Value,
    index: usize,
) -> Option<Value> {
    let obj = value.get_object_info();
    let ObjectKind::TypedArray(ref info) = obj.kind else {
        return None;
    };
    let length = typed_array_effective_length(info)?;
    if index >= length || array_buffer_detached(info.buffer) {
        return None;
    }
    let bytes = array_buffer_bytes(info.buffer)?;
    let offset = info.byte_offset + index * info.element_size;
    if offset + info.element_size > bytes.len() {
        return None;
    }
    Some(read_element(
        factory,
        &bytes[offset..offset + info.element_size],
        info.kind,
    ))
}

pub fn typed_array_set_index(value: Value, index: usize, val: Value) -> Option<bool> {
    let obj = value.get_object_info();
    let ObjectKind::TypedArray(ref info) = obj.kind else {
        return None;
    };
    let Some(length) = typed_array_effective_length(info) else {
        return Some(false);
    };
    if index >= length || array_buffer_detached(info.buffer) {
        return Some(false);
    }
    let bytes = match info.kind {
        TypedArrayElementKind::BigInt64 | TypedArrayElementKind::BigUint64 => {
            let decimal = val.bigint_decimal().or_else(|| {
                if val.is_object() {
                    val.get_property("__bigint_data").bigint_decimal()
                } else {
                    None
                }
            })?;
            match info.kind {
                TypedArrayElementKind::BigInt64 => decimal
                    .parse::<i128>()
                    .map(|value| (value as i64).to_ne_bytes().to_vec())
                    .unwrap_or_else(|_| 0i64.to_ne_bytes().to_vec()),
                TypedArrayElementKind::BigUint64 => decimal
                    .parse::<i128>()
                    .map(|value| (value as u64).to_ne_bytes().to_vec())
                    .unwrap_or_else(|_| 0u64.to_ne_bytes().to_vec()),
                _ => unreachable!(),
            }
        }
        _ => {
            let number = val.to_number(&mut crate::gc::MemoryAllocator::new());
            write_element(number, info.kind)
        }
    };
    let offset = info.byte_offset + index * info.element_size;
    if let Some(buffer_bytes) = array_buffer_bytes(info.buffer) {
        if offset + bytes.len() > buffer_bytes.len() {
            return Some(false);
        }
    }
    write_array_buffer_bytes(info.buffer, offset, &bytes);
    Some(true)
}

pub fn typed_array_length(value: Value) -> Option<usize> {
    let obj = value.get_object_info();
    let ObjectKind::TypedArray(ref info) = obj.kind else {
        return None;
    };
    typed_array_effective_length(info).or(Some(0))
}

fn typed_array_constructor_object(
    factory: &mut Factory,
    spec: TypedArraySpec,
    common_constructor: Value,
    common_prototype: Value,
) -> Value {
    let prototype = typed_array_prototype(factory, spec);
    prototype.get_object_info().prototype = common_prototype;
    let constructor =
        factory.generate_builtin_constructor(spec.name, typed_array_constructor, prototype);
    constructor.get_object_info().prototype = common_constructor;
    set_function_length(constructor, 3.0);
    constructor.get_object_info().insert_property(
        "BYTES_PER_ELEMENT".to_string(),
        Property::new_data(DataProperty::new(Value::Number(spec.element_size as f64))),
    );
    constructor
}

pub fn typed_array_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !vm.builtin_constructor_call {
        return Err(vm.current_context.error_type("TypedArray constructor"));
    }
    let spec = typed_array_spec_from_constructor(this.get_prototype().get_property("constructor"))
        .or_else(|| {
            typed_array_spec_from_constructor_name(this.get_prototype().get_property("constructor"))
        })
        .unwrap_or(SPECS[1]);

    let first = args.get(0).copied().unwrap_or(Value::undefined());
    let mut initial_values = None;
    let (buffer, byte_offset, length, length_tracking) = if first.is_object()
        && matches!(first.get_object_info().kind, ObjectKind::ArrayBuffer(_))
    {
        construct_from_buffer(vm, spec, first, args)?
    } else if first.is_object() {
        let values = collect_values_from_source(vm, first)?;
        let length = values.len();
        let byte_length = length
            .checked_mul(spec.element_size)
            .ok_or_else(|| vm.current_context.error_range("TypedArray length"))?;
        ensure_allocatable(vm, byte_length)?;
        let buffer = create_array_buffer(vm, byte_length)?;
        initial_values = Some(values);
        (buffer, 0, length, false)
    } else {
        let length = to_index(vm, first)?;
        let byte_length = length
            .checked_mul(spec.element_size)
            .ok_or_else(|| vm.current_context.error_range("TypedArray length"))?;
        ensure_allocatable(vm, byte_length)?;
        let buffer = create_array_buffer(vm, byte_length)?;
        (buffer, 0, length, false)
    };

    this.get_object_info().kind = ObjectKind::TypedArray(TypedArrayObjectInfo {
        buffer,
        byte_offset,
        length,
        length_tracking,
        element_size: spec.element_size,
        kind: spec.kind,
        name: spec.name,
    });
    if let Some(values) = initial_values {
        for (index, value) in values.into_iter().enumerate() {
            write_typed_array_index(vm, this, index, value)?;
        }
    }
    Ok(this)
}

pub fn typed_array_prototype_buffer(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info(vm, this)?;
    Ok(info.buffer)
}

pub fn typed_array_prototype_byte_length(
    vm: &mut VM,
    _args: &[Value],
    this: Value,
) -> VMValueResult {
    let info = typed_array_info(vm, this)?;
    Ok(Value::Number(
        typed_array_effective_length(&info)
            .map(|length| length * info.element_size)
            .unwrap_or(0) as f64,
    ))
}

pub fn typed_array_prototype_byte_offset(
    vm: &mut VM,
    _args: &[Value],
    this: Value,
) -> VMValueResult {
    let info = typed_array_info(vm, this)?;
    Ok(Value::Number(
        if array_buffer_detached(info.buffer) || typed_array_effective_length(&info).is_none() {
            0.0
        } else {
            info.byte_offset as f64
        },
    ))
}

pub fn typed_array_prototype_length(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info(vm, this)?;
    Ok(Value::Number(
        typed_array_effective_length(&info).unwrap_or(0) as f64,
    ))
}

pub fn typed_array_prototype_to_string_tag(
    vm: &mut VM,
    _args: &[Value],
    this: Value,
) -> VMValueResult {
    if !this.is_object() {
        return Ok(Value::undefined());
    }
    match this.get_object_info().kind {
        ObjectKind::TypedArray(ref info) => Ok(vm.factory.string(info.name)),
        _ => Ok(Value::undefined()),
    }
}

pub fn typed_array_prototype_at(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let relative_index =
        to_integer_or_infinity(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let k = if relative_index >= 0.0 {
        relative_index
    } else {
        info.length as f64 + relative_index
    };
    if k < 0.0 || k >= info.length as f64 {
        return Ok(Value::undefined());
    }
    Ok(read_typed_array_index(vm, this, k as usize).unwrap_or(Value::undefined()))
}

pub fn typed_array_prototype_copy_within(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let target = to_integer_or_infinity(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let start = to_integer_or_infinity(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let end = match args.get(2).copied() {
        Some(end) if !end.is_undefined() => to_integer_or_infinity(vm, end)?,
        _ => info.length as f64,
    };
    let to = relative_to_index(target, info.length);
    let from = relative_to_index(start, info.length);
    let final_index = relative_to_index(end, info.length);
    let count = final_index
        .saturating_sub(from)
        .min(info.length.saturating_sub(to));
    if count == 0 {
        return Ok(this);
    }

    ensure_not_detached(vm, info.buffer)?;
    let bytes = array_buffer_bytes(info.buffer)
        .ok_or_else(|| vm.current_context.error_type("TypedArray buffer"))?;
    let mut copied = Vec::with_capacity(count);
    for n in 0..count {
        let offset = info.byte_offset + (from + n) * info.element_size;
        copied.push(bytes[offset..offset + info.element_size].to_vec());
    }
    for (n, element) in copied.iter().enumerate() {
        let offset = info.byte_offset + (to + n) * info.element_size;
        write_array_buffer_bytes(info.buffer, offset, element);
    }
    Ok(this)
}

pub fn typed_array_prototype_entries(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    typed_array_info_not_detached(vm, this)?;
    crate::builtins::array::array_prototype_entries(vm, args, this)
}

pub fn typed_array_prototype_keys(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    typed_array_info_not_detached(vm, this)?;
    crate::builtins::array::array_prototype_keys(vm, args, this)
}

pub fn typed_array_prototype_values(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    typed_array_info_not_detached(vm, this)?;
    crate::builtins::array::array_prototype_values(vm, args, this)
}

pub fn typed_array_prototype_every(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let callback = typed_array_callback(vm, args, "TypedArray.prototype.every")?;
    let this_arg = args.get(1).copied().unwrap_or(Value::undefined());
    for k in 0..info.length {
        let value = read_typed_array_index(vm, this, k).unwrap_or(Value::undefined());
        let selected =
            vm.call_function(callback, &[value, Value::Number(k as f64), this], this_arg)?;
        if !selected.to_boolean() {
            return Ok(Value::bool(false));
        }
    }
    Ok(Value::bool(true))
}

pub fn typed_array_prototype_fill(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let value = args.get(0).copied().unwrap_or(Value::undefined());
    let start = to_integer_or_infinity(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let mut k = relative_to_index(start, info.length);
    let end = match args.get(2).copied() {
        Some(end) if !end.is_undefined() => {
            relative_to_index(to_integer_or_infinity(vm, end)?, info.length)
        }
        _ => info.length,
    };
    let bytes = match info.kind {
        TypedArrayElementKind::BigInt64 | TypedArrayElementKind::BigUint64 => {
            write_bigint_element(vm, value, info.kind)?
        }
        _ => write_element(to_number(vm, value)?, info.kind),
    };
    ensure_not_detached(vm, info.buffer)?;
    while k < end {
        let offset = info.byte_offset + k * info.element_size;
        write_array_buffer_bytes(info.buffer, offset, &bytes);
        k += 1;
    }
    Ok(this)
}

pub fn typed_array_prototype_filter(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let callback = args.get(0).copied().unwrap_or(Value::undefined());
    if !callback.is_function_object() {
        return Err(vm.current_context.error_type("TypedArray.prototype.filter"));
    }
    let this_arg = args.get(1).copied().unwrap_or(Value::undefined());
    let mut selected = Vec::new();
    for k in 0..info.length {
        let value = read_typed_array_index(vm, this, k).unwrap_or(Value::undefined());
        if vm
            .call_function(callback, &[value, Value::Number(k as f64), this], this_arg)?
            .to_boolean()
        {
            selected.push(value);
        }
    }
    create_typed_array_from_values(vm, info.spec(), &selected)
}

pub fn typed_array_prototype_find(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    typed_array_find(vm, args, this, false, false)
}

pub fn typed_array_prototype_find_index(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    typed_array_find(vm, args, this, false, true)
}

pub fn typed_array_prototype_find_last(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    typed_array_find(vm, args, this, true, false)
}

pub fn typed_array_prototype_find_last_index(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    typed_array_find(vm, args, this, true, true)
}

pub fn typed_array_prototype_for_each(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let callback = typed_array_callback(vm, args, "TypedArray.prototype.forEach")?;
    let this_arg = args.get(1).copied().unwrap_or(Value::undefined());
    for k in 0..info.length {
        let value = read_typed_array_index(vm, this, k).unwrap_or(Value::undefined());
        vm.call_function(callback, &[value, Value::Number(k as f64), this], this_arg)?;
    }
    Ok(Value::undefined())
}

pub fn typed_array_prototype_includes(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let search_element = args.get(0).copied().unwrap_or(Value::undefined());
    let from_index =
        to_integer_or_infinity(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let mut k = relative_to_index(from_index, info.length);
    while k < info.length {
        let value = read_typed_array_index(vm, this, k).unwrap_or(Value::undefined());
        if same_value_zero(value, search_element) {
            return Ok(Value::bool(true));
        }
        k += 1;
    }
    Ok(Value::bool(false))
}

pub fn typed_array_prototype_index_of(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let search_element = args.get(0).copied().unwrap_or(Value::undefined());
    let from_index =
        to_integer_or_infinity(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let mut k = relative_to_index(from_index, info.length);
    while k < info.length {
        let value = read_typed_array_index(vm, this, k).unwrap_or(Value::undefined());
        if value.strict_eq_bool(search_element) {
            return Ok(Value::Number(k as f64));
        }
        k += 1;
    }
    Ok(Value::Number(-1.0))
}

pub fn typed_array_prototype_join(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let separator = match args.get(0).copied() {
        Some(value) if !value.is_undefined() => value.to_string(),
        _ => ",".to_string(),
    };
    let mut parts = Vec::with_capacity(info.length);
    for k in 0..info.length {
        let value = read_typed_array_index(vm, this, k).unwrap_or(Value::undefined());
        parts.push(value.to_string());
    }
    Ok(vm.factory.string(parts.join(&separator)))
}

pub fn typed_array_prototype_last_index_of(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    if info.length == 0 {
        return Ok(Value::Number(-1.0));
    }
    let search_element = args.get(0).copied().unwrap_or(Value::undefined());
    let from_index = match args.get(1).copied() {
        Some(value) if !value.is_undefined() => to_integer_or_infinity(vm, value)?,
        _ => info.length as f64 - 1.0,
    };
    let mut k = if from_index == f64::INFINITY {
        info.length - 1
    } else if from_index < 0.0 {
        let index = info.length as f64 + from_index;
        if index < 0.0 {
            return Ok(Value::Number(-1.0));
        }
        index as usize
    } else {
        (from_index as usize).min(info.length - 1)
    };
    loop {
        let value = read_typed_array_index(vm, this, k).unwrap_or(Value::undefined());
        if value.strict_eq_bool(search_element) {
            return Ok(Value::Number(k as f64));
        }
        if k == 0 {
            break;
        }
        k -= 1;
    }
    Ok(Value::Number(-1.0))
}

pub fn typed_array_prototype_map(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let callback = args.get(0).copied().unwrap_or(Value::undefined());
    if !callback.is_function_object() {
        return Err(vm.current_context.error_type("TypedArray.prototype.map"));
    }
    let this_arg = args.get(1).copied().unwrap_or(Value::undefined());
    let result = create_typed_array(vm, info.spec(), info.length)?;
    for k in 0..info.length {
        let value = read_typed_array_index(vm, this, k).unwrap_or(Value::undefined());
        let mapped =
            vm.call_function(callback, &[value, Value::Number(k as f64), this], this_arg)?;
        write_typed_array_index(vm, result, k, mapped)?;
    }
    Ok(result)
}

pub fn typed_array_prototype_reduce(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    typed_array_reduce(vm, args, this, false)
}

pub fn typed_array_prototype_reduce_right(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    typed_array_reduce(vm, args, this, true)
}

pub fn typed_array_prototype_reverse(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let middle = info.length / 2;
    for lower in 0..middle {
        let upper = info.length - lower - 1;
        let lower_value = read_typed_array_index(vm, this, lower).unwrap_or(Value::undefined());
        let upper_value = read_typed_array_index(vm, this, upper).unwrap_or(Value::undefined());
        write_typed_array_index(vm, this, lower, upper_value)?;
        write_typed_array_index(vm, this, upper, lower_value)?;
    }
    Ok(this)
}

pub fn typed_array_prototype_set(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let target_info = typed_array_info_not_detached(vm, this)?;
    let source = args.get(0).copied().unwrap_or(Value::undefined());
    let target_offset = to_index(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let mut values = Vec::new();

    if is_typed_array(source) {
        let source_info = typed_array_info_not_detached(vm, source)?;
        if target_offset
            .checked_add(source_info.length)
            .map(|end| end > target_info.length)
            .unwrap_or(true)
        {
            return Err(vm.current_context.error_range("TypedArray.prototype.set"));
        }
        for k in 0..source_info.length {
            values.push(read_typed_array_index(vm, source, k).unwrap_or(Value::undefined()));
        }
    } else {
        if source.is_null() || source.is_undefined() {
            return Err(vm.current_context.error_type("TypedArray.prototype.set"));
        }
        let length_key = vm.factory.string("length".to_string());
        let source_len_value = vm.get_property_by_value(source, length_key)?;
        let source_len = to_length(vm, source_len_value)?;
        if target_offset
            .checked_add(source_len)
            .map(|end| end > target_info.length)
            .unwrap_or(true)
        {
            return Err(vm.current_context.error_range("TypedArray.prototype.set"));
        }
        for k in 0..source_len {
            values.push(vm.get_property_by_value(source, Value::Number(k as f64))?);
        }
    }

    ensure_not_detached(vm, target_info.buffer)?;
    for (index, value) in values.into_iter().enumerate() {
        write_typed_array_index(vm, this, target_offset + index, value)?;
    }
    Ok(Value::undefined())
}

pub fn typed_array_prototype_slice(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let start = to_integer_or_infinity(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let mut k = relative_to_index(start, info.length);
    let final_index = match args.get(1).copied() {
        Some(end) if !end.is_undefined() => {
            relative_to_index(to_integer_or_infinity(vm, end)?, info.length)
        }
        _ => info.length,
    };
    let count = final_index.saturating_sub(k);
    let result = create_typed_array(vm, info.spec(), count)?;
    let mut n = 0usize;
    while k < final_index {
        let value = read_typed_array_index(vm, this, k).unwrap_or(Value::undefined());
        write_typed_array_index(vm, result, n, value)?;
        k += 1;
        n += 1;
    }
    Ok(result)
}

pub fn typed_array_prototype_some(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let callback = typed_array_callback(vm, args, "TypedArray.prototype.some")?;
    let this_arg = args.get(1).copied().unwrap_or(Value::undefined());
    for k in 0..info.length {
        let value = read_typed_array_index(vm, this, k).unwrap_or(Value::undefined());
        let selected =
            vm.call_function(callback, &[value, Value::Number(k as f64), this], this_arg)?;
        if selected.to_boolean() {
            return Ok(Value::bool(true));
        }
    }
    Ok(Value::bool(false))
}

pub fn typed_array_prototype_sort(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let comparefn = args.get(0).copied().unwrap_or(Value::undefined());
    if !comparefn.is_undefined() && !comparefn.is_function_object() {
        return Err(vm.current_context.error_type("TypedArray.prototype.sort"));
    }
    let comparefn = if comparefn.is_function_object() {
        Some(comparefn)
    } else {
        None
    };
    let mut items = Vec::with_capacity(info.length);
    for k in 0..info.length {
        items.push(read_typed_array_index(vm, this, k).unwrap_or(Value::undefined()));
    }
    for i in 1..items.len() {
        let mut j = i;
        while j > 0
            && typed_array_sort_compare(vm, comparefn, items[j - 1], items[j], info.kind)?
                == std::cmp::Ordering::Greater
        {
            items.swap(j - 1, j);
            j -= 1;
        }
    }
    ensure_not_detached(vm, info.buffer)?;
    for (index, value) in items.into_iter().enumerate() {
        write_typed_array_index(vm, this, index, value)?;
    }
    Ok(this)
}

pub fn typed_array_prototype_subarray(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let start = to_integer_or_infinity(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let begin = relative_to_index(start, info.length);
    let end = match args.get(1).copied() {
        Some(end) if !end.is_undefined() => {
            relative_to_index(to_integer_or_infinity(vm, end)?, info.length)
        }
        _ => info.length,
    };
    let length = end.saturating_sub(begin);
    create_typed_array_view(
        vm,
        info.spec(),
        info.buffer,
        info.byte_offset + begin * info.element_size,
        length,
    )
}

pub fn typed_array_prototype_to_locale_string(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    typed_array_prototype_join(vm, args, this)
}

pub fn typed_array_prototype_to_reversed(
    vm: &mut VM,
    _args: &[Value],
    this: Value,
) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let result = create_typed_array(vm, info.spec(), info.length)?;
    for k in 0..info.length {
        let value =
            read_typed_array_index(vm, this, info.length - k - 1).unwrap_or(Value::undefined());
        write_typed_array_index(vm, result, k, value)?;
    }
    Ok(result)
}

pub fn typed_array_prototype_to_sorted(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let result = typed_array_prototype_slice(vm, &[], this)?;
    typed_array_prototype_sort(vm, args, result)?;
    ensure_not_detached(vm, info.buffer)?;
    Ok(result)
}

pub fn typed_array_prototype_to_string(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    typed_array_prototype_join(vm, args, this)
}

pub fn typed_array_prototype_with(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let index = to_integer_or_infinity(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let actual_index = if index >= 0.0 {
        index
    } else {
        info.length as f64 + index
    };
    if actual_index < 0.0 || actual_index >= info.length as f64 {
        return Err(vm.current_context.error_range("TypedArray.prototype.with"));
    }
    let result = typed_array_prototype_slice(vm, &[], this)?;
    write_typed_array_index(
        vm,
        result,
        actual_index as usize,
        args.get(1).copied().unwrap_or(Value::undefined()),
    )?;
    Ok(result)
}

fn typed_array_prototype(factory: &mut Factory, spec: TypedArraySpec) -> Value {
    let buffer = builtin_getter(factory, "get buffer", typed_array_prototype_buffer);
    let byte_length = builtin_getter(factory, "get byteLength", typed_array_prototype_byte_length);
    let byte_offset = builtin_getter(factory, "get byteOffset", typed_array_prototype_byte_offset);
    let length = builtin_getter(factory, "get length", typed_array_prototype_length);
    let common_methods = if spec.name == "TypedArray" {
        vec![
            (
                "at",
                typed_array_prototype_at as crate::builtins::BuiltinFuncTy,
                1.0,
            ),
            ("copyWithin", typed_array_prototype_copy_within, 2.0),
            ("entries", typed_array_prototype_entries, 0.0),
            ("every", typed_array_prototype_every, 1.0),
            ("fill", typed_array_prototype_fill, 1.0),
            ("filter", typed_array_prototype_filter, 1.0),
            ("find", typed_array_prototype_find, 1.0),
            ("findIndex", typed_array_prototype_find_index, 1.0),
            ("findLast", typed_array_prototype_find_last, 1.0),
            ("findLastIndex", typed_array_prototype_find_last_index, 1.0),
            ("forEach", typed_array_prototype_for_each, 1.0),
            ("includes", typed_array_prototype_includes, 1.0),
            ("indexOf", typed_array_prototype_index_of, 1.0),
            ("join", typed_array_prototype_join, 1.0),
            ("keys", typed_array_prototype_keys, 0.0),
            ("lastIndexOf", typed_array_prototype_last_index_of, 1.0),
            ("map", typed_array_prototype_map, 1.0),
            ("reduce", typed_array_prototype_reduce, 1.0),
            ("reduceRight", typed_array_prototype_reduce_right, 1.0),
            ("reverse", typed_array_prototype_reverse, 0.0),
            ("set", typed_array_prototype_set, 1.0),
            ("slice", typed_array_prototype_slice, 2.0),
            ("some", typed_array_prototype_some, 1.0),
            ("sort", typed_array_prototype_sort, 1.0),
            ("subarray", typed_array_prototype_subarray, 2.0),
            (
                "toLocaleString",
                typed_array_prototype_to_locale_string,
                0.0,
            ),
            ("toReversed", typed_array_prototype_to_reversed, 0.0),
            ("toSorted", typed_array_prototype_to_sorted, 1.0),
            ("toString", typed_array_prototype_to_string, 0.0),
            ("values", typed_array_prototype_values, 0.0),
            ("with", typed_array_prototype_with, 2.0),
        ]
    } else {
        Vec::new()
    };
    let mut property = FxHashMap::default();
    property.insert(
        "buffer".to_string(),
        Property::Accessor(AccessorProperty {
            get: buffer,
            set: Value::undefined(),
            enumerable: false,
            configurable: true,
        }),
    );
    property.insert(
        "byteLength".to_string(),
        Property::Accessor(AccessorProperty {
            get: byte_length,
            set: Value::undefined(),
            enumerable: false,
            configurable: true,
        }),
    );
    property.insert(
        "byteOffset".to_string(),
        Property::Accessor(AccessorProperty {
            get: byte_offset,
            set: Value::undefined(),
            enumerable: false,
            configurable: true,
        }),
    );
    property.insert(
        "length".to_string(),
        Property::Accessor(AccessorProperty {
            get: length,
            set: Value::undefined(),
            enumerable: false,
            configurable: true,
        }),
    );
    property.insert(
        "BYTES_PER_ELEMENT".to_string(),
        Property::new_data(DataProperty::new(Value::Number(spec.element_size as f64))),
    );
    for (name, func, length) in common_methods {
        property.insert(
            name.to_string(),
            Property::new_data(
                DataProperty::new(builtin_method(factory, name, func, length))
                    .set_writable()
                    .set_configurable(),
            ),
        );
    }

    let is_common = spec.name == "TypedArray";
    let iterator = property
        .get("values")
        .copied()
        .unwrap_or_else(|| Property::new_data(DataProperty::new(Value::undefined())));
    let prototype = Value::Object(factory.alloc(crate::vm::jsvalue::object::Object {
        kind: ObjectKind::Ordinary,
        prototype: factory.object_prototypes.object,
        property,
        property_order: Vec::new(),
        private_elements: rustc_hash::FxHashMap::default(),
        sym_property: FxHashMap::default(),
        sym_property_order: Vec::new(),
        extensible: true,
    }));
    if is_common {
        let to_string_tag = builtin_getter(
            factory,
            "get [Symbol.toStringTag]",
            typed_array_prototype_to_string_tag,
        );
        define_well_known_symbol_property(
            factory,
            prototype,
            SYMBOL_TO_STRING_TAG_ID,
            Property::Accessor(AccessorProperty {
                get: to_string_tag,
                set: Value::undefined(),
                enumerable: false,
                configurable: true,
            }),
        );
        define_well_known_symbol_property(factory, prototype, SYMBOL_ITERATOR_ID, iterator);
    }
    prototype
}

fn collect_values_from_source(
    vm: &mut VM,
    source: Value,
) -> Result<Vec<Value>, crate::vm::error::RuntimeError> {
    let iterator_key = vm.factory.well_known_symbol(SYMBOL_ITERATOR_ID);
    let using_iterator = vm.get_property_by_value(source, iterator_key)?;
    if !using_iterator.is_undefined() {
        if !using_iterator.is_function_object() {
            return Err(vm.current_context.error_type("TypedArray constructor"));
        }
        let iterator = vm.call_function(using_iterator, &[], source)?;
        if !iterator.is_object() {
            return Err(vm.current_context.error_type("TypedArray constructor"));
        }
        let next_key = vm.factory.string("next".to_string());
        let next_method = vm.get_property_by_value(iterator, next_key)?;
        if !next_method.is_function_object() {
            return Err(vm.current_context.error_type("TypedArray constructor"));
        }
        let mut values = Vec::new();
        loop {
            let next = vm.call_function(next_method, &[], iterator)?;
            if !next.is_object() {
                return Err(vm.current_context.error_type("TypedArray constructor"));
            }
            let done_key = vm.factory.string("done".to_string());
            if vm.get_property_by_value(next, done_key)?.to_boolean() {
                return Ok(values);
            }
            let value_key = vm.factory.string("value".to_string());
            values.push(vm.get_property_by_value(next, value_key)?);
        }
    }

    let length_key = vm.factory.string("length".to_string());
    let len_value = vm.get_property_by_value(source, length_key)?;
    let len = to_length(vm, len_value)?;
    let mut values = Vec::with_capacity(len);
    for k in 0..len {
        values.push(vm.get_property_by_value(source, Value::Number(k as f64))?);
    }
    Ok(values)
}

fn construct_from_buffer(
    vm: &mut VM,
    spec: TypedArraySpec,
    buffer: Value,
    args: &[Value],
) -> Result<(Value, usize, usize, bool), crate::vm::error::RuntimeError> {
    if array_buffer_detached(buffer) {
        return Err(vm.current_context.error_type("ArrayBuffer is detached"));
    }
    let buffer_len = array_buffer_byte_length(buffer)?;
    let byte_offset = to_index(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    if byte_offset % spec.element_size != 0 {
        return Err(vm.current_context.error_range("TypedArray byteOffset"));
    }
    if byte_offset > buffer_len {
        return Err(vm.current_context.error_range("TypedArray byteOffset"));
    }
    let (length, length_tracking) = match args.get(2).copied() {
        Some(value) if !value.is_undefined() => {
            let length = to_index(vm, value)?;
            let byte_length = length
                .checked_mul(spec.element_size)
                .ok_or_else(|| vm.current_context.error_range("TypedArray length"))?;
            if byte_offset
                .checked_add(byte_length)
                .map(|end| end > buffer_len)
                .unwrap_or(true)
            {
                return Err(vm.current_context.error_range("TypedArray length"));
            }
            (length, false)
        }
        _ => {
            let remaining = buffer_len - byte_offset;
            if remaining % spec.element_size != 0 {
                return Err(vm.current_context.error_range("TypedArray length"));
            }
            (
                remaining / spec.element_size,
                array_buffer_resizable(buffer),
            )
        }
    };
    Ok((buffer, byte_offset, length, length_tracking))
}

fn typed_array_info(
    vm: &mut VM,
    value: Value,
) -> Result<TypedArrayObjectInfo, crate::vm::error::RuntimeError> {
    if !value.is_object() {
        return Err(vm.current_context.error_type("TypedArray receiver"));
    }
    match value.get_object_info().kind {
        ObjectKind::TypedArray(ref info) => Ok(info.clone()),
        _ => Err(vm.current_context.error_type("TypedArray receiver")),
    }
}

impl TypedArrayObjectInfo {
    fn spec(&self) -> TypedArraySpec {
        TypedArraySpec {
            name: self.name,
            element_size: self.element_size,
            kind: self.kind,
        }
    }
}

fn typed_array_info_not_detached(
    vm: &mut VM,
    value: Value,
) -> Result<TypedArrayObjectInfo, crate::vm::error::RuntimeError> {
    let mut info = typed_array_info(vm, value)?;
    ensure_not_detached(vm, info.buffer)?;
    let length = typed_array_effective_length(&info)
        .ok_or_else(|| vm.current_context.error_type("TypedArray is out of bounds"))?;
    info.length = length;
    Ok(info)
}

fn ensure_not_detached(vm: &mut VM, buffer: Value) -> Result<(), crate::vm::error::RuntimeError> {
    if array_buffer_detached(buffer) {
        Err(vm.current_context.error_type("ArrayBuffer is detached"))
    } else {
        Ok(())
    }
}

fn typed_array_effective_length(info: &TypedArrayObjectInfo) -> Option<usize> {
    if array_buffer_detached(info.buffer) {
        return Some(0);
    }
    let buffer_len = array_buffer_byte_length(info.buffer).ok()?;
    if info.byte_offset > buffer_len {
        return None;
    }
    if info.length_tracking {
        return Some((buffer_len - info.byte_offset) / info.element_size);
    }
    let byte_length = info.length.checked_mul(info.element_size)?;
    if info.byte_offset.checked_add(byte_length)? > buffer_len {
        None
    } else {
        Some(info.length)
    }
}

fn typed_array_callback(
    vm: &mut VM,
    args: &[Value],
    name: &str,
) -> Result<Value, crate::vm::error::RuntimeError> {
    let callback = args.get(0).copied().unwrap_or(Value::undefined());
    if !callback.is_function_object() {
        return Err(vm.current_context.error_type(name));
    }
    Ok(callback)
}

fn typed_array_find(
    vm: &mut VM,
    args: &[Value],
    this: Value,
    from_end: bool,
    return_index: bool,
) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let callback = typed_array_callback(vm, args, "TypedArray.prototype.find")?;
    let this_arg = args.get(1).copied().unwrap_or(Value::undefined());
    for step in 0..info.length {
        let k = if from_end {
            info.length - step - 1
        } else {
            step
        };
        let value = read_typed_array_index(vm, this, k).unwrap_or(Value::undefined());
        let selected =
            vm.call_function(callback, &[value, Value::Number(k as f64), this], this_arg)?;
        if selected.to_boolean() {
            return Ok(if return_index {
                Value::Number(k as f64)
            } else {
                value
            });
        }
    }
    Ok(if return_index {
        Value::Number(-1.0)
    } else {
        Value::undefined()
    })
}

fn typed_array_reduce(vm: &mut VM, args: &[Value], this: Value, from_end: bool) -> VMValueResult {
    let info = typed_array_info_not_detached(vm, this)?;
    let callback = typed_array_callback(vm, args, "TypedArray.prototype.reduce")?;
    if info.length == 0 && args.len() < 2 {
        return Err(vm.current_context.error_type("TypedArray.prototype.reduce"));
    }

    let mut index = if from_end { info.length } else { 0 };
    let mut accumulator = if args.len() >= 2 {
        args[1]
    } else if from_end {
        index -= 1;
        read_typed_array_index(vm, this, index).unwrap_or(Value::undefined())
    } else {
        let value = read_typed_array_index(vm, this, index).unwrap_or(Value::undefined());
        index += 1;
        value
    };

    if from_end {
        while index > 0 {
            index -= 1;
            let value = read_typed_array_index(vm, this, index).unwrap_or(Value::undefined());
            accumulator = vm.call_function(
                callback,
                &[accumulator, value, Value::Number(index as f64), this],
                Value::undefined(),
            )?;
        }
    } else {
        while index < info.length {
            let value = read_typed_array_index(vm, this, index).unwrap_or(Value::undefined());
            accumulator = vm.call_function(
                callback,
                &[accumulator, value, Value::Number(index as f64), this],
                Value::undefined(),
            )?;
            index += 1;
        }
    }
    Ok(accumulator)
}

fn typed_array_sort_compare(
    vm: &mut VM,
    comparefn: Option<Value>,
    x: Value,
    y: Value,
    kind: TypedArrayElementKind,
) -> Result<std::cmp::Ordering, crate::vm::error::RuntimeError> {
    if let Some(comparefn) = comparefn {
        let value = vm.call_function(comparefn, &[x, y], Value::undefined())?;
        let number = to_number(vm, value)?;
        return Ok(if number.is_nan() || number == 0.0 {
            std::cmp::Ordering::Equal
        } else if number < 0.0 {
            std::cmp::Ordering::Less
        } else {
            std::cmp::Ordering::Greater
        });
    }

    match kind {
        TypedArrayElementKind::BigInt64 | TypedArrayElementKind::BigUint64 => {
            let x = x
                .bigint_decimal()
                .and_then(|value| value.parse::<i128>().ok())
                .unwrap_or(0);
            let y = y
                .bigint_decimal()
                .and_then(|value| value.parse::<i128>().ok())
                .unwrap_or(0);
            Ok(x.cmp(&y))
        }
        _ => {
            let x = x.into_number();
            let y = y.into_number();
            if x.is_nan() && y.is_nan() {
                Ok(std::cmp::Ordering::Equal)
            } else if x.is_nan() {
                Ok(std::cmp::Ordering::Greater)
            } else if y.is_nan() {
                Ok(std::cmp::Ordering::Less)
            } else if x < y {
                Ok(std::cmp::Ordering::Less)
            } else if x > y {
                Ok(std::cmp::Ordering::Greater)
            } else {
                Ok(std::cmp::Ordering::Equal)
            }
        }
    }
}

fn read_typed_array_index(vm: &mut VM, value: Value, index: usize) -> Option<Value> {
    typed_array_get_index(&mut vm.factory, value, index)
}

pub fn write_typed_array_index(
    vm: &mut VM,
    value: Value,
    index: usize,
    val: Value,
) -> Result<(), crate::vm::error::RuntimeError> {
    let info = typed_array_info_not_detached(vm, value)?;
    if index >= info.length {
        return Err(vm.current_context.error_range("TypedArray index"));
    }
    let bytes = match info.kind {
        TypedArrayElementKind::BigInt64 | TypedArrayElementKind::BigUint64 => {
            write_bigint_element(vm, val, info.kind)?
        }
        _ => write_element(to_number(vm, val)?, info.kind),
    };
    let offset = info.byte_offset + index * info.element_size;
    write_array_buffer_bytes(info.buffer, offset, &bytes);
    Ok(())
}

fn create_typed_array(
    vm: &mut VM,
    spec: TypedArraySpec,
    length: usize,
) -> Result<Value, crate::vm::error::RuntimeError> {
    let byte_length = length
        .checked_mul(spec.element_size)
        .ok_or_else(|| vm.current_context.error_range("TypedArray length"))?;
    let buffer = create_array_buffer(vm, byte_length)?;
    create_typed_array_view(vm, spec, buffer, 0, length)
}

fn create_typed_array_view(
    vm: &mut VM,
    spec: TypedArraySpec,
    buffer: Value,
    byte_offset: usize,
    length: usize,
) -> Result<Value, crate::vm::error::RuntimeError> {
    let byte_length = length
        .checked_mul(spec.element_size)
        .ok_or_else(|| vm.current_context.error_range("TypedArray length"))?;
    let buffer_len = array_buffer_byte_length(buffer)?;
    if byte_offset
        .checked_add(byte_length)
        .map(|end| end > buffer_len)
        .unwrap_or(true)
    {
        return Err(vm.current_context.error_range("TypedArray length"));
    }
    let prototype = vm
        .current_context
        .lex_env()
        .get_value(spec.name)
        .map(|constructor| constructor.get_property("prototype"))
        .unwrap_or(vm.factory.object_prototypes.object);
    Ok(Value::Object(vm.factory.alloc(
        crate::vm::jsvalue::object::Object {
            kind: ObjectKind::TypedArray(TypedArrayObjectInfo {
                buffer,
                byte_offset,
                length,
                length_tracking: false,
                element_size: spec.element_size,
                kind: spec.kind,
                name: spec.name,
            }),
            prototype,
            property: FxHashMap::default(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        },
    )))
}

fn create_typed_array_from_values(
    vm: &mut VM,
    spec: TypedArraySpec,
    values: &[Value],
) -> VMValueResult {
    let result = create_typed_array(vm, spec, values.len())?;
    for (index, value) in values.iter().copied().enumerate() {
        write_typed_array_index(vm, result, index, value)?;
    }
    Ok(result)
}

fn typed_array_spec_from_constructor(value: Value) -> Option<TypedArraySpec> {
    if !value.is_function_object() {
        return None;
    }
    typed_array_spec_from_name(value.as_function().name.as_deref().unwrap_or(""))
}

fn typed_array_spec_from_constructor_name(value: Value) -> Option<TypedArraySpec> {
    if !value.is_function_object() {
        return None;
    }
    typed_array_spec_from_name(value.as_function().name.as_deref().unwrap_or(""))
}

fn typed_array_spec_from_name(name: &str) -> Option<TypedArraySpec> {
    SPECS.iter().copied().find(|spec| spec.name == name)
}

fn create_array_buffer(
    vm: &mut VM,
    byte_length: usize,
) -> Result<Value, crate::vm::error::RuntimeError> {
    ensure_allocatable(vm, byte_length)?;
    Ok(Value::Object(vm.factory.alloc(
        crate::vm::jsvalue::object::Object {
            kind: ObjectKind::ArrayBuffer(ArrayBufferObjectInfo {
                bytes: vec![0; byte_length],
                max_byte_length: None,
                detached: false,
                shared: false,
            }),
            prototype: vm.factory.object_prototypes.array_buffer,
            property: FxHashMap::default(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        },
    )))
}

fn array_buffer_byte_length(value: Value) -> Result<usize, crate::vm::error::RuntimeError> {
    match value.get_object_info().kind {
        ObjectKind::ArrayBuffer(ref info) => Ok(if info.detached { 0 } else { info.bytes.len() }),
        _ => Err(crate::vm::error::RuntimeError::typeerr("TypedArray buffer")),
    }
}

fn array_buffer_detached(value: Value) -> bool {
    if !value.is_object() {
        return false;
    }
    match value.get_object_info().kind {
        ObjectKind::ArrayBuffer(ref info) => info.detached,
        _ => false,
    }
}

fn array_buffer_resizable(value: Value) -> bool {
    if !value.is_object() {
        return false;
    }
    match value.get_object_info().kind {
        ObjectKind::ArrayBuffer(ref info) => info.max_byte_length.is_some(),
        _ => false,
    }
}

fn array_buffer_bytes(value: Value) -> Option<Vec<u8>> {
    match value.get_object_info().kind {
        ObjectKind::ArrayBuffer(ref info) => Some(info.bytes.clone()),
        _ => None,
    }
}

fn write_array_buffer_bytes(value: Value, offset: usize, data: &[u8]) {
    let mut obj = value.get_object_info();
    if let ObjectKind::ArrayBuffer(ref mut info) = obj.kind {
        if offset + data.len() > info.bytes.len() {
            return;
        }
        info.bytes[offset..offset + data.len()].copy_from_slice(data);
    }
}

fn read_element(
    factory: &mut crate::vm::vm::Factory,
    bytes: &[u8],
    kind: TypedArrayElementKind,
) -> Value {
    match kind {
        TypedArrayElementKind::Int8 => Value::Number(bytes[0] as i8 as f64),
        TypedArrayElementKind::Uint8 | TypedArrayElementKind::Uint8Clamped => {
            Value::Number(bytes[0] as f64)
        }
        TypedArrayElementKind::Int16 => {
            Value::Number(i16::from_ne_bytes([bytes[0], bytes[1]]) as f64)
        }
        TypedArrayElementKind::Uint16 => {
            Value::Number(u16::from_ne_bytes([bytes[0], bytes[1]]) as f64)
        }
        TypedArrayElementKind::Int32 => {
            Value::Number(i32::from_ne_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]) as f64)
        }
        TypedArrayElementKind::Uint32 => {
            Value::Number(u32::from_ne_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]) as f64)
        }
        TypedArrayElementKind::Float32 => {
            Value::Number(f32::from_ne_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]) as f64)
        }
        TypedArrayElementKind::Float64 => Value::Number(f64::from_ne_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ])),
        TypedArrayElementKind::BigInt64 => factory.bigint(
            i64::from_ne_bytes([
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            ])
            .to_string(),
        ),
        TypedArrayElementKind::BigUint64 => factory.bigint(
            u64::from_ne_bytes([
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            ])
            .to_string(),
        ),
    }
}

fn write_element(value: f64, kind: TypedArrayElementKind) -> Vec<u8> {
    match kind {
        TypedArrayElementKind::Int8 | TypedArrayElementKind::Uint8 => {
            vec![to_uint_n(value, 8) as u8]
        }
        TypedArrayElementKind::Uint8Clamped => vec![clamp_u8(value)],
        TypedArrayElementKind::Int16 => (to_int_n(value, 16) as i16).to_ne_bytes().to_vec(),
        TypedArrayElementKind::Uint16 => (to_uint_n(value, 16) as u16).to_ne_bytes().to_vec(),
        TypedArrayElementKind::Int32 => (to_int_n(value, 32) as i32).to_ne_bytes().to_vec(),
        TypedArrayElementKind::Uint32 => (to_uint_n(value, 32) as u32).to_ne_bytes().to_vec(),
        TypedArrayElementKind::Float32 => (value as f32).to_ne_bytes().to_vec(),
        TypedArrayElementKind::Float64 => value.to_ne_bytes().to_vec(),
        TypedArrayElementKind::BigInt64 => (value as i64).to_ne_bytes().to_vec(),
        TypedArrayElementKind::BigUint64 => to_uint_n(value, 64).to_ne_bytes().to_vec(),
    }
}

fn write_bigint_element(
    vm: &mut VM,
    value: Value,
    kind: TypedArrayElementKind,
) -> Result<Vec<u8>, crate::vm::error::RuntimeError> {
    let decimal = value
        .bigint_decimal()
        .or_else(|| {
            if value.is_object() {
                value.get_property("__bigint_data").bigint_decimal()
            } else {
                None
            }
        })
        .ok_or_else(|| vm.current_context.error_type("Cannot convert to BigInt"))?;
    let bytes = match kind {
        TypedArrayElementKind::BigInt64 => decimal
            .parse::<i128>()
            .map(|value| (value as i64).to_ne_bytes().to_vec())
            .unwrap_or_else(|_| 0i64.to_ne_bytes().to_vec()),
        TypedArrayElementKind::BigUint64 => decimal
            .parse::<i128>()
            .map(|value| (value as u64).to_ne_bytes().to_vec())
            .unwrap_or_else(|_| 0u64.to_ne_bytes().to_vec()),
        _ => unreachable!(),
    };
    Ok(bytes)
}

fn to_length(vm: &mut VM, value: Value) -> Result<usize, crate::vm::error::RuntimeError> {
    let integer = to_integer_or_infinity(vm, value)?;
    if integer <= 0.0 {
        return Ok(0);
    }
    if integer.is_infinite() {
        return Ok(MAX_SAFE_INTEGER as usize);
    }
    Ok(integer.min(MAX_SAFE_INTEGER) as usize)
}

fn to_uint_n(value: f64, bits: u32) -> u64 {
    if !value.is_finite() || value == 0.0 {
        return 0;
    }
    let modulo = 2f64.powi(bits as i32);
    ((value.trunc() % modulo + modulo) % modulo) as u64
}

fn to_int_n(value: f64, bits: u32) -> i64 {
    let unsigned = to_uint_n(value, bits);
    let threshold = 1u64 << (bits - 1);
    if unsigned >= threshold {
        unsigned as i64 - (1i64 << bits)
    } else {
        unsigned as i64
    }
}

fn clamp_u8(value: f64) -> u8 {
    if value.is_nan() || value <= 0.0 {
        0
    } else if value >= 255.0 {
        255
    } else {
        let floor = value.floor();
        if value < floor + 0.5 {
            floor as u8
        } else if value > floor + 0.5 {
            (floor + 1.0) as u8
        } else if (floor as u8) % 2 == 0 {
            floor as u8
        } else {
            (floor + 1.0) as u8
        }
    }
}

fn ensure_allocatable(
    vm: &mut VM,
    byte_length: usize,
) -> Result<(), crate::vm::error::RuntimeError> {
    if byte_length > MAX_ARRAY_BUFFER_BYTES {
        return Err(vm.current_context.error_range("TypedArray allocation"));
    }
    Ok(())
}

fn builtin_getter(
    factory: &mut Factory,
    name: &str,
    func: crate::builtins::BuiltinFuncTy,
) -> Value {
    builtin_method(factory, name, func, 0.0)
}

fn builtin_method(
    factory: &mut Factory,
    name: &str,
    func: crate::builtins::BuiltinFuncTy,
    length: f64,
) -> Value {
    let function = Value::builtin_function_with_proto(
        &mut factory.memory_allocator,
        factory.object_prototypes.function,
        name,
        func,
    );
    set_function_length(function, length);
    function
}
