use super::helpers::{
    define_well_known_symbol_property, set_function_length, to_index, to_integer_or_infinity,
    to_number,
};
use crate::builtins::{typed_array, BuiltinFuncTy};
use crate::vm::{
    error::RuntimeError,
    jsvalue::{symbol::SYMBOL_TO_STRING_TAG_ID, value::*},
    vm::{Factory, VMValueResult, VM},
};

#[derive(Clone, Copy)]
enum AtomicOp {
    Add,
    And,
    Or,
    Sub,
    Xor,
}

pub fn atomics(factory: &mut Factory) -> Value {
    let obj = factory.object(FxHashMap::default());
    for (name, length, func) in [
        ("add", 3.0, atomics_add as BuiltinFuncTy),
        ("and", 3.0, atomics_and),
        ("compareExchange", 4.0, atomics_compare_exchange),
        ("exchange", 3.0, atomics_exchange),
        ("isLockFree", 1.0, atomics_is_lock_free),
        ("load", 2.0, atomics_load),
        ("notify", 3.0, atomics_notify),
        ("or", 3.0, atomics_or),
        ("store", 3.0, atomics_store),
        ("sub", 3.0, atomics_sub),
        ("wait", 4.0, atomics_wait),
        ("waitAsync", 4.0, atomics_wait_async),
        ("xor", 3.0, atomics_xor),
    ] {
        let func = factory.builtin_function(name, func);
        set_function_length(func, length);
        obj.get_object_info().insert_property(
            name.to_string(),
            Property::new_data(DataProperty::new(func).set_writable().set_configurable()),
        );
    }
    let tag = factory.string("Atomics");
    define_well_known_symbol_property(
        factory,
        obj,
        SYMBOL_TO_STRING_TAG_ID,
        Property::new_data(DataProperty::new(tag).set_configurable()),
    );
    obj
}

pub fn atomics_add(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    atomic_read_modify_write(vm, args, AtomicOp::Add)
}

pub fn atomics_and(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    atomic_read_modify_write(vm, args, AtomicOp::And)
}

pub fn atomics_or(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    atomic_read_modify_write(vm, args, AtomicOp::Or)
}

pub fn atomics_sub(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    atomic_read_modify_write(vm, args, AtomicOp::Sub)
}

pub fn atomics_xor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    atomic_read_modify_write(vm, args, AtomicOp::Xor)
}

pub fn atomics_exchange(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = args.get(0).copied().unwrap_or(Value::undefined());
    let info = validate_integer_typed_array(vm, target, false, false)?;
    let index = validate_index(
        vm,
        &info,
        args.get(1).copied().unwrap_or(Value::undefined()),
    )?;
    let replacement = atomic_value(
        vm,
        info.kind,
        args.get(2).copied().unwrap_or(Value::undefined()),
    )?;
    let old = read_value(vm, target, index)?;
    typed_array::write_typed_array_index(vm, target, index, replacement)?;
    Ok(old)
}

pub fn atomics_compare_exchange(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = args.get(0).copied().unwrap_or(Value::undefined());
    let info = validate_integer_typed_array(vm, target, false, false)?;
    let index = validate_index(
        vm,
        &info,
        args.get(1).copied().unwrap_or(Value::undefined()),
    )?;
    let expected = canonical_atomic_value(
        vm,
        info.kind,
        args.get(2).copied().unwrap_or(Value::undefined()),
    )?;
    let replacement = atomic_value(
        vm,
        info.kind,
        args.get(3).copied().unwrap_or(Value::undefined()),
    )?;
    let old = read_value(vm, target, index)?;
    if numeric_same_value(old, expected) {
        typed_array::write_typed_array_index(vm, target, index, replacement)?;
    }
    Ok(old)
}

pub fn atomics_load(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = args.get(0).copied().unwrap_or(Value::undefined());
    let info = validate_integer_typed_array(vm, target, false, false)?;
    let index = validate_index(
        vm,
        &info,
        args.get(1).copied().unwrap_or(Value::undefined()),
    )?;
    read_value(vm, target, index)
}

pub fn atomics_store(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = args.get(0).copied().unwrap_or(Value::undefined());
    let info = validate_integer_typed_array(vm, target, false, false)?;
    let index = validate_index(
        vm,
        &info,
        args.get(1).copied().unwrap_or(Value::undefined()),
    )?;
    let value = atomic_value(
        vm,
        info.kind,
        args.get(2).copied().unwrap_or(Value::undefined()),
    )?;
    typed_array::write_typed_array_index(vm, target, index, value)?;
    Ok(value)
}

pub fn atomics_is_lock_free(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let size = to_integer_or_infinity(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::bool(matches!(size as i64, 1 | 2 | 4 | 8)))
}

pub fn atomics_notify(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = args.get(0).copied().unwrap_or(Value::undefined());
    let info = validate_integer_typed_array(vm, target, true, false)?;
    validate_index(
        vm,
        &info,
        args.get(1).copied().unwrap_or(Value::undefined()),
    )?;
    let count = args.get(2).copied().unwrap_or(Value::undefined());
    if !count.is_undefined() {
        to_integer_or_infinity(vm, count)?;
    }
    Ok(Value::Number(0.0))
}

pub fn atomics_wait(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = args.get(0).copied().unwrap_or(Value::undefined());
    let info = validate_integer_typed_array(vm, target, true, true)?;
    let index = validate_index(
        vm,
        &info,
        args.get(1).copied().unwrap_or(Value::undefined()),
    )?;
    let expected = atomic_value(
        vm,
        info.kind,
        args.get(2).copied().unwrap_or(Value::undefined()),
    )?;
    let current = read_value(vm, target, index)?;
    let timeout = args.get(3).copied().unwrap_or(Value::undefined());
    let _timeout = if timeout.is_undefined() {
        f64::INFINITY
    } else {
        to_number(vm, timeout)?.max(0.0)
    };
    let result = if !numeric_same_value(current, expected) {
        "not-equal"
    } else {
        "timed-out"
    };
    Ok(vm.factory.string(result))
}

pub fn atomics_wait_async(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let value = atomics_wait(vm, args, Value::undefined())?;
    let mut props = FxHashMap::default();
    props.insert(
        "async".to_string(),
        Property::new_data(
            DataProperty::new(Value::bool(false))
                .set_writable()
                .set_configurable(),
        ),
    );
    props.insert(
        "value".to_string(),
        Property::new_data(DataProperty::new(value).set_writable().set_configurable()),
    );
    Ok(vm.factory.object(props))
}

fn atomic_read_modify_write(vm: &mut VM, args: &[Value], op: AtomicOp) -> VMValueResult {
    let target = args.get(0).copied().unwrap_or(Value::undefined());
    let info = validate_integer_typed_array(vm, target, false, false)?;
    let index = validate_index(
        vm,
        &info,
        args.get(1).copied().unwrap_or(Value::undefined()),
    )?;
    let value = args.get(2).copied().unwrap_or(Value::undefined());
    let old = read_value(vm, target, index)?;
    let new_value = match info.kind {
        typed_array::TypedArrayElementKind::BigInt64
        | typed_array::TypedArrayElementKind::BigUint64 => {
            let lhs = bigint_i128(vm, old)?;
            let rhs = bigint_i128(vm, value)?;
            vm.factory.bigint(apply_bigint_op(lhs, rhs, op).to_string())
        }
        _ => {
            let lhs = old.to_number(&mut vm.factory.memory_allocator);
            let rhs = to_integer_or_infinity(vm, value)?;
            Value::Number(apply_number_op(lhs, rhs, op))
        }
    };
    typed_array::write_typed_array_index(vm, target, index, new_value)?;
    Ok(old)
}

fn validate_integer_typed_array(
    vm: &mut VM,
    value: Value,
    waitable: bool,
    shared_required: bool,
) -> Result<TypedArrayObjectInfo, RuntimeError> {
    if !value.is_object() {
        return Err(vm.current_context.error_type("Atomics typedArray"));
    }
    let info = match value.get_object_info().kind {
        ObjectKind::TypedArray(ref info) => info.clone(),
        _ => return Err(vm.current_context.error_type("Atomics typedArray")),
    };
    if !is_integer_kind(info.kind) || (waitable && !is_waitable_kind(info.kind)) {
        return Err(vm.current_context.error_type("Atomics typedArray type"));
    }
    if array_buffer_detached(info.buffer) {
        return Err(vm.current_context.error_type("ArrayBuffer is detached"));
    }
    if shared_required && !array_buffer_shared(info.buffer) {
        return Err(vm.current_context.error_type("SharedArrayBuffer required"));
    }
    Ok(info)
}

fn validate_index(
    vm: &mut VM,
    info: &TypedArrayObjectInfo,
    index: Value,
) -> Result<usize, RuntimeError> {
    let index = to_index(vm, index)?;
    if index >= info.length {
        Err(vm.current_context.error_range("Atomics index"))
    } else {
        Ok(index)
    }
}

fn atomic_value(
    vm: &mut VM,
    kind: typed_array::TypedArrayElementKind,
    value: Value,
) -> Result<Value, RuntimeError> {
    match kind {
        typed_array::TypedArrayElementKind::BigInt64
        | typed_array::TypedArrayElementKind::BigUint64 => {
            bigint_i128(vm, value).map(|value| vm.factory.bigint(value.to_string()))
        }
        _ => to_integer_or_infinity(vm, value).map(Value::Number),
    }
}

fn canonical_atomic_value(
    vm: &mut VM,
    kind: typed_array::TypedArrayElementKind,
    value: Value,
) -> Result<Value, RuntimeError> {
    match kind {
        typed_array::TypedArrayElementKind::Int8 => Ok(Value::Number(to_int_n(
            to_integer_or_infinity(vm, value)?,
            8,
        ) as f64)),
        typed_array::TypedArrayElementKind::Uint8 => Ok(Value::Number(to_uint_n(
            to_integer_or_infinity(vm, value)?,
            8,
        ) as f64)),
        typed_array::TypedArrayElementKind::Int16 => Ok(Value::Number(to_int_n(
            to_integer_or_infinity(vm, value)?,
            16,
        ) as f64)),
        typed_array::TypedArrayElementKind::Uint16 => Ok(Value::Number(to_uint_n(
            to_integer_or_infinity(vm, value)?,
            16,
        ) as f64)),
        typed_array::TypedArrayElementKind::Int32 => Ok(Value::Number(to_int_n(
            to_integer_or_infinity(vm, value)?,
            32,
        ) as f64)),
        typed_array::TypedArrayElementKind::Uint32 => Ok(Value::Number(to_uint_n(
            to_integer_or_infinity(vm, value)?,
            32,
        ) as f64)),
        typed_array::TypedArrayElementKind::BigInt64 => {
            let value = bigint_i128(vm, value)? as i64;
            Ok(vm.factory.bigint(value.to_string()))
        }
        typed_array::TypedArrayElementKind::BigUint64 => {
            let value = bigint_i128(vm, value)? as u64;
            Ok(vm.factory.bigint(value.to_string()))
        }
        _ => Err(vm.current_context.error_type("Atomics typedArray type")),
    }
}

fn read_value(vm: &mut VM, target: Value, index: usize) -> VMValueResult {
    typed_array::typed_array_get_index(&mut vm.factory, target, index)
        .ok_or_else(|| vm.current_context.error_range("Atomics index"))
}

fn is_integer_kind(kind: typed_array::TypedArrayElementKind) -> bool {
    matches!(
        kind,
        typed_array::TypedArrayElementKind::Int8
            | typed_array::TypedArrayElementKind::Uint8
            | typed_array::TypedArrayElementKind::Int16
            | typed_array::TypedArrayElementKind::Uint16
            | typed_array::TypedArrayElementKind::Int32
            | typed_array::TypedArrayElementKind::Uint32
            | typed_array::TypedArrayElementKind::BigInt64
            | typed_array::TypedArrayElementKind::BigUint64
    )
}

fn is_waitable_kind(kind: typed_array::TypedArrayElementKind) -> bool {
    matches!(
        kind,
        typed_array::TypedArrayElementKind::Int32 | typed_array::TypedArrayElementKind::BigInt64
    )
}

fn array_buffer_detached(value: Value) -> bool {
    matches!(
        value.get_object_info().kind,
        ObjectKind::ArrayBuffer(ref info) if info.detached
    )
}

fn array_buffer_shared(value: Value) -> bool {
    matches!(
        value.get_object_info().kind,
        ObjectKind::ArrayBuffer(ref info) if info.shared
    )
}

fn numeric_same_value(lhs: Value, rhs: Value) -> bool {
    if lhs.is_bigint() || rhs.is_bigint() {
        lhs.bigint_decimal() == rhs.bigint_decimal()
    } else {
        lhs.to_number(&mut crate::gc::MemoryAllocator::new())
            == rhs.to_number(&mut crate::gc::MemoryAllocator::new())
    }
}

fn apply_number_op(lhs: f64, rhs: f64, op: AtomicOp) -> f64 {
    match op {
        AtomicOp::Add => lhs + rhs,
        AtomicOp::And => ((lhs as i64) & (rhs as i64)) as f64,
        AtomicOp::Or => ((lhs as i64) | (rhs as i64)) as f64,
        AtomicOp::Sub => lhs - rhs,
        AtomicOp::Xor => ((lhs as i64) ^ (rhs as i64)) as f64,
    }
}

fn apply_bigint_op(lhs: i128, rhs: i128, op: AtomicOp) -> i128 {
    match op {
        AtomicOp::Add => lhs.wrapping_add(rhs),
        AtomicOp::And => lhs & rhs,
        AtomicOp::Or => lhs | rhs,
        AtomicOp::Sub => lhs.wrapping_sub(rhs),
        AtomicOp::Xor => lhs ^ rhs,
    }
}

fn bigint_i128(vm: &mut VM, value: Value) -> Result<i128, RuntimeError> {
    if let Some(decimal) = value.bigint_decimal().or_else(|| {
        if value.is_object() {
            value.get_property("__bigint_data").bigint_decimal()
        } else {
            None
        }
    }) {
        return decimal
            .parse::<i128>()
            .map_err(|_| vm.current_context.error_range("BigInt range"));
    }
    match value {
        Value::Bool(0) => return Ok(0),
        Value::Bool(1) => return Ok(1),
        Value::String(s) => {
            return parse_bigint_i128(cstrp_to_str(s)).ok_or_else(|| {
                vm.current_context
                    .error_syntax("Cannot convert string to BigInt")
            });
        }
        Value::Number(_) => {
            return Err(vm
                .current_context
                .error_type("Cannot convert Number to BigInt"))
        }
        _ => {}
    }
    if value.is_object() {
        if let Some(primitive) = super::helpers::call_to_primitive(vm, value, "number")? {
            return bigint_i128(vm, primitive);
        }
        for method_name in ["valueOf", "toString"] {
            let key = vm.factory.string(method_name);
            let method = vm.get_property_by_value(value, key)?;
            if method.is_function_object() {
                let primitive = vm.call_function(method, &[], value)?;
                if primitive.is_bigint() || !primitive.is_object() {
                    return bigint_i128(vm, primitive);
                }
            }
        }
    }
    Err(vm.current_context.error_type("Cannot convert to BigInt"))
}

fn parse_bigint_i128(source: &str) -> Option<i128> {
    let source = source.trim();
    if source.is_empty() {
        return Some(0);
    }
    let (negative, body) = source
        .strip_prefix('-')
        .map(|body| (true, body))
        .or_else(|| source.strip_prefix('+').map(|body| (false, body)))
        .unwrap_or((false, source));
    let (radix, digits) = body
        .strip_prefix("0x")
        .or_else(|| body.strip_prefix("0X"))
        .map(|digits| (16, digits))
        .or_else(|| {
            body.strip_prefix("0b")
                .or_else(|| body.strip_prefix("0B"))
                .map(|digits| (2, digits))
        })
        .or_else(|| {
            body.strip_prefix("0o")
                .or_else(|| body.strip_prefix("0O"))
                .map(|digits| (8, digits))
        })
        .unwrap_or((10, body));
    if digits.is_empty() || !digits.chars().all(|c| c.to_digit(radix).is_some()) {
        return None;
    }
    i128::from_str_radix(digits, radix)
        .ok()
        .map(|value| if negative { -value } else { value })
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
