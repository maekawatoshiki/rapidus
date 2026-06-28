//! Shared helper functions for builtin implementations.
//!
//! These consolidate helpers that were previously duplicated across the
//! builtin modules (array, array_buffer, atomics, data_view, string,
//! typed_array, ...). New code should use these instead of redefining
//! private copies.

use crate::vm::{
    error::RuntimeError,
    factory::Factory,
    jsvalue::{
        object::{AccessorProperty, DataProperty, Property},
        symbol::{SYMBOL_SPECIES_ID, SYMBOL_TO_PRIMITIVE_ID},
        value::Value,
    },
    vm::{VMValueResult, VM},
};
use super::BuiltinFuncTy;

const MAX_SAFE_INTEGER: f64 = 9_007_199_254_740_991.0;

/// ToIndex (https://tc39.es/ecma262/#sec-toindex).
/// `undefined` is treated as 0. Throws a RangeError for negative values or
/// values above 2^53 - 1.
pub fn to_index(vm: &mut VM, value: Value) -> Result<usize, RuntimeError> {
    if value.is_undefined() {
        return Ok(0);
    }
    let integer = to_integer_or_infinity(vm, value)?;
    if integer < 0.0 || integer > MAX_SAFE_INTEGER {
        return Err(vm.current_context.error_range("Invalid index"));
    }
    Ok(integer as usize)
}

/// ToIntegerOrInfinity (https://tc39.es/ecma262/#sec-tointegerorinfinity).
/// Returns an f64 so that +Infinity / -Infinity are preserved (the isize
/// variants previously used in some modules saturated to isize::MAX/MIN).
pub fn to_integer_or_infinity(vm: &mut VM, value: Value) -> Result<f64, RuntimeError> {
    let number = to_number(vm, value)?;
    Ok(if number.is_nan() || number == 0.0 {
        0.0
    } else if number.is_infinite() {
        number
    } else {
        number.trunc()
    })
}

/// ToNumber (https://tc39.es/ecma262/#sec-tonumber).
/// Rejects Symbol and BigInt, honors Symbol.toPrimitive with hint "number",
/// then falls back to valueOf/toString (OrdinaryToPrimitive).
pub fn to_number(vm: &mut VM, value: Value) -> Result<f64, RuntimeError> {
    if value.is_symbol() || value.is_bigint() {
        return Err(vm.current_context.error_type("Cannot convert to Number"));
    }
    if !value.is_object() {
        return Ok(value.to_number(&mut vm.factory.memory_allocator));
    }
    if let Some(primitive) = call_to_primitive(vm, value, "number")? {
        if primitive.is_symbol() || primitive.is_bigint() {
            return Err(vm.current_context.error_type("Cannot convert to Number"));
        }
        return Ok(primitive.to_number(&mut vm.factory.memory_allocator));
    }
    for method_name in ["valueOf", "toString"] {
        let key = vm.factory.string(method_name);
        let method = vm.get_property_by_value(value, key)?;
        if method.is_function_object() {
            let primitive = vm.call_function(method, &[], value)?;
            if primitive.is_symbol() || primitive.is_bigint() {
                return Err(vm.current_context.error_type("Cannot convert to Number"));
            }
            if !primitive.is_object() {
                return Ok(primitive.to_number(&mut vm.factory.memory_allocator));
            }
        }
    }
    Err(vm
        .current_context
        .error_type("Cannot convert object to Number"))
}

/// Invokes `value[Symbol.toPrimitive](hint)` if present.
/// Returns Ok(None) when the method is null or undefined. Throws a TypeError
/// if the method is not callable or if it returns an object.
pub fn call_to_primitive(
    vm: &mut VM,
    value: Value,
    hint: &str,
) -> Result<Option<Value>, RuntimeError> {
    let key = vm.factory.well_known_symbol(SYMBOL_TO_PRIMITIVE_ID);
    let method = vm.get_property_by_value(value, key)?;
    if method.is_null() || method.is_undefined() {
        return Ok(None);
    }
    if !method.is_function_object() {
        return Err(vm.current_context.error_type("Symbol.toPrimitive"));
    }
    let hint = vm.factory.string(hint);
    let primitive = vm.call_function(method, &[hint], value)?;
    if primitive.is_object() && !primitive.is_symbol() && !primitive.is_bigint() {
        return Err(vm
            .current_context
            .error_type("Cannot convert object to primitive"));
    }
    Ok(Some(primitive))
}

/// Sets the non-writable, non-enumerable, configurable "length" property of
/// a (builtin) function object.
pub fn set_function_length(func: Value, length: f64) {
    func.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(length)).set_configurable()),
    );
}

/// Creates a builtin function on the realm's `%Function.prototype%` and sets
/// the standard non-writable, non-enumerable, configurable `length` property.
pub fn builtin_function_with_length(
    factory: &mut Factory,
    name: &str,
    func: BuiltinFuncTy,
    length: f64,
) -> Value {
    let function = factory.builtin_function(name, func);
    set_function_length(function, length);
    function
}

pub fn builtin_function_with_proto_and_length(
    factory: &mut Factory,
    function_prototype: Value,
    name: &str,
    func: BuiltinFuncTy,
    length: f64,
) -> Value {
    let function =
        Value::builtin_function_with_proto(&mut factory.memory_allocator, function_prototype, name, func);
    set_function_length(function, length);
    function
}

/// SameValueZero (https://tc39.es/ecma262/#sec-samevaluezero):
/// like strict equality but NaN equals NaN (+0 and -0 are equal).
pub fn same_value_zero(a: Value, b: Value) -> bool {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) if x.is_nan() && y.is_nan() => true,
        _ => a.strict_eq_bool(b),
    }
}

/// Clamps a relative index (already passed through ToIntegerOrInfinity) into
/// [0, len], resolving negative values from the end of the array.
pub fn relative_to_index(relative: f64, len: usize) -> usize {
    if relative == f64::NEG_INFINITY {
        0
    } else if relative < 0.0 {
        (len as f64 + relative).max(0.0) as usize
    } else if relative == f64::INFINITY {
        len
    } else {
        relative.min(len as f64) as usize
    }
}

/// ToObject (https://tc39.es/ecma262/#sec-toobject).
/// Throws a TypeError for null/undefined; wraps other primitives with the
/// corresponding constructor.
pub fn to_object(vm: &mut VM, val: Value) -> VMValueResult {
    if val.is_null() || val.is_undefined() {
        return Err(vm.current_context.error_type("Cannot convert to object"));
    }
    if let Some(obj) = primitive_wrapper_object(vm, val) {
        return Ok(obj);
    }
    if val.is_object() {
        return Ok(val);
    }

    Err(vm.current_context.error_type("Cannot convert to object"))
}

/// Creates the ordinary wrapper object used by ToObject/Object(...) for
/// primitive values. This VM represents Symbol and BigInt primitives as
/// ObjectKind::Symbol/BigInt values, so they must be handled before the
/// generic object branch.
pub fn primitive_wrapper_object(vm: &mut VM, val: Value) -> Option<Value> {
    let (prototype, slot) = if val.is_string() {
        (vm.factory.object_prototypes.string, "__string_data")
    } else if val.is_number() {
        (vm.factory.object_prototypes.number, "__number_data")
    } else if matches!(val, Value::Bool(_)) {
        (vm.factory.object_prototypes.boolean, "__boolean_data")
    } else if val.is_symbol() {
        (vm.factory.object_prototypes.symbol, "__symbol_data")
    } else if val.is_bigint() {
        (vm.factory.object_prototypes.bigint, "__bigint_data")
    } else {
        return None;
    };

    let obj = vm.factory.object(rustc_hash::FxHashMap::default());
    let mut info = obj.get_object_info();
    info.prototype = prototype;
    info.property
        .insert(slot.to_string(), Property::new_data(DataProperty::new(val)));
    Some(obj)
}

/// Defines a symbol-keyed property on `obj`, keeping `sym_property` (keyed
/// by symbol id) and `sym_property_order` (insertion-ordered symbol Values,
/// iterated by Object.getOwnPropertySymbols) in sync. Inserting into
/// `sym_property` alone makes the property invisible to enumeration; this
/// helper also avoids pushing a duplicate order entry when the symbol is
/// already present.
pub fn define_symbol_property(obj: Value, symbol: Value, prop: Property) {
    let id = symbol.get_symbol_info().id;
    let mut info = obj.get_object_info();
    if !info.sym_property.contains_key(&id) {
        info.sym_property_order.push(symbol);
    }
    info.sym_property.insert(id, prop);
}

/// Like [`define_symbol_property`], for well-known symbols referenced by id
/// (SYMBOL_SPECIES_ID, SYMBOL_TO_STRING_TAG_ID, ...). Uses the canonical
/// symbol instance from the factory so the enumerated symbol is
/// strict-equal to e.g. the global Symbol.species.
pub fn define_well_known_symbol_property(
    factory: &mut Factory,
    obj: Value,
    id: usize,
    prop: Property,
) {
    let symbol = factory.well_known_symbol(id);
    define_symbol_property(obj, symbol, prop);
}

/// The default `get [Symbol.species]` accessor body: returns `this`.
pub fn species_getter(_vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    Ok(this)
}

/// Installs the default `@@species` accessor (a getter returning `this`) on
/// a builtin constructor.
pub fn define_species_getter(factory: &mut Factory, constructor: Value) {
    let getter = factory.builtin_function("get [Symbol.species]", species_getter);
    set_function_length(getter, 0.0);
    define_well_known_symbol_property(
        factory,
        constructor,
        SYMBOL_SPECIES_ID,
        Property::Accessor(AccessorProperty {
            get: getter,
            set: Value::undefined(),
            enumerable: false,
            configurable: true,
        }),
    );
}
