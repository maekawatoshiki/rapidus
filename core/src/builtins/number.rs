use super::helpers::set_function_length;
use crate::vm::{
    jsvalue::{
        object::{DataProperty, Property},
        value::*,
    },
    vm::{Factory, VMValueResult, VM},
};
use rustc_hash::FxHashMap;

pub fn number(factory: &mut Factory) -> Value {
    let prototype = factory.object(FxHashMap::default());
    prototype.get_object_info().property.insert(
        "__number_data".to_string(),
        Property::new_data(DataProperty::new(Value::Number(0.0))),
    );
    let to_string = factory.builtin_function("toString", number_prototype_to_string);
    let value_of = factory.builtin_function("valueOf", number_prototype_value_of);
    let to_fixed = factory.builtin_function("toFixed", number_prototype_to_fixed);
    let to_exponential = factory.builtin_function("toExponential", number_prototype_to_exponential);
    let to_precision = factory.builtin_function("toPrecision", number_prototype_to_precision);
    let to_locale_string =
        factory.builtin_function("toLocaleString", number_prototype_to_locale_string);
    set_function_length(to_string, 1.0);
    set_function_length(value_of, 0.0);
    set_function_length(to_fixed, 1.0);
    set_function_length(to_exponential, 1.0);
    set_function_length(to_precision, 1.0);
    set_function_length(to_locale_string, 0.0);
    set_constructor_function(prototype, "toString", to_string);
    set_constructor_function(prototype, "valueOf", value_of);
    set_constructor_function(prototype, "toFixed", to_fixed);
    set_constructor_function(prototype, "toExponential", to_exponential);
    set_constructor_function(prototype, "toPrecision", to_precision);
    set_constructor_function(prototype, "toLocaleString", to_locale_string);
    factory.object_prototypes.number = prototype;
    let obj = factory.generate_builtin_constructor("Number", number_constructor, prototype);
    let is_finite = factory.builtin_function("isFinite", number_is_finite);
    let is_integer = factory.builtin_function("isInteger", number_is_integer);
    let is_nan = factory.builtin_function("isNaN", number_is_nan);
    let is_safe_integer = factory.builtin_function("isSafeInteger", number_is_safe_integer);
    set_function_length(is_finite, 1.0);
    set_function_length(is_integer, 1.0);
    set_function_length(is_nan, 1.0);
    set_function_length(is_safe_integer, 1.0);
    set_constructor_function(obj, "isFinite", is_finite);
    set_constructor_function(obj, "isInteger", is_integer);
    set_constructor_function(obj, "isNaN", is_nan);
    set_constructor_function(obj, "isSafeInteger", is_safe_integer);
    set_constructor_constant(obj, "NaN", Value::Number(::std::f64::NAN));
    set_constructor_constant(
        obj,
        "POSITIVE_INFINITY",
        Value::Number(::std::f64::INFINITY),
    );
    set_constructor_constant(
        obj,
        "NEGATIVE_INFINITY",
        Value::Number(::std::f64::NEG_INFINITY),
    );
    set_constructor_constant(obj, "MAX_VALUE", Value::Number(::std::f64::MAX));
    set_constructor_constant(obj, "MIN_VALUE", Value::Number(::std::f64::MIN_POSITIVE));
    set_constructor_constant(
        obj,
        "MAX_SAFE_INTEGER",
        Value::Number(9_007_199_254_740_991.0),
    );
    set_constructor_constant(
        obj,
        "MIN_SAFE_INTEGER",
        Value::Number(-9_007_199_254_740_991.0),
    );
    set_constructor_constant(obj, "EPSILON", Value::Number(::std::f64::EPSILON));
    set_function_length(obj, 1.0);
    obj
}

fn set_constructor_function(constructor: Value, name: &str, value: Value) {
    constructor.get_object_info().property.insert(
        name.to_string(),
        Property::new_data(DataProperty::new(value).set_writable().set_configurable()),
    );
}

fn set_constructor_constant(constructor: Value, name: &str, value: Value) {
    constructor.get_object_info().property.insert(
        name.to_string(),
        Property::new_data(DataProperty::new(value)),
    );
}

pub fn number_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let num = args
        .get(0)
        .map(|value| value.to_number(&mut vm.factory.memory_allocator))
        .unwrap_or(0.0);
    let value = Value::Number(num);
    if this.is_object() {
        this.get_object_info().property.insert(
            "__number_data".to_string(),
            Property::new_data(DataProperty::new(value)),
        );
    }
    Ok(value)
}

pub fn number_prototype_to_string(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = number_this_value(vm, this)?;
    let radix = match args.get(0) {
        Some(radix) if !radix.is_undefined() => {
            let radix = radix.to_number(&mut vm.factory.memory_allocator).trunc();
            if !(2.0..=36.0).contains(&radix) {
                return Err(vm.current_context.error_range("Number.prototype.toString"));
            }
            radix as u32
        }
        _ => 10,
    };
    Ok(vm.factory.string(number_to_string_radix(value, radix)))
}

pub fn number_prototype_value_of(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    Ok(Value::Number(number_this_value(vm, this)?))
}

pub fn number_prototype_to_fixed(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = number_this_value(vm, this)?;
    let digits = number_format_digits(vm, args, 0, 0, 100, 0)?;
    let result = if value.is_nan() || value.is_infinite() {
        Value::Number(value).to_string()
    } else if value.abs() >= 1e21 {
        Value::Number(value).to_string()
    } else {
        format!("{:.*}", digits, value)
    };
    Ok(vm.factory.string(result))
}

pub fn number_prototype_to_exponential(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = number_this_value(vm, this)?;
    if value.is_nan() || value.is_infinite() {
        return Ok(vm.factory.string(Value::Number(value).to_string()));
    }
    let result = if args
        .get(0)
        .map(|value| value.is_undefined())
        .unwrap_or(true)
    {
        normalize_exponent(format!("{:e}", value))
    } else {
        let digits = number_format_digits(vm, args, 0, 0, 100, 0)?;
        normalize_exponent(format!("{:.*e}", digits, value))
    };
    Ok(vm.factory.string(result))
}

pub fn number_prototype_to_precision(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = number_this_value(vm, this)?;
    if args
        .get(0)
        .map(|value| value.is_undefined())
        .unwrap_or(true)
    {
        return Ok(vm.factory.string(value.to_string()));
    }
    if value.is_nan() || value.is_infinite() {
        return Ok(vm.factory.string(Value::Number(value).to_string()));
    }
    let precision = number_format_digits(vm, args, 0, 1, 100, 1)?;
    Ok(vm.factory.string(format!("{:.*}", precision, value)))
}

pub fn number_prototype_to_locale_string(
    vm: &mut VM,
    _args: &[Value],
    this: Value,
) -> VMValueResult {
    let value = number_this_value(vm, this)?;
    Ok(vm.factory.string(value.to_string()))
}

fn number_format_digits(
    vm: &mut VM,
    args: &[Value],
    index: usize,
    min: usize,
    max: usize,
    default: usize,
) -> Result<usize, crate::vm::error::RuntimeError> {
    let Some(value) = args.get(index) else {
        return Ok(default);
    };
    if value.is_undefined() {
        return Ok(default);
    }
    let number = value.to_number(&mut vm.factory.memory_allocator).trunc();
    let number = if number.is_nan() { 0.0 } else { number };
    if !number.is_finite() || number < min as f64 || number > max as f64 {
        return Err(vm.current_context.error_range("Number.prototype"));
    }
    Ok(number as usize)
}

fn normalize_exponent(mut value: String) -> String {
    if let Some(index) = value.find('e') {
        let next = value.as_bytes().get(index + 1).copied();
        if !matches!(next, Some(b'+') | Some(b'-')) {
            value.insert(index + 1, '+');
        }
    }
    value
}

fn number_to_string_radix(value: f64, radix: u32) -> String {
    if radix == 10 || !value.is_finite() || value.fract() != 0.0 {
        return Value::Number(value).to_string();
    }

    let negative = value.is_sign_negative();
    let mut integer = value.abs() as u128;
    if integer == 0 {
        return "0".to_string();
    }
    let mut digits = Vec::new();
    while integer > 0 {
        let digit = (integer % radix as u128) as u8;
        digits.push(if digit < 10 {
            (b'0' + digit) as char
        } else {
            (b'a' + digit - 10) as char
        });
        integer /= radix as u128;
    }
    if negative {
        digits.push('-');
    }
    digits.iter().rev().collect()
}

fn number_this_value(vm: &mut VM, this: Value) -> Result<f64, crate::vm::error::RuntimeError> {
    if this.is_number() {
        return Ok(this.into_number());
    }
    if this.is_object() {
        let value = this.get_property("__number_data");
        if value.is_number() {
            return Ok(value.into_number());
        }
    }
    Err(vm.current_context.error_type("Number.prototype"))
}

pub fn number_is_finite(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    Ok(Value::bool(
        matches!(args.get(0), Some(Value::Number(value)) if value.is_finite()),
    ))
}

pub fn number_is_integer(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    Ok(Value::bool(
        matches!(args.get(0), Some(Value::Number(value)) if value.is_finite() && value.trunc() == *value),
    ))
}

pub fn number_is_nan(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    Ok(Value::bool(
        matches!(args.get(0), Some(Value::Number(value)) if value.is_nan()),
    ))
}

pub fn number_is_safe_integer(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    const MAX_SAFE_INTEGER: f64 = 9_007_199_254_740_991.0;
    Ok(Value::bool(
        matches!(args.get(0), Some(Value::Number(value)) if value.is_finite() && value.trunc() == *value && value.abs() <= MAX_SAFE_INTEGER),
    ))
}
