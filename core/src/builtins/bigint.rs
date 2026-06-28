use super::helpers::{define_well_known_symbol_property, set_function_length};
use crate::vm::{
    jsvalue::{
        object::{DataProperty, Property},
        symbol::{SYMBOL_TO_PRIMITIVE_ID, SYMBOL_TO_STRING_TAG_ID},
        value::*,
    },
    vm::{Factory, VMValueResult, VM},
};
use rustc_hash::FxHashMap;

pub fn bigint(factory: &mut Factory) -> Value {
    let prototype = factory.object(FxHashMap::default());
    let to_string = factory.builtin_function("toString", bigint_prototype_to_string);
    let to_locale_string =
        factory.builtin_function("toLocaleString", bigint_prototype_to_locale_string);
    let value_of = factory.builtin_function("valueOf", bigint_prototype_value_of);
    set_function_length(to_string, 0.0);
    set_function_length(to_locale_string, 0.0);
    set_function_length(value_of, 0.0);
    set_constructor_function(prototype, "toString", to_string);
    set_constructor_function(prototype, "toLocaleString", to_locale_string);
    set_constructor_function(prototype, "valueOf", value_of);
    let tag = factory.string("BigInt");
    define_well_known_symbol_property(
        factory,
        prototype,
        SYMBOL_TO_STRING_TAG_ID,
        Property::new_data(DataProperty::new(tag).set_configurable()),
    );
    factory.object_prototypes.bigint = prototype;

    let obj = factory.generate_builtin_constructor("BigInt", bigint_constructor, prototype);
    let as_int_n = factory.builtin_function("asIntN", bigint_as_int_n);
    let as_uint_n = factory.builtin_function("asUintN", bigint_as_uint_n);
    set_function_length(obj, 1.0);
    set_function_length(as_int_n, 2.0);
    set_function_length(as_uint_n, 2.0);
    set_constructor_function(obj, "asIntN", as_int_n);
    set_constructor_function(obj, "asUintN", as_uint_n);
    obj
}

fn set_constructor_function(constructor: Value, name: &str, value: Value) {
    constructor.get_object_info().property.insert(
        name.to_string(),
        Property::new_data(DataProperty::new(value).set_writable().set_configurable()),
    );
}

pub fn bigint_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    if vm.builtin_constructor_call {
        return Err(vm.current_context.error_type("BigInt is not a constructor"));
    }
    let value = args.get(0).copied().unwrap_or(Value::undefined());
    let decimal = to_bigint_decimal(vm, value, true)?;
    Ok(vm.factory.bigint(decimal))
}

pub fn bigint_prototype_to_string(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let decimal = this_bigint_decimal(vm, this)?;
    let radix = match args.get(0) {
        Some(radix) if !radix.is_undefined() => {
            let radix = radix.to_number(&mut vm.factory.memory_allocator).trunc();
            if !(2.0..=36.0).contains(&radix) {
                return Err(vm.current_context.error_range("BigInt.prototype.toString"));
            }
            radix as u32
        }
        _ => 10,
    };
    Ok(vm.factory.string(bigint_to_string_radix(&decimal, radix)))
}

pub fn bigint_prototype_value_of(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let decimal = this_bigint_decimal(vm, this)?;
    Ok(vm.factory.bigint(decimal))
}

pub fn bigint_prototype_to_locale_string(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    let decimal = this_bigint_decimal(vm, this)?;
    let formatted = super::intl::format_bigint_to_locale_string(vm, &decimal, args)?;
    Ok(vm.factory.string(formatted))
}

pub fn bigint_as_int_n(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let bits = to_index_bits(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let value = to_bigint_decimal(
        vm,
        args.get(1).copied().unwrap_or(Value::undefined()),
        false,
    )?;
    if bits == 0 {
        return Ok(vm.factory.bigint("0"));
    }
    let modulo = decimal_pow2(bits);
    let sign = decimal_pow2(bits - 1);
    let residue = signed_decimal_mod(&value, &modulo);
    let result = if decimal_cmp_abs(&residue, &sign) != std::cmp::Ordering::Less {
        let magnitude = decimal_sub_abs(&modulo, &residue);
        if magnitude == "0" {
            magnitude
        } else {
            format!("-{}", magnitude)
        }
    } else {
        residue
    };
    Ok(vm.factory.bigint(result))
}

pub fn bigint_as_uint_n(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let bits = to_index_bits(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let value = to_bigint_decimal(
        vm,
        args.get(1).copied().unwrap_or(Value::undefined()),
        false,
    )?;
    if bits == 0 {
        return Ok(vm.factory.bigint("0"));
    }
    let modulo = decimal_pow2(bits);
    Ok(vm.factory.bigint(signed_decimal_mod(&value, &modulo)))
}

fn this_bigint_decimal(vm: &mut VM, this: Value) -> Result<String, crate::vm::error::RuntimeError> {
    if let Some(decimal) = this.bigint_decimal() {
        return Ok(decimal);
    }
    if this.is_object() {
        let data = this.get_property("__bigint_data");
        if let Some(decimal) = data.bigint_decimal() {
            return Ok(decimal);
        }
    }
    Err(vm.current_context.error_type("BigInt.prototype"))
}

fn to_bigint_decimal(
    vm: &mut VM,
    value: Value,
    allow_number: bool,
) -> Result<String, crate::vm::error::RuntimeError> {
    if let Some(decimal) = value.bigint_decimal() {
        return Ok(decimal);
    }
    match value {
        Value::Bool(0) => Ok("0".to_string()),
        Value::Bool(1) => Ok("1".to_string()),
        Value::Number(number) if allow_number => {
            if !number.is_finite() || number.trunc() != number {
                Err(vm
                    .current_context
                    .error_range("Cannot convert Number to BigInt"))
            } else {
                Ok(format!("{:.0}", number))
            }
        }
        Value::Number(_) => Err(vm
            .current_context
            .error_type("Cannot convert Number to BigInt")),
        Value::String(s) => parse_bigint_string(cstrp_to_str(s)).ok_or_else(|| {
            vm.current_context
                .error_syntax("Cannot convert string to BigInt")
        }),
        Value::Object(_) => to_bigint_decimal_from_object(vm, value, allow_number),
        _ => Err(vm.current_context.error_type("Cannot convert to BigInt")),
    }
}

fn to_bigint_decimal_from_object(
    vm: &mut VM,
    value: Value,
    allow_number: bool,
) -> Result<String, crate::vm::error::RuntimeError> {
    let key = vm.factory.symbol_with_id(
        SYMBOL_TO_PRIMITIVE_ID,
        Some("Symbol.toPrimitive".to_string()),
    );
    let method = vm.get_property_by_value(value, key)?;
    if !method.is_undefined() {
        if !method.is_function_object() {
            return Err(vm.current_context.error_type("Symbol.toPrimitive"));
        }
        let hint = vm.factory.string("number");
        let primitive = vm.call_function(method, &[hint], value)?;
        if primitive.is_bigint() || !primitive.is_object() {
            return to_bigint_decimal(vm, primitive, allow_number);
        }
        return Err(vm
            .current_context
            .error_type("Cannot convert object to BigInt"));
    }

    for method_name in ["valueOf", "toString"] {
        let key = vm.factory.string(method_name);
        let method = vm.get_property_by_value(value, key)?;
        if method.is_function_object() {
            let primitive = vm.call_function(method, &[], value)?;
            if primitive.is_bigint() || !primitive.is_object() {
                return to_bigint_decimal(vm, primitive, allow_number);
            }
        }
    }
    Err(vm
        .current_context
        .error_type("Cannot convert object to BigInt"))
}

fn to_index_bits(vm: &mut VM, value: Value) -> Result<u64, crate::vm::error::RuntimeError> {
    if value.is_undefined() {
        return Ok(0);
    }
    if value.is_bigint() {
        return Err(vm.current_context.error_type("Invalid BigInt bits"));
    }
    let number = value.to_number(&mut vm.factory.memory_allocator);
    if number.is_nan() {
        return Ok(0);
    }
    let integer = number.trunc();
    if !integer.is_finite() || integer < 0.0 {
        return Err(vm.current_context.error_range("Invalid BigInt bits"));
    }
    if integer > 9_007_199_254_740_991.0 {
        return Err(vm.current_context.error_range("Invalid BigInt bits"));
    }
    Ok(integer as u64)
}

fn parse_bigint_string(source: &str) -> Option<String> {
    let source = source.trim();
    if source.is_empty() {
        return Some("0".to_string());
    }
    let (negative, body) = source
        .strip_prefix('-')
        .map(|body| (true, body))
        .or_else(|| source.strip_prefix('+').map(|body| (false, body)))
        .unwrap_or((false, source));
    if (negative || body.len() != source.len())
        && (body.starts_with("0x")
            || body.starts_with("0X")
            || body.starts_with("0b")
            || body.starts_with("0B")
            || body.starts_with("0o")
            || body.starts_with("0O"))
    {
        return None;
    }
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
    let mut decimal = "0".to_string();
    for digit in digits.chars().filter_map(|c| c.to_digit(radix)) {
        decimal = decimal_mul_add(decimal.as_str(), radix, digit);
    }
    let decimal = normalize_decimal(decimal);
    if negative && decimal != "0" {
        Some(format!("-{}", decimal))
    } else {
        Some(decimal)
    }
}

fn decimal_mul_add(decimal: &str, radix: u32, digit: u32) -> String {
    let mut carry = digit;
    let mut out = Vec::with_capacity(decimal.len() + 1);
    for byte in decimal.bytes().rev() {
        let value = (byte - b'0') as u32 * radix + carry;
        out.push((b'0' + (value % 10) as u8) as char);
        carry = value / 10;
    }
    while carry != 0 {
        out.push((b'0' + (carry % 10) as u8) as char);
        carry /= 10;
    }
    out.iter().rev().collect()
}

fn decimal_pow2(bits: u64) -> String {
    let mut decimal = "1".to_string();
    for _ in 0..bits {
        decimal = decimal_mul_add(&decimal, 2, 0);
    }
    decimal
}

fn decimal_abs(decimal: &str) -> &str {
    decimal.strip_prefix('-').unwrap_or(decimal)
}

fn decimal_cmp_abs(left: &str, right: &str) -> std::cmp::Ordering {
    let left = decimal_abs(left).trim_start_matches('0');
    let right = decimal_abs(right).trim_start_matches('0');
    let left = if left.is_empty() { "0" } else { left };
    let right = if right.is_empty() { "0" } else { right };
    left.len().cmp(&right.len()).then_with(|| left.cmp(right))
}

fn decimal_sub_abs(left: &str, right: &str) -> String {
    debug_assert!(decimal_cmp_abs(left, right) != std::cmp::Ordering::Less);
    let mut borrow = 0i32;
    let mut out = Vec::with_capacity(left.len());
    let mut right_digits = decimal_abs(right).bytes().rev();
    for l in decimal_abs(left).bytes().rev() {
        let r = right_digits.next().map(|b| (b - b'0') as i32).unwrap_or(0);
        let mut digit = (l - b'0') as i32 - borrow - r;
        if digit < 0 {
            digit += 10;
            borrow = 1;
        } else {
            borrow = 0;
        }
        out.push((b'0' + digit as u8) as char);
    }
    normalize_decimal(out.iter().rev().collect())
}

fn decimal_mod_abs(left: &str, modulus: &str) -> String {
    let mut remainder = "0".to_string();
    for digit in decimal_abs(left).bytes() {
        remainder = decimal_mul_add(&remainder, 10, (digit - b'0') as u32);
        while decimal_cmp_abs(&remainder, modulus) != std::cmp::Ordering::Less {
            remainder = decimal_sub_abs(&remainder, modulus);
        }
    }
    remainder
}

fn signed_decimal_mod(value: &str, modulus: &str) -> String {
    let residue = decimal_mod_abs(value, modulus);
    if value.starts_with('-') && residue != "0" {
        decimal_sub_abs(modulus, &residue)
    } else {
        residue
    }
}

fn normalize_decimal(decimal: String) -> String {
    let trimmed = decimal.trim_start_matches('0');
    if trimmed.is_empty() {
        "0".to_string()
    } else {
        trimmed.to_string()
    }
}

fn bigint_to_string_radix(decimal: &str, radix: u32) -> String {
    if radix == 10 {
        return decimal.to_string();
    }
    match decimal.parse::<i128>() {
        Ok(value) => format_i128_radix(value, radix),
        Err(_) => decimal.to_string(),
    }
}

fn format_i128_radix(value: i128, radix: u32) -> String {
    if value == 0 {
        return "0".to_string();
    }
    let negative = value < 0;
    let mut value = value.unsigned_abs();
    let mut out = Vec::new();
    while value != 0 {
        let digit = (value % radix as u128) as u8;
        out.push(match digit {
            0..=9 => (b'0' + digit) as char,
            _ => (b'a' + digit - 10) as char,
        });
        value /= radix as u128;
    }
    if negative {
        out.push('-');
    }
    out.iter().rev().collect()
}
