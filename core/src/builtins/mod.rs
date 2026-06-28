pub mod array;
pub mod array_buffer;
pub mod atomics;
pub mod bigint;
pub mod boolean;
pub mod collection;
pub mod console;
pub mod data_view;
pub mod date;
pub mod dynamic_import;
pub mod error;
pub mod finalization_registry;
pub mod function;
pub mod generator;
pub mod helpers;
pub mod intl;
pub mod iterator;
pub mod json;
pub mod math;
pub mod number;
pub mod object;
pub mod promise;
pub mod proxy;
pub mod reflect;
pub mod regexp;
pub mod shadow_realm;
pub mod string;
pub mod symbol;
pub mod temporal;
mod temporal_spec;
pub mod test262;
pub mod typed_array;
pub mod weak_ref;

pub use dynamic_import::dynamic_import;
pub use regexp::regexp;
pub use test262::{
    test262_create_realm, test262_detach_array_buffer, test262_drain_promise_jobs,
    test262_eval_script, test262_gc,
};

use crate::vm::{
    jsvalue::{
        object::{ObjectKind, Property},
        value::*,
    },
    vm::{CallMode, ThisMode, VMValueResult, VM},
};

pub type BuiltinFuncTy = fn(&mut VM, &[Value], Value) -> VMValueResult;

pub fn class_heritage_setup(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let class = args.get(0).copied().unwrap_or(Value::undefined());
    let superclass = args.get(1).copied().unwrap_or(Value::undefined());
    if !class.is_object() {
        return Err(vm.current_context.error_type("class heritage"));
    }

    let class_prototype_key = vm.factory.string("prototype".to_string());
    let class_prototype = vm.get_property_by_value(class, class_prototype_key)?;
    if !class_prototype.is_object() {
        return Err(vm.current_context.error_type("class prototype"));
    }

    if superclass.is_null() {
        object::object_set_prototype_of(vm, &[class_prototype, Value::null()], Value::undefined())?;
        if let ObjectKind::Function(ref mut info) = class.get_object_info().kind {
            info.super_constructor = Some(superclass);
        }
        return Ok(Value::undefined());
    }

    if !vm.is_constructor(superclass) {
        return Err(vm
            .current_context
            .error_type("Class extends value is not a constructor"));
    }

    let superclass_prototype_key = vm.factory.string("prototype".to_string());
    let superclass_prototype = vm.get_property_by_value(superclass, superclass_prototype_key)?;
    if !superclass_prototype.is_object() && !superclass_prototype.is_null() {
        return Err(vm
            .current_context
            .error_type("Class extends value has invalid prototype"));
    }

    object::object_set_prototype_of(
        vm,
        &[class_prototype, superclass_prototype],
        Value::undefined(),
    )?;
    object::object_set_prototype_of(vm, &[class, superclass], Value::undefined())?;
    if let ObjectKind::Function(ref mut info) = class.get_object_info().kind {
        info.super_constructor = Some(superclass);
    }
    Ok(Value::undefined())
}

pub fn super_construct(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    vm.super_construct(args)
}

pub fn super_construct_arguments(vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    let arguments = vm.current_context.lex_env().get_value("arguments")?;
    let length_key = vm.factory.string("length".to_string());
    let length = vm.get_property_by_value(arguments, length_key)?;
    let length = if length.is_number() {
        length.into_number().max(0.0) as usize
    } else {
        0
    };

    let mut args = Vec::with_capacity(length);
    for index in 0..length {
        let key = Value::Number(index as f64);
        args.push(vm.get_property_by_value(arguments, key)?);
    }
    vm.super_construct(&args)
}

pub fn throw_type_error(vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    let error = vm.factory.native_error("TypeError", "ThrowTypeError");
    Err(vm.current_context.error_exception(error))
}




pub fn parse_float(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let string = vm.to_string(*args.get(0).unwrap_or(&Value::undefined()))?;
    Ok(Value::Number(parse_float_prefix(&string)))
}

fn parse_float_prefix(input: &str) -> f64 {
    let trimmed = input.trim_start();
    let bytes = trimmed.as_bytes();
    let mut index = 0;

    if matches!(bytes.get(index), Some(b'+') | Some(b'-')) {
        index += 1;
    }

    if trimmed[index..].starts_with("Infinity") {
        let sign = if bytes.first() == Some(&b'-') {
            -1.0
        } else {
            1.0
        };
        return sign * f64::INFINITY;
    }

    let start = index;
    while matches!(bytes.get(index), Some(b'0'..=b'9')) {
        index += 1;
    }
    let digits_before_dot = index > start;

    if bytes.get(index) == Some(&b'.') {
        index += 1;
        while matches!(bytes.get(index), Some(b'0'..=b'9')) {
            index += 1;
        }
    }
    let has_digits = digits_before_dot || index > start + usize::from(!digits_before_dot);
    if !has_digits {
        return f64::NAN;
    }

    if matches!(bytes.get(index), Some(b'e') | Some(b'E')) {
        let exponent_start = index;
        index += 1;
        if matches!(bytes.get(index), Some(b'+') | Some(b'-')) {
            index += 1;
        }
        let digit_start = index;
        while matches!(bytes.get(index), Some(b'0'..=b'9')) {
            index += 1;
        }
        if digit_start == index {
            index = exponent_start;
        }
    }

    trimmed[..index].parse::<f64>().unwrap_or(f64::NAN)
}

pub fn is_nan(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let val = args
        .get(0)
        .unwrap_or(&Value::undefined())
        .to_number(&mut vm.factory.memory_allocator);
    Ok(Value::bool(val.is_nan()))
}

pub fn is_finite(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let val = args
        .get(0)
        .unwrap_or(&Value::undefined())
        .to_number(&mut vm.factory.memory_allocator);
    Ok(Value::bool(val.is_finite()))
}

pub fn escape(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let string = args.get(0).unwrap_or(&Value::undefined()).to_string();
    let mut escaped = String::new();
    for unit in string.encode_utf16() {
        if is_escape_unescaped(unit) {
            escaped.push(char::from_u32(unit as u32).unwrap());
        } else if unit < 256 {
            escaped.push('%');
            escaped.push_str(&format!("{:02X}", unit));
        } else {
            escaped.push_str(&format!("%u{:04X}", unit));
        }
    }
    Ok(vm.factory.string(escaped))
}

pub fn unescape(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let string = args.get(0).unwrap_or(&Value::undefined()).to_string();
    let chars = string.chars().collect::<Vec<_>>();
    let mut units = vec![];
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == '%' {
            if i + 5 < chars.len()
                && (chars[i + 1] == 'u' || chars[i + 1] == 'U')
                && chars[i + 2..i + 6].iter().all(|c| c.is_digit(16))
            {
                let hex = chars[i + 2..i + 6].iter().collect::<String>();
                units.push(u16::from_str_radix(&hex, 16).unwrap());
                i += 6;
                continue;
            }
            if i + 2 < chars.len() && chars[i + 1..i + 3].iter().all(|c| c.is_digit(16)) {
                let hex = chars[i + 1..i + 3].iter().collect::<String>();
                units.push(u16::from_str_radix(&hex, 16).unwrap());
                i += 3;
                continue;
            }
        }

        let mut buf = [0u16; 2];
        let encoded = chars[i].encode_utf16(&mut buf);
        units.extend_from_slice(encoded);
        i += 1;
    }
    Ok(vm.factory.string(String::from_utf16_lossy(&units)))
}

pub fn decode_uri(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let string = to_string_for_uri(vm, *args.get(0).unwrap_or(&Value::undefined()))?;
    let decoded = uri_decode(&string, true)
        .map_err(|_| vm.current_context.error_uri("malformed URI sequence"))?;
    Ok(vm.factory.string(decoded))
}

pub fn decode_uri_component(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let string = to_string_for_uri(vm, *args.get(0).unwrap_or(&Value::undefined()))?;
    let decoded = uri_decode(&string, false)
        .map_err(|_| vm.current_context.error_uri("malformed URI sequence"))?;
    Ok(vm.factory.string(decoded))
}

pub fn encode_uri(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let string = to_string_for_uri(vm, *args.get(0).unwrap_or(&Value::undefined()))?;
    Ok(vm.factory.string(uri_encode(&string, true)))
}

pub fn encode_uri_component(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let string = to_string_for_uri(vm, *args.get(0).unwrap_or(&Value::undefined()))?;
    Ok(vm.factory.string(uri_encode(&string, false)))
}

fn is_escape_unescaped(unit: u16) -> bool {
    (unit >= b'A' as u16 && unit <= b'Z' as u16)
        || (unit >= b'a' as u16 && unit <= b'z' as u16)
        || (unit >= b'0' as u16 && unit <= b'9' as u16)
        || matches!(unit, 0x40 | 0x2A | 0x5F | 0x2B | 0x2D | 0x2E | 0x2F)
}

fn to_string_for_uri(vm: &mut VM, value: Value) -> Result<String, crate::vm::error::RuntimeError> {
    if value.is_symbol() {
        return Err(vm
            .current_context
            .error_type("Cannot convert Symbol to string"));
    }
    if !value.is_object() {
        return Ok(value.to_string());
    }

    for method_name in ["toString", "valueOf"] {
        let key = vm.factory.string(method_name);
        let method = vm.get_property_by_value(value, key)?;
        if method.is_function_object() {
            let primitive = vm.call_function(method, &[], value)?;
            if !primitive.is_object() || primitive.is_symbol() {
                if primitive.is_symbol() {
                    return Err(vm
                        .current_context
                        .error_type("Cannot convert Symbol to string"));
                }
                return Ok(primitive.to_string());
            }
        }
    }
    Err(vm
        .current_context
        .error_type("Cannot convert object to string"))
}

fn uri_encode(input: &str, keep_reserved: bool) -> String {
    let mut encoded = String::new();
    for ch in input.chars() {
        if is_uri_unescaped(ch)
            || (keep_reserved && is_uri_reserved(ch))
            || ch == '#' && keep_reserved
        {
            encoded.push(ch);
        } else {
            let mut buf = [0; 4];
            for byte in ch.encode_utf8(&mut buf).bytes() {
                encoded.push('%');
                encoded.push_str(&format!("{:02X}", byte));
            }
        }
    }
    encoded
}

fn uri_decode(input: &str, keep_reserved_escaped: bool) -> Result<String, ()> {
    let bytes = input.as_bytes();
    let mut decoded = String::new();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] != b'%' {
            let ch = input[i..].chars().next().ok_or(())?;
            decoded.push(ch);
            i += ch.len_utf8();
            continue;
        }

        let first = parse_percent_byte(bytes, i)?;
        if first < 0x80 {
            let ch = first as char;
            if keep_reserved_escaped && (is_uri_reserved(ch) || ch == '#') {
                decoded.push_str(&input[i..i + 3]);
            } else {
                decoded.push(ch);
            }
            i += 3;
            continue;
        }

        let width = utf8_sequence_width(first).ok_or(())?;
        let start = i;
        let mut utf8 = Vec::with_capacity(width);
        utf8.push(first);
        i += 3;
        for _ in 1..width {
            let byte = parse_percent_byte(bytes, i)?;
            if !(0x80..=0xBF).contains(&byte) {
                return Err(());
            }
            utf8.push(byte);
            i += 3;
        }
        let decoded_part = std::str::from_utf8(&utf8).map_err(|_| ())?;
        if decoded_part.chars().count() == 1 {
            decoded.push_str(decoded_part);
        } else {
            return Err(());
        }
        let _ = start;
    }
    Ok(decoded)
}

fn parse_percent_byte(bytes: &[u8], index: usize) -> Result<u8, ()> {
    if index + 2 >= bytes.len() || bytes[index] != b'%' {
        return Err(());
    }
    let hi = hex_value(bytes[index + 1]).ok_or(())?;
    let lo = hex_value(bytes[index + 2]).ok_or(())?;
    Ok((hi << 4) | lo)
}

fn hex_value(byte: u8) -> Option<u8> {
    match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'a'..=b'f' => Some(byte - b'a' + 10),
        b'A'..=b'F' => Some(byte - b'A' + 10),
        _ => None,
    }
}

fn utf8_sequence_width(first: u8) -> Option<usize> {
    match first {
        0xC2..=0xDF => Some(2),
        0xE0..=0xEF => Some(3),
        0xF0..=0xF4 => Some(4),
        _ => None,
    }
}

fn is_uri_unescaped(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || matches!(ch, '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')')
}

fn is_uri_reserved(ch: char) -> bool {
    matches!(
        ch,
        ';' | '/' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ','
    )
}

pub fn eval(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let input = *args.get(0).unwrap_or(&Value::undefined());
    if !input.is_string() {
        return Ok(input);
    }

    let source = if vm.direct_eval_call && vm.current_context.func_ref.this_mode == ThisMode::Strict
    {
        format!("\"use strict\";\n{}", input.to_string())
    } else {
        input.to_string()
    };
    let mut parser = rapidus_parser::Parser::new("eval", source);
    let node = parser.parse_all().map_err(|err| {
        let error = vm.factory.native_error("SyntaxError", format!("{:?}", err));
        vm.current_context.error_exception(error)
    })?;
    let func_info = vm.compile(&node, true).map_err(|err| {
        let error = vm.factory.error(err.msg);
        vm.current_context.error_exception(error)
    })?;
    let script_info = parser.into_script_info();
    vm.script_info.insert(func_info.module_func_id, script_info);

    vm.prepare_context_for_function_invokation(
        Value::undefined(),
        func_info,
        Some(vm.current_context.lexical_environment),
        &[],
        this,
        Value::undefined(),
        CallMode::Native,
        false,
    )?;

    let is_called_from_native = vm.is_called_from_native;
    vm.is_called_from_native = true;
    let res = vm.run();
    vm.is_called_from_native = is_called_from_native;
    res
}

/// https://tc39.es/ecma262/#sec-parseint-string-radix
pub fn parse_int(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let input_string = args.get(0).unwrap_or(&Value::undefined()).to_string();
    let mut s = input_string.trim_start();
    let mut strip_prefix = true;
    let mut radix = args
        .get(1)
        .unwrap_or(&Value::undefined())
        .to_int32(&mut vm.factory.memory_allocator) as u32;
    if radix != 0 {
        if radix < 2 || radix > 36 {
            return Ok(Value::Number(f64::NAN));
        }
        if radix != 16 {
            strip_prefix = false;
        }
    } else {
        radix = 10
    };
    if strip_prefix {
        if s.starts_with("0x") || s.starts_with("0X") {
            s = &s[2..];
            radix = 16;
        }
    }
    let z = s.split(|c: char| !c.is_digit(radix)).next().unwrap_or("");
    if z.is_empty() {
        return Ok(Value::Number(f64::NAN));
    }
    let val = Value::Number(i64::from_str_radix(z, radix).expect("unreachable") as f64);
    Ok(val)
}

pub fn deep_seq(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    if args.len() != 2 {
        return Err(vm
            .current_context
            .error_general("__assert_deep_seq(): Two arguments are needed."));
    };
    let lval = args.get(0).unwrap();
    let rval = args.get(1).unwrap();
    let val = Value::bool(deep_seq_bool(lval, rval));
    Ok(val)
}

/// Check deep strict equality.
/// Currently, only Object and Array are supported.
/// Accesor property is not suppoeed. (alway return false)
fn deep_seq_bool(lval: &Value, rval: &Value) -> bool {
    match (*lval, *rval) {
        (Value::Object(l_info), Value::Object(r_info)) => {
            let lobj_info = ObjectRef(l_info);
            let robj_info = ObjectRef(r_info);
            // sort and compare properties
            let mut l_sorted_propmap = (&lobj_info.property)
                .iter()
                .collect::<Vec<(&String, &Property)>>();
            l_sorted_propmap.sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));
            let mut r_sorted_propmap = (&robj_info.property)
                .iter()
                .collect::<Vec<(&String, &Property)>>();
            r_sorted_propmap.sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));
            if l_sorted_propmap.len() != r_sorted_propmap.len() {
                return false;
            }
            for i in 0..l_sorted_propmap.len() {
                // compare keys
                if l_sorted_propmap[i].0 != r_sorted_propmap[i].0 {
                    return false;
                }
                // compare values
                match (l_sorted_propmap[i].1, r_sorted_propmap[i].1) {
                    (Property::Data(lprop), Property::Data(rprop)) => {
                        if !deep_seq_bool(&lprop.val, &rprop.val) {
                            return false;
                        }
                    }
                    (_, _) => return false,
                }
            }
            match (&lobj_info.kind, &robj_info.kind) {
                (ObjectKind::Ordinary, ObjectKind::Ordinary) => true,
                (ObjectKind::Array(l_info), ObjectKind::Array(r_info)) => {
                    let l_elems = &*l_info.elems;
                    let r_elems = &*r_info.elems;
                    if l_elems.len() != r_elems.len() {
                        return false;
                    };
                    for i in 0..l_elems.len() {
                        let (lval, rval) = match (l_elems[i], r_elems[i]) {
                            (Property::Data(lprop), Property::Data(rprop)) => {
                                (lprop.val, rprop.val)
                            }
                            (_, _) => return false,
                        };
                        if !deep_seq_bool(&lval, &rval) {
                            return false;
                        }
                    }
                    true
                }
                (_, _) => false,
            }
        }
        (_, _) => lval.strict_eq_bool(*rval),
    }
}

pub fn require(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let file_name = {
        let val = args.get(0).ok_or(
            vm.current_context
                .error_general("require(): One argument is needed."),
        )?;
        match val {
            Value::String(_) => val.to_string(),
            _ => {
                return Err(vm
                    .current_context
                    .error_type("require(): An argument should be string."));
            }
        }
    };

    use rapidus_parser::Parser;
    let mut parser = Parser::load_module(file_name.clone())
        .map_err(|e| return vm.current_context.error_general(format!("{:?}", e)))?;
    let absolute_path = parser.file_name.to_path_buf();

    let node = parser.parse_all().map_err(|parse_err| {
        parser.handle_error(&parse_err);
        vm.current_context
            .error_general(format!("Error in parsing module \"{}\"", file_name))
    })?;

    use crate::vm::codegen::Error;
    let module_info = vm.compile(&node, true).map_err(|codegen_err| {
        let Error { msg, loc, .. } = codegen_err;
        parser.show_error_at(loc, msg);
        vm.current_context
            .error_general(format!("Error in parsing module \"{}\"", file_name))
    })?;
    let id = module_info.module_func_id;
    let script_info = parser.into_script_info();
    vm.script_info.insert(id, script_info);

    vm.prepare_context_for_function_invokation(
        Value::undefined(), // TODO: wrong?
        module_info,
        Some(vm.global_environment),
        args,
        Value::undefined(),
        Value::undefined(),
        CallMode::Module,
        false,
    )?;

    let empty_object = make_normal_object!(vm.factory);
    let id_object = vm.factory.string(absolute_path.to_string_lossy());
    let module = make_normal_object!(
        vm.factory,
        id       => false, false, false: id_object,
        exports  => true,  false, false: empty_object
    );
    vm.current_context
        .lex_env_mut()
        .set_own_value("module", module)?;

    if vm.is_trace {
        println!("--> call module");
        println!(
            "  module_id:{:?} func_id:{:?}",
            vm.current_context.func_ref.module_func_id, vm.current_context.func_ref.func_id
        );
    };

    Ok(Value::empty())
}
