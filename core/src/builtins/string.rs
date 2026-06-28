use super::helpers::{call_to_primitive, to_number as to_number_coerce};
use crate::vm::{
    jsvalue::{
        object::ObjectKind,
        symbol::{
            SYMBOL_MATCH_ALL_ID, SYMBOL_MATCH_ID, SYMBOL_REPLACE_ID, SYMBOL_SEARCH_ID,
            SYMBOL_SPLIT_ID,
        },
        value::{DataProperty, Property, Value},
    },
    vm::Factory,
    vm::VMValueResult,
    vm::VM,
};
use rustc_hash::FxHashMap;

pub fn string(factory: &mut Factory) -> Value {
    let constructor = factory.generate_builtin_constructor(
        "String",
        string_constructor,
        factory.object_prototypes.string,
    );
    constructor.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
    );
    constructor
}

pub fn string_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let string = match args.get(0).copied() {
        Some(value) if value.is_symbol() => super::symbol::symbol_descriptive_string(value)
            .expect("symbol_descriptive_string requires a Symbol value"),
        Some(value) => vm.to_string(value)?,
        None => String::new(),
    };
    let length = string.encode_utf16().count();
    let value = vm.factory.string(string);
    let this = _this;
    if this.is_object() {
        this.get_object_info().property.insert(
            "__string_data".to_string(),
            Property::new_data(DataProperty::new(value)),
        );
        this.get_object_info().insert_property(
            "length".to_string(),
            Property::new_data(DataProperty::new(Value::Number(length as f64))),
        );
    }
    Ok(value)
}

pub fn string_from_char_code(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let mut units = Vec::with_capacity(args.len());
    for arg in args {
        units.push(to_uint16(to_number_coerce(vm, *arg)?) as u16);
    }
    Ok(vm.factory.string(String::from_utf16_lossy(&units)))
}

pub fn string_from_code_point(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let mut units = Vec::new();
    for arg in args {
        let number = to_number_coerce(vm, *arg)?;
        if !number.is_finite()
            || number.trunc() != number
            || !(0.0..=0x10ffff as f64).contains(&number)
        {
            let error = vm.factory.native_error("RangeError", "Invalid code point");
            return Err(vm.current_context.error_exception(error));
        }
        let code_point = number as u32;
        if code_point <= 0xffff {
            units.push(code_point as u16);
        } else {
            let adjusted = code_point - 0x10000;
            units.push(0xd800 + (adjusted >> 10) as u16);
            units.push(0xdc00 + (adjusted & 0x3ff) as u16);
        }
    }
    Ok(vm.factory.string(String::from_utf16_lossy(&units)))
}

pub fn string_raw(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let template = args.get(0).copied().unwrap_or(Value::undefined());
    require_object_coercible(vm, template)?;
    let raw_key = vm.factory.string("raw".to_string());
    let raw = vm.get_property_by_value(template, raw_key)?;
    require_object_coercible(vm, raw)?;

    let length_key = vm.factory.string("length".to_string());
    let raw_length = vm.get_property_by_value(raw, length_key)?;
    let literal_segments = to_length(vm, raw_length)?;
    if literal_segments == 0 {
        return Ok(vm.factory.string(String::new()));
    }

    let mut result = String::new();
    for next_index in 0..literal_segments {
        let next_key = vm.factory.string(next_index.to_string());
        let next_seg = vm.get_property_by_value(raw, next_key)?;
        result.push_str(&to_string_or_throw(vm, next_seg)?);

        if next_index + 1 == literal_segments {
            break;
        }

        if let Some(substitution) = args.get(next_index + 1).copied() {
            result.push_str(&to_string_or_throw(vm, substitution)?);
        }
    }

    Ok(vm.factory.string(result))
}

pub fn string_prototype_to_string(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if this.is_string() {
        return Ok(this);
    }
    if this.is_object() {
        let value = this.get_property("__string_data");
        if value.is_string() {
            return Ok(value);
        }
    }
    Err(vm.current_context.error_type("String.prototype"))
}

pub fn string_prototype_value_of(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_prototype_to_string(vm, args, this)
}

pub fn string_prototype_iterator(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let mut properties = FxHashMap::default();
    let string_value = vm.factory.string(string);
    properties.insert(
        "__string_iterator_string".to_string(),
        Property::new_data_simple(string_value),
    );
    properties.insert(
        "__string_iterator_next_index".to_string(),
        Property::new_data_simple(Value::Number(0.0)),
    );
    let iterator = vm.factory.object(properties);
    iterator.get_object_info().prototype = vm.factory.object_prototypes.string_iterator;
    Ok(iterator)
}

pub fn string_iterator_next(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if !this.is_object() {
        return Err(vm.current_context.error_type("String Iterator next"));
    }

    let (string_value, next_index) = {
        let object = this.get_object_info();
        let string_value = object
            .property
            .get("__string_iterator_string")
            .and_then(|prop| prop.get_data())
            .map(|data| data.val);
        let next_index = object
            .property
            .get("__string_iterator_next_index")
            .and_then(|prop| prop.get_data())
            .map(|data| data.val);
        match (string_value, next_index) {
            (Some(string_value), Some(next_index)) => (string_value, next_index),
            _ => return Err(vm.current_context.error_type("String Iterator next")),
        }
    };

    if string_value.is_undefined() {
        return Ok(create_iter_result_object(vm, Value::undefined(), true));
    }

    let string = string_value.to_string();
    let chars = string.chars().collect::<Vec<_>>();
    let index = next_index.to_number(&mut vm.factory.memory_allocator) as usize;
    if index >= chars.len() {
        this.set_property("__string_iterator_string", Value::undefined());
        return Ok(create_iter_result_object(vm, Value::undefined(), true));
    }

    this.set_property(
        "__string_iterator_next_index",
        Value::Number((index + 1) as f64),
    );
    let value = vm.factory.string(chars[index].to_string());
    Ok(create_iter_result_object(vm, value, false))
}

pub fn string_prototype_char_at(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let units = string.encode_utf16().collect::<Vec<_>>();
    let pos = string_position(vm, args.get(0).copied().unwrap_or(Value::Number(0.0)))?;
    if pos < 0 || pos as usize >= units.len() {
        return Ok(vm.factory.string(String::new()));
    }
    Ok(vm
        .factory
        .string(String::from_utf16_lossy(&[units[pos as usize]])))
}

pub fn string_prototype_char_code_at(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let units = string.encode_utf16().collect::<Vec<_>>();
    let pos = string_position(vm, args.get(0).copied().unwrap_or(Value::Number(0.0)))?;
    if pos < 0 || pos as usize >= units.len() {
        return Ok(Value::Number(::std::f64::NAN));
    }
    Ok(Value::Number(units[pos as usize] as f64))
}

pub fn string_prototype_code_point_at(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let units = string.encode_utf16().collect::<Vec<_>>();
    let pos = string_position(vm, args.get(0).copied().unwrap_or(Value::Number(0.0)))?;
    if pos < 0 || pos as usize >= units.len() {
        return Ok(Value::undefined());
    }
    let pos = pos as usize;
    let first = units[pos];
    if (0xD800..=0xDBFF).contains(&first) && pos + 1 < units.len() {
        let second = units[pos + 1];
        if (0xDC00..=0xDFFF).contains(&second) {
            let high = (first as u32) - 0xD800;
            let low = (second as u32) - 0xDC00;
            return Ok(Value::Number(((high << 10) + low + 0x10000) as f64));
        }
    }
    Ok(Value::Number(first as f64))
}

pub fn string_prototype_concat(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let mut result = this_string(vm, this)?;
    for arg in args {
        result.push_str(&to_string_coerce(vm, *arg)?);
    }
    Ok(vm.factory.string(result))
}

pub fn string_prototype_substring(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let units = string.encode_utf16().collect::<Vec<_>>();
    let len = units.len() as isize;
    let start = string_position(vm, args.get(0).copied().unwrap_or(Value::Number(0.0)))?;
    let end = if let Some(end) = args.get(1).copied() {
        string_position(vm, end)?
    } else {
        len
    };
    let start = start.clamp(0, len);
    let end = end.clamp(0, len);
    let from = start.min(end) as usize;
    let to = start.max(end) as usize;
    Ok(vm
        .factory
        .string(String::from_utf16_lossy(&units[from..to])))
}

pub fn string_prototype_includes(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let search_value = args.get(0).copied().unwrap_or(Value::undefined());
    if is_regexp(vm, search_value)? {
        return Err(vm.current_context.error_type("String.prototype.includes"));
    }
    let search = to_string_coerce(vm, search_value)?;
    let position = string_position(vm, args.get(1).copied().unwrap_or(Value::Number(0.0)))?;
    let start = position.max(0) as usize;
    if start >= string.len() {
        return Ok(Value::bool(search.is_empty()));
    }
    Ok(Value::bool(string[start..].contains(&search)))
}

pub fn string_prototype_starts_with(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let search_value = args.get(0).copied().unwrap_or(Value::undefined());
    if is_regexp(vm, search_value)? {
        return Err(vm.current_context.error_type("String.prototype.startsWith"));
    }
    let search = to_string_coerce(vm, search_value)?;
    let position = string_position(vm, args.get(1).copied().unwrap_or(Value::Number(0.0)))?;
    let start = (position.max(0) as usize).min(string.len());
    Ok(Value::bool(string[start..].starts_with(&search)))
}

pub fn string_prototype_ends_with(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let search_value = args.get(0).copied().unwrap_or(Value::undefined());
    if is_regexp(vm, search_value)? {
        return Err(vm.current_context.error_type("String.prototype.endsWith"));
    }
    let search = to_string_coerce(vm, search_value)?;
    let end = if let Some(position) = args.get(1).copied() {
        string_position(vm, position)?.clamp(0, string.len() as isize) as usize
    } else {
        string.len()
    };
    Ok(Value::bool(string[..end].ends_with(&search)))
}

pub fn string_prototype_last_index_of(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let search = to_string_coerce(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let len = string.len() as isize;
    let position = if let Some(position) = args.get(1).copied() {
        let pos = string_position(vm, position)?;
        if pos == isize::MAX {
            len
        } else {
            pos
        }
    } else {
        len
    };
    let end = position.clamp(0, len) as usize;
    Ok(Value::Number(
        string[..end]
            .rfind(&search)
            .map(|pos| pos as f64)
            .unwrap_or(-1.0),
    ))
}

pub fn string_prototype_slice(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let chars = string.chars().collect::<Vec<_>>();
    let len = chars.len() as isize;
    let start = string_position(vm, args.get(0).copied().unwrap_or(Value::Number(0.0)))?;
    let end = if let Some(end) = args.get(1).copied() {
        string_position(vm, end)?
    } else {
        len
    };
    let from = relative_index(start, len);
    let to = relative_index(end, len);
    if to <= from {
        return Ok(vm.factory.string(String::new()));
    }
    let result = chars[from as usize..to as usize].iter().collect::<String>();
    Ok(vm.factory.string(result))
}

pub fn string_prototype_at(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let chars = string.chars().collect::<Vec<_>>();
    let len = chars.len() as isize;
    let index = string_position(vm, args.get(0).copied().unwrap_or(Value::Number(0.0)))?;
    let index = if index < 0 { len + index } else { index };
    if index < 0 || index >= len {
        return Ok(Value::undefined());
    }
    Ok(vm.factory.string(chars[index as usize].to_string()))
}

pub fn string_prototype_repeat(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let count = string_position(vm, args.get(0).copied().unwrap_or(Value::Number(0.0)))?;
    if count < 0 || count == isize::MAX {
        return Err(vm.current_context.error_range("String.prototype.repeat"));
    }
    Ok(vm.factory.string(string.repeat(count as usize)))
}

pub fn string_prototype_pad_start(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_pad(vm, args, this, true)
}

pub fn string_prototype_pad_end(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_pad(vm, args, this, false)
}

fn string_pad(vm: &mut VM, args: &[Value], this: Value, at_start: bool) -> VMValueResult {
    let string = this_string(vm, this)?;
    let max_length = string_position(vm, args.get(0).copied().unwrap_or(Value::Number(0.0)))?;
    let max_length = max_length.max(0) as usize;
    let string_len = string.encode_utf16().count();
    if max_length <= string_len {
        return Ok(vm.factory.string(string));
    }
    let filler = if let Some(fill) = args.get(1).copied() {
        if fill.is_undefined() {
            " ".to_string()
        } else {
            to_string_coerce(vm, fill)?
        }
    } else {
        " ".to_string()
    };
    if filler.is_empty() {
        return Ok(vm.factory.string(string));
    }
    let needed = max_length - string_len;
    let padding = repeat_utf16_prefix(&filler, needed);
    Ok(vm.factory.string(if at_start {
        format!("{}{}", padding, string)
    } else {
        format!("{}{}", string, padding)
    }))
}

fn repeat_utf16_prefix(filler: &str, needed: usize) -> String {
    let units = filler.encode_utf16().collect::<Vec<_>>();
    let mut result = Vec::with_capacity(needed);
    for idx in 0..needed {
        result.push(units[idx % units.len()]);
    }
    String::from_utf16_lossy(&result)
}

pub fn string_prototype_trim(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?.trim().to_string();
    Ok(vm.factory.string(string))
}

pub fn string_prototype_trim_start(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?.trim_start().to_string();
    Ok(vm.factory.string(string))
}

pub fn string_prototype_trim_end(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?.trim_end().to_string();
    Ok(vm.factory.string(string))
}

pub fn string_prototype_to_lower_case(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?.to_lowercase();
    Ok(vm.factory.string(string))
}

pub fn string_prototype_to_upper_case(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?.to_uppercase();
    Ok(vm.factory.string(string))
}

pub fn string_prototype_is_well_formed(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    this_string(vm, this)?;
    Ok(Value::bool(true))
}

pub fn string_prototype_to_well_formed(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    Ok(vm.factory.string(string))
}

pub fn string_prototype_locale_compare(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let other = to_string_coerce(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Number(match string.cmp(&other) {
        std::cmp::Ordering::Less => -1.0,
        std::cmp::Ordering::Equal => 0.0,
        std::cmp::Ordering::Greater => 1.0,
    }))
}

pub fn string_prototype_normalize(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    if let Some(form) = args.get(0).copied() {
        if !form.is_undefined() {
            match to_string_coerce(vm, form)?.as_str() {
                "NFC" | "NFD" | "NFKC" | "NFKD" => {}
                _ => return Err(vm.current_context.error_range("String.prototype.normalize")),
            }
        }
    }
    Ok(vm.factory.string(string))
}

pub fn string_prototype_match(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    require_object_coercible(vm, this)?;
    let regexp = args.get(0).copied().unwrap_or(Value::undefined());
    if !regexp.is_null() && !regexp.is_undefined() {
        if let Some(matcher) = get_symbol_method(vm, regexp, SYMBOL_MATCH_ID, "Symbol.match")? {
            return vm.call_function(matcher, &[this], regexp);
        }
    }

    let string = to_string_coerce(vm, this)?;
    let search = if regexp.is_undefined() {
        String::new()
    } else {
        pattern_string(vm, regexp)?
    };
    if let Some((pos, matched)) = find_pattern(&string, &search) {
        let result = create_match_result(vm, matched, pos, string);
        Ok(result)
    } else {
        Ok(Value::null())
    }
}

pub fn string_prototype_match_all(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    require_object_coercible(vm, this)?;
    let regexp = args.get(0).copied().unwrap_or(Value::undefined());
    if !regexp.is_null() && !regexp.is_undefined() {
        if let Some(matcher) =
            get_symbol_method(vm, regexp, SYMBOL_MATCH_ALL_ID, "Symbol.matchAll")?
        {
            return vm.call_function(matcher, &[this], regexp);
        }
    }

    let string = to_string_coerce(vm, this)?;
    let search = if regexp.is_undefined() {
        String::new()
    } else {
        if is_regexp(vm, regexp)? {
            let flags = regexp_flags(regexp).unwrap_or_default();
            if !flags.contains('g') {
                return Err(vm.current_context.error_type("String.prototype.matchAll"));
            }
        }
        pattern_string(vm, regexp)?
    };
    let matches = match_all_results(vm, &string, &search);
    super::array::array_prototype_values(vm, &[], matches)
}

pub fn string_prototype_search(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    require_object_coercible(vm, this)?;
    let regexp = args.get(0).copied().unwrap_or(Value::undefined());
    if !regexp.is_null() && !regexp.is_undefined() {
        if let Some(searcher) = get_symbol_method(vm, regexp, SYMBOL_SEARCH_ID, "Symbol.search")? {
            return vm.call_function(searcher, &[this], regexp);
        }
    }

    let string = to_string_coerce(vm, this)?;
    let search = if regexp.is_undefined() {
        String::new()
    } else {
        pattern_string(vm, regexp)?
    };
    Ok(Value::Number(
        find_pattern(&string, &search)
            .map(|(pos, _)| pos as f64)
            .unwrap_or(-1.0),
    ))
}

pub fn string_prototype_replace(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    require_object_coercible(vm, this)?;
    let search_value = args.get(0).copied().unwrap_or(Value::undefined());
    let replace_value = args.get(1).copied().unwrap_or(Value::undefined());
    if !search_value.is_null() && !search_value.is_undefined() {
        if let Some(replacer) =
            get_symbol_method(vm, search_value, SYMBOL_REPLACE_ID, "Symbol.replace")?
        {
            return vm.call_function(replacer, &[this, replace_value], search_value);
        }
    }

    let string = to_string_coerce(vm, this)?;
    let search = pattern_string(vm, search_value)?;
    let functional_replace = replace_value.is_function_object();
    let replace_string = if functional_replace {
        String::new()
    } else {
        to_string_coerce(vm, replace_value)?
    };

    let Some((position, matched_string)) = find_pattern(&string, &search) else {
        return Ok(vm.factory.string(string));
    };
    if regexp_flags(search_value)
        .map(|flags| flags.contains('g'))
        .unwrap_or(false)
    {
        return replace_all_string_matches(
            vm,
            string,
            matched_string,
            search,
            replace_value,
            functional_replace,
            replace_string,
        );
    }
    let replacement = if functional_replace {
        let matched = vm.factory.string(matched_string.clone());
        let position_value = Value::Number(position as f64);
        let whole = vm.factory.string(string.clone());
        let replacement = vm.call_function(
            replace_value,
            &[matched, position_value, whole],
            Value::undefined(),
        )?;
        to_string_coerce(vm, replacement)?
    } else {
        get_substitution(&search, &string, position, &replace_string)
    };

    let mut result = String::new();
    result.push_str(&string[..position]);
    result.push_str(&replacement);
    result.push_str(&string[position + matched_string.len()..]);
    Ok(vm.factory.string(result))
}

pub fn string_prototype_replace_all(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    require_object_coercible(vm, this)?;
    let search_value = args.get(0).copied().unwrap_or(Value::undefined());
    let replace_value = args.get(1).copied().unwrap_or(Value::undefined());

    if is_regexp(vm, search_value)? {
        let flags_key = vm.factory.string("flags".to_string());
        let flags = vm.get_property_by_value(search_value, flags_key)?;
        let flags = to_string_coerce(vm, flags)?;
        if !flags.contains('g') {
            return Err(vm.current_context.error_type("String.prototype.replaceAll"));
        }
    }

    if !search_value.is_null() && !search_value.is_undefined() {
        if let Some(replacer) =
            get_symbol_method(vm, search_value, SYMBOL_REPLACE_ID, "Symbol.replace")?
        {
            return vm.call_function(replacer, &[this, replace_value], search_value);
        }
    }

    let string = to_string_coerce(vm, this)?;
    let search = pattern_string(vm, search_value)?;
    let functional_replace = replace_value.is_function_object();
    let replace_string = if functional_replace {
        String::new()
    } else {
        to_string_coerce(vm, replace_value)?
    };

    replace_all_string_matches(
        vm,
        string,
        search.clone(),
        search,
        replace_value,
        functional_replace,
        replace_string,
    )
}

fn replace_all_string_matches(
    vm: &mut VM,
    string: String,
    empty_match_text: String,
    search: String,
    replace_value: Value,
    functional_replace: bool,
    replace_string: String,
) -> VMValueResult {
    let mut result = String::new();
    if search.is_empty() {
        for (position, ch) in string.char_indices() {
            result.push_str(&replacement_for_match(
                vm,
                replace_value,
                functional_replace,
                &empty_match_text,
                &string,
                position,
                &replace_string,
            )?);
            result.push(ch);
        }
        result.push_str(&replacement_for_match(
            vm,
            replace_value,
            functional_replace,
            &empty_match_text,
            &string,
            string.len(),
            &replace_string,
        )?);
        return Ok(vm.factory.string(result));
    }

    let mut next_source_position = 0;
    while let Some((relative_position, matched)) =
        find_pattern(&string[next_source_position..], &search)
    {
        let position = next_source_position + relative_position;
        result.push_str(&string[next_source_position..position]);
        result.push_str(&replacement_for_match(
            vm,
            replace_value,
            functional_replace,
            &matched,
            &string,
            position,
            &replace_string,
        )?);
        next_source_position = position + matched.len();
    }
    result.push_str(&string[next_source_position..]);
    Ok(vm.factory.string(result))
}

pub fn string_prototype_anchor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_create_html(vm, args, this, "a", Some("name"))
}

pub fn string_prototype_big(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_create_html(vm, args, this, "big", None)
}

pub fn string_prototype_blink(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_create_html(vm, args, this, "blink", None)
}

pub fn string_prototype_bold(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_create_html(vm, args, this, "b", None)
}

pub fn string_prototype_fixed(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_create_html(vm, args, this, "tt", None)
}

pub fn string_prototype_fontcolor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_create_html(vm, args, this, "font", Some("color"))
}

pub fn string_prototype_fontsize(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_create_html(vm, args, this, "font", Some("size"))
}

pub fn string_prototype_italics(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_create_html(vm, args, this, "i", None)
}

pub fn string_prototype_link(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_create_html(vm, args, this, "a", Some("href"))
}

pub fn string_prototype_small(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_create_html(vm, args, this, "small", None)
}

pub fn string_prototype_strike(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_create_html(vm, args, this, "strike", None)
}

pub fn string_prototype_sub(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_create_html(vm, args, this, "sub", None)
}

pub fn string_prototype_sup(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    string_create_html(vm, args, this, "sup", None)
}

pub fn string_prototype_substr(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let units = string.encode_utf16().collect::<Vec<_>>();
    let size = units.len() as isize;
    let int_start = to_integer_or_infinity(vm, args.get(0).copied().unwrap_or(Value::Number(0.0)))?;
    let start = if int_start == isize::MIN {
        0
    } else if int_start < 0 {
        (size + int_start).max(0)
    } else {
        int_start.min(size)
    };

    let int_length = if let Some(length) = args.get(1).copied() {
        if length.is_undefined() {
            size
        } else {
            to_integer_or_infinity(vm, length)?
        }
    } else {
        size
    };
    let result_len = int_length.max(0).min(size - start);
    let end = start + result_len;
    Ok(vm.factory.string(String::from_utf16_lossy(
        &units[start as usize..end as usize],
    )))
}

fn string_create_html(
    vm: &mut VM,
    args: &[Value],
    this: Value,
    tag: &str,
    attribute: Option<&str>,
) -> VMValueResult {
    let string = this_string(vm, this)?;
    let mut result = format!("<{}", tag);
    if let Some(attribute) = attribute {
        let value = to_string_or_throw(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
        result.push(' ');
        result.push_str(attribute);
        result.push_str("=\"");
        result.push_str(&value.replace('"', "&quot;"));
        result.push('"');
    }
    result.push('>');
    result.push_str(&string);
    result.push_str("</");
    result.push_str(tag);
    result.push('>');
    Ok(vm.factory.string(result))
}

fn relative_index(index: isize, len: isize) -> isize {
    if index < 0 {
        (len + index).max(0)
    } else {
        index.min(len)
    }
}

fn to_uint16(number: f64) -> u32 {
    if !number.is_finite() || number == 0.0 {
        return 0;
    }
    let int = number.trunc();
    let modulo = 65536.0;
    ((int % modulo + modulo) % modulo) as u32
}

/// String methods index with isize; saturates ±Infinity from the shared
/// ToIntegerOrInfinity to isize::MAX/MIN.
fn to_integer_or_infinity(
    vm: &mut VM,
    value: Value,
) -> Result<isize, crate::vm::error::RuntimeError> {
    let number = super::helpers::to_integer_or_infinity(vm, value)?;
    Ok(if number == f64::INFINITY {
        isize::MAX
    } else if number == f64::NEG_INFINITY {
        isize::MIN
    } else {
        number as isize
    })
}

fn require_object_coercible(
    vm: &mut VM,
    value: Value,
) -> Result<(), crate::vm::error::RuntimeError> {
    if value.is_null() || value.is_undefined() {
        Err(vm
            .current_context
            .error_type("Cannot convert undefined or null to object"))
    } else {
        Ok(())
    }
}

fn to_length(vm: &mut VM, value: Value) -> Result<usize, crate::vm::error::RuntimeError> {
    let number = to_number_coerce(vm, value)?;
    if number.is_nan() || number <= 0.0 {
        return Ok(0);
    }
    if number.is_infinite() {
        return Ok((1u64 << 53) as usize - 1);
    }
    Ok((number.trunc() as usize).min((1u64 << 53) as usize - 1))
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

fn get_symbol_method(
    vm: &mut VM,
    value: Value,
    id: usize,
    description: &str,
) -> Result<Option<Value>, crate::vm::error::RuntimeError> {
    let key = vm.factory.symbol_with_id(id, Some(description.to_string()));
    let method = vm.get_property_by_value(value, key)?;
    if method.is_null() || method.is_undefined() {
        return Ok(None);
    }
    if !method.is_function_object() {
        return Err(vm.current_context.error_type("GetMethod"));
    }
    Ok(Some(method))
}

fn is_regexp(vm: &mut VM, value: Value) -> Result<bool, crate::vm::error::RuntimeError> {
    if !value.is_object() {
        return Ok(false);
    }
    let key = vm.factory.well_known_symbol(SYMBOL_MATCH_ID);
    let matcher = vm.get_property_by_value(value, key)?;
    if !matcher.is_undefined() {
        return Ok(matcher.to_boolean());
    }
    Ok(regexp_pattern(value).is_some())
}

fn regexp_pattern(value: Value) -> Option<String> {
    if !value.is_object() {
        return None;
    }
    match value.get_object_info().kind {
        ObjectKind::RegExp(ref info) => Some(info.original_source.clone()),
        _ => None,
    }
}

fn regexp_flags(value: Value) -> Option<String> {
    if !value.is_object() {
        return None;
    }
    match value.get_object_info().kind {
        ObjectKind::RegExp(ref info) => Some(info.original_flags.clone()),
        _ => None,
    }
}

fn pattern_string(vm: &mut VM, value: Value) -> Result<String, crate::vm::error::RuntimeError> {
    if let Some(pattern) = regexp_pattern(value) {
        Ok(pattern)
    } else {
        to_string_coerce(vm, value)
    }
}

fn find_pattern(string: &str, pattern: &str) -> Option<(usize, String)> {
    if pattern == "\\d" {
        return string
            .char_indices()
            .find(|(_, ch)| ch.is_ascii_digit())
            .map(|(pos, ch)| (pos, ch.to_string()));
    }
    string.find(pattern).map(|pos| (pos, pattern.to_string()))
}

fn match_all_results(vm: &mut VM, string: &str, pattern: &str) -> Value {
    let mut elems = Vec::new();
    if pattern.is_empty() {
        for index in 0..=string.len() {
            elems.push(Property::new_data_simple(create_match_result(
                vm,
                String::new(),
                index,
                string.to_string(),
            )));
        }
    } else {
        let mut offset = 0;
        while offset <= string.len() {
            let Some(pos) = string[offset..].find(pattern) else {
                break;
            };
            let index = offset + pos;
            elems.push(Property::new_data_simple(create_match_result(
                vm,
                pattern.to_string(),
                index,
                string.to_string(),
            )));
            offset = index + pattern.len();
        }
    }
    vm.factory.array(elems)
}

fn create_match_result(vm: &mut VM, matched: String, index: usize, input: String) -> Value {
    let matched = vm.factory.string(matched);
    let input = vm.factory.string(input);
    let result = vm.factory.array(vec![Property::new_data_simple(matched)]);
    let mut info = result.get_object_info();
    info.insert_property(
        "index".to_string(),
        Property::new_data_simple(Value::Number(index as f64)),
    );
    info.insert_property("input".to_string(), Property::new_data_simple(input));
    result
}

fn replacement_for_match(
    vm: &mut VM,
    replace_value: Value,
    functional_replace: bool,
    matched: &str,
    string: &str,
    position: usize,
    replace_string: &str,
) -> Result<String, crate::vm::error::RuntimeError> {
    if functional_replace {
        let matched = vm.factory.string(matched.to_string());
        let position_value = Value::Number(position as f64);
        let whole = vm.factory.string(string.to_string());
        let replacement = vm.call_function(
            replace_value,
            &[matched, position_value, whole],
            Value::undefined(),
        )?;
        return to_string_coerce(vm, replacement);
    }

    Ok(get_substitution(matched, string, position, replace_string))
}

fn get_substitution(matched: &str, string: &str, position: usize, replacement: &str) -> String {
    let mut result = String::new();
    let mut chars = replacement.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch != '$' {
            result.push(ch);
            continue;
        }
        match chars.peek().copied() {
            Some('$') => {
                chars.next();
                result.push('$');
            }
            Some('&') => {
                chars.next();
                result.push_str(matched);
            }
            Some('`') => {
                chars.next();
                result.push_str(&string[..position]);
            }
            Some('\'') => {
                chars.next();
                result.push_str(&string[position + matched.len()..]);
            }
            _ => result.push('$'),
        }
    }
    result
}

fn to_string_or_throw(vm: &mut VM, value: Value) -> Result<String, crate::vm::error::RuntimeError> {
    if value.is_symbol() {
        return Err(vm
            .current_context
            .error_type("Cannot convert Symbol to string"));
    }
    if value.is_bigint() {
        return Ok(value.to_string());
    }
    if value.is_object() {
        for method_name in ["toString", "valueOf"] {
            let key = vm.factory.string(method_name);
            let method = vm.get_property_by_value(value, key)?;
            if method.is_function_object() {
                let primitive = vm.call_function(method, &[], value)?;
                if primitive.is_symbol() {
                    return Err(vm
                        .current_context
                        .error_type("Cannot convert Symbol to string"));
                }
                if !primitive.is_object() || primitive.is_bigint() {
                    return Ok(primitive.to_string());
                }
            }
        }
        return Err(vm
            .current_context
            .error_type("Cannot convert object to string"));
    }
    Ok(value.to_string())
}

pub fn string_prototype_split(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    require_object_coercible(vm, this)?;
    let separator = args.get(0).copied().unwrap_or(Value::undefined());
    let limit = args.get(1).copied().unwrap_or(Value::undefined());

    if !separator.is_null() && !separator.is_undefined() {
        if let Some(splitter) = get_symbol_method(vm, separator, SYMBOL_SPLIT_ID, "Symbol.split")? {
            return vm.call_function(splitter, &[this, limit], separator);
        }
    }

    let string = to_string_coerce(vm, this)?;
    let limit = if limit.is_undefined() {
        u32::MAX
    } else {
        to_uint32_coerce(vm, limit)?
    };
    if limit == 0 {
        return Ok(vm.factory.array(vec![]));
    }

    if separator.is_undefined() {
        let value = vm.factory.string(string);
        return Ok(vm.factory.array(vec![Property::new_data_simple(value)]));
    }

    let separator = pattern_string(vm, separator)?;
    let mut elems = vec![];
    if separator.is_empty() {
        for unit in string.encode_utf16() {
            elems.push(Property::new_data_simple(
                vm.factory.string(String::from_utf16_lossy(&[unit])),
            ));
            if elems.len() as u32 == limit {
                break;
            }
        }
        return Ok(vm.factory.array(elems));
    }

    let mut start = 0;
    while let Some(relative_position) = string[start..].find(separator.as_str()) {
        let position = start + relative_position;
        elems.push(Property::new_data_simple(
            vm.factory.string(string[start..position].to_string()),
        ));
        if elems.len() as u32 == limit {
            return Ok(vm.factory.array(elems));
        }
        start = position + separator.len();
    }
    elems.push(Property::new_data_simple(
        vm.factory.string(string[start..].to_string()),
    ));
    Ok(vm.factory.array(elems))
}

pub fn string_prototype_index_of(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let string = this_string(vm, this)?;
    let search = to_string_coerce(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let position = string_position(vm, args.get(1).copied().unwrap_or(Value::Number(0.0)))?;
    let start = (position.max(0) as usize).min(string.len());
    let found_pos = string[start..]
        .find(search.as_str())
        .map_or(-1.0, |pos| (start + pos) as f64);
    Ok(Value::Number(found_pos))
}

fn this_string(vm: &mut VM, this: Value) -> Result<String, crate::vm::error::RuntimeError> {
    if this.is_null() || this.is_undefined() {
        return Err(vm.current_context.error_type("String.prototype"));
    }
    if this.is_symbol() {
        return Err(vm
            .current_context
            .error_type("Cannot convert Symbol to string"));
    }
    if this.is_string() {
        return Ok(this.to_string());
    }
    if this.is_object() {
        for internal in ["__string_data", "__number_data", "__boolean_data"] {
            let value = this.get_property(internal);
            if !value.is_undefined() {
                return Ok(value.to_string());
            }
        }
    }
    to_string_coerce(vm, this)
}

fn to_string_coerce(vm: &mut VM, value: Value) -> Result<String, crate::vm::error::RuntimeError> {
    if value.is_symbol() {
        return Err(vm
            .current_context
            .error_type("Cannot convert Symbol to string"));
    }
    if value.is_bigint() {
        return Ok(value.to_string());
    }
    if value.is_object() && !value.is_symbol() {
        if let Some(primitive) = call_to_primitive(vm, value, "string")? {
            if primitive.is_symbol() {
                return Err(vm
                    .current_context
                    .error_type("Cannot convert Symbol to string"));
            }
            if primitive.is_bigint() {
                return Ok(primitive.to_string());
            }
            return Ok(primitive.to_string());
        }
        let to_string_key = vm.factory.string("toString".to_string());
        let to_string = vm.get_property_by_value(value, to_string_key)?;
        if to_string.is_function_object() {
            let primitive = vm.call_function(to_string, &[], value)?;
            if primitive.is_symbol() {
                return Err(vm
                    .current_context
                    .error_type("Cannot convert Symbol to string"));
            }
            if !primitive.is_object() || primitive.is_bigint() {
                return Ok(primitive.to_string());
            }
        }

        let value_of_key = vm.factory.string("valueOf".to_string());
        let value_of = vm.get_property_by_value(value, value_of_key)?;
        if value_of.is_function_object() {
            let primitive = vm.call_function(value_of, &[], value)?;
            if primitive.is_symbol() {
                return Err(vm
                    .current_context
                    .error_type("Cannot convert Symbol to string"));
            }
            if !primitive.is_object() || primitive.is_bigint() {
                return Ok(primitive.to_string());
            }
        }
        return Err(vm
            .current_context
            .error_type("Cannot convert object to string"));
    }
    Ok(value.to_string())
}

fn to_uint32_coerce(vm: &mut VM, value: Value) -> Result<u32, crate::vm::error::RuntimeError> {
    let number = to_number_coerce(vm, value)?;
    if !number.is_finite() || number == 0.0 {
        return Ok(0);
    }
    let int = number.trunc();
    let modulo = 4294967296.0;
    Ok(((int % modulo + modulo) % modulo) as u32)
}

fn string_position(vm: &mut VM, value: Value) -> Result<isize, crate::vm::error::RuntimeError> {
    let number = to_number_coerce(vm, value)?;
    Ok(if number.is_nan() {
        0
    } else if number.is_infinite() {
        if number.is_sign_negative() {
            isize::MIN
        } else {
            isize::MAX
        }
    } else {
        number.trunc() as isize
    })
}
