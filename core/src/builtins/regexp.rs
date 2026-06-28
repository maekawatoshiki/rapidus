use super::{helpers, VMValueResult};
use crate::vm::{
    error::RuntimeError,
    jsvalue::{
        object::{AccessorProperty, DataProperty, Object, ObjectKind, Property, RegExpObjectInfo},
        symbol::{SYMBOL_MATCH_ID, SYMBOL_REPLACE_ID, SYMBOL_SEARCH_ID, SYMBOL_SPLIT_ID},
        value::Value,
    },
    vm::{Factory, VM},
};
use rustc_hash::FxHashMap;

pub fn regexp(factory: &mut Factory) -> Value {
    let prototype = regexp_prototype(factory);
    factory.object_prototypes.regexp = prototype;
    let constructor = factory.generate_builtin_constructor("RegExp", regexp_constructor, prototype);
    constructor.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(2.0)).set_configurable()),
    );
    constructor.get_object_info().insert_property(
        "prototype".to_string(),
        Property::new_data(DataProperty::new(prototype)),
    );
    helpers::define_species_getter(factory, constructor);
    prototype.get_object_info().insert_property(
        "constructor".to_string(),
        Property::new_data(
            DataProperty::new(constructor)
                .set_writable()
                .set_configurable(),
        ),
    );
    constructor
}

pub fn regexp_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let pattern = args.get(0).copied().unwrap_or(Value::undefined());
    let pattern = if pattern.is_undefined() {
        String::new()
    } else {
        vm.to_string(pattern)?
    };
    let flags = args.get(1).copied().unwrap_or(Value::undefined());
    let flags = if flags.is_undefined() {
        String::new()
    } else {
        vm.to_string(flags)?
    };
    validate_regexp_flags(vm, &flags)?;

    let regexp = if vm.builtin_constructor_call {
        this
    } else {
        Value::Object(vm.factory.alloc(Object {
            kind: ObjectKind::Ordinary,
            prototype: vm.factory.object_prototypes.regexp,
            property: FxHashMap::default(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }))
    };
    initialize_regexp_object(regexp, pattern, flags);
    Ok(regexp)
}

fn initialize_regexp_object(regexp: Value, pattern: String, flags: String) {
    let mut object = regexp.get_object_info();
    object.kind = ObjectKind::RegExp(RegExpObjectInfo {
        original_source: pattern,
        original_flags: flags,
    });
    object.insert_property(
        "lastIndex".to_string(),
        Property::Data(DataProperty {
            val: Value::Number(0.0),
            writable: true,
            enumerable: false,
            configurable: false,
        }),
    );
}

fn validate_regexp_flags(vm: &mut VM, flags: &str) -> Result<(), RuntimeError> {
    let mut seen = Vec::new();
    for flag in flags.chars() {
        if !matches!(flag, 'd' | 'g' | 'i' | 'm' | 's' | 'u' | 'v' | 'y') {
            return Err(vm
                .current_context
                .error_syntax("Invalid regular expression flags"));
        }
        if seen.contains(&flag) {
            return Err(vm
                .current_context
                .error_syntax("Duplicate regular expression flag"));
        }
        seen.push(flag);
    }
    if seen.contains(&'u') && seen.contains(&'v') {
        return Err(vm
            .current_context
            .error_syntax("Invalid regular expression flags"));
    }
    Ok(())
}

fn regexp_prototype(factory: &mut Factory) -> Value {
    let source = helpers::builtin_function_with_length(factory, "get source", regexp_prototype_source, 0.0);
    let flags = helpers::builtin_function_with_length(factory, "get flags", regexp_prototype_flags, 0.0);
    let global = helpers::builtin_function_with_length(factory, "get global", regexp_prototype_global, 0.0);
    let ignore_case =
        helpers::builtin_function_with_length(factory, "get ignoreCase", regexp_prototype_ignore_case, 0.0);
    let multiline =
        helpers::builtin_function_with_length(factory, "get multiline", regexp_prototype_multiline, 0.0);
    let dot_all =
        helpers::builtin_function_with_length(factory, "get dotAll", regexp_prototype_dot_all, 0.0);
    let has_indices =
        helpers::builtin_function_with_length(factory, "get hasIndices", regexp_prototype_has_indices, 0.0);
    let sticky = helpers::builtin_function_with_length(factory, "get sticky", regexp_prototype_sticky, 0.0);
    let unicode =
        helpers::builtin_function_with_length(factory, "get unicode", regexp_prototype_unicode, 0.0);
    let unicode_sets = helpers::builtin_function_with_length(
        factory,
        "get unicodeSets",
        regexp_prototype_unicode_sets,
        0.0,
    );
    let exec = helpers::builtin_function_with_length(factory, "exec", regexp_exec, 1.0);
    let test = helpers::builtin_function_with_length(factory, "test", regexp_test, 1.0);
    let compile = helpers::builtin_function_with_length(factory, "compile", regexp_compile, 2.0);
    let symbol_match = helpers::builtin_function_with_length(factory, "[Symbol.match]", regexp_match, 1.0);
    let symbol_search =
        helpers::builtin_function_with_length(factory, "[Symbol.search]", regexp_search, 1.0);
    let symbol_replace =
        helpers::builtin_function_with_length(factory, "[Symbol.replace]", regexp_replace, 2.0);
    let symbol_split = helpers::builtin_function_with_length(factory, "[Symbol.split]", regexp_split, 2.0);
    let tag = factory.string("RegExp");

    let prototype = Value::Object(factory.alloc(Object {
        kind: ObjectKind::Ordinary,
        prototype: factory.object_prototypes.object,
        property: {
            let mut property = FxHashMap::default();
            for (name, get) in [
                ("source", source),
                ("flags", flags),
                ("global", global),
                ("ignoreCase", ignore_case),
                ("multiline", multiline),
                ("dotAll", dot_all),
                ("hasIndices", has_indices),
                ("sticky", sticky),
                ("unicode", unicode),
                ("unicodeSets", unicode_sets),
            ] {
                property.insert(
                    name.to_string(),
                    Property::Accessor(AccessorProperty {
                        get,
                        set: Value::undefined(),
                        enumerable: false,
                        configurable: true,
                    }),
                );
            }
            property.insert(
                "exec".to_string(),
                Property::new_data(DataProperty::new(exec).set_writable().set_configurable()),
            );
            property.insert(
                "test".to_string(),
                Property::new_data(DataProperty::new(test).set_writable().set_configurable()),
            );
            property.insert(
                "compile".to_string(),
                Property::new_data(DataProperty::new(compile).set_writable().set_configurable()),
            );
            property
        },
        property_order: vec![
            "source".to_string(),
            "flags".to_string(),
            "global".to_string(),
            "ignoreCase".to_string(),
            "multiline".to_string(),
            "dotAll".to_string(),
            "hasIndices".to_string(),
            "sticky".to_string(),
            "unicode".to_string(),
            "unicodeSets".to_string(),
            "exec".to_string(),
            "test".to_string(),
            "compile".to_string(),
        ],
        private_elements: rustc_hash::FxHashMap::default(),
        sym_property: FxHashMap::default(),
        sym_property_order: Vec::new(),
        extensible: true,
    }));
    helpers::define_well_known_symbol_property(
        factory,
        prototype,
        crate::vm::jsvalue::symbol::SYMBOL_TO_STRING_TAG_ID,
        Property::new_data(DataProperty::new(tag).set_configurable()),
    );
    for (id, func) in [
        (SYMBOL_MATCH_ID, symbol_match),
        (SYMBOL_SEARCH_ID, symbol_search),
        (SYMBOL_REPLACE_ID, symbol_replace),
        (SYMBOL_SPLIT_ID, symbol_split),
    ] {
        helpers::define_well_known_symbol_property(
            factory,
            prototype,
            id,
            Property::new_data(DataProperty::new(func).set_writable().set_configurable()),
        );
    }
    prototype
}

fn regexp_original_info(
    vm: &mut VM,
    this: Value,
) -> Result<Option<RegExpObjectInfo>, RuntimeError> {
    if !this.is_object() {
        return Err(vm.current_context.error_type("RegExp receiver"));
    }
    let object = this.get_object_info();
    match object.kind {
        ObjectKind::RegExp(ref info) => Ok(Some(info.clone())),
        ObjectKind::Ordinary if this.strict_eq_bool(vm.factory.object_prototypes.regexp) => {
            Ok(None)
        }
        _ => Err(vm.current_context.error_type("RegExp receiver")),
    }
}

fn regexp_prototype_source(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    Ok(match regexp_original_info(vm, this)? {
        Some(info) => vm
            .factory
            .string(escape_regexp_source(&info.original_source)),
        None => vm.factory.string("(?:)"),
    })
}

fn escape_regexp_source(source: &str) -> String {
    if source.is_empty() {
        return "(?:)".to_string();
    }
    let mut escaped = String::new();
    for ch in source.chars() {
        match ch {
            '/' => escaped.push_str("\\/"),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\u{2028}' => escaped.push_str("\\u2028"),
            '\u{2029}' => escaped.push_str("\\u2029"),
            _ => escaped.push(ch),
        }
    }
    escaped
}

fn regexp_flag(vm: &mut VM, this: Value, flag: char) -> VMValueResult {
    Ok(match regexp_original_info(vm, this)? {
        Some(info) => Value::bool(info.original_flags.contains(flag)),
        None => Value::undefined(),
    })
}

fn regexp_prototype_global(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    regexp_flag(vm, this, 'g')
}

fn regexp_prototype_ignore_case(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    regexp_flag(vm, this, 'i')
}

fn regexp_prototype_multiline(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    regexp_flag(vm, this, 'm')
}

fn regexp_prototype_dot_all(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    regexp_flag(vm, this, 's')
}

fn regexp_prototype_has_indices(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    regexp_flag(vm, this, 'd')
}

fn regexp_prototype_sticky(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    regexp_flag(vm, this, 'y')
}

fn regexp_prototype_unicode(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    regexp_flag(vm, this, 'u')
}

fn regexp_prototype_unicode_sets(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    regexp_flag(vm, this, 'v')
}

fn regexp_prototype_flags(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if !this.is_object() {
        return Err(vm.current_context.error_type("RegExp receiver"));
    }
    let mut flags = String::new();
    for (name, flag) in [
        ("hasIndices", 'd'),
        ("global", 'g'),
        ("ignoreCase", 'i'),
        ("multiline", 'm'),
        ("dotAll", 's'),
        ("unicode", 'u'),
        ("sticky", 'y'),
    ] {
        let key = vm.factory.string(name.to_string());
        if vm.get_property_by_value(this, key)?.to_boolean() {
            flags.push(flag);
        }
    }
    Ok(vm.factory.string(flags))
}

pub fn regexp_exec(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let info = match regexp_original_info(vm, this)? {
        Some(info) => info,
        None => return Err(vm.current_context.error_type("RegExp receiver")),
    };
    let string = vm.to_string(args.get(0).copied().unwrap_or(Value::undefined()))?;
    let Some((index, captures)) =
        find_regexp_captures(&string, &info.original_source, &info.original_flags)
    else {
        return Ok(Value::null());
    };

    let input = vm.factory.string(string);
    let elems = captures
        .into_iter()
        .map(|capture| {
            Property::new_data_simple(
                capture
                    .map(|matched| vm.factory.string(matched))
                    .unwrap_or(Value::undefined()),
            )
        })
        .collect();
    let result = vm.factory.array(elems);
    let mut info = result.get_object_info();
    info.insert_property(
        "index".to_string(),
        Property::new_data_simple(Value::Number(index as f64)),
    );
    info.insert_property("input".to_string(), Property::new_data_simple(input));
    Ok(result)
}

pub fn regexp_test(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let exec_key = vm.factory.string("exec");
    let exec = vm.get_property_by_value(this, exec_key)?;
    if !exec.is_function_object() {
        return Err(vm.current_context.error_type("RegExp.prototype.test"));
    }
    let string = args.get(0).copied().unwrap_or(Value::undefined());
    let result = vm.call_function(exec, &[string], this)?;
    Ok(Value::bool(!result.is_null()))
}

pub fn regexp_compile(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !this.is_object() {
        return Err(vm.current_context.error_type("RegExp.prototype.compile"));
    }
    let pattern = args.get(0).copied().unwrap_or(Value::undefined());
    let flags = args.get(1).copied().unwrap_or(Value::undefined());
    if is_regexp_object(pattern) && !flags.is_undefined() {
        return Err(vm.current_context.error_type("RegExp.prototype.compile"));
    }
    let (source, flags) = if is_regexp_object(pattern) {
        let ObjectKind::RegExp(ref info) = pattern.get_object_info().kind else {
            unreachable!();
        };
        (info.original_source.clone(), info.original_flags.clone())
    } else {
        let source = if pattern.is_undefined() {
            String::new()
        } else {
            vm.to_string(pattern)?
        };
        let flags = if flags.is_undefined() {
            String::new()
        } else {
            vm.to_string(flags)?
        };
        (source, flags)
    };
    validate_regexp_flags(vm, &flags)?;
    initialize_regexp_object(this, source, flags);
    Ok(this)
}

pub fn regexp_match(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    regexp_exec(vm, args, this)
}

pub fn regexp_search(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let result = regexp_exec(vm, args, this)?;
    if result.is_null() {
        return Ok(Value::Number(-1.0));
    }
    let index_key = vm.factory.string("index");
    Ok(vm.get_property_by_value(result, index_key)?)
}

pub fn regexp_replace(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let pattern = match regexp_original_info(vm, this)? {
        Some(info) => info.original_source,
        None => return Err(vm.current_context.error_type("RegExp receiver")),
    };
    let string = vm.to_string(args.get(0).copied().unwrap_or(Value::undefined()))?;
    let replacement = vm.to_string(args.get(1).copied().unwrap_or(Value::undefined()))?;
    let Some((index, matched)) = find_regexp_pattern(&string, &pattern) else {
        return Ok(vm.factory.string(string));
    };
    let mut result = String::new();
    result.push_str(&string[..index]);
    result.push_str(&replacement);
    result.push_str(&string[index + matched.len()..]);
    Ok(vm.factory.string(result))
}

pub fn regexp_split(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let pattern = match regexp_original_info(vm, this)? {
        Some(info) => info.original_source,
        None => return Err(vm.current_context.error_type("RegExp receiver")),
    };
    let string = vm.to_string(args.get(0).copied().unwrap_or(Value::undefined()))?;
    let limit = args
        .get(1)
        .map(|value| value.to_uint32(&mut vm.factory.memory_allocator) as usize)
        .unwrap_or(usize::MAX);
    let parts = if pattern.is_empty() {
        string.chars().map(|ch| ch.to_string()).collect::<Vec<_>>()
    } else {
        string
            .split(pattern.as_str())
            .map(|part| part.to_string())
            .collect::<Vec<_>>()
    };
    let elems = parts
        .into_iter()
        .take(limit)
        .map(|part| Property::new_data_simple(vm.factory.string(part)))
        .collect();
    Ok(vm.factory.array(elems))
}

fn is_regexp_object(value: Value) -> bool {
    value.is_object() && matches!(value.get_object_info().kind, ObjectKind::RegExp(_))
}

fn find_regexp_pattern(string: &str, pattern: &str) -> Option<(usize, String)> {
    if let Some(found) = find_with_rust_regex(string, pattern) {
        return Some(found);
    }
    if pattern == "\\d" {
        return string
            .char_indices()
            .find(|(_, ch)| ch.is_ascii_digit())
            .map(|(pos, ch)| (pos, ch.to_string()));
    }
    if pattern == "\\w" {
        return string
            .char_indices()
            .find(|(_, ch)| ch.is_ascii_alphanumeric() || *ch == '_')
            .map(|(pos, ch)| (pos, ch.to_string()));
    }
    if let Some(class) = simple_character_class(pattern) {
        return string
            .char_indices()
            .find(|(_, ch)| class.matches(*ch))
            .map(|(pos, ch)| (pos, ch.to_string()));
    }
    string.find(pattern).map(|pos| (pos, pattern.to_string()))
}

fn find_regexp_captures(
    string: &str,
    pattern: &str,
    flags: &str,
) -> Option<(usize, Vec<Option<String>>)> {
    if let Some(found) = find_captures_with_rust_regex(string, pattern, flags) {
        return Some(found);
    }
    find_regexp_pattern(string, pattern).map(|(index, matched)| (index, vec![Some(matched)]))
}

fn find_captures_with_rust_regex(
    string: &str,
    pattern: &str,
    flags: &str,
) -> Option<(usize, Vec<Option<String>>)> {
    let mut builder = regex::RegexBuilder::new(pattern);
    builder
        .case_insensitive(flags.contains('i'))
        .multi_line(flags.contains('m'))
        .dot_matches_new_line(flags.contains('s'));
    let regexp = builder.build().ok()?;
    let captures = regexp.captures(string)?;
    let matched = captures.get(0)?;
    let values = (0..captures.len())
        .map(|index| {
            captures
                .get(index)
                .map(|matched| matched.as_str().to_string())
        })
        .collect();
    Some((matched.start(), values))
}

fn find_with_rust_regex(string: &str, pattern: &str) -> Option<(usize, String)> {
    let regexp = regex::Regex::new(pattern).ok()?;
    let matched = regexp.find(string)?;
    Some((matched.start(), matched.as_str().to_string()))
}

struct SimpleCharacterClass {
    negated: bool,
    atoms: Vec<ClassAtom>,
}

enum ClassAtom {
    Char(char),
    Range(char, char),
}

impl SimpleCharacterClass {
    fn matches(&self, ch: char) -> bool {
        let matched = self.atoms.iter().any(|atom| match *atom {
            ClassAtom::Char(c) => c == ch,
            ClassAtom::Range(start, end) => start <= ch && ch <= end,
        });
        if self.negated {
            !matched
        } else {
            matched
        }
    }
}

fn simple_character_class(pattern: &str) -> Option<SimpleCharacterClass> {
    let body = simple_character_class_body(pattern)?;
    let mut chars = body.chars().peekable();
    let negated = matches!(chars.peek(), Some('^'));
    if negated {
        chars.next();
    }
    let mut atoms = Vec::new();
    let mut previous = None;
    while chars.peek().is_some() {
        let ch = read_class_char(&mut chars)?;
        if matches!(chars.peek(), Some('-')) {
            chars.next();
            if chars.peek().is_some() {
                let end = read_class_char(&mut chars)?;
                atoms.push(ClassAtom::Range(ch, end));
                previous = None;
                continue;
            }
            atoms.push(ClassAtom::Char(ch));
            previous = Some('-');
            continue;
        }
        if let Some(prev) = previous.take() {
            atoms.push(ClassAtom::Char(prev));
        }
        previous = Some(ch);
    }
    if let Some(prev) = previous {
        atoms.push(ClassAtom::Char(prev));
    }
    Some(SimpleCharacterClass { negated, atoms })
}

fn simple_character_class_body(pattern: &str) -> Option<&str> {
    if let Some(rest) = pattern.strip_prefix('[') {
        if let Some(body) = rest.strip_suffix(']') {
            return Some(body);
        }
    }
    if !pattern.starts_with("(?:[") {
        return None;
    }
    let mut escaped = false;
    for (index, ch) in pattern.char_indices().skip(4) {
        if escaped {
            escaped = false;
            continue;
        }
        if ch == '\\' {
            escaped = true;
            continue;
        }
        if ch == ']' {
            return Some(&pattern[4..index]);
        }
    }
    None
}

fn read_class_char<I>(chars: &mut std::iter::Peekable<I>) -> Option<char>
where
    I: Iterator<Item = char>,
{
    match chars.next()? {
        '\\' => match chars.next()? {
            'u' => read_hex_escape(chars, 4),
            'x' => read_hex_escape(chars, 2),
            't' => Some('\t'),
            'n' => Some('\n'),
            'r' => Some('\r'),
            'f' => Some('\x0c'),
            'v' => Some('\x0b'),
            c => Some(c),
        },
        ch => Some(ch),
    }
}

fn read_hex_escape<I>(chars: &mut std::iter::Peekable<I>, len: usize) -> Option<char>
where
    I: Iterator<Item = char>,
{
    let mut value = 0u32;
    for _ in 0..len {
        value = value * 16 + chars.next()?.to_digit(16)?;
    }
    char::from_u32(value)
}
