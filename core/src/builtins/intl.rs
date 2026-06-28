use super::{BuiltinFuncTy, VMValueResult};
use crate::vm::{
    error::RuntimeError,
    jsvalue::{
        object::{AccessorProperty, DataProperty, Object, ObjectKind, Property},
        symbol::SYMBOL_TO_STRING_TAG_ID,
        value::Value,
    },
    vm::{Factory, VM},
};
use rustc_hash::FxHashMap;

type MethodSpec = (&'static str, f64);

struct IntlTypeSpec {
    name: &'static str,
    length: f64,
    statics: &'static [MethodSpec],
    methods: &'static [MethodSpec],
    accessors: &'static [&'static str],
}

const SUPPORTED_LOCALES_OF: &[MethodSpec] = &[("supportedLocalesOf", 1.0)];

const COLLATOR_METHODS: &[MethodSpec] = &[("resolvedOptions", 0.0)];
const COLLATOR_ACCESSORS: &[&str] = &["compare"];

const DATE_TIME_FORMAT_METHODS: &[MethodSpec] = &[
    ("formatRange", 2.0),
    ("formatRangeToParts", 2.0),
    ("formatToParts", 1.0),
    ("resolvedOptions", 0.0),
];
const DATE_TIME_FORMAT_ACCESSORS: &[&str] = &["format"];

const DISPLAY_NAMES_METHODS: &[MethodSpec] = &[("of", 1.0), ("resolvedOptions", 0.0)];

const DURATION_FORMAT_METHODS: &[MethodSpec] = &[
    ("format", 1.0),
    ("formatToParts", 1.0),
    ("resolvedOptions", 0.0),
];

const LIST_FORMAT_METHODS: &[MethodSpec] = &[
    ("format", 1.0),
    ("formatToParts", 1.0),
    ("resolvedOptions", 0.0),
];

const LOCALE_METHODS: &[MethodSpec] = &[("maximize", 0.0), ("minimize", 0.0), ("toString", 0.0)];
const LOCALE_ACCESSORS: &[&str] = &[
    "baseName",
    "calendar",
    "calendars",
    "caseFirst",
    "collation",
    "collations",
    "hourCycle",
    "hourCycles",
    "language",
    "numberingSystem",
    "numberingSystems",
    "numeric",
    "region",
    "script",
    "textInfo",
    "timeZones",
    "weekInfo",
];

const NUMBER_FORMAT_METHODS: &[MethodSpec] = &[
    ("formatRange", 2.0),
    ("formatRangeToParts", 2.0),
    ("formatToParts", 1.0),
    ("resolvedOptions", 0.0),
];
const NUMBER_FORMAT_ACCESSORS: &[&str] = &["format"];

const PLURAL_RULES_METHODS: &[MethodSpec] = &[
    ("resolvedOptions", 0.0),
    ("select", 1.0),
    ("selectRange", 2.0),
];

const RELATIVE_TIME_FORMAT_METHODS: &[MethodSpec] = &[
    ("format", 2.0),
    ("formatToParts", 2.0),
    ("resolvedOptions", 0.0),
];

const SEGMENTER_METHODS: &[MethodSpec] = &[("resolvedOptions", 0.0), ("segment", 1.0)];

const TYPES: &[IntlTypeSpec] = &[
    IntlTypeSpec {
        name: "Collator",
        length: 0.0,
        statics: SUPPORTED_LOCALES_OF,
        methods: COLLATOR_METHODS,
        accessors: COLLATOR_ACCESSORS,
    },
    IntlTypeSpec {
        name: "DateTimeFormat",
        length: 0.0,
        statics: SUPPORTED_LOCALES_OF,
        methods: DATE_TIME_FORMAT_METHODS,
        accessors: DATE_TIME_FORMAT_ACCESSORS,
    },
    IntlTypeSpec {
        name: "DisplayNames",
        length: 2.0,
        statics: &[],
        methods: DISPLAY_NAMES_METHODS,
        accessors: &[],
    },
    IntlTypeSpec {
        name: "DurationFormat",
        length: 0.0,
        statics: SUPPORTED_LOCALES_OF,
        methods: DURATION_FORMAT_METHODS,
        accessors: &[],
    },
    IntlTypeSpec {
        name: "ListFormat",
        length: 0.0,
        statics: SUPPORTED_LOCALES_OF,
        methods: LIST_FORMAT_METHODS,
        accessors: &[],
    },
    IntlTypeSpec {
        name: "Locale",
        length: 1.0,
        statics: &[],
        methods: LOCALE_METHODS,
        accessors: LOCALE_ACCESSORS,
    },
    IntlTypeSpec {
        name: "NumberFormat",
        length: 0.0,
        statics: SUPPORTED_LOCALES_OF,
        methods: NUMBER_FORMAT_METHODS,
        accessors: NUMBER_FORMAT_ACCESSORS,
    },
    IntlTypeSpec {
        name: "PluralRules",
        length: 0.0,
        statics: SUPPORTED_LOCALES_OF,
        methods: PLURAL_RULES_METHODS,
        accessors: &[],
    },
    IntlTypeSpec {
        name: "RelativeTimeFormat",
        length: 0.0,
        statics: SUPPORTED_LOCALES_OF,
        methods: RELATIVE_TIME_FORMAT_METHODS,
        accessors: &[],
    },
    IntlTypeSpec {
        name: "Segmenter",
        length: 0.0,
        statics: SUPPORTED_LOCALES_OF,
        methods: SEGMENTER_METHODS,
        accessors: &[],
    },
];

pub fn intl(factory: &mut Factory) -> Value {
    let intl = ordinary_object(factory);
    insert_data_property(
        intl,
        "getCanonicalLocales",
        builtin_function(
            factory,
            "getCanonicalLocales",
            intl_supported_locales_of,
            1.0,
        ),
    );
    insert_data_property(
        intl,
        "supportedValuesOf",
        builtin_function(factory, "supportedValuesOf", intl_supported_values_of, 1.0),
    );

    for spec in TYPES {
        let constructor = intl_constructor_object(factory, spec);
        insert_data_property(intl, spec.name, constructor);
    }
    insert_to_string_tag(factory, intl, "Intl");
    intl
}

fn intl_constructor_object(factory: &mut Factory, spec: &IntlTypeSpec) -> Value {
    let prototype = ordinary_object(factory);
    insert_data_property(prototype, "__intl_type", factory.string(spec.name));
    for &(name, length) in spec.methods {
        let func = intl_method_for(spec.name, name);
        insert_data_property(
            prototype,
            name,
            builtin_function(factory, name, func, length),
        );
    }
    for &name in spec.accessors {
        let getter = builtin_function(
            factory,
            format!("get {}", name),
            intl_accessor_for(spec.name, name),
            0.0,
        );
        prototype.get_object_info().insert_property(
            name.to_string(),
            Property::Accessor(AccessorProperty {
                get: getter,
                set: Value::undefined(),
                enumerable: false,
                configurable: true,
            }),
        );
    }
    insert_to_string_tag(factory, prototype, &format!("Intl.{}", spec.name));

    let constructor = factory.generate_builtin_constructor(spec.name, intl_constructor, prototype);
    constructor.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(spec.length)).set_configurable()),
    );
    for &(name, length) in spec.statics {
        insert_data_property(
            constructor,
            name,
            builtin_function(factory, name, intl_supported_locales_of, length),
        );
    }
    prototype.set_constructor(constructor);
    constructor
}

fn ordinary_object(factory: &mut Factory) -> Value {
    Value::Object(factory.alloc(Object {
        kind: ObjectKind::Ordinary,
        prototype: factory.object_prototypes.object,
        property: FxHashMap::default(),
        property_order: Vec::new(),
        private_elements: FxHashMap::default(),
        sym_property: FxHashMap::default(),
        sym_property_order: Vec::new(),
        extensible: true,
    }))
}

fn builtin_function(
    factory: &mut Factory,
    name: impl Into<String>,
    func: BuiltinFuncTy,
    length: f64,
) -> Value {
    let function = Value::builtin_function_with_proto(
        &mut factory.memory_allocator,
        factory.object_prototypes.function,
        name,
        func,
    );
    function.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(length)).set_configurable()),
    );
    function
}

fn insert_data_property(object: Value, name: &str, value: Value) {
    object.get_object_info().insert_property(
        name.to_string(),
        Property::new_data(DataProperty::new(value).set_writable().set_configurable()),
    );
}

fn insert_to_string_tag(factory: &mut Factory, object: Value, tag: &str) {
    let tag = factory.string(tag);
    super::helpers::define_well_known_symbol_property(
        factory,
        object,
        SYMBOL_TO_STRING_TAG_ID,
        Property::new_data(DataProperty::new(tag).set_configurable()),
    );
}

fn intl_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let type_name = intl_type_name(vm, this);
    let locales =
        canonicalize_locale_list(vm, args.get(0).copied().unwrap_or(Value::undefined()), true)?;
    if type_name == "NumberFormat" {
        validate_number_format_options(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    }
    let locale = locales
        .first()
        .cloned()
        .unwrap_or_else(|| "en-US".to_string());
    this.set_property("__intl_type", vm.factory.string(type_name));
    this.set_property("__intl_locale", vm.factory.string(locale));
    this.set_property(
        "__intl_options",
        args.get(1).copied().unwrap_or(Value::undefined()),
    );
    Ok(this)
}

fn intl_method_for(type_name: &str, name: &str) -> BuiltinFuncTy {
    match (type_name, name) {
        (_, "resolvedOptions") => intl_resolved_options,
        (_, "supportedLocalesOf") => intl_supported_locales_of,
        ("Collator", "compare") => intl_collator_compare,
        ("DateTimeFormat", "formatRange") => intl_format_range,
        ("DateTimeFormat", "formatRangeToParts") => intl_format_range_to_parts,
        ("DateTimeFormat", "formatToParts") => intl_format_to_parts,
        ("DisplayNames", "of") => intl_format_value,
        ("DurationFormat", "format") => intl_format_value,
        ("DurationFormat", "formatToParts") => intl_format_to_parts,
        ("ListFormat", "format") => intl_list_format,
        ("ListFormat", "formatToParts") => intl_list_format_to_parts,
        ("NumberFormat", "formatRange") => intl_format_range,
        ("NumberFormat", "formatRangeToParts") => intl_format_range_to_parts,
        ("NumberFormat", "formatToParts") => intl_format_to_parts,
        ("PluralRules", "select") => intl_plural_rules_select,
        ("PluralRules", "selectRange") => intl_plural_rules_select,
        ("RelativeTimeFormat", "format") => intl_relative_time_format,
        ("RelativeTimeFormat", "formatToParts") => intl_format_to_parts,
        ("Segmenter", "segment") => intl_segmenter_segment,
        _ => intl_format_value,
    }
}

fn intl_accessor_for(type_name: &str, name: &str) -> BuiltinFuncTy {
    match (type_name, name) {
        ("Collator", "compare") => intl_bound_compare,
        (_, "format") => intl_bound_format,
        ("Locale", _) => intl_locale_accessor,
        _ => intl_unimplemented,
    }
}

fn intl_bound_format(vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    let function = builtin_function(&mut vm.factory, "format", intl_format_value, 1.0);
    copy_intl_slots(function, _this);
    Ok(function)
}

fn intl_bound_compare(vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    let function = builtin_function(&mut vm.factory, "compare", intl_collator_compare, 2.0);
    copy_intl_slots(function, _this);
    Ok(function)
}

fn intl_format_value(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = args.get(0).copied().unwrap_or(Value::undefined());
    if intl_type_name(vm, this) == "NumberFormat" {
        let options = this.get_property("__intl_options");
        let locale = this.get_property("__intl_locale");
        let locale = if locale.is_string() {
            locale.to_string()
        } else {
            "en-US".to_string()
        };
        let formatted = format_number_value(vm, value, options, &locale)?;
        return Ok(vm.factory.string(formatted));
    }
    Ok(vm.factory.string(value.to_string()))
}

fn intl_format_range(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let start = args
        .get(0)
        .copied()
        .unwrap_or(Value::undefined())
        .to_string();
    let end = args
        .get(1)
        .copied()
        .unwrap_or(Value::undefined())
        .to_string();
    Ok(vm.factory.string(format!("{}–{}", start, end)))
}

fn intl_format_to_parts(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let value = args.get(0).copied().unwrap_or(Value::undefined());
    let part = ordinary_object(&mut vm.factory);
    insert_data_property(part, "type", vm.factory.string("literal"));
    insert_data_property(part, "value", vm.factory.string(value.to_string()));
    Ok(vm.factory.array(vec![Property::new_data_simple(part)]))
}

fn intl_format_range_to_parts(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    intl_format_to_parts(vm, args, this)
}

fn intl_list_format(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let list = args.get(0).copied().unwrap_or(Value::undefined());
    if list.is_array_object() {
        let array = list.as_array_mut();
        let mut values = Vec::new();
        for index in 0..array.get_length() {
            values.push(array.get_element(index).as_data().val.to_string());
        }
        return Ok(vm.factory.string(values.join(", ")));
    }
    Ok(vm.factory.string(list.to_string()))
}

fn intl_list_format_to_parts(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    intl_format_to_parts(vm, args, this)
}

fn intl_relative_time_format(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let value = args
        .get(0)
        .copied()
        .unwrap_or(Value::undefined())
        .to_string();
    let unit = args
        .get(1)
        .copied()
        .unwrap_or(Value::undefined())
        .to_string();
    Ok(vm.factory.string(format!("{} {}", value, unit)))
}

fn intl_collator_compare(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let lhs = args
        .get(0)
        .copied()
        .unwrap_or(Value::undefined())
        .to_string();
    let rhs = args
        .get(1)
        .copied()
        .unwrap_or(Value::undefined())
        .to_string();
    Ok(Value::Number(lhs.cmp(&rhs) as i32 as f64))
}

fn intl_plural_rules_select(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let value = args
        .get(0)
        .copied()
        .unwrap_or(Value::undefined())
        .to_number(&mut vm.factory.memory_allocator);
    Ok(vm
        .factory
        .string(if value == 1.0 { "one" } else { "other" }))
}

fn intl_resolved_options(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let locale = this.get_property("__intl_locale");
    let locale = if locale.is_undefined() {
        vm.factory.string("en-US")
    } else {
        locale
    };
    let object = ordinary_object(&mut vm.factory);
    insert_data_property(object, "locale", locale);
    insert_data_property(object, "numberingSystem", vm.factory.string("latn"));
    insert_data_property(object, "calendar", vm.factory.string("gregory"));
    insert_data_property(object, "timeZone", vm.factory.string("UTC"));
    insert_data_property(object, "style", vm.factory.string("decimal"));
    insert_data_property(object, "type", vm.factory.string("cardinal"));
    insert_data_property(object, "granularity", vm.factory.string("grapheme"));
    Ok(object)
}

fn intl_supported_locales_of(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let locales = canonicalize_locale_list(
        vm,
        args.get(0).copied().unwrap_or(Value::undefined()),
        false,
    )?;
    let elems = locales
        .into_iter()
        .map(|locale| Property::new_data_simple(vm.factory.string(locale)))
        .collect();
    Ok(vm.factory.array(elems))
}

fn intl_supported_values_of(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let key = args
        .get(0)
        .copied()
        .unwrap_or(Value::undefined())
        .to_string();
    let values: &[&str] = match key.as_str() {
        "calendar" => &["gregory", "iso8601"],
        "collation" => &["default"],
        "currency" => &["USD", "EUR", "JPY"],
        "numberingSystem" => &["latn"],
        "timeZone" => &["UTC"],
        "unit" => &["meter", "second", "kilometer-per-hour"],
        _ => &[],
    };
    let elems = values
        .iter()
        .map(|value| Property::new_data_simple(vm.factory.string(*value)))
        .collect();
    Ok(vm.factory.array(elems))
}

fn intl_locale_accessor(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let locale = this.get_property("__intl_locale");
    if locale.is_undefined() {
        Ok(vm.factory.string("en-US"))
    } else {
        Ok(locale)
    }
}

fn intl_segmenter_segment(vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    let segments = ordinary_object(&mut vm.factory);
    let containing = builtin_function(&mut vm.factory, "containing", intl_segment_containing, 1.0);
    insert_data_property(segments, "containing", containing);
    Ok(segments)
}

fn intl_segment_containing(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let index = args
        .get(0)
        .copied()
        .unwrap_or(Value::Number(0.0))
        .to_number(&mut vm.factory.memory_allocator);
    let object = ordinary_object(&mut vm.factory);
    insert_data_property(object, "segment", vm.factory.string(""));
    insert_data_property(object, "index", Value::Number(index));
    Ok(object)
}

fn copy_intl_slots(target: Value, source: Value) {
    for key in ["__intl_type", "__intl_locale", "__intl_options"] {
        let value = source.get_property(key);
        if !value.is_undefined() {
            target.set_property(key, value);
        }
    }
}

fn intl_type_name(_vm: &mut VM, this: Value) -> String {
    let direct = this.get_property("__intl_type");
    if direct.is_string() {
        return direct.to_string();
    }
    if this.is_object() {
        let prototype = this.get_object_info().prototype;
        let value = prototype.get_property("__intl_type");
        if value.is_string() {
            return value.to_string();
        }
    }
    String::new()
}

fn canonicalize_locale_list(
    vm: &mut VM,
    locales: Value,
    reject_null: bool,
) -> Result<Vec<String>, RuntimeError> {
    if locales.is_undefined() {
        return Ok(Vec::new());
    }
    if locales.is_null() {
        return if reject_null {
            Err(vm.current_context.error_type("Invalid locale list"))
        } else {
            Ok(Vec::new())
        };
    }
    if locales.is_string() {
        return Ok(vec![canonicalize_locale_tag(vm, &locales.to_string())?]);
    }
    if !locales.is_object() {
        return Ok(Vec::new());
    }

    let mut raw = Vec::new();
    let length_key = vm.factory.string("length");
    let length = vm.get_property_by_value(locales, length_key)?;
    if length.is_undefined() {
        raw.push(to_locale_string(vm, locales)?);
    } else {
        let len = length
            .to_number(&mut vm.factory.memory_allocator)
            .max(0.0)
            .min(9_007_199_254_740_991.0) as usize;
        for index in 0..len {
            let value = vm.get_property_by_value(locales, Value::Number(index as f64))?;
            if value.is_undefined() {
                continue;
            }
            raw.push(to_locale_string(vm, value)?);
        }
    }

    let mut result = Vec::new();
    for locale in raw {
        let locale = canonicalize_locale_tag(vm, &locale)?;
        if !result.iter().any(|existing| existing == &locale) {
            result.push(locale);
        }
    }
    Ok(result)
}

fn to_locale_string(vm: &mut VM, value: Value) -> Result<String, RuntimeError> {
    let locale = value.get_property("__intl_locale");
    if locale.is_string() {
        return Ok(locale.to_string());
    }
    if value.is_object() {
        let key = vm.factory.string("toString");
        let method = vm.get_property_by_value(value, key)?;
        if method.is_function_object() {
            let primitive = vm.call_function(method, &[], value)?;
            if primitive.is_object() {
                return Err(vm
                    .current_context
                    .error_type("Cannot convert locale to string"));
            }
            return Ok(primitive.to_string());
        }
    }
    Ok(value.to_string())
}

fn canonicalize_locale_tag(vm: &mut VM, locale: &str) -> Result<String, RuntimeError> {
    let locale = match locale {
        "art-lojban" | "i-lux" => "jbo",
        "mo" => "ro",
        _ => locale,
    };
    if !is_structurally_valid_language_tag(locale) {
        return Err(vm.current_context.error_range("Invalid language tag"));
    }
    let mut seen = Vec::new();
    let mut parts = Vec::new();
    for (index, subtag) in locale.split('-').enumerate() {
        let canonical = if index == 0 {
            subtag.to_ascii_lowercase()
        } else if subtag.len() == 4 && subtag.chars().all(|ch| ch.is_ascii_alphabetic()) {
            let mut chars = subtag.chars();
            let first = chars.next().unwrap().to_ascii_uppercase();
            let rest = chars.as_str().to_ascii_lowercase();
            format!("{}{}", first, rest)
        } else if subtag.len() == 2 && subtag.chars().all(|ch| ch.is_ascii_alphabetic())
            || subtag.len() == 3 && subtag.chars().all(|ch| ch.is_ascii_digit())
        {
            subtag.to_ascii_uppercase()
        } else {
            subtag.to_ascii_lowercase()
        };
        if index > 0 && !matches!(canonical.as_str(), "u" | "t" | "x") {
            if seen.iter().any(|seen| seen == &canonical) {
                return Err(vm
                    .current_context
                    .error_range("Duplicate language tag subtag"));
            }
            seen.push(canonical.clone());
        }
        parts.push(canonical);
    }
    Ok(parts.join("-"))
}

fn is_structurally_valid_language_tag(locale: &str) -> bool {
    if locale.is_empty()
        || locale.contains('_')
        || locale.eq_ignore_ascii_case("nan")
        || locale.starts_with('-')
        || locale.ends_with('-')
        || locale.contains("--")
    {
        return false;
    }
    let mut parts = locale.split('-');
    let Some(language) = parts.next() else {
        return false;
    };
    if !(language.len() == 2 || language.len() == 3 || language == "und")
        || !language.chars().all(|ch| ch.is_ascii_alphabetic())
    {
        return false;
    }
    let mut extension_singleton = false;
    for subtag in parts {
        if subtag.len() == 1 {
            extension_singleton = true;
            if !subtag.chars().all(|ch| ch.is_ascii_alphanumeric()) {
                return false;
            }
            continue;
        }
        if extension_singleton && subtag.len() < 2 {
            return false;
        }
        if subtag.len() > 8 || !subtag.chars().all(|ch| ch.is_ascii_alphanumeric()) {
            return false;
        }
        extension_singleton = false;
    }
    !extension_singleton
}

fn validate_number_format_options(vm: &mut VM, options: Value) -> Result<(), RuntimeError> {
    if options.is_undefined() {
        return Ok(());
    }
    if !options.is_object() {
        return Err(vm.current_context.error_type("Intl.NumberFormat options"));
    }
    if let Some(locale_matcher) = get_option_string(vm, options, "localeMatcher")? {
        if locale_matcher != "lookup" && locale_matcher != "best fit" {
            return Err(vm.current_context.error_range("Invalid localeMatcher"));
        }
    }
    let style = get_option_string(vm, options, "style")?;
    if let Some(style) = style.as_deref() {
        if !matches!(style, "decimal" | "percent" | "currency" | "unit") {
            return Err(vm
                .current_context
                .error_range("Invalid number format style"));
        }
    }
    if style.as_deref() == Some("currency") {
        let currency = get_option_string(vm, options, "currency")?;
        let valid = currency
            .as_deref()
            .map(|currency| {
                currency.len() == 3 && currency.chars().all(|ch| ch.is_ascii_alphabetic())
            })
            .unwrap_or(false);
        if !valid {
            return Err(vm.current_context.error_range("Invalid currency code"));
        }
    }
    if let Some(value) = get_option_number(vm, options, "maximumSignificantDigits")? {
        if !value.is_finite() || value < 1.0 || value > 21.0 {
            return Err(vm
                .current_context
                .error_range("Invalid maximumSignificantDigits"));
        }
    }
    Ok(())
}

fn get_option_string(
    vm: &mut VM,
    options: Value,
    name: &str,
) -> Result<Option<String>, RuntimeError> {
    let key = vm.factory.string(name);
    let value = vm.get_property_by_value(options, key)?;
    if value.is_undefined() {
        Ok(None)
    } else {
        Ok(Some(value.to_string()))
    }
}

fn get_option_number(vm: &mut VM, options: Value, name: &str) -> Result<Option<f64>, RuntimeError> {
    let key = vm.factory.string(name);
    let value = vm.get_property_by_value(options, key)?;
    if value.is_undefined() {
        Ok(None)
    } else {
        Ok(Some(value.to_number(&mut vm.factory.memory_allocator)))
    }
}

fn format_number_value(
    vm: &mut VM,
    value: Value,
    options: Value,
    locale: &str,
) -> Result<String, RuntimeError> {
    validate_number_format_options(vm, options)?;
    let decimal = if let Some(decimal) = value.bigint_decimal() {
        decimal
    } else {
        let number = value.to_number(&mut vm.factory.memory_allocator);
        if number.is_nan() || number.is_infinite() {
            return Ok(Value::Number(number).to_string());
        }
        format!("{:.0}", number.trunc())
    };
    format_decimal_for_number_format(vm, &decimal, options, locale)
}

pub fn format_bigint_to_locale_string(
    vm: &mut VM,
    decimal: &str,
    args: &[Value],
) -> Result<String, RuntimeError> {
    let locales =
        canonicalize_locale_list(vm, args.get(0).copied().unwrap_or(Value::undefined()), true)?;
    let locale = locales
        .first()
        .cloned()
        .unwrap_or_else(|| "en-US".to_string());
    let options = args.get(1).copied().unwrap_or(Value::undefined());
    validate_number_format_options(vm, options)?;
    format_decimal_for_number_format(vm, decimal, options, &locale)
}

fn format_decimal_for_number_format(
    vm: &mut VM,
    decimal: &str,
    options: Value,
    locale: &str,
) -> Result<String, RuntimeError> {
    let german = locale.starts_with("de");
    let group = if german { '.' } else { ',' };
    let decimal_separator = if german { ',' } else { '.' };
    let mut number = decimal.trim_start_matches('+').to_string();
    let negative = number.starts_with('-');
    if negative {
        number.remove(0);
    }
    while number.len() > 1 && number.starts_with('0') {
        number.remove(0);
    }

    let style = if options.is_object() {
        get_option_string(vm, options, "style")?.unwrap_or_else(|| "decimal".to_string())
    } else {
        "decimal".to_string()
    };
    if style == "percent" {
        number.push_str("00");
    }

    if options.is_object() {
        if let Some(max) = get_option_number(vm, options, "maximumSignificantDigits")? {
            number = round_decimal_to_significant_digits(&number, max as usize);
        }
        let min_integer = get_option_number(vm, options, "minimumIntegerDigits")?
            .unwrap_or(1.0)
            .max(1.0) as usize;
        while number.len() < min_integer {
            number.insert(0, '0');
        }
    }

    let use_grouping = if options.is_object() {
        let key = vm.factory.string("useGrouping");
        let value = vm.get_property_by_value(options, key)?;
        value.is_undefined() || value.to_boolean()
    } else {
        true
    };
    let mut formatted = if use_grouping {
        group_decimal_integer(&number, group)
    } else {
        number
    };
    if negative && formatted != "0" {
        formatted.insert(0, '-');
    }

    if options.is_object() {
        if let Some(min_fraction) = get_option_number(vm, options, "minimumFractionDigits")? {
            let count = min_fraction.max(0.0) as usize;
            if count > 0 {
                formatted.push(decimal_separator);
                formatted.push_str(&"0".repeat(count));
            }
        }
    }
    if style == "percent" {
        if german {
            formatted.push('\u{a0}');
        }
        formatted.push('%');
    } else if style == "currency" {
        let currency = if options.is_object() {
            get_option_string(vm, options, "currency")?.unwrap_or_else(|| "USD".to_string())
        } else {
            "USD".to_string()
        };
        formatted = format!("{} {}", currency.to_ascii_uppercase(), formatted);
    }
    Ok(formatted)
}

fn round_decimal_to_significant_digits(decimal: &str, digits: usize) -> String {
    if digits == 0 || decimal.len() <= digits {
        return decimal.to_string();
    }
    let mut chars: Vec<u8> = decimal.as_bytes().to_vec();
    let round_up = chars[digits] >= b'5';
    chars.truncate(digits);
    if round_up {
        let mut index = chars.len();
        loop {
            if index == 0 {
                chars.insert(0, b'1');
                break;
            }
            index -= 1;
            if chars[index] == b'9' {
                chars[index] = b'0';
            } else {
                chars[index] += 1;
                break;
            }
        }
    }
    chars.extend(std::iter::repeat(b'0').take(decimal.len().saturating_sub(digits)));
    String::from_utf8(chars).unwrap_or_else(|_| decimal.to_string())
}

fn group_decimal_integer(decimal: &str, separator: char) -> String {
    let mut result = String::new();
    for (index, ch) in decimal.chars().rev().enumerate() {
        if index > 0 && index % 3 == 0 {
            result.push(separator);
        }
        result.push(ch);
    }
    result.chars().rev().collect()
}

fn intl_unimplemented(vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    Err(intl_type_error(vm))
}

fn intl_type_error(vm: &mut VM) -> RuntimeError {
    vm.current_context
        .error_type("Intl operation not implemented")
}
