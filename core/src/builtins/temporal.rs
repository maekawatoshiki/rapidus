use super::helpers::to_number;
use super::{BuiltinFuncTy, VMValueResult};
use crate::vm::{
    error::RuntimeError,
    exec_context::EnvironmentRecord,
    jsvalue::{
        function::FunctionObjectKind,
        object::{
            AccessorProperty, DataProperty, Object, ObjectKind, Property, TemporalCalendarInfo,
            TemporalDateInfo, TemporalDateTimeInfo, TemporalDurationInfo, TemporalInstantInfo,
            TemporalMonthDayInfo, TemporalObjectInfo, TemporalObjectKind, TemporalTimeInfo,
            TemporalTimeZoneInfo, TemporalYearMonthInfo, TemporalZonedDateTimeInfo,
        },
        symbol::SYMBOL_ITERATOR_ID,
        symbol::SYMBOL_TO_STRING_TAG_ID,
        value::Value,
    },
    vm::{Factory, VM},
};
use chrono::Utc;
use rustc_hash::FxHashMap;

use super::temporal_spec::{TypeSpec, NOW_METHODS, TYPES};

pub fn temporal(factory: &mut Factory) -> Value {
    let temporal = ordinary_object(factory);
    for spec in TYPES {
        let constructor = temporal_constructor_object(factory, spec);
        insert_data_property(temporal, spec.name, constructor);
    }

    let now = ordinary_object(factory);
    for &(name, length) in NOW_METHODS {
        let method = builtin_function(factory, name, temporal_now_for(name), length);
        insert_data_property(now, name, method);
    }
    insert_to_string_tag(factory, now, "Temporal.Now");
    insert_data_property(temporal, "Now", now);
    insert_to_string_tag(factory, temporal, "Temporal");
    temporal
}

fn temporal_constructor_object(factory: &mut Factory, spec: &TypeSpec) -> Value {
    let prototype = ordinary_object(factory);
    for &(name, length) in spec.methods {
        let method = builtin_function(factory, name, temporal_method_for(spec.name, name), length);
        insert_data_property(prototype, name, method);
    }
    for &name in spec.accessors {
        let getter = builtin_function(
            factory,
            format!("get {}", name),
            temporal_accessor_for(name),
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
    insert_to_string_tag(factory, prototype, &format!("Temporal.{}", spec.name));

    let constructor_func = temporal_constructor_for(spec.name);
    let constructor = factory.generate_builtin_constructor(spec.name, constructor_func, prototype);
    constructor.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(spec.length)).set_configurable()),
    );
    for &(name, length) in spec.statics {
        let func = temporal_static_for(spec.name, name);
        let method = builtin_function(factory, name, func, length);
        insert_data_property(constructor, name, method);
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

fn null_proto_object(factory: &mut Factory) -> Value {
    Value::Object(factory.alloc(Object {
        kind: ObjectKind::Ordinary,
        prototype: Value::null(),
        property: FxHashMap::default(),
        property_order: Vec::new(),
        private_elements: FxHashMap::default(),
        sym_property: FxHashMap::default(),
        sym_property_order: Vec::new(),
        extensible: true,
    }))
}

fn string_array(vm: &mut VM, names: &[&str]) -> Value {
    let values = names
        .iter()
        .map(|name| Property::new_data_simple(vm.factory.string(*name)))
        .collect();
    vm.factory.array(values)
}

fn string_list(names: &[&str]) -> Vec<String> {
    names.iter().map(|name| (*name).to_string()).collect()
}

fn iterable_to_string_list(vm: &mut VM, iterable: Value) -> Result<Vec<String>, RuntimeError> {
    let iterator_key = vm.factory.well_known_symbol(SYMBOL_ITERATOR_ID);
    let method = vm.get_property_by_value(iterable, iterator_key)?;
    let iterator = if vm.is_callable(method) {
        vm.call_function(method, &[], iterable)?
    } else {
        iterable
    };
    if !iterator.is_object() {
        return Err(vm.current_context.error_type("iterator must be object"));
    }

    let next_key = vm.factory.string("next");
    let next = vm.get_property_by_value(iterator, next_key)?;
    if !vm.is_callable(next) {
        return Err(vm.current_context.error_type("iterator next"));
    }

    let mut result = Vec::new();
    loop {
        let step = vm.call_function(next, &[], iterator)?;
        if !step.is_object() {
            return Err(vm.current_context.error_type("iterator result"));
        }
        let done_key = vm.factory.string("done");
        if vm.get_property_by_value(step, done_key)?.to_boolean() {
            return Ok(result);
        }
        let value_key = vm.factory.string("value");
        let value = vm.get_property_by_value(step, value_key)?;
        if !value.is_string() {
            return Err(vm.current_context.error_type("calendar field name"));
        }
        let name = value.into_str().to_string();
        if result.iter().any(|item| item == &name) {
            return Err(vm.current_context.error_range("duplicate calendar field"));
        }
        result.push(name);
    }
}

fn calendar_fields(
    vm: &mut VM,
    calendar: Value,
    defaults: &[&str],
) -> Result<Vec<String>, RuntimeError> {
    if !calendar.is_object() {
        return Ok(defaults.iter().map(|name| (*name).to_string()).collect());
    }
    let method = get_property(vm, calendar, "fields")?;
    if method.is_undefined() || method.is_null() {
        return Ok(defaults.iter().map(|name| (*name).to_string()).collect());
    }
    let custom_method =
        calendar.has_own_property("fields") || !is_builtin_function_named(method, "fields");
    if !custom_method {
        return Ok(defaults.iter().map(|name| (*name).to_string()).collect());
    }
    if !vm.is_callable(method) {
        return Err(vm.current_context.error_type("calendar fields"));
    }
    let field_names = string_array(vm, defaults);
    let result = vm.call_function(method, &[field_names], calendar)?;
    iterable_to_string_list(vm, result)
}

fn prepare_temporal_fields(
    vm: &mut VM,
    source: Value,
    names: &[String],
) -> Result<Value, RuntimeError> {
    let fields = null_proto_object(&mut vm.factory);
    for name in names {
        if name == "constructor" || name == "__proto__" {
            return Err(vm
                .current_context
                .error_range("PrepareTemporalFields: disallowed field name"));
        }
        let value = get_property(vm, source, name)?;
        if !value.is_undefined() {
            fields.set_property(name, value);
        }
    }
    Ok(fields)
}

fn insert_iso_date_field(vm: &mut VM, fields: Value, name: &str, year: i32, month: u8, day: u8) {
    let value = match name {
        "day" => Value::Number(day as f64),
        "month" => Value::Number(month as f64),
        "monthCode" => vm.factory.string(format_month_code(month)),
        "year" => Value::Number(year as f64),
        _ => return,
    };
    fields.set_property(name, value);
}

fn iso_date_fields_object(vm: &mut VM, names: &[String], year: i32, month: u8, day: u8) -> Value {
    let fields = null_proto_object(&mut vm.factory);
    for name in names {
        insert_iso_date_field(vm, fields, name, year, month, day);
    }
    fields
}

fn copy_present_fields(
    vm: &mut VM,
    target: Value,
    source: Value,
    names: &[String],
) -> Result<(), RuntimeError> {
    if !source.is_object() {
        return Err(vm
            .current_context
            .error_type("Temporal fields must be object"));
    }
    for name in names {
        let value = get_property(vm, source, name)?;
        if !value.is_undefined() {
            target.set_property(name, value);
        }
    }
    Ok(())
}

fn call_custom_calendar_from_fields(
    vm: &mut VM,
    calendar: Value,
    method_name: &str,
    fields: Value,
) -> Result<Option<Value>, RuntimeError> {
    if !calendar.is_object() {
        return Ok(None);
    }
    let method = get_property(vm, calendar, method_name)?;
    if method.is_undefined() || method.is_null() {
        return Ok(None);
    }
    let custom_method =
        calendar.has_own_property(method_name) || !is_builtin_function_named(method, method_name);
    if !custom_method {
        return Ok(None);
    }
    if !vm.is_callable(method) {
        return Err(vm.current_context.error_type(method_name));
    }
    Ok(Some(vm.call_function(
        method,
        &[fields, Value::undefined()],
        calendar,
    )?))
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

fn temporal_constructor_for(name: &str) -> BuiltinFuncTy {
    match name {
        "Calendar" => temporal_calendar_constructor,
        "Duration" => temporal_duration_constructor,
        "Instant" => temporal_instant_constructor,
        "PlainDate" => temporal_plain_date_constructor,
        "PlainDateTime" => temporal_plain_date_time_constructor,
        "PlainMonthDay" => temporal_plain_month_day_constructor,
        "PlainTime" => temporal_plain_time_constructor,
        "PlainYearMonth" => temporal_plain_year_month_constructor,
        "TimeZone" => temporal_time_zone_constructor,
        "ZonedDateTime" => temporal_zoned_date_time_constructor,
        _ => temporal_unimplemented,
    }
}

fn temporal_static_for(type_name: &str, name: &str) -> BuiltinFuncTy {
    match (type_name, name) {
        ("Calendar", "from") => temporal_calendar_from,
        ("Duration", "compare") => temporal_duration_compare,
        ("Duration", "from") => temporal_duration_from,
        ("Instant", "compare") => temporal_instant_compare,
        ("Instant", "from") => temporal_instant_from,
        ("Instant", "fromEpochMicroseconds") => temporal_instant_from_epoch_microseconds,
        ("Instant", "fromEpochMilliseconds") => temporal_instant_from_epoch_milliseconds,
        ("Instant", "fromEpochNanoseconds") => temporal_instant_from_epoch_nanoseconds,
        ("Instant", "fromEpochSeconds") => temporal_instant_from_epoch_seconds,
        ("PlainDate", "compare") => temporal_plain_date_compare,
        ("PlainDate", "from") => temporal_plain_date_from,
        ("PlainDateTime", "compare") => temporal_plain_date_time_compare,
        ("PlainDateTime", "from") => temporal_plain_date_time_from,
        ("PlainMonthDay", "from") => temporal_plain_month_day_from,
        ("PlainTime", "compare") => temporal_plain_time_compare,
        ("PlainTime", "from") => temporal_plain_time_from,
        ("PlainYearMonth", "compare") => temporal_plain_year_month_compare,
        ("PlainYearMonth", "from") => temporal_plain_year_month_from,
        ("TimeZone", "from") => temporal_time_zone_from,
        ("ZonedDateTime", "compare") => temporal_zoned_date_time_compare,
        ("ZonedDateTime", "from") => temporal_zoned_date_time_from,
        _ => temporal_unimplemented,
    }
}

fn temporal_method_for(type_name: &str, name: &str) -> BuiltinFuncTy {
    match (type_name, name) {
        ("Calendar", "dateAdd") => temporal_calendar_date_add,
        ("Calendar", "dateFromFields") => temporal_calendar_date_from_fields,
        ("Calendar", "dateUntil") => temporal_calendar_date_until,
        ("Calendar", "day") => temporal_calendar_day,
        ("Calendar", "dayOfWeek") => temporal_calendar_day_of_week,
        ("Calendar", "dayOfYear") => temporal_calendar_day_of_year,
        ("Calendar", "daysInMonth") => temporal_calendar_days_in_month,
        ("Calendar", "daysInWeek") => temporal_calendar_days_in_week,
        ("Calendar", "daysInYear") => temporal_calendar_days_in_year,
        ("Calendar", "fields") => temporal_calendar_fields,
        ("Calendar", "inLeapYear") => temporal_calendar_in_leap_year,
        ("Calendar", "mergeFields") => temporal_calendar_merge_fields,
        ("Calendar", "month") => temporal_calendar_month,
        ("Calendar", "monthCode") => temporal_calendar_month_code,
        ("Calendar", "monthDayFromFields") => temporal_calendar_month_day_from_fields,
        ("Calendar", "monthsInYear") => temporal_calendar_months_in_year,
        ("Calendar", "era") | ("Calendar", "eraYear") => temporal_calendar_era,
        ("Calendar", "toJSON") | ("Calendar", "toString") => temporal_to_string,
        ("Calendar", "weekOfYear") => temporal_calendar_week_of_year,
        ("Calendar", "year") => temporal_calendar_year,
        ("Calendar", "yearMonthFromFields") => temporal_calendar_year_month_from_fields,
        ("Calendar", "yearOfWeek") => temporal_calendar_year_of_week,
        (_, "abs") => temporal_duration_abs,
        (_, "add") => temporal_add,
        (_, "equals") => temporal_equals,
        (_, "getCalendar") => temporal_get_calendar,
        (_, "getISOFields") => temporal_get_iso_fields,
        (_, "getTimeZone") => temporal_get_time_zone,
        (_, "negated") => temporal_duration_negated,
        (_, "round") => temporal_round,
        (_, "since") => temporal_since,
        (_, "subtract") => temporal_subtract,
        (_, "total") => temporal_duration_total,
        (_, "toInstant") => temporal_to_instant,
        (_, "toJSON") | (_, "toLocaleString") | (_, "toString") => temporal_to_string,
        (_, "toPlainDate") => temporal_to_plain_date,
        (_, "toPlainDateTime") => temporal_to_plain_date_time,
        (_, "toPlainMonthDay") => temporal_to_plain_month_day,
        (_, "toPlainTime") => temporal_to_plain_time,
        (_, "toPlainYearMonth") => temporal_to_plain_year_month,
        (_, "toZonedDateTime") => temporal_to_zoned_date_time,
        (_, "toZonedDateTimeISO") => temporal_to_zoned_date_time_iso,
        (_, "valueOf") => temporal_value_of,
        (_, "with") => temporal_with,
        (_, "withCalendar") => temporal_with_calendar,
        (_, "withPlainDate") => temporal_with_plain_date,
        (_, "withPlainTime") => temporal_with_plain_time,
        (_, "withTimeZone") => temporal_with_time_zone,
        (_, "until") => temporal_until,
        ("TimeZone", "getInstantFor") => temporal_time_zone_get_instant_for,
        ("TimeZone", "getNextTransition") | ("TimeZone", "getPreviousTransition") => {
            temporal_time_zone_get_transition
        }
        ("TimeZone", "getOffsetNanosecondsFor") => temporal_time_zone_get_offset_nanoseconds_for,
        ("TimeZone", "getOffsetStringFor") => temporal_time_zone_get_offset_string_for,
        ("TimeZone", "getPlainDateTimeFor") => temporal_time_zone_get_plain_date_time_for,
        ("TimeZone", "getPossibleInstantsFor") => temporal_time_zone_get_possible_instants_for,
        ("ZonedDateTime", "startOfDay") => temporal_zoned_date_time_start_of_day,
        _ => temporal_unimplemented,
    }
}

fn temporal_accessor_for(name: &str) -> BuiltinFuncTy {
    match name {
        "blank" => temporal_get_blank,
        "calendarId" => temporal_get_calendar_id,
        "day" => temporal_get_day,
        "dayOfWeek" => temporal_get_day_of_week,
        "dayOfYear" => temporal_get_day_of_year,
        "days" => temporal_get_duration_days,
        "daysInMonth" => temporal_get_days_in_month,
        "daysInWeek" => temporal_get_days_in_week,
        "daysInYear" => temporal_get_days_in_year,
        "epochMicroseconds" => temporal_get_epoch_microseconds,
        "epochMilliseconds" => temporal_get_epoch_milliseconds,
        "epochNanoseconds" => temporal_get_epoch_nanoseconds,
        "epochSeconds" => temporal_get_epoch_seconds,
        "era" | "eraYear" => temporal_get_undefined,
        "hour" => temporal_get_hour,
        "hours" => temporal_get_duration_hours,
        "hoursInDay" => temporal_get_hours_in_day,
        "id" => temporal_get_id,
        "inLeapYear" => temporal_get_in_leap_year,
        "microsecond" => temporal_get_microsecond,
        "microseconds" => temporal_get_duration_microseconds,
        "millisecond" => temporal_get_millisecond,
        "milliseconds" => temporal_get_duration_milliseconds,
        "minute" => temporal_get_minute,
        "minutes" => temporal_get_duration_minutes,
        "month" => temporal_get_month,
        "monthCode" => temporal_get_month_code,
        "months" => temporal_get_duration_months,
        "monthsInYear" => temporal_get_months_in_year,
        "nanosecond" => temporal_get_nanosecond,
        "nanoseconds" => temporal_get_duration_nanoseconds,
        "offset" => temporal_get_offset,
        "offsetNanoseconds" => temporal_get_offset_nanoseconds,
        "second" => temporal_get_second,
        "seconds" => temporal_get_duration_seconds,
        "sign" => temporal_get_duration_sign,
        "timeZoneId" => temporal_get_time_zone_id,
        "weekOfYear" => temporal_get_week_of_year,
        "weeks" => temporal_get_duration_weeks,
        "year" => temporal_get_year,
        "years" => temporal_get_duration_years,
        "yearOfWeek" => temporal_get_year_of_week,
        _ => temporal_unimplemented,
    }
}

fn temporal_now_for(name: &str) -> BuiltinFuncTy {
    match name {
        "instant" => temporal_now_instant,
        "plainDate" => temporal_now_plain_date,
        "plainDateISO" => temporal_now_plain_date_iso,
        "plainDateTime" => temporal_now_plain_date_time,
        "plainDateTimeISO" => temporal_now_plain_date_time_iso,
        "plainTimeISO" => temporal_now_plain_time_iso,
        "timeZoneId" => temporal_now_time_zone_id,
        "zonedDateTime" => temporal_now_zoned_date_time,
        "zonedDateTimeISO" => temporal_now_zoned_date_time_iso,
        _ => temporal_unimplemented,
    }
}

fn temporal_now_instant(vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    Ok(temporal_object_by_name(
        vm,
        "Instant",
        TemporalObjectKind::Instant(TemporalInstantInfo {
            epoch_nanoseconds: now_epoch_nanoseconds(),
        }),
    ))
}

fn temporal_now_plain_date(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let (calendar, calendar_object) = now_calendar(vm, args.get(0).copied())?;
    let time_zone = now_time_zone(vm, args.get(1).copied())?;
    let date_time = now_date_time(vm, calendar, calendar_object, &time_zone)?;
    Ok(temporal_object_by_name(
        vm,
        "PlainDate",
        TemporalObjectKind::PlainDate(date_time.date),
    ))
}

fn temporal_now_plain_date_iso(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let time_zone = now_time_zone(vm, args.get(0).copied())?;
    let date_time = now_date_time(vm, "iso8601".to_string(), None, &time_zone)?;
    Ok(temporal_object_by_name(
        vm,
        "PlainDate",
        TemporalObjectKind::PlainDate(date_time.date),
    ))
}

fn temporal_now_plain_date_time(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let (calendar, calendar_object) = now_calendar(vm, args.get(0).copied())?;
    let time_zone = now_time_zone(vm, args.get(1).copied())?;
    let date_time = now_date_time(vm, calendar, calendar_object, &time_zone)?;
    Ok(temporal_object_by_name(
        vm,
        "PlainDateTime",
        TemporalObjectKind::PlainDateTime(date_time),
    ))
}

fn temporal_now_plain_date_time_iso(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let time_zone = now_time_zone(vm, args.get(0).copied())?;
    let date_time = now_date_time(vm, "iso8601".to_string(), None, &time_zone)?;
    Ok(temporal_object_by_name(
        vm,
        "PlainDateTime",
        TemporalObjectKind::PlainDateTime(date_time),
    ))
}

fn temporal_now_plain_time_iso(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let time_zone = now_time_zone(vm, args.get(0).copied())?;
    let date_time = now_date_time(vm, "iso8601".to_string(), None, &time_zone)?;
    Ok(temporal_object_by_name(
        vm,
        "PlainTime",
        TemporalObjectKind::PlainTime(date_time.time),
    ))
}

fn temporal_now_time_zone_id(vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    Ok(vm.factory.string("UTC"))
}

fn temporal_now_zoned_date_time(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let (calendar, calendar_object) = now_calendar(vm, args.get(0).copied())?;
    let time_zone = now_time_zone(vm, args.get(1).copied())?;
    Ok(temporal_object_by_name(
        vm,
        "ZonedDateTime",
        TemporalObjectKind::ZonedDateTime(TemporalZonedDateTimeInfo {
            epoch_nanoseconds: now_epoch_nanoseconds(),
            time_zone,
            calendar,
            calendar_object,
        }),
    ))
}

fn temporal_now_zoned_date_time_iso(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let time_zone = now_time_zone(vm, args.get(0).copied())?;
    Ok(temporal_object_by_name(
        vm,
        "ZonedDateTime",
        TemporalObjectKind::ZonedDateTime(TemporalZonedDateTimeInfo {
            epoch_nanoseconds: now_epoch_nanoseconds(),
            time_zone,
            calendar: "iso8601".to_string(),
            calendar_object: None,
        }),
    ))
}

fn temporal_calendar_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_constructor(vm)?;
    let arg = args.get(0).copied().unwrap_or(Value::undefined());
    if !arg.is_string() {
        return Err(vm.current_context.error_type("invalid calendar"));
    }
    let identifier = calendar_identifier(vm, arg)?;
    set_temporal(
        this,
        TemporalObjectKind::Calendar(TemporalCalendarInfo { identifier }),
    );
    Ok(this)
}

fn temporal_duration_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_constructor(vm)?;
    let duration = TemporalDurationInfo {
        years: to_integer_arg(vm, args, 0, 0)?,
        months: to_integer_arg(vm, args, 1, 0)?,
        weeks: to_integer_arg(vm, args, 2, 0)?,
        days: to_integer_arg(vm, args, 3, 0)?,
        hours: to_integer_arg(vm, args, 4, 0)?,
        minutes: to_integer_arg(vm, args, 5, 0)?,
        seconds: to_integer_arg(vm, args, 6, 0)?,
        milliseconds: to_integer_arg(vm, args, 7, 0)?,
        microseconds: to_integer_arg(vm, args, 8, 0)?,
        nanoseconds: to_integer_arg(vm, args, 9, 0)?,
    };
    validate_duration_sign(vm, &duration)?;
    set_temporal(this, TemporalObjectKind::Duration(duration));
    Ok(this)
}

fn temporal_instant_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_constructor(vm)?;
    let epoch_nanoseconds = to_bigint_i128(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    set_temporal(
        this,
        TemporalObjectKind::Instant(TemporalInstantInfo { epoch_nanoseconds }),
    );
    Ok(this)
}

fn temporal_plain_date_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_constructor(vm)?;
    let calendar_value = args.get(3).copied().unwrap_or(Value::undefined());
    let (calendar, calendar_object) = calendar_identifier_and_object(vm, calendar_value)?;
    let date = TemporalDateInfo {
        year: to_i32_arg(vm, args, 0, 0)?,
        month: to_u8_arg(vm, args, 1, 1)?,
        day: to_u8_arg(vm, args, 2, 1)?,
        calendar,
        calendar_object,
    };
    validate_date(vm, date.year, date.month, date.day)?;
    set_temporal(this, TemporalObjectKind::PlainDate(date));
    Ok(this)
}

fn temporal_plain_date_time_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_constructor(vm)?;
    let calendar_value = args.get(9).copied().unwrap_or(Value::undefined());
    let (calendar, calendar_object) = calendar_identifier_and_object(vm, calendar_value)?;
    let date = TemporalDateInfo {
        year: to_i32_arg(vm, args, 0, 0)?,
        month: to_u8_arg(vm, args, 1, 1)?,
        day: to_u8_arg(vm, args, 2, 1)?,
        calendar,
        calendar_object,
    };
    validate_date(vm, date.year, date.month, date.day)?;
    let time = read_time_args(vm, args, 3)?;
    set_temporal(
        this,
        TemporalObjectKind::PlainDateTime(TemporalDateTimeInfo { date, time }),
    );
    Ok(this)
}

fn temporal_plain_month_day_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_constructor(vm)?;
    let month = to_u8_arg(vm, args, 0, 1)?;
    let day = to_u8_arg(vm, args, 1, 1)?;
    validate_date(vm, 1972, month, day)?;
    let calendar_value = args.get(2).copied().unwrap_or(Value::undefined());
    let (calendar, calendar_object) = calendar_identifier_and_object(vm, calendar_value)?;
    set_temporal(
        this,
        TemporalObjectKind::PlainMonthDay(TemporalMonthDayInfo {
            month,
            day,
            calendar,
            calendar_object,
        }),
    );
    Ok(this)
}

fn temporal_plain_time_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_constructor(vm)?;
    let time = read_time_args(vm, args, 0)?;
    set_temporal(this, TemporalObjectKind::PlainTime(time));
    Ok(this)
}

fn temporal_plain_year_month_constructor(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    ensure_constructor(vm)?;
    let year = to_i32_arg(vm, args, 0, 0)?;
    let month = to_u8_arg(vm, args, 1, 1)?;
    validate_date(vm, year, month, 1)?;
    let calendar_value = args.get(2).copied().unwrap_or(Value::undefined());
    let (calendar, calendar_object) = calendar_identifier_and_object(vm, calendar_value)?;
    set_temporal(
        this,
        TemporalObjectKind::PlainYearMonth(TemporalYearMonthInfo {
            year,
            month,
            calendar,
            calendar_object,
        }),
    );
    Ok(this)
}

fn temporal_time_zone_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_constructor(vm)?;
    let arg = args.get(0).copied().unwrap_or(Value::undefined());
    if !arg.is_string() {
        return Err(vm.current_context.error_type("invalid time zone"));
    }
    let mut time_zone = time_zone_identifier(vm, arg)?;
    time_zone.object = Some(this);
    set_temporal(this, TemporalObjectKind::TimeZone(time_zone));
    Ok(this)
}

fn temporal_zoned_date_time_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_constructor(vm)?;
    let epoch_nanoseconds = to_bigint_i128(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let time_zone = time_zone_identifier(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let calendar_value = args.get(2).copied().unwrap_or(Value::undefined());
    let (calendar, calendar_object) = calendar_identifier_and_object(vm, calendar_value)?;
    set_temporal(
        this,
        TemporalObjectKind::ZonedDateTime(TemporalZonedDateTimeInfo {
            epoch_nanoseconds,
            time_zone,
            calendar,
            calendar_object,
        }),
    );
    Ok(this)
}

fn temporal_calendar_from(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let arg = args.get(0).copied().unwrap_or(Value::undefined());
    if let Ok(TemporalObjectKind::Calendar(_)) = temporal_kind(vm, arg) {
        return Ok(arg);
    }
    if arg.is_object() && !matches!(temporal_kind(vm, arg), Ok(_)) {
        let id = get_property(vm, arg, "id")?;
        if id.is_string() {
            return Ok(arg);
        }
    }
    let identifier = calendar_identifier(vm, arg)?;
    Ok(temporal_object_from_constructor(
        vm,
        this,
        TemporalObjectKind::Calendar(TemporalCalendarInfo { identifier }),
    ))
}

fn temporal_duration_from(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let duration = to_temporal_duration(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(temporal_object_from_constructor(
        vm,
        this,
        TemporalObjectKind::Duration(duration),
    ))
}

fn temporal_instant_from(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let instant = to_temporal_instant(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(temporal_object_from_constructor(
        vm,
        this,
        TemporalObjectKind::Instant(instant),
    ))
}

fn temporal_instant_from_epoch_seconds(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    temporal_instant_from_epoch_scaled(vm, args, this, 1_000_000_000)
}

fn temporal_instant_from_epoch_milliseconds(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    temporal_instant_from_epoch_scaled(vm, args, this, 1_000_000)
}

fn temporal_instant_from_epoch_microseconds(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    temporal_instant_from_epoch_scaled(vm, args, this, 1_000)
}

fn temporal_instant_from_epoch_nanoseconds(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    let epoch_nanoseconds = to_bigint_i128(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(temporal_object_from_constructor(
        vm,
        this,
        TemporalObjectKind::Instant(TemporalInstantInfo { epoch_nanoseconds }),
    ))
}

fn temporal_instant_from_epoch_scaled(
    vm: &mut VM,
    args: &[Value],
    this: Value,
    scale: i128,
) -> VMValueResult {
    let value = to_number(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    if !value.is_finite() || value.fract() != 0.0 {
        return Err(vm.current_context.error_range("invalid epoch value"));
    }
    let epoch_nanoseconds = (value as i128)
        .checked_mul(scale)
        .ok_or_else(|| vm.current_context.error_range("invalid epoch value"))?;
    Ok(temporal_object_from_constructor(
        vm,
        this,
        TemporalObjectKind::Instant(TemporalInstantInfo { epoch_nanoseconds }),
    ))
}

fn temporal_plain_date_from(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    validate_temporal_overflow_option(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let date = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(temporal_object_from_constructor(
        vm,
        this,
        TemporalObjectKind::PlainDate(date),
    ))
}

fn temporal_plain_date_time_from(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    validate_temporal_overflow_option(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let date_time = to_temporal_date_time(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(temporal_object_from_constructor(
        vm,
        this,
        TemporalObjectKind::PlainDateTime(date_time),
    ))
}

fn temporal_plain_month_day_from(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    validate_temporal_overflow_option(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let month_day = to_temporal_month_day(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(temporal_object_from_constructor(
        vm,
        this,
        TemporalObjectKind::PlainMonthDay(month_day),
    ))
}

fn temporal_plain_time_from(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    validate_temporal_overflow_option(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let time = to_temporal_time(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(temporal_object_from_constructor(
        vm,
        this,
        TemporalObjectKind::PlainTime(time),
    ))
}

fn temporal_plain_year_month_from(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    validate_temporal_overflow_option(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let year_month =
        to_temporal_year_month(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(temporal_object_from_constructor(
        vm,
        this,
        TemporalObjectKind::PlainYearMonth(year_month),
    ))
}

fn temporal_time_zone_from(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let time_zone = time_zone_identifier(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(temporal_object_from_constructor(
        vm,
        this,
        TemporalObjectKind::TimeZone(time_zone),
    ))
}

fn temporal_zoned_date_time_from(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    validate_temporal_options_object(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let zoned =
        to_temporal_zoned_date_time(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(temporal_object_from_constructor(
        vm,
        this,
        TemporalObjectKind::ZonedDateTime(zoned),
    ))
}

fn temporal_plain_date_compare(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let one = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let two = to_temporal_date(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Number(
        compare_i64(iso_date_key(&one), iso_date_key(&two)) as f64,
    ))
}

fn temporal_plain_time_compare(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let one = to_temporal_time(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let two = to_temporal_time(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Number(
        compare_i128(time_nanoseconds(&one), time_nanoseconds(&two)) as f64,
    ))
}

fn temporal_plain_date_time_compare(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let one = to_temporal_date_time(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let two = to_temporal_date_time(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Number(compare_date_time(&one, &two) as f64))
}

fn temporal_plain_year_month_compare(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let one = to_temporal_year_month(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let two = to_temporal_year_month(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Number(
        compare_i64(year_month_key(&one), year_month_key(&two)) as f64,
    ))
}

fn temporal_instant_compare(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let one = to_temporal_instant(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let two = to_temporal_instant(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Number(
        compare_i128(one.epoch_nanoseconds, two.epoch_nanoseconds) as f64,
    ))
}

fn temporal_zoned_date_time_compare(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let one = to_temporal_zoned_date_time(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let two = to_temporal_zoned_date_time(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Number(
        compare_i128(one.epoch_nanoseconds, two.epoch_nanoseconds) as f64,
    ))
}

fn temporal_duration_compare(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let one = to_temporal_duration(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let two = to_temporal_duration(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let options = args.get(2).copied().unwrap_or(Value::undefined());
    validate_temporal_options_object(vm, options)?;
    validate_relative_to_option(vm, options)?;
    Ok(Value::Number(compare_i128(
        duration_total_nanoseconds(&one),
        duration_total_nanoseconds(&two),
    ) as f64))
}

fn temporal_get_id(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::Calendar(info) => Ok(vm.factory.string(info.identifier)),
        TemporalObjectKind::TimeZone(info) => Ok(vm.factory.string(info.identifier)),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_get_calendar_id(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let id = match temporal_kind(vm, this)? {
        TemporalObjectKind::PlainDate(info) => info.calendar,
        TemporalObjectKind::PlainDateTime(info) => info.date.calendar,
        TemporalObjectKind::PlainMonthDay(info) => info.calendar,
        TemporalObjectKind::PlainYearMonth(info) => info.calendar,
        TemporalObjectKind::ZonedDateTime(info) => info.calendar,
        _ => return Err(temporal_brand_error(vm)),
    };
    Ok(vm.factory.string(id))
}

fn temporal_get_time_zone_id(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::ZonedDateTime(info) => Ok(vm.factory.string(info.time_zone.identifier)),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_get_year(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_from_this(vm, this)?;
    Ok(Value::Number(date.year as f64))
}

fn temporal_get_month(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_from_this(vm, this)?;
    Ok(Value::Number(date.month as f64))
}

fn temporal_get_day(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_from_this(vm, this)?;
    Ok(Value::Number(date.day as f64))
}

fn temporal_get_month_code(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_from_this(vm, this)?;
    Ok(vm.factory.string(format_month_code(date.month)))
}

fn temporal_get_day_of_week(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_from_this(vm, this)?;
    Ok(Value::Number(
        day_of_week(date.year, date.month, date.day) as f64
    ))
}

fn temporal_get_day_of_year(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_from_this(vm, this)?;
    Ok(Value::Number(
        day_of_year(date.year, date.month, date.day) as f64
    ))
}

fn temporal_get_days_in_month(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_from_this(vm, this)?;
    Ok(Value::Number(days_in_month(date.year, date.month) as f64))
}

fn temporal_get_days_in_year(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_from_this(vm, this)?;
    Ok(Value::Number(days_in_year(date.year) as f64))
}

fn temporal_get_in_leap_year(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_from_this(vm, this)?;
    Ok(Value::Bool(is_leap_year(date.year) as u8))
}

fn temporal_get_days_in_week(_vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    Ok(Value::Number(7.0))
}

fn temporal_get_months_in_year(_vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    Ok(Value::Number(12.0))
}

fn temporal_get_week_of_year(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_from_this(vm, this)?;
    Ok(Value::Number(iso_week_fields(&date).0 as f64))
}

fn temporal_get_year_of_week(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_from_this(vm, this)?;
    Ok(Value::Number(iso_week_fields(&date).1 as f64))
}

fn temporal_get_undefined(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    date_from_this(vm, this)?;
    Ok(Value::undefined())
}

fn temporal_get_hour(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    Ok(Value::Number(time_from_this(vm, this)?.hour as f64))
}

fn temporal_get_minute(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    Ok(Value::Number(time_from_this(vm, this)?.minute as f64))
}

fn temporal_get_second(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    Ok(Value::Number(time_from_this(vm, this)?.second as f64))
}

fn temporal_get_millisecond(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    Ok(Value::Number(time_from_this(vm, this)?.millisecond as f64))
}

fn temporal_get_microsecond(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    Ok(Value::Number(time_from_this(vm, this)?.microsecond as f64))
}

fn temporal_get_nanosecond(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    Ok(Value::Number(time_from_this(vm, this)?.nanosecond as f64))
}

fn temporal_get_epoch_nanoseconds(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let ns = instant_nanoseconds_from_this(vm, this)?;
    Ok(vm.factory.bigint(ns.to_string()))
}

fn temporal_get_epoch_microseconds(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let ns = instant_nanoseconds_from_this(vm, this)?;
    Ok(vm.factory.bigint(floor_div_i128(ns, 1_000).to_string()))
}

fn temporal_get_epoch_milliseconds(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let ns = instant_nanoseconds_from_this(vm, this)?;
    Ok(Value::Number(floor_div_i128(ns, 1_000_000) as f64))
}

fn temporal_get_epoch_seconds(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let ns = instant_nanoseconds_from_this(vm, this)?;
    Ok(Value::Number(floor_div_i128(ns, 1_000_000_000) as f64))
}

fn temporal_get_offset(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let zone = zoned_time_zone_from_this(vm, this)?;
    Ok(vm
        .factory
        .string(format_offset(zone.offset_nanoseconds.unwrap_or(0))))
}

fn temporal_get_offset_nanoseconds(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let zone = zoned_time_zone_from_this(vm, this)?;
    Ok(Value::Number(zone.offset_nanoseconds.unwrap_or(0) as f64))
}

fn temporal_get_hours_in_day(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::ZonedDateTime(_) => Ok(Value::Number(24.0)),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn duration_accessor(
    vm: &mut VM,
    this: Value,
    select: fn(&TemporalDurationInfo) -> i64,
) -> VMValueResult {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::Duration(info) => Ok(Value::Number(select(&info) as f64)),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_get_duration_years(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    duration_accessor(vm, this, |d| d.years)
}

fn temporal_get_duration_months(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    duration_accessor(vm, this, |d| d.months)
}

fn temporal_get_duration_weeks(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    duration_accessor(vm, this, |d| d.weeks)
}

fn temporal_get_duration_days(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    duration_accessor(vm, this, |d| d.days)
}

fn temporal_get_duration_hours(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    duration_accessor(vm, this, |d| d.hours)
}

fn temporal_get_duration_minutes(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    duration_accessor(vm, this, |d| d.minutes)
}

fn temporal_get_duration_seconds(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    duration_accessor(vm, this, |d| d.seconds)
}

fn temporal_get_duration_milliseconds(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    duration_accessor(vm, this, |d| d.milliseconds)
}

fn temporal_get_duration_microseconds(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    duration_accessor(vm, this, |d| d.microseconds)
}

fn temporal_get_duration_nanoseconds(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    duration_accessor(vm, this, |d| d.nanoseconds)
}

fn temporal_get_duration_sign(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::Duration(info) => Ok(Value::Number(duration_sign(&info) as f64)),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_get_blank(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::Duration(info) => Ok(Value::Bool((duration_sign(&info) == 0) as u8)),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_to_string(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let kind = temporal_kind(vm, this)?;
    let options = args.get(0).copied().unwrap_or(Value::undefined());
    validate_temporal_to_string_options(vm, options, &kind)?;
    let text = match kind {
        TemporalObjectKind::Calendar(info) => info.identifier,
        TemporalObjectKind::Duration(info) => format_duration(&info),
        TemporalObjectKind::Instant(info) => format_instant(info.epoch_nanoseconds),
        TemporalObjectKind::PlainDate(info) => format_date(info.year, info.month, info.day),
        TemporalObjectKind::PlainDateTime(info) => format_date_time(&info),
        TemporalObjectKind::PlainMonthDay(info) => {
            format!("{}-{}", two_digits(info.month), two_digits(info.day))
        }
        TemporalObjectKind::PlainTime(info) => format_time(&info),
        TemporalObjectKind::PlainYearMonth(info) => {
            format!("{}-{}", format_year(info.year), two_digits(info.month))
        }
        TemporalObjectKind::TimeZone(info) => info.identifier,
        TemporalObjectKind::ZonedDateTime(info) => format_zoned_date_time(&info),
    };
    Ok(vm.factory.string(text))
}

fn temporal_value_of(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    temporal_kind(vm, this)?;
    Err(vm
        .current_context
        .error_type("use compare() or equals() to compare Temporal objects"))
}

fn temporal_equals(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let lhs = temporal_kind(vm, this)?;
    let rhs = args.get(0).copied().unwrap_or(Value::undefined());
    let result = match lhs {
        TemporalObjectKind::Calendar(info) => calendar_identifier(vm, rhs)? == info.identifier,
        TemporalObjectKind::Duration(info) => {
            let other = to_temporal_duration(vm, rhs)?;
            duration_equal(&info, &other)
        }
        TemporalObjectKind::Instant(info) => {
            let other = to_temporal_instant(vm, rhs)?;
            info.epoch_nanoseconds == other.epoch_nanoseconds
        }
        TemporalObjectKind::PlainDate(info) => {
            let other = to_temporal_date(vm, rhs)?;
            date_equal(&info, &other)
        }
        TemporalObjectKind::PlainDateTime(info) => {
            let other = to_temporal_date_time(vm, rhs)?;
            date_time_equal(&info, &other)
        }
        TemporalObjectKind::PlainMonthDay(info) => {
            let other = to_temporal_month_day(vm, rhs)?;
            info.month == other.month && info.day == other.day
        }
        TemporalObjectKind::PlainTime(info) => {
            let other = to_temporal_time(vm, rhs)?;
            time_equal(&info, &other)
        }
        TemporalObjectKind::PlainYearMonth(info) => {
            let other = to_temporal_year_month(vm, rhs)?;
            info.year == other.year && info.month == other.month
        }
        TemporalObjectKind::TimeZone(info) => {
            let other = time_zone_identifier(vm, rhs)?;
            info.identifier == other.identifier
        }
        TemporalObjectKind::ZonedDateTime(info) => {
            let other = to_temporal_zoned_date_time(vm, rhs)?;
            info.epoch_nanoseconds == other.epoch_nanoseconds
                && info.time_zone.identifier == other.time_zone.identifier
                && info.calendar == other.calendar
        }
    };
    Ok(Value::Bool(result as u8))
}

fn temporal_get_iso_fields(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::PlainDate(info) => {
            let calendar = calendar_slot_value(vm, &info.calendar, info.calendar_object);
            Ok(object_from_pairs(
                vm,
                &[
                    ("calendar", calendar),
                    ("isoDay", Value::Number(info.day as f64)),
                    ("isoMonth", Value::Number(info.month as f64)),
                    ("isoYear", Value::Number(info.year as f64)),
                ],
            ))
        }
        TemporalObjectKind::PlainDateTime(info) => {
            let calendar = calendar_slot_value(vm, &info.date.calendar, info.date.calendar_object);
            Ok(object_from_pairs(
                vm,
                &[
                    ("calendar", calendar),
                    ("isoDay", Value::Number(info.date.day as f64)),
                    ("isoHour", Value::Number(info.time.hour as f64)),
                    (
                        "isoMicrosecond",
                        Value::Number(info.time.microsecond as f64),
                    ),
                    (
                        "isoMillisecond",
                        Value::Number(info.time.millisecond as f64),
                    ),
                    ("isoMinute", Value::Number(info.time.minute as f64)),
                    ("isoMonth", Value::Number(info.date.month as f64)),
                    ("isoNanosecond", Value::Number(info.time.nanosecond as f64)),
                    ("isoSecond", Value::Number(info.time.second as f64)),
                    ("isoYear", Value::Number(info.date.year as f64)),
                ],
            ))
        }
        TemporalObjectKind::PlainMonthDay(info) => {
            let calendar = calendar_slot_value(vm, &info.calendar, info.calendar_object);
            Ok(object_from_pairs(
                vm,
                &[
                    ("calendar", calendar),
                    ("isoDay", Value::Number(info.day as f64)),
                    ("isoMonth", Value::Number(info.month as f64)),
                    ("isoYear", Value::Number(1972.0)),
                ],
            ))
        }
        TemporalObjectKind::PlainTime(info) => Ok(object_from_pairs(
            vm,
            &[
                ("isoHour", Value::Number(info.hour as f64)),
                ("isoMicrosecond", Value::Number(info.microsecond as f64)),
                ("isoMillisecond", Value::Number(info.millisecond as f64)),
                ("isoMinute", Value::Number(info.minute as f64)),
                ("isoNanosecond", Value::Number(info.nanosecond as f64)),
                ("isoSecond", Value::Number(info.second as f64)),
            ],
        )),
        TemporalObjectKind::PlainYearMonth(info) => {
            let calendar = calendar_slot_value(vm, &info.calendar, info.calendar_object);
            Ok(object_from_pairs(
                vm,
                &[
                    ("calendar", calendar),
                    ("isoDay", Value::Number(1.0)),
                    ("isoMonth", Value::Number(info.month as f64)),
                    ("isoYear", Value::Number(info.year as f64)),
                ],
            ))
        }
        TemporalObjectKind::ZonedDateTime(info) => {
            let date_time = zoned_date_time_to_plain_date_time_checked(vm, &info)?;
            let calendar = calendar_slot_value(vm, &info.calendar, info.calendar_object);
            let offset_nanoseconds = time_zone_offset_nanoseconds_for(
                vm,
                &info.time_zone,
                TemporalInstantInfo {
                    epoch_nanoseconds: info.epoch_nanoseconds,
                },
            )?;
            let offset = vm.factory.string(format_offset(offset_nanoseconds));
            let time_zone = time_zone_object(vm, &info.time_zone);
            Ok(object_from_pairs(
                vm,
                &[
                    ("calendar", calendar),
                    ("isoDay", Value::Number(date_time.date.day as f64)),
                    ("isoHour", Value::Number(date_time.time.hour as f64)),
                    (
                        "isoMicrosecond",
                        Value::Number(date_time.time.microsecond as f64),
                    ),
                    (
                        "isoMillisecond",
                        Value::Number(date_time.time.millisecond as f64),
                    ),
                    ("isoMinute", Value::Number(date_time.time.minute as f64)),
                    ("isoMonth", Value::Number(date_time.date.month as f64)),
                    (
                        "isoNanosecond",
                        Value::Number(date_time.time.nanosecond as f64),
                    ),
                    ("isoSecond", Value::Number(date_time.time.second as f64)),
                    ("isoYear", Value::Number(date_time.date.year as f64)),
                    ("offset", offset),
                    ("timeZone", time_zone),
                ],
            ))
        }
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_get_calendar(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let (id, object) = match temporal_kind(vm, this)? {
        TemporalObjectKind::PlainDate(info) => (info.calendar, info.calendar_object),
        TemporalObjectKind::PlainDateTime(info) => (info.date.calendar, info.date.calendar_object),
        TemporalObjectKind::PlainMonthDay(info) => (info.calendar, info.calendar_object),
        TemporalObjectKind::PlainYearMonth(info) => (info.calendar, info.calendar_object),
        TemporalObjectKind::ZonedDateTime(info) => (info.calendar, info.calendar_object),
        _ => return Err(temporal_brand_error(vm)),
    };
    Ok(calendar_slot_value(vm, &id, object))
}

fn temporal_get_time_zone(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::ZonedDateTime(info) => Ok(time_zone_object(vm, &info.time_zone)),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_to_plain_date(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let kind = temporal_kind(vm, this)?;
    let date = match kind {
        TemporalObjectKind::PlainDate(info) => info,
        TemporalObjectKind::PlainDateTime(info) => info.date,
        TemporalObjectKind::PlainYearMonth(info) => {
            let calendar = calendar_slot_value(vm, &info.calendar, info.calendar_object);
            let receiver_names = calendar_fields(vm, calendar, &["monthCode", "year"])?;
            let input_names = calendar_fields(vm, calendar, &["day"])?;
            let input = args.get(0).copied().unwrap_or(Value::undefined());
            let fields = iso_date_fields_object(vm, &receiver_names, info.year, info.month, 1);
            copy_present_fields(vm, fields, input, &input_names)?;
            if let Some(result) =
                call_custom_calendar_from_fields(vm, calendar, "dateFromFields", fields)?
            {
                to_temporal_date(vm, result)?
            } else {
                let year = require_i32_property(vm, fields, "year")?;
                let month = month_from_object(vm, fields)?;
                let day = require_u8_property(vm, fields, "day")?;
                validate_date(vm, year, month, day)?;
                TemporalDateInfo {
                    year,
                    month,
                    day,
                    calendar: info.calendar,
                    calendar_object: info.calendar_object,
                }
            }
        }
        TemporalObjectKind::PlainMonthDay(info) => {
            let calendar = calendar_slot_value(vm, &info.calendar, info.calendar_object);
            let receiver_names = calendar_fields(vm, calendar, &["day", "monthCode"])?;
            let input_names = calendar_fields(vm, calendar, &["year"])?;
            let input = args.get(0).copied().unwrap_or(Value::undefined());
            let fields = iso_date_fields_object(vm, &receiver_names, 1972, info.month, info.day);
            copy_present_fields(vm, fields, input, &input_names)?;
            if let Some(result) =
                call_custom_calendar_from_fields(vm, calendar, "dateFromFields", fields)?
            {
                to_temporal_date(vm, result)?
            } else {
                let year = require_i32_property(vm, fields, "year")?;
                let month = month_from_object(vm, fields)?;
                let day = require_u8_property(vm, fields, "day")?;
                validate_date(vm, year, month, day)?;
                TemporalDateInfo {
                    year,
                    month,
                    day,
                    calendar: info.calendar,
                    calendar_object: info.calendar_object,
                }
            }
        }
        TemporalObjectKind::ZonedDateTime(info) => {
            zoned_date_time_to_plain_date_time_checked(vm, &info)?.date
        }
        _ => return Err(temporal_brand_error(vm)),
    };
    Ok(temporal_object_by_name(
        vm,
        "PlainDate",
        TemporalObjectKind::PlainDate(date),
    ))
}

fn temporal_to_plain_time(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let time = match temporal_kind(vm, this)? {
        TemporalObjectKind::PlainTime(info) => info,
        TemporalObjectKind::PlainDateTime(info) => info.time,
        TemporalObjectKind::ZonedDateTime(info) => {
            zoned_date_time_to_plain_date_time_checked(vm, &info)?.time
        }
        _ => return Err(temporal_brand_error(vm)),
    };
    Ok(temporal_object_by_name(
        vm,
        "PlainTime",
        TemporalObjectKind::PlainTime(time),
    ))
}

fn temporal_to_plain_date_time(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let date_time = match temporal_kind(vm, this)? {
        TemporalObjectKind::PlainDate(info) => {
            let time = if args
                .get(0)
                .copied()
                .unwrap_or(Value::undefined())
                .is_undefined()
            {
                default_time()
            } else {
                to_temporal_time(vm, args[0])?
            };
            TemporalDateTimeInfo { date: info, time }
        }
        TemporalObjectKind::PlainTime(info) => {
            let date = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
            TemporalDateTimeInfo { date, time: info }
        }
        TemporalObjectKind::PlainDateTime(info) => info,
        TemporalObjectKind::ZonedDateTime(info) => {
            zoned_date_time_to_plain_date_time_checked(vm, &info)?
        }
        _ => return Err(temporal_brand_error(vm)),
    };
    Ok(temporal_object_by_name(
        vm,
        "PlainDateTime",
        TemporalObjectKind::PlainDateTime(date_time),
    ))
}

fn temporal_to_plain_year_month(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_from_this(vm, this)?;
    let calendar = calendar_slot_value(vm, &date.calendar, date.calendar_object);
    let field_names = calendar_fields(vm, calendar, &["monthCode", "year"])?;
    let fields = iso_date_fields_object(vm, &field_names, date.year, date.month, date.day);
    if let Some(result) =
        call_custom_calendar_from_fields(vm, calendar, "yearMonthFromFields", fields)?
    {
        let year_month = to_temporal_year_month(vm, result)?;
        return Ok(temporal_object_by_name(
            vm,
            "PlainYearMonth",
            TemporalObjectKind::PlainYearMonth(year_month),
        ));
    }
    let year = require_i32_property(vm, fields, "year")?;
    let month = month_from_object(vm, fields)?;
    validate_date(vm, year, month, 1)?;
    Ok(temporal_object_by_name(
        vm,
        "PlainYearMonth",
        TemporalObjectKind::PlainYearMonth(TemporalYearMonthInfo {
            year,
            month,
            calendar: date.calendar,
            calendar_object: date.calendar_object,
        }),
    ))
}

fn temporal_to_plain_month_day(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_from_this(vm, this)?;
    let calendar = calendar_slot_value(vm, &date.calendar, date.calendar_object);
    let field_names = calendar_fields(vm, calendar, &["day", "monthCode"])?;
    let fields = iso_date_fields_object(vm, &field_names, date.year, date.month, date.day);
    if let Some(result) =
        call_custom_calendar_from_fields(vm, calendar, "monthDayFromFields", fields)?
    {
        let month_day = to_temporal_month_day(vm, result)?;
        return Ok(temporal_object_by_name(
            vm,
            "PlainMonthDay",
            TemporalObjectKind::PlainMonthDay(month_day),
        ));
    }
    let month = month_from_object(vm, fields)?;
    let day = require_u8_property(vm, fields, "day")?;
    validate_date(vm, 1972, month, day)?;
    Ok(temporal_object_by_name(
        vm,
        "PlainMonthDay",
        TemporalObjectKind::PlainMonthDay(TemporalMonthDayInfo {
            month,
            day,
            calendar: date.calendar,
            calendar_object: date.calendar_object,
        }),
    ))
}

fn temporal_to_instant(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::ZonedDateTime(info) => Ok(temporal_object_by_name(
            vm,
            "Instant",
            TemporalObjectKind::Instant(TemporalInstantInfo {
                epoch_nanoseconds: info.epoch_nanoseconds,
            }),
        )),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_to_zoned_date_time(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    validate_temporal_options_object(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let item = args.get(0).copied().unwrap_or(Value::undefined());
    let zoned = match temporal_kind(vm, this)? {
        TemporalObjectKind::Instant(info) => {
            if !item.is_object() {
                return Err(vm
                    .current_context
                    .error_type("Temporal.Instant.toZonedDateTime requires object"));
            }
            let time_zone_value = get_property(vm, item, "timeZone")?;
            let time_zone = time_zone_identifier(vm, time_zone_value)?;
            let calendar_value = get_property(vm, item, "calendar")?;
            let (calendar, calendar_object) = calendar_identifier_and_object(vm, calendar_value)?;
            TemporalZonedDateTimeInfo {
                epoch_nanoseconds: info.epoch_nanoseconds,
                time_zone,
                calendar,
                calendar_object,
            }
        }
        TemporalObjectKind::PlainDate(date) => {
            let (time_zone, time) = plain_date_to_zoned_args(vm, item)?;
            zoned_from_date_time(vm, TemporalDateTimeInfo { date, time }, time_zone)?
        }
        TemporalObjectKind::PlainDateTime(date_time) => {
            let time_zone = time_zone_identifier(vm, item)?;
            zoned_from_date_time(vm, date_time, time_zone)?
        }
        TemporalObjectKind::PlainTime(time) => {
            if !item.is_object() {
                return Err(vm
                    .current_context
                    .error_type("Temporal.PlainTime.toZonedDateTime requires object"));
            }
            let time_zone_value = get_property(vm, item, "timeZone")?;
            let time_zone = time_zone_identifier(vm, time_zone_value)?;
            let plain_date = get_property(vm, item, "plainDate")?;
            let date = to_temporal_date(vm, plain_date)?;
            zoned_from_date_time(vm, TemporalDateTimeInfo { date, time }, time_zone)?
        }
        _ => return Err(temporal_brand_error(vm)),
    };
    Ok(temporal_object_by_name(
        vm,
        "ZonedDateTime",
        TemporalObjectKind::ZonedDateTime(zoned),
    ))
}

fn temporal_to_zoned_date_time_iso(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let time_zone = time_zone_identifier(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    match temporal_kind(vm, this)? {
        TemporalObjectKind::Instant(info) => Ok(temporal_object_by_name(
            vm,
            "ZonedDateTime",
            TemporalObjectKind::ZonedDateTime(TemporalZonedDateTimeInfo {
                epoch_nanoseconds: info.epoch_nanoseconds,
                time_zone,
                calendar: "iso8601".to_string(),
                calendar_object: None,
            }),
        )),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_with(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let fields = args.get(0).copied().unwrap_or(Value::undefined());
    let kind = temporal_kind(vm, this)?;
    validate_temporal_options_object(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    match kind {
        TemporalObjectKind::PlainDate(mut info) => {
            let calendar = calendar_slot_value(vm, &info.calendar, info.calendar_object);
            let field_names =
                calendar_fields(vm, calendar, &["day", "month", "monthCode", "year"])?;
            let merged = iso_date_fields_object(vm, &field_names, info.year, info.month, info.day);
            copy_present_fields(vm, merged, fields, &field_names)?;
            if let Some(result) =
                call_custom_calendar_from_fields(vm, calendar, "dateFromFields", merged)?
            {
                let date = to_temporal_date(vm, result)?;
                return Ok(temporal_object_by_name(
                    vm,
                    "PlainDate",
                    TemporalObjectKind::PlainDate(date),
                ));
            }
            patch_date_fields(vm, merged, &mut info)?;
            Ok(temporal_object_by_name(
                vm,
                "PlainDate",
                TemporalObjectKind::PlainDate(info),
            ))
        }
        TemporalObjectKind::PlainDateTime(mut info) => {
            let calendar = calendar_slot_value(vm, &info.date.calendar, info.date.calendar_object);
            let field_names =
                calendar_fields(vm, calendar, &["day", "month", "monthCode", "year"])?;
            let merged = iso_date_fields_object(
                vm,
                &field_names,
                info.date.year,
                info.date.month,
                info.date.day,
            );
            copy_present_fields(vm, merged, fields, &field_names)?;
            if let Some(result) =
                call_custom_calendar_from_fields(vm, calendar, "dateFromFields", merged)?
            {
                info.date = to_temporal_date(vm, result)?;
            } else {
                patch_date_fields(vm, merged, &mut info.date)?;
            }
            patch_time_fields(vm, fields, &mut info.time)?;
            Ok(temporal_object_by_name(
                vm,
                "PlainDateTime",
                TemporalObjectKind::PlainDateTime(info),
            ))
        }
        TemporalObjectKind::PlainTime(mut info) => {
            patch_time_fields(vm, fields, &mut info)?;
            Ok(temporal_object_by_name(
                vm,
                "PlainTime",
                TemporalObjectKind::PlainTime(info),
            ))
        }
        TemporalObjectKind::PlainYearMonth(mut info) => {
            let calendar = calendar_slot_value(vm, &info.calendar, info.calendar_object);
            let field_names = calendar_fields(vm, calendar, &["month", "monthCode", "year"])?;
            let merged = iso_date_fields_object(vm, &field_names, info.year, info.month, 1);
            copy_present_fields(vm, merged, fields, &field_names)?;
            if let Some(result) =
                call_custom_calendar_from_fields(vm, calendar, "yearMonthFromFields", merged)?
            {
                let year_month = to_temporal_year_month(vm, result)?;
                return Ok(temporal_object_by_name(
                    vm,
                    "PlainYearMonth",
                    TemporalObjectKind::PlainYearMonth(year_month),
                ));
            }
            patch_year_month_fields(vm, merged, &mut info)?;
            Ok(temporal_object_by_name(
                vm,
                "PlainYearMonth",
                TemporalObjectKind::PlainYearMonth(info),
            ))
        }
        TemporalObjectKind::PlainMonthDay(mut info) => {
            let calendar = calendar_slot_value(vm, &info.calendar, info.calendar_object);
            let field_names =
                calendar_fields(vm, calendar, &["day", "month", "monthCode", "year"])?;
            let merged = iso_date_fields_object(vm, &field_names, 1972, info.month, info.day);
            copy_present_fields(vm, merged, fields, &field_names)?;
            if let Some(result) =
                call_custom_calendar_from_fields(vm, calendar, "monthDayFromFields", merged)?
            {
                let month_day = to_temporal_month_day(vm, result)?;
                return Ok(temporal_object_by_name(
                    vm,
                    "PlainMonthDay",
                    TemporalObjectKind::PlainMonthDay(month_day),
                ));
            }
            patch_month_day_fields(vm, merged, &mut info)?;
            Ok(temporal_object_by_name(
                vm,
                "PlainMonthDay",
                TemporalObjectKind::PlainMonthDay(info),
            ))
        }
        TemporalObjectKind::ZonedDateTime(mut info) => {
            let mut date_time = zoned_date_time_to_plain_date_time_checked(vm, &info)?;
            let calendar =
                calendar_slot_value(vm, &date_time.date.calendar, date_time.date.calendar_object);
            let field_names =
                calendar_fields(vm, calendar, &["day", "month", "monthCode", "year"])?;
            let merged = iso_date_fields_object(
                vm,
                &field_names,
                date_time.date.year,
                date_time.date.month,
                date_time.date.day,
            );
            copy_present_fields(vm, merged, fields, &field_names)?;
            if let Some(result) =
                call_custom_calendar_from_fields(vm, calendar, "dateFromFields", merged)?
            {
                date_time.date = to_temporal_date(vm, result)?;
            } else {
                patch_date_fields(vm, merged, &mut date_time.date)?;
            }
            patch_time_fields(vm, fields, &mut date_time.time)?;
            info.epoch_nanoseconds = date_time_to_epoch_nanoseconds(&date_time)
                - info.time_zone.offset_nanoseconds.unwrap_or(0) as i128;
            info.calendar = date_time.date.calendar;
            info.calendar_object = date_time.date.calendar_object;
            Ok(temporal_object_by_name(
                vm,
                "ZonedDateTime",
                TemporalObjectKind::ZonedDateTime(info),
            ))
        }
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_with_calendar(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let calendar_value = args.get(0).copied().unwrap_or(Value::undefined());
    let (calendar, calendar_object) = calendar_identifier_and_object(vm, calendar_value)?;
    match temporal_kind(vm, this)? {
        TemporalObjectKind::PlainDate(mut info) => {
            info.calendar = calendar;
            info.calendar_object = calendar_object;
            Ok(temporal_object_by_name(
                vm,
                "PlainDate",
                TemporalObjectKind::PlainDate(info),
            ))
        }
        TemporalObjectKind::PlainDateTime(mut info) => {
            info.date.calendar = calendar;
            info.date.calendar_object = calendar_object;
            Ok(temporal_object_by_name(
                vm,
                "PlainDateTime",
                TemporalObjectKind::PlainDateTime(info),
            ))
        }
        TemporalObjectKind::PlainMonthDay(mut info) => {
            info.calendar = calendar;
            info.calendar_object = calendar_object;
            Ok(temporal_object_by_name(
                vm,
                "PlainMonthDay",
                TemporalObjectKind::PlainMonthDay(info),
            ))
        }
        TemporalObjectKind::PlainYearMonth(mut info) => {
            info.calendar = calendar;
            info.calendar_object = calendar_object;
            Ok(temporal_object_by_name(
                vm,
                "PlainYearMonth",
                TemporalObjectKind::PlainYearMonth(info),
            ))
        }
        TemporalObjectKind::ZonedDateTime(mut info) => {
            info.calendar = calendar;
            info.calendar_object = calendar_object;
            Ok(temporal_object_by_name(
                vm,
                "ZonedDateTime",
                TemporalObjectKind::ZonedDateTime(info),
            ))
        }
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_with_time_zone(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::ZonedDateTime(mut info) => {
            info.time_zone =
                time_zone_identifier(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
            Ok(temporal_object_by_name(
                vm,
                "ZonedDateTime",
                TemporalObjectKind::ZonedDateTime(info),
            ))
        }
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_with_plain_date(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let date = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    match temporal_kind(vm, this)? {
        TemporalObjectKind::PlainDateTime(mut info) => {
            info.date = date;
            Ok(temporal_object_by_name(
                vm,
                "PlainDateTime",
                TemporalObjectKind::PlainDateTime(info),
            ))
        }
        TemporalObjectKind::ZonedDateTime(mut info) => {
            let mut date_time = zoned_date_time_to_plain_date_time_checked(vm, &info)?;
            date_time.date = date;
            info.epoch_nanoseconds = date_time_to_epoch_nanoseconds(&date_time)
                - info.time_zone.offset_nanoseconds.unwrap_or(0) as i128;
            info.calendar = date_time.date.calendar;
            info.calendar_object = date_time.date.calendar_object;
            Ok(temporal_object_by_name(
                vm,
                "ZonedDateTime",
                TemporalObjectKind::ZonedDateTime(info),
            ))
        }
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_with_plain_time(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let time = if args
        .get(0)
        .copied()
        .unwrap_or(Value::undefined())
        .is_undefined()
    {
        default_time()
    } else {
        to_temporal_time(vm, args[0])?
    };
    match temporal_kind(vm, this)? {
        TemporalObjectKind::PlainDateTime(mut info) => {
            info.time = time;
            Ok(temporal_object_by_name(
                vm,
                "PlainDateTime",
                TemporalObjectKind::PlainDateTime(info),
            ))
        }
        TemporalObjectKind::ZonedDateTime(mut info) => {
            let mut date_time = zoned_date_time_to_plain_date_time(&info);
            date_time.time = time;
            info.epoch_nanoseconds = date_time_to_epoch_nanoseconds(&date_time)
                - info.time_zone.offset_nanoseconds.unwrap_or(0) as i128;
            Ok(temporal_object_by_name(
                vm,
                "ZonedDateTime",
                TemporalObjectKind::ZonedDateTime(info),
            ))
        }
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_duration_abs(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::Duration(info) => Ok(temporal_object_by_name(
            vm,
            "Duration",
            TemporalObjectKind::Duration(map_duration(info, i64::abs)),
        )),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_duration_negated(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::Duration(info) => Ok(temporal_object_by_name(
            vm,
            "Duration",
            TemporalObjectKind::Duration(map_duration(info, |value| -value)),
        )),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_add(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    temporal_add_subtract(vm, args, this, 1)
}

fn temporal_subtract(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    temporal_add_subtract(vm, args, this, -1)
}

fn temporal_add_subtract(vm: &mut VM, args: &[Value], this: Value, sign: i64) -> VMValueResult {
    let duration = to_temporal_duration(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    match temporal_kind(vm, this)? {
        TemporalObjectKind::Duration(info) => {
            let options = args.get(1).copied().unwrap_or(Value::undefined());
            validate_temporal_options_object(vm, options)?;
            validate_relative_to_option(vm, options)?;
            let mut result = add_duration(&info, &duration, sign);
            if result.years.signum() != 0
                && result.months.signum() != 0
                && result.years.signum() != result.months.signum()
            {
                let total_months = result.years * 12 + result.months;
                result.years = total_months / 12;
                result.months = total_months % 12;
            }
            validate_duration_sign(vm, &result)?;
            Ok(temporal_object_by_name(
                vm,
                "Duration",
                TemporalObjectKind::Duration(result),
            ))
        }
        TemporalObjectKind::PlainDate(mut info) => {
            validate_temporal_overflow_option(
                vm,
                args.get(1).copied().unwrap_or(Value::undefined()),
            )?;
            add_duration_to_date(vm, &mut info, &duration, sign)?;
            Ok(temporal_object_by_name(
                vm,
                "PlainDate",
                TemporalObjectKind::PlainDate(info),
            ))
        }
        TemporalObjectKind::PlainDateTime(mut info) => {
            validate_temporal_overflow_option(
                vm,
                args.get(1).copied().unwrap_or(Value::undefined()),
            )?;
            add_duration_to_date(vm, &mut info.date, &duration, sign)?;
            add_duration_to_time(&mut info.time, &duration, sign);
            Ok(temporal_object_by_name(
                vm,
                "PlainDateTime",
                TemporalObjectKind::PlainDateTime(info),
            ))
        }
        TemporalObjectKind::PlainYearMonth(info) => {
            validate_temporal_overflow_option(
                vm,
                args.get(1).copied().unwrap_or(Value::undefined()),
            )?;
            let mut date = TemporalDateInfo {
                year: info.year,
                month: info.month,
                day: 1,
                calendar: info.calendar,
                calendar_object: info.calendar_object,
            };
            let calendar = calendar_slot_value(vm, &date.calendar, date.calendar_object);
            let date_fields = iso_date_fields_object(
                vm,
                &string_list(&["year", "month", "monthCode", "day"]),
                date.year,
                date.month,
                date.day,
            );
            if let Some(result) =
                call_custom_calendar_from_fields(vm, calendar, "dateFromFields", date_fields)?
            {
                date = to_temporal_date(vm, result)?;
            }
            add_duration_to_date(vm, &mut date, &duration, sign)?;
            if sign < 0 {
                let date_fields = iso_date_fields_object(
                    vm,
                    &string_list(&["year", "month", "monthCode", "day"]),
                    date.year,
                    date.month,
                    date.day,
                );
                if let Some(result) =
                    call_custom_calendar_from_fields(vm, calendar, "dateFromFields", date_fields)?
                {
                    date = to_temporal_date(vm, result)?;
                }
            }
            let field_names = calendar_fields(vm, calendar, &["monthCode", "year"])?;
            let fields = iso_date_fields_object(vm, &field_names, date.year, date.month, date.day);
            if let Some(result) =
                call_custom_calendar_from_fields(vm, calendar, "yearMonthFromFields", fields)?
            {
                let year_month = to_temporal_year_month(vm, result)?;
                return Ok(temporal_object_by_name(
                    vm,
                    "PlainYearMonth",
                    TemporalObjectKind::PlainYearMonth(year_month),
                ));
            }
            let year = require_i32_property(vm, fields, "year")?;
            let month = month_from_object(vm, fields)?;
            validate_date(vm, year, month, 1)?;
            Ok(temporal_object_by_name(
                vm,
                "PlainYearMonth",
                TemporalObjectKind::PlainYearMonth(TemporalYearMonthInfo {
                    year,
                    month,
                    calendar: date.calendar,
                    calendar_object: date.calendar_object,
                }),
            ))
        }
        TemporalObjectKind::PlainTime(mut info) => {
            add_duration_to_time(&mut info, &duration, sign);
            Ok(temporal_object_by_name(
                vm,
                "PlainTime",
                TemporalObjectKind::PlainTime(info),
            ))
        }
        TemporalObjectKind::Instant(mut info) => {
            info.epoch_nanoseconds += duration_total_nanoseconds(&duration) * sign as i128;
            Ok(temporal_object_by_name(
                vm,
                "Instant",
                TemporalObjectKind::Instant(info),
            ))
        }
        TemporalObjectKind::ZonedDateTime(mut info) => {
            validate_temporal_options_object(
                vm,
                args.get(1).copied().unwrap_or(Value::undefined()),
            )?;
            info.epoch_nanoseconds += duration_total_nanoseconds(&duration) * sign as i128;
            Ok(temporal_object_by_name(
                vm,
                "ZonedDateTime",
                TemporalObjectKind::ZonedDateTime(info),
            ))
        }
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_until(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    temporal_difference(vm, args, this, 1)
}

fn temporal_since(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    temporal_difference(vm, args, this, -1)
}

fn temporal_difference(vm: &mut VM, args: &[Value], this: Value, sign: i64) -> VMValueResult {
    let other = args.get(0).copied().unwrap_or(Value::undefined());
    let kind = temporal_kind(vm, this)?;
    let options = args.get(1).copied().unwrap_or(Value::undefined());
    let (largest_unit, smallest_unit, rounding_increment, rounding_mode) =
        temporal_difference_settings(vm, options)?;
    let default_largest_unit = match &kind {
        TemporalObjectKind::Instant(_) => "second",
        TemporalObjectKind::PlainTime(_) => "hour",
        TemporalObjectKind::PlainYearMonth(_) => "month",
        _ => "day",
    };
    let duration = match kind {
        TemporalObjectKind::PlainDate(info) => {
            let other = to_temporal_date(vm, other)?;
            duration_from_days((iso_date_key(&other) - iso_date_key(&info)) * sign)
        }
        TemporalObjectKind::PlainDateTime(info) => {
            let other = to_temporal_date_time(vm, other)?;
            duration_from_nanoseconds(
                (date_time_to_epoch_nanoseconds(&other) - date_time_to_epoch_nanoseconds(&info))
                    * sign as i128,
            )
        }
        TemporalObjectKind::PlainTime(info) => {
            let other = to_temporal_time(vm, other)?;
            duration_from_nanoseconds(
                (time_nanoseconds(&other) - time_nanoseconds(&info)) * sign as i128,
            )
        }
        TemporalObjectKind::PlainYearMonth(info) => {
            let calendar = calendar_slot_value(vm, &info.calendar, info.calendar_object);
            let _ = calendar_fields(vm, calendar, &["monthCode", "year"])?;
            let this_fields = iso_date_fields_object(
                vm,
                &string_list(&["year", "month", "monthCode", "day"]),
                info.year,
                info.month,
                1,
            );
            let _ = call_custom_calendar_from_fields(vm, calendar, "dateFromFields", this_fields)?;
            let other = to_temporal_year_month(vm, other)?;
            let other_fields = iso_date_fields_object(
                vm,
                &string_list(&["year", "month", "monthCode", "day"]),
                other.year,
                other.month,
                1,
            );
            let _ = call_custom_calendar_from_fields(vm, calendar, "dateFromFields", other_fields)?;
            let months = (year_month_key(&other) - year_month_key(&info)) * sign;
            TemporalDurationInfo {
                years: months / 12,
                months: months % 12,
                weeks: 0,
                days: 0,
                hours: 0,
                minutes: 0,
                seconds: 0,
                milliseconds: 0,
                microseconds: 0,
                nanoseconds: 0,
            }
        }
        TemporalObjectKind::Instant(info) => {
            let other = to_temporal_instant(vm, other)?;
            duration_from_nanoseconds(
                (other.epoch_nanoseconds - info.epoch_nanoseconds) * sign as i128,
            )
        }
        TemporalObjectKind::ZonedDateTime(info) => {
            let other = to_temporal_zoned_date_time(vm, other)?;
            duration_from_nanoseconds(
                (other.epoch_nanoseconds - info.epoch_nanoseconds) * sign as i128,
            )
        }
        _ => return Err(temporal_brand_error(vm)),
    };
    let duration = if let Some(unit) = smallest_unit {
        round_duration(duration, &unit, rounding_increment, &rounding_mode)
    } else {
        duration
    };
    let largest_unit = largest_unit
        .filter(|unit| unit != "auto")
        .unwrap_or_else(|| default_largest_unit.to_string());
    let duration = balance_duration_to_largest_unit(duration, &largest_unit);
    Ok(temporal_object_by_name(
        vm,
        "Duration",
        TemporalObjectKind::Duration(duration),
    ))
}

fn temporal_round(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let options = args.get(0).copied().unwrap_or(Value::undefined());
    if options.is_undefined() {
        return Err(vm
            .current_context
            .error_type("Temporal round options required"));
    }
    if !options.is_string() {
        validate_temporal_options_object(vm, options)?;
    }
    match temporal_kind(vm, this)? {
        TemporalObjectKind::Duration(info) => {
            validate_relative_to_option(vm, options)?;
            let unit = temporal_unit_from_options(vm, options, "smallestUnit")?
                .or_else(|| {
                    temporal_unit_from_options(vm, options, "largestUnit")
                        .ok()
                        .flatten()
                })
                .ok_or_else(|| {
                    vm.current_context
                        .error_range("missing Temporal rounding unit")
                })?;
            let increment = temporal_rounding_increment(vm, options)?;
            let mode = temporal_rounding_mode(vm, options)?;
            let result = round_duration(info, &unit, increment, &mode);
            Ok(temporal_object_by_name(
                vm,
                "Duration",
                TemporalObjectKind::Duration(result),
            ))
        }
        TemporalObjectKind::Instant(mut info) => {
            let unit =
                temporal_unit_from_options(vm, options, "smallestUnit")?.ok_or_else(|| {
                    vm.current_context
                        .error_range("missing Temporal rounding unit")
                })?;
            let increment = temporal_rounding_increment(vm, options)?;
            let mode = temporal_rounding_mode(vm, options)?;
            if let Some(ns) = unit_nanoseconds(&unit) {
                info.epoch_nanoseconds =
                    round_to_increment(info.epoch_nanoseconds, ns * increment as i128, &mode);
            }
            Ok(temporal_object_by_name(
                vm,
                "Instant",
                TemporalObjectKind::Instant(info),
            ))
        }
        TemporalObjectKind::PlainTime(mut info) => {
            let unit =
                temporal_unit_from_options(vm, options, "smallestUnit")?.ok_or_else(|| {
                    vm.current_context
                        .error_range("missing Temporal rounding unit")
                })?;
            let increment = temporal_rounding_increment(vm, options)?;
            let mode = temporal_rounding_mode(vm, options)?;
            info = round_time(info, &unit, increment, &mode);
            Ok(temporal_object_by_name(
                vm,
                "PlainTime",
                TemporalObjectKind::PlainTime(info),
            ))
        }
        TemporalObjectKind::PlainDateTime(mut info) => {
            let unit =
                temporal_unit_from_options(vm, options, "smallestUnit")?.ok_or_else(|| {
                    vm.current_context
                        .error_range("missing Temporal rounding unit")
                })?;
            let increment = temporal_rounding_increment(vm, options)?;
            let mode = temporal_rounding_mode(vm, options)?;
            info.time = round_time(info.time, &unit, increment, &mode);
            Ok(temporal_object_by_name(
                vm,
                "PlainDateTime",
                TemporalObjectKind::PlainDateTime(info),
            ))
        }
        TemporalObjectKind::ZonedDateTime(mut info) => {
            let unit =
                temporal_unit_from_options(vm, options, "smallestUnit")?.ok_or_else(|| {
                    vm.current_context
                        .error_range("missing Temporal rounding unit")
                })?;
            let increment = temporal_rounding_increment(vm, options)?;
            let mode = temporal_rounding_mode(vm, options)?;
            if let Some(ns) = unit_nanoseconds(&unit) {
                info.epoch_nanoseconds =
                    round_to_increment(info.epoch_nanoseconds, ns * increment as i128, &mode);
            }
            Ok(temporal_object_by_name(
                vm,
                "ZonedDateTime",
                TemporalObjectKind::ZonedDateTime(info),
            ))
        }
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_duration_total(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let duration = match temporal_kind(vm, this)? {
        TemporalObjectKind::Duration(info) => info,
        _ => return Err(temporal_brand_error(vm)),
    };
    let options = args.get(0).copied().unwrap_or(Value::undefined());
    if options.is_undefined() {
        return Err(vm
            .current_context
            .error_type("Temporal total options required"));
    }
    if !options.is_string() {
        validate_temporal_options_object(vm, options)?;
    }
    validate_relative_to_option(vm, options)?;
    let unit = temporal_unit_from_options(vm, options, "unit")?.ok_or_else(|| {
        vm.current_context
            .error_range("missing Temporal total unit")
    })?;
    let value = match unit.as_str() {
        "year" => duration.years as f64 + duration.months as f64 / 12.0,
        "month" => duration.years as f64 * 12.0 + duration.months as f64,
        "week" => {
            duration.weeks as f64
                + (duration.days as f64
                    + duration_time_nanoseconds(&duration) as f64 / 86_400_000_000_000.0)
                    / 7.0
        }
        "day" => {
            (duration.weeks * 7 + duration.days) as f64
                + duration_time_nanoseconds(&duration) as f64 / 86_400_000_000_000.0
        }
        "hour" => duration_total_nanoseconds(&duration) as f64 / 3_600_000_000_000.0,
        "minute" => duration_total_nanoseconds(&duration) as f64 / 60_000_000_000.0,
        "second" => duration_total_nanoseconds(&duration) as f64 / 1_000_000_000.0,
        "millisecond" => duration_total_nanoseconds(&duration) as f64 / 1_000_000.0,
        "microsecond" => duration_total_nanoseconds(&duration) as f64 / 1_000.0,
        "nanosecond" => duration_total_nanoseconds(&duration) as f64,
        _ => return Err(vm.current_context.error_range("invalid Temporal unit")),
    };
    Ok(Value::Number(value))
}

fn temporal_calendar_year(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    Ok(Value::Number(
        to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?.year as f64,
    ))
}

fn temporal_calendar_month(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    Ok(Value::Number(
        to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?.month as f64,
    ))
}

fn temporal_calendar_day(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    Ok(Value::Number(
        to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?.day as f64,
    ))
}

fn temporal_calendar_month_code(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    let date = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(vm.factory.string(format_month_code(date.month)))
}

fn temporal_calendar_day_of_week(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    let date = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Number(
        day_of_week(date.year, date.month, date.day) as f64
    ))
}

fn temporal_calendar_day_of_year(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    let date = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Number(
        day_of_year(date.year, date.month, date.day) as f64
    ))
}

fn temporal_calendar_days_in_month(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    let date = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Number(days_in_month(date.year, date.month) as f64))
}

fn temporal_calendar_days_in_year(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    let date = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Number(days_in_year(date.year) as f64))
}

fn temporal_calendar_in_leap_year(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    let date = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Bool(is_leap_year(date.year) as u8))
}

fn temporal_calendar_days_in_week(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    let _ = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Number(7.0))
}

fn temporal_calendar_months_in_year(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    let value = args.get(0).copied().unwrap_or(Value::undefined());
    if !matches!(
        temporal_kind(vm, value),
        Ok(TemporalObjectKind::PlainYearMonth(_))
    ) {
        let _ = to_temporal_date(vm, value)?;
    }
    Ok(Value::Number(12.0))
}

fn temporal_calendar_era(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    let _ = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::undefined())
}

fn temporal_calendar_week_of_year(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    let date = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Number(iso_week_fields(&date).0 as f64))
}

fn temporal_calendar_year_of_week(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    let date = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(Value::Number(iso_week_fields(&date).1 as f64))
}

fn temporal_calendar_fields(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    let fields = args.get(0).copied().unwrap_or(Value::undefined());
    if fields.is_undefined() {
        return Ok(vm.factory.array(Vec::new()));
    }
    if !fields.is_object() {
        return Err(vm
            .current_context
            .error_type("calendar fields must be iterable"));
    }
    Ok(fields)
}

fn temporal_calendar_merge_fields(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    let fields = args.get(0).copied().unwrap_or(Value::undefined());
    let additional = args.get(1).copied().unwrap_or(Value::undefined());
    if !fields.is_object() || !additional.is_object() {
        return Err(vm
            .current_context
            .error_type("calendar fields must be objects"));
    }
    let result = ordinary_object(&mut vm.factory);
    for name in ["year", "month", "monthCode", "day"] {
        let value = get_property(vm, fields, name)?;
        if !value.is_undefined() {
            result.set_property(name, value);
        }
    }
    for name in ["year", "month", "monthCode", "day"] {
        let value = get_property(vm, additional, name)?;
        if !value.is_undefined() {
            result.set_property(name, value);
        }
    }
    Ok(result)
}

fn temporal_calendar_date_add(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    validate_temporal_overflow_option(vm, args.get(2).copied().unwrap_or(Value::undefined()))?;
    let mut date = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let duration = to_temporal_duration(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    add_duration_to_date(vm, &mut date, &duration, 1)?;
    Ok(temporal_object_by_name(
        vm,
        "PlainDate",
        TemporalObjectKind::PlainDate(date),
    ))
}

fn temporal_calendar_date_until(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    validate_temporal_options_object(vm, args.get(2).copied().unwrap_or(Value::undefined()))?;
    let one = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let two = to_temporal_date(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    Ok(temporal_object_by_name(
        vm,
        "Duration",
        TemporalObjectKind::Duration(duration_from_days(iso_date_key(&two) - iso_date_key(&one))),
    ))
}

fn temporal_calendar_date_from_fields(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    ensure_calendar(vm, this)?;
    validate_temporal_overflow_option(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let date = to_temporal_date(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(temporal_object_by_name(
        vm,
        "PlainDate",
        TemporalObjectKind::PlainDate(date),
    ))
}

fn temporal_calendar_year_month_from_fields(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    ensure_calendar(vm, this)?;
    validate_temporal_overflow_option(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let year_month =
        to_temporal_year_month(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(temporal_object_by_name(
        vm,
        "PlainYearMonth",
        TemporalObjectKind::PlainYearMonth(year_month),
    ))
}

fn temporal_calendar_month_day_from_fields(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    ensure_calendar(vm, this)?;
    validate_temporal_overflow_option(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let month_day = to_temporal_month_day(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    Ok(temporal_object_by_name(
        vm,
        "PlainMonthDay",
        TemporalObjectKind::PlainMonthDay(month_day),
    ))
}

fn temporal_time_zone_get_offset_nanoseconds_for(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    let instant = to_temporal_instant(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    match temporal_kind(vm, this)? {
        TemporalObjectKind::TimeZone(info) => Ok(Value::Number(time_zone_offset_nanoseconds_for(
            vm, &info, instant,
        )? as f64)),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_time_zone_get_offset_string_for(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    let instant = to_temporal_instant(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    match temporal_kind(vm, this)? {
        TemporalObjectKind::TimeZone(info) => {
            let offset = time_zone_offset_nanoseconds_for(vm, &info, instant)?;
            Ok(vm.factory.string(format_offset(offset)))
        }
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_time_zone_get_plain_date_time_for(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    let zone = match temporal_kind(vm, this)? {
        TemporalObjectKind::TimeZone(info) => info,
        _ => return Err(temporal_brand_error(vm)),
    };
    let instant = to_temporal_instant(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let epoch_nanoseconds = instant.epoch_nanoseconds;
    let offset = time_zone_offset_nanoseconds_for(vm, &zone, instant)?;
    let date_time = epoch_to_date_time(
        epoch_nanoseconds + offset as i128,
        "iso8601".to_string(),
        None,
    );
    Ok(temporal_object_by_name(
        vm,
        "PlainDateTime",
        TemporalObjectKind::PlainDateTime(date_time),
    ))
}

fn temporal_time_zone_get_instant_for(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let zone = match temporal_kind(vm, this)? {
        TemporalObjectKind::TimeZone(info) => info,
        _ => return Err(temporal_brand_error(vm)),
    };
    validate_temporal_options_object(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    let date_time = to_temporal_date_time(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let ns =
        date_time_to_epoch_nanoseconds(&date_time) - zone.offset_nanoseconds.unwrap_or(0) as i128;
    Ok(temporal_object_by_name(
        vm,
        "Instant",
        TemporalObjectKind::Instant(TemporalInstantInfo {
            epoch_nanoseconds: ns,
        }),
    ))
}

fn temporal_time_zone_get_possible_instants_for(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    let instant = temporal_time_zone_get_instant_for(vm, args, this)?;
    Ok(vm.factory.array(vec![Property::new_data_simple(instant)]))
}

fn temporal_time_zone_get_transition(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::TimeZone(_) => {
            let _ = to_temporal_instant(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
            Ok(Value::null())
        }
        _ => Err(temporal_brand_error(vm)),
    }
}

fn temporal_zoned_date_time_start_of_day(
    vm: &mut VM,
    _args: &[Value],
    this: Value,
) -> VMValueResult {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::ZonedDateTime(mut info) => {
            let mut date_time = zoned_date_time_to_plain_date_time(&info);
            date_time.time = default_time();
            info.epoch_nanoseconds = date_time_to_epoch_nanoseconds(&date_time)
                - info.time_zone.offset_nanoseconds.unwrap_or(0) as i128;
            Ok(temporal_object_by_name(
                vm,
                "ZonedDateTime",
                TemporalObjectKind::ZonedDateTime(info),
            ))
        }
        _ => Err(temporal_brand_error(vm)),
    }
}

fn ensure_constructor(vm: &mut VM) -> Result<(), RuntimeError> {
    if vm.builtin_constructor_call {
        Ok(())
    } else {
        Err(vm
            .current_context
            .error_type("Temporal constructor requires new"))
    }
}

fn set_temporal(this: Value, kind: TemporalObjectKind) {
    this.get_object_info().kind = ObjectKind::Temporal(TemporalObjectInfo { kind });
}

fn temporal_kind(vm: &mut VM, value: Value) -> Result<TemporalObjectKind, RuntimeError> {
    if let Value::Object(info) = value {
        if let ObjectKind::Temporal(ref temporal) = unsafe { &*info }.kind {
            return Ok(temporal.kind.clone());
        }
    }
    Err(temporal_brand_error(vm))
}

fn temporal_brand_error(vm: &mut VM) -> RuntimeError {
    vm.current_context.error_type("invalid Temporal receiver")
}

fn temporal_type_error(vm: &mut VM) -> RuntimeError {
    vm.current_context
        .error_type("Temporal operation not implemented")
}

fn temporal_unimplemented(vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    Err(temporal_type_error(vm))
}

fn temporal_object_from_constructor(
    vm: &mut VM,
    _constructor: Value,
    kind: TemporalObjectKind,
) -> Value {
    let name = match kind {
        TemporalObjectKind::Calendar(_) => "Calendar",
        TemporalObjectKind::Duration(_) => "Duration",
        TemporalObjectKind::Instant(_) => "Instant",
        TemporalObjectKind::PlainDate(_) => "PlainDate",
        TemporalObjectKind::PlainDateTime(_) => "PlainDateTime",
        TemporalObjectKind::PlainMonthDay(_) => "PlainMonthDay",
        TemporalObjectKind::PlainTime(_) => "PlainTime",
        TemporalObjectKind::PlainYearMonth(_) => "PlainYearMonth",
        TemporalObjectKind::TimeZone(_) => "TimeZone",
        TemporalObjectKind::ZonedDateTime(_) => "ZonedDateTime",
    };
    temporal_object_by_name(vm, name, kind)
}

fn temporal_object_by_name(vm: &mut VM, name: &str, kind: TemporalObjectKind) -> Value {
    let prototype = global_temporal_constructor(vm, name)
        .map(|constructor| constructor.get_property("prototype"))
        .filter(|prototype| prototype.is_object())
        .unwrap_or(vm.factory.object_prototypes.object);
    temporal_object_with_prototype(vm, prototype, kind)
}

fn temporal_object_with_prototype(
    vm: &mut VM,
    prototype: Value,
    kind: TemporalObjectKind,
) -> Value {
    Value::Object(vm.factory.alloc(Object {
        kind: ObjectKind::Temporal(TemporalObjectInfo { kind }),
        prototype,
        property: FxHashMap::default(),
        property_order: Vec::new(),
        private_elements: FxHashMap::default(),
        sym_property: FxHashMap::default(),
        sym_property_order: Vec::new(),
        extensible: true,
    }))
}

fn global_temporal_constructor(vm: &mut VM, name: &str) -> Option<Value> {
    let mut env_ref = vm.current_context.lexical_environment;
    loop {
        let env = unsafe { &*env_ref.0 };
        if let EnvironmentRecord::Global(global) = env.record {
            let temporal = global.get_property("Temporal");
            let constructor = temporal.get_property(name);
            if constructor.is_object() {
                return Some(constructor);
            }
        }
        match env.outer {
            Some(outer) => env_ref = outer,
            None => return None,
        }
    }
}

fn object_from_pairs(vm: &mut VM, pairs: &[(&str, Value)]) -> Value {
    let object = ordinary_object(&mut vm.factory);
    for (name, value) in pairs {
        insert_data_property(object, name, *value);
    }
    object
}

fn get_property(vm: &mut VM, object: Value, name: &str) -> Result<Value, RuntimeError> {
    let key = vm.factory.string(name.to_string());
    vm.get_property_by_value(object, key)
}

fn to_integer_arg(
    vm: &mut VM,
    args: &[Value],
    index: usize,
    default: i64,
) -> Result<i64, RuntimeError> {
    let value = args.get(index).copied().unwrap_or(Value::undefined());
    to_integer(vm, value, default)
}

fn to_i32_arg(
    vm: &mut VM,
    args: &[Value],
    index: usize,
    default: i32,
) -> Result<i32, RuntimeError> {
    let value = args.get(index).copied().unwrap_or(Value::undefined());
    to_i32(vm, value, default)
}

fn to_u8_arg(vm: &mut VM, args: &[Value], index: usize, default: u8) -> Result<u8, RuntimeError> {
    let value = args.get(index).copied().unwrap_or(Value::undefined());
    to_u8(vm, value, default)
}

fn to_integer(vm: &mut VM, value: Value, default: i64) -> Result<i64, RuntimeError> {
    if value.is_undefined() {
        return Ok(default);
    }
    let number = to_number(vm, value)?;
    if !number.is_finite() || number.fract() != 0.0 {
        return Err(vm.current_context.error_range("invalid Temporal integer"));
    }
    Ok(number as i64)
}

fn to_i32(vm: &mut VM, value: Value, default: i32) -> Result<i32, RuntimeError> {
    let integer = to_integer(vm, value, default as i64)?;
    if integer < i32::MIN as i64 || integer > i32::MAX as i64 {
        return Err(vm.current_context.error_range("invalid Temporal integer"));
    }
    Ok(integer as i32)
}

fn to_u8(vm: &mut VM, value: Value, default: u8) -> Result<u8, RuntimeError> {
    let integer = to_integer(vm, value, default as i64)?;
    if integer < 0 || integer > u8::MAX as i64 {
        return Err(vm.current_context.error_range("invalid Temporal integer"));
    }
    Ok(integer as u8)
}

fn to_bigint_i128(vm: &mut VM, value: Value) -> Result<i128, RuntimeError> {
    value
        .bigint_decimal()
        .and_then(|decimal| decimal.parse::<i128>().ok())
        .ok_or_else(|| {
            vm.current_context
                .error_type("Temporal Instant requires BigInt")
        })
}

fn read_time_args(
    vm: &mut VM,
    args: &[Value],
    start: usize,
) -> Result<TemporalTimeInfo, RuntimeError> {
    let time = TemporalTimeInfo {
        hour: to_u8_arg(vm, args, start, 0)?,
        minute: to_u8_arg(vm, args, start + 1, 0)?,
        second: to_u8_arg(vm, args, start + 2, 0)?,
        millisecond: to_integer_arg(vm, args, start + 3, 0)? as u16,
        microsecond: to_integer_arg(vm, args, start + 4, 0)? as u16,
        nanosecond: to_integer_arg(vm, args, start + 5, 0)? as u16,
    };
    validate_time(vm, &time)?;
    Ok(time)
}

fn default_time() -> TemporalTimeInfo {
    TemporalTimeInfo {
        hour: 0,
        minute: 0,
        second: 0,
        millisecond: 0,
        microsecond: 0,
        nanosecond: 0,
    }
}

fn calendar_identifier(vm: &mut VM, value: Value) -> Result<String, RuntimeError> {
    if value.is_undefined() {
        return Ok("iso8601".to_string());
    }
    if value.is_string() {
        let text = value.into_str();
        if let Some(identifier) = builtin_calendar_identifier(text) {
            return Ok(identifier);
        }
        if looks_like_temporal_date_time_string(text) {
            if let Some(identifier) = calendar_annotation_identifier(text) {
                if let Some(identifier) = builtin_calendar_identifier(identifier) {
                    return Ok(identifier);
                }
                return Err(vm
                    .current_context
                    .error_range("invalid calendar identifier"));
            }
            return Ok("iso8601".to_string());
        }
        return Err(vm
            .current_context
            .error_range("invalid calendar identifier"));
    }
    match temporal_kind(vm, value) {
        Ok(TemporalObjectKind::Calendar(info)) => return Ok(info.identifier),
        Ok(TemporalObjectKind::PlainDate(info)) => return Ok(info.calendar),
        Ok(TemporalObjectKind::PlainDateTime(info)) => return Ok(info.date.calendar),
        Ok(TemporalObjectKind::PlainMonthDay(info)) => return Ok(info.calendar),
        Ok(TemporalObjectKind::PlainYearMonth(info)) => return Ok(info.calendar),
        Ok(TemporalObjectKind::ZonedDateTime(info)) => return Ok(info.calendar),
        Ok(_) => {
            return Err(vm.current_context.error_type("invalid calendar"));
        }
        _ => {}
    }
    if value.is_object() {
        let id = get_property(vm, value, "id")?;
        if id.is_string() {
            return Ok(id.into_str().to_string());
        }
        let calendar = get_property(vm, value, "calendar")?;
        if !calendar.is_undefined() {
            return calendar_identifier(vm, calendar);
        }
    }
    Err(vm.current_context.error_type("invalid calendar"))
}

fn calendar_identifier_and_object(
    vm: &mut VM,
    value: Value,
) -> Result<(String, Option<Value>), RuntimeError> {
    match temporal_kind(vm, value) {
        Ok(TemporalObjectKind::Calendar(info)) => return Ok((info.identifier, Some(value))),
        Ok(TemporalObjectKind::PlainDate(info)) => {
            return Ok((info.calendar, info.calendar_object))
        }
        Ok(TemporalObjectKind::PlainDateTime(info)) => {
            return Ok((info.date.calendar, info.date.calendar_object))
        }
        Ok(TemporalObjectKind::PlainMonthDay(info)) => {
            return Ok((info.calendar, info.calendar_object))
        }
        Ok(TemporalObjectKind::PlainYearMonth(info)) => {
            return Ok((info.calendar, info.calendar_object))
        }
        Ok(TemporalObjectKind::ZonedDateTime(info)) => {
            return Ok((info.calendar, info.calendar_object))
        }
        Ok(_) => return Err(vm.current_context.error_type("invalid calendar")),
        _ => {}
    }

    let identifier = calendar_identifier(vm, value)?;
    let object = if value.is_object() && !value.is_undefined() {
        Some(value)
    } else {
        None
    };
    Ok((identifier, object))
}

fn time_zone_identifier(vm: &mut VM, value: Value) -> Result<TemporalTimeZoneInfo, RuntimeError> {
    if let Ok(TemporalObjectKind::TimeZone(info)) = temporal_kind(vm, value) {
        return Ok(info);
    }
    match temporal_kind(vm, value) {
        Ok(TemporalObjectKind::ZonedDateTime(info)) => return Ok(info.time_zone),
        Ok(_) => return Err(vm.current_context.error_type("invalid time zone")),
        _ => {}
    }
    if value.is_string() {
        let text = value.into_str();
        if text == "UTC" || text.eq_ignore_ascii_case("utc") {
            return Ok(TemporalTimeZoneInfo {
                identifier: "UTC".to_string(),
                offset_nanoseconds: Some(0),
                object: None,
            });
        }
        if let Some(offset) = parse_offset_nanoseconds(text) {
            return Ok(TemporalTimeZoneInfo {
                identifier: text.to_string(),
                offset_nanoseconds: Some(offset),
                object: None,
            });
        }
        if !text.is_empty() && text.contains('/') {
            return Ok(TemporalTimeZoneInfo {
                identifier: text.to_string(),
                offset_nanoseconds: Some(0),
                object: None,
            });
        }
        return Err(vm
            .current_context
            .error_range("invalid time zone identifier"));
    }
    if value.is_object() {
        let time_zone = get_property(vm, value, "timeZone")?;
        if !time_zone.is_undefined() {
            return time_zone_identifier(vm, time_zone);
        }
        let id = get_property(vm, value, "id")?;
        if !id.is_undefined() {
            let mut time_zone = time_zone_identifier(vm, id)?;
            time_zone.object = Some(value);
            return Ok(time_zone);
        }
    }
    Err(vm.current_context.error_type("invalid time zone"))
}

fn to_temporal_date(vm: &mut VM, value: Value) -> Result<TemporalDateInfo, RuntimeError> {
    match temporal_kind(vm, value) {
        Ok(TemporalObjectKind::PlainDate(info)) => return Ok(info),
        Ok(TemporalObjectKind::PlainDateTime(info)) => return Ok(info.date),
        Ok(TemporalObjectKind::PlainMonthDay(info)) => {
            return Ok(TemporalDateInfo {
                year: 1972,
                month: info.month,
                day: info.day,
                calendar: info.calendar,
                calendar_object: info.calendar_object,
            })
        }
        Ok(TemporalObjectKind::ZonedDateTime(info)) => {
            return Ok(zoned_date_time_to_plain_date_time_checked(vm, &info)?.date)
        }
        _ => {}
    }
    if value.is_string() {
        return parse_plain_date(vm, value.into_str());
    }
    if value.is_object() {
        let calendar_value = get_property(vm, value, "calendar")?;
        let field_names =
            calendar_fields(vm, calendar_value, &["day", "month", "monthCode", "year"])?;
        let fields = prepare_temporal_fields(vm, value, &field_names)?;
        if let Some(result) =
            call_custom_calendar_from_fields(vm, calendar_value, "dateFromFields", fields)?
        {
            return to_temporal_date(vm, result);
        }
        let year = require_i32_property(vm, fields, "year")?;
        let month = month_from_object(vm, fields)?;
        let day = require_u8_property(vm, fields, "day")?;
        let (calendar, calendar_object) = calendar_identifier_and_object(vm, calendar_value)?;
        validate_date(vm, year, month, day)?;
        return Ok(TemporalDateInfo {
            year,
            month,
            day,
            calendar,
            calendar_object,
        });
    }
    Err(vm.current_context.error_type("invalid Temporal date"))
}

fn to_temporal_time(vm: &mut VM, value: Value) -> Result<TemporalTimeInfo, RuntimeError> {
    to_temporal_time_inner(vm, value, false)
}

fn to_temporal_time_inner(
    vm: &mut VM,
    value: Value,
    allow_empty_property_bag: bool,
) -> Result<TemporalTimeInfo, RuntimeError> {
    match temporal_kind(vm, value) {
        Ok(TemporalObjectKind::PlainTime(info)) => return Ok(info),
        Ok(TemporalObjectKind::PlainDateTime(info)) => return Ok(info.time),
        Ok(TemporalObjectKind::ZonedDateTime(info)) => {
            return Ok(zoned_date_time_to_plain_date_time_checked(vm, &info)?.time)
        }
        _ => {}
    }
    if value.is_undefined() {
        return Err(vm.current_context.error_type("invalid Temporal time"));
    }
    if value.is_string() {
        return parse_plain_time(vm, value.into_str());
    }
    if value.is_object() {
        let hour = get_property(vm, value, "hour")?;
        let minute = get_property(vm, value, "minute")?;
        let second = get_property(vm, value, "second")?;
        let millisecond = get_property(vm, value, "millisecond")?;
        let microsecond = get_property(vm, value, "microsecond")?;
        let nanosecond = get_property(vm, value, "nanosecond")?;
        if !allow_empty_property_bag
            && [hour, minute, second, millisecond, microsecond, nanosecond]
                .iter()
                .all(|value| value.is_undefined())
        {
            return Err(vm.current_context.error_type("invalid Temporal time"));
        }
        let time = TemporalTimeInfo {
            hour: to_u8(vm, hour, 0)?,
            minute: to_u8(vm, minute, 0)?,
            second: to_u8(vm, second, 0)?,
            millisecond: to_integer(vm, millisecond, 0)? as u16,
            microsecond: to_integer(vm, microsecond, 0)? as u16,
            nanosecond: to_integer(vm, nanosecond, 0)? as u16,
        };
        validate_time(vm, &time)?;
        return Ok(time);
    }
    Err(vm.current_context.error_type("invalid Temporal time"))
}

fn to_temporal_date_time(vm: &mut VM, value: Value) -> Result<TemporalDateTimeInfo, RuntimeError> {
    match temporal_kind(vm, value) {
        Ok(TemporalObjectKind::PlainDateTime(info)) => return Ok(info),
        Ok(TemporalObjectKind::PlainDate(info)) => {
            return Ok(TemporalDateTimeInfo {
                date: info,
                time: default_time(),
            })
        }
        Ok(TemporalObjectKind::ZonedDateTime(info)) => {
            return Ok(zoned_date_time_to_plain_date_time_checked(vm, &info)?)
        }
        _ => {}
    }
    if value.is_string() {
        return parse_plain_date_time(vm, value.into_str());
    }
    if value.is_object() {
        let date = to_temporal_date(vm, value)?;
        let time = to_temporal_time_inner(vm, value, true)?;
        return Ok(TemporalDateTimeInfo { date, time });
    }
    Err(vm.current_context.error_type("invalid Temporal date-time"))
}

fn to_temporal_year_month(
    vm: &mut VM,
    value: Value,
) -> Result<TemporalYearMonthInfo, RuntimeError> {
    match temporal_kind(vm, value) {
        Ok(TemporalObjectKind::PlainYearMonth(info)) => return Ok(info),
        Ok(TemporalObjectKind::PlainDate(info)) => {
            return Ok(TemporalYearMonthInfo {
                year: info.year,
                month: info.month,
                calendar: info.calendar,
                calendar_object: info.calendar_object,
            })
        }
        _ => {}
    }
    if value.is_string() {
        return parse_year_month(vm, value.into_str());
    }
    if value.is_object() {
        let calendar_value = get_property(vm, value, "calendar")?;
        let field_names = calendar_fields(vm, calendar_value, &["month", "monthCode", "year"])?;
        let fields = prepare_temporal_fields(vm, value, &field_names)?;
        if let Some(result) =
            call_custom_calendar_from_fields(vm, calendar_value, "yearMonthFromFields", fields)?
        {
            return to_temporal_year_month(vm, result);
        }
        let year = require_i32_property(vm, fields, "year")?;
        let month = month_from_object(vm, fields)?;
        let (calendar, calendar_object) = calendar_identifier_and_object(vm, calendar_value)?;
        validate_date(vm, year, month, 1)?;
        return Ok(TemporalYearMonthInfo {
            year,
            month,
            calendar,
            calendar_object,
        });
    }
    Err(vm.current_context.error_type("invalid Temporal year-month"))
}

fn to_temporal_month_day(vm: &mut VM, value: Value) -> Result<TemporalMonthDayInfo, RuntimeError> {
    match temporal_kind(vm, value) {
        Ok(TemporalObjectKind::PlainMonthDay(info)) => return Ok(info),
        Ok(TemporalObjectKind::PlainDate(info)) => {
            return Ok(TemporalMonthDayInfo {
                month: info.month,
                day: info.day,
                calendar: info.calendar,
                calendar_object: info.calendar_object,
            })
        }
        _ => {}
    }
    if value.is_string() {
        let text = value.into_str();
        if let Some((month, day)) = parse_month_day(text) {
            validate_date(vm, 1972, month, day)?;
            return Ok(TemporalMonthDayInfo {
                month,
                day,
                calendar: "iso8601".to_string(),
                calendar_object: None,
            });
        }
        let date = parse_plain_date(vm, text)?;
        return Ok(TemporalMonthDayInfo {
            month: date.month,
            day: date.day,
            calendar: date.calendar,
            calendar_object: date.calendar_object,
        });
    }
    if value.is_object() {
        let calendar_value = get_property(vm, value, "calendar")?;
        let field_names =
            calendar_fields(vm, calendar_value, &["day", "month", "monthCode", "year"])?;
        let fields = prepare_temporal_fields(vm, value, &field_names)?;
        if let Some(result) =
            call_custom_calendar_from_fields(vm, calendar_value, "monthDayFromFields", fields)?
        {
            return to_temporal_month_day(vm, result);
        }
        let month = month_from_object(vm, fields)?;
        let day = require_u8_property(vm, fields, "day")?;
        let (calendar, calendar_object) = calendar_identifier_and_object(vm, calendar_value)?;
        validate_date(vm, 1972, month, day)?;
        return Ok(TemporalMonthDayInfo {
            month,
            day,
            calendar,
            calendar_object,
        });
    }
    Err(vm.current_context.error_type("invalid Temporal month-day"))
}

fn to_temporal_duration(vm: &mut VM, value: Value) -> Result<TemporalDurationInfo, RuntimeError> {
    if let Ok(TemporalObjectKind::Duration(info)) = temporal_kind(vm, value) {
        return Ok(info);
    }
    if value.is_string() {
        return parse_duration(vm, value.into_str());
    }
    if value.is_object() {
        let years = get_property(vm, value, "years")?;
        let months = get_property(vm, value, "months")?;
        let weeks = get_property(vm, value, "weeks")?;
        let days = get_property(vm, value, "days")?;
        let hours = get_property(vm, value, "hours")?;
        let minutes = get_property(vm, value, "minutes")?;
        let seconds = get_property(vm, value, "seconds")?;
        let milliseconds = get_property(vm, value, "milliseconds")?;
        let microseconds = get_property(vm, value, "microseconds")?;
        let nanoseconds = get_property(vm, value, "nanoseconds")?;
        if [
            years,
            months,
            weeks,
            days,
            hours,
            minutes,
            seconds,
            milliseconds,
            microseconds,
            nanoseconds,
        ]
        .iter()
        .all(|value| value.is_undefined())
        {
            return Err(vm
                .current_context
                .error_type("missing Temporal duration field"));
        }
        let duration = TemporalDurationInfo {
            years: to_integer(vm, years, 0)?,
            months: to_integer(vm, months, 0)?,
            weeks: to_integer(vm, weeks, 0)?,
            days: to_integer(vm, days, 0)?,
            hours: to_integer(vm, hours, 0)?,
            minutes: to_integer(vm, minutes, 0)?,
            seconds: to_integer(vm, seconds, 0)?,
            milliseconds: to_integer(vm, milliseconds, 0)?,
            microseconds: to_integer(vm, microseconds, 0)?,
            nanoseconds: to_integer(vm, nanoseconds, 0)?,
        };
        validate_duration_sign(vm, &duration)?;
        return Ok(duration);
    }
    Err(vm.current_context.error_type("invalid Temporal duration"))
}

fn to_temporal_instant(vm: &mut VM, value: Value) -> Result<TemporalInstantInfo, RuntimeError> {
    if let Ok(TemporalObjectKind::Instant(info)) = temporal_kind(vm, value) {
        return Ok(info);
    }
    if value.is_string() {
        return parse_instant_string(vm, value.into_str());
    }
    if value.is_object() && !value.is_symbol() && !value.is_bigint() {
        let text = temporal_option_to_string(vm, value)?;
        return parse_instant_string(vm, &text);
    }
    Err(vm.current_context.error_type("invalid Temporal instant"))
}

fn to_temporal_zoned_date_time(
    vm: &mut VM,
    value: Value,
) -> Result<TemporalZonedDateTimeInfo, RuntimeError> {
    if let Ok(TemporalObjectKind::ZonedDateTime(info)) = temporal_kind(vm, value) {
        return Ok(info);
    }
    if value.is_string() {
        return parse_zoned_date_time(vm, value.into_str());
    }
    if value.is_object() {
        let date_time = to_temporal_date_time(vm, value)?;
        let time_zone_value = get_property(vm, value, "timeZone")?;
        let time_zone = time_zone_identifier(vm, time_zone_value)?;
        let calendar_value = get_property(vm, value, "calendar")?;
        let (calendar, calendar_object) = calendar_identifier_and_object(vm, calendar_value)?;
        let epoch_nanoseconds = date_time_to_epoch_nanoseconds(&date_time)
            - time_zone.offset_nanoseconds.unwrap_or(0) as i128;
        return Ok(TemporalZonedDateTimeInfo {
            epoch_nanoseconds,
            time_zone,
            calendar,
            calendar_object,
        });
    }
    Err(vm
        .current_context
        .error_type("invalid Temporal zoned date-time"))
}

fn require_i32_property(vm: &mut VM, object: Value, name: &str) -> Result<i32, RuntimeError> {
    let value = get_property(vm, object, name)?;
    if value.is_undefined() {
        return Err(vm.current_context.error_type("missing Temporal field"));
    }
    to_i32(vm, value, 0)
}

fn require_u8_property(vm: &mut VM, object: Value, name: &str) -> Result<u8, RuntimeError> {
    let value = get_property(vm, object, name)?;
    if value.is_undefined() {
        return Err(vm.current_context.error_type("missing Temporal field"));
    }
    to_u8(vm, value, 0)
}

fn month_from_object(vm: &mut VM, object: Value) -> Result<u8, RuntimeError> {
    let month = get_property(vm, object, "month")?;
    if !month.is_undefined() {
        return to_u8(vm, month, 1);
    }
    let month_code = get_property(vm, object, "monthCode")?;
    if month_code.is_string() {
        if let Some(month) = parse_month_code(month_code.into_str()) {
            return Ok(month);
        }
    }
    Err(vm.current_context.error_type("missing Temporal month"))
}

fn parse_plain_date(vm: &mut VM, text: &str) -> Result<TemporalDateInfo, RuntimeError> {
    validate_plain_date_string(vm, text)?;
    let date_part = text
        .split(['T', 't', ' '])
        .next()
        .unwrap_or(text)
        .trim_start_matches('+');
    let parts: Vec<&str> = date_part.split('-').collect();
    if parts.len() < 3 {
        return Err(vm
            .current_context
            .error_range("invalid Temporal date string"));
    }
    let year = parts[0].parse::<i32>().map_err(|_| {
        vm.current_context
            .error_range("invalid Temporal date string")
    })?;
    let month = parts[1].parse::<u8>().map_err(|_| {
        vm.current_context
            .error_range("invalid Temporal date string")
    })?;
    let day_text: String = parts[2]
        .chars()
        .take_while(|ch| ch.is_ascii_digit())
        .collect();
    let day = day_text.parse::<u8>().map_err(|_| {
        vm.current_context
            .error_range("invalid Temporal date string")
    })?;
    validate_date(vm, year, month, day)?;
    Ok(TemporalDateInfo {
        year,
        month,
        day,
        calendar: "iso8601".to_string(),
        calendar_object: None,
    })
}

fn validate_plain_date_string(vm: &mut VM, text: &str) -> Result<(), RuntimeError> {
    let mut calendar_count = 0;
    let mut critical_calendar_count = 0;
    let mut time_zone_count = 0;
    for annotation in temporal_annotations(text) {
        let critical = annotation.starts_with('!');
        let name = annotation.strip_prefix('!').unwrap_or(annotation);
        if name.starts_with("u-ca=") {
            calendar_count += 1;
            if critical {
                critical_calendar_count += 1;
            }
        } else if name.contains('=') {
            if critical {
                return Err(vm
                    .current_context
                    .error_range("invalid Temporal annotation"));
            }
        } else {
            time_zone_count += 1;
        }
    }
    if time_zone_count > 1 || (calendar_count > 1 && critical_calendar_count > 0) {
        return Err(vm
            .current_context
            .error_range("invalid Temporal annotation"));
    }
    let main = text.split('[').next().unwrap_or(text);
    if main.contains(['Z', 'z']) {
        return Err(vm
            .current_context
            .error_range("invalid Temporal date string"));
    }
    if !main.contains(['T', 't', ' ']) {
        let parts: Vec<&str> = main.trim_start_matches('+').split('-').collect();
        if parts.len() > 3
            || parts
                .get(2)
                .map(|day| day.contains(['+', 'Z', 'z']))
                .unwrap_or(false)
        {
            return Err(vm
                .current_context
                .error_range("invalid Temporal date string"));
        }
    }
    Ok(())
}

fn temporal_annotations(text: &str) -> Vec<&str> {
    let mut result = Vec::new();
    let mut rest = text;
    while let Some(start) = rest.find('[') {
        rest = &rest[start + 1..];
        let Some(end) = rest.find(']') else {
            break;
        };
        result.push(&rest[..end]);
        rest = &rest[end + 1..];
    }
    result
}

fn looks_like_temporal_date_time_string(text: &str) -> bool {
    let text = text
        .strip_prefix('+')
        .or_else(|| text.strip_prefix('-'))
        .unwrap_or(text);
    text.chars()
        .next()
        .map(|ch| ch.is_ascii_digit())
        .unwrap_or(false)
        && text.contains('-')
}

fn calendar_annotation_identifier(text: &str) -> Option<&str> {
    temporal_annotations(text)
        .into_iter()
        .find_map(|annotation| {
            annotation
                .strip_prefix('!')
                .unwrap_or(annotation)
                .strip_prefix("u-ca=")
        })
}

fn builtin_calendar_identifier(text: &str) -> Option<String> {
    let lower = text.to_ascii_lowercase();
    match lower.as_str() {
        "iso8601" | "buddhist" | "chinese" | "coptic" | "dangi" | "ethioaa" | "ethiopic"
        | "gregory" | "hebrew" | "indian" | "islamic" | "islamic-civil" | "islamic-rgsa"
        | "islamic-tbla" | "islamic-umalqura" | "japanese" | "persian" | "roc" => Some(lower),
        _ => None,
    }
}

fn parse_plain_time(vm: &mut VM, text: &str) -> Result<TemporalTimeInfo, RuntimeError> {
    let mut time = text.trim();
    if let Some(index) = time.find('T').or_else(|| time.find('t')) {
        time = &time[index + 1..];
    }
    time = time
        .trim_end_matches('Z')
        .trim_end_matches('z')
        .split(['[', '+', '-'])
        .next()
        .unwrap_or(time);
    let mut pieces = time.split(':');
    let hour = pieces
        .next()
        .and_then(|part| part.parse::<u8>().ok())
        .ok_or_else(|| {
            vm.current_context
                .error_range("invalid Temporal time string")
        })?;
    let minute = pieces
        .next()
        .and_then(|part| part.parse::<u8>().ok())
        .unwrap_or(0);
    let second_part = pieces.next().unwrap_or("0");
    let (second_text, fraction_text) = second_part
        .split_once('.')
        .map(|(s, f)| (s, f))
        .unwrap_or((second_part, ""));
    let second = second_text
        .chars()
        .take_while(|ch| ch.is_ascii_digit())
        .collect::<String>()
        .parse::<u8>()
        .unwrap_or(0);
    let fraction = parse_fraction(fraction_text);
    let time = TemporalTimeInfo {
        hour,
        minute,
        second,
        millisecond: (fraction / 1_000_000) as u16,
        microsecond: ((fraction / 1_000) % 1_000) as u16,
        nanosecond: (fraction % 1_000) as u16,
    };
    validate_time(vm, &time)?;
    Ok(time)
}

fn parse_plain_date_time(vm: &mut VM, text: &str) -> Result<TemporalDateTimeInfo, RuntimeError> {
    let date = parse_plain_date(vm, text)?;
    let time = if text.contains('T') || text.contains('t') || text.contains(' ') {
        parse_plain_time(vm, text)?
    } else {
        default_time()
    };
    Ok(TemporalDateTimeInfo { date, time })
}

fn parse_instant_string(vm: &mut VM, text: &str) -> Result<TemporalInstantInfo, RuntimeError> {
    validate_instant_string_annotations(vm, text)?;
    let main = text.split('[').next().unwrap_or(text);
    let separator = main
        .find('T')
        .or_else(|| main.find('t'))
        .ok_or_else(|| vm.current_context.error_range("invalid Temporal instant"))?;
    let date_part = &main[..separator];
    let time_part = &main[separator + 1..];
    let offset = instant_offset_nanoseconds(time_part)
        .ok_or_else(|| vm.current_context.error_range("invalid Temporal instant"))?;
    let date = parse_plain_date(vm, date_part)?;
    let time = parse_plain_time(vm, time_part)?;
    let date_time = TemporalDateTimeInfo { date, time };
    Ok(TemporalInstantInfo {
        epoch_nanoseconds: date_time_to_epoch_nanoseconds(&date_time) - offset as i128,
    })
}

fn validate_instant_string_annotations(vm: &mut VM, text: &str) -> Result<(), RuntimeError> {
    let mut calendar_count = 0;
    let mut critical_calendar_count = 0;
    let mut time_zone_count = 0;
    for annotation in temporal_annotations(text) {
        let critical = annotation.starts_with('!');
        let name = annotation.strip_prefix('!').unwrap_or(annotation);
        if name.starts_with("u-ca=") {
            calendar_count += 1;
            if critical {
                critical_calendar_count += 1;
            }
        } else if name.contains('=') {
            if critical {
                return Err(vm
                    .current_context
                    .error_range("invalid Temporal annotation"));
            }
        } else {
            time_zone_count += 1;
        }
    }
    if time_zone_count > 1 || (calendar_count > 1 && critical_calendar_count > 0) {
        return Err(vm
            .current_context
            .error_range("invalid Temporal annotation"));
    }
    Ok(())
}

fn instant_offset_nanoseconds(time_part: &str) -> Option<i64> {
    let time_part = time_part.trim();
    if time_part.ends_with('Z') || time_part.ends_with('z') {
        return Some(0);
    }
    let offset_start = time_part
        .char_indices()
        .skip(1)
        .find_map(|(idx, ch)| matches!(ch, '+' | '-').then_some(idx))?;
    parse_offset_nanoseconds(&time_part[offset_start..])
}

fn parse_month_day(text: &str) -> Option<(u8, u8)> {
    let cleaned = text.trim_start_matches("--");
    let parts: Vec<&str> = cleaned.split('-').collect();
    if parts.len() < 2 {
        return None;
    }
    Some((parts[0].parse().ok()?, parts[1].parse().ok()?))
}

fn parse_year_month(vm: &mut VM, text: &str) -> Result<TemporalYearMonthInfo, RuntimeError> {
    validate_plain_date_string(vm, text)?;
    let date_part = text
        .split(['T', 't', ' '])
        .next()
        .unwrap_or(text)
        .trim_start_matches('+');
    let parts: Vec<&str> = date_part.split('-').collect();
    if parts.len() < 2 {
        return Err(vm
            .current_context
            .error_range("invalid Temporal year-month string"));
    }
    let year = parts[0].parse::<i32>().map_err(|_| {
        vm.current_context
            .error_range("invalid Temporal year-month string")
    })?;
    let month = parts[1].parse::<u8>().map_err(|_| {
        vm.current_context
            .error_range("invalid Temporal year-month string")
    })?;
    validate_date(vm, year, month, 1)?;
    let calendar = calendar_annotation_identifier(text)
        .and_then(builtin_calendar_identifier)
        .unwrap_or_else(|| "iso8601".to_string());
    Ok(TemporalYearMonthInfo {
        year,
        month,
        calendar,
        calendar_object: None,
    })
}

fn parse_zoned_date_time(
    vm: &mut VM,
    text: &str,
) -> Result<TemporalZonedDateTimeInfo, RuntimeError> {
    let date_time_part = plain_date_time_part_from_zoned(text);
    let date_time = parse_plain_date_time(vm, &date_time_part)?;
    let time_zone = if let Some(start) = text.find('[') {
        if let Some(end) = text[start + 1..].find(']') {
            let value = vm.factory.string(&text[start + 1..start + 1 + end]);
            time_zone_identifier(vm, value)?
        } else {
            let value = vm.factory.string("UTC");
            time_zone_identifier(vm, value)?
        }
    } else {
        let value = vm.factory.string("UTC");
        time_zone_identifier(vm, value)?
    };
    let offset = zoned_date_time_offset_nanoseconds(text).or(time_zone.offset_nanoseconds);
    let epoch_nanoseconds =
        date_time_to_epoch_nanoseconds(&date_time) - offset.unwrap_or(0) as i128;
    Ok(TemporalZonedDateTimeInfo {
        epoch_nanoseconds,
        time_zone,
        calendar: date_time.date.calendar,
        calendar_object: date_time.date.calendar_object,
    })
}

fn plain_date_time_part_from_zoned(text: &str) -> String {
    let mut main = text.split('[').next().unwrap_or(text).trim().to_string();
    if main.ends_with('Z') || main.ends_with('z') {
        main.pop();
        return main;
    }
    let Some(separator) = main
        .find('T')
        .or_else(|| main.find('t'))
        .or_else(|| main.find(' '))
    else {
        return main;
    };
    let time_part = &main[separator + 1..];
    if let Some(offset_start) = time_part
        .char_indices()
        .skip(1)
        .find_map(|(idx, ch)| matches!(ch, '+' | '-').then_some(idx))
    {
        main.truncate(separator + 1 + offset_start);
    }
    main
}

fn zoned_date_time_offset_nanoseconds(text: &str) -> Option<i64> {
    let main = text.split('[').next().unwrap_or(text).trim();
    let separator = main
        .find('T')
        .or_else(|| main.find('t'))
        .or_else(|| main.find(' '))?;
    instant_offset_nanoseconds(&main[separator + 1..])
}

fn parse_duration(vm: &mut VM, text: &str) -> Result<TemporalDurationInfo, RuntimeError> {
    let mut sign = 1;
    let mut chars = text.trim();
    if chars.starts_with('-') {
        sign = -1;
        chars = &chars[1..];
    } else if chars.starts_with('+') {
        chars = &chars[1..];
    }
    if !chars.starts_with('P') && !chars.starts_with('p') {
        return Err(vm.current_context.error_range("invalid Temporal duration"));
    }
    let mut duration = TemporalDurationInfo {
        years: 0,
        months: 0,
        weeks: 0,
        days: 0,
        hours: 0,
        minutes: 0,
        seconds: 0,
        milliseconds: 0,
        microseconds: 0,
        nanoseconds: 0,
    };
    let mut number = String::new();
    let mut time = false;
    for ch in chars[1..].chars() {
        if ch == 'T' || ch == 't' {
            time = true;
            continue;
        }
        if ch.is_ascii_digit() || ch == '.' {
            number.push(ch);
            continue;
        }
        let value = number.parse::<f64>().unwrap_or(0.0);
        number.clear();
        match ch {
            'Y' | 'y' => duration.years = value as i64 * sign,
            'M' | 'm' if time => duration.minutes = value as i64 * sign,
            'M' | 'm' => duration.months = value as i64 * sign,
            'W' | 'w' => duration.weeks = value as i64 * sign,
            'D' | 'd' => duration.days = value as i64 * sign,
            'H' | 'h' => duration.hours = value as i64 * sign,
            'S' | 's' => {
                let whole = value.trunc() as i64;
                let frac = ((value.fract().abs() * 1_000_000_000.0).round() as i64) * sign;
                duration.seconds = whole * sign;
                duration.milliseconds = frac / 1_000_000;
                duration.microseconds = (frac / 1_000) % 1_000;
                duration.nanoseconds = frac % 1_000;
            }
            _ => return Err(vm.current_context.error_range("invalid Temporal duration")),
        }
    }
    validate_duration_sign(vm, &duration)?;
    Ok(duration)
}

fn parse_offset_nanoseconds(text: &str) -> Option<i64> {
    let sign = if text.starts_with('-') {
        -1
    } else if text.starts_with('+') {
        1
    } else {
        return None;
    };
    let rest = &text[1..];
    let parts: Vec<&str> = rest.split(':').collect();
    let hour = parts.get(0)?.parse::<i64>().ok()?;
    let minute = parts
        .get(1)
        .and_then(|part| part.parse::<i64>().ok())
        .unwrap_or(0);
    let second = parts
        .get(2)
        .and_then(|part| part.parse::<i64>().ok())
        .unwrap_or(0);
    if hour > 23 || minute > 59 || second > 59 {
        return None;
    }
    Some(sign * (hour * 3_600 + minute * 60 + second) * 1_000_000_000)
}

fn parse_month_code(text: &str) -> Option<u8> {
    if text.len() == 3 && text.starts_with('M') {
        let month = text[1..].parse::<u8>().ok()?;
        if (1..=12).contains(&month) {
            return Some(month);
        }
    }
    None
}

fn parse_fraction(text: &str) -> u32 {
    let mut digits = text
        .chars()
        .take_while(|ch| ch.is_ascii_digit())
        .take(9)
        .collect::<String>();
    while digits.len() < 9 {
        digits.push('0');
    }
    digits.parse::<u32>().unwrap_or(0)
}

fn validate_date(vm: &mut VM, year: i32, month: u8, day: u8) -> Result<(), RuntimeError> {
    if !(1..=12).contains(&month) || day == 0 || day > days_in_month(year, month) {
        return Err(vm.current_context.error_range("invalid Temporal date"));
    }
    Ok(())
}

fn validate_time(vm: &mut VM, time: &TemporalTimeInfo) -> Result<(), RuntimeError> {
    if time.hour > 23
        || time.minute > 59
        || time.second > 59
        || time.millisecond > 999
        || time.microsecond > 999
        || time.nanosecond > 999
    {
        return Err(vm.current_context.error_range("invalid Temporal time"));
    }
    Ok(())
}

fn validate_duration_sign(
    vm: &mut VM,
    duration: &TemporalDurationInfo,
) -> Result<(), RuntimeError> {
    let sign = duration_sign(duration);
    if sign == 0 {
        return Ok(());
    }
    for value in duration_fields(duration) {
        if value != 0 && value.signum() != sign {
            return Err(vm
                .current_context
                .error_range("mixed-sign Temporal duration"));
        }
    }
    Ok(())
}

fn date_from_temporal_kind(
    vm: &mut VM,
    kind: TemporalObjectKind,
) -> Result<TemporalDateInfo, RuntimeError> {
    match kind {
        TemporalObjectKind::PlainDate(info) => Ok(info),
        TemporalObjectKind::PlainDateTime(info) => Ok(info.date),
        TemporalObjectKind::PlainMonthDay(info) => Ok(TemporalDateInfo {
            year: 1972,
            month: info.month,
            day: info.day,
            calendar: info.calendar,
            calendar_object: info.calendar_object,
        }),
        TemporalObjectKind::PlainYearMonth(info) => Ok(TemporalDateInfo {
            year: info.year,
            month: info.month,
            day: 1,
            calendar: info.calendar,
            calendar_object: info.calendar_object,
        }),
        TemporalObjectKind::ZonedDateTime(info) => {
            Ok(zoned_date_time_to_plain_date_time_checked(vm, &info)?.date)
        }
        _ => Err(temporal_brand_error(vm)),
    }
}

fn date_from_this(vm: &mut VM, this: Value) -> Result<TemporalDateInfo, RuntimeError> {
    let kind = temporal_kind(vm, this)?;
    date_from_temporal_kind(vm, kind)
}

fn time_from_temporal_kind(
    vm: &mut VM,
    kind: TemporalObjectKind,
) -> Result<TemporalTimeInfo, RuntimeError> {
    match kind {
        TemporalObjectKind::PlainTime(info) => Ok(info),
        TemporalObjectKind::PlainDateTime(info) => Ok(info.time),
        TemporalObjectKind::ZonedDateTime(info) => {
            Ok(zoned_date_time_to_plain_date_time_checked(vm, &info)?.time)
        }
        _ => Err(temporal_brand_error(vm)),
    }
}

fn time_from_this(vm: &mut VM, this: Value) -> Result<TemporalTimeInfo, RuntimeError> {
    let kind = temporal_kind(vm, this)?;
    time_from_temporal_kind(vm, kind)
}

fn instant_nanoseconds(vm: &mut VM, kind: TemporalObjectKind) -> Result<i128, RuntimeError> {
    match kind {
        TemporalObjectKind::Instant(info) => Ok(info.epoch_nanoseconds),
        TemporalObjectKind::ZonedDateTime(info) => Ok(info.epoch_nanoseconds),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn instant_nanoseconds_from_this(vm: &mut VM, this: Value) -> Result<i128, RuntimeError> {
    let kind = temporal_kind(vm, this)?;
    instant_nanoseconds(vm, kind)
}

fn zoned_time_zone(
    vm: &mut VM,
    kind: TemporalObjectKind,
) -> Result<TemporalTimeZoneInfo, RuntimeError> {
    match kind {
        TemporalObjectKind::ZonedDateTime(info) => Ok(info.time_zone),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn zoned_time_zone_from_this(
    vm: &mut VM,
    this: Value,
) -> Result<TemporalTimeZoneInfo, RuntimeError> {
    let kind = temporal_kind(vm, this)?;
    zoned_time_zone(vm, kind)
}

fn ensure_calendar(vm: &mut VM, this: Value) -> Result<(), RuntimeError> {
    match temporal_kind(vm, this)? {
        TemporalObjectKind::Calendar(_) => Ok(()),
        _ => Err(temporal_brand_error(vm)),
    }
}

fn calendar_object(vm: &mut VM, identifier: &str) -> Value {
    temporal_object_by_name(
        vm,
        "Calendar",
        TemporalObjectKind::Calendar(TemporalCalendarInfo {
            identifier: identifier.to_string(),
        }),
    )
}

fn calendar_slot_value(vm: &mut VM, identifier: &str, object: Option<Value>) -> Value {
    object.unwrap_or_else(|| calendar_object(vm, identifier))
}

fn time_zone_object(vm: &mut VM, info: &TemporalTimeZoneInfo) -> Value {
    temporal_object_by_name(vm, "TimeZone", TemporalObjectKind::TimeZone(info.clone()))
}

fn plain_date_to_zoned_args(
    vm: &mut VM,
    item: Value,
) -> Result<(TemporalTimeZoneInfo, TemporalTimeInfo), RuntimeError> {
    if let Ok(TemporalObjectKind::TimeZone(info)) = temporal_kind(vm, item) {
        return Ok((info, default_time()));
    }
    if item.is_string() {
        return Ok((time_zone_identifier(vm, item)?, default_time()));
    }
    if item.is_object() {
        let time_zone_value = get_property(vm, item, "timeZone")?;
        let time_zone = if time_zone_value.is_undefined() {
            time_zone_identifier(vm, item)?
        } else {
            time_zone_identifier(vm, time_zone_value)?
        };
        let plain_time = get_property(vm, item, "plainTime")?;
        let time = if plain_time.is_undefined() {
            default_time()
        } else {
            to_temporal_time(vm, plain_time)?
        };
        return Ok((time_zone, time));
    }
    Err(vm
        .current_context
        .error_type("Temporal.PlainDate.toZonedDateTime requires time zone"))
}

fn zoned_from_date_time(
    vm: &mut VM,
    date_time: TemporalDateTimeInfo,
    time_zone: TemporalTimeZoneInfo,
) -> Result<TemporalZonedDateTimeInfo, RuntimeError> {
    let epoch_nanoseconds = date_time_to_epoch_nanoseconds(&date_time);
    let offset = time_zone_offset_nanoseconds_for(
        vm,
        &time_zone,
        TemporalInstantInfo { epoch_nanoseconds },
    )?;
    Ok(TemporalZonedDateTimeInfo {
        epoch_nanoseconds: epoch_nanoseconds - offset as i128,
        time_zone,
        calendar: date_time.date.calendar,
        calendar_object: date_time.date.calendar_object,
    })
}

fn patch_date_fields(
    vm: &mut VM,
    fields: Value,
    date: &mut TemporalDateInfo,
) -> Result<(), RuntimeError> {
    if !fields.is_object() {
        return Err(vm
            .current_context
            .error_type("Temporal fields must be object"));
    }
    let year = get_property(vm, fields, "year")?;
    if !year.is_undefined() {
        date.year = to_i32(vm, year, date.year)?;
    }
    let month = get_property(vm, fields, "month")?;
    if !month.is_undefined() {
        date.month = to_u8(vm, month, date.month)?;
    }
    let month_code = get_property(vm, fields, "monthCode")?;
    if !month_code.is_undefined() {
        if let Some(month) = parse_month_code(month_code.into_str()) {
            date.month = month;
        }
    }
    let day = get_property(vm, fields, "day")?;
    if !day.is_undefined() {
        date.day = to_u8(vm, day, date.day)?;
    }
    validate_date(vm, date.year, date.month, date.day)
}

fn patch_time_fields(
    vm: &mut VM,
    fields: Value,
    time: &mut TemporalTimeInfo,
) -> Result<(), RuntimeError> {
    if !fields.is_object() {
        return Err(vm
            .current_context
            .error_type("Temporal fields must be object"));
    }
    let hour = get_property(vm, fields, "hour")?;
    if !hour.is_undefined() {
        time.hour = to_u8(vm, hour, time.hour)?;
    }
    let minute = get_property(vm, fields, "minute")?;
    if !minute.is_undefined() {
        time.minute = to_u8(vm, minute, time.minute)?;
    }
    let second = get_property(vm, fields, "second")?;
    if !second.is_undefined() {
        time.second = to_u8(vm, second, time.second)?;
    }
    let millisecond = get_property(vm, fields, "millisecond")?;
    if !millisecond.is_undefined() {
        time.millisecond = to_integer(vm, millisecond, time.millisecond as i64)? as u16;
    }
    let microsecond = get_property(vm, fields, "microsecond")?;
    if !microsecond.is_undefined() {
        time.microsecond = to_integer(vm, microsecond, time.microsecond as i64)? as u16;
    }
    let nanosecond = get_property(vm, fields, "nanosecond")?;
    if !nanosecond.is_undefined() {
        time.nanosecond = to_integer(vm, nanosecond, time.nanosecond as i64)? as u16;
    }
    validate_time(vm, time)
}

fn patch_year_month_fields(
    vm: &mut VM,
    fields: Value,
    info: &mut TemporalYearMonthInfo,
) -> Result<(), RuntimeError> {
    if !fields.is_object() {
        return Err(vm
            .current_context
            .error_type("Temporal fields must be object"));
    }
    let year = get_property(vm, fields, "year")?;
    if !year.is_undefined() {
        info.year = to_i32(vm, year, info.year)?;
    }
    let month = get_property(vm, fields, "month")?;
    if !month.is_undefined() {
        info.month = to_u8(vm, month, info.month)?;
    }
    let month_code = get_property(vm, fields, "monthCode")?;
    if !month_code.is_undefined() {
        if let Some(month) = parse_month_code(month_code.into_str()) {
            info.month = month;
        }
    }
    validate_date(vm, info.year, info.month, 1)
}

fn patch_month_day_fields(
    vm: &mut VM,
    fields: Value,
    info: &mut TemporalMonthDayInfo,
) -> Result<(), RuntimeError> {
    if !fields.is_object() {
        return Err(vm
            .current_context
            .error_type("Temporal fields must be object"));
    }
    let month = get_property(vm, fields, "month")?;
    if !month.is_undefined() {
        info.month = to_u8(vm, month, info.month)?;
    }
    let month_code = get_property(vm, fields, "monthCode")?;
    if !month_code.is_undefined() {
        if let Some(month) = parse_month_code(month_code.into_str()) {
            info.month = month;
        }
    }
    let day = get_property(vm, fields, "day")?;
    if !day.is_undefined() {
        info.day = to_u8(vm, day, info.day)?;
    }
    validate_date(vm, 1972, info.month, info.day)
}

fn map_duration(info: TemporalDurationInfo, map: fn(i64) -> i64) -> TemporalDurationInfo {
    TemporalDurationInfo {
        years: map(info.years),
        months: map(info.months),
        weeks: map(info.weeks),
        days: map(info.days),
        hours: map(info.hours),
        minutes: map(info.minutes),
        seconds: map(info.seconds),
        milliseconds: map(info.milliseconds),
        microseconds: map(info.microseconds),
        nanoseconds: map(info.nanoseconds),
    }
}

fn add_duration(
    lhs: &TemporalDurationInfo,
    rhs: &TemporalDurationInfo,
    sign: i64,
) -> TemporalDurationInfo {
    TemporalDurationInfo {
        years: lhs.years + rhs.years * sign,
        months: lhs.months + rhs.months * sign,
        weeks: lhs.weeks + rhs.weeks * sign,
        days: lhs.days + rhs.days * sign,
        hours: lhs.hours + rhs.hours * sign,
        minutes: lhs.minutes + rhs.minutes * sign,
        seconds: lhs.seconds + rhs.seconds * sign,
        milliseconds: lhs.milliseconds + rhs.milliseconds * sign,
        microseconds: lhs.microseconds + rhs.microseconds * sign,
        nanoseconds: lhs.nanoseconds + rhs.nanoseconds * sign,
    }
}

fn add_duration_to_date(
    vm: &mut VM,
    date: &mut TemporalDateInfo,
    duration: &TemporalDurationInfo,
    sign: i64,
) -> Result<(), RuntimeError> {
    let mut year = date.year as i64 + duration.years * sign;
    let mut month0 = date.month as i64 - 1 + duration.months * sign;
    year += month0.div_euclid(12);
    month0 = month0.rem_euclid(12);
    let month = month0 as u8 + 1;
    let day = date.day.min(days_in_month(year as i32, month));
    let days =
        days_from_civil(year as i32, month, day) + (duration.weeks * 7 + duration.days) * sign;
    let (new_year, new_month, new_day) = civil_from_days(days);
    validate_date(vm, new_year, new_month, new_day)?;
    date.year = new_year;
    date.month = new_month;
    date.day = new_day;
    Ok(())
}

fn add_duration_to_time(time: &mut TemporalTimeInfo, duration: &TemporalDurationInfo, sign: i64) {
    let day_ns = 86_400_000_000_000i128;
    let current = time_nanoseconds(time);
    let delta = duration_time_nanoseconds(duration) * sign as i128;
    let next = (current + delta).rem_euclid(day_ns);
    *time = time_from_nanoseconds(next);
}

fn validate_temporal_options_object(vm: &mut VM, options: Value) -> Result<(), RuntimeError> {
    if options.is_undefined()
        || (options.is_object() && !options.is_symbol() && !options.is_bigint())
    {
        Ok(())
    } else {
        Err(vm
            .current_context
            .error_type("Temporal options must be object"))
    }
}

fn validate_temporal_overflow_option(vm: &mut VM, options: Value) -> Result<String, RuntimeError> {
    validate_temporal_options_object(vm, options)?;
    if !options.is_object() {
        return Ok("constrain".to_string());
    }
    let value = get_property(vm, options, "overflow")?;
    if value.is_undefined() {
        return Ok("constrain".to_string());
    }
    let value = temporal_option_to_string(vm, value)?;
    if matches!(value.as_str(), "constrain" | "reject") {
        Ok(value)
    } else {
        Err(vm.current_context.error_range("invalid Temporal overflow"))
    }
}

fn temporal_option_to_string(vm: &mut VM, value: Value) -> Result<String, RuntimeError> {
    if value.is_symbol() {
        return Err(vm.current_context.error_type("Cannot convert Symbol"));
    }
    if value.is_object() {
        let key = vm.factory.string("toString");
        let method = vm.get_property_by_value(value, key)?;
        if method.is_function_object() {
            let result = vm.call_function(method, &[], value)?;
            if result.is_object() {
                return Err(vm
                    .current_context
                    .error_type("Cannot convert object to string"));
            }
            return Ok(result.to_string());
        }
    }
    Ok(value.to_string())
}

fn temporal_unit_from_options(
    vm: &mut VM,
    options: Value,
    name: &str,
) -> Result<Option<String>, RuntimeError> {
    if options.is_undefined() {
        return Ok(None);
    }
    if options.is_string() {
        if name == "largestUnit" && options.into_str() == "auto" {
            return Ok(Some("auto".to_string()));
        }
        return normalize_temporal_unit(options.into_str())
            .map(Some)
            .ok_or_else(|| vm.current_context.error_range("invalid Temporal unit"));
    }
    if !options.is_object() {
        return Err(vm
            .current_context
            .error_type("Temporal options must be object"));
    }
    let value = get_property(vm, options, name)?;
    if value.is_undefined() {
        return Ok(None);
    }
    let value = temporal_option_to_string(vm, value)?;
    if name == "largestUnit" && value == "auto" {
        return Ok(Some("auto".to_string()));
    }
    normalize_temporal_unit(value.as_str())
        .map(Some)
        .ok_or_else(|| vm.current_context.error_range("invalid Temporal unit"))
}

fn temporal_rounding_increment(vm: &mut VM, options: Value) -> Result<i64, RuntimeError> {
    if !options.is_object() {
        return Ok(1);
    }
    let value = get_property(vm, options, "roundingIncrement")?;
    if value.is_undefined() {
        return Ok(1);
    }
    let increment = to_number(vm, value)?;
    if !increment.is_finite() {
        return Err(vm.current_context.error_range("invalid roundingIncrement"));
    }
    let increment = increment.trunc() as i64;
    if !(1..=1_000_000_000).contains(&increment) {
        return Err(vm.current_context.error_range("invalid roundingIncrement"));
    }
    Ok(increment)
}

fn temporal_rounding_mode(vm: &mut VM, options: Value) -> Result<String, RuntimeError> {
    if !options.is_object() {
        return Ok("halfExpand".to_string());
    }
    let value = get_property(vm, options, "roundingMode")?;
    if value.is_undefined() {
        Ok("halfExpand".to_string())
    } else {
        let mode = temporal_option_to_string(vm, value)?;
        if matches!(
            mode.as_str(),
            "ceil"
                | "floor"
                | "expand"
                | "trunc"
                | "halfCeil"
                | "halfFloor"
                | "halfExpand"
                | "halfTrunc"
                | "halfEven"
        ) {
            Ok(mode)
        } else {
            Err(vm.current_context.error_range("invalid roundingMode"))
        }
    }
}

fn validate_temporal_to_string_options(
    vm: &mut VM,
    options: Value,
    kind: &TemporalObjectKind,
) -> Result<(), RuntimeError> {
    validate_temporal_options_object(vm, options)?;
    if !options.is_object() {
        return Ok(());
    }

    match kind {
        TemporalObjectKind::PlainDate(_)
        | TemporalObjectKind::PlainDateTime(_)
        | TemporalObjectKind::PlainMonthDay(_)
        | TemporalObjectKind::PlainYearMonth(_)
        | TemporalObjectKind::ZonedDateTime(_) => {
            validate_temporal_string_option(
                vm,
                options,
                "calendarName",
                &["auto", "always", "never", "critical"],
            )?;
        }
        _ => {}
    }

    match kind {
        TemporalObjectKind::Duration(_)
        | TemporalObjectKind::Instant(_)
        | TemporalObjectKind::PlainDateTime(_)
        | TemporalObjectKind::PlainTime(_)
        | TemporalObjectKind::ZonedDateTime(_) => {
            validate_fractional_second_digits(vm, options)?;
            validate_temporal_time_precision_unit(vm, options, kind)?;
            temporal_rounding_mode(vm, options)?;
        }
        _ => {}
    }

    if let TemporalObjectKind::Instant(info) = kind {
        let time_zone = get_property(vm, options, "timeZone")?;
        if !time_zone.is_undefined() {
            if time_zone.is_object() {
                let instant = temporal_object_by_name(
                    vm,
                    "Instant",
                    TemporalObjectKind::Instant(info.clone()),
                );
                validate_time_zone_offset_method(vm, time_zone, instant)?;
            }
            time_zone_identifier(vm, time_zone)?;
        }
    }

    if matches!(kind, TemporalObjectKind::ZonedDateTime(_)) {
        validate_temporal_string_option(vm, options, "offset", &["auto", "never"])?;
        validate_temporal_string_option(
            vm,
            options,
            "timeZoneName",
            &["auto", "never", "critical"],
        )?;
    }

    Ok(())
}

fn temporal_difference_settings(
    vm: &mut VM,
    options: Value,
) -> Result<(Option<String>, Option<String>, i64, String), RuntimeError> {
    validate_temporal_options_object(vm, options)?;
    if !options.is_object() {
        return Ok((None, None, 1, "halfExpand".to_string()));
    }
    let largest_unit = temporal_unit_from_options(vm, options, "largestUnit")?;
    let smallest_unit = temporal_unit_from_options(vm, options, "smallestUnit")?;
    if let (Some(largest), Some(smallest)) = (&largest_unit, &smallest_unit) {
        if largest != "auto" && temporal_unit_rank(largest) < temporal_unit_rank(smallest) {
            return Err(vm
                .current_context
                .error_range("largestUnit is smaller than smallestUnit"));
        }
    }
    let rounding_increment = temporal_rounding_increment(vm, options)?;
    let rounding_mode = temporal_rounding_mode(vm, options)?;
    Ok((
        largest_unit,
        smallest_unit,
        rounding_increment,
        rounding_mode,
    ))
}

fn validate_relative_to_option(vm: &mut VM, options: Value) -> Result<(), RuntimeError> {
    if !options.is_object() {
        return Ok(());
    }
    let relative_to = get_property(vm, options, "relativeTo")?;
    if relative_to.is_undefined() {
        return Ok(());
    }
    if let Ok(TemporalObjectKind::ZonedDateTime(info)) = temporal_kind(vm, relative_to) {
        let _ = zoned_date_time_to_plain_date_time_checked(vm, &info)?;
        return Ok(());
    }
    if relative_to.is_object() {
        let time_zone = get_property(vm, relative_to, "timeZone")?;
        if !time_zone.is_undefined() {
            let info = to_temporal_zoned_date_time(vm, relative_to)?;
            let _ = zoned_date_time_to_plain_date_time_checked(vm, &info)?;
            return Ok(());
        }
    }
    let _ = to_temporal_date_time(vm, relative_to)?;
    Ok(())
}

fn temporal_unit_rank(unit: &str) -> i32 {
    match unit {
        "year" => 9,
        "month" => 8,
        "week" => 7,
        "day" => 6,
        "hour" => 5,
        "minute" => 4,
        "second" => 3,
        "millisecond" => 2,
        "microsecond" => 1,
        "nanosecond" => 0,
        _ => -1,
    }
}

fn validate_time_zone_offset_method(
    vm: &mut VM,
    time_zone: Value,
    instant: Value,
) -> Result<(), RuntimeError> {
    let method = get_property(vm, time_zone, "getOffsetNanosecondsFor")?;
    if !vm.is_callable(method) {
        return Err(vm
            .current_context
            .error_type("timeZone.getOffsetNanosecondsFor"));
    }
    let offset = vm.call_function(method, &[instant], time_zone)?;
    if !offset.is_number() {
        return Err(vm
            .current_context
            .error_type("time zone offset must be Number"));
    }
    let offset = offset.into_number();
    if !offset.is_finite() || offset.fract() != 0.0 || offset.abs() >= 86_400_000_000_000.0 {
        return Err(vm.current_context.error_range("invalid time zone offset"));
    }
    Ok(())
}

fn validate_temporal_string_option(
    vm: &mut VM,
    options: Value,
    name: &str,
    allowed: &[&str],
) -> Result<(), RuntimeError> {
    let value = get_property(vm, options, name)?;
    if value.is_undefined() {
        return Ok(());
    }
    let value = temporal_option_to_string(vm, value)?;
    if allowed.contains(&value.as_str()) {
        Ok(())
    } else {
        Err(vm
            .current_context
            .error_range(format!("invalid Temporal option {}", name)))
    }
}

fn validate_fractional_second_digits(vm: &mut VM, options: Value) -> Result<(), RuntimeError> {
    let value = get_property(vm, options, "fractionalSecondDigits")?;
    if value.is_undefined() {
        return Ok(());
    }
    if value.is_number() {
        let number = value.into_number().floor();
        if number.is_finite() && (0.0..=9.0).contains(&number) {
            return Ok(());
        }
        return Err(vm
            .current_context
            .error_range("invalid fractionalSecondDigits"));
    }
    let value = temporal_option_to_string(vm, value)?;
    if value == "auto" {
        Ok(())
    } else {
        Err(vm
            .current_context
            .error_range("invalid fractionalSecondDigits"))
    }
}

fn validate_temporal_time_precision_unit(
    vm: &mut VM,
    options: Value,
    kind: &TemporalObjectKind,
) -> Result<(), RuntimeError> {
    let unit = temporal_unit_from_options(vm, options, "smallestUnit")?;
    if let Some(unit) = unit {
        let valid = if matches!(kind, TemporalObjectKind::Duration(_)) {
            matches!(
                unit.as_str(),
                "second" | "millisecond" | "microsecond" | "nanosecond"
            )
        } else {
            matches!(
                unit.as_str(),
                "minute" | "second" | "millisecond" | "microsecond" | "nanosecond"
            )
        };
        if valid {
            Ok(())
        } else {
            Err(vm
                .current_context
                .error_range("invalid Temporal smallestUnit"))
        }
    } else {
        Ok(())
    }
}

fn normalize_temporal_unit(unit: &str) -> Option<String> {
    let singular = unit.strip_suffix('s').unwrap_or(unit);
    match singular {
        "year" | "month" | "week" | "day" | "hour" | "minute" | "second" | "millisecond"
        | "microsecond" | "nanosecond" => Some(singular.to_string()),
        _ => None,
    }
}

fn unit_nanoseconds(unit: &str) -> Option<i128> {
    match unit {
        "day" => Some(86_400_000_000_000),
        "hour" => Some(3_600_000_000_000),
        "minute" => Some(60_000_000_000),
        "second" => Some(1_000_000_000),
        "millisecond" => Some(1_000_000),
        "microsecond" => Some(1_000),
        "nanosecond" => Some(1),
        _ => None,
    }
}

fn round_duration(
    info: TemporalDurationInfo,
    unit: &str,
    increment: i64,
    mode: &str,
) -> TemporalDurationInfo {
    let Some(unit_ns) = unit_nanoseconds(unit) else {
        return info;
    };
    let total = duration_total_nanoseconds(&info);
    let mut rounded =
        duration_from_nanoseconds(round_to_increment(total, unit_ns * increment as i128, mode));
    rounded.years = info.years;
    rounded.months = info.months;
    rounded
}

fn round_time(info: TemporalTimeInfo, unit: &str, increment: i64, mode: &str) -> TemporalTimeInfo {
    let Some(unit_ns) = unit_nanoseconds(unit) else {
        return info;
    };
    let day_ns = 86_400_000_000_000i128;
    let ns = round_to_increment(time_nanoseconds(&info), unit_ns * increment as i128, mode);
    time_from_nanoseconds(ns.rem_euclid(day_ns))
}

fn round_to_increment(value: i128, quantum: i128, mode: &str) -> i128 {
    if quantum <= 1 {
        return value;
    }
    let quotient = value.div_euclid(quantum);
    let remainder = value.rem_euclid(quantum);
    match mode {
        "floor" => quotient * quantum,
        "ceil" => {
            if remainder == 0 {
                quotient * quantum
            } else {
                (quotient + 1) * quantum
            }
        }
        "trunc" => {
            if value >= 0 || remainder == 0 {
                quotient * quantum
            } else {
                (quotient + 1) * quantum
            }
        }
        "expand" => {
            if value >= 0 {
                if remainder == 0 {
                    quotient * quantum
                } else {
                    (quotient + 1) * quantum
                }
            } else {
                quotient * quantum
            }
        }
        _ => {
            let sign = if value < 0 { -1 } else { 1 };
            let rounded = ((value.abs() + quantum / 2) / quantum) * quantum;
            rounded * sign
        }
    }
}

fn zoned_date_time_to_plain_date_time(info: &TemporalZonedDateTimeInfo) -> TemporalDateTimeInfo {
    epoch_to_date_time(
        info.epoch_nanoseconds + info.time_zone.offset_nanoseconds.unwrap_or(0) as i128,
        info.calendar.clone(),
        info.calendar_object,
    )
}

fn zoned_date_time_to_plain_date_time_checked(
    vm: &mut VM,
    info: &TemporalZonedDateTimeInfo,
) -> Result<TemporalDateTimeInfo, RuntimeError> {
    let offset = time_zone_offset_nanoseconds_for(
        vm,
        &info.time_zone,
        TemporalInstantInfo {
            epoch_nanoseconds: info.epoch_nanoseconds,
        },
    )?;
    Ok(epoch_to_date_time(
        info.epoch_nanoseconds + offset as i128,
        info.calendar.clone(),
        info.calendar_object,
    ))
}

fn time_zone_offset_nanoseconds_for(
    vm: &mut VM,
    zone: &TemporalTimeZoneInfo,
    instant: TemporalInstantInfo,
) -> Result<i64, RuntimeError> {
    let Some(object) = zone.object else {
        return Ok(zone.offset_nanoseconds.unwrap_or(0));
    };
    let method = get_property(vm, object, "getOffsetNanosecondsFor")?;
    let custom_method = object.has_own_property("getOffsetNanosecondsFor")
        || !is_builtin_function_named(method, "getOffsetNanosecondsFor");
    if !custom_method {
        return Ok(zone.offset_nanoseconds.unwrap_or(0));
    }
    if !method.is_function_object() {
        return Err(vm
            .current_context
            .error_type("getOffsetNanosecondsFor must be callable"));
    }
    let instant = temporal_object_by_name(vm, "Instant", TemporalObjectKind::Instant(instant));
    let offset = vm.call_function(method, &[instant], object)?;
    if !offset.is_number() {
        return Err(vm
            .current_context
            .error_type("getOffsetNanosecondsFor must return a number"));
    }
    let offset = offset.into_number();
    if !offset.is_finite() || offset.trunc() != offset || offset.abs() >= 86_400_000_000_000.0 {
        return Err(vm.current_context.error_range("invalid time zone offset"));
    }
    Ok(offset as i64)
}

fn is_builtin_function_named(value: Value, name: &str) -> bool {
    if !value.is_object() {
        return false;
    }
    let info = value.get_object_info();
    let ObjectKind::Function(ref function) = info.kind else {
        return false;
    };
    matches!(function.kind, FunctionObjectKind::Builtin(_))
        && function.name.as_deref() == Some(name)
}

fn now_epoch_nanoseconds() -> i128 {
    let now = Utc::now();
    now.timestamp() as i128 * 1_000_000_000 + now.timestamp_subsec_nanos() as i128
}

fn now_date_time(
    vm: &mut VM,
    calendar: String,
    calendar_object: Option<Value>,
    time_zone: &TemporalTimeZoneInfo,
) -> Result<TemporalDateTimeInfo, RuntimeError> {
    let epoch_nanoseconds = now_epoch_nanoseconds();
    let offset =
        time_zone_offset_nanoseconds_for(vm, time_zone, TemporalInstantInfo { epoch_nanoseconds })?;
    Ok(epoch_to_date_time(
        epoch_nanoseconds + offset as i128,
        calendar,
        calendar_object,
    ))
}

fn now_calendar(
    vm: &mut VM,
    value: Option<Value>,
) -> Result<(String, Option<Value>), RuntimeError> {
    match value {
        Some(value) if !value.is_undefined() => calendar_identifier_and_object(vm, value),
        _ => Ok(("iso8601".to_string(), None)),
    }
}

fn now_time_zone(vm: &mut VM, value: Option<Value>) -> Result<TemporalTimeZoneInfo, RuntimeError> {
    match value {
        Some(value) if !value.is_undefined() => time_zone_identifier(vm, value),
        _ => {
            let utc = vm.factory.string("UTC");
            time_zone_identifier(vm, utc)
        }
    }
}

fn epoch_to_date_time(
    epoch_nanoseconds: i128,
    calendar: String,
    calendar_object: Option<Value>,
) -> TemporalDateTimeInfo {
    let day_ns = 86_400_000_000_000i128;
    let days = floor_div_i128(epoch_nanoseconds, day_ns);
    let time_ns = epoch_nanoseconds.rem_euclid(day_ns);
    let (year, month, day) = civil_from_days(days as i64);
    TemporalDateTimeInfo {
        date: TemporalDateInfo {
            year,
            month,
            day,
            calendar,
            calendar_object,
        },
        time: time_from_nanoseconds(time_ns),
    }
}

fn date_time_to_epoch_nanoseconds(info: &TemporalDateTimeInfo) -> i128 {
    days_from_civil(info.date.year, info.date.month, info.date.day) as i128 * 86_400_000_000_000
        + time_nanoseconds(&info.time)
}

fn time_from_nanoseconds(ns: i128) -> TemporalTimeInfo {
    let hour = ns / 3_600_000_000_000;
    let ns = ns % 3_600_000_000_000;
    let minute = ns / 60_000_000_000;
    let ns = ns % 60_000_000_000;
    let second = ns / 1_000_000_000;
    let ns = ns % 1_000_000_000;
    TemporalTimeInfo {
        hour: hour as u8,
        minute: minute as u8,
        second: second as u8,
        millisecond: (ns / 1_000_000) as u16,
        microsecond: ((ns / 1_000) % 1_000) as u16,
        nanosecond: (ns % 1_000) as u16,
    }
}

fn time_nanoseconds(time: &TemporalTimeInfo) -> i128 {
    time.hour as i128 * 3_600_000_000_000
        + time.minute as i128 * 60_000_000_000
        + time.second as i128 * 1_000_000_000
        + time.millisecond as i128 * 1_000_000
        + time.microsecond as i128 * 1_000
        + time.nanosecond as i128
}

fn duration_time_nanoseconds(duration: &TemporalDurationInfo) -> i128 {
    duration.hours as i128 * 3_600_000_000_000
        + duration.minutes as i128 * 60_000_000_000
        + duration.seconds as i128 * 1_000_000_000
        + duration.milliseconds as i128 * 1_000_000
        + duration.microseconds as i128 * 1_000
        + duration.nanoseconds as i128
}

fn duration_total_nanoseconds(duration: &TemporalDurationInfo) -> i128 {
    (duration.days + duration.weeks * 7) as i128 * 86_400_000_000_000
        + duration_time_nanoseconds(duration)
}

fn duration_from_days(days: i64) -> TemporalDurationInfo {
    TemporalDurationInfo {
        years: 0,
        months: 0,
        weeks: 0,
        days,
        hours: 0,
        minutes: 0,
        seconds: 0,
        milliseconds: 0,
        microseconds: 0,
        nanoseconds: 0,
    }
}

fn duration_from_nanoseconds(value: i128) -> TemporalDurationInfo {
    let sign = if value < 0 { -1 } else { 1 };
    let mut rest = value.abs();
    let days = rest / 86_400_000_000_000;
    rest %= 86_400_000_000_000;
    let hours = rest / 3_600_000_000_000;
    rest %= 3_600_000_000_000;
    let minutes = rest / 60_000_000_000;
    rest %= 60_000_000_000;
    let seconds = rest / 1_000_000_000;
    rest %= 1_000_000_000;
    let milliseconds = rest / 1_000_000;
    rest %= 1_000_000;
    let microseconds = rest / 1_000;
    let nanoseconds = rest % 1_000;
    TemporalDurationInfo {
        years: 0,
        months: 0,
        weeks: 0,
        days: days as i64 * sign,
        hours: hours as i64 * sign,
        minutes: minutes as i64 * sign,
        seconds: seconds as i64 * sign,
        milliseconds: milliseconds as i64 * sign,
        microseconds: microseconds as i64 * sign,
        nanoseconds: nanoseconds as i64 * sign,
    }
}

fn balance_duration_to_largest_unit(
    duration: TemporalDurationInfo,
    largest_unit: &str,
) -> TemporalDurationInfo {
    let Some(unit_ns) = unit_nanoseconds(largest_unit) else {
        return duration;
    };
    let total = duration_total_nanoseconds(&duration);
    let sign = if total < 0 { -1 } else { 1 };
    let mut rest = total.abs();
    let largest = (rest / unit_ns) as i64 * sign;
    rest %= unit_ns;
    let mut balanced = duration_from_nanoseconds(rest * sign as i128);
    match largest_unit {
        "day" => {
            balanced.days += largest;
        }
        "hour" => {
            balanced.hours += largest;
        }
        "minute" => {
            balanced.minutes += largest;
        }
        "second" => {
            balanced.seconds += largest;
        }
        "millisecond" => {
            balanced.milliseconds += largest;
        }
        "microsecond" => {
            balanced.microseconds += largest;
        }
        "nanosecond" => {
            balanced.nanoseconds += largest;
        }
        _ => return duration,
    }
    balanced.years = duration.years;
    balanced.months = duration.months;
    balanced.weeks = duration.weeks;
    balanced
}

fn duration_fields(duration: &TemporalDurationInfo) -> [i64; 10] {
    [
        duration.years,
        duration.months,
        duration.weeks,
        duration.days,
        duration.hours,
        duration.minutes,
        duration.seconds,
        duration.milliseconds,
        duration.microseconds,
        duration.nanoseconds,
    ]
}

fn duration_sign(duration: &TemporalDurationInfo) -> i64 {
    for value in duration_fields(duration) {
        if value != 0 {
            return value.signum();
        }
    }
    0
}

fn date_equal(lhs: &TemporalDateInfo, rhs: &TemporalDateInfo) -> bool {
    lhs.year == rhs.year && lhs.month == rhs.month && lhs.day == rhs.day
}

fn time_equal(lhs: &TemporalTimeInfo, rhs: &TemporalTimeInfo) -> bool {
    lhs.hour == rhs.hour
        && lhs.minute == rhs.minute
        && lhs.second == rhs.second
        && lhs.millisecond == rhs.millisecond
        && lhs.microsecond == rhs.microsecond
        && lhs.nanosecond == rhs.nanosecond
}

fn date_time_equal(lhs: &TemporalDateTimeInfo, rhs: &TemporalDateTimeInfo) -> bool {
    date_equal(&lhs.date, &rhs.date) && time_equal(&lhs.time, &rhs.time)
}

fn duration_equal(lhs: &TemporalDurationInfo, rhs: &TemporalDurationInfo) -> bool {
    duration_fields(lhs) == duration_fields(rhs)
}

fn compare_i64(lhs: i64, rhs: i64) -> i32 {
    if lhs < rhs {
        -1
    } else if lhs > rhs {
        1
    } else {
        0
    }
}

fn compare_i128(lhs: i128, rhs: i128) -> i32 {
    if lhs < rhs {
        -1
    } else if lhs > rhs {
        1
    } else {
        0
    }
}

fn compare_date_time(lhs: &TemporalDateTimeInfo, rhs: &TemporalDateTimeInfo) -> i32 {
    let date_cmp = compare_i64(iso_date_key(&lhs.date), iso_date_key(&rhs.date));
    if date_cmp != 0 {
        return date_cmp;
    }
    compare_i128(time_nanoseconds(&lhs.time), time_nanoseconds(&rhs.time))
}

fn iso_date_key(date: &TemporalDateInfo) -> i64 {
    days_from_civil(date.year, date.month, date.day)
}

fn year_month_key(info: &TemporalYearMonthInfo) -> i64 {
    info.year as i64 * 12 + info.month as i64
}

fn is_leap_year(year: i32) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}

fn days_in_month(year: i32, month: u8) -> u8 {
    match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 if is_leap_year(year) => 29,
        2 => 28,
        _ => 0,
    }
}

fn days_in_year(year: i32) -> u16 {
    if is_leap_year(year) {
        366
    } else {
        365
    }
}

fn day_of_year(year: i32, month: u8, day: u8) -> u16 {
    let mut total = day as u16;
    for m in 1..month {
        total += days_in_month(year, m) as u16;
    }
    total
}

fn day_of_week(year: i32, month: u8, day: u8) -> u8 {
    ((days_from_civil(year, month, day) + 3).rem_euclid(7) + 1) as u8
}

fn iso_week_fields(date: &TemporalDateInfo) -> (u8, i32) {
    let mut year = date.year;
    let day = day_of_year(date.year, date.month, date.day) as i32;
    let weekday = day_of_week(date.year, date.month, date.day) as i32;
    let mut week = (day - weekday + 10).div_euclid(7);
    if week < 1 {
        year -= 1;
        week = iso_weeks_in_year(year) as i32;
    } else {
        let weeks = iso_weeks_in_year(year) as i32;
        if week > weeks {
            year += 1;
            week = 1;
        }
    }
    (week as u8, year)
}

fn iso_weeks_in_year(year: i32) -> u8 {
    let jan_1 = day_of_week(year, 1, 1);
    if jan_1 == 4 || (jan_1 == 3 && is_leap_year(year)) {
        53
    } else {
        52
    }
}

fn days_from_civil(year: i32, month: u8, day: u8) -> i64 {
    let y = year as i64 - (month <= 2) as i64;
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let m = month as i64;
    let doy = (153 * (m + if m > 2 { -3 } else { 9 }) + 2) / 5 + day as i64 - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146097 + doe - 719468
}

fn civil_from_days(days: i64) -> (i32, u8, u8) {
    let z = days + 719468;
    let era = z.div_euclid(146097);
    let doe = z - era * 146097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let day = doy - (153 * mp + 2) / 5 + 1;
    let month = mp + if mp < 10 { 3 } else { -9 };
    let year = y + (month <= 2) as i64;
    (year as i32, month as u8, day as u8)
}

fn floor_div_i128(lhs: i128, rhs: i128) -> i128 {
    lhs.div_euclid(rhs)
}

fn format_year(year: i32) -> String {
    if (0..=9999).contains(&year) {
        format!("{:04}", year)
    } else if year < 0 {
        format!("-{:06}", -(year as i64))
    } else {
        format!("+{:06}", year)
    }
}

fn two_digits(value: u8) -> String {
    format!("{:02}", value)
}

fn format_month_code(month: u8) -> String {
    format!("M{:02}", month)
}

fn format_date(year: i32, month: u8, day: u8) -> String {
    format!(
        "{}-{}-{}",
        format_year(year),
        two_digits(month),
        two_digits(day)
    )
}

fn format_time(time: &TemporalTimeInfo) -> String {
    let fraction = time.millisecond as u32 * 1_000_000
        + time.microsecond as u32 * 1_000
        + time.nanosecond as u32;
    if fraction == 0 {
        format!(
            "{}:{}:{}",
            two_digits(time.hour),
            two_digits(time.minute),
            two_digits(time.second)
        )
    } else {
        let mut text = format!("{:09}", fraction);
        while text.ends_with('0') {
            text.pop();
        }
        format!(
            "{}:{}:{}.{}",
            two_digits(time.hour),
            two_digits(time.minute),
            two_digits(time.second),
            text
        )
    }
}

fn format_date_time(info: &TemporalDateTimeInfo) -> String {
    format!(
        "{}T{}",
        format_date(info.date.year, info.date.month, info.date.day),
        format_time(&info.time)
    )
}

fn format_instant(epoch_nanoseconds: i128) -> String {
    let date_time = epoch_to_date_time(epoch_nanoseconds, "iso8601".to_string(), None);
    format!("{}Z", format_date_time(&date_time))
}

fn format_zoned_date_time(info: &TemporalZonedDateTimeInfo) -> String {
    let date_time = zoned_date_time_to_plain_date_time(info);
    format!(
        "{}{}[{}]",
        format_date_time(&date_time),
        format_offset(info.time_zone.offset_nanoseconds.unwrap_or(0)),
        info.time_zone.identifier
    )
}

fn format_offset(offset_nanoseconds: i64) -> String {
    let sign = if offset_nanoseconds < 0 { '-' } else { '+' };
    let total_seconds = offset_nanoseconds.abs() / 1_000_000_000;
    let hour = total_seconds / 3600;
    let minute = (total_seconds / 60) % 60;
    let second = total_seconds % 60;
    if second == 0 {
        format!("{}{:02}:{:02}", sign, hour, minute)
    } else {
        format!("{}{:02}:{:02}:{:02}", sign, hour, minute, second)
    }
}

fn format_duration(info: &TemporalDurationInfo) -> String {
    if duration_sign(info) == 0 {
        return "PT0S".to_string();
    }
    let sign = if duration_sign(info) < 0 { "-" } else { "" };
    let abs = map_duration(info.clone(), i64::abs);
    let mut date = String::new();
    if abs.years != 0 {
        date.push_str(&format!("{}Y", abs.years));
    }
    if abs.months != 0 {
        date.push_str(&format!("{}M", abs.months));
    }
    if abs.weeks != 0 {
        date.push_str(&format!("{}W", abs.weeks));
    }
    if abs.days != 0 {
        date.push_str(&format!("{}D", abs.days));
    }
    let mut time = String::new();
    if abs.hours != 0 {
        time.push_str(&format!("{}H", abs.hours));
    }
    if abs.minutes != 0 {
        time.push_str(&format!("{}M", abs.minutes));
    }
    if abs.seconds != 0 || abs.milliseconds != 0 || abs.microseconds != 0 || abs.nanoseconds != 0 {
        let fraction = abs.milliseconds as u32 * 1_000_000
            + abs.microseconds as u32 * 1_000
            + abs.nanoseconds as u32;
        if fraction == 0 {
            time.push_str(&format!("{}S", abs.seconds));
        } else {
            let mut text = format!("{:09}", fraction);
            while text.ends_with('0') {
                text.pop();
            }
            time.push_str(&format!("{}.{}S", abs.seconds, text));
        }
    }
    if time.is_empty() {
        format!("{}P{}", sign, date)
    } else {
        format!("{}P{}T{}", sign, date, time)
    }
}
