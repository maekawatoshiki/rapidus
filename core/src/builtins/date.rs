use super::helpers::{to_number, to_object};
use crate::vm::{
    error::RuntimeError,
    jsvalue::value::*,
    vm::{Factory, VMValueResult, VM},
};
use chrono::{DateTime, Datelike, Timelike, Utc};

const MAX_DATE_MILLIS: f64 = 8.64e15;
const MS_PER_SECOND: i64 = 1_000;
const MS_PER_MINUTE: i64 = 60 * MS_PER_SECOND;
const MS_PER_HOUR: i64 = 60 * MS_PER_MINUTE;
const MS_PER_DAY: i64 = 24 * MS_PER_HOUR;

pub fn date(factory: &mut Factory) -> Value {
    let constructor = factory.generate_builtin_constructor(
        "Date",
        date_constructor,
        factory.object_prototypes.date,
    );
    let now = factory.builtin_function("now", date_now);
    let parse = factory.builtin_function("parse", date_parse);
    let utc = factory.builtin_function("UTC", date_utc);
    constructor.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(7.0)).set_configurable()),
    );
    now.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
    );
    parse.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
    );
    utc.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(7.0)).set_configurable()),
    );
    for (name, func) in [("now", now), ("parse", parse), ("UTC", utc)] {
        constructor.get_object_info().insert_property(
            name.to_string(),
            Property::new_data(DataProperty::new(func).set_writable().set_configurable()),
        );
    }
    factory
        .object_prototypes
        .date
        .get_object_info()
        .insert_property(
            "constructor".to_string(),
            Property::new_data(
                DataProperty::new(constructor)
                    .set_writable()
                    .set_configurable(),
            ),
        );
    constructor
}

pub fn date_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    if !vm.builtin_constructor_call {
        return Ok(vm.factory.string(DateObjectInfo::default().to_string()));
    }

    let millis = match args.len() {
        0 => return Ok(vm.factory.date()),
        1 => date_millis_from_single_arg(vm, args[0])?,
        _ => date_millis_from_parts(vm, args)?,
    };
    Ok(vm
        .factory
        .date_from_millis(millis.map(|millis| millis as f64).unwrap_or(f64::NAN)))
}

pub fn date_now(_vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    Ok(Value::Number(Utc::now().timestamp_millis() as f64))
}

pub fn date_parse(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let text = args.get(0).unwrap_or(&Value::undefined()).to_string();
    let millis = parse_date_millis(&text)
        .map(|millis| millis as f64)
        .unwrap_or(f64::NAN);
    Ok(Value::Number(millis))
}

pub fn date_utc(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let year = number_arg(vm, args, 0, f64::NAN)?;
    let month = number_arg(vm, args, 1, 0.0)?;
    let date = number_arg(vm, args, 2, 1.0)?;
    let hours = number_arg(vm, args, 3, 0.0)?;
    let minutes = number_arg(vm, args, 4, 0.0)?;
    let seconds = number_arg(vm, args, 5, 0.0)?;
    let millis = number_arg(vm, args, 6, 0.0)?;
    Ok(Value::Number(
        make_date_millis(year, month, date, hours, minutes, seconds, millis)
            .map(|millis| millis as f64)
            .unwrap_or(f64::NAN),
    ))
}

pub fn date_get_hours(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    date_get_utc_hours(vm, _args, this)
}

pub fn date_get_minutes(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    date_get_utc_minutes(vm, _args, this)
}

pub fn date_get_time(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_info(vm, this)?;
    Ok(Value::Number(
        date.millis()
            .map(|millis| millis as f64)
            .unwrap_or(f64::NAN),
    ))
}

pub fn date_value_of(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    date_get_time(vm, args, this)
}

pub fn date_get_utc_full_year(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    date_part(vm, this, |date| date.year() as f64)
}

pub fn date_get_full_year(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    date_get_utc_full_year(vm, args, this)
}

pub fn date_get_utc_month(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    date_part(vm, this, |date| date.month0() as f64)
}

pub fn date_get_month(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    date_get_utc_month(vm, args, this)
}

pub fn date_get_utc_date(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    date_part(vm, this, |date| date.day() as f64)
}

pub fn date_get_date(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    date_get_utc_date(vm, args, this)
}

pub fn date_get_utc_day(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    date_part(vm, this, |date| {
        date.weekday().num_days_from_sunday() as f64
    })
}

pub fn date_get_day(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    date_get_utc_day(vm, args, this)
}

pub fn date_get_utc_hours(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    date_part(vm, this, |date| date.hour() as f64)
}

pub fn date_get_utc_minutes(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    date_part(vm, this, |date| date.minute() as f64)
}

pub fn date_get_utc_seconds(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    date_part(vm, this, |date| date.second() as f64)
}

pub fn date_get_seconds(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    date_get_utc_seconds(vm, args, this)
}

pub fn date_get_utc_milliseconds(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    date_part(vm, this, |date| date.timestamp_subsec_millis() as f64)
}

pub fn date_get_milliseconds(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    date_get_utc_milliseconds(vm, args, this)
}

pub fn date_get_timezone_offset(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_info(vm, this)?;
    Ok(Value::Number(if date.millis().is_some() {
        0.0
    } else {
        f64::NAN
    }))
}

pub fn date_get_year(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    date_part(vm, this, |date| (date.year() - 1900) as f64)
}

pub fn date_set_time(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !this.is_date_object() {
        return Err(vm.current_context.error_type("Date method receiver"));
    }
    let time = to_number(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let millis = if time.is_finite() {
        Some(time.trunc() as i64)
    } else {
        None
    };
    this.as_date_mut().unwrap().set_millis(millis);
    Ok(Value::Number(
        millis.map(|millis| millis as f64).unwrap_or(f64::NAN),
    ))
}

pub fn date_set_year(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !this.is_date_object() {
        return Err(vm.current_context.error_type("Date method receiver"));
    }
    let year = to_number(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    if year.is_nan() {
        this.as_date_mut().unwrap().set_millis(None);
        return Ok(Value::Number(f64::NAN));
    }

    let base_millis = this.as_date().unwrap().millis().unwrap_or(0);
    let base = DateObjectInfo::from_millis(base_millis as f64)
        .utc()
        .unwrap();
    let millis = make_date_millis(
        year,
        base.month0() as f64,
        base.day() as f64,
        base.hour() as f64,
        base.minute() as f64,
        base.second() as f64,
        base.timestamp_subsec_millis() as f64,
    );
    this.as_date_mut().unwrap().set_millis(millis);
    Ok(Value::Number(
        millis.map(|millis| millis as f64).unwrap_or(f64::NAN),
    ))
}

pub fn date_set_milliseconds(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_time_component(vm, args, this, TimeComponent::Milliseconds, false)
}

pub fn date_set_utc_milliseconds(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_time_component(vm, args, this, TimeComponent::Milliseconds, true)
}

pub fn date_set_seconds(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_time_component(vm, args, this, TimeComponent::Seconds, false)
}

pub fn date_set_utc_seconds(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_time_component(vm, args, this, TimeComponent::Seconds, true)
}

pub fn date_set_minutes(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_time_component(vm, args, this, TimeComponent::Minutes, false)
}

pub fn date_set_utc_minutes(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_time_component(vm, args, this, TimeComponent::Minutes, true)
}

pub fn date_set_hours(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_time_component(vm, args, this, TimeComponent::Hours, false)
}

pub fn date_set_utc_hours(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_time_component(vm, args, this, TimeComponent::Hours, true)
}

pub fn date_set_date(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_date_component(vm, args, this, DateComponent::Date, false)
}

pub fn date_set_utc_date(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_date_component(vm, args, this, DateComponent::Date, true)
}

pub fn date_set_month(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_date_component(vm, args, this, DateComponent::Month, false)
}

pub fn date_set_utc_month(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_date_component(vm, args, this, DateComponent::Month, true)
}

pub fn date_set_full_year(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_date_component(vm, args, this, DateComponent::FullYear, false)
}

pub fn date_set_utc_full_year(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_date_component(vm, args, this, DateComponent::FullYear, true)
}

pub fn date_to_iso_string(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_info(vm, this)?;
    let Some(utc) = date.utc() else {
        return Err(vm.current_context.error_range("Invalid Date"));
    };
    Ok(vm
        .factory
        .string(utc.format("%Y-%m-%dT%H:%M:%S%.3fZ").to_string()))
}

pub fn date_to_json(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let object = to_object(vm, this)?;
    let primitive = vm.to_primitive(object, "number")?;
    if primitive.is_number() && !primitive.into_number().is_finite() {
        return Ok(Value::null());
    }

    let key = vm.factory.string("toISOString");
    let to_iso_string = vm.get_property_by_value(object, key)?;
    if !vm.is_callable(to_iso_string) {
        return Err(vm
            .current_context
            .error_type("Date.prototype.toJSON toISOString"));
    }
    vm.call_function(to_iso_string, &[], object)
}

pub fn date_to_string(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let date = date_info(vm, this)?;
    Ok(vm.factory.string(date.to_string()))
}

pub fn date_to_utc_string(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let _ = args;
    let date = date_info(vm, this)?;
    let Some(utc) = date.utc() else {
        return Ok(vm.factory.string("Invalid Date"));
    };
    Ok(vm.factory.string(format_utc_string(utc)))
}

pub fn date_to_date_string(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    date_to_string(vm, args, this)
}

pub fn date_to_time_string(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    date_to_string(vm, args, this)
}

pub fn date_to_locale_string(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    date_to_string(vm, args, this)
}

pub fn date_to_primitive(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !this.is_object() {
        return Err(vm.current_context.error_type("Date @@toPrimitive"));
    }
    let hint = args
        .get(0)
        .copied()
        .unwrap_or(Value::undefined())
        .to_string();
    let methods = match hint.as_str() {
        "string" | "default" => ["toString", "valueOf"],
        "number" => ["valueOf", "toString"],
        _ => return Err(vm.current_context.error_type("Date @@toPrimitive hint")),
    };

    for method_name in methods {
        let key = vm.factory.string(method_name.to_string());
        let method = vm.get_property_by_value(this, key)?;
        if method.is_function_object() {
            let value = vm.call_function(method, &[], this)?;
            if !value.is_object() {
                return Ok(value);
            }
        }
    }
    Err(vm.current_context.error_type("Date @@toPrimitive"))
}

fn date_info(
    vm: &mut VM,
    this: Value,
) -> Result<crate::vm::jsvalue::date::DateObjectInfo, crate::vm::error::RuntimeError> {
    if !this.is_date_object() {
        return Err(vm.current_context.error_type("Date method receiver"));
    }
    Ok(this.as_date().unwrap().clone())
}

fn date_part(
    vm: &mut VM,
    this: Value,
    f: impl FnOnce(chrono::DateTime<chrono::Utc>) -> f64,
) -> VMValueResult {
    let date = date_info(vm, this)?;
    Ok(Value::Number(date.utc().map(f).unwrap_or(f64::NAN)))
}

#[derive(Clone, Copy)]
enum TimeComponent {
    Milliseconds,
    Seconds,
    Minutes,
    Hours,
}

#[derive(Clone, Copy)]
enum DateComponent {
    Date,
    Month,
    FullYear,
}

fn set_time_component(
    vm: &mut VM,
    args: &[Value],
    this: Value,
    component: TimeComponent,
    _utc: bool,
) -> VMValueResult {
    if !this.is_date_object() {
        return Err(vm.current_context.error_type("Date method receiver"));
    }

    let current = this.as_date().unwrap().millis();
    let parts = current.and_then(|millis| DateObjectInfo::from_millis(millis as f64).utc());
    let Some(parts) = parts else {
        let _ = number_arg(vm, args, 0, f64::NAN)?;
        this.as_date_mut().unwrap().set_millis(None);
        return Ok(Value::Number(f64::NAN));
    };

    let mut hours = parts.hour() as f64;
    let mut minutes = parts.minute() as f64;
    let mut seconds = parts.second() as f64;
    let mut millis = parts.timestamp_subsec_millis() as f64;

    match component {
        TimeComponent::Milliseconds => {
            millis = number_arg(vm, args, 0, f64::NAN)?;
        }
        TimeComponent::Seconds => {
            seconds = number_arg(vm, args, 0, f64::NAN)?;
            millis = number_arg(vm, args, 1, millis)?;
        }
        TimeComponent::Minutes => {
            minutes = number_arg(vm, args, 0, f64::NAN)?;
            seconds = number_arg(vm, args, 1, seconds)?;
            millis = number_arg(vm, args, 2, millis)?;
        }
        TimeComponent::Hours => {
            hours = number_arg(vm, args, 0, f64::NAN)?;
            minutes = number_arg(vm, args, 1, minutes)?;
            seconds = number_arg(vm, args, 2, seconds)?;
            millis = number_arg(vm, args, 3, millis)?;
        }
    }

    let new_millis = make_date_millis_without_year_adjust(
        parts.year() as f64,
        parts.month0() as f64,
        parts.day() as f64,
        hours,
        minutes,
        seconds,
        millis,
    );
    set_date_millis(this, new_millis)
}

fn set_date_component(
    vm: &mut VM,
    args: &[Value],
    this: Value,
    component: DateComponent,
    _utc: bool,
) -> VMValueResult {
    if !this.is_date_object() {
        return Err(vm.current_context.error_type("Date method receiver"));
    }

    let current = this.as_date().unwrap().millis();
    let use_zero_for_invalid = matches!(component, DateComponent::FullYear);
    let parts = current
        .or_else(|| use_zero_for_invalid.then_some(0))
        .and_then(|millis| DateObjectInfo::from_millis(millis as f64).utc());
    let Some(parts) = parts else {
        let _ = number_arg(vm, args, 0, f64::NAN)?;
        this.as_date_mut().unwrap().set_millis(None);
        return Ok(Value::Number(f64::NAN));
    };

    let mut year = parts.year() as f64;
    let mut month = parts.month0() as f64;
    let mut date = parts.day() as f64;

    match component {
        DateComponent::Date => {
            date = number_arg(vm, args, 0, f64::NAN)?;
        }
        DateComponent::Month => {
            month = number_arg(vm, args, 0, f64::NAN)?;
            date = number_arg(vm, args, 1, date)?;
        }
        DateComponent::FullYear => {
            year = number_arg(vm, args, 0, f64::NAN)?;
            month = number_arg(vm, args, 1, month)?;
            date = number_arg(vm, args, 2, date)?;
        }
    }

    let new_millis = make_date_millis_without_year_adjust(
        year,
        month,
        date,
        parts.hour() as f64,
        parts.minute() as f64,
        parts.second() as f64,
        parts.timestamp_subsec_millis() as f64,
    );
    set_date_millis(this, new_millis)
}

fn set_date_millis(this: Value, millis: Option<i64>) -> VMValueResult {
    this.as_date_mut().unwrap().set_millis(millis);
    Ok(Value::Number(
        millis.map(|millis| millis as f64).unwrap_or(f64::NAN),
    ))
}

fn parse_date_millis(text: &str) -> Option<i64> {
    let text = text.trim();
    if let Ok(date) = DateTime::parse_from_rfc3339(text) {
        return time_clip(date.timestamp_millis() as f64);
    }

    parse_ecma_date_time(text)
        .or_else(|| parse_utc_string(text))
        .and_then(|millis| time_clip(millis as f64))
}

fn parse_ecma_date_time(text: &str) -> Option<i64> {
    let mut pos = 0;
    let year = parse_year(text, &mut pos)?;
    if pos == text.len() {
        return make_date_millis_without_year_adjust(year as f64, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0);
    }

    expect_byte(text, &mut pos, b'-')?;
    let month = parse_fixed_digits(text, &mut pos, 2)? as u32;
    let mut day = 1;
    if pos < text.len() {
        expect_byte(text, &mut pos, b'-')?;
        day = parse_fixed_digits(text, &mut pos, 2)? as u32;
    }

    let mut hour = 0;
    let mut minute = 0;
    let mut second = 0;
    let mut millis = 0;
    let mut offset_minutes = 0i64;

    if pos < text.len() {
        if text.as_bytes().get(pos) != Some(&b'T') {
            return None;
        }
        pos += 1;
        hour = parse_fixed_digits(text, &mut pos, 2)? as u32;
        expect_byte(text, &mut pos, b':')?;
        minute = parse_fixed_digits(text, &mut pos, 2)? as u32;

        if text.as_bytes().get(pos) == Some(&b':') {
            pos += 1;
            second = parse_fixed_digits(text, &mut pos, 2)? as u32;
        }

        if text.as_bytes().get(pos) == Some(&b'.') {
            pos += 1;
            let start = pos;
            while text
                .as_bytes()
                .get(pos)
                .is_some_and(|byte| byte.is_ascii_digit())
            {
                pos += 1;
            }
            if pos == start {
                return None;
            }
            let fraction = &text[start..pos];
            let mut padded = fraction.chars().take(3).collect::<String>();
            while padded.len() < 3 {
                padded.push('0');
            }
            millis = padded.parse::<i64>().ok()?;
        }

        if text.as_bytes().get(pos) == Some(&b'Z') {
            pos += 1;
        } else if matches!(text.as_bytes().get(pos), Some(b'+') | Some(b'-')) {
            let sign = if text.as_bytes()[pos] == b'-' { -1 } else { 1 };
            pos += 1;
            let offset_hour = parse_fixed_digits(text, &mut pos, 2)? as i64;
            expect_byte(text, &mut pos, b':')?;
            let offset_minute = parse_fixed_digits(text, &mut pos, 2)? as i64;
            offset_minutes = sign * (offset_hour * 60 + offset_minute);
        }
    }

    if pos != text.len() {
        return None;
    }

    make_date_millis_without_year_adjust(
        year as f64,
        month as f64 - 1.0,
        day as f64,
        hour as f64,
        minute as f64,
        second as f64,
        millis as f64,
    )?
    .checked_sub(offset_minutes.checked_mul(60_000)?)
}

fn parse_year(text: &str, pos: &mut usize) -> Option<i32> {
    let sign = match text.as_bytes().get(*pos) {
        Some(b'+') => {
            *pos += 1;
            1
        }
        Some(b'-') => {
            *pos += 1;
            if text.get(*pos..*pos + 6) == Some("000000") {
                return None;
            }
            -1
        }
        _ => 1,
    };
    let digits = if sign == 1 && *pos == 0 { 4 } else { 6 };
    let year = parse_fixed_digits(text, pos, digits)?;
    Some(sign * year)
}

fn parse_fixed_digits(text: &str, pos: &mut usize, digits: usize) -> Option<i32> {
    let end = pos.checked_add(digits)?;
    let slice = text.get(*pos..end)?;
    if !slice.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    *pos = end;
    slice.parse::<i32>().ok()
}

fn parse_utc_string(text: &str) -> Option<i64> {
    let parts = text.split(' ').collect::<Vec<_>>();
    if parts.len() != 6 || parts[5] != "GMT" {
        return None;
    }
    if !matches!(
        parts[0],
        "Sun," | "Mon," | "Tue," | "Wed," | "Thu," | "Fri," | "Sat,"
    ) {
        return None;
    }
    let day = parts[1].parse::<f64>().ok()?;
    let month = match parts[2] {
        "Jan" => 0.0,
        "Feb" => 1.0,
        "Mar" => 2.0,
        "Apr" => 3.0,
        "May" => 4.0,
        "Jun" => 5.0,
        "Jul" => 6.0,
        "Aug" => 7.0,
        "Sep" => 8.0,
        "Oct" => 9.0,
        "Nov" => 10.0,
        "Dec" => 11.0,
        _ => return None,
    };
    let year = parts[3].parse::<f64>().ok()?;
    let time = parts[4].split(':').collect::<Vec<_>>();
    if time.len() != 3 {
        return None;
    }
    make_date_millis_without_year_adjust(
        year,
        month,
        day,
        time[0].parse::<f64>().ok()?,
        time[1].parse::<f64>().ok()?,
        time[2].parse::<f64>().ok()?,
        0.0,
    )
}

fn expect_byte(text: &str, pos: &mut usize, expected: u8) -> Option<()> {
    if text.as_bytes().get(*pos) == Some(&expected) {
        *pos += 1;
        Some(())
    } else {
        None
    }
}

fn time_clip(millis: f64) -> Option<i64> {
    if millis.is_finite() && millis.abs() <= MAX_DATE_MILLIS {
        Some(millis.trunc() as i64)
    } else {
        None
    }
}

fn date_millis_from_single_arg(vm: &mut VM, value: Value) -> Result<Option<i64>, RuntimeError> {
    if value.is_date_object() {
        Ok(value.as_date().unwrap().millis())
    } else if value.is_string() {
        Ok(parse_date_millis(value.into_str()))
    } else {
        Ok(time_clip(to_number(vm, value)?))
    }
}

fn date_millis_from_parts(vm: &mut VM, args: &[Value]) -> Result<Option<i64>, RuntimeError> {
    let year = number_arg(vm, args, 0, f64::NAN)?;
    let month = number_arg(vm, args, 1, f64::NAN)?;
    let date = number_arg(vm, args, 2, 1.0)?;
    let hours = number_arg(vm, args, 3, 0.0)?;
    let minutes = number_arg(vm, args, 4, 0.0)?;
    let seconds = number_arg(vm, args, 5, 0.0)?;
    let millis = number_arg(vm, args, 6, 0.0)?;
    Ok(make_date_millis(
        year, month, date, hours, minutes, seconds, millis,
    ))
}

fn number_arg(
    vm: &mut VM,
    args: &[Value],
    index: usize,
    default: f64,
) -> Result<f64, RuntimeError> {
    args.get(index)
        .copied()
        .map(|value| to_number(vm, value))
        .unwrap_or(Ok(default))
}

fn make_date_millis(
    year: f64,
    month: f64,
    date: f64,
    hours: f64,
    minutes: f64,
    seconds: f64,
    millis: f64,
) -> Option<i64> {
    make_date_millis_inner(year, month, date, hours, minutes, seconds, millis, true)
}

fn make_date_millis_without_year_adjust(
    year: f64,
    month: f64,
    date: f64,
    hours: f64,
    minutes: f64,
    seconds: f64,
    millis: f64,
) -> Option<i64> {
    make_date_millis_inner(year, month, date, hours, minutes, seconds, millis, false)
}

fn make_date_millis_inner(
    year: f64,
    month: f64,
    date: f64,
    hours: f64,
    minutes: f64,
    seconds: f64,
    millis: f64,
    adjust_two_digit_year: bool,
) -> Option<i64> {
    let mut year = to_i64(year)?;
    if adjust_two_digit_year && (0..=99).contains(&year) {
        year += 1900;
    }
    let month = to_i64(month)?;
    let date = to_i64(date)?;
    let hours = to_i64(hours)?;
    let minutes = to_i64(minutes)?;
    let seconds = to_i64(seconds)?;
    let millis = to_i64(millis)?;

    let year = year.checked_add(month.div_euclid(12))?;
    let month = month.rem_euclid(12) as u32 + 1;
    let date_offset = date.checked_sub(1)?.checked_mul(MS_PER_DAY)?;
    let time_offset = hours
        .checked_mul(MS_PER_HOUR)?
        .checked_add(minutes.checked_mul(MS_PER_MINUTE)?)?
        .checked_add(seconds.checked_mul(MS_PER_SECOND)?)?
        .checked_add(millis)?;

    let base_days = days_from_civil(year, month as i64, 1)?;
    let millis = base_days
        .checked_mul(MS_PER_DAY)?
        .checked_add(date_offset)?
        .checked_add(time_offset)?;
    time_clip(millis as f64)
}

fn to_i64(number: f64) -> Option<i64> {
    if number.is_finite() && number.abs() <= i64::MAX as f64 {
        Some(number.trunc() as i64)
    } else {
        None
    }
}

fn days_from_civil(mut year: i64, month: i64, day: i64) -> Option<i64> {
    year -= if month <= 2 { 1 } else { 0 };
    let era = year.div_euclid(400);
    let year_of_era = year - era * 400;
    let month_for_day = month + if month > 2 { -3 } else { 9 };
    let day_of_year = (153 * month_for_day + 2) / 5 + day - 1;
    let day_of_era = year_of_era * 365 + year_of_era / 4 - year_of_era / 100 + day_of_year;
    era.checked_mul(146_097)?
        .checked_add(day_of_era)?
        .checked_sub(719_468)
}

fn format_utc_string(date: chrono::DateTime<chrono::Utc>) -> String {
    let weekday = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
        [date.weekday().num_days_from_sunday() as usize];
    let month = [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    ][date.month0() as usize];
    format!(
        "{}, {:02} {} {} {:02}:{:02}:{:02} GMT",
        weekday,
        date.day(),
        month,
        format_utc_year(date.year()),
        date.hour(),
        date.minute(),
        date.second(),
    )
}

fn format_utc_year(year: i32) -> String {
    if year < 0 {
        format!("-{:04}", year.abs())
    } else {
        format!("{:04}", year)
    }
}

// use chrono::Utc;
// use vm::value::*;
// use vm::{error::RuntimeError, vm::VM};
//
// thread_local!(
//     pub static DATE_PROTOTYPE: Value = {
//         make_object!(
//             // TODO: Add methods
//         )
//     };
//
//     pub static DATE_OBJ: Value = {
//         let mut prototype = DATE_PROTOTYPE.with(|x| x.clone());
//         let date = Value::builtin_function(
//             date,
//             None,
//             &mut make_npp!(
//                 // TODO: Add methods
//                 now:    Value::default_builtin_function(date_now)
//             ),
//             Some(prototype.clone())
//         );
//
//         prototype.set_constructor(date.clone());
//
//         date
//     }
// );
//
// pub fn date(vm: &mut VM, _args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
//     let now = Utc::now();
//
//     vm.state.stack.push(Value::string(now.to_rfc3339()));
//
//     Ok(())
// }
//
// pub fn date_new(vm: &mut VM, _args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
//     let now = Utc::now();
//
//     vm.state.stack.push(Value::date(now));
//
//     Ok(())
// }
//
// pub fn date_now(vm: &mut VM, _args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
//     let now = Utc::now();
//     let now_millis = now.timestamp_millis();
//     vm.state.stack.push(Value::Number(now_millis as f64));
//     Ok(())
// }
