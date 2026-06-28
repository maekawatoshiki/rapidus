use super::helpers::{
    call_to_primitive, define_well_known_symbol_property, set_function_length, to_index, to_number,
};
use crate::vm::{
    jsvalue::{
        object::{AccessorProperty, DataProperty, DataViewObjectInfo, ObjectKind, Property},
        symbol::SYMBOL_TO_STRING_TAG_ID,
        value::Value,
    },
    vm::{Factory, VMValueResult, VM},
};
use rustc_hash::FxHashMap;

#[derive(Clone, Copy)]
enum ViewType {
    Int8,
    Uint8,
    Int16,
    Uint16,
    Int32,
    Uint32,
    Float32,
    Float64,
    BigInt64,
    BigUint64,
}

impl ViewType {
    fn size(self) -> usize {
        match self {
            ViewType::Int8 | ViewType::Uint8 => 1,
            ViewType::Int16 | ViewType::Uint16 => 2,
            ViewType::Int32 | ViewType::Uint32 | ViewType::Float32 => 4,
            ViewType::Float64 | ViewType::BigInt64 | ViewType::BigUint64 => 8,
        }
    }
}

pub fn data_view(factory: &mut Factory) -> Value {
    let prototype = data_view_prototype(factory);
    factory.object_prototypes.data_view = prototype;
    let constructor =
        factory.generate_builtin_constructor("DataView", data_view_constructor, prototype);
    set_function_length(constructor, 1.0);
    constructor
}

pub fn data_view_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !vm.builtin_constructor_call {
        return Err(vm.current_context.error_type("DataView constructor"));
    }

    let buffer = args.get(0).copied().unwrap_or(Value::undefined());
    let buffer_len = array_buffer_byte_length(vm, buffer)?;
    let byte_offset = to_index(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    if array_buffer_detached(buffer) {
        return Err(vm.current_context.error_type("ArrayBuffer is detached"));
    }

    if byte_offset > buffer_len {
        return Err(vm.current_context.error_range("DataView byteOffset"));
    }

    let (byte_length, length_tracking) = match args.get(2).copied() {
        Some(value) if !value.is_undefined() => {
            let byte_length = to_index(vm, value)?;
            if byte_offset
                .checked_add(byte_length)
                .map(|end| end > buffer_len)
                .unwrap_or(true)
            {
                return Err(vm.current_context.error_range("DataView byteLength"));
            }
            (byte_length, false)
        }
        _ => (buffer_len - byte_offset, array_buffer_resizable(buffer)),
    };

    this.get_object_info().kind = ObjectKind::DataView(DataViewObjectInfo {
        buffer,
        byte_offset,
        byte_length,
        length_tracking,
    });
    Ok(this)
}

pub fn data_view_prototype_buffer(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let info = data_view_info(vm, this)?;
    Ok(info.buffer)
}

pub fn data_view_prototype_byte_length(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let info = data_view_info(vm, this)?;
    if array_buffer_detached(info.buffer) {
        return Err(vm.current_context.error_type("ArrayBuffer is detached"));
    }
    Ok(Value::Number(
        data_view_effective_byte_length(&info).unwrap_or(0) as f64,
    ))
}

pub fn data_view_prototype_byte_offset(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let info = data_view_info(vm, this)?;
    if array_buffer_detached(info.buffer) {
        return Err(vm.current_context.error_type("ArrayBuffer is detached"));
    }
    Ok(Value::Number(
        if data_view_effective_byte_length(&info).is_some() {
            info.byte_offset as f64
        } else {
            0.0
        },
    ))
}

pub fn data_view_prototype_get_int8(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    get_view_value(vm, args, this, ViewType::Int8)
}

pub fn data_view_prototype_get_uint8(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    get_view_value(vm, args, this, ViewType::Uint8)
}

pub fn data_view_prototype_get_int16(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    get_view_value(vm, args, this, ViewType::Int16)
}

pub fn data_view_prototype_get_uint16(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    get_view_value(vm, args, this, ViewType::Uint16)
}

pub fn data_view_prototype_get_int32(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    get_view_value(vm, args, this, ViewType::Int32)
}

pub fn data_view_prototype_get_uint32(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    get_view_value(vm, args, this, ViewType::Uint32)
}

pub fn data_view_prototype_get_float32(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    get_view_value(vm, args, this, ViewType::Float32)
}

pub fn data_view_prototype_get_float64(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    get_view_value(vm, args, this, ViewType::Float64)
}

pub fn data_view_prototype_get_big_int64(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    get_view_value(vm, args, this, ViewType::BigInt64)
}

pub fn data_view_prototype_get_big_uint64(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    get_view_value(vm, args, this, ViewType::BigUint64)
}

pub fn data_view_prototype_set_int8(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_view_value(vm, args, this, ViewType::Int8)
}

pub fn data_view_prototype_set_uint8(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_view_value(vm, args, this, ViewType::Uint8)
}

pub fn data_view_prototype_set_int16(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_view_value(vm, args, this, ViewType::Int16)
}

pub fn data_view_prototype_set_uint16(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_view_value(vm, args, this, ViewType::Uint16)
}

pub fn data_view_prototype_set_int32(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_view_value(vm, args, this, ViewType::Int32)
}

pub fn data_view_prototype_set_uint32(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_view_value(vm, args, this, ViewType::Uint32)
}

pub fn data_view_prototype_set_float32(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_view_value(vm, args, this, ViewType::Float32)
}

pub fn data_view_prototype_set_float64(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    set_view_value(vm, args, this, ViewType::Float64)
}

pub fn data_view_prototype_set_big_int64(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    set_big_int_view_value(vm, args, this, ViewType::BigInt64)
}

pub fn data_view_prototype_set_big_uint64(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    set_big_int_view_value(vm, args, this, ViewType::BigUint64)
}

fn data_view_prototype(factory: &mut Factory) -> Value {
    let function_prototype = factory.object_prototypes.function;
    let buffer = builtin_getter(
        factory,
        function_prototype,
        "get buffer",
        data_view_prototype_buffer,
    );
    let byte_length = builtin_getter(
        factory,
        function_prototype,
        "get byteLength",
        data_view_prototype_byte_length,
    );
    let byte_offset = builtin_getter(
        factory,
        function_prototype,
        "get byteOffset",
        data_view_prototype_byte_offset,
    );
    let tag = factory.string("DataView");
    let methods = [
        (
            "getInt8",
            data_view_prototype_get_int8 as crate::builtins::BuiltinFuncTy,
            1.0,
        ),
        ("getUint8", data_view_prototype_get_uint8, 1.0),
        ("getInt16", data_view_prototype_get_int16, 1.0),
        ("getUint16", data_view_prototype_get_uint16, 1.0),
        ("getInt32", data_view_prototype_get_int32, 1.0),
        ("getUint32", data_view_prototype_get_uint32, 1.0),
        ("getFloat32", data_view_prototype_get_float32, 1.0),
        ("getFloat64", data_view_prototype_get_float64, 1.0),
        ("getBigInt64", data_view_prototype_get_big_int64, 1.0),
        ("getBigUint64", data_view_prototype_get_big_uint64, 1.0),
        ("setInt8", data_view_prototype_set_int8, 2.0),
        ("setUint8", data_view_prototype_set_uint8, 2.0),
        ("setInt16", data_view_prototype_set_int16, 2.0),
        ("setUint16", data_view_prototype_set_uint16, 2.0),
        ("setInt32", data_view_prototype_set_int32, 2.0),
        ("setUint32", data_view_prototype_set_uint32, 2.0),
        ("setFloat32", data_view_prototype_set_float32, 2.0),
        ("setFloat64", data_view_prototype_set_float64, 2.0),
        ("setBigInt64", data_view_prototype_set_big_int64, 2.0),
        ("setBigUint64", data_view_prototype_set_big_uint64, 2.0),
    ];
    let method_values = methods
        .into_iter()
        .map(|(name, func, length)| {
            (
                name,
                builtin_method(factory, function_prototype, name, func, length),
            )
        })
        .collect::<Vec<_>>();

    let mut property = FxHashMap::default();
    property.insert(
        "buffer".to_string(),
        Property::Accessor(AccessorProperty {
            get: buffer,
            set: Value::undefined(),
            enumerable: false,
            configurable: true,
        }),
    );
    property.insert(
        "byteLength".to_string(),
        Property::Accessor(AccessorProperty {
            get: byte_length,
            set: Value::undefined(),
            enumerable: false,
            configurable: true,
        }),
    );
    property.insert(
        "byteOffset".to_string(),
        Property::Accessor(AccessorProperty {
            get: byte_offset,
            set: Value::undefined(),
            enumerable: false,
            configurable: true,
        }),
    );
    for (name, method) in method_values {
        property.insert(
            name.to_string(),
            Property::new_data(DataProperty::new(method).set_writable().set_configurable()),
        );
    }

    let property_order = crate::vm::jsvalue::object::property_order_from_map(&property);
    let prototype = Value::Object(factory.alloc(crate::vm::jsvalue::object::Object {
        kind: ObjectKind::Ordinary,
        prototype: factory.object_prototypes.object,
        property,
        property_order,
        private_elements: rustc_hash::FxHashMap::default(),
        sym_property: FxHashMap::default(),
        sym_property_order: Vec::new(),
        extensible: true,
    }));
    define_well_known_symbol_property(
        factory,
        prototype,
        SYMBOL_TO_STRING_TAG_ID,
        Property::new_data(DataProperty::new(tag).set_configurable()),
    );
    prototype
}

fn get_view_value(vm: &mut VM, args: &[Value], this: Value, view_type: ViewType) -> VMValueResult {
    let info = data_view_info(vm, this)?;
    let get_index = to_index(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    if array_buffer_detached(info.buffer) {
        return Err(vm.current_context.error_type("ArrayBuffer is detached"));
    }
    let byte_length = data_view_effective_byte_length(&info)
        .ok_or_else(|| vm.current_context.error_type("DataView is out of bounds"))?;
    let size = view_type.size();
    if get_index
        .checked_add(size)
        .map(|end| end > byte_length)
        .unwrap_or(true)
    {
        return Err(vm.current_context.error_range("DataView byteOffset"));
    }
    let little_endian = args.get(1).map(|value| value.to_boolean()).unwrap_or(false);
    let buffer_index = info.byte_offset + get_index;
    let bytes = array_buffer_bytes(vm, info.buffer)?;
    Ok(read_value(
        &mut vm.factory,
        &bytes[buffer_index..buffer_index + size],
        view_type,
        little_endian,
    ))
}

fn set_view_value(vm: &mut VM, args: &[Value], this: Value, view_type: ViewType) -> VMValueResult {
    let info = data_view_info(vm, this)?;
    let set_index = to_index(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let value = to_number(vm, args.get(1).copied().unwrap_or(Value::undefined()))?;
    if array_buffer_detached(info.buffer) {
        return Err(vm.current_context.error_type("ArrayBuffer is detached"));
    }
    let byte_length = data_view_effective_byte_length(&info)
        .ok_or_else(|| vm.current_context.error_type("DataView is out of bounds"))?;
    let size = view_type.size();
    if set_index
        .checked_add(size)
        .map(|end| end > byte_length)
        .unwrap_or(true)
    {
        return Err(vm.current_context.error_range("DataView byteOffset"));
    }
    let little_endian = args.get(2).map(|value| value.to_boolean()).unwrap_or(false);
    let buffer_index = info.byte_offset + set_index;
    let bytes = write_value(value, view_type, little_endian);
    write_array_buffer_bytes(vm, info.buffer, buffer_index, &bytes)?;
    Ok(Value::undefined())
}

fn set_big_int_view_value(
    vm: &mut VM,
    args: &[Value],
    this: Value,
    view_type: ViewType,
) -> VMValueResult {
    let info = data_view_info(vm, this)?;
    let set_index = to_index(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let value = args.get(1).copied().unwrap_or(Value::undefined());
    let bytes = bigint_bytes(vm, value, view_type)?;
    if array_buffer_detached(info.buffer) {
        return Err(vm.current_context.error_type("ArrayBuffer is detached"));
    }
    let byte_length = data_view_effective_byte_length(&info)
        .ok_or_else(|| vm.current_context.error_type("DataView is out of bounds"))?;
    if set_index
        .checked_add(8)
        .map(|end| end > byte_length)
        .unwrap_or(true)
    {
        return Err(vm.current_context.error_range("DataView byteOffset"));
    }
    let little_endian = args.get(2).map(|value| value.to_boolean()).unwrap_or(false);
    let bytes = ordered_bytes_8(bytes, little_endian);
    write_array_buffer_bytes(vm, info.buffer, info.byte_offset + set_index, &bytes)?;
    Ok(Value::undefined())
}

fn read_value(
    factory: &mut Factory,
    bytes: &[u8],
    view_type: ViewType,
    little_endian: bool,
) -> Value {
    match view_type {
        ViewType::Int8 => Value::Number(bytes[0] as i8 as f64),
        ViewType::Uint8 => Value::Number(bytes[0] as f64),
        ViewType::Int16 => {
            let bytes = [bytes[0], bytes[1]];
            let value = if little_endian {
                i16::from_le_bytes(bytes)
            } else {
                i16::from_be_bytes(bytes)
            };
            Value::Number(value as f64)
        }
        ViewType::Uint16 => {
            let bytes = [bytes[0], bytes[1]];
            let value = if little_endian {
                u16::from_le_bytes(bytes)
            } else {
                u16::from_be_bytes(bytes)
            };
            Value::Number(value as f64)
        }
        ViewType::Int32 => {
            let bytes = [bytes[0], bytes[1], bytes[2], bytes[3]];
            let value = if little_endian {
                i32::from_le_bytes(bytes)
            } else {
                i32::from_be_bytes(bytes)
            };
            Value::Number(value as f64)
        }
        ViewType::Uint32 => {
            let bytes = [bytes[0], bytes[1], bytes[2], bytes[3]];
            let value = if little_endian {
                u32::from_le_bytes(bytes)
            } else {
                u32::from_be_bytes(bytes)
            };
            Value::Number(value as f64)
        }
        ViewType::Float32 => {
            let bytes = [bytes[0], bytes[1], bytes[2], bytes[3]];
            let value = if little_endian {
                f32::from_le_bytes(bytes)
            } else {
                f32::from_be_bytes(bytes)
            };
            Value::Number(value as f64)
        }
        ViewType::Float64 => {
            let bytes = [
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            ];
            let value = if little_endian {
                f64::from_le_bytes(bytes)
            } else {
                f64::from_be_bytes(bytes)
            };
            Value::Number(value)
        }
        ViewType::BigInt64 => {
            let bytes = ordered_read_8(bytes, little_endian);
            factory.bigint(i64::from_ne_bytes(bytes).to_string())
        }
        ViewType::BigUint64 => {
            let bytes = ordered_read_8(bytes, little_endian);
            factory.bigint(u64::from_ne_bytes(bytes).to_string())
        }
    }
}

fn write_value(value: f64, view_type: ViewType, little_endian: bool) -> Vec<u8> {
    match view_type {
        ViewType::Int8 | ViewType::Uint8 => vec![to_uint_n(value, 8) as u8],
        ViewType::Int16 => ordered_bytes((to_int_n(value, 16) as i16).to_ne_bytes(), little_endian),
        ViewType::Uint16 => {
            ordered_bytes((to_uint_n(value, 16) as u16).to_ne_bytes(), little_endian)
        }
        ViewType::Int32 => ordered_bytes((to_int_n(value, 32) as i32).to_ne_bytes(), little_endian),
        ViewType::Uint32 => {
            ordered_bytes((to_uint_n(value, 32) as u32).to_ne_bytes(), little_endian)
        }
        ViewType::Float32 => ordered_bytes((value as f32).to_ne_bytes(), little_endian),
        ViewType::Float64 => ordered_bytes(value.to_ne_bytes(), little_endian),
        ViewType::BigInt64 | ViewType::BigUint64 => unreachable!(),
    }
}

fn ordered_bytes<const N: usize>(bytes: [u8; N], little_endian: bool) -> Vec<u8> {
    if cfg!(target_endian = "little") == little_endian {
        bytes.to_vec()
    } else {
        bytes.into_iter().rev().collect()
    }
}

fn ordered_read_8(bytes: &[u8], little_endian: bool) -> [u8; 8] {
    let mut bytes = [
        bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
    ];
    if cfg!(target_endian = "little") != little_endian {
        bytes.reverse();
    }
    bytes
}

fn ordered_bytes_8(mut bytes: [u8; 8], little_endian: bool) -> Vec<u8> {
    if cfg!(target_endian = "little") != little_endian {
        bytes.reverse();
    }
    bytes.to_vec()
}

fn data_view_effective_byte_length(info: &DataViewObjectInfo) -> Option<usize> {
    if array_buffer_detached(info.buffer) {
        return Some(0);
    }
    let buffer_len = array_buffer_current_len(info.buffer)?;
    if info.byte_offset > buffer_len {
        return None;
    }
    if info.length_tracking {
        return Some(buffer_len - info.byte_offset);
    }
    if info.byte_offset.checked_add(info.byte_length)? > buffer_len {
        None
    } else {
        Some(info.byte_length)
    }
}

fn bigint_bytes(
    vm: &mut VM,
    value: Value,
    view_type: ViewType,
) -> Result<[u8; 8], crate::vm::error::RuntimeError> {
    let decimal = to_bigint_decimal(vm, value)?;
    let bytes = match view_type {
        ViewType::BigInt64 => decimal
            .parse::<i128>()
            .map(|value| (value as i64).to_ne_bytes())
            .unwrap_or_else(|_| 0i64.to_ne_bytes()),
        ViewType::BigUint64 => decimal
            .parse::<i128>()
            .map(|value| (value as u64).to_ne_bytes())
            .unwrap_or_else(|_| 0u64.to_ne_bytes()),
        _ => unreachable!(),
    };
    Ok(bytes)
}

fn to_bigint_decimal(vm: &mut VM, value: Value) -> Result<String, crate::vm::error::RuntimeError> {
    if let Some(decimal) = value.bigint_decimal() {
        return Ok(decimal);
    }
    if value.is_string() {
        let string = value.into_str().trim().to_string();
        if string.parse::<i128>().is_ok() {
            return Ok(string);
        }
        return Err(vm
            .current_context
            .error_syntax("Cannot convert string to BigInt"));
    }
    if let Value::Bool(value) = value {
        return Ok(if value != 0 { "1" } else { "0" }.to_string());
    }
    if value.is_number() || value.is_symbol() || value.is_null() || value.is_undefined() {
        return Err(vm.current_context.error_type("Cannot convert to BigInt"));
    }
    if value.is_object() {
        if let Some(decimal) = value.get_property("__bigint_data").bigint_decimal() {
            return Ok(decimal);
        }
        if let Some(primitive) = call_to_primitive(vm, value, "number")? {
            return to_bigint_decimal(vm, primitive);
        }
        for method_name in ["valueOf", "toString"] {
            let key = vm.factory.string(method_name);
            let method = vm.get_property_by_value(value, key)?;
            if method.is_function_object() {
                let primitive = vm.call_function(method, &[], value)?;
                if !primitive.is_object() {
                    return to_bigint_decimal(vm, primitive);
                }
            }
        }
    }
    Err(vm.current_context.error_type("Cannot convert to BigInt"))
}

fn data_view_info(
    vm: &mut VM,
    value: Value,
) -> Result<DataViewObjectInfo, crate::vm::error::RuntimeError> {
    if !value.is_object() {
        return Err(vm.current_context.error_type("DataView receiver"));
    }
    match value.get_object_info().kind {
        ObjectKind::DataView(ref info) => Ok(info.clone()),
        _ => Err(vm.current_context.error_type("DataView receiver")),
    }
}

fn array_buffer_byte_length(
    vm: &mut VM,
    value: Value,
) -> Result<usize, crate::vm::error::RuntimeError> {
    if !value.is_object() {
        return Err(vm.current_context.error_type("DataView buffer"));
    }
    match value.get_object_info().kind {
        ObjectKind::ArrayBuffer(ref info) => Ok(if info.detached { 0 } else { info.bytes.len() }),
        _ => Err(vm.current_context.error_type("DataView buffer")),
    }
}

fn array_buffer_detached(value: Value) -> bool {
    if !value.is_object() {
        return false;
    }
    match value.get_object_info().kind {
        ObjectKind::ArrayBuffer(ref info) => info.detached,
        _ => false,
    }
}

fn array_buffer_resizable(value: Value) -> bool {
    if !value.is_object() {
        return false;
    }
    match value.get_object_info().kind {
        ObjectKind::ArrayBuffer(ref info) => info.max_byte_length.is_some(),
        _ => false,
    }
}

fn array_buffer_current_len(value: Value) -> Option<usize> {
    if !value.is_object() {
        return None;
    }
    match value.get_object_info().kind {
        ObjectKind::ArrayBuffer(ref info) if !info.detached => Some(info.bytes.len()),
        ObjectKind::ArrayBuffer(_) => Some(0),
        _ => None,
    }
}

fn array_buffer_bytes(
    vm: &mut VM,
    value: Value,
) -> Result<Vec<u8>, crate::vm::error::RuntimeError> {
    match value.get_object_info().kind {
        ObjectKind::ArrayBuffer(ref info) => Ok(info.bytes.clone()),
        _ => Err(vm.current_context.error_type("DataView buffer")),
    }
}

fn write_array_buffer_bytes(
    vm: &mut VM,
    value: Value,
    offset: usize,
    data: &[u8],
) -> Result<(), crate::vm::error::RuntimeError> {
    let mut obj = value.get_object_info();
    match obj.kind {
        ObjectKind::ArrayBuffer(ref mut info) => {
            info.bytes[offset..offset + data.len()].copy_from_slice(data);
            Ok(())
        }
        _ => Err(vm.current_context.error_type("DataView buffer")),
    }
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

fn builtin_getter(
    factory: &mut Factory,
    function_prototype: Value,
    name: &str,
    func: crate::builtins::BuiltinFuncTy,
) -> Value {
    builtin_method(factory, function_prototype, name, func, 0.0)
}

fn builtin_method(
    factory: &mut Factory,
    function_prototype: Value,
    name: &str,
    func: crate::builtins::BuiltinFuncTy,
    length: f64,
) -> Value {
    let function = Value::builtin_function_with_proto(
        &mut factory.memory_allocator,
        function_prototype,
        name,
        func,
    );
    set_function_length(function, length);
    function
}
