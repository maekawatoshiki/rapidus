use super::helpers::{
    define_species_getter, define_well_known_symbol_property, set_function_length, to_index,
};
use crate::vm::{
    jsvalue::{
        object::{AccessorProperty, ArrayBufferObjectInfo, DataProperty, ObjectKind, Property},
        symbol::{SYMBOL_SPECIES_ID, SYMBOL_TO_STRING_TAG_ID},
        value::Value,
    },
    vm::{Factory, VMValueResult, VM},
};
use rustc_hash::FxHashMap;

const MAX_ARRAY_BUFFER_BYTES: usize = 1 << 30;

pub fn array_buffer(factory: &mut Factory) -> Value {
    let prototype = array_buffer_prototype(factory);
    factory.object_prototypes.array_buffer = prototype;
    let constructor =
        factory.generate_builtin_constructor("ArrayBuffer", array_buffer_constructor, prototype);
    set_function_length(constructor, 1.0);

    let is_view = factory.builtin_function("isView", array_buffer_is_view);
    set_function_length(is_view, 1.0);
    constructor.get_object_info().insert_property(
        "isView".to_string(),
        Property::new_data(DataProperty::new(is_view).set_writable().set_configurable()),
    );

    define_species_getter(factory, constructor);
    constructor
}

pub fn array_buffer_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !vm.builtin_constructor_call {
        return Err(vm.current_context.error_type("ArrayBuffer constructor"));
    }

    let byte_length = to_index(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let max_byte_length = array_buffer_max_byte_length_option(
        vm,
        args.get(1).copied().unwrap_or(Value::undefined()),
    )?;
    if let Some(max_byte_length) = max_byte_length {
        if byte_length > max_byte_length {
            return Err(vm.current_context.error_range("ArrayBuffer maxByteLength"));
        }
    }

    set_array_buffer_data(this, byte_length, max_byte_length, vm)?;
    Ok(this)
}

pub fn array_buffer_is_view(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let arg = args.get(0).copied().unwrap_or(Value::undefined());
    if !arg.is_object() {
        return Ok(Value::bool(false));
    }
    Ok(Value::bool(matches!(
        arg.get_object_info().kind,
        ObjectKind::DataView(_) | ObjectKind::TypedArray(_)
    )))
}

pub fn shared_array_buffer(factory: &mut Factory) -> Value {
    let prototype = shared_array_buffer_prototype(factory);
    factory.object_prototypes.shared_array_buffer = prototype;
    let constructor = factory.generate_builtin_constructor(
        "SharedArrayBuffer",
        shared_array_buffer_constructor,
        prototype,
    );
    set_function_length(constructor, 1.0);
    constructor
}

pub fn shared_array_buffer_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !vm.builtin_constructor_call {
        return Err(vm
            .current_context
            .error_type("SharedArrayBuffer constructor"));
    }
    let byte_length = to_index(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let max_byte_length = array_buffer_max_byte_length_option(
        vm,
        args.get(1).copied().unwrap_or(Value::undefined()),
    )?;
    if let Some(max_byte_length) = max_byte_length {
        if byte_length > max_byte_length {
            return Err(vm
                .current_context
                .error_range("SharedArrayBuffer maxByteLength"));
        }
    }
    ensure_allocatable(vm, byte_length)?;
    this.get_object_info().kind = ObjectKind::ArrayBuffer(ArrayBufferObjectInfo {
        bytes: vec![0; byte_length],
        max_byte_length,
        detached: false,
        shared: true,
    });
    Ok(this)
}

pub fn shared_array_buffer_prototype_byte_length(
    vm: &mut VM,
    _args: &[Value],
    this: Value,
) -> VMValueResult {
    let info = shared_array_buffer_info(vm, this)?;
    Ok(Value::Number(if info.detached {
        0.0
    } else {
        info.bytes.len() as f64
    }))
}

pub fn shared_array_buffer_prototype_max_byte_length(
    vm: &mut VM,
    _args: &[Value],
    this: Value,
) -> VMValueResult {
    let info = shared_array_buffer_info(vm, this)?;
    Ok(Value::Number(if info.detached {
        0.0
    } else {
        info.max_byte_length.unwrap_or(info.bytes.len()) as f64
    }))
}

pub fn shared_array_buffer_prototype_growable(
    vm: &mut VM,
    _args: &[Value],
    this: Value,
) -> VMValueResult {
    let info = shared_array_buffer_info(vm, this)?;
    Ok(Value::bool(info.max_byte_length.is_some()))
}

pub fn array_buffer_prototype_byte_length(
    vm: &mut VM,
    _args: &[Value],
    this: Value,
) -> VMValueResult {
    let info = array_buffer_info(vm, this)?;
    Ok(Value::Number(if info.detached {
        0.0
    } else {
        info.bytes.len() as f64
    }))
}

pub fn array_buffer_prototype_max_byte_length(
    vm: &mut VM,
    _args: &[Value],
    this: Value,
) -> VMValueResult {
    let info = array_buffer_info(vm, this)?;
    Ok(Value::Number(if info.detached {
        0.0
    } else {
        info.max_byte_length.unwrap_or(info.bytes.len()) as f64
    }))
}

pub fn array_buffer_prototype_resizable(
    vm: &mut VM,
    _args: &[Value],
    this: Value,
) -> VMValueResult {
    let info = array_buffer_info(vm, this)?;
    Ok(Value::bool(info.max_byte_length.is_some()))
}

pub fn array_buffer_prototype_detached(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let info = array_buffer_info(vm, this)?;
    Ok(Value::bool(info.detached))
}

pub fn array_buffer_prototype_slice(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let (bytes, len) = {
        let info = array_buffer_info(vm, this)?;
        if info.detached {
            return Err(vm.current_context.error_type("ArrayBuffer is detached"));
        }
        (info.bytes.clone(), info.bytes.len() as isize)
    };

    let start = to_integer_or_infinity(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let first = relative_index(start, len);
    let end_arg = args.get(1).copied().unwrap_or(Value::undefined());
    let end = if end_arg.is_undefined() {
        len
    } else {
        let end = to_integer_or_infinity(vm, end_arg)?;
        relative_index(end, len)
    };
    let new_len = (end - first).max(0) as usize;

    let result = species_construct_array_buffer(vm, this, new_len)?;
    if result == this {
        return Err(vm
            .current_context
            .error_type("ArrayBuffer species returned this"));
    }

    {
        let mut result_obj = result.get_object_info();
        match result_obj.kind {
            ObjectKind::ArrayBuffer(ref mut result_info) => {
                if result_info.detached {
                    return Err(vm
                        .current_context
                        .error_type("ArrayBuffer species result detached"));
                }
                if result_info.bytes.len() < new_len {
                    return Err(vm
                        .current_context
                        .error_type("ArrayBuffer species result too small"));
                }
                let first = first as usize;
                result_info.bytes[..new_len].copy_from_slice(&bytes[first..first + new_len]);
            }
            _ => return Err(vm.current_context.error_type("ArrayBuffer species result")),
        }
    }
    Ok(result)
}

pub fn array_buffer_prototype_resize(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let new_len = to_index(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let mut obj = require_array_buffer_object(vm, this)?;
    match obj.kind {
        ObjectKind::ArrayBuffer(ref mut info) => {
            if info.detached {
                return Err(vm.current_context.error_type("ArrayBuffer is detached"));
            }
            let Some(max_byte_length) = info.max_byte_length else {
                return Err(vm
                    .current_context
                    .error_type("ArrayBuffer is not resizable"));
            };
            if new_len > max_byte_length {
                return Err(vm.current_context.error_range("ArrayBuffer resize"));
            }
            ensure_allocatable(vm, new_len)?;
            info.bytes.resize(new_len, 0);
            Ok(Value::undefined())
        }
        _ => unreachable!(),
    }
}

pub fn array_buffer_prototype_transfer(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    transfer_array_buffer(vm, args, this, false)
}

pub fn array_buffer_prototype_transfer_to_fixed_length(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    transfer_array_buffer(vm, args, this, true)
}

fn transfer_array_buffer(
    vm: &mut VM,
    args: &[Value],
    this: Value,
    fixed_length: bool,
) -> VMValueResult {
    let (bytes, old_len, max_byte_length) = {
        let info = array_buffer_info(vm, this)?;
        if info.detached {
            return Err(vm.current_context.error_type("ArrayBuffer is detached"));
        }
        (
            info.bytes.clone(),
            info.bytes.len(),
            if fixed_length {
                None
            } else {
                info.max_byte_length
            },
        )
    };
    let new_len = if args
        .get(0)
        .map(|value| value.is_undefined())
        .unwrap_or(true)
    {
        old_len
    } else {
        to_index(vm, args[0])?
    };
    if let Some(max_byte_length) = max_byte_length {
        if new_len > max_byte_length {
            return Err(vm.current_context.error_range("ArrayBuffer transfer"));
        }
    }

    let result = create_array_buffer(vm, new_len, max_byte_length)?;
    {
        let mut result_obj = result.get_object_info();
        if let ObjectKind::ArrayBuffer(ref mut result_info) = result_obj.kind {
            let copy_len = old_len.min(new_len);
            result_info.bytes[..copy_len].copy_from_slice(&bytes[..copy_len]);
        }
    }
    detach_array_buffer(this);
    Ok(result)
}

fn array_buffer_prototype(factory: &mut Factory) -> Value {
    let function_prototype = factory.object_prototypes.function;
    let byte_length = builtin_getter(
        factory,
        function_prototype,
        "get byteLength",
        array_buffer_prototype_byte_length,
    );
    let max_byte_length = builtin_getter(
        factory,
        function_prototype,
        "get maxByteLength",
        array_buffer_prototype_max_byte_length,
    );
    let resizable = builtin_getter(
        factory,
        function_prototype,
        "get resizable",
        array_buffer_prototype_resizable,
    );
    let detached = builtin_getter(
        factory,
        function_prototype,
        "get detached",
        array_buffer_prototype_detached,
    );
    let slice = builtin_method(
        factory,
        function_prototype,
        "slice",
        array_buffer_prototype_slice,
        2.0,
    );
    let resize = builtin_method(
        factory,
        function_prototype,
        "resize",
        array_buffer_prototype_resize,
        1.0,
    );
    let transfer = builtin_method(
        factory,
        function_prototype,
        "transfer",
        array_buffer_prototype_transfer,
        0.0,
    );
    let transfer_to_fixed_length = builtin_method(
        factory,
        function_prototype,
        "transferToFixedLength",
        array_buffer_prototype_transfer_to_fixed_length,
        0.0,
    );
    let tag = factory.string("ArrayBuffer");

    let prototype = Value::Object(factory.alloc(crate::vm::jsvalue::object::Object {
        kind: ObjectKind::Ordinary,
        prototype: factory.object_prototypes.object,
        property: {
            let mut property = FxHashMap::default();
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
                "maxByteLength".to_string(),
                Property::Accessor(AccessorProperty {
                    get: max_byte_length,
                    set: Value::undefined(),
                    enumerable: false,
                    configurable: true,
                }),
            );
            property.insert(
                "resizable".to_string(),
                Property::Accessor(AccessorProperty {
                    get: resizable,
                    set: Value::undefined(),
                    enumerable: false,
                    configurable: true,
                }),
            );
            property.insert(
                "detached".to_string(),
                Property::Accessor(AccessorProperty {
                    get: detached,
                    set: Value::undefined(),
                    enumerable: false,
                    configurable: true,
                }),
            );
            property.insert(
                "slice".to_string(),
                Property::new_data(DataProperty::new(slice).set_writable().set_configurable()),
            );
            property.insert(
                "resize".to_string(),
                Property::new_data(DataProperty::new(resize).set_writable().set_configurable()),
            );
            property.insert(
                "transfer".to_string(),
                Property::new_data(
                    DataProperty::new(transfer)
                        .set_writable()
                        .set_configurable(),
                ),
            );
            property.insert(
                "transferToFixedLength".to_string(),
                Property::new_data(
                    DataProperty::new(transfer_to_fixed_length)
                        .set_writable()
                        .set_configurable(),
                ),
            );
            property
        },
        property_order: vec![
            "byteLength".to_string(),
            "maxByteLength".to_string(),
            "resizable".to_string(),
            "detached".to_string(),
            "slice".to_string(),
            "resize".to_string(),
            "transfer".to_string(),
            "transferToFixedLength".to_string(),
        ],
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

fn shared_array_buffer_prototype(factory: &mut Factory) -> Value {
    let byte_length = builtin_getter(
        factory,
        factory.object_prototypes.function,
        "get byteLength",
        shared_array_buffer_prototype_byte_length,
    );
    let max_byte_length = builtin_getter(
        factory,
        factory.object_prototypes.function,
        "get maxByteLength",
        shared_array_buffer_prototype_max_byte_length,
    );
    let growable = builtin_getter(
        factory,
        factory.object_prototypes.function,
        "get growable",
        shared_array_buffer_prototype_growable,
    );
    let tag = factory.string("SharedArrayBuffer");
    let prototype = Value::Object(factory.alloc(crate::vm::jsvalue::object::Object {
        kind: ObjectKind::Ordinary,
        prototype: factory.object_prototypes.object,
        property: {
            let mut property = FxHashMap::default();
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
                "maxByteLength".to_string(),
                Property::Accessor(AccessorProperty {
                    get: max_byte_length,
                    set: Value::undefined(),
                    enumerable: false,
                    configurable: true,
                }),
            );
            property.insert(
                "growable".to_string(),
                Property::Accessor(AccessorProperty {
                    get: growable,
                    set: Value::undefined(),
                    enumerable: false,
                    configurable: true,
                }),
            );
            property
        },
        property_order: vec![
            "byteLength".to_string(),
            "maxByteLength".to_string(),
            "growable".to_string(),
        ],
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

fn create_array_buffer(
    vm: &mut VM,
    byte_length: usize,
    max_byte_length: Option<usize>,
) -> Result<Value, crate::vm::error::RuntimeError> {
    ensure_allocatable(vm, byte_length)?;
    Ok(Value::Object(vm.factory.alloc(
        crate::vm::jsvalue::object::Object {
            kind: ObjectKind::ArrayBuffer(ArrayBufferObjectInfo {
                bytes: vec![0; byte_length],
                max_byte_length,
                detached: false,
                shared: false,
            }),
            prototype: vm.factory.object_prototypes.array_buffer,
            property: FxHashMap::default(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        },
    )))
}

fn set_array_buffer_data(
    target: Value,
    byte_length: usize,
    max_byte_length: Option<usize>,
    vm: &mut VM,
) -> Result<(), crate::vm::error::RuntimeError> {
    ensure_allocatable(vm, byte_length)?;
    let mut obj = target.get_object_info();
    obj.kind = ObjectKind::ArrayBuffer(ArrayBufferObjectInfo {
        bytes: vec![0; byte_length],
        max_byte_length,
        detached: false,
        shared: false,
    });
    Ok(())
}

fn array_buffer_info(
    vm: &mut VM,
    value: Value,
) -> Result<ArrayBufferObjectInfo, crate::vm::error::RuntimeError> {
    let obj = require_array_buffer_object(vm, value)?;
    match obj.kind {
        ObjectKind::ArrayBuffer(ref info) if !info.shared => Ok(info.clone()),
        ObjectKind::ArrayBuffer(_) => Err(vm.current_context.error_type("SharedArrayBuffer")),
        _ => unreachable!(),
    }
}

fn shared_array_buffer_info(
    vm: &mut VM,
    value: Value,
) -> Result<ArrayBufferObjectInfo, crate::vm::error::RuntimeError> {
    if !value.is_object() {
        return Err(vm.current_context.error_type("SharedArrayBuffer receiver"));
    }
    let obj = value.get_object_info();
    match obj.kind {
        ObjectKind::ArrayBuffer(ref info) if info.shared => Ok(info.clone()),
        ObjectKind::ArrayBuffer(_) => Err(vm.current_context.error_type("ArrayBuffer")),
        _ => Err(vm.current_context.error_type("SharedArrayBuffer receiver")),
    }
}

fn require_array_buffer_object(
    vm: &mut VM,
    value: Value,
) -> Result<crate::vm::jsvalue::object::ObjectRef, crate::vm::error::RuntimeError> {
    if !value.is_object() {
        return Err(vm.current_context.error_type("ArrayBuffer receiver"));
    }
    let obj = value.get_object_info();
    match obj.kind {
        ObjectKind::ArrayBuffer(_) => Ok(obj),
        _ => Err(vm.current_context.error_type("ArrayBuffer receiver")),
    }
}

pub fn detach_array_buffer(value: Value) -> bool {
    if !value.is_object() {
        return false;
    }
    let mut obj = value.get_object_info();
    if let ObjectKind::ArrayBuffer(ref mut info) = obj.kind {
        if info.shared {
            return false;
        }
        info.bytes.clear();
        info.detached = true;
        return true;
    }
    false
}

fn species_construct_array_buffer(
    vm: &mut VM,
    object: Value,
    length: usize,
) -> Result<Value, crate::vm::error::RuntimeError> {
    let default_constructor = vm
        .factory
        .object_prototypes
        .array_buffer
        .get_property("constructor");
    let constructor_key = vm.factory.string("constructor");
    let constructor = vm.get_property_by_value(object, constructor_key)?;
    let constructor = if constructor.is_undefined() {
        default_constructor
    } else if constructor.is_symbol() || !constructor.is_object() {
        return Err(vm
            .current_context
            .error_type("ArrayBuffer species constructor"));
    } else {
        let species_key = vm.factory.well_known_symbol(SYMBOL_SPECIES_ID);
        let species = vm.get_property_by_value(constructor, species_key)?;
        if species.is_null() || species.is_undefined() {
            default_constructor
        } else if !vm.is_constructor(species) {
            return Err(vm.current_context.error_type("ArrayBuffer species"));
        } else {
            species
        }
    };

    if !vm.is_constructor(constructor) {
        return Err(vm.current_context.error_type("ArrayBuffer species"));
    }
    vm.construct_function(constructor, &[Value::Number(length as f64)])
}

fn array_buffer_max_byte_length_option(
    vm: &mut VM,
    options: Value,
) -> Result<Option<usize>, crate::vm::error::RuntimeError> {
    if !options.is_object() {
        return Ok(None);
    }
    let key = vm.factory.string("maxByteLength");
    let max_byte_length = vm.get_property_by_value(options, key)?;
    if max_byte_length.is_undefined() {
        return Ok(None);
    }
    Ok(Some(to_index(vm, max_byte_length)?))
}

/// ArrayBuffer slicing indexes with isize; saturates ±Infinity from the
/// shared ToIntegerOrInfinity to isize::MAX/MIN.
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

fn relative_index(value: isize, length: isize) -> isize {
    if value < 0 {
        (length + value).max(0)
    } else {
        value.min(length)
    }
}

fn ensure_allocatable(
    vm: &mut VM,
    byte_length: usize,
) -> Result<(), crate::vm::error::RuntimeError> {
    if byte_length > MAX_ARRAY_BUFFER_BYTES {
        return Err(vm.current_context.error_range("ArrayBuffer allocation"));
    }
    Ok(())
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
