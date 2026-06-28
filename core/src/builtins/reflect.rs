use super::helpers::{
    define_symbol_property, define_well_known_symbol_property, set_function_length,
};
use crate::builtins::{object, BuiltinFuncTy};
use crate::vm::{
    jsvalue::symbol::SYMBOL_TO_STRING_TAG_ID,
    jsvalue::value::*,
    jsvalue::{function::FunctionObjectKind, object::ObjectKind},
    vm::{Factory, VMValueResult, VM},
};

pub fn reflect(factory: &mut Factory) -> Value {
    let obj = factory.object(FxHashMap::default());
    for (name, length, func) in [
        ("apply", 3.0, reflect_apply as BuiltinFuncTy),
        ("construct", 2.0, reflect_construct),
        ("defineProperty", 3.0, reflect_define_property),
        ("deleteProperty", 2.0, reflect_delete_property),
        ("get", 2.0, reflect_get),
        (
            "getOwnPropertyDescriptor",
            2.0,
            reflect_get_own_property_descriptor,
        ),
        ("getPrototypeOf", 1.0, reflect_get_prototype_of),
        ("has", 2.0, reflect_has),
        ("isExtensible", 1.0, reflect_is_extensible),
        ("ownKeys", 1.0, reflect_own_keys),
        ("preventExtensions", 1.0, reflect_prevent_extensions),
        ("set", 3.0, reflect_set),
        ("setPrototypeOf", 2.0, reflect_set_prototype_of),
    ] {
        let func = factory.builtin_function(name, func);
        set_function_length(func, length);
        obj.get_object_info().insert_property(
            name.to_string(),
            Property::new_data(DataProperty::new(func).set_writable().set_configurable()),
        );
    }
    let tag = factory.string("Reflect");
    define_well_known_symbol_property(
        factory,
        obj,
        SYMBOL_TO_STRING_TAG_ID,
        Property::new_data(DataProperty::new(tag).set_configurable()),
    );
    obj
}

fn is_object_value(value: Value) -> bool {
    value.is_object() && !value.is_symbol()
}

pub fn reflect_apply(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = *args.get(0).unwrap_or(&Value::undefined());
    if !vm.is_callable(target) {
        return Err(vm.current_context.error_type("Reflect.apply"));
    }
    let this_arg = *args.get(1).unwrap_or(&Value::undefined());
    let arguments_arg = *args.get(2).unwrap_or(&Value::undefined());
    if is_builtin_named(target, "fromCodePoint") {
        if let Some(result) = fast_string_from_code_point(vm, arguments_arg)? {
            return Ok(result);
        }
    }
    let arguments = create_list_from_array_like(vm, arguments_arg)?;
    vm.call_function(target, &arguments, this_arg)
}

pub fn reflect_construct(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = *args.get(0).unwrap_or(&Value::undefined());
    if !vm.is_constructor(target) {
        return Err(vm.current_context.error_type("Reflect.construct"));
    }
    if let Some(new_target) = args.get(2) {
        if !vm.is_constructor(*new_target) {
            return Err(vm.current_context.error_type("Reflect.construct"));
        }
    }
    let new_target = *args.get(2).unwrap_or(&target);
    let arguments = create_list_from_array_like(vm, *args.get(1).unwrap_or(&Value::undefined()))?;
    vm.construct_function_with_new_target(target, &arguments, new_target)
}

pub fn reflect_define_property(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = *args.get(0).unwrap_or(&Value::undefined());
    let key = *args.get(1).unwrap_or(&Value::undefined());
    let descriptor = *args.get(2).unwrap_or(&Value::undefined());
    if !is_object_value(target) || !is_object_value(descriptor) {
        return Err(vm.current_context.error_type("Reflect.defineProperty"));
    }
    if key.is_symbol() {
        return reflect_define_symbol_property(vm, target, key, descriptor);
    }
    match object::object_define_property(vm, args, Value::undefined()) {
        Ok(_) => Ok(Value::bool(true)),
        Err(_) => Ok(Value::bool(false)),
    }
}

fn reflect_define_symbol_property(
    vm: &mut VM,
    target: Value,
    key: Value,
    descriptor: Value,
) -> VMValueResult {
    let id = key.get_symbol_info().id;
    let existing = target.get_object_info().sym_property.get(&id).copied();
    if existing.is_none() && !target.get_object_info().extensible {
        return Ok(Value::bool(false));
    }

    let has_get = descriptor.has_own_property("get");
    let has_set = descriptor.has_own_property("set");
    let has_value = descriptor.has_own_property("value");
    let has_writable = descriptor.has_own_property("writable");
    let has_enumerable = descriptor.has_own_property("enumerable");
    let has_configurable = descriptor.has_own_property("configurable");

    let enumerable = if has_enumerable {
        descriptor.get_property("enumerable").to_boolean()
    } else {
        existing
            .map(|prop| match prop {
                Property::Data(data) => data.enumerable,
                Property::Accessor(accessor) => accessor.enumerable,
            })
            .unwrap_or(false)
    };
    let configurable = if has_configurable {
        descriptor.get_property("configurable").to_boolean()
    } else {
        existing.map(|prop| prop.configurable()).unwrap_or(false)
    };

    let property = if has_get || has_set {
        let get = if has_get {
            descriptor.get_property("get")
        } else {
            existing
                .and_then(|prop| match prop {
                    Property::Accessor(accessor) => Some(accessor.get),
                    _ => None,
                })
                .unwrap_or(Value::undefined())
        };
        let set = if has_set {
            descriptor.get_property("set")
        } else {
            existing
                .and_then(|prop| match prop {
                    Property::Accessor(accessor) => Some(accessor.set),
                    _ => None,
                })
                .unwrap_or(Value::undefined())
        };
        if (!get.is_undefined() && !get.is_function_object())
            || (!set.is_undefined() && !set.is_function_object())
        {
            return Err(vm.current_context.error_type("Reflect.defineProperty"));
        }
        Property::Accessor(AccessorProperty {
            get,
            set,
            enumerable,
            configurable,
        })
    } else {
        let old_data = existing.and_then(|prop| match prop {
            Property::Data(data) => Some(data),
            _ => None,
        });
        Property::new_data(DataProperty {
            val: if has_value {
                descriptor.get_property("value")
            } else {
                old_data.map(|data| data.val).unwrap_or(Value::undefined())
            },
            writable: if has_writable {
                descriptor.get_property("writable").to_boolean()
            } else {
                old_data.map(|data| data.writable).unwrap_or(false)
            },
            enumerable,
            configurable,
        })
    };

    define_symbol_property(target, key, property);
    Ok(Value::bool(true))
}

pub fn reflect_delete_property(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = *args.get(0).unwrap_or(&Value::undefined());
    if !is_object_value(target) {
        return Err(vm.current_context.error_type("Reflect.deleteProperty"));
    }
    let key = *args.get(1).unwrap_or(&Value::undefined());
    let deleted = target
        .get_object_info()
        .delete_property_by_value(&mut vm.factory.memory_allocator, key)?;
    Ok(Value::bool(deleted))
}

pub fn reflect_get(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = *args.get(0).unwrap_or(&Value::undefined());
    if !is_object_value(target) {
        return Err(vm.current_context.error_type("Reflect.get"));
    }
    let key = *args.get(1).unwrap_or(&Value::undefined());
    let receiver = *args.get(2).unwrap_or(&target);
    match target
        .get_object_info()
        .get_property_by_value(&mut vm.factory, key)?
    {
        Property::Data(data) => Ok(data.val),
        Property::Accessor(accessor) => {
            if accessor.get.is_undefined() {
                Ok(Value::undefined())
            } else {
                vm.call_function(accessor.get, &[], receiver)
            }
        }
    }
}

pub fn reflect_get_own_property_descriptor(
    vm: &mut VM,
    args: &[Value],
    _this: Value,
) -> VMValueResult {
    if !is_object_value(*args.get(0).unwrap_or(&Value::undefined())) {
        return Err(vm
            .current_context
            .error_type("Reflect.getOwnPropertyDescriptor"));
    }
    object::object_get_own_property_descriptor(vm, args, Value::undefined())
}

pub fn reflect_get_prototype_of(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = *args.get(0).unwrap_or(&Value::undefined());
    if !is_object_value(target) {
        return Err(vm.current_context.error_type("Reflect.getPrototypeOf"));
    }
    if let ObjectKind::Proxy(proxy) = target.get_object_info().kind.clone() {
        return super::proxy::proxy_get_prototype_of(vm, proxy);
    }
    Ok(target.get_prototype())
}

pub fn reflect_has(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = *args.get(0).unwrap_or(&Value::undefined());
    if !is_object_value(target) {
        return Err(vm.current_context.error_type("Reflect.has"));
    }
    let key = *args.get(1).unwrap_or(&Value::undefined());
    if key.is_symbol() {
        return Ok(Value::bool(has_symbol_property(
            target,
            key.get_symbol_info().id,
        )));
    }
    vm.has_property(key, target)
}

pub fn reflect_is_extensible(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = *args.get(0).unwrap_or(&Value::undefined());
    if !is_object_value(target) {
        return Err(vm.current_context.error_type("Reflect.isExtensible"));
    }
    Ok(Value::bool(target.get_object_info().extensible))
}

pub fn reflect_own_keys(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = *args.get(0).unwrap_or(&Value::undefined());
    if !is_object_value(target) {
        return Err(vm.current_context.error_type("Reflect.ownKeys"));
    }
    let mut keys = Vec::new();
    let info = target.get_object_info();
    for key in info.own_string_property_keys() {
        keys.push(Property::new_data_simple(vm.factory.string(key)));
    }
    for symbol in &info.sym_property_order {
        if info.sym_property.contains_key(&symbol.get_symbol_info().id) {
            keys.push(Property::new_data_simple(*symbol));
        }
    }
    Ok(vm.factory.array(keys))
}

pub fn reflect_prevent_extensions(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = *args.get(0).unwrap_or(&Value::undefined());
    if !is_object_value(target) {
        return Err(vm.current_context.error_type("Reflect.preventExtensions"));
    }
    target.get_object_info().extensible = false;
    Ok(Value::bool(true))
}

pub fn reflect_set(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = *args.get(0).unwrap_or(&Value::undefined());
    if !is_object_value(target) {
        return Err(vm.current_context.error_type("Reflect.set"));
    }
    let key = *args.get(1).unwrap_or(&Value::undefined());
    let value = *args.get(2).unwrap_or(&Value::undefined());
    let receiver = *args.get(3).unwrap_or(&target);
    let (setter, success) = target.get_object_info().set_property_by_value(
        &mut vm.factory.memory_allocator,
        key,
        value,
    )?;
    if let Some(setter) = setter {
        vm.call_function(setter, &[value], receiver)?;
    }
    Ok(Value::bool(success))
}

pub fn reflect_set_prototype_of(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let target = *args.get(0).unwrap_or(&Value::undefined());
    let proto = *args.get(1).unwrap_or(&Value::undefined());
    if !is_object_value(target) || (!is_object_value(proto) && !proto.is_null()) {
        return Err(vm.current_context.error_type("Reflect.setPrototypeOf"));
    }
    if let ObjectKind::Proxy(proxy) = target.get_object_info().kind.clone() {
        return Ok(Value::bool(super::proxy::proxy_set_prototype_of(
            vm, proxy, proto,
        )?));
    }
    let success = super::object::ordinary_set_prototype_of(
        target,
        proto,
        target.strict_eq_bool(vm.factory.object_prototypes.object),
    );
    Ok(Value::bool(success))
}

fn create_list_from_array_like(
    vm: &mut VM,
    obj: Value,
) -> Result<Vec<Value>, crate::vm::error::RuntimeError> {
    if !is_object_value(obj) {
        return Err(vm.current_context.error_type("CreateListFromArrayLike"));
    }
    let length_key = vm.factory.string("length");
    let length = vm.get_property_by_value(obj, length_key)?;
    let len = to_length(vm, length);
    let mut list = Vec::with_capacity(len.min(1024));
    for index in 0..len {
        list.push(vm.get_property_by_value(obj, Value::Number(index as f64))?);
    }
    Ok(list)
}

fn is_builtin_named(value: Value, name: &str) -> bool {
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

fn fast_string_from_code_point(
    vm: &mut VM,
    args: Value,
) -> Result<Option<Value>, crate::vm::error::RuntimeError> {
    if !args.is_object() {
        return Ok(None);
    }
    let info = args.get_object_info();
    let ObjectKind::Array(ref array) = info.kind else {
        return Ok(None);
    };
    if array.elems.len() != array.length {
        return Ok(None);
    }

    let mut units = Vec::with_capacity(array.length);
    for element in &array.elems {
        let Property::Data(data) = element else {
            return Ok(None);
        };
        if !data.val.is_number() {
            return Ok(None);
        }
        let number = data.val.into_number();
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
    Ok(Some(vm.factory.string(String::from_utf16_lossy(&units))))
}

fn has_symbol_property(mut target: Value, id: usize) -> bool {
    while target.is_object() && !target.is_symbol() {
        let info = target.get_object_info();
        if info.sym_property.contains_key(&id) {
            return true;
        }
        target = info.prototype;
    }
    false
}

fn to_length(vm: &mut VM, value: Value) -> usize {
    const MAX_SAFE_INTEGER: f64 = 9_007_199_254_740_991.0;
    let number = value.to_number(&mut vm.factory.memory_allocator);
    if !number.is_finite() || number <= 0.0 {
        0
    } else {
        number.trunc().min(MAX_SAFE_INTEGER) as usize
    }
}
