use crate::vm::{
    jsvalue::{
        object::{DataProperty, Object, ObjectKind, Property, ProxyObjectInfo},
        value::*,
    },
    vm::{Factory, VMValueResult, VM},
};
use rustc_hash::FxHashMap;

pub fn proxy(factory: &mut Factory) -> Value {
    let constructor = factory.builtin_function("Proxy", proxy_constructor);
    set_length(constructor, 2.0);
    let revocable = factory.builtin_function("revocable", proxy_revocable);
    set_length(revocable, 2.0);
    constructor.get_object_info().insert_property(
        "revocable".to_string(),
        Property::new_data(
            DataProperty::new(revocable)
                .set_writable()
                .set_configurable(),
        ),
    );
    constructor
}

pub fn proxy_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    if !vm.builtin_constructor_call {
        return Err(vm.current_context.error_type("Proxy constructor"));
    }
    proxy_create(vm, args)
}

pub fn proxy_revocable(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let proxy = proxy_create(vm, args)?;
    let revoke = vm.factory.builtin_function("", proxy_revoke);
    let mut props = FxHashMap::default();
    props.insert(
        "proxy".to_string(),
        Property::new_data(DataProperty::new(proxy).set_writable().set_configurable()),
    );
    props.insert(
        "revoke".to_string(),
        Property::new_data(DataProperty::new(revoke).set_writable().set_configurable()),
    );
    Ok(vm.factory.object(props))
}

pub fn proxy_revoke(_vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    Ok(Value::undefined())
}

fn proxy_create(vm: &mut VM, args: &[Value]) -> VMValueResult {
    let target = args.get(0).copied().unwrap_or(Value::undefined());
    let handler = args.get(1).copied().unwrap_or(Value::undefined());
    if !is_object_type(target) || !is_object_type(handler) {
        return Err(vm.current_context.error_type("Proxy target/handler"));
    }
    Ok(Value::Object(vm.factory.alloc(Object {
        kind: ObjectKind::Proxy(ProxyObjectInfo { target, handler }),
        prototype: target.get_prototype(),
        property: FxHashMap::default(),
        property_order: Vec::new(),
        private_elements: rustc_hash::FxHashMap::default(),
        sym_property: FxHashMap::default(),
        sym_property_order: Vec::new(),
        extensible: true,
    })))
}

pub fn proxy_set_prototype_of(
    vm: &mut VM,
    proxy: ProxyObjectInfo,
    proto: Value,
) -> Result<bool, crate::vm::error::RuntimeError> {
    let trap_key = vm.factory.string("setPrototypeOf");
    let trap = vm.get_property_by_value(proxy.handler, trap_key)?;
    if trap.is_undefined() || trap.is_null() {
        return proxy_target_set_prototype_of(vm, proxy.target, proto);
    }
    if !vm.is_callable(trap) {
        return Err(vm.current_context.error_type("Proxy setPrototypeOf"));
    }

    let trap_result = vm.call_function(trap, &[proxy.target, proto], proxy.handler)?;
    let success = trap_result.to_boolean();
    if success && !proxy_target_is_extensible(vm, proxy.target)? {
        let target_proto = proxy_target_get_prototype_of(vm, proxy.target)?;
        if !target_proto.strict_eq_bool(proto) {
            return Err(vm.current_context.error_type("Proxy setPrototypeOf"));
        }
    }
    Ok(success)
}

pub fn proxy_get_prototype_of(
    vm: &mut VM,
    proxy: ProxyObjectInfo,
) -> Result<Value, crate::vm::error::RuntimeError> {
    let trap_key = vm.factory.string("getPrototypeOf");
    let trap = vm.get_property_by_value(proxy.handler, trap_key)?;
    if trap.is_undefined() || trap.is_null() {
        return proxy_target_get_prototype_of(vm, proxy.target);
    }
    if !vm.is_callable(trap) {
        return Err(vm.current_context.error_type("Proxy getPrototypeOf"));
    }

    let result = vm.call_function(trap, &[proxy.target], proxy.handler)?;
    if !is_object_type(result) && !result.is_null() {
        return Err(vm.current_context.error_type("Proxy getPrototypeOf"));
    }
    if !proxy_target_is_extensible(vm, proxy.target)? {
        let target_proto = proxy_target_get_prototype_of(vm, proxy.target)?;
        if !result.strict_eq_bool(target_proto) {
            return Err(vm.current_context.error_type("Proxy getPrototypeOf"));
        }
    }
    Ok(result)
}

fn proxy_target_set_prototype_of(
    vm: &mut VM,
    target: Value,
    proto: Value,
) -> Result<bool, crate::vm::error::RuntimeError> {
    if let ObjectKind::Proxy(proxy) = target.get_object_info().kind.clone() {
        return proxy_set_prototype_of(vm, proxy, proto);
    }
    Ok(super::object::ordinary_set_prototype_of(
        target,
        proto,
        target.strict_eq_bool(vm.factory.object_prototypes.object),
    ))
}

fn proxy_target_get_prototype_of(
    vm: &mut VM,
    target: Value,
) -> Result<Value, crate::vm::error::RuntimeError> {
    if let ObjectKind::Proxy(proxy) = target.get_object_info().kind.clone() {
        return proxy_get_prototype_of(vm, proxy);
    }
    Ok(target.get_prototype())
}

fn proxy_target_is_extensible(
    vm: &mut VM,
    target: Value,
) -> Result<bool, crate::vm::error::RuntimeError> {
    if let ObjectKind::Proxy(proxy) = target.get_object_info().kind.clone() {
        return proxy_is_extensible(vm, proxy);
    }
    Ok(target.get_object_info().extensible)
}

fn proxy_is_extensible(
    vm: &mut VM,
    proxy: ProxyObjectInfo,
) -> Result<bool, crate::vm::error::RuntimeError> {
    let trap_key = vm.factory.string("isExtensible");
    let trap = vm.get_property_by_value(proxy.handler, trap_key)?;
    if trap.is_undefined() || trap.is_null() {
        return proxy_target_is_extensible(vm, proxy.target);
    }
    if !vm.is_callable(trap) {
        return Err(vm.current_context.error_type("Proxy isExtensible"));
    }
    Ok(vm
        .call_function(trap, &[proxy.target], proxy.handler)?
        .to_boolean())
}

fn is_object_type(value: Value) -> bool {
    value.is_object() && !value.is_symbol() && !value.is_bigint()
}

fn set_length(func: Value, length: f64) {
    func.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(length)).set_configurable()),
    );
}
