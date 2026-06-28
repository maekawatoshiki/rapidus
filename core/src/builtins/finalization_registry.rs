use crate::vm::{
    jsvalue::{
        object::{
            DataProperty, FinalizationRegistryCell, FinalizationRegistryObjectInfo, Object,
            ObjectKind, Property,
        },
        symbol::SYMBOL_TO_STRING_TAG_ID,
        value::Value,
    },
    vm::{Factory, VMValueResult, VM},
};
use rustc_hash::FxHashMap;

pub fn finalization_registry(factory: &mut Factory) -> Value {
    let prototype = finalization_registry_prototype(factory);
    let constructor = factory.generate_builtin_constructor(
        "FinalizationRegistry",
        finalization_registry_constructor,
        prototype,
    );
    constructor.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
    );
    constructor
}

fn finalization_registry_prototype(factory: &mut Factory) -> Value {
    let register = method(
        factory,
        "register",
        finalization_registry_prototype_register,
        2.0,
    );
    let unregister = method(
        factory,
        "unregister",
        finalization_registry_prototype_unregister,
        1.0,
    );
    let cleanup_some = method(
        factory,
        "cleanupSome",
        finalization_registry_prototype_cleanup_some,
        0.0,
    );
    let tag = factory.string("FinalizationRegistry");
    let prototype = Value::Object(factory.alloc(Object {
        kind: ObjectKind::Ordinary,
        prototype: factory.object_prototypes.object,
        property: make_property_map!(
            register => true, false, true: register,
            unregister => true, false, true: unregister,
            cleanupSome => true, false, true: cleanup_some
        ),
        property_order: make_property_order!(
            register => true, false, true: register,
            unregister => true, false, true: unregister,
            cleanupSome => true, false, true: cleanup_some
        ),
        private_elements: rustc_hash::FxHashMap::default(),
        sym_property: FxHashMap::default(),
        sym_property_order: Vec::new(),
        extensible: true,
    }));
    super::helpers::define_well_known_symbol_property(
        factory,
        prototype,
        SYMBOL_TO_STRING_TAG_ID,
        Property::new_data(DataProperty::new(tag).set_configurable()),
    );
    prototype
}

fn method(
    factory: &mut Factory,
    name: &str,
    func: crate::builtins::BuiltinFuncTy,
    length: f64,
) -> Value {
    let method = factory.builtin_function(name, func);
    method.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(length)).set_configurable()),
    );
    method
}

pub fn finalization_registry_constructor(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    if !vm.builtin_constructor_call {
        return Err(vm
            .current_context
            .error_type("FinalizationRegistry constructor"));
    }
    let cleanup_callback = args.get(0).copied().unwrap_or(Value::undefined());
    if !cleanup_callback.is_function_object() {
        return Err(vm
            .current_context
            .error_type("FinalizationRegistry cleanup callback"));
    }
    this.get_object_info().kind =
        ObjectKind::FinalizationRegistry(FinalizationRegistryObjectInfo {
            cleanup_callback,
            cells: Vec::new(),
        });
    Ok(this)
}

pub fn finalization_registry_prototype_register(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    if !this.is_object() {
        return Err(vm
            .current_context
            .error_type("FinalizationRegistry.prototype.register"));
    }
    let target = args.get(0).copied().unwrap_or(Value::undefined());
    if !can_be_held_weakly(target) {
        return Err(vm.current_context.error_type("FinalizationRegistry target"));
    }
    let holdings = args.get(1).copied().unwrap_or(Value::undefined());
    if target.strict_eq_bool(holdings) {
        return Err(vm
            .current_context
            .error_type("FinalizationRegistry holdings"));
    }
    let unregister_token = args.get(2).copied().unwrap_or(Value::undefined());
    let unregister_token = if unregister_token.is_undefined() {
        None
    } else if can_be_held_weakly(unregister_token) {
        Some(unregister_token)
    } else {
        return Err(vm
            .current_context
            .error_type("FinalizationRegistry unregister token"));
    };

    let mut object = this.get_object_info();
    match object.kind {
        ObjectKind::FinalizationRegistry(ref mut info) => {
            info.cells.push(FinalizationRegistryCell {
                target,
                holdings,
                unregister_token,
            });
            Ok(Value::undefined())
        }
        _ => Err(vm
            .current_context
            .error_type("FinalizationRegistry.prototype.register")),
    }
}

pub fn finalization_registry_prototype_unregister(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    if !this.is_object() {
        return Err(vm
            .current_context
            .error_type("FinalizationRegistry.prototype.unregister"));
    }
    let unregister_token = args.get(0).copied().unwrap_or(Value::undefined());
    if !can_be_held_weakly(unregister_token) {
        return Err(vm
            .current_context
            .error_type("FinalizationRegistry unregister token"));
    }
    let mut object = this.get_object_info();
    match object.kind {
        ObjectKind::FinalizationRegistry(ref mut info) => {
            let before = info.cells.len();
            info.cells.retain(|cell| {
                cell.unregister_token
                    .map(|token| !token.strict_eq_bool(unregister_token))
                    .unwrap_or(true)
            });
            Ok(Value::bool(info.cells.len() != before))
        }
        _ => Err(vm
            .current_context
            .error_type("FinalizationRegistry.prototype.unregister")),
    }
}

pub fn finalization_registry_prototype_cleanup_some(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    if !this.is_object() {
        return Err(vm
            .current_context
            .error_type("FinalizationRegistry.prototype.cleanupSome"));
    }
    let callback = args.get(0).copied().unwrap_or(Value::undefined());
    if !callback.is_undefined() && !callback.is_function_object() {
        return Err(vm
            .current_context
            .error_type("FinalizationRegistry cleanup callback"));
    }
    let object = this.get_object_info();
    match object.kind {
        ObjectKind::FinalizationRegistry(_) => Ok(Value::undefined()),
        _ => Err(vm
            .current_context
            .error_type("FinalizationRegistry.prototype.cleanupSome")),
    }
}

fn can_be_held_weakly(value: Value) -> bool {
    if !value.is_object() {
        return false;
    }
    let obj = value.get_object_info();
    match obj.kind {
        ObjectKind::Symbol(ref info) => !info.registered,
        _ => true,
    }
}
