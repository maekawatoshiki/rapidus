use crate::builtins::promise;
use crate::vm::{
    jsvalue::{
        object::{DataProperty, Object, ObjectKind, Property, ShadowRealmObjectInfo},
        symbol::{SYMBOL_TO_PRIMITIVE_ID, SYMBOL_TO_STRING_TAG_ID},
        value::{cstrp_to_str, Value},
    },
    vm::{Factory, VMValueResult, VM},
};
use rapidus_parser::Parser;
use rustc_hash::FxHashMap;

pub fn shadow_realm(factory: &mut Factory) -> Value {
    let prototype = shadow_realm_prototype(factory);
    let constructor =
        factory.generate_builtin_constructor("ShadowRealm", shadow_realm_constructor, prototype);
    constructor.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
    );
    constructor
}

fn shadow_realm_prototype(factory: &mut Factory) -> Value {
    let evaluate = method(factory, "evaluate", shadow_realm_prototype_evaluate, 1.0);
    let import_value = method(
        factory,
        "importValue",
        shadow_realm_prototype_import_value,
        2.0,
    );
    let tag = factory.string("ShadowRealm");
    let prototype = Value::Object(factory.alloc(Object {
        kind: ObjectKind::Ordinary,
        prototype: factory.object_prototypes.object,
        property: make_property_map!(
            evaluate => true, false, true: evaluate,
            importValue => true, false, true: import_value
        ),
        property_order: make_property_order!(
            evaluate => true, false, true: evaluate,
            importValue => true, false, true: import_value
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

pub fn shadow_realm_constructor(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if !vm.builtin_constructor_call {
        return Err(vm.current_context.error_type("ShadowRealm constructor"));
    }
    let realm = Box::leak(Box::new(VM::new()));
    this.get_object_info().kind = ObjectKind::ShadowRealm(ShadowRealmObjectInfo {
        realm: realm as *mut VM,
    });
    Ok(this)
}

pub fn shadow_realm_prototype_evaluate(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let source_text = args.get(0).copied().unwrap_or(Value::undefined());
    if !source_text.is_string() {
        return Err(vm
            .current_context
            .error_type("ShadowRealm.prototype.evaluate sourceText"));
    }
    let realm = shadow_realm_from_this(vm, this)?;
    let source_text = source_text.to_string();
    let result = evaluate_in_realm(vm, realm, source_text)?;
    copy_shadow_realm_value(vm, result)
}

pub fn shadow_realm_prototype_import_value(
    vm: &mut VM,
    args: &[Value],
    this: Value,
) -> VMValueResult {
    let _realm = shadow_realm_from_this(vm, this)?;
    let specifier = args.get(0).copied().unwrap_or(Value::undefined());
    let _specifier = to_string_for_import(vm, specifier)?;
    let export_name = args.get(1).copied().unwrap_or(Value::undefined());
    if !export_name.is_string() {
        return Err(vm
            .current_context
            .error_type("ShadowRealm.prototype.importValue exportName"));
    }
    let reason = vm
        .factory
        .native_error("TypeError", "ShadowRealm importValue is not implemented");
    promise::promise_reject_direct(vm, reason)
}

fn shadow_realm_from_this(
    vm: &mut VM,
    this: Value,
) -> Result<*mut VM, crate::vm::error::RuntimeError> {
    if let Value::Object(info) = this {
        if let ObjectKind::ShadowRealm(ref realm) = crate::vm::jsvalue::object::ObjectRef(info).kind
        {
            return Ok(realm.realm);
        }
    }
    Err(vm
        .current_context
        .error_type("ShadowRealm prototype method"))
}

fn evaluate_in_realm(
    vm: &mut VM,
    realm: *mut VM,
    source: String,
) -> Result<Value, crate::vm::error::RuntimeError> {
    let realm = unsafe { &mut *realm };
    let mut parser = Parser::new("ShadowRealm", source);
    let node = parser.parse_all().map_err(|err| {
        let error = vm.factory.native_error("SyntaxError", format!("{:?}", err));
        vm.current_context.error_exception(error)
    })?;
    let func_info = realm.compile(&node, true).map_err(|err| {
        let error = vm.factory.native_error("SyntaxError", err.msg);
        vm.current_context.error_exception(error)
    })?;
    let script_info = parser.into_script_info();
    realm
        .script_info
        .insert(func_info.module_func_id, script_info);
    realm.current_context = realm.create_global_context(func_info);
    realm.run().map_err(|err| {
        let error = vm.factory.native_error("TypeError", format!("{:?}", err));
        vm.current_context.error_exception(error)
    })
}

fn copy_shadow_realm_value(vm: &mut VM, value: Value) -> VMValueResult {
    if value.is_empty() {
        return Ok(Value::undefined());
    }
    match value {
        Value::String(ptr) => Ok(vm.factory.string(cstrp_to_str(ptr).to_string())),
        Value::Object(info) => {
            let object = crate::vm::jsvalue::object::ObjectRef(info);
            match object.kind {
                ObjectKind::Symbol(ref symbol) => {
                    let copied = vm
                        .factory
                        .symbol_with_id(symbol.id, symbol.description.clone());
                    copied.get_symbol_info().registered = symbol.registered;
                    Ok(copied)
                }
                ObjectKind::BigInt(ref bigint) => Ok(vm.factory.bigint(bigint.decimal.clone())),
                ObjectKind::Function(_) => Err(vm
                    .current_context
                    .error_type("ShadowRealm callable wrapping")),
                _ => Err(vm
                    .current_context
                    .error_type("ShadowRealm non-primitive result")),
            }
        }
        _ => Ok(value),
    }
}

fn to_string_for_import(
    vm: &mut VM,
    value: Value,
) -> Result<String, crate::vm::error::RuntimeError> {
    if is_primitive_for_to_string(value) {
        if value.is_symbol() {
            return Err(vm
                .current_context
                .error_type("Cannot convert Symbol to string"));
        }
        return Ok(value.to_string());
    }

    let to_primitive_key = vm.factory.symbol_with_id(
        SYMBOL_TO_PRIMITIVE_ID,
        Some("Symbol.toPrimitive".to_string()),
    );
    let to_primitive = vm.get_property_by_value(value, to_primitive_key)?;
    if !to_primitive.is_undefined() && !to_primitive.is_null() {
        if !to_primitive.is_function_object() {
            return Err(vm.current_context.error_type("Symbol.toPrimitive"));
        }
        let hint = vm.factory.string("string");
        let primitive = vm.call_function(to_primitive, &[hint], value)?;
        if !is_primitive_for_to_string(primitive) {
            return Err(vm.current_context.error_type("Symbol.toPrimitive"));
        }
        return to_string_for_import(vm, primitive);
    }

    for name in ["toString", "valueOf"] {
        let key = vm.factory.string(name);
        let method = vm.get_property_by_value(value, key)?;
        if method.is_function_object() {
            let primitive = vm.call_function(method, &[], value)?;
            if is_primitive_for_to_string(primitive) {
                return to_string_for_import(vm, primitive);
            }
        }
    }

    Err(vm
        .current_context
        .error_type("Cannot convert object to string"))
}

fn is_primitive_for_to_string(value: Value) -> bool {
    !value.is_object() || value.is_symbol() || value.is_bigint()
}
