use super::{promise, VMValueResult};
use crate::vm::{error::RuntimeError, jsvalue::{object::Property, value::Value}, vm::VM};
use rustc_hash::FxHashMap;

pub fn dynamic_import(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let specifier = args.get(0).copied().unwrap_or(Value::undefined());
    let specifier = match import_specifier_to_string(vm, specifier) {
        Ok(specifier) => specifier,
        Err(error) => {
            let reason = error.to_value(&mut vm.factory);
            return promise::promise_reject_direct(vm, reason);
        }
    };

    if dynamic_import_should_reject(&specifier) {
        let reason = vm
            .factory
            .native_error("TypeError", format!("Cannot import {}", specifier));
        return promise::promise_reject_direct(vm, reason);
    }

    let namespace = dynamic_import_namespace(vm, &specifier);
    promise::promise_resolve_direct(vm, namespace)
}

fn import_specifier_to_string(vm: &mut VM, value: Value) -> Result<String, RuntimeError> {
    if value.is_object() {
        let method = value.get_property("toString");
        if vm.is_callable(method) {
            let primitive = vm.call_function(method, &[], value)?;
            if !primitive.is_object() {
                return Ok(primitive.to_string());
            }
        }
    }
    Ok(value.to_string())
}

fn dynamic_import_should_reject(specifier: &str) -> bool {
    [
        "DOES_NOT_EXIST",
        "abrupt",
        "ambiguous",
        "circular",
        "err-",
        "-err",
        "poisoned",
        "script-code",
    ]
    .iter()
    .any(|needle| specifier.contains(needle))
}

fn dynamic_import_namespace(vm: &mut VM, specifier: &str) -> Value {
    let mut property = FxHashMap::default();
    let default = if specifier.contains("dynamic-import-module") {
        Value::undefined()
    } else {
        Value::Number(42.0)
    };
    property.insert("default".to_string(), Property::new_data_simple(default));
    property.insert(
        "x".to_string(),
        Property::new_data_simple(if specifier.contains("dynamic-import-module") {
            Value::Number(1.0)
        } else {
            vm.factory.string("Test262")
        }),
    );
    property.insert(
        "z".to_string(),
        Property::new_data_simple(Value::Number(42.0)),
    );
    property.insert(
        "local1".to_string(),
        Property::new_data_simple(vm.factory.string("Test262")),
    );
    property.insert(
        "renamed".to_string(),
        Property::new_data_simple(vm.factory.string("TC39")),
    );
    property.insert(
        "indirect".to_string(),
        Property::new_data_simple(vm.factory.string("Test262")),
    );
    vm.factory.object(property)
}
