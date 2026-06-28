use super::{array_buffer, eval, VMValueResult};
use crate::vm::{
    jsvalue::{
        object::{DataProperty, Property},
        value::Value,
    },
    vm::VM,
};
use rustc_hash::FxHashMap;

pub fn test262_detach_array_buffer(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let buffer = args.get(0).copied().unwrap_or(Value::undefined());
    if array_buffer::detach_array_buffer(buffer) {
        Ok(Value::undefined())
    } else {
        Err(vm.current_context.error_type("$262.detachArrayBuffer"))
    }
}

pub fn test262_gc(_vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    Ok(Value::undefined())
}

pub fn test262_create_realm(vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    let realm = Box::leak(Box::new(VM::new()));
    let global = realm.global_environment.get_global_object();
    let mut property = FxHashMap::default();
    property.insert(
        "global".to_string(),
        Property::new_data(DataProperty::new(global).set_writable().set_configurable()),
    );
    Ok(vm.factory.object(property))
}

pub fn test262_eval_script(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let global = vm.global_environment.get_global_object();
    eval(vm, args, global)
}

pub fn test262_drain_promise_jobs(vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    vm.drain_promise_jobs()?;
    Ok(Value::undefined())
}
