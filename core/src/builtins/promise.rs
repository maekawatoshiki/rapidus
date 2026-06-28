use super::helpers::{
    builtin_function_with_length, define_species_getter, define_well_known_symbol_property,
};
use crate::builtins;
use crate::vm::{
    error::RuntimeError,
    jsvalue::{
        object::{property_order_from_map, DataProperty, Object, ObjectKind, Property},
        symbol::{SYMBOL_ITERATOR_ID, SYMBOL_TO_STRING_TAG_ID},
        value::Value,
    },
    vm::{Factory, VMValueResult, VM},
};
use rustc_hash::FxHashMap;

const STATE_PENDING: i32 = 0;
const STATE_FULFILLED: i32 = 1;
const STATE_REJECTED: i32 = 2;

pub fn promise(factory: &mut Factory) -> Value {
    let function_prototype = factory.object_prototypes.function;
    let object_prototype = factory.object_prototypes.object;
    let then = builtin_function_with_length(factory, "then", promise_prototype_then, 2.0);
    let catch = builtin_function_with_length(factory, "catch", promise_prototype_catch, 1.0);
    let finally = builtin_function_with_length(factory, "finally", promise_prototype_finally, 1.0);
    let tag = factory.string("Promise");
    let property = make_property_map!(
        then => true, false, true: then,
        catch => true, false, true: catch,
        finally => true, false, true: finally
    );
    let property_order = property_order_from_map(&property);
    let prototype = Value::Object(factory.alloc(Object {
        kind: ObjectKind::Ordinary,
        prototype: object_prototype,
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

    let constructor =
        factory.generate_builtin_constructor("Promise", promise_constructor, prototype);
    constructor.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
    );
    define_species_getter(factory, constructor);
    for (name, func, length) in [
        ("resolve", promise_resolve as builtins::BuiltinFuncTy, 1.0),
        ("reject", promise_reject, 1.0),
        ("all", promise_all, 1.0),
        ("race", promise_race, 1.0),
        ("allSettled", promise_all_settled, 1.0),
        ("any", promise_any, 1.0),
    ] {
        let builtin = Value::builtin_function_with_proto(
            &mut factory.memory_allocator,
            function_prototype,
            name,
            func,
        );
        builtin.get_object_info().insert_property(
            "length".to_string(),
            Property::new_data(DataProperty::new(Value::Number(length)).set_configurable()),
        );
        constructor.get_object_info().insert_property(
            name.to_string(),
            Property::new_data(DataProperty::new(builtin).set_writable().set_configurable()),
        );
    }
    prototype.get_object_info().insert_property(
        "constructor".to_string(),
        Property::new_data(
            DataProperty::new(constructor)
                .set_writable()
                .set_configurable(),
        ),
    );
    constructor
}

pub fn promise_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !vm.builtin_constructor_call || !this.is_object() {
        return Err(vm.current_context.error_type("Promise constructor"));
    }
    let executor = args.get(0).copied().unwrap_or(Value::undefined());
    if !executor.is_function_object() {
        return Err(vm.current_context.error_type("Promise executor"));
    }

    init_promise(this);
    let resolve = resolving_function(vm, this, true);
    let reject = resolving_function(vm, this, false);
    if let Err(error) = vm.call_function(executor, &[resolve, reject], Value::undefined()) {
        let reason = error.to_value(&mut vm.factory);
        reject_promise(vm, this, reason)?;
    }
    Ok(this)
}

pub fn promise_resolve(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = args.get(0).copied().unwrap_or(Value::undefined());
    if is_promise(value) {
        let constructor_key = vm.factory.string("constructor");
        let constructor = vm.get_property_by_value(value, constructor_key)?;
        if constructor == this {
            return Ok(value);
        }
    }
    let capability = new_promise_capability(vm, this)?;
    vm.call_function(capability.resolve, &[value], Value::undefined())?;
    Ok(capability.promise)
}

pub fn promise_reject(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let reason = args.get(0).copied().unwrap_or(Value::undefined());
    let capability = new_promise_capability(vm, this)?;
    vm.call_function(capability.reject, &[reason], Value::undefined())?;
    Ok(capability.promise)
}

pub fn promise_resolve_direct(vm: &mut VM, value: Value) -> VMValueResult {
    let constructor = vm.global_environment.get_value("Promise")?;
    let promise = new_promise(vm, constructor);
    resolve_promise(vm, promise, value)?;
    Ok(promise)
}

pub fn promise_reject_direct(vm: &mut VM, reason: Value) -> VMValueResult {
    let constructor = vm.global_environment.get_value("Promise")?;
    let promise = new_promise(vm, constructor);
    reject_promise(vm, promise, reason)?;
    Ok(promise)
}

pub fn promise_prototype_then(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !is_promise(this) {
        return Err(vm.current_context.error_type("Promise.prototype.then"));
    }
    let on_fulfilled = args.get(0).copied().unwrap_or(Value::undefined());
    let on_rejected = args.get(1).copied().unwrap_or(Value::undefined());
    let constructor = vm.current_context.lex_env().get_value("Promise")?;
    let result = new_promise(vm, constructor);
    add_or_run_reaction(vm, this, result, on_fulfilled, on_rejected)?;
    Ok(result)
}

pub fn promise_prototype_catch(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let on_rejected = args.get(0).copied().unwrap_or(Value::undefined());
    promise_prototype_then(vm, &[Value::undefined(), on_rejected], this)
}

pub fn promise_prototype_finally(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let on_finally = args.get(0).copied().unwrap_or(Value::undefined());
    promise_prototype_then(vm, &[on_finally, on_finally], this)
}

pub fn promise_all(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let capability = new_promise_capability(vm, this)?;
    let promise_resolve = match get_promise_resolve(vm, this) {
        Ok(resolve) => resolve,
        Err(error) => return reject_capability(vm, capability, error),
    };
    let values = match collect_iterable(vm, args.get(0).copied().unwrap_or(Value::undefined())) {
        Ok(values) => values,
        Err(error) => return reject_capability(vm, capability, error),
    };
    let results = vm.factory.array(
        (0..values.len())
            .map(|_| Property::new_data_simple(Value::undefined()))
            .collect(),
    );
    if values.is_empty() {
        vm.call_function(capability.resolve, &[results], Value::undefined())?;
        return Ok(capability.promise);
    }
    let remaining = number_record(vm, values.len());
    for (index, value) in values.into_iter().enumerate() {
        let next = match vm.call_function(promise_resolve, &[value], this) {
            Ok(next) => next,
            Err(error) => return reject_capability(vm, capability, error),
        };
        let on_fulfilled =
            promise_all_resolve_element(vm, results, remaining, capability.resolve, index);
        if let Err(error) = invoke_then(vm, next, on_fulfilled, capability.reject) {
            return reject_capability(vm, capability, error);
        }
    }
    Ok(capability.promise)
}

pub fn promise_race(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let capability = new_promise_capability(vm, this)?;
    let promise_resolve = match get_promise_resolve(vm, this) {
        Ok(resolve) => resolve,
        Err(error) => return reject_capability(vm, capability, error),
    };
    let values = match collect_iterable(vm, args.get(0).copied().unwrap_or(Value::undefined())) {
        Ok(values) => values,
        Err(error) => return reject_capability(vm, capability, error),
    };
    for value in values {
        let next = match vm.call_function(promise_resolve, &[value], this) {
            Ok(next) => next,
            Err(error) => return reject_capability(vm, capability, error),
        };
        if let Err(error) = invoke_then(vm, next, capability.resolve, capability.reject) {
            return reject_capability(vm, capability, error);
        }
    }
    Ok(capability.promise)
}

pub fn promise_all_settled(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let capability = new_promise_capability(vm, this)?;
    let promise_resolve = match get_promise_resolve(vm, this) {
        Ok(resolve) => resolve,
        Err(error) => return reject_capability(vm, capability, error),
    };
    let values = match collect_iterable(vm, args.get(0).copied().unwrap_or(Value::undefined())) {
        Ok(values) => values,
        Err(error) => return reject_capability(vm, capability, error),
    };
    let results = vm.factory.array(
        (0..values.len())
            .map(|_| Property::new_data_simple(Value::undefined()))
            .collect(),
    );
    if values.is_empty() {
        vm.call_function(capability.resolve, &[results], Value::undefined())?;
        return Ok(capability.promise);
    }
    let remaining = number_record(vm, values.len());
    for (index, value) in values.into_iter().enumerate() {
        let next = match vm.call_function(promise_resolve, &[value], this) {
            Ok(next) => next,
            Err(error) => return reject_capability(vm, capability, error),
        };
        let on_fulfilled =
            promise_all_settled_element(vm, results, remaining, capability.resolve, index, true);
        let on_rejected =
            promise_all_settled_element(vm, results, remaining, capability.resolve, index, false);
        if let Err(error) = invoke_then(vm, next, on_fulfilled, on_rejected) {
            return reject_capability(vm, capability, error);
        }
    }
    Ok(capability.promise)
}

pub fn promise_any(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let capability = new_promise_capability(vm, this)?;
    let promise_resolve = match get_promise_resolve(vm, this) {
        Ok(resolve) => resolve,
        Err(error) => return reject_capability(vm, capability, error),
    };
    let values = match collect_iterable(vm, args.get(0).copied().unwrap_or(Value::undefined())) {
        Ok(values) => values,
        Err(error) => return reject_capability(vm, capability, error),
    };
    let errors = vm.factory.array(
        (0..values.len())
            .map(|_| Property::new_data_simple(Value::undefined()))
            .collect(),
    );
    if values.is_empty() {
        let error = aggregate_error(vm, errors);
        vm.call_function(capability.reject, &[error], Value::undefined())?;
        return Ok(capability.promise);
    }
    let remaining = number_record(vm, values.len());
    for (index, value) in values.into_iter().enumerate() {
        let next = match vm.call_function(promise_resolve, &[value], this) {
            Ok(next) => next,
            Err(error) => return reject_capability(vm, capability, error),
        };
        let on_rejected =
            promise_any_reject_element(vm, errors, remaining, capability.reject, index);
        if let Err(error) = invoke_then(vm, next, capability.resolve, on_rejected) {
            return reject_capability(vm, capability, error);
        }
    }
    Ok(capability.promise)
}

#[derive(Clone, Copy)]
struct PromiseCapability {
    promise: Value,
    resolve: Value,
    reject: Value,
}

fn new_promise_capability(
    vm: &mut VM,
    constructor: Value,
) -> Result<PromiseCapability, RuntimeError> {
    if !constructor.is_object() || !vm.is_constructor(constructor) {
        return Err(vm.current_context.error_type("Promise constructor"));
    }
    let (executor, executor_state) = capability_executor(vm);
    let promise = vm.construct_function(constructor, &[executor])?;
    let resolve = executor_state.get_property("__capability_resolve");
    let reject = executor_state.get_property("__capability_reject");
    if !vm.is_callable(resolve) || !vm.is_callable(reject) {
        return Err(vm.current_context.error_type("Promise capability"));
    }
    Ok(PromiseCapability {
        promise,
        resolve,
        reject,
    })
}

fn capability_executor(vm: &mut VM) -> (Value, Value) {
    let target = vm.factory.builtin_function("", promise_capability_executor);
    target.set_property("__capability_resolve", Value::undefined());
    target.set_property("__capability_reject", Value::undefined());
    let wrapper = bound_builtin(vm, "", promise_capability_executor, target, 2.0);
    (wrapper, target)
}

fn promise_capability_executor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !this.get_property("__capability_resolve").is_undefined()
        || !this.get_property("__capability_reject").is_undefined()
    {
        return Err(vm.current_context.error_type("Promise capability"));
    }
    this.set_property(
        "__capability_resolve",
        args.get(0).copied().unwrap_or(Value::undefined()),
    );
    this.set_property(
        "__capability_reject",
        args.get(1).copied().unwrap_or(Value::undefined()),
    );
    Ok(Value::undefined())
}

fn reject_capability(
    vm: &mut VM,
    capability: PromiseCapability,
    error: RuntimeError,
) -> VMValueResult {
    let reason = error.to_value(&mut vm.factory);
    vm.call_function(capability.reject, &[reason], Value::undefined())?;
    Ok(capability.promise)
}

fn get_promise_resolve(vm: &mut VM, constructor: Value) -> Result<Value, RuntimeError> {
    let key = vm.factory.string("resolve");
    let resolve = vm.get_property_by_value(constructor, key)?;
    if !vm.is_callable(resolve) {
        return Err(vm.current_context.error_type("Promise resolve"));
    }
    Ok(resolve)
}

fn invoke_then(
    vm: &mut VM,
    promise: Value,
    on_fulfilled: Value,
    on_rejected: Value,
) -> Result<Value, RuntimeError> {
    let then_key = vm.factory.string("then");
    let then = vm.get_property_by_value(promise, then_key)?;
    if !vm.is_callable(then) {
        return Err(vm.current_context.error_type("Promise then"));
    }
    vm.call_function(then, &[on_fulfilled, on_rejected], promise)
}

fn number_record(vm: &mut VM, value: usize) -> Value {
    let mut props = FxHashMap::default();
    props.insert(
        "value".to_string(),
        Property::new_data_simple(Value::Number(value as f64)),
    );
    vm.factory.object(props)
}

fn decrement_record(record: Value) -> usize {
    let next = record.get_property("value").into_number() as usize - 1;
    record.set_property("value", Value::Number(next as f64));
    next
}

fn bound_builtin(
    vm: &mut VM,
    name: &str,
    func: builtins::BuiltinFuncTy,
    target: Value,
    length: f64,
) -> Value {
    let wrapper = vm.factory.builtin_function(name, func);
    wrapper.set_property("__bound_target", target);
    wrapper.set_property("__bound_this", target);
    wrapper.set_property("__bound_arg_count", Value::Number(0.0));
    wrapper.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(length)).set_configurable()),
    );
    wrapper
}

fn promise_all_resolve_element(
    vm: &mut VM,
    values: Value,
    remaining: Value,
    resolve: Value,
    index: usize,
) -> Value {
    let target = vm
        .factory
        .builtin_function("", promise_all_resolve_element_function);
    target.set_property("__already_called", Value::bool(false));
    target.set_property("__values", values);
    target.set_property("__remaining", remaining);
    target.set_property("__resolve", resolve);
    target.set_property("__index", Value::Number(index as f64));
    bound_builtin(vm, "", promise_all_resolve_element_function, target, 1.0)
}

fn promise_all_resolve_element_function(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if this.get_property("__already_called").to_boolean() {
        return Ok(Value::undefined());
    }
    this.set_property("__already_called", Value::bool(true));
    let values = this.get_property("__values");
    let index = this.get_property("__index").into_number() as usize;
    let value = args.get(0).copied().unwrap_or(Value::undefined());
    vm.set_property_by_value(values, Value::Number(index as f64), value)?;
    if decrement_record(this.get_property("__remaining")) == 0 {
        let resolve = this.get_property("__resolve");
        vm.call_function(resolve, &[values], Value::undefined())?;
    }
    Ok(Value::undefined())
}

fn promise_all_settled_element(
    vm: &mut VM,
    values: Value,
    remaining: Value,
    resolve: Value,
    index: usize,
    fulfilled: bool,
) -> Value {
    let target = vm
        .factory
        .builtin_function("", promise_all_settled_element_function);
    target.set_property("__already_called", Value::bool(false));
    target.set_property("__values", values);
    target.set_property("__remaining", remaining);
    target.set_property("__resolve", resolve);
    target.set_property("__index", Value::Number(index as f64));
    target.set_property("__fulfilled", Value::bool(fulfilled));
    bound_builtin(vm, "", promise_all_settled_element_function, target, 1.0)
}

fn promise_all_settled_element_function(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if this.get_property("__already_called").to_boolean() {
        return Ok(Value::undefined());
    }
    this.set_property("__already_called", Value::bool(true));
    let fulfilled = this.get_property("__fulfilled").to_boolean();
    let settled = args.get(0).copied().unwrap_or(Value::undefined());
    let status = if fulfilled { "fulfilled" } else { "rejected" };
    let key = if fulfilled { "value" } else { "reason" };
    let mut props = FxHashMap::default();
    props.insert(
        "status".to_string(),
        Property::new_data_simple(vm.factory.string(status.to_string())),
    );
    props.insert(key.to_string(), Property::new_data_simple(settled));
    let entry = vm.factory.object(props);
    let values = this.get_property("__values");
    let index = this.get_property("__index").into_number() as usize;
    vm.set_property_by_value(values, Value::Number(index as f64), entry)?;
    if decrement_record(this.get_property("__remaining")) == 0 {
        let resolve = this.get_property("__resolve");
        vm.call_function(resolve, &[values], Value::undefined())?;
    }
    Ok(Value::undefined())
}

fn promise_any_reject_element(
    vm: &mut VM,
    errors: Value,
    remaining: Value,
    reject: Value,
    index: usize,
) -> Value {
    let target = vm
        .factory
        .builtin_function("", promise_any_reject_element_function);
    target.set_property("__already_called", Value::bool(false));
    target.set_property("__errors", errors);
    target.set_property("__remaining", remaining);
    target.set_property("__reject", reject);
    target.set_property("__index", Value::Number(index as f64));
    bound_builtin(vm, "", promise_any_reject_element_function, target, 1.0)
}

fn promise_any_reject_element_function(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if this.get_property("__already_called").to_boolean() {
        return Ok(Value::undefined());
    }
    this.set_property("__already_called", Value::bool(true));
    let errors = this.get_property("__errors");
    let index = this.get_property("__index").into_number() as usize;
    let reason = args.get(0).copied().unwrap_or(Value::undefined());
    vm.set_property_by_value(errors, Value::Number(index as f64), reason)?;
    if decrement_record(this.get_property("__remaining")) == 0 {
        let error = aggregate_error(vm, errors);
        let reject = this.get_property("__reject");
        vm.call_function(reject, &[error], Value::undefined())?;
    }
    Ok(Value::undefined())
}

fn aggregate_error(vm: &mut VM, errors: Value) -> Value {
    let error = vm
        .factory
        .native_error("AggregateError", "No Promise.any element fulfilled");
    error.set_property("errors", errors);
    error
}

fn resolving_function(vm: &mut VM, promise: Value, fulfill: bool) -> Value {
    let target = vm.factory.builtin_function(
        if fulfill { "resolve" } else { "reject" },
        if fulfill {
            promise_resolve_function
        } else {
            promise_reject_function
        },
    );
    target.set_property("__promise", promise);
    target.set_property("__already_resolved", Value::bool(false));

    let wrapper = vm.factory.builtin_function(
        if fulfill { "resolve" } else { "reject" },
        if fulfill {
            promise_resolve_function
        } else {
            promise_reject_function
        },
    );
    wrapper.set_property("__bound_target", target);
    wrapper.set_property("__bound_this", target);
    wrapper.set_property("__bound_arg_count", Value::Number(0.0));
    wrapper.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
    );
    wrapper
}

fn promise_resolve_function(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if this.get_property("__already_resolved").to_boolean() {
        return Ok(Value::undefined());
    }
    this.set_property("__already_resolved", Value::bool(true));
    let promise = this.get_property("__promise");
    let value = args.get(0).copied().unwrap_or(Value::undefined());
    resolve_promise(vm, promise, value)?;
    Ok(Value::undefined())
}

fn promise_reject_function(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if this.get_property("__already_resolved").to_boolean() {
        return Ok(Value::undefined());
    }
    this.set_property("__already_resolved", Value::bool(true));
    let promise = this.get_property("__promise");
    let reason = args.get(0).copied().unwrap_or(Value::undefined());
    reject_promise(vm, promise, reason)?;
    Ok(Value::undefined())
}

fn init_promise(promise: Value) {
    promise.set_property("__promise_state", Value::Number(STATE_PENDING as f64));
    promise.set_property("__promise_result", Value::undefined());
    promise.set_property("__promise_reaction_count", Value::Number(0.0));
}

fn new_promise(vm: &mut VM, constructor: Value) -> Value {
    let promise = vm.factory.object(FxHashMap::default());
    let prototype = constructor.get_property("prototype");
    if prototype.is_object() {
        promise.get_object_info().prototype = prototype;
    }
    init_promise(promise);
    promise
}

fn is_promise(value: Value) -> bool {
    value.is_object() && !value.get_property("__promise_state").is_undefined()
}

fn promise_state(promise: Value) -> i32 {
    match promise.get_property("__promise_state") {
        Value::Number(state) => state as i32,
        _ => STATE_PENDING,
    }
}

fn resolve_promise(vm: &mut VM, promise: Value, value: Value) -> Result<(), RuntimeError> {
    if promise == value {
        let error = vm
            .factory
            .native_error("TypeError", "Cannot resolve promise with itself");
        return reject_promise(vm, promise, error);
    }
    if is_promise(value) {
        match promise_state(value) {
            STATE_FULFILLED => {
                return fulfill_promise(vm, promise, value.get_property("__promise_result"))
            }
            STATE_REJECTED => {
                return reject_promise(vm, promise, value.get_property("__promise_result"))
            }
            _ => {
                add_reaction(vm, value, promise, Value::undefined(), Value::undefined());
                return Ok(());
            }
        }
    }
    if value.is_object() {
        let then_key = vm.factory.string("then");
        let then = match vm.get_property_by_value(value, then_key) {
            Ok(then) => then,
            Err(error) => {
                let reason = error.to_value(&mut vm.factory);
                return reject_promise(vm, promise, reason);
            }
        };
        if vm.is_callable(then) {
            vm.enqueue_promise_thenable(promise, value, then);
            return Ok(());
        }
    }
    fulfill_promise(vm, promise, value)
}

fn fulfill_promise(vm: &mut VM, promise: Value, value: Value) -> Result<(), RuntimeError> {
    settle_promise(vm, promise, STATE_FULFILLED, value)
}

fn reject_promise(vm: &mut VM, promise: Value, reason: Value) -> Result<(), RuntimeError> {
    settle_promise(vm, promise, STATE_REJECTED, reason)
}

fn settle_promise(
    vm: &mut VM,
    promise: Value,
    state: i32,
    result: Value,
) -> Result<(), RuntimeError> {
    if promise_state(promise) != STATE_PENDING {
        return Ok(());
    }
    promise.set_property("__promise_state", Value::Number(state as f64));
    promise.set_property("__promise_result", result);
    let count = promise
        .get_property("__promise_reaction_count")
        .to_number(&mut vm.factory.memory_allocator) as usize;
    for index in 0..count {
        let child = promise.get_property(&format!("__promise_reaction_child_{}", index));
        let on_fulfilled = promise.get_property(&format!("__promise_reaction_fulfill_{}", index));
        let on_rejected = promise.get_property(&format!("__promise_reaction_reject_{}", index));
        vm.enqueue_promise_reaction(state, result, child, on_fulfilled, on_rejected);
    }
    Ok(())
}

fn add_or_run_reaction(
    vm: &mut VM,
    promise: Value,
    child: Value,
    on_fulfilled: Value,
    on_rejected: Value,
) -> Result<(), RuntimeError> {
    let state = promise_state(promise);
    if state == STATE_PENDING {
        add_reaction(vm, promise, child, on_fulfilled, on_rejected);
        return Ok(());
    }
    let result = promise.get_property("__promise_result");
    vm.enqueue_promise_reaction(state, result, child, on_fulfilled, on_rejected);
    Ok(())
}

fn add_reaction(
    vm: &mut VM,
    promise: Value,
    child: Value,
    on_fulfilled: Value,
    on_rejected: Value,
) {
    let count = promise
        .get_property("__promise_reaction_count")
        .to_number(&mut vm.factory.memory_allocator) as usize;
    promise.set_property(&format!("__promise_reaction_child_{}", count), child);
    promise.set_property(
        &format!("__promise_reaction_fulfill_{}", count),
        on_fulfilled,
    );
    promise.set_property(&format!("__promise_reaction_reject_{}", count), on_rejected);
    promise.set_property(
        "__promise_reaction_count",
        Value::Number((count + 1) as f64),
    );
}

pub(crate) fn run_reaction(
    vm: &mut VM,
    state: i32,
    result: Value,
    child: Value,
    on_fulfilled: Value,
    on_rejected: Value,
) -> Result<(), RuntimeError> {
    let handler = if state == STATE_FULFILLED {
        on_fulfilled
    } else {
        on_rejected
    };
    if !handler.is_function_object() {
        if state == STATE_FULFILLED {
            return resolve_promise(vm, child, result);
        }
        return reject_promise(vm, child, result);
    }
    match vm.call_function(handler, &[result], Value::undefined()) {
        Ok(value) => resolve_promise(vm, child, value),
        Err(error) => {
            let reason = error.to_value(&mut vm.factory);
            reject_promise(vm, child, reason)
        }
    }
}

pub(crate) fn run_thenable_job(
    vm: &mut VM,
    promise: Value,
    thenable: Value,
    then_action: Value,
) -> Result<(), RuntimeError> {
    let resolve = resolving_function(vm, promise, true);
    let reject = resolving_function(vm, promise, false);
    if let Err(error) = vm.call_function(then_action, &[resolve, reject], thenable) {
        let reason = error.to_value(&mut vm.factory);
        vm.call_function(reject, &[reason], Value::undefined())?;
    }
    Ok(())
}

fn collect_iterable(vm: &mut VM, value: Value) -> Result<Vec<Value>, RuntimeError> {
    if value.is_null() || value.is_undefined() {
        return Err(vm.current_context.error_type("Promise iterable"));
    }
    let iterator_key = vm.factory.well_known_symbol(SYMBOL_ITERATOR_ID);
    let method = vm.get_property_by_value(value, iterator_key)?;
    if !vm.is_callable(method) {
        return Err(vm.current_context.error_type("Promise iterable"));
    }
    let iterator = vm.call_function(method, &[], value)?;
    if !iterator.is_object() {
        return Err(vm.current_context.error_type("Promise iterator"));
    }
    let next_key = vm.factory.string("next");
    let next = vm.get_property_by_value(iterator, next_key)?;
    if !vm.is_callable(next) {
        return Err(vm.current_context.error_type("Promise iterator"));
    }
    let done_key = vm.factory.string("done");
    let value_key = vm.factory.string("value");
    let mut values = vec![];
    loop {
        let result = vm.call_function(next, &[], iterator)?;
        if !result.is_object() {
            return Err(vm.current_context.error_type("Promise iterator"));
        }
        if vm.get_property_by_value(result, done_key)?.to_boolean() {
            break;
        }
        values.push(vm.get_property_by_value(result, value_key)?);
    }
    Ok(values)
}
