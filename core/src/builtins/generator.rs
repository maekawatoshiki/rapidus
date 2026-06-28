use crate::builtins::helpers::builtin_function_with_proto_and_length;
use crate::vm::{
    error::RuntimeError,
    jsvalue::{
        object::{DataProperty, GeneratorState, ObjectKind, Property},
        symbol::{SYMBOL_ASYNC_ITERATOR_ID, SYMBOL_ITERATOR_ID, SYMBOL_TO_STRING_TAG_ID},
        value::Value,
    },
    vm::{VMValueResult, VM},
};

pub fn generator_next(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = args.get(0).copied().unwrap_or_else(Value::undefined);
    resume_generator(vm, this, value)
}

pub fn generator_return(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = args.get(0).copied().unwrap_or_else(Value::undefined);
    if let Some(state) = suspended_pending_iterator_close(this) {
        let close_result = vm.close_iterator_state(state);
        complete_generator(vm, this)?;
        close_result?;
        return Ok(generator_result(vm, value, true));
    }
    complete_generator(vm, this)?;
    Ok(generator_result(vm, value, true))
}

pub fn generator_throw(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = args.get(0).copied().unwrap_or_else(Value::undefined);
    complete_generator(vm, this)?;
    Err(vm.current_context.error_exception(value))
}

pub fn generator_identity(_vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    Ok(this)
}

pub fn async_generator_next(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let result = generator_next(vm, args, this);
    promise_wrap_generator_result(vm, result)
}

pub fn async_generator_return(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let result = generator_return(vm, args, this);
    promise_wrap_generator_result(vm, result)
}

pub fn async_generator_throw(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let result = generator_throw(vm, args, this);
    promise_wrap_generator_result(vm, result)
}

fn promise_wrap_generator_result(vm: &mut VM, result: VMValueResult) -> VMValueResult {
    match result {
        Ok(value) => crate::builtins::promise::promise_resolve_direct(vm, value),
        Err(error) => {
            let reason = error.to_value(&mut vm.factory);
            crate::builtins::promise::promise_reject_direct(vm, reason)
        }
    }
}

fn resume_generator(vm: &mut VM, generator: Value, resume_value: Value) -> VMValueResult {
    let (context, suspended_yield) = {
        let mut object = generator.get_object_info();
        let ObjectKind::Generator(ref mut info) = object.kind else {
            return Err(RuntimeError::typeerr("Generator"));
        };
        let suspended_yield = match info.state {
            GeneratorState::Completed => {
                return Ok(generator_result(vm, Value::undefined(), true));
            }
            GeneratorState::Executing => {
                return Err(vm
                    .current_context
                    .error_type("Generator is already running"));
            }
            GeneratorState::SuspendedStart => false,
            GeneratorState::SuspendedYield => true,
        };
        info.state = GeneratorState::Executing;
        (info.context.take(), suspended_yield)
    };

    let Some(context) = context else {
        complete_generator(vm, generator)?;
        return Ok(generator_result(vm, Value::undefined(), true));
    };

    let mut context = context;
    if suspended_yield {
        if let Some(slot) = context.stack.last_mut() {
            *slot = resume_value.into();
        }
    }
    let outer_context = std::mem::replace(&mut vm.current_context, context);
    let outer_saved_context = std::mem::take(&mut vm.saved_context);
    vm.generator_yielded = false;
    let result = vm.run();
    let yielded = vm.generator_yielded;
    let saved_context = vm.current_context.clone();
    vm.current_context = outer_context;
    vm.saved_context = outer_saved_context;

    match result {
        Ok(value) if yielded => {
            let mut object = generator.get_object_info();
            if let ObjectKind::Generator(ref mut info) = object.kind {
                info.context = Some(saved_context);
                info.state = GeneratorState::SuspendedYield;
            }
            Ok(generator_result(vm, value, false))
        }
        Ok(value) => {
            let mut object = generator.get_object_info();
            if let ObjectKind::Generator(ref mut info) = object.kind {
                info.context = None;
                info.state = GeneratorState::Completed;
            }
            Ok(generator_result(vm, value, true))
        }
        Err(err) => {
            let mut object = generator.get_object_info();
            if let ObjectKind::Generator(ref mut info) = object.kind {
                info.context = None;
                info.state = GeneratorState::Completed;
            }
            Err(err)
        }
    }
}

fn complete_generator(_vm: &mut VM, generator: Value) -> Result<(), RuntimeError> {
    if !generator.is_object() {
        return Err(RuntimeError::typeerr("Generator"));
    }
    let mut object = generator.get_object_info();
    let ObjectKind::Generator(ref mut info) = object.kind else {
        return Err(RuntimeError::typeerr("Generator"));
    };
    info.context = None;
    info.state = GeneratorState::Completed;
    Ok(())
}

fn suspended_pending_iterator_close(generator: Value) -> Option<Value> {
    if !generator.is_object() {
        return None;
    }
    let object = generator.get_object_info();
    let ObjectKind::Generator(ref info) = object.kind else {
        return None;
    };
    if info.state != GeneratorState::SuspendedYield {
        return None;
    }
    info.context
        .as_ref()
        .and_then(|context| context.pending_iterator_close_stack.last().copied())
}

fn generator_result(vm: &mut VM, value: Value, done: bool) -> Value {
    vm.factory.object_with_property_order(
        make_property_map!(
            value => true, true, true: value,
            done  => true, true, true: Value::bool(done)
        ),
        make_property_order!(
            value => true, true, true: value,
            done  => true, true, true: Value::bool(done)
        ),
    )
}

pub fn generator_prototype(
    factory: &mut crate::vm::vm::Factory,
    function_prototype: Value,
    iterator_prototype: Value,
) -> Value {
    let next = builtin_function_with_proto_and_length(
        factory,
        function_prototype,
        "next",
        generator_next,
        1.0,
    );
    let return_fn = builtin_function_with_proto_and_length(
        factory,
        function_prototype,
        "return",
        generator_return,
        1.0,
    );
    let throw = builtin_function_with_proto_and_length(
        factory,
        function_prototype,
        "throw",
        generator_throw,
        1.0,
    );
    let iterator = builtin_function_with_proto_and_length(
        factory,
        function_prototype,
        "[Symbol.iterator]",
        generator_identity,
        0.0,
    );
    let tag = factory.string("Generator");

    Value::Object(factory.alloc(crate::vm::jsvalue::object::Object {
        kind: crate::vm::jsvalue::object::ObjectKind::Ordinary,
        prototype: iterator_prototype,
        property: make_property_map!(
            next => true, false, true: next,
            return => true, false, true: return_fn,
            throw => true, false, true: throw
        ),
        property_order: make_property_order!(
            next => true, false, true: next,
            return => true, false, true: return_fn,
            throw => true, false, true: throw
        ),
        private_elements: rustc_hash::FxHashMap::default(),
        sym_property: {
            let mut property = rustc_hash::FxHashMap::default();
            property.insert(
                SYMBOL_ITERATOR_ID,
                Property::new_data(
                    DataProperty::new(iterator)
                        .set_writable()
                        .set_configurable(),
                ),
            );
            property.insert(
                SYMBOL_TO_STRING_TAG_ID,
                Property::new_data(DataProperty::new(tag).set_configurable()),
            );
            property
        },
        sym_property_order: Vec::new(),
        extensible: true,
    }))
}

pub fn async_generator_prototype(
    factory: &mut crate::vm::vm::Factory,
    function_prototype: Value,
    iterator_prototype: Value,
) -> Value {
    let next = builtin_function_with_proto_and_length(
        factory,
        function_prototype,
        "next",
        async_generator_next,
        1.0,
    );
    let return_fn = builtin_function_with_proto_and_length(
        factory,
        function_prototype,
        "return",
        async_generator_return,
        1.0,
    );
    let throw = builtin_function_with_proto_and_length(
        factory,
        function_prototype,
        "throw",
        async_generator_throw,
        1.0,
    );
    let iterator = builtin_function_with_proto_and_length(
        factory,
        function_prototype,
        "[Symbol.asyncIterator]",
        generator_identity,
        0.0,
    );
    let tag = factory.string("AsyncGenerator");

    Value::Object(factory.alloc(crate::vm::jsvalue::object::Object {
        kind: crate::vm::jsvalue::object::ObjectKind::Ordinary,
        prototype: iterator_prototype,
        property: make_property_map!(
            next => true, false, true: next,
            return => true, false, true: return_fn,
            throw => true, false, true: throw
        ),
        property_order: make_property_order!(
            next => true, false, true: next,
            return => true, false, true: return_fn,
            throw => true, false, true: throw
        ),
        private_elements: rustc_hash::FxHashMap::default(),
        sym_property: {
            let mut property = rustc_hash::FxHashMap::default();
            let iterator_prop = Property::new_data(
                DataProperty::new(iterator)
                    .set_writable()
                    .set_configurable(),
            );
            property.insert(SYMBOL_ASYNC_ITERATOR_ID, iterator_prop.clone());
            property.insert(SYMBOL_ITERATOR_ID, iterator_prop);
            property.insert(
                SYMBOL_TO_STRING_TAG_ID,
                Property::new_data(DataProperty::new(tag).set_configurable()),
            );
            property
        },
        sym_property_order: Vec::new(),
        extensible: true,
    }))
}
