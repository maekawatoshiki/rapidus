use crate::builtins::helpers::builtin_function_with_proto_and_length;
use crate::vm::{
    error::RuntimeError,
    jsvalue::{
        object::{DataProperty, Property},
        symbol::{SYMBOL_ITERATOR_ID, SYMBOL_TO_STRING_TAG_ID},
        value::Value,
    },
    vm::{Factory, VMValueResult, VM},
};

pub fn iterator(factory: &mut Factory) -> Value {
    let constructor = factory.builtin_function("Iterator", iterator_constructor);
    let from = factory.builtin_function("from", iterator_from);
    constructor.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
    );
    from.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
    );
    constructor.get_object_info().insert_property(
        "from".to_string(),
        Property::new_data(DataProperty::new(from).set_writable().set_configurable()),
    );
    constructor.get_object_info().insert_property(
        "prototype".to_string(),
        Property::new_data(DataProperty::new(factory.object_prototypes.iterator)),
    );
    constructor
}

pub fn iterator_constructor(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if !vm.builtin_constructor_call || vm.builtin_new_target.strict_eq_bool(vm.builtin_callee) {
        return Err(vm.current_context.error_type("Iterator constructor"));
    }
    Ok(this)
}

pub fn iterator_from(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let value = args.get(0).copied().unwrap_or(Value::undefined());
    if value.is_null() || value.is_undefined() || (value.is_symbol() || value.is_bigint()) {
        return Err(vm.current_context.error_type("Iterator.from"));
    }
    if !value.is_object() && !value.is_string() {
        return Err(vm.current_context.error_type("Iterator.from"));
    }

    let iterator_key = vm.factory.well_known_symbol(SYMBOL_ITERATOR_ID);
    let iterator_method = vm.get_property_by_value(value, iterator_key)?;
    let record = if iterator_method.is_null() || iterator_method.is_undefined() {
        get_iterator_direct(vm, value)?
    } else {
        if !iterator_method.is_function_object() {
            return Err(vm.current_context.error_type("Iterator.from"));
        }
        let iterator = vm.call_function(iterator_method, &[], value)?;
        get_iterator_direct(vm, iterator)?
    };
    Ok(iterator_helper(
        vm,
        "from",
        record,
        Value::undefined(),
        Value::Number(0.0),
    ))
}

pub fn iterator_identity(_vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if !this.is_object() {
        return Err(RuntimeError::typeerr("Iterator.prototype"));
    }
    Ok(this)
}

pub fn iterator_to_array(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let record = get_iterator_direct(vm, this)?;
    let mut elems = vec![];
    let mut index = 0usize;
    while let Some(value) = iterator_step_value(vm, record.iterator, record.next)? {
        elems.push(Property::new_data_simple(value));
        index += 1;
        if index > u32::MAX as usize {
            return Err(vm.current_context.error_range("Iterator.prototype.toArray"));
        }
    }
    Ok(vm.factory.array(elems))
}

pub fn iterator_for_each(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let callback = args.get(0).copied().unwrap_or(Value::undefined());
    if !callback.is_function_object() {
        return Err(vm.current_context.error_type("Iterator.prototype.forEach"));
    }
    let record = get_iterator_direct(vm, this)?;
    let mut index = 0usize;
    while let Some(value) = iterator_step_value(vm, record.iterator, record.next)? {
        if let Err(err) = vm.call_function(
            callback,
            &[value, Value::Number(index as f64)],
            Value::undefined(),
        ) {
            let _ = iterator_close(vm, this);
            return Err(err);
        }
        index += 1;
    }
    Ok(Value::undefined())
}

pub fn iterator_every(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let predicate = args.get(0).copied().unwrap_or(Value::undefined());
    if !predicate.is_function_object() {
        return Err(vm.current_context.error_type("Iterator.prototype.every"));
    }
    let record = get_iterator_direct(vm, this)?;
    let mut index = 0usize;
    while let Some(value) = iterator_step_value(vm, record.iterator, record.next)? {
        let selected = match vm.call_function(
            predicate,
            &[value, Value::Number(index as f64)],
            Value::undefined(),
        ) {
            Ok(value) => value.to_boolean(),
            Err(err) => {
                let _ = iterator_close(vm, this);
                return Err(err);
            }
        };
        if !selected {
            let _ = iterator_close(vm, this);
            return Ok(Value::bool(false));
        }
        index += 1;
    }
    Ok(Value::bool(true))
}

pub fn iterator_some(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let predicate = args.get(0).copied().unwrap_or(Value::undefined());
    if !predicate.is_function_object() {
        return Err(vm.current_context.error_type("Iterator.prototype.some"));
    }
    let record = get_iterator_direct(vm, this)?;
    let mut index = 0usize;
    while let Some(value) = iterator_step_value(vm, record.iterator, record.next)? {
        let selected = match vm.call_function(
            predicate,
            &[value, Value::Number(index as f64)],
            Value::undefined(),
        ) {
            Ok(value) => value.to_boolean(),
            Err(err) => {
                let _ = iterator_close(vm, this);
                return Err(err);
            }
        };
        if selected {
            let _ = iterator_close(vm, this);
            return Ok(Value::bool(true));
        }
        index += 1;
    }
    Ok(Value::bool(false))
}

pub fn iterator_find(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let predicate = args.get(0).copied().unwrap_or(Value::undefined());
    if !predicate.is_function_object() {
        return Err(vm.current_context.error_type("Iterator.prototype.find"));
    }
    let record = get_iterator_direct(vm, this)?;
    let mut index = 0usize;
    while let Some(value) = iterator_step_value(vm, record.iterator, record.next)? {
        let selected = match vm.call_function(
            predicate,
            &[value, Value::Number(index as f64)],
            Value::undefined(),
        ) {
            Ok(value) => value.to_boolean(),
            Err(err) => {
                let _ = iterator_close(vm, this);
                return Err(err);
            }
        };
        if selected {
            let _ = iterator_close(vm, this);
            return Ok(value);
        }
        index += 1;
    }
    Ok(Value::undefined())
}

pub fn iterator_reduce(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let reducer = args.get(0).copied().unwrap_or(Value::undefined());
    if !reducer.is_function_object() {
        return Err(vm.current_context.error_type("Iterator.prototype.reduce"));
    }
    let record = get_iterator_direct(vm, this)?;
    let mut index = 0usize;
    let mut accumulator = if let Some(initial) = args.get(1).copied() {
        initial
    } else {
        match iterator_step_value(vm, record.iterator, record.next)? {
            Some(value) => {
                index = 1;
                value
            }
            None => return Err(vm.current_context.error_type("Iterator.prototype.reduce")),
        }
    };

    while let Some(value) = iterator_step_value(vm, record.iterator, record.next)? {
        accumulator = match vm.call_function(
            reducer,
            &[accumulator, value, Value::Number(index as f64)],
            Value::undefined(),
        ) {
            Ok(value) => value,
            Err(err) => {
                let _ = iterator_close(vm, this);
                return Err(err);
            }
        };
        index += 1;
    }
    Ok(accumulator)
}

pub fn iterator_map(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let mapper = args.get(0).copied().unwrap_or(Value::undefined());
    if !this.is_object() {
        return Err(vm.current_context.error_type("Iterator.prototype.map"));
    }
    if !mapper.is_function_object() {
        return Err(vm.current_context.error_type("Iterator.prototype.map"));
    }
    let record = get_iterator_direct(vm, this)?;
    Ok(iterator_helper(
        vm,
        "map",
        record,
        mapper,
        Value::Number(0.0),
    ))
}

pub fn iterator_filter(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let predicate = args.get(0).copied().unwrap_or(Value::undefined());
    if !this.is_object() {
        return Err(vm.current_context.error_type("Iterator.prototype.filter"));
    }
    if !predicate.is_function_object() {
        return Err(vm.current_context.error_type("Iterator.prototype.filter"));
    }
    let record = get_iterator_direct(vm, this)?;
    Ok(iterator_helper(
        vm,
        "filter",
        record,
        predicate,
        Value::Number(0.0),
    ))
}

pub fn iterator_take(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !this.is_object() {
        return Err(vm.current_context.error_type("Iterator.prototype.take"));
    }
    let limit = iterator_limit(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let record = get_iterator_direct(vm, this)?;
    Ok(iterator_helper(
        vm,
        "take",
        record,
        Value::undefined(),
        Value::Number(limit),
    ))
}

pub fn iterator_drop(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !this.is_object() {
        return Err(vm.current_context.error_type("Iterator.prototype.drop"));
    }
    let limit = iterator_limit(vm, args.get(0).copied().unwrap_or(Value::undefined()))?;
    let record = get_iterator_direct(vm, this)?;
    Ok(iterator_helper(
        vm,
        "drop",
        record,
        Value::undefined(),
        Value::Number(limit),
    ))
}

pub fn iterator_flat_map(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let mapper = args.get(0).copied().unwrap_or(Value::undefined());
    if !this.is_object() {
        return Err(vm.current_context.error_type("Iterator.prototype.flatMap"));
    }
    if !mapper.is_function_object() {
        return Err(vm.current_context.error_type("Iterator.prototype.flatMap"));
    }
    let record = get_iterator_direct(vm, this)?;
    Ok(iterator_helper(
        vm,
        "flatMap",
        record,
        mapper,
        Value::Number(0.0),
    ))
}

pub fn iterator_helper_next(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if !this.is_object() || !this.get_property("__iterator_helper").to_boolean() {
        return Err(vm.current_context.error_type("Iterator helper"));
    }
    if this.get_property("__iterator_helper_done").to_boolean() {
        return Ok(iterator_result(vm, Value::undefined(), true));
    }

    let kind = this.get_property("__iterator_helper_kind").to_string();
    let iterator = this.get_property("__iterator_helper_iterator");
    let next = this.get_property("__iterator_helper_next");
    let mapper = this.get_property("__iterator_helper_mapper");
    let mut counter = this
        .get_property("__iterator_helper_counter")
        .to_number(&mut vm.factory.memory_allocator) as usize;

    match kind.as_str() {
        "map" => match iterator_step_value(vm, iterator, next)? {
            Some(value) => {
                let mapped = call_helper_callback(vm, this, mapper, value, counter)?;
                set_internal(
                    this,
                    "__iterator_helper_counter",
                    Value::Number((counter + 1) as f64),
                );
                Ok(iterator_result(vm, mapped, false))
            }
            None => helper_done(vm, this),
        },
        "from" => match iterator_step_value(vm, iterator, next)? {
            Some(value) => Ok(iterator_result(vm, value, false)),
            None => helper_done(vm, this),
        },
        "filter" => loop {
            match iterator_step_value(vm, iterator, next)? {
                Some(value) => {
                    let selected = call_helper_callback(vm, this, mapper, value, counter)?;
                    counter += 1;
                    set_internal(
                        this,
                        "__iterator_helper_counter",
                        Value::Number(counter as f64),
                    );
                    if selected.to_boolean() {
                        return Ok(iterator_result(vm, value, false));
                    }
                }
                None => return helper_done(vm, this),
            }
        },
        "take" => {
            let mut remaining = this
                .get_property("__iterator_helper_remaining")
                .to_number(&mut vm.factory.memory_allocator);
            if remaining <= 0.0 {
                let _ = iterator_close(vm, iterator);
                return helper_done(vm, this);
            }
            match iterator_step_value(vm, iterator, next)? {
                Some(value) => {
                    if remaining.is_finite() {
                        remaining -= 1.0;
                        set_internal(
                            this,
                            "__iterator_helper_remaining",
                            Value::Number(remaining),
                        );
                    }
                    Ok(iterator_result(vm, value, false))
                }
                None => helper_done(vm, this),
            }
        }
        "drop" => {
            let mut remaining = this
                .get_property("__iterator_helper_remaining")
                .to_number(&mut vm.factory.memory_allocator);
            while remaining > 0.0 {
                match iterator_step_value(vm, iterator, next)? {
                    Some(_) => {
                        if remaining.is_finite() {
                            remaining -= 1.0;
                            set_internal(
                                this,
                                "__iterator_helper_remaining",
                                Value::Number(remaining),
                            );
                        }
                    }
                    None => return helper_done(vm, this),
                }
            }
            match iterator_step_value(vm, iterator, next)? {
                Some(value) => Ok(iterator_result(vm, value, false)),
                None => helper_done(vm, this),
            }
        }
        "flatMap" => loop {
            let inner = this.get_property("__iterator_helper_inner");
            if !inner.is_undefined() {
                let inner_next = this.get_property("__iterator_helper_inner_next");
                match iterator_step_value(vm, inner, inner_next)? {
                    Some(value) => return Ok(iterator_result(vm, value, false)),
                    None => {
                        set_internal(this, "__iterator_helper_inner", Value::undefined());
                        set_internal(this, "__iterator_helper_inner_next", Value::undefined());
                    }
                }
            }

            match iterator_step_value(vm, iterator, next)? {
                Some(value) => {
                    let mapped = call_helper_callback(vm, this, mapper, value, counter)?;
                    counter += 1;
                    set_internal(
                        this,
                        "__iterator_helper_counter",
                        Value::Number(counter as f64),
                    );
                    let inner_record = get_flattened_iterator(vm, mapped)?;
                    set_internal(this, "__iterator_helper_inner", inner_record.iterator);
                    set_internal(this, "__iterator_helper_inner_next", inner_record.next);
                }
                None => return helper_done(vm, this),
            }
        },
        _ => Err(vm.current_context.error_type("Iterator helper")),
    }
}

pub fn iterator_helper_return(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if !this.is_object() || !this.get_property("__iterator_helper").to_boolean() {
        return Err(vm.current_context.error_type("Iterator helper"));
    }
    if !this.get_property("__iterator_helper_done").to_boolean() {
        let inner = this.get_property("__iterator_helper_inner");
        if !inner.is_undefined() {
            let _ = iterator_close(vm, inner);
        }
        let iterator = this.get_property("__iterator_helper_iterator");
        let _ = iterator_close(vm, iterator);
        set_internal(this, "__iterator_helper_done", Value::bool(true));
    }
    Ok(iterator_result(vm, Value::undefined(), true))
}

#[derive(Clone, Copy)]
struct IteratorRecord {
    iterator: Value,
    next: Value,
}

fn get_iterator_direct(vm: &mut VM, iterator: Value) -> Result<IteratorRecord, RuntimeError> {
    if !iterator.is_object() {
        return Err(vm.current_context.error_type("Iterator receiver"));
    }
    let next_key = vm.factory.string("next".to_string());
    let next = vm.get_property_by_value(iterator, next_key)?;
    if !next.is_function_object() {
        return Err(vm.current_context.error_type("Iterator next"));
    }
    Ok(IteratorRecord { iterator, next })
}

fn iterator_step_value(
    vm: &mut VM,
    iterator: Value,
    next: Value,
) -> Result<Option<Value>, RuntimeError> {
    let result = vm.call_function(next, &[], iterator)?;
    if !result.is_object() {
        return Err(vm.current_context.error_type("Iterator result"));
    }
    let done_key = vm.factory.string("done".to_string());
    if vm.get_property_by_value(result, done_key)?.to_boolean() {
        return Ok(None);
    }
    let value_key = vm.factory.string("value".to_string());
    Ok(Some(vm.get_property_by_value(result, value_key)?))
}

fn iterator_close(vm: &mut VM, iterator: Value) -> Result<(), RuntimeError> {
    let return_key = vm.factory.string("return".to_string());
    let return_method = vm.get_property_by_value(iterator, return_key)?;
    if return_method.is_undefined() || return_method.is_null() {
        return Ok(());
    }
    if !return_method.is_function_object() {
        return Err(vm.current_context.error_type("Iterator return"));
    }
    let result = vm.call_function(return_method, &[], iterator)?;
    if !result.is_object() {
        return Err(vm.current_context.error_type("Iterator return result"));
    }
    Ok(())
}

fn iterator_helper(
    vm: &mut VM,
    kind: &str,
    record: IteratorRecord,
    mapper: Value,
    remaining: Value,
) -> Value {
    let function_prototype = vm.factory.object_prototypes.function;
    let next = builtin_function_with_proto_and_length(
        &mut vm.factory,
        function_prototype,
        "next",
        iterator_helper_next,
        0.0,
    );
    let return_fn = builtin_function_with_proto_and_length(
        &mut vm.factory,
        function_prototype,
        "return",
        iterator_helper_return,
        0.0,
    );
    let helper = Value::Object(vm.factory.alloc(crate::vm::jsvalue::object::Object {
        kind: crate::vm::jsvalue::object::ObjectKind::Ordinary,
        prototype: vm.factory.object_prototypes.iterator,
        property: make_property_map!(
            next   => true, false, true: next,
            return => true, false, true: return_fn
        ),
        property_order: make_property_order!(
            next   => true, false, true: next,
            return => true, false, true: return_fn
        ),
        private_elements: rustc_hash::FxHashMap::default(),
        sym_property: rustc_hash::FxHashMap::default(),
        sym_property_order: Vec::new(),
        extensible: true,
    }));
    let kind_value = vm.factory.string(kind.to_string());
    set_internal(helper, "__iterator_helper", Value::bool(true));
    set_internal(helper, "__iterator_helper_kind", kind_value);
    set_internal(helper, "__iterator_helper_iterator", record.iterator);
    set_internal(helper, "__iterator_helper_next", record.next);
    set_internal(helper, "__iterator_helper_mapper", mapper);
    set_internal(helper, "__iterator_helper_counter", Value::Number(0.0));
    set_internal(helper, "__iterator_helper_remaining", remaining);
    set_internal(helper, "__iterator_helper_done", Value::bool(false));
    set_internal(helper, "__iterator_helper_inner", Value::undefined());
    set_internal(helper, "__iterator_helper_inner_next", Value::undefined());
    helper
}

fn call_helper_callback(
    vm: &mut VM,
    helper: Value,
    callback: Value,
    value: Value,
    index: usize,
) -> VMValueResult {
    match vm.call_function(
        callback,
        &[value, Value::Number(index as f64)],
        Value::undefined(),
    ) {
        Ok(value) => Ok(value),
        Err(err) => {
            let inner = helper.get_property("__iterator_helper_inner");
            if !inner.is_undefined() {
                let _ = iterator_close(vm, inner);
            }
            let iterator = helper.get_property("__iterator_helper_iterator");
            let _ = iterator_close(vm, iterator);
            set_internal(helper, "__iterator_helper_done", Value::bool(true));
            Err(err)
        }
    }
}

fn get_flattened_iterator(vm: &mut VM, value: Value) -> Result<IteratorRecord, RuntimeError> {
    if !value.is_object() {
        return Err(vm.current_context.error_type("Iterator.prototype.flatMap"));
    }
    let next_key = vm.factory.string("next".to_string());
    let next = vm.get_property_by_value(value, next_key)?;
    if next.is_function_object() {
        return Ok(IteratorRecord {
            iterator: value,
            next,
        });
    }
    let iterator_key = vm.factory.well_known_symbol(SYMBOL_ITERATOR_ID);
    let iterator_method = vm.get_property_by_value(value, iterator_key)?;
    if iterator_method.is_function_object() {
        let iterator = vm.call_function(iterator_method, &[], value)?;
        return get_iterator_direct(vm, iterator);
    }
    Err(vm.current_context.error_type("Iterator.prototype.flatMap"))
}

fn helper_done(vm: &mut VM, helper: Value) -> VMValueResult {
    set_internal(helper, "__iterator_helper_done", Value::bool(true));
    Ok(iterator_result(vm, Value::undefined(), true))
}

fn iterator_result(vm: &mut VM, value: Value, done: bool) -> Value {
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

fn set_internal(object: Value, key: &str, value: Value) {
    object
        .get_object_info()
        .insert_property(key.to_string(), Property::new_data_simple(value));
}

fn iterator_limit(vm: &mut VM, value: Value) -> Result<f64, RuntimeError> {
    let number = super::helpers::to_number(vm, value)?;
    if number.is_nan() || number < 0.0 {
        return Err(vm.current_context.error_range("Iterator limit"));
    }
    if number.is_infinite() {
        return Ok(f64::INFINITY);
    }
    Ok(number.trunc())
}

pub fn iterator_prototype(
    factory: &mut Factory,
    function_prototype: Value,
    object_prototype: Value,
) -> Value {
    let iterator = builtin_function_with_proto_and_length(
        factory,
        function_prototype,
        "[Symbol.iterator]",
        iterator_identity,
        0.0,
    );
    let to_array = builtin_function_with_proto_and_length(
        factory,
        function_prototype,
        "toArray",
        iterator_to_array,
        0.0,
    );
    let for_each = builtin_function_with_proto_and_length(
        factory,
        function_prototype,
        "forEach",
        iterator_for_each,
        1.0,
    );
    let every =
        builtin_function_with_proto_and_length(factory, function_prototype, "every", iterator_every, 1.0);
    let some =
        builtin_function_with_proto_and_length(factory, function_prototype, "some", iterator_some, 1.0);
    let find =
        builtin_function_with_proto_and_length(factory, function_prototype, "find", iterator_find, 1.0);
    let reduce =
        builtin_function_with_proto_and_length(factory, function_prototype, "reduce", iterator_reduce, 1.0);
    let map = builtin_function_with_proto_and_length(factory, function_prototype, "map", iterator_map, 1.0);
    let filter =
        builtin_function_with_proto_and_length(factory, function_prototype, "filter", iterator_filter, 1.0);
    let take =
        builtin_function_with_proto_and_length(factory, function_prototype, "take", iterator_take, 1.0);
    let drop =
        builtin_function_with_proto_and_length(factory, function_prototype, "drop", iterator_drop, 1.0);
    let flat_map = builtin_function_with_proto_and_length(
        factory,
        function_prototype,
        "flatMap",
        iterator_flat_map,
        1.0,
    );
    let tag = factory.string("Iterator");

    Value::Object(factory.alloc(crate::vm::jsvalue::object::Object {
        kind: crate::vm::jsvalue::object::ObjectKind::Ordinary,
        prototype: object_prototype,
        property: make_property_map!(
            toArray => true, false, true: to_array,
            forEach => true, false, true: for_each,
            every => true, false, true: every,
            some => true, false, true: some,
            find => true, false, true: find,
            reduce => true, false, true: reduce,
            map => true, false, true: map,
            filter => true, false, true: filter,
            take => true, false, true: take,
            drop => true, false, true: drop,
            flatMap => true, false, true: flat_map
        ),
        property_order: Vec::new(),
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
