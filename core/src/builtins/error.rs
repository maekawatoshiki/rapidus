use crate::vm::{
    error::RuntimeError,
    jsvalue::{
        error::ErrorObjectInfo,
        object::{DataProperty, Object, ObjectKind, Property},
        symbol::{SYMBOL_ITERATOR_ID, SYMBOL_TO_PRIMITIVE_ID},
        value::Value,
    },
    vm::{Factory, VMValueResult, VM},
};
use rustc_hash::FxHashMap;

pub fn error(factory: &mut Factory) -> Value {
    native_error_constructor(
        factory,
        "Error",
        error_constructor,
        factory.object_prototypes.error,
    )
}

pub fn aggregate_error(factory: &mut Factory) -> Value {
    let constructor = native_error_constructor(
        factory,
        "AggregateError",
        aggregate_error_constructor,
        factory.object_prototypes.aggregate_error,
    );
    constructor.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(2.0)).set_configurable()),
    );
    constructor
}

pub fn eval_error(factory: &mut Factory) -> Value {
    native_error_constructor(
        factory,
        "EvalError",
        eval_error_constructor,
        factory.object_prototypes.eval_error,
    )
}

pub fn range_error(factory: &mut Factory) -> Value {
    native_error_constructor(
        factory,
        "RangeError",
        range_error_constructor,
        factory.object_prototypes.range_error,
    )
}

pub fn reference_error(factory: &mut Factory) -> Value {
    native_error_constructor(
        factory,
        "ReferenceError",
        reference_error_constructor,
        factory.object_prototypes.reference_error,
    )
}

pub fn syntax_error(factory: &mut Factory) -> Value {
    native_error_constructor(
        factory,
        "SyntaxError",
        syntax_error_constructor,
        factory.object_prototypes.syntax_error,
    )
}

pub fn type_error(factory: &mut Factory) -> Value {
    native_error_constructor(
        factory,
        "TypeError",
        type_error_constructor,
        factory.object_prototypes.type_error,
    )
}

pub fn uri_error(factory: &mut Factory) -> Value {
    native_error_constructor(
        factory,
        "URIError",
        uri_error_constructor,
        factory.object_prototypes.uri_error,
    )
}

pub fn error_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    construct_native_error(vm, args, "Error")
}

pub fn aggregate_error_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let errors = args.get(0).copied().unwrap_or(Value::undefined());
    let message = args.get(1).copied().unwrap_or(Value::undefined());
    let options = args.get(2).copied().unwrap_or(Value::undefined());

    let message = if message.is_undefined() {
        None
    } else {
        Some(to_string(vm, message)?)
    };

    let error = Value::Object(vm.factory.alloc(Object {
        kind: ObjectKind::Error(ErrorObjectInfo::new()),
        prototype: vm.factory.object_prototypes.aggregate_error,
        property: FxHashMap::default(),
        property_order: Vec::new(),
        private_elements: rustc_hash::FxHashMap::default(),
        sym_property: FxHashMap::default(),
        sym_property_order: Vec::new(),
        extensible: true,
    }));

    if let Some(message) = message {
        let message = vm.factory.string(message);
        error.get_object_info().insert_property(
            "message".to_string(),
            Property::new_data(DataProperty::new(message).set_writable().set_configurable()),
        );
    }

    install_error_cause(vm, error, options)?;
    let errors = iterable_to_array(vm, errors)?;
    error.get_object_info().insert_property(
        "errors".to_string(),
        Property::new_data(DataProperty::new(errors).set_writable().set_configurable()),
    );
    Ok(error)
}

pub fn eval_error_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    construct_native_error(vm, args, "EvalError")
}

pub fn range_error_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    construct_native_error(vm, args, "RangeError")
}

pub fn reference_error_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    construct_native_error(vm, args, "ReferenceError")
}

pub fn syntax_error_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    construct_native_error(vm, args, "SyntaxError")
}

pub fn type_error_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    construct_native_error(vm, args, "TypeError")
}

pub fn uri_error_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    construct_native_error(vm, args, "URIError")
}

fn native_error_constructor(
    factory: &mut Factory,
    name: &str,
    func: crate::builtins::BuiltinFuncTy,
    prototype: Value,
) -> Value {
    let constructor = factory.generate_builtin_constructor(name, func, prototype);
    constructor.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
    );
    constructor
}

fn construct_native_error(vm: &mut VM, args: &[Value], name: &str) -> VMValueResult {
    if args.is_empty() {
        return Ok(vm.factory.native_error_without_message(name));
    }
    let message = args[0].to_string();
    Ok(vm.factory.native_error(name, message))
}

fn install_error_cause(vm: &mut VM, error: Value, options: Value) -> Result<(), RuntimeError> {
    if !options.is_object() {
        return Ok(());
    }
    let cause_key = vm.factory.string("cause");
    if !vm.has_property(cause_key, options)?.to_boolean() {
        return Ok(());
    }
    let cause = vm.get_property_by_value(options, cause_key)?;
    error.get_object_info().insert_property(
        "cause".to_string(),
        Property::new_data(DataProperty::new(cause).set_writable().set_configurable()),
    );
    Ok(())
}

fn iterable_to_array(vm: &mut VM, iterable: Value) -> Result<Value, RuntimeError> {
    let iterator_key = vm.factory.well_known_symbol(SYMBOL_ITERATOR_ID);
    let method = vm.get_property_by_value(iterable, iterator_key)?;
    if !method.is_function_object() {
        return Err(vm.current_context.error_type("Object is not iterable"));
    }
    let iterator = vm.call_function(method, &[], iterable)?;
    if !iterator.is_object() {
        return Err(vm.current_context.error_type("Iterator is not an object"));
    }
    let next_key = vm.factory.string("next");
    let next = vm.get_property_by_value(iterator, next_key)?;
    if !next.is_function_object() {
        return Err(vm.current_context.error_type("Iterator next"));
    }

    let mut values = Vec::new();
    loop {
        let result = vm.call_function(next, &[], iterator)?;
        if !result.is_object() {
            return Err(vm.current_context.error_type("Iterator result"));
        }
        let done_key = vm.factory.string("done");
        if vm.get_property_by_value(result, done_key)?.to_boolean() {
            break;
        }
        let value_key = vm.factory.string("value");
        values.push(Property::new_data_simple(
            vm.get_property_by_value(result, value_key)?,
        ));
    }
    Ok(vm.factory.array(values))
}

fn to_string(vm: &mut VM, value: Value) -> Result<String, RuntimeError> {
    if is_primitive(value) {
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
        if !is_primitive(primitive) {
            return Err(vm
                .current_context
                .error_type("Cannot convert object to primitive"));
        }
        return to_string(vm, primitive);
    }

    for name in ["toString", "valueOf"] {
        let key = vm.factory.string(name);
        let method = vm.get_property_by_value(value, key)?;
        if method.is_function_object() {
            let primitive = vm.call_function(method, &[], value)?;
            if is_primitive(primitive) {
                return to_string(vm, primitive);
            }
        }
    }

    Err(vm
        .current_context
        .error_type("Cannot convert object to string"))
}

fn is_primitive(value: Value) -> bool {
    !value.is_object() || value.is_symbol() || value.is_bigint()
}

// pub fn init() -> Value {
//     let mut prototype = ERROR_PROTOTYPE.with(|x| x.clone());
//     let obj = Value::builtin_function(error_new, None, &mut vec![], Some(prototype.clone()));
//     prototype.set_constructor(obj.clone());
//
//     obj
// }

// fn error_new(vm: &mut VM, args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
//     let message = match args.len() {
//         0 => "".to_string(),
//         _ => args[0].to_string(),
//     };
//     let prototype = ERROR_PROTOTYPE.with(|x| x.clone());
//     let obj = make_object!(
//         message:    Value::string(message),
//         name:       Value::string("Error".to_string()),
//         __proto__:  prototype
//     );
//     vm.set_return_value(obj);
//
//     Ok(())
// }
