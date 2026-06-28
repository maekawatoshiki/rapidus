use crate::vm::{
    error::RuntimeError,
    jsvalue::{
        object::{DataProperty, Property},
        value::Value,
    },
    vm::{Factory, VMValueResult, VM},
};

pub fn function(factory: &mut Factory) -> Value {
    let constructor = factory.generate_builtin_constructor(
        "Function",
        function_constructor,
        factory.object_prototypes.function,
    );
    constructor.get_object_info().property.insert(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
    );
    constructor
}

// TODO: https://www.ecma-international.org/ecma-262/9.0/index.html#sec-function-constructor
pub fn function_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let (params, body) = match args.split_last() {
        Some((body, params)) => (
            params
                .iter()
                .map(|arg| arg.to_string())
                .collect::<Vec<_>>()
                .join(","),
            body.to_string(),
        ),
        None => (String::new(), String::new()),
    };
    let source = format!("(function anonymous({}\n) {{\n{}\n}})", params, body);
    let mut parser = rapidus_parser::Parser::new("Function", source);
    let node = parser.parse_all().map_err(|_| syntax_error(vm))?;
    let func_info = vm.compile(&node, true).map_err(|_| syntax_error(vm))?;

    let saved_context = vm.current_context.clone();
    let saved_context_stack = vm.saved_context.clone();
    let saved_called_from_native = vm.is_called_from_native;

    vm.current_context = vm.create_global_context(func_info);
    vm.saved_context.clear();
    vm.is_called_from_native = true;
    let result = vm.run();

    vm.current_context = saved_context;
    vm.saved_context = saved_context_stack;
    vm.is_called_from_native = saved_called_from_native;

    result
}

fn syntax_error(vm: &mut VM) -> RuntimeError {
    let error = vm.factory.native_error("SyntaxError", "Function");
    vm.current_context.error_exception(error)
}

pub fn function_prototype_call(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !vm.is_callable(this) {
        return Err(vm.current_context.error_type("Function.prototype.call"));
    }
    let this_arg = *args.get(0).unwrap_or(&Value::undefined());
    let func = this;
    vm.call_function(func, args.get(1..).unwrap_or(&[]), this_arg)
}

pub fn function_prototype_apply(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !vm.is_callable(this) {
        return Err(vm.current_context.error_type("Function.prototype.apply"));
    }

    let this_arg = *args.get(0).unwrap_or(&Value::undefined());
    let arg_array = *args.get(1).unwrap_or(&Value::undefined());
    if arg_array.is_null() || arg_array.is_undefined() {
        return vm.call_function(this, &[], this_arg);
    }
    if !arg_array.is_object() {
        return Err(vm.current_context.error_type("Function.prototype.apply"));
    }

    let length_key = vm.factory.string("length".to_string());
    let len = vm
        .get_property_by_value(arg_array, length_key)?
        .to_number(&mut vm.factory.memory_allocator)
        .max(0.0)
        .min((1usize << 53) as f64) as usize;
    let mut call_args = vec![];
    for index in 0..len {
        call_args.push(vm.get_property_by_value(arg_array, Value::Number(index as f64))?);
    }

    vm.call_function(this, &call_args, this_arg)
}

pub fn function_prototype_bind(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !vm.is_callable(this) {
        return Err(vm.current_context.error_type("Function.prototype.bind"));
    }

    let bound_this = *args.get(0).unwrap_or(&Value::undefined());
    let bound_args = args.get(1..).unwrap_or(&[]);
    let bound = vm.factory.builtin_function("bound", bound_function_dummy);
    bound.set_property("__bound_target", this);
    bound.set_property("__bound_this", bound_this);
    bound.set_property("__bound_arg_count", Value::Number(bound_args.len() as f64));
    for (index, arg) in bound_args.iter().enumerate() {
        bound.set_property(format!("__bound_arg_{}", index).as_str(), *arg);
    }

    let target_len = this
        .get_property("length")
        .to_number(&mut vm.factory.memory_allocator);
    let length = if target_len.is_nan() {
        0.0
    } else {
        (target_len - bound_args.len() as f64).max(0.0)
    };
    bound.get_object_info().property.insert(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(length)).set_configurable()),
    );

    Ok(bound)
}

pub fn function_prototype_to_string(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if !vm.is_callable(this) {
        return Err(vm.current_context.error_type("Function.prototype.toString"));
    }

    Ok(vm.factory.string("function () { [native code] }"))
}

fn bound_function_dummy(_vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    Ok(Value::undefined())
}
