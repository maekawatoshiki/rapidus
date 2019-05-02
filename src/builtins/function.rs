use vm::{frame, jsvalue::value::Value, vm};

pub fn function(vm: &mut vm::VM2) -> Value {
    let func = vm
        .factory
        .builtin_function("Function".to_string(), function_constructor);
    func.set_property_by_string_key(
        "prototype".to_string(),
        vm.factory.object_prototypes.function,
    );
    func.get_property_by_str_key("prototype")
        .set_constructor(func);
    func
}

pub fn function_constructor(
    vm: &mut vm::VM2,
    _args: &[Value],
    _cur_frame: &frame::Frame,
) -> vm::VMResult {
    vm.stack.push(Value::undefined().into());
    Ok(())
}

pub fn function_prototype_call(
    vm: &mut vm::VM2,
    args: &[Value],
    cur_frame: &frame::Frame,
) -> vm::VMResult {
    let this_arg = *args.get(0).unwrap_or(&Value::undefined());
    let func = cur_frame.this;
    vm.call_function(func, args.get(1..).unwrap_or(&[]), this_arg, cur_frame)
}
