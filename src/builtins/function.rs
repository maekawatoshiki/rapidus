use crate::vm::{
    frame,
    jsvalue::value::Value,
    vm::{Factory, VMResult, VM},
};

pub fn function(factory: &mut Factory) -> Value {
    factory.generate_builtin_constructor(
        "Function",
        function_constructor,
        factory.object_prototypes.function,
    )
}

pub fn function_constructor(
    vm: &mut VM,
    _args: &[Value],
    _this: Value,
    _cur_frame: &mut frame::Frame,
) -> VMResult {
    vm.stack.push(Value::undefined().into());
    Ok(())
}

pub fn function_prototype_call(
    vm: &mut VM,
    args: &[Value],
    this: Value,
    cur_frame: &mut frame::Frame,
) -> VMResult {
    let this_arg = *args.get(0).unwrap_or(&Value::undefined());
    let func = this;
    vm.call_function(func, args.get(1..).unwrap_or(&[]), this_arg, cur_frame)
}
