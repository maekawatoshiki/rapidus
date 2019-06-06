use crate::vm::{
    frame,
    jsvalue::value::Value,
    vm::{Factory, VMResult, VM},
};

pub fn function(factory: &mut Factory) -> Value {
    let func = factory.builtin_function("Function", function_constructor);
    func.set_property_by_string_key("prototype", factory.object_prototypes.function);
    func.get_property_by_str_key("prototype")
        .set_constructor(func);
    func
}

pub fn function_constructor(vm: &mut VM, _args: &[Value], _cur_frame: &frame::Frame) -> VMResult {
    vm.stack.push(Value::undefined().into());
    Ok(())
}

pub fn function_prototype_call(vm: &mut VM, args: &[Value], cur_frame: &frame::Frame) -> VMResult {
    let this_arg = *args.get(0).unwrap_or(&Value::undefined());
    let func = cur_frame.this;
    vm.call_function(func, args.get(1..).unwrap_or(&[]), this_arg, cur_frame)
}
