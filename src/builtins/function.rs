use gc;
use vm::{frame, jsvalue, jsvalue::value::Value2, vm};

pub fn function(
    memory_allocator: &mut gc::MemoryAllocator,
    object_prototypes: &jsvalue::prototype::ObjectPrototypes,
) -> Value2 {
    let func = Value2::builtin_function(
        memory_allocator,
        object_prototypes,
        "Function".to_string(),
        function_constructor,
    );
    func.set_property_by_string_key("prototype".to_string(), object_prototypes.function);
    func.get_property_by_str_key("prototype")
        .set_constructor(func);
    func
}

pub fn function_constructor(
    vm: &mut vm::VM2,
    _args: &[Value2],
    _cur_frame: &frame::Frame,
) -> vm::VMResult {
    vm.stack.push(Value2::undefined().into());
    Ok(())
}

pub fn function_prototype_call(
    vm: &mut vm::VM2,
    args: &[Value2],
    cur_frame: &frame::Frame,
) -> vm::VMResult {
    let this_arg = *args.get(0).unwrap_or(&Value2::undefined());
    let func = cur_frame.this;
    vm.call_function(func, args.get(1..).unwrap_or(&[]), this_arg, cur_frame)
}
