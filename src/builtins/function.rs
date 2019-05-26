use crate::gc;
use crate::vm::{frame, jsvalue, jsvalue::value::Value, vm};

pub fn function(
    memory_allocator: &mut gc::MemoryAllocator,
    object_prototypes: &jsvalue::prototype::ObjectPrototypes,
) -> Value {
    let func = Value::builtin_function(
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
    vm: &mut vm::VM,
    _args: &[Value],
    _cur_frame: &frame::Frame,
) -> vm::VMResult {
    vm.stack.push(Value::undefined().into());
    Ok(())
}

pub fn function_prototype_call(
    vm: &mut vm::VM,
    args: &[Value],
    cur_frame: &frame::Frame,
) -> vm::VMResult {
    let this_arg = *args.get(0).unwrap_or(&Value::undefined());
    let func = cur_frame.this;
    vm.call_function(func, args.get(1..).unwrap_or(&[]), this_arg, cur_frame)
}
