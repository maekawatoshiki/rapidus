use crate::gc::MemoryAllocator;
use crate::vm::{
    frame,
    jsvalue::prototype::ObjectPrototypes,
    jsvalue::{
        object::{DataProperty, ObjectInfo, ObjectKind, Property},
        value::Value,
    },
    vm,
};
use rand::random;
use rustc_hash::FxHashMap;

pub fn math(memory_allocator: &mut MemoryAllocator, object_prototypes: &ObjectPrototypes) -> Value {
    let math_random = Value::builtin_function(
        memory_allocator,
        object_prototypes,
        "random".to_string(),
        math_random,
    );

    make_normal_object!(memory_allocator, object_prototypes,
        random => true, false, true: math_random
    )
}

pub fn math_random(vm: &mut vm::VM, _args: &[Value], _cur_frame: &frame::Frame) -> vm::VMResult {
    vm.stack.push(Value::Number(random::<f64>()).into());
    Ok(())
}
