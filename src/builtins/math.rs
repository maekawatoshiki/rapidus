use gc::MemoryAllocator;
use rand::random;
use rustc_hash::FxHashMap;
use vm::{
    frame,
    jsvalue::prototype::ObjectPrototypes,
    jsvalue::{
        object::{DataProperty, ObjectInfo, ObjectKind2, Property2},
        value::Value2,
    },
    vm,
};

pub fn math(
    memory_allocator: &mut MemoryAllocator,
    object_prototypes: &ObjectPrototypes,
) -> Value2 {
    let math_random = Value2::builtin_function(
        memory_allocator,
        object_prototypes,
        "random".to_string(),
        math_random,
    );

    make_normal_object!(memory_allocator, object_prototypes,
        random => true, false, true: math_random
    )
}

pub fn math_random(vm: &mut vm::VM2, _args: &[Value2], _cur_frame: &frame::Frame) -> vm::VMResult {
    vm.stack.push(Value2::Number(random::<f64>()).into());
    Ok(())
}
