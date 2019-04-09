use gc;
use rustc_hash::FxHashMap;
use vm::{frame, jsvalue::value::*, vm};

pub fn object(
    memory_allocator: &mut gc::MemoryAllocator,
    object_prototypes: &ObjectPrototypes,
) -> Value {
    let obj = Value::builtin_function(
        memory_allocator,
        object_prototypes,
        "Object".to_string(),
        object_constructor,
    );
    obj.set_property_by_string_key("prototype".to_string(), object_prototypes.object);
    obj.get_property_by_str_key("prototype")
        .set_constructor(obj);
    obj
}

pub fn object_constructor(
    vm: &mut vm::VM2,
    args: &[Value],
    _cur_frame: &frame::Frame,
) -> vm::VMResult {
    if args.len() == 0 {
        let empty_obj = Value::object(
            &mut vm.memory_allocator,
            &vm.object_prototypes,
            FxHashMap::default(),
        );
        vm.stack.push(empty_obj.into());
        return Ok(());
    }

    match &args[0] {
        Value::Other(NULL) | Value::Other(UNDEFINED) => {
            let empty_obj = Value::object(
                &mut vm.memory_allocator,
                &vm.object_prototypes,
                FxHashMap::default(),
            );
            vm.stack.push(empty_obj.into());
        }
        Value::Other(EMPTY) => unreachable!(),
        _ => {
            // TODO: Follow the specification
            vm.stack.push(args[0].into());
        }
    }

    Ok(())
}
