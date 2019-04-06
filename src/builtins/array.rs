use gc;
use vm::{
    error::RuntimeError,
    frame::Frame,
    jsvalue::{object::Property2, prototype::ObjectPrototypes, value::Value2},
    vm::{VMResult, VM2},
};

pub fn array(
    memory_allocator: &mut gc::MemoryAllocator,
    object_prototypes: &ObjectPrototypes,
) -> Value2 {
    let ary = Value2::builtin_function(
        memory_allocator,
        object_prototypes,
        "Array".to_string(),
        array_constructor,
    );
    ary.set_property_by_string_key("prototype".to_string(), object_prototypes.array);
    ary.get_property_by_str_key("prototype")
        .set_constructor(ary);
    ary
}

pub fn array_constructor(vm: &mut VM2, _args: &[Value2], _cur_frame: &Frame) -> VMResult {
    vm.stack.push(Value2::undefined().into());
    Ok(())
}

pub fn array_prototype_push(vm: &mut VM2, args: &[Value2], cur_frame: &Frame) -> VMResult {
    if !cur_frame.this.is_array_object() {
        return Err(RuntimeError::Unknown);
    }

    let ary_info = cur_frame.this.as_array_mut();

    for arg in args {
        ary_info.elems.push(Property2::new_data_simple(*arg));
    }

    vm.stack
        .push(Value2::Number(ary_info.get_length() as f64).into());

    Ok(())
}

pub fn array_prototype_map(vm: &mut VM2, args: &[Value2], cur_frame: &Frame) -> VMResult {
    if !cur_frame.this.is_array_object() {
        return Err(RuntimeError::Unknown);
    }

    let ary_info = cur_frame.this.as_array_mut();

    let callback = args[0];
    let mut args_for_callback = [
        /* value = */ Value2::undefined(),
        /* nth element = */ Value2::Number(0.0),
        /* array itself = */ cur_frame.this,
    ];

    let mut new_ary = vec![];

    for i in 0..ary_info.get_length() {
        args_for_callback[0] =
            vm.get_property(cur_frame.this, Value2::Number(i as f64), cur_frame)?; // 'i'th element may be getter
        args_for_callback[1] = Value2::Number(i as f64);

        vm.call_function(callback, &args_for_callback, Value2::undefined(), cur_frame)?;

        new_ary.push(Property2::new_data_simple(
            vm.stack.pop().unwrap().into(): Value2,
        ));
    }

    vm.stack
        .push(Value2::array(&mut vm.memory_allocator, &vm.object_prototypes, new_ary).into());

    Ok(())
}
