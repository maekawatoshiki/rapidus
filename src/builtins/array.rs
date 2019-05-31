use crate::vm::{
    error::RuntimeError,
    frame::Frame,
    jsvalue::{object::Property, value::Value},
    vm::{Factory, VMResult, VM},
};

pub fn array(factory: &mut Factory) -> Value {
    let ary = factory.builtin_function("Array", array_constructor);
    ary.set_property_by_string_key("prototype", factory.object_prototypes.array);
    ary.get_property_by_str_key("prototype")
        .set_constructor(ary);
    ary
}

pub fn array_constructor(vm: &mut VM, _args: &[Value], _cur_frame: &Frame) -> VMResult {
    vm.stack.push(Value::undefined().into());
    Ok(())
}

pub fn array_prototype_push(vm: &mut VM, args: &[Value], cur_frame: &Frame) -> VMResult {
    if !cur_frame.this.is_array_object() {
        return Err(RuntimeError::Unknown);
    }

    let ary_info = cur_frame.this.as_array_mut();

    for arg in args {
        ary_info.elems.push(Property::new_data_simple(*arg));
    }

    vm.stack
        .push(Value::Number(ary_info.get_length() as f64).into());

    Ok(())
}

pub fn array_prototype_map(vm: &mut VM, args: &[Value], cur_frame: &Frame) -> VMResult {
    if !cur_frame.this.is_array_object() {
        return Err(RuntimeError::Unknown);
    }

    let ary_info = cur_frame.this.as_array_mut();

    let callback = args[0];
    let mut args_for_callback = [
        /* value = */ Value::undefined(),
        /* nth element = */ Value::Number(0.0),
        /* array itself = */ cur_frame.this,
    ];

    let mut new_ary = vec![];

    for i in 0..ary_info.get_length() {
        args_for_callback[0] =
            vm.get_property(cur_frame.this, Value::Number(i as f64), cur_frame)?; // 'i'th element may be getter
        args_for_callback[1] = Value::Number(i as f64);

        vm.call_function(callback, &args_for_callback, Value::undefined(), cur_frame)?;

        new_ary.push(Property::new_data_simple(
            vm.stack.pop().unwrap().into(): Value,
        ));
    }

    vm.stack.push(vm.factory.array(new_ary).into());

    Ok(())
}
