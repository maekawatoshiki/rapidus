use crate::vm::{
    jsvalue::{object::Property, value::Value},
    vm::{Factory, VMValueResult, VM},
};

pub fn array(factory: &mut Factory) -> Value {
    factory.generate_builtin_constructor(
        "Array",
        array_constructor,
        factory.object_prototypes.array,
    )
}

pub fn array_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let arg_length = args.len();
    let props = {
        match arg_length {
            0 => vec![],
            1 => {
                let len = args[0];
                if len.is_number() {
                    let len = len.to_uint32(&mut vm.factory.memory_allocator) as usize;
                    vec![Property::new_data_simple(Value::empty()); len]
                } else {
                    vec![Property::new_data_simple(args[0])]
                }
            }
            _ => {
                let mut ary = vec![];
                for i in 0..arg_length {
                    ary.push(Property::new_data_simple(args[i]));
                }
                ary
            }
        }
    };
    let val = vm.factory.array(props);
    Ok(val)
}

pub fn array_prototype_join(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !this.is_array_object() {
        return Err(vm.current_context.error_unknown());
    }

    let ary_info = this.as_array_mut();
    let seperator = match args.len() {
        0 => None,
        1 if args[0].is_undefined() => None,
        _ => Some(args[0].to_string()),
    };
    let result = ary_info.join(seperator);
    let val = vm.factory.string(result);
    Ok(val)
}

pub fn array_prototype_push(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !this.is_array_object() {
        return Err(vm.current_context.error_unknown());
    }

    let ary_info = this.as_array_mut();

    for arg in args {
        ary_info.elems.push(Property::new_data_simple(*arg));
    }

    let val = Value::Number(ary_info.get_length() as f64);

    Ok(val)
}

pub fn array_prototype_map(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !this.is_array_object() {
        return Err(vm.current_context.error_unknown());
    }

    let ary_info = this.as_array_mut();

    let callback = args[0];
    let mut args_for_callback = [
        /* value = */ Value::undefined(),
        /* nth element = */ Value::Number(0.0),
        /* array itself = */ this,
    ];

    let mut new_ary = vec![];

    for i in 0..ary_info.get_length() {
        args_for_callback[0] = vm.get_property_by_value(this, Value::Number(i as f64))?; // 'i'th element may be getter
        args_for_callback[1] = Value::Number(i as f64);

        let val = vm.call_function(callback, &args_for_callback, Value::undefined())?;
        if val.is_empty() {
            unreachable!("EMPTY")
        };

        new_ary.push(Property::new_data_simple(val));
    }
    let val = vm.factory.array(new_ary);
    Ok(val)
}
