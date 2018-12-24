use vm::{callobj::CallObject, error::RuntimeError, value::Value, vm::VM};

thread_local!(
    pub static OBJECT_PROTOTYPE: Value = {
        Value::Object(Value::propmap_from_nvp(&make_nvp!(
            __proto__:  Value::Null
        )))
    };

    pub static OBJECT_OBJ: Value = {
        let prototype = OBJECT_PROTOTYPE.with(|x| x.clone());
        let object = Value::builtin_function(
            object_new,
            None,
            &mut vec![("create", Value::default_builtin_function(object_create))],
            None,//Some(prototype.clone())
        );

        prototype.set_constructor(object.clone());

        object
    }
);

/// https://www.ecma-international.org/ecma-262/6.0/#sec-object-objects
pub fn object_new(vm: &mut VM, args: &Vec<Value>, _: &CallObject) -> Result<(), RuntimeError> {
    if args.len() == 0 {
        vm.set_return_value(Value::object_from_nvp(&vec![]));
        return Ok(());
    }

    match &args[0] {
        Value::Null | Value::Undefined => {
            vm.set_return_value(Value::object_from_nvp(&vec![]));
            return Ok(());
        }
        Value::Empty => unreachable!(),
        _ => {
            // TODO: Follow the specification
            vm.set_return_value(args[0].clone());
            return Ok(());
        }
    }
}

pub fn object_create(vm: &mut VM, args: &Vec<Value>, _: &CallObject) -> Result<(), RuntimeError> {
    let maybe_obj = match args.len() {
        0 => {
            return Err(RuntimeError::General(
                "Object.create needs one argument at least".to_string(),
            ));
        }
        1 => &args[0],
        // TODO: Implement the case when args.len() == 2
        _ => return Err(RuntimeError::Unimplemented),
    };

    let obj = match maybe_obj {
        Value::Object(map) => {
            //let properties = unsafe { &*properties };
            let new_obj = Value::object_from_nvp(&vec![]);
            let proto = new_obj.get_property(Value::string("__proto__".to_string()), None);
            for (name, prop) in unsafe { (**map).iter() } {
                proto.set_property(Value::string(name.to_string()), prop.clone().val, None)
            }
            new_obj
        }
        Value::Null => Value::object_from_nvp(&vec![]),
        _ => {
            return Err(RuntimeError::Type(
                "type error: Object.create: 1st argument must be Object or null".to_string(),
            ));
        }
    };

    vm.set_return_value(obj);

    Ok(())
}
