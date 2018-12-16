use gc;
use vm::{
    callobj::CallObject,
    error::RuntimeError,
    value::{Value, ValueBase},
    vm::VM,
};

use rustc_hash::FxHashMap;

thread_local!(
    pub static OBJECT_PROTOTYPE: Value = {
        Value::new(ValueBase::Object(gc::new( {
            make_hashmap!(
                // __proto__: Value::null()
            )
        })))
    };

    pub static OBJECT_OBJ: Value = {
        let prototype = OBJECT_PROTOTYPE.with(|x| x.clone());
        let object = Value::builtin_function_with_obj_and_prototype(
            object_new,
            None,
            CallObject::new(Value::undefined()),
            make_hashmap!(
                create: Value::default_builtin_function(object_create)
            ),
            prototype.clone()
        );

        prototype.set_constructor(object.clone());

        object
    }
);

/// https://www.ecma-international.org/ecma-262/6.0/#sec-object-objects
pub fn object_new(vm: &mut VM, args: &Vec<Value>, _: &CallObject) -> Result<(), RuntimeError> {
    if args.len() == 0 {
        vm.set_return_value(make_object!());
        return Ok(());
    }

    match &args[0].val {
        ValueBase::Null | ValueBase::Undefined => {
            vm.set_return_value(make_object!());
            return Ok(());
        }
        ValueBase::Empty => unreachable!(),
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

    let obj = match maybe_obj.val {
        ValueBase::Object(properties) => {
            let properties = unsafe { &*properties };
            let new_obj = make_object!();
            let proto = new_obj.get_property(Value::string("__proto__".to_string()), None);
            for (name, value) in properties {
                proto.set_property(Value::string(name.to_string()), value.clone(), None)
            }
            new_obj
        }
        ValueBase::Null => make_object!(),
        _ => {
            return Err(RuntimeError::Type(
                "type error: Object.create: 1st argument must be Object or null".to_string(),
            ));
        }
    };

    vm.set_return_value(obj);

    Ok(())
}
