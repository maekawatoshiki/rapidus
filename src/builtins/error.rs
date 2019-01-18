use gc::GcType;
use vm::{callobj::CallObject, error::RuntimeError, value::Property, value::Value, vm::VM};

thread_local! {
    pub static ERROR_PROTOTYPE: Value = {
        make_object!(
            message:    Value::string("".to_string()),
            name:       Value::string("Error".to_string())
        )
    }
}

pub fn init() -> Value {
    let mut prototype = ERROR_PROTOTYPE.with(|x| x.clone());
    let obj = Value::builtin_function(error_new, None, &mut vec![], Some(prototype.clone()));
    prototype.set_constructor(obj.clone());

    obj
}

fn error_new(vm: &mut VM, args: &Vec<Value>, _: GcType<CallObject>) -> Result<(), RuntimeError> {
    let message = match args.len() {
        0 => "".to_string(),
        _ => args[0].to_string(),
    };
    let prototype = ERROR_PROTOTYPE.with(|x| x.clone());
    let obj = make_object!(
        message:    Value::string(message),
        name:       Value::string("Error".to_string()),
        __proto__:  prototype
    );
    vm.set_return_value(obj);

    Ok(())
}
