use crate::vm::{
    exec_context::ExecContext,
    jsvalue::value::Value,
    vm::{Factory, VMValueResult, VM},
};

pub fn error(factory: &mut Factory) -> Value {
    factory.generate_builtin_constructor(
        "Error",
        error_constructor,
        factory.object_prototypes.error,
    )
}

pub fn error_constructor(
    vm: &mut VM,
    args: &[Value],
    _this: Value,
    _cur_frame: &mut ExecContext,
) -> VMValueResult {
    let message = if args.len() == 0 {
        "".to_string()
    } else {
        args[0].to_string()
    };
    let obj = vm.factory.error(message);
    Ok(obj)
}

// pub fn init() -> Value {
//     let mut prototype = ERROR_PROTOTYPE.with(|x| x.clone());
//     let obj = Value::builtin_function(error_new, None, &mut vec![], Some(prototype.clone()));
//     prototype.set_constructor(obj.clone());
//
//     obj
// }

// fn error_new(vm: &mut VM, args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
//     let message = match args.len() {
//         0 => "".to_string(),
//         _ => args[0].to_string(),
//     };
//     let prototype = ERROR_PROTOTYPE.with(|x| x.clone());
//     let obj = make_object!(
//         message:    Value::string(message),
//         name:       Value::string("Error".to_string()),
//         __proto__:  prototype
//     );
//     vm.set_return_value(obj);
//
//     Ok(())
// }
