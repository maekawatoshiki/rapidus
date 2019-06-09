use crate::vm::{
    frame,
    jsvalue::value::*,
    vm::{Factory, VMResult, VM},
};
use rustc_hash::FxHashMap;

pub fn object(factory: &mut Factory) -> Value {
    factory.generate_builtin_constructor(
        "Object",
        object_constructor,
        factory.object_prototypes.object,
    )
}

pub fn object_constructor(
    vm: &mut VM,
    args: &[Value],
    _this: Value,
    _cur_frame: &mut frame::Frame,
) -> VMResult {
    if args.len() == 0 {
        let empty_obj = vm.factory.object(FxHashMap::default());
        vm.stack.push(empty_obj.into());
        return Ok(());
    }

    match &args[0] {
        Value::Other(NULL) | Value::Other(UNDEFINED) => {
            let empty_obj = vm.factory.object(FxHashMap::default());
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
