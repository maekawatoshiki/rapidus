use crate::vm::{
    exec_context::ExecContext,
    jsvalue::value::*,
    vm::{Factory, VMValueResult, VM},
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
    _cur_frame: &mut ExecContext,
) -> VMValueResult {
    if args.len() == 0 {
        let empty_obj = vm.factory.object(FxHashMap::default());
        vm.stack.push(empty_obj.into());
        return Ok(empty_obj);
    }

    match &args[0] {
        Value::Other(NULL) | Value::Other(UNDEFINED) => {
            let empty_obj = vm.factory.object(FxHashMap::default());
            Ok(empty_obj)
        }
        Value::Other(EMPTY) => unreachable!(),
        _ => {
            // TODO: Follow the specification
            Ok(args[0])
        }
    }
}
