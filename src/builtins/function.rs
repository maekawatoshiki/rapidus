use crate::vm::{
    jsvalue::{function::UserFunctionInfo, value::Value},
    vm::{Factory, VMValueResult, VM},
};

pub fn function(factory: &mut Factory) -> Value {
    factory.generate_builtin_constructor(
        "Function",
        function_constructor,
        factory.object_prototypes.function,
    )
}

// TODO: https://www.ecma-international.org/ecma-262/9.0/index.html#sec-function-constructor
pub fn function_constructor(vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    let info = UserFunctionInfo::new(vm.current_context.module_func_id);
    let func = vm.factory.function(None, info);
    Ok(func)
}

pub fn function_prototype_call(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let this_arg = *args.get(0).unwrap_or(&Value::undefined());
    let func = this;
    vm.call_function(func, args.get(1..).unwrap_or(&[]), this_arg)
}
