use crate::vm::jsvalue::value::Value;
use crate::vm::vm::{Factory, VMValueResult, VM};
use rand::random;

pub fn math(factory: &mut Factory) -> Value {
    let math_random = factory.builtin_function("random", math_random);

    make_normal_object!(factory,
        random => true, false, true: math_random
    )
}

pub fn math_random(_vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    let val = Value::Number(random::<f64>());
    Ok(val)
}
