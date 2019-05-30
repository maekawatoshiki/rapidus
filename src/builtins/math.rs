use crate::vm::{
    frame,
    jsvalue::{
        object::{DataProperty, ObjectInfo, ObjectKind, Property},
        value::Value,
    },
    vm::{Factory, VMResult, VM},
};
use rand::random;
use rustc_hash::FxHashMap;

pub fn math(factory: &mut Factory) -> Value {
    let math_random = factory.builtin_function("random".to_string(), math_random);

    make_normal_object!(factory,
        random => true, false, true: math_random
    )
}

pub fn math_random(vm: &mut VM, _args: &[Value], _cur_frame: &frame::Frame) -> VMResult {
    vm.stack.push(Value::Number(random::<f64>()).into());
    Ok(())
}
