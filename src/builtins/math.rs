use rand::random;
use rustc_hash::FxHashMap;
use vm::{
    frame,
    jsvalue::{
        object::{ObjectInfo, ObjectKind2},
        value::Value,
    },
    vm,
};

pub fn math(vm: &mut vm::VM2) -> Value {
    let math_random = vm
        .factory
        .builtin_function("random".to_string(), math_random);

    make_normal_object!(vm.factory,
        random => true, false, true: math_random
    )
}

pub fn math_random(vm: &mut vm::VM2, _args: &[Value], _cur_frame: &frame::Frame) -> vm::VMResult {
    vm.stack.push(Value::Number(random::<f64>()).into());
    Ok(())
}
