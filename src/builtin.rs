use vm::{error::RuntimeError, frame::Frame, jsvalue::value::*, vm::VM2};

pub type BuiltinFuncTy2 = fn(&mut VM2, &[Value], &Frame) -> Result<(), RuntimeError>;

pub fn parse_float(vm: &mut VM2, args: &[Value], _cur_frame: &Frame) -> Result<(), RuntimeError> {
    let string = args.get(0).unwrap_or(&Value::undefined()).to_string();
    vm.stack
        .push(Value::Number(string.parse::<f64>().unwrap_or(::std::f64::NAN)).into());
    Ok(())
}
