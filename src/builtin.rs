use vm::{error::RuntimeError, frame::Frame, jsvalue::value::*, vm::VM2};

pub type BuiltinFuncTy2 = fn(&mut VM2, &[Value2], &Frame) -> Result<(), RuntimeError>;

pub fn parse_float(vm: &mut VM2, args: &[Value2], _cur_frame: &Frame) -> Result<(), RuntimeError> {
    let string = args.get(0).unwrap_or(&Value2::undefined()).to_string();
    vm.stack
        .push(Value2::Number(string.parse::<f64>().unwrap_or(::std::f64::NAN)).into());
    Ok(())
}
