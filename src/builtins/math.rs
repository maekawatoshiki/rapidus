//use libc;
use builtin::BuiltinJITFuncInfo;
use gc::GcType;
use jit::TracingJit;
use llvm::core::*;
use rand::random;
use std::ffi::CString;
use vm::value::*;
use vm::{callobj::CallObject, error::RuntimeError, vm::VM};

pub fn init(jit: TracingJit) -> Value {
    make_object!(
        PI:     Value::Number(::std::f64::consts::PI),
        abs:    Value::default_builtin_function(math_abs),
        acos:   Value::default_builtin_function(math_acos),
        acosh:  Value::default_builtin_function(math_acosh),
        asin:   Value::default_builtin_function(math_asin),
        asinh:  Value::default_builtin_function(math_asinh),
        atan:   Value::default_builtin_function(math_atan),
        atanh:  Value::default_builtin_function(math_atanh),
        atan2:  Value::default_builtin_function(math_atan2),
        cbrt:   Value::default_builtin_function(math_cbrt),
        ceil:   Value::default_builtin_function(math_ceil),
        clz32:  Value::default_builtin_function(math_clz32),
        cos:    Value::default_builtin_function(math_cos),
        cosh:   Value::default_builtin_function(math_cosh),
        exp:    Value::default_builtin_function(math_exp),
        expm1:  Value::default_builtin_function(math_expm1),
        fround: Value::default_builtin_function(math_fround),
        hypot:  Value::default_builtin_function(math_hypot),
        log:    Value::default_builtin_function(math_log),
        log1p:  Value::default_builtin_function(math_log1p),
        log10:  Value::default_builtin_function(math_log10),
        log2:   Value::default_builtin_function(math_log2),
        max:    Value::default_builtin_function(math_max),
        min:    Value::default_builtin_function(math_min),
        round:  Value::default_builtin_function(math_round),
        sign:   Value::default_builtin_function(math_sign),
        sin:    Value::default_builtin_function(math_sin),
        sinh:   Value::default_builtin_function(math_sinh),
        sqrt:   Value::default_builtin_function(math_sqrt),
        tan:    Value::default_builtin_function(math_tan),
        tanh:   Value::default_builtin_function(math_tanh),
        trunc:  Value::default_builtin_function(math_trunc),
        floor:  {
            let llvm_func = unsafe {
                LLVMAddFunction(
                    jit.module,
                    CString::new("jit_math_floor").unwrap().as_ptr(),
                    LLVMFunctionType(
                        LLVMDoubleTypeInContext(jit.context),
                        vec![LLVMDoubleTypeInContext(jit.context)]
                            .as_mut_slice()
                            .as_mut_ptr(),
                        1,
                        0,
                    ),
                )
            };
            Value::builtin_function_with_jit(
                math_floor,
                BuiltinJITFuncInfo::Normal {
                    func: jit_math_floor as *mut libc::c_void,
                    llvm_func,
                },
            )
        },
        random: {
            let llvm_func = unsafe {
                LLVMAddFunction(
                    jit.module,
                    CString::new("jit_math_random").unwrap().as_ptr(),
                    LLVMFunctionType(
                        LLVMDoubleTypeInContext(jit.context),
                        vec![].as_mut_slice().as_mut_ptr(),
                        0,
                        0,
                    ),
                )
            };
            Value::builtin_function_with_jit(
                math_random,
                BuiltinJITFuncInfo::Normal {
                    func: jit_math_random as *mut libc::c_void,
                    llvm_func,
                },
            )
        },
        pow:    {
            let llvm_func = unsafe {
                LLVMAddFunction(
                    jit.module,
                    CString::new("jit_math_pow").unwrap().as_ptr(),
                    LLVMFunctionType(
                        LLVMDoubleTypeInContext(jit.context),
                        vec![
                            LLVMDoubleTypeInContext(jit.context),
                            LLVMDoubleTypeInContext(jit.context),
                        ]
                            .as_mut_slice()
                            .as_mut_ptr(),
                        2,
                        0,
                    ),
                )
            };
            Value::builtin_function_with_jit(
                math_pow,
                BuiltinJITFuncInfo::Normal {
                    func: jit_math_pow as *mut libc::c_void,
                    llvm_func,
                },
            )
                }
    )
}

macro_rules! simple_math {
    ($name:ident, $f:ident) => {
        #[allow(unused_variables)]
        fn $name(
            vm: &mut VM,
            args: &Vec<Value>,
            callobj: GcType<CallObject>,
        ) -> Result<(), RuntimeError> {
            if let Value::Number(n) = args[0] {
                vm.state.stack.push(Value::Number(n.$f()));
                return Ok(());
            }
            vm.state.stack.push(Value::Undefined);
            Ok(())
        }
    };
}

simple_math!(math_floor, floor);
simple_math!(math_abs, abs);
simple_math!(math_acos, acos);
simple_math!(math_acosh, acosh);
simple_math!(math_asin, asin);
simple_math!(math_asinh, asinh);
simple_math!(math_atan, atan);
simple_math!(math_atanh, atanh);

fn math_atan2(vm: &mut VM, args: &Vec<Value>, _: GcType<CallObject>) -> Result<(), RuntimeError> {
    if let Value::Number(n1) = args[0] {
        if let Value::Number(n2) = args[1] {
            vm.state.stack.push(Value::Number(n1.atan2(n2)));
            return Ok(());
        }
        vm.state.stack.push(Value::Undefined)
    }
    Ok(())
}
simple_math!(math_cbrt, cbrt);
simple_math!(math_ceil, ceil);

fn math_clz32(vm: &mut VM, args: &Vec<Value>, _: GcType<CallObject>) -> Result<(), RuntimeError> {
    if let Value::Number(n) = args[0] {
        vm.state.stack.push(Value::Number(if n == 0.0 {
            32.0
        } else {
            // TODO: >> ? >>> ?
            31.0 - ((n as i32 >> 0) as f64 * ::std::f64::consts::LOG2_E)
                .log(::std::f64::consts::E)
                .floor()
        }));
        return Ok(());
    }
    vm.state.stack.push(Value::Undefined);
    Ok(())
}
simple_math!(math_cos, cos);
simple_math!(math_cosh, cosh);
simple_math!(math_exp, exp);
simple_math!(math_expm1, exp_m1);
simple_math!(math_fround, round);

fn math_hypot(vm: &mut VM, args: &Vec<Value>, _: GcType<CallObject>) -> Result<(), RuntimeError> {
    let mut sum2 = 0.0;
    for n in args {
        if let Value::Number(n) = n {
            sum2 += n * n;
        }
    }
    vm.state.stack.push(Value::Number(sum2.sqrt()));
;    Ok(())
}

fn math_log(vm: &mut VM, args: &Vec<Value>, _: GcType<CallObject>) -> Result<(), RuntimeError> {
    if let Value::Number(n1) = args[0] {
        vm.state
            .stack
            .push(Value::Number(n1.log(::std::f64::consts::E)));
        return Ok(());
    }
    vm.state.stack.push(Value::Undefined);
    Ok(())
}

fn math_log1p(vm: &mut VM, args: &Vec<Value>, _: GcType<CallObject>) -> Result<(), RuntimeError> {
    if let Value::Number(n1) = args[0] {
        vm.state
            .stack
            .push(Value::Number(n1.log(1.0 + ::std::f64::consts::E)));
        return Ok(());
    }
    vm.state.stack.push(Value::Undefined);
    Ok(())
}

simple_math!(math_log10, log10);
simple_math!(math_log2, log2);

fn math_max(vm: &mut VM, args: &Vec<Value>, _: GcType<CallObject>) -> Result<(), RuntimeError> {
    let mut max = if let Value::Number(n) = args[0] {
        n
    } else {
        0.0
    };
    for n in args[1..].iter() {
        if let Value::Number(n) = n {
            if n > &max {
                max = *n;
            }
        }
    }
    vm.state.stack.push(Value::Number(max));
    Ok(())
}

fn math_min(vm: &mut VM, args: &Vec<Value>, _: GcType<CallObject>) -> Result<(), RuntimeError> {
    let mut min = if let Value::Number(n) = args[0] {
        n
    } else {
        0.0
    };
    for n in args[1..].iter() {
        if let Value::Number(n) = n {
            if n < &min {
                min = *n;
            }
        }
    }
    vm.state.stack.push(Value::Number(min));
    Ok(())
}

simple_math!(math_round, round);

fn math_sign(vm: &mut VM, args: &Vec<Value>, _: GcType<CallObject>) -> Result<(), RuntimeError> {
    if let Value::Number(n) = args[0] {
        vm.state.stack.push(Value::Number(if n == 0.0 {
            n
        } else if n > 0.0 {
            1.0
        } else {
            -1.0
        }));
        return Ok(());
    }
    vm.state.stack.push(Value::Undefined);
    Ok(())
}

simple_math!(math_sin, sin);
simple_math!(math_sinh, sinh);
simple_math!(math_sqrt, sqrt);
simple_math!(math_tan, tan);
simple_math!(math_tanh, tanh);
simple_math!(math_trunc, trunc);

fn math_random(vm: &mut VM, _: &Vec<Value>, _: GcType<CallObject>) -> Result<(), RuntimeError> {
    vm.state.stack.push(Value::Number(random::<f64>()));
    Ok(())
}

fn math_pow(vm: &mut VM, args: &Vec<Value>, _: GcType<CallObject>) -> Result<(), RuntimeError> {
    if let Value::Number(f1) = args[0] {
        if let Value::Number(f2) = args[1] {
            vm.state.stack.push(Value::Number(f1.powf(f2)));
            return Ok(());
        }
    }
    vm.state.stack.push(Value::Undefined);
    Ok(())
}

// *** Functions for JIT ***

#[no_mangle]
pub extern "C" fn jit_math_floor(n: f64) -> f64 {
    n.floor()
}

// TODO: Find a better way for rand gen. (rand::random is slow)
pub static mut MATH_RAND_SEED: u64 = 0xf6d582196d588cac;
#[no_mangle]
pub extern "C" fn jit_math_random() -> f64 {
    unsafe {
        MATH_RAND_SEED = MATH_RAND_SEED ^ (MATH_RAND_SEED << 13);
        MATH_RAND_SEED = MATH_RAND_SEED ^ (MATH_RAND_SEED >> 17);
        MATH_RAND_SEED = MATH_RAND_SEED ^ (MATH_RAND_SEED << 5);
        (MATH_RAND_SEED as f64) / ::std::u64::MAX as f64
    }
}

#[no_mangle]
pub extern "C" fn jit_math_pow(x: f64, y: f64) -> f64 {
    x.powf(y)
}
