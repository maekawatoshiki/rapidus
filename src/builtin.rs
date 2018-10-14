use vm::{call_function, CallObject, RawStringPtr, Value, VM};

use libc;
use rand::random;

use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::CString;
use std::rc::Rc;

pub const CONSOLE_LOG: usize = 0;
pub const PROCESS_STDOUT_WRITE: usize = 1;
pub const ARRAY_PUSH: usize = 2;
pub const MATH_FLOOR: usize = 3;
pub const MATH_RANDOM: usize = 4;
pub const MATH_POW: usize = 5;
pub const MATH_ABS: usize = 6;
pub const MATH_ACOS: usize = 7;
pub const MATH_ACOSH: usize = 8;
pub const MATH_ASIN: usize = 9;
pub const MATH_ASINH: usize = 10;
pub const MATH_ATAN: usize = 11;
pub const MATH_ATANH: usize = 12;
pub const MATH_ATAN2: usize = 13;
pub const MATH_CBRT: usize = 14;
pub const MATH_CEIL: usize = 15;
pub const MATH_CLZ32: usize = 16;
pub const MATH_COS: usize = 17;
pub const MATH_COSH: usize = 18;
pub const MATH_EXP: usize = 19;
pub const MATH_EXPM1: usize = 20;
pub const MATH_FROUND: usize = 21;
pub const MATH_HYPOT: usize = 22;
pub const MATH_LOG: usize = 23;
pub const MATH_LOG1P: usize = 24;
pub const MATH_LOG10: usize = 25;
pub const MATH_LOG2: usize = 26;
pub const MATH_MAX: usize = 27;
pub const MATH_MIN: usize = 28;
pub const MATH_ROUND: usize = 29;
pub const MATH_SIGN: usize = 30;
pub const MATH_SIN: usize = 31;
pub const MATH_SINH: usize = 32;
pub const MATH_SQRT: usize = 33;
pub const MATH_TAN: usize = 34;
pub const MATH_TANH: usize = 35;
pub const MATH_TRUNC: usize = 36;
pub const FUNCTION_PROTOTYPE_CALL: usize = 37;

// BuiltinFunction(0)
pub unsafe fn console_log(_: CallObject, args: Vec<Value>, _: &mut VM) {
    let args_len = args.len();
    for i in 0..args_len {
        match args[i] {
            Value::String(ref s) => {
                libc::printf(b"%s\0".as_ptr() as RawStringPtr, s.as_ptr());
            }
            Value::Number(ref n) => {
                libc::printf(b"%.15g\0".as_ptr() as RawStringPtr, *n);
            }
            Value::Bool(true) => {
                libc::printf(b"true\0".as_ptr() as RawStringPtr);
            }
            Value::Bool(false) => {
                libc::printf(b"false\0".as_ptr() as RawStringPtr);
            }
            Value::Object(_) | Value::Array(_) | Value::Function(_, _, _) => debug_print(&args[i]),
            Value::Undefined => {
                libc::printf(b"undefined\0".as_ptr() as RawStringPtr);
            }
            _ => {}
        }
        if args_len - 1 != i {
            libc::printf(b" \0".as_ptr() as RawStringPtr);
        }
    }
    libc::puts(b"\0".as_ptr() as RawStringPtr);
}

// BuiltinFunction(1)
pub unsafe fn process_stdout_write(_: CallObject, args: Vec<Value>, _: &mut VM) {
    let args_len = args.len();
    for i in 0..args_len {
        match args[i] {
            Value::String(ref s) => {
                libc::printf(b"%s\0".as_ptr() as RawStringPtr, s.as_ptr());
            }
            Value::Number(ref n) => {
                libc::printf(b"%.15g\0".as_ptr() as RawStringPtr, *n);
            }
            Value::Undefined => {
                libc::printf(b"undefined\0".as_ptr() as RawStringPtr);
            }
            _ => {}
        }
        if args_len - 1 != i {
            libc::printf(b" \0".as_ptr() as RawStringPtr);
        }
    }
}

pub unsafe fn debug_print(val: &Value) {
    match val {
        &Value::String(ref s) => {
            libc::printf("'%s'\0".as_ptr() as RawStringPtr, s.as_ptr());
        }
        &Value::Number(ref n) => {
            libc::printf("%.15g\0".as_ptr() as RawStringPtr, *n);
        }
        &Value::Object(ref values) => {
            libc::printf("{ \0".as_ptr() as RawStringPtr);
            for (key, val) in &*(*values).borrow() {
                libc::printf(
                    "'%s'\0".as_ptr() as RawStringPtr,
                    CString::new(key.as_str()).unwrap().into_raw(),
                );
                libc::printf(": \0".as_ptr() as RawStringPtr);
                debug_print(&val);
                libc::printf(", \0".as_ptr() as RawStringPtr);
            }
            libc::printf("}\0".as_ptr() as RawStringPtr);
        }
        &Value::Array(ref values) => {
            libc::printf("[ \0".as_ptr() as RawStringPtr);
            let arr = &*(*values).borrow();
            let elems = &arr.elems;
            for i in 0..arr.length {
                debug_print(&elems[i]);
                libc::printf(", \0".as_ptr() as RawStringPtr);
            }
            libc::printf("]\0".as_ptr() as RawStringPtr);
        }
        &Value::Function(_, _, _) => {
            libc::printf("[Function]\0".as_ptr() as RawStringPtr);
        }
        &Value::Undefined => {
            libc::printf(b"undefined\0".as_ptr() as RawStringPtr);
        }
        _ => {}
    }
}

// BuiltinFunction(2)
pub unsafe fn array_push(callobj: CallObject, args: Vec<Value>, _: &mut VM) {
    if let box Value::Array(ref map) = callobj.this {
        let mut map = map.borrow_mut();
        // let mut elems = &mut map.elems;
        for val in &args {
            map.elems.push(val.clone());
        }
        map.length += args.len();
    } else {
        unreachable!()
    };
}

macro_rules! simple_math {
    ($name:ident, $f:ident) => {
        pub unsafe fn $name(_: CallObject, args: Vec<Value>, self_: &mut VM) {
            if let Value::Number(n) = args[0] {
                self_.state.stack.push(Value::Number(n.$f()))
            }
        }
    };
}

simple_math!(math_floor, floor); // BuiltinFunction(3)
simple_math!(math_abs, abs); // builtinfunction(6)
simple_math!(math_acos, acos); // builtinfunction(7)
simple_math!(math_acosh, acosh); // builtinfunction(8)
simple_math!(math_asin, asin); // builtinfunction(9)
simple_math!(math_asinh, asinh); // builtinfunction(10)
simple_math!(math_atan, atan); // builtinfunction(11)
simple_math!(math_atanh, atanh); // builtinfunction(12)

// builtinfunction(13)
pub unsafe fn math_atan2(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let Value::Number(n1) = args[0] {
        if let Value::Number(n2) = args[1] {
            self_.state.stack.push(Value::Number(n1.atan2(n2)))
        }
    }
}
simple_math!(math_cbrt, cbrt); // builtinfunction(14)
simple_math!(math_ceil, ceil); // builtinfunction(15)

// builtinfunction(16)
pub unsafe fn math_clz32(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let Value::Number(n) = args[0] {
        self_.state.stack.push(Value::Number(if n == 0.0 {
            32.0
        } else {
            // TODO: >> ? >>> ?
            31.0 - ((n as i32 >> 0) as f64 * ::std::f64::consts::LOG2_E)
                .log(::std::f64::consts::E)
                .floor()
        }))
    }
}
simple_math!(math_cos, cos); // builtinfunction(17)
simple_math!(math_cosh, cosh); // builtinfunction(18)
simple_math!(math_exp, exp); // builtinfunction(19)
simple_math!(math_expm1, exp_m1); // builtinfunction(20)
simple_math!(math_fround, round); // builtinfunction(21) TODO: Implement correctly

// builtinfunction(22)
pub unsafe fn math_hypot(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    let mut sum2 = 0.0;
    for n in args {
        if let Value::Number(n) = n {
            sum2 += n * n;
        }
    }
    self_.state.stack.push(Value::Number(sum2.sqrt()));
}

// builtinfunction(23)
pub unsafe fn math_log(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let Value::Number(n1) = args[0] {
        self_
            .state
            .stack
            .push(Value::Number(n1.log(::std::f64::consts::E)));
    }
}

// builtinfunction(24)
pub unsafe fn math_log1p(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let Value::Number(n1) = args[0] {
        self_
            .state
            .stack
            .push(Value::Number(n1.log(1.0 + ::std::f64::consts::E)));
    }
}

simple_math!(math_log10, log10); // builtinfunction(25)
simple_math!(math_log2, log2); // builtinfunction(26)

// builtinfunction(27)
pub unsafe fn math_max(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    let mut max = if let Value::Number(n) = args[0] {
        n
    } else {
        0.0
    };
    for n in args[1..].iter() {
        if let Value::Number(n) = n {
            if *n > max {
                max = *n;
            }
        }
    }
    self_.state.stack.push(Value::Number(max));
}

// builtinfunction(28)
pub unsafe fn math_min(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    let mut min = if let Value::Number(n) = args[0] {
        n
    } else {
        0.0
    };
    for n in args[1..].iter() {
        if let Value::Number(n) = n {
            if *n < min {
                min = *n;
            }
        }
    }
    self_.state.stack.push(Value::Number(min));
}

simple_math!(math_round, round); // builtinfunction(29)

// builtinfunction(30)
pub unsafe fn math_sign(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let Value::Number(n) = args[0] {
        self_.state.stack.push(Value::Number(if n == 0.0 {
            n
        } else if n > 0.0 {
            1.0
        } else {
            -1.0
        }));
    }
}

simple_math!(math_sin, sin); // builtinfunction(31)
simple_math!(math_sinh, sinh); // builtinfunction(32)
simple_math!(math_sqrt, sqrt); // builtinfunction(33)
simple_math!(math_tan, tan); // builtinfunction(34)
simple_math!(math_tanh, tanh); // builtinfunction(35)
simple_math!(math_trunc, trunc); // builtinfunction(36)

// BuiltinFunction(4)
pub unsafe fn math_random(_: CallObject, _args: Vec<Value>, self_: &mut VM) {
    self_.state.stack.push(Value::Number(random::<f64>()))
}

// BuiltinFunction(5)
pub unsafe fn math_pow(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let Value::Number(f1) = args[0] {
        if let Value::Number(f2) = args[1] {
            self_.state.stack.push(Value::Number(f1.powf(f2)))
        }
    }
}

// BuiltinFunction(37)
pub unsafe fn function_prototype_call(callobj: CallObject, args: Vec<Value>, self_: &mut VM) {
    let callee = *callobj.this;
    let arg_this = args[0].clone();
    match callee {
        Value::Function(dst, _obj, mut callobj) => {
            *callobj.this = arg_this;
            callobj.vals = Rc::new(RefCell::new(HashMap::new()));
            call_function(self_, dst, args[1..].to_vec(), callobj);
        }
        _ => {}
    }
}