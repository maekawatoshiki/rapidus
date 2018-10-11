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
pub const FUNCTION_PROTOTYPE_CALL: usize = 17;

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
    ($self:ident, $args:ident, $f:ident) => {
        if let Value::Number(n) = $args[0] {
            $self.state.stack.push(Value::Number(n.$f()))
        }
    };
}

// BuiltinFunction(3)
pub unsafe fn math_floor(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    simple_math!(self_, args, floor);
}
// builtinfunction(6)
pub unsafe fn math_abs(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    simple_math!(self_, args, abs);
}
// builtinfunction(7)
pub unsafe fn math_acos(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    simple_math!(self_, args, acos);
}
// builtinfunction(8)
pub unsafe fn math_acosh(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    simple_math!(self_, args, acosh);
}
// builtinfunction(9)
pub unsafe fn math_asin(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    simple_math!(self_, args, asin);
}
// builtinfunction(10)
pub unsafe fn math_asinh(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    simple_math!(self_, args, asinh);
}
// builtinfunction(11)
pub unsafe fn math_atan(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    simple_math!(self_, args, atan);
}
// builtinfunction(12)
pub unsafe fn math_atanh(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    simple_math!(self_, args, atanh);
}
// builtinfunction(13)
pub unsafe fn math_atan2(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let Value::Number(n1) = args[0] {
        if let Value::Number(n2) = args[1] {
            self_.state.stack.push(Value::Number(n1.atan2(n2)))
        }
    }
}
// builtinfunction(14)
pub unsafe fn math_cbrt(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    simple_math!(self_, args, cbrt);
}
// builtinfunction(15)
pub unsafe fn math_ceil(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    simple_math!(self_, args, ceil);
}
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

// BuiltinFunction(10)
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
