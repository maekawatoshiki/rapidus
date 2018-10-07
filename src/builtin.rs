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
pub const FUNCTION_PROTOTYPE_CALL: usize = 6;

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

// BuiltinFunction(3)
pub unsafe fn math_floor(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let Value::Number(f) = args[0] {
        self_.state.stack.push(Value::Number(f.floor()))
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

// BuiltinFunction(6)
pub unsafe fn function_prototype_call(callobj: CallObject, args: Vec<Value>, self_: &mut VM) {
    let callee = *callobj.this;
    let arg_this = args[0].clone();
    match callee {
        Value::Function(dst, _obj, mut callobj) => {
            *callobj.this = arg_this;
            callobj.vals = Rc::new(RefCell::new(HashMap::new()));
            call_function(self_,dst, args[1..].to_vec(), callobj);
        }
        _ => {}
    }
}
