use vm::{RawStringPtr, Value, VM};

use libc;
use rand::random;

use std::ffi::CString;

pub const CONSOLE_LOG: usize = 0;
pub const PROCESS_STDOUT_WRITE: usize = 1;
pub const ARRAY_PUSH: usize = 2;
pub const MATH_FLOOR: usize = 3;
pub const MATH_RANDOM: usize = 4;
pub const MATH_POW: usize = 5;
pub const FUNCTION_PROTOTYPE_CALL: usize = 6;

// BuiltinFunction(0)
pub unsafe fn console_log(args: Vec<Value>, _: &mut VM) {
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
            Value::Object(_) | Value::Array(_) | Value::Function(_, _,_) => debug_print(&args[i]),
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
pub unsafe fn process_stdout_write(args: Vec<Value>, _: &mut VM) {
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
        &Value::Function(_, _,_) => {
            libc::printf("[Function]\0".as_ptr() as RawStringPtr);
        }
        &Value::Undefined => {
            libc::printf(b"undefined\0".as_ptr() as RawStringPtr);
        }
        _ => {}
    }
}

// BuiltinFunction(2)
pub unsafe fn array_push(args: Vec<Value>, _: &mut VM) {
    if let Value::Array(ref map) = args[0] {
        let mut map = map.borrow_mut();
        // let mut elems = &mut map.elems;
        for val in args[1..].iter() {
            map.elems.push(val.clone());
        }
        map.length += args[1..].len();
    } else {
        unreachable!()
    };
}

// BuiltinFunction(3)
pub unsafe fn math_floor(args: Vec<Value>, self_: &mut VM) {
    if let Value::Number(f) = args[0] {
        self_.state.stack.push(Value::Number(f.floor()))
    }
}

// BuiltinFunction(4)
pub unsafe fn math_random(_args: Vec<Value>, self_: &mut VM) {
    self_.state.stack.push(Value::Number(random::<f64>()))
}

// BuiltinFunction(5)
pub unsafe fn math_pow(args: Vec<Value>, self_: &mut VM) {
    if let Value::Number(f1) = args[0] {
        if let Value::Number(f2) = args[1] {
            self_.state.stack.push(Value::Number(f1.powf(f2)))
        }
    }
}

// BuiltinFunction(6)
pub unsafe fn function_prototype_call(args: Vec<Value>, self_: &mut VM) {
    unimplemented!();
    // let mut callee = args[0].clone();
    // loop {
    //     match callee {
    //         Value::Function(dst, _obj,) => {
    //             self_.state.history.push((0, 0, 0, self_.state.pc));
    //
    //             self_.state.stack.push(args[1].clone());
    //
    //             for arg in args[2..].iter() {
    //                 self_.state.stack.push(arg.clone());
    //             }
    //
    //             self_.state.pc = dst as isize;
    //             self_
    //                 .state
    //                 .stack
    //                 .push(Value::Number(args.len() as f64 - 1.0 /*callee*/));
    //
    //             self_.do_run();
    //
    //             match self_.state.stack.last_mut().unwrap() {
    //                 &mut Value::Object(_)
    //                 | &mut Value::Array(_)
    //                 | &mut Value::Function(_, _)
    //                 | &mut Value::BuiltinFunction(_) => {}
    //                 others => *others = args[1].clone(),
    //             };
    //             break;
    //         }
    //         Value::NeedThis(callee_) => {
    //             callee = *callee_;
    //         }
    //         Value::WithThis(box (callee_, _)) => {
    //             callee = callee_;
    //         }
    //         c => {
    //             println!(
    //                 "Function.prototype.call: err: {:?}, pc = {}",
    //                 c, self_.state.pc
    //             );
    //             break;
    //         }
    //     }
    // }
}
