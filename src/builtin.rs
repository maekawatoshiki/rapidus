use chrono::Utc;
use gc;
use libc;
use libloading;
use llvm::prelude::LLVMValueRef;
use parser;
use std::ffi::CString;
use std::fs::OpenOptions;
use std::io::prelude::*;
use std::path;
use vm::{
    error::RuntimeError,
    task::{Task, TimerID, TimerKind},
    value::{CallObjectRef, FuncInfo, ObjectKind, Property, RawStringPtr, Value},
    vm::VM,
};
use vm_codegen;

pub type BuiltinFuncTy = fn(&mut VM, &Vec<Value>, CallObjectRef) -> Result<(), RuntimeError>;
pub type BuiltinJITFuncTy = *mut libc::c_void;

#[derive(Clone)]
pub struct BuiltinFuncInfo {
    pub func: BuiltinFuncTy,
    pub jit_info: Option<BuiltinJITFuncInfo>,
}

#[derive(Clone, Debug)]
pub enum BuiltinJITFuncInfo {
    ConsoleLog {
        bool: (BuiltinJITFuncTy, LLVMValueRef),
        f64: (BuiltinJITFuncTy, LLVMValueRef),
        string: (BuiltinJITFuncTy, LLVMValueRef),
        newline: (BuiltinJITFuncTy, LLVMValueRef),
    }, // 'console.log' has variable arguments so treat specially now.
    Normal {
        func: BuiltinJITFuncTy,
        llvm_func: LLVMValueRef,
    },
}

impl BuiltinFuncInfo {
    pub fn new(
        func: BuiltinFuncTy,
        builtin_jit_func_info: Option<BuiltinJITFuncInfo>,
    ) -> BuiltinFuncInfo {
        BuiltinFuncInfo {
            func,
            jit_info: builtin_jit_func_info,
        }
    }
}

impl PartialEq for BuiltinFuncInfo {
    fn eq(&self, other: &BuiltinFuncInfo) -> bool {
        self.func as *mut libc::c_void == other.func as *mut libc::c_void
    }
}

impl ::std::fmt::Debug for BuiltinFuncInfo {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "[BuiltinFunction]")
    }
}

pub fn set_timeout(vm: &mut VM, args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
    if args.len() == 0 {
        return Err(RuntimeError::General(
            "error: setTimeout() needs one argument at least".to_string(),
        ));
    }

    let callback = args[0].clone();
    let timeout = if let Value::Number(millis) = &args[1] {
        *millis as i64
    } else {
        return Err(RuntimeError::Type(
            "type error: second argument must be number".to_string(),
        ));
    };

    let id = vm.task_mgr.add_timer(Task::Timer {
        kind: TimerKind::Timeout {
            now: Utc::now().timestamp_millis(),
            timeout,
        },
        id: 0,
        callback,
        args: vec![],
    });

    vm.state.stack.push(Value::Number(id as f64));

    Ok(())
}

pub fn set_interval(vm: &mut VM, args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
    if args.len() == 0 {
        return Err(RuntimeError::General(
            "error: setInterval() needs one argument at least".to_string(),
        ));
    }

    let callback = args[0].clone();
    let interval = if let Value::Number(millis) = &args[1] {
        *millis as i64
    } else {
        return Err(RuntimeError::Type(
            "type error: second argument must be number".to_string(),
        ));
    };

    let id = vm.task_mgr.add_timer(Task::Timer {
        kind: TimerKind::Interval {
            previous: Utc::now().timestamp_millis(),
            interval,
        },
        id: 0,
        callback,
        args: vec![],
    });

    vm.state.stack.push(Value::Number(id as f64));

    Ok(())
}

pub fn clear_timer(vm: &mut VM, args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
    if args.len() == 0 {
        return Err(RuntimeError::General(
            "error: clearInterval() or clearTimer needs an argument".to_string(),
        ));
    }

    let timer_id = if let Value::Number(id) = &args[0] {
        *id as TimerID
    } else {
        return Err(RuntimeError::Type(
            "type error: first argument must be number".to_string(),
        ));
    };

    vm.task_mgr.clear_timer(timer_id);

    vm.state.stack.push(Value::Undefined);

    Ok(())
}

pub fn console_log(
    self_: &mut VM,
    args: &Vec<Value>,
    _: CallObjectRef,
) -> Result<(), RuntimeError> {
    let args_len = args.len();
    unsafe {
        for i in 0..args_len {
            debug_print(&args[i], false);
            if args_len - 1 != i {
                libc::printf(b" \0".as_ptr() as RawStringPtr);
            }
        }
        libc::puts(b"\0".as_ptr() as RawStringPtr);
    }
    self_.state.stack.push(Value::Undefined);
    Ok(())
}

pub fn process_stdout_write(
    vm: &mut VM,
    args: &Vec<Value>,
    _: CallObjectRef,
) -> Result<(), RuntimeError> {
    let args_len = args.len();
    unsafe {
        for i in 0..args_len {
            debug_print(&args[i], false);
            if args_len - 1 != i {
                libc::printf(b" \0".as_ptr() as RawStringPtr);
            }
        }
    }
    vm.set_return_value(Value::Undefined);
    Ok(())
}

pub fn enable_jit(vm: &mut VM, args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
    let args_len = args.len();
    if args_len == 0 {
        return Err(RuntimeError::General(
            "error: enable_jit() needs one argument".to_string(),
        ));
    };
    match args[0] {
        Value::Bool(b) => vm.jit_on = b,
        _ => {}
    };
    vm.set_return_value(Value::Undefined);
    Ok(())
}

/// assertion by strict equality comparison.
/// Usage:
/// assert_seq(actual, expected)
/// if actual === expected, return true.
pub fn assert_seq(vm: &mut VM, args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
    let args_len = args.len();
    if args_len < 2 {
        return Err(RuntimeError::General(
            "error: assert() needs two arguments".to_string(),
        ));
    };
    if args[0].clone().strict_equal(args[1].clone())? {
        vm.set_return_value(Value::Undefined);
        Ok(())
    } else {
        Err(RuntimeError::General(format!(
            "AssertionError [ERR_ASSERTION]: {} == {}",
            args[0].format(1, true),
            args[1].format(1, true),
        )))
    }
}

pub fn debug_print(val: &Value, nest: bool) {
    fn show_obj(sorted_key_val: Vec<(&String, &Property)>) {
        for (i, tupple) in sorted_key_val.iter().enumerate() {
            unsafe {
                libc::printf(
                    "'%s'\0".as_ptr() as RawStringPtr,
                    CString::new(tupple.0.as_str()).unwrap().into_raw(),
                );
                libc::printf(": \0".as_ptr() as RawStringPtr);
                debug_print(&tupple.1.val, true);
                libc::printf(if i != sorted_key_val.len() - 1 {
                    ", \0".as_ptr() as RawStringPtr
                } else {
                    " \0".as_ptr() as RawStringPtr
                });
            }
        }
    }

    unsafe {
        match val {
            Value::Empty => {
                libc::printf(b"empty\0".as_ptr() as RawStringPtr);
            }
            Value::Object(_, ObjectKind::Arguments(state)) => {
                let args = &state.arguments;
                libc::printf("[ \0".as_ptr() as RawStringPtr);

                let mut i = 0;
                let length = args.len();
                while i < length {
                    if i != 0 {
                        libc::printf(", \0".as_ptr() as RawStringPtr);
                    };
                    match state.get_arguments_nth_value(i) {
                        Ok(val) => {
                            debug_print(&val, true);
                        }
                        Err(_) => {
                            libc::printf(" \0".as_ptr() as RawStringPtr);
                        }
                    };

                    i += 1;
                }
                libc::printf(" ]\0".as_ptr() as RawStringPtr);
            }
            Value::Null => {
                libc::printf(b"null\0".as_ptr() as RawStringPtr);
            }
            Value::Undefined => {
                libc::printf(b"undefined\0".as_ptr() as RawStringPtr);
            }
            Value::Bool(true) => {
                libc::printf(b"true\0".as_ptr() as RawStringPtr);
            }
            Value::Bool(false) => {
                libc::printf(b"false\0".as_ptr() as RawStringPtr);
            }
            Value::Number(n) => {
                if n.is_nan() {
                    libc::printf("NaN\0".as_ptr() as RawStringPtr);
                } else if n.is_infinite() {
                    libc::printf("Infinity\0".as_ptr() as RawStringPtr);
                } else {
                    libc::printf("%.15g\0".as_ptr() as RawStringPtr, *n);
                }
            }
            Value::String(ref s) => {
                libc::printf(
                    if nest { "'%s'\0" } else { "%s\0" }.as_ptr() as RawStringPtr,
                    s.as_ptr(),
                );
            }
            Value::Object(ref map, ObjectKind::Ordinary) => {
                libc::printf("{ \0".as_ptr() as RawStringPtr);

                let mut sorted_key_val = (&*map).iter().collect::<Vec<(&String, &Property)>>();
                sorted_key_val.sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));
                sorted_key_val.retain(|(ref key, _)| key != &"__proto__");

                show_obj(sorted_key_val);

                libc::printf("}\0".as_ptr() as RawStringPtr);
            }
            Value::Object(map, ObjectKind::Array(ref values)) => {
                libc::printf("[ \0".as_ptr() as RawStringPtr);
                let arr = &*(values);
                let elems = &arr.elems;
                let is_last_idx = |idx: usize| -> bool { idx == arr.length - 1 };
                let mut i = 0;
                let mut sorted_key_val = (&*map).iter().collect::<Vec<(&String, &Property)>>();
                sorted_key_val.sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));
                sorted_key_val.retain(|(ref key, _)| key != &"__proto__");

                while i < arr.length {
                    let mut empty_elems = 0;
                    while i < arr.length && Value::Empty == elems[i].val {
                        empty_elems += 1;
                        i += 1;
                    }

                    if empty_elems > 0 {
                        libc::printf(
                            "<%u empty item%s>%s\0".as_ptr() as RawStringPtr,
                            empty_elems,
                            if empty_elems >= 2 { "s\0" } else { "\0" }.as_ptr() as RawStringPtr,
                            if is_last_idx(i - 1) && sorted_key_val.len() == 0 {
                                " \0"
                            } else {
                                ", \0"
                            }
                            .as_ptr() as RawStringPtr,
                        );

                        if is_last_idx(i - 1) {
                            break;
                        }
                    }

                    debug_print(&elems[i].val, true);
                    libc::printf(
                        if is_last_idx(i) && sorted_key_val.len() == 0 {
                            " \0"
                        } else {
                            ", \0"
                        }
                        .as_ptr() as RawStringPtr,
                    );

                    i += 1;
                }

                show_obj(sorted_key_val);

                libc::printf("]\0".as_ptr() as RawStringPtr);
            }
            Value::Object(_, ObjectKind::Function(_))
            | Value::Object(_, ObjectKind::BuiltinFunction(_)) => {
                libc::printf("[Function]\0".as_ptr() as RawStringPtr);
            }
            Value::Object(_, ObjectKind::Date(box time_val)) => {
                // TODO: Date needs toString() ?
                libc::printf(
                    "%s\0".as_ptr() as RawStringPtr,
                    CString::new(time_val.to_rfc3339()).unwrap().as_ptr(),
                );
            }
        }
    }
}

pub fn require(vm: &mut VM, args: &Vec<Value>, callobj: CallObjectRef) -> Result<(), RuntimeError> {
    enum RequireFileKind {
        DLL(String),
        Normal(String),
        NotFound,
    }

    fn find_file(file_name: &str) -> RequireFileKind {
        let paths = vec!["#.js", "lib#.so", "lib#.dylib"];
        match paths
            .iter()
            .find(|path| {
                let file_name = path.replace("#", file_name);
                path::Path::new(file_name.as_str()).exists()
            })
            .and_then(|path| Some(path.replace("#", file_name)))
        {
            Some(path) => {
                let path = path.to_ascii_lowercase();
                if path.ends_with(".so") || path.ends_with(".dylib") {
                    RequireFileKind::DLL(path)
                } else {
                    RequireFileKind::Normal(path)
                }
            }
            None => RequireFileKind::NotFound,
        }
    }

    if args.len() == 0 {
        return Err(RuntimeError::General(
            "require() needs an argument.".to_string(),
        ));
    }

    let file_name = match args[0] {
        Value::String(ref s) => s.to_str().unwrap().clone(),
        _ => {
            return Err(RuntimeError::Type(
                "require() arguments must be string.".to_string(),
            ));
        }
    };

    match find_file(file_name) {
        RequireFileKind::DLL(name) => {
            let dylib_path = name.as_str();
            let dylib = libloading::Library::new(dylib_path);
            let symbol_name = b"initialize\0";

            match dylib {
                Ok(lib) => {
                    let initialize: Result<libloading::Symbol<BuiltinFuncTy>, _> =
                        unsafe { lib.get(symbol_name) };
                    match initialize {
                        Ok(initialize) => initialize(vm, args, callobj)?,
                        Err(_) => println!("'initialize' needs to be defined in DLL."),
                    }
                }
                Err(msg) => println!("{}: {}", msg, dylib_path),
            }

            return Ok(());
        }
        RequireFileKind::Normal(name) => {
            let file_name = name.as_str();
            let mut file_body = String::new();

            match OpenOptions::new().read(true).open(file_name) {
                Ok(mut ok) => match ok.read_to_string(&mut file_body).ok() {
                    Some(x) => x,
                    None => {
                        return Err(RuntimeError::General(format!(
                            "error: Couldn't read file '{}'",
                            file_name
                        )));
                    }
                },
                Err(_) => {
                    return Err(RuntimeError::General(format!(
                        "error: Couldn't find module '{}'",
                        file_name
                    )));
                }
            };

            if file_body.len() == 0 {
                return Ok(());
            }

            if file_body.as_bytes()[0] == b'#' {
                let first_ln = file_body.find('\n').unwrap_or(file_body.len());
                file_body.drain(..first_ln);
            }
            let mut parser = parser::Parser::new(file_body);

            let node = match parser.parse_all() {
                Ok(ok) => ok,
                Err(err) => {
                    parser.handle_error(err);
                    return Err(RuntimeError::General(
                        "parse error in require()".to_string(),
                    ));
                }
            };
            // this FuncInfo is a dummy.
            let func_info = FuncInfo::new(0, vec![], vec![]);
            let mut callobj = callobj.new_callobj_from_func(None);
            callobj.parent = Some(vm.state.scope.last().unwrap().clone());
            vm.store_state();
            vm.state.scope.push(callobj);
            vm.state.apply_arguments(func_info.clone(), args);

            let mut iseq = vec![];
            match vm.codegen.compile(&node, &mut iseq, false) {
                Ok(()) => {}
                Err(vm_codegen::Error::General { msg, token_pos }) => {
                    parser.show_error_at(token_pos, msg.as_str());
                    return Ok(());
                }
                Err(e) => panic!(e),
            }

            vm.state.scope.last_mut().unwrap().set_value(
                "module".to_string(),
                Value::object_from_npp(&make_npp!(exports: Value::Undefined)),
            );

            vm.do_run(&iseq)?;

            let module_exports = vm
                .state
                .scope
                .last()
                .unwrap()
                .get_value(&"module".to_string())?
                .get_property(Value::string("exports".to_string()), None);
            vm.state.scope.pop();
            vm.restore_state();

            vm.state.stack.push(module_exports);
            gc::mark_and_sweep(vm);
        }
        RequireFileKind::NotFound => {
            return Err(RuntimeError::General(format!(
                "error: Couldn't find module '{}'",
                file_name
            )));
        }
    }
    Ok(())
}

// Functions for JIT

#[no_mangle]
pub extern "C" fn jit_console_log_string(s: RawStringPtr) {
    unsafe {
        libc::printf(b"%s \0".as_ptr() as RawStringPtr, s);
    }
}

#[no_mangle]
pub extern "C" fn jit_console_log_bool(b: bool) {
    unsafe {
        if b {
            libc::printf(b"true \0".as_ptr() as RawStringPtr);
        } else {
            libc::printf(b"false \0".as_ptr() as RawStringPtr);
        }
    }
}

#[no_mangle]
pub extern "C" fn jit_console_log_f64(n: f64) {
    unsafe {
        libc::printf(b"%.15g \0".as_ptr() as RawStringPtr, n);
    }
}

#[no_mangle]
pub extern "C" fn jit_console_log_newline() {
    unsafe {
        libc::printf(b"\n\0".as_ptr() as RawStringPtr);
    }
}

#[no_mangle]
pub extern "C" fn jit_process_stdout_write(s: RawStringPtr) {
    unsafe {
        libc::printf(b"%s\0".as_ptr() as RawStringPtr, s);
    }
}
