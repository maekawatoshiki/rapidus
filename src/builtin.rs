use chrono::Utc;
use libc;
use libloading;
use llvm::prelude::LLVMValueRef;
use std::ffi::CString;
use std::fs::OpenOptions;
use std::io::prelude::*;
use std::path;

use lexer;
use parser;
use parser::Error::*;
use vm::{
    callobj::CallObject,
    error::RuntimeError,
    task::{Task, TimerID, TimerKind},
    value::{RawStringPtr, Value, ValueBase},
    vm::VM,
};
use vm_codegen;

pub type BuiltinFuncTy = fn(&mut VM, &Vec<Value>, &CallObject) -> Result<(), RuntimeError>;
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

pub fn set_timeout(vm: &mut VM, args: &Vec<Value>, _: &CallObject) -> Result<(), RuntimeError> {
    if args.len() == 0 {
        return Err(RuntimeError::General(
            "error: setTimeout() needs one argument at least".to_string(),
        ));
    }

    let callback = args[0].clone();
    let timeout = if let ValueBase::Number(millis) = &args[1].val {
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

    vm.state.stack.push(Value::number(id as f64));

    Ok(())
}

pub fn set_interval(vm: &mut VM, args: &Vec<Value>, _: &CallObject) -> Result<(), RuntimeError> {
    if args.len() == 0 {
        return Err(RuntimeError::General(
            "error: setInterval() needs one argument at least".to_string(),
        ));
    }

    let callback = args[0].clone();
    let interval = if let ValueBase::Number(millis) = &args[1].val {
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

    vm.state.stack.push(Value::number(id as f64));

    Ok(())
}

pub fn clear_timer(vm: &mut VM, args: &Vec<Value>, _: &CallObject) -> Result<(), RuntimeError> {
    if args.len() == 0 {
        return Err(RuntimeError::General(
            "error: clearInterval() or clearTimer needs an argument".to_string(),
        ));
    }

    let timer_id = if let ValueBase::Number(id) = &args[0].val {
        *id as TimerID
    } else {
        return Err(RuntimeError::Type(
            "type error: first argument must be number".to_string(),
        ));
    };

    vm.task_mgr.clear_timer(timer_id);

    vm.state.stack.push(Value::undefined());

    Ok(())
}

pub fn console_log(self_: &mut VM, args: &Vec<Value>, _: &CallObject) -> Result<(), RuntimeError> {
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
    self_.state.stack.push(Value::undefined());
    Ok(())
}

pub fn process_stdout_write(
    vm: &mut VM,
    args: &Vec<Value>,
    _: &CallObject,
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
    vm.state.stack.push(Value::undefined());
    Ok(())
}

pub fn debug_print(val: &Value, nest: bool) {
    unsafe fn show_obj(sorted_key_val: Vec<(&String, &Value)>) {
        for (i, (key, val)) in sorted_key_val.iter().enumerate() {
            libc::printf(
                "'%s'\0".as_ptr() as RawStringPtr,
                CString::new(key.as_str()).unwrap().into_raw(),
            );
            libc::printf(": \0".as_ptr() as RawStringPtr);
            debug_print(&val, true);
            libc::printf(if i != sorted_key_val.len() - 1 {
                ", \0".as_ptr() as RawStringPtr
            } else {
                " \0".as_ptr() as RawStringPtr
            });
        }
    }

    unsafe {
        match val.val {
            ValueBase::Empty | ValueBase::Arguments => {
                libc::printf("'Unsupported. Sorry.'\0".as_ptr() as RawStringPtr);
            }
            ValueBase::Null => {
                libc::printf(b"null\0".as_ptr() as RawStringPtr);
            }
            ValueBase::Undefined => {
                libc::printf(b"undefined\0".as_ptr() as RawStringPtr);
            }
            ValueBase::Bool(true) => {
                libc::printf(b"true\0".as_ptr() as RawStringPtr);
            }
            ValueBase::Bool(false) => {
                libc::printf(b"false\0".as_ptr() as RawStringPtr);
            }
            ValueBase::Number(n) => {
                if n.is_nan() {
                    libc::printf("NaN\0".as_ptr() as RawStringPtr);
                } else if n.is_infinite() {
                    libc::printf("Infinity\0".as_ptr() as RawStringPtr);
                } else {
                    libc::printf("%.15g\0".as_ptr() as RawStringPtr, n);
                }
            }
            ValueBase::String(ref s) => {
                libc::printf(
                    if nest { "'%s'\0" } else { "%s\0" }.as_ptr() as RawStringPtr,
                    s.as_ptr(),
                );
            }
            ValueBase::Object(ref values) => {
                libc::printf("{ \0".as_ptr() as RawStringPtr);

                let key_val = &**values;
                let mut sorted_key_val = key_val.iter().collect::<Vec<(&String, &Value)>>();
                sorted_key_val.sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));
                sorted_key_val.retain(|(ref key, _)| key != &"__proto__");

                show_obj(sorted_key_val);

                libc::printf("}\0".as_ptr() as RawStringPtr);
            }
            ValueBase::Array(ref values) => {
                libc::printf("[ \0".as_ptr() as RawStringPtr);
                let arr = &*(*values);
                let elems = &arr.elems;
                let is_last_idx = |idx: usize| -> bool { idx == arr.length - 1 };
                let mut i = 0;

                let key_val = &arr.obj;
                let mut sorted_key_val = key_val.iter().collect::<Vec<(&String, &Value)>>();
                sorted_key_val.sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));
                sorted_key_val.retain(|(ref key, _)| key != &"__proto__");

                while i < arr.length {
                    let mut empty_elems = 0;
                    while i < arr.length && ValueBase::Empty == elems[i].val {
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

                    debug_print(&elems[i], true);
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
            ValueBase::Function(_) | ValueBase::BuiltinFunction(_) => {
                libc::printf("[Function]\0".as_ptr() as RawStringPtr);
            }
            ValueBase::Date(box (time_val, _)) => {
                // TODO: Date needs toString() ?
                libc::printf(
                    "%s\0".as_ptr() as RawStringPtr,
                    CString::new(time_val.to_rfc3339()).unwrap().as_ptr(),
                );
            }
        }
    }
}

pub fn require(vm: &mut VM, args: &Vec<Value>, callobj: &CallObject) -> Result<(), RuntimeError> {
    enum RequireFileKind {
        DLL(String),
        Normal(String),
        NotFound,
    }

    fn find_file(file_name: &str) -> RequireFileKind {
        let paths = vec!["#", "lib#.so", "lib#.dylib"];
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
            "error: require(NEED AN ARGUMENT)".to_string(),
        ));
    }

    let file_name = match args[0].val {
        ValueBase::String(ref s) => s.to_str().unwrap().clone(),
        _ => {
            return Err(RuntimeError::Type(
                "type error: require(ARGUMENT MUST BE STRING)".to_string(),
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
                Err(NormalEOF) => unreachable!(),
                Err(Expect(pos, kind, msg))
                | Err(General(pos, kind, msg))
                | Err(UnexpectedEOF(pos, kind, msg))
                | Err(UnexpectedToken(pos, kind, msg)) => {
                    parser.show_error_at(pos, kind, msg.as_str());
                    vm.state.stack.push(Value::undefined());
                    return Ok(());
                }
                Err(UnsupportedFeature(pos)) => {
                    parser.enhanced_show_error_at(pos, "unsupported feature");
                    vm.state.stack.push(Value::undefined());
                    return Ok(());
                }
            };

            let mut vm_codegen = vm_codegen::VMCodeGen::new();
            let mut iseq = vec![];
            vm_codegen.bytecode_gen.const_table = vm.const_table.clone();

            match vm_codegen.compile(&node, &mut iseq, false) {
                Ok(()) => {}
                Err(vm_codegen::Error::General { msg, token_pos }) => {
                    parser.show_error_at(token_pos, lexer::ErrorMsgKind::Normal, msg.as_str());
                    return Ok(());
                }
                Err(e) => panic!(e),
            }

            vm.const_table = vm_codegen.bytecode_gen.const_table.clone();

            let mut vm = VM::new(vm_codegen.global_varmap);
            vm.const_table = vm_codegen.bytecode_gen.const_table;
            vm.run(iseq).unwrap();

            let module_exports =
                unsafe { (**vm.state.scope.last().unwrap()).get_value(&"module".to_string()) }
                    .unwrap()
                    .get_property(Value::string("exports".to_string()).val, None);
            vm.state.stack.push(module_exports);
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
