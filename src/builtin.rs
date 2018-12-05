use libc;
use libloading;
use llvm::prelude::LLVMValueRef;
use std::ffi::CString;

use vm::{
    callobj::CallObject,
    value::{RawStringPtr, Value, ValueBase},
    vm::{call_function, VM},
};

pub type BuiltinFuncTy = fn(&mut VM, &Vec<Value>, &CallObject);
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

pub fn console_log(self_: &mut VM, args: &Vec<Value>, _: &CallObject) {
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
    self_.state.stack.push(Value::undefined())
}

pub fn process_stdout_write(vm: &mut VM, args: &Vec<Value>, _: &CallObject) {
    let args_len = args.len();
    unsafe {
        for i in 0..args_len {
            debug_print(&args[i], false);
            if args_len - 1 != i {
                libc::printf(b" \0".as_ptr() as RawStringPtr);
            }
        }
    }
    vm.state.stack.push(Value::undefined())
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

pub fn function_prototype_apply(vm: &mut VM, args: &Vec<Value>, callobj: &CallObject) {
    let callee = &*callobj.this;
    let arg_this = args[0].clone();
    let arg = match args[1].val {
        ValueBase::Array(aryval) => {
            let aryval = unsafe { &*aryval };
            let mut elems = vec![];
            for i in 0..aryval.length {
                elems.push(aryval.elems[i].clone());
            }
            elems
        }
        ValueBase::Arguments => {
            let mut elems = vec![];
            let callobj = unsafe { &**vm.state.scope.last().unwrap() };
            let length = callobj.get_arguments_length();
            for i in 0..length {
                elems.push(callobj.get_arguments_nth_value(i).unwrap());
            }
            elems
        }
        _ => vec![],
    };

    match callee.val {
        ValueBase::BuiltinFunction(box (ref info, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            (info.func)(vm, &arg, &callobj);
        }
        ValueBase::Function(box (id, ref iseq, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            call_function(vm, id, iseq, &arg, callobj).unwrap();
        }
        _ => vm.state.stack.push(Value::undefined()),
    }
}

pub fn function_prototype_call(vm: &mut VM, args: &Vec<Value>, callobj: &CallObject) {
    let callee = &*callobj.this;
    let arg_this = args[0].clone();
    match callee.val {
        ValueBase::BuiltinFunction(box (ref info, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            (info.func)(vm, &args[1..].to_vec(), &callobj);
        }
        ValueBase::Function(box (id, ref iseq, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            call_function(vm, id, iseq, &args[1..].to_vec(), callobj).unwrap();
        }
        _ => vm.state.stack.push(Value::undefined()),
    }
}

pub fn require(vm: &mut VM, args: &Vec<Value>, callobj: &CallObject) {
    // TODO: REFINE CODE!!!!
    use ansi_term::Colour;
    use parser;
    use parser::Error::*;
    use std::ffi::CString;
    use std::fs::OpenOptions;
    use std::io::prelude::*;
    use vm_codegen;

    let file_name = match args[0].val {
        ValueBase::String(ref s) => s.to_str().unwrap().clone(),
        _ => panic!(),
    };

    // TODO: Consider better way
    if file_name.starts_with("DLL:") {
        let dylib_path = &file_name[4..];
        let dylib = libloading::Library::new(dylib_path);
        let symbol_name = b"initialize\0";

        match dylib {
            Ok(lib) => {
                let initialize: Result<
                    libloading::Symbol<fn(&mut VM, &Vec<Value>, &CallObject)>,
                    _,
                > = unsafe { lib.get(symbol_name) };
                match initialize {
                    Ok(initialize) => {
                        initialize(vm, args, callobj);
                    }
                    Err(_) => println!("'initialize' needs to be defined in DLL."),
                }
            }
            Err(msg) => println!("{}: {}", msg, dylib_path),
        }

        return;
    }

    let mut file_body = String::new();

    match OpenOptions::new().read(true).open(file_name) {
        Ok(mut ok) => match ok.read_to_string(&mut file_body).ok() {
            Some(x) => x,
            None => {
                eprintln!(
                    "{}: Couldn't read the file '{}'",
                    Colour::Red.bold().paint("error"),
                    file_name,
                );
                return;
            }
        },
        Err(_e) => {
            eprintln!(
                "{}: No such file or directory '{}'",
                Colour::Red.bold().paint("error"),
                file_name,
            );
            return;
        }
    };

    if file_body.len() == 0 {
        return;
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
        | Err(UnexpectedEOF(pos, kind, msg))
        | Err(UnexpectedToken(pos, kind, msg)) => {
            parser.show_error_at(pos, kind, msg.as_str());
            vm.state.stack.push(Value::undefined());
            return;
        }
        Err(UnsupportedFeature(pos)) => {
            parser.enhanced_show_error_at(pos, "unsupported feature");
            vm.state.stack.push(Value::undefined());
            return;
        }
    };

    let mut vm_codegen = vm_codegen::VMCodeGen::new();
    let mut iseq = vec![];
    vm_codegen.bytecode_gen.const_table = vm.const_table.clone();
    vm_codegen.compile(&node, &mut iseq, false);
    vm.const_table = vm_codegen.bytecode_gen.const_table.clone();

    let mut vm = VM::new(vm_codegen.global_varmap);
    vm.const_table = vm_codegen.bytecode_gen.const_table;
    // TODO: Do not unwrap
    vm.run(iseq).unwrap();

    let module_exports =
        unsafe { (**vm.state.scope.last().unwrap()).get_value(&"module".to_string()) }
            .unwrap()
            .get_property(Value::string(CString::new("exports").unwrap()).val, None);
    vm.state.stack.push(module_exports);
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
