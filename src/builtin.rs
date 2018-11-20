use gc;
use vm::{call_function, ArrayValue, CallObject, RawStringPtr, Value, ValueBase, VM};

use libc;
use rand::random;

use rustc_hash::FxHashMap;
use std::ffi::CString;

pub const CONSOLE_LOG: usize = 0;
pub const PROCESS_STDOUT_WRITE: usize = 1;
pub const ARRAY_NEW: usize = 2;
pub const ARRAY_PUSH: usize = 3;
pub const ARRAY_POP: usize = 4;
pub const MATH_FLOOR: usize = 5;
pub const MATH_RANDOM: usize = 6;
pub const MATH_POW: usize = 7;
pub const MATH_ABS: usize = 8;
pub const MATH_ACOS: usize = 9;
pub const MATH_ACOSH: usize = 10;
pub const MATH_ASIN: usize = 11;
pub const MATH_ASINH: usize = 12;
pub const MATH_ATAN: usize = 13;
pub const MATH_ATANH: usize = 14;
pub const MATH_ATAN2: usize = 14;
pub const MATH_CBRT: usize = 16;
pub const MATH_CEIL: usize = 17;
pub const MATH_CLZ32: usize = 17;
pub const MATH_COS: usize = 19;
pub const MATH_COSH: usize = 20;
pub const MATH_EXP: usize = 21;
pub const MATH_EXPM1: usize = 21;
pub const MATH_FROUND: usize = 23;
pub const MATH_HYPOT: usize = 24;
pub const MATH_LOG: usize = 25;
pub const MATH_LOG1P: usize = 25;
pub const MATH_LOG10: usize = 26;
pub const MATH_LOG2: usize = 27;
pub const MATH_MAX: usize = 29;
pub const MATH_MIN: usize = 30;
pub const MATH_ROUND: usize = 31;
pub const MATH_SIGN: usize = 32;
pub const MATH_SIN: usize = 33;
pub const MATH_SINH: usize = 34;
pub const MATH_SQRT: usize = 35;
pub const MATH_TAN: usize = 36;
pub const MATH_TANH: usize = 37;
pub const MATH_TRUNC: usize = 38;
pub const FUNCTION_PROTOTYPE_APPLY: usize = 39;
pub const FUNCTION_PROTOTYPE_CALL: usize = 40;
pub const REQUIRE: usize = 41;

pub unsafe fn console_log(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    let args_len = args.len();
    for i in 0..args_len {
        debug_print(&args[i], false);
        if args_len - 1 != i {
            libc::printf(b" \0".as_ptr() as RawStringPtr);
        }
    }
    libc::puts(b"\0".as_ptr() as RawStringPtr);
    self_.state.stack.push(Value::undefined())
}

pub unsafe fn process_stdout_write(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    let args_len = args.len();
    for i in 0..args_len {
        debug_print(&args[i], false);
        if args_len - 1 != i {
            libc::printf(b" \0".as_ptr() as RawStringPtr);
        }
    }
    self_.state.stack.push(Value::undefined())
}

pub unsafe fn debug_print(val: &Value, nest: bool) {
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

    match val.val {
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
                        }.as_ptr() as RawStringPtr,
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
                    }.as_ptr() as RawStringPtr,
                );

                i += 1;
            }

            show_obj(sorted_key_val);

            libc::printf("]\0".as_ptr() as RawStringPtr);
        }
        ValueBase::Function(_) | ValueBase::BuiltinFunction(_) => {
            libc::printf("[Function]\0".as_ptr() as RawStringPtr);
        }
        _ => {}
    }
}

pub unsafe fn array_new(_callobj: CallObject, args: Vec<Value>, self_: &mut VM) {
    let args_len = args.len();

    if args_len == 0 {
        self_
            .state
            .stack
            .push(Value::array(gc::new(ArrayValue::new(vec![]))));
        gc::mark_and_sweep(&self_.state);
        return;
    }

    let mut elems = vec![];

    match args[0].val {
        ValueBase::Number(length) if args_len == 1 => {
            for _ in 0..length as usize {
                elems.push(Value::empty());
            }
        }
        _ => {
            for arg in args {
                elems.push(arg);
            }
        }
    }

    self_
        .state
        .stack
        .push(Value::array(gc::new(ArrayValue::new(elems))));

    gc::mark_and_sweep(&self_.state);
}

pub unsafe fn array_push(callobj: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let ValueBase::Array(ref map) = callobj.this.val {
        let mut map = &mut **map;
        for val in &args {
            map.elems.push(val.clone());
        }
        map.length += args.len();
        self_.state.stack.push(Value::number(map.length as f64))
    } else {
        self_.state.stack.push(Value::undefined())
    }
}

pub unsafe fn array_pop(callobj: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let ValueBase::Array(ref map) = callobj.this.val {
        let mut map = &mut **map;
        for val in &args {
            map.elems.push(val.clone());
        }
        map.length += args.len();
        self_.state.stack.push(Value::number(map.length as f64))
    } else {
        self_.state.stack.push(Value::undefined())
    }
}

macro_rules! simple_math {
    ($name:ident, $f:ident) => {
        pub unsafe fn $name(_: CallObject, args: Vec<Value>, self_: &mut VM) {
            if let ValueBase::Number(n) = args[0].val {
                return self_.state.stack.push(Value::number(n.$f()));
            }
            self_.state.stack.push(Value::undefined())
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

pub unsafe fn math_atan2(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let ValueBase::Number(n1) = args[0].val {
        if let ValueBase::Number(n2) = args[1].val {
            return self_.state.stack.push(Value::number(n1.atan2(n2)));
        }
        self_.state.stack.push(Value::undefined())
    }
}
simple_math!(math_cbrt, cbrt);
simple_math!(math_ceil, ceil);

pub unsafe fn math_clz32(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let ValueBase::Number(n) = args[0].val {
        return self_.state.stack.push(Value::number(if n == 0.0 {
            32.0
        } else {
            // TODO: >> ? >>> ?
            31.0 - ((n as i32 >> 0) as f64 * ::std::f64::consts::LOG2_E)
                .log(::std::f64::consts::E)
                .floor()
        }));
    }
    self_.state.stack.push(Value::undefined())
}
simple_math!(math_cos, cos);
simple_math!(math_cosh, cosh);
simple_math!(math_exp, exp);
simple_math!(math_expm1, exp_m1);
simple_math!(math_fround, round);

pub unsafe fn math_hypot(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    let mut sum2 = 0.0;
    for n in args {
        if let ValueBase::Number(n) = n.val {
            sum2 += n * n;
        }
    }
    self_.state.stack.push(Value::number(sum2.sqrt()));
}

pub unsafe fn math_log(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let ValueBase::Number(n1) = args[0].val {
        return self_
            .state
            .stack
            .push(Value::number(n1.log(::std::f64::consts::E)));
    }
    self_.state.stack.push(Value::undefined())
}

pub unsafe fn math_log1p(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let ValueBase::Number(n1) = args[0].val {
        return self_
            .state
            .stack
            .push(Value::number(n1.log(1.0 + ::std::f64::consts::E)));
    }
    self_.state.stack.push(Value::undefined())
}

simple_math!(math_log10, log10);
simple_math!(math_log2, log2);

pub unsafe fn math_max(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    let mut max = if let ValueBase::Number(n) = args[0].val {
        n
    } else {
        0.0
    };
    for n in args[1..].iter() {
        if let ValueBase::Number(n) = n.val {
            if n > max {
                max = n;
            }
        }
    }
    self_.state.stack.push(Value::number(max));
}

pub unsafe fn math_min(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    let mut min = if let ValueBase::Number(n) = args[0].val {
        n
    } else {
        0.0
    };
    for n in args[1..].iter() {
        if let ValueBase::Number(n) = n.val {
            if n < min {
                min = n;
            }
        }
    }
    self_.state.stack.push(Value::number(min));
}

simple_math!(math_round, round);

pub unsafe fn math_sign(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let ValueBase::Number(n) = args[0].val {
        return self_.state.stack.push(Value::number(if n == 0.0 {
            n
        } else if n > 0.0 {
            1.0
        } else {
            -1.0
        }));
    }
    self_.state.stack.push(Value::undefined())
}

simple_math!(math_sin, sin);
simple_math!(math_sinh, sinh);
simple_math!(math_sqrt, sqrt);
simple_math!(math_tan, tan);
simple_math!(math_tanh, tanh);
simple_math!(math_trunc, trunc);

pub unsafe fn math_random(_: CallObject, _args: Vec<Value>, self_: &mut VM) {
    self_.state.stack.push(Value::number(random::<f64>()))
}

pub unsafe fn math_pow(_: CallObject, args: Vec<Value>, self_: &mut VM) {
    if let ValueBase::Number(f1) = args[0].val {
        if let ValueBase::Number(f2) = args[1].val {
            return self_.state.stack.push(Value::number(f1.powf(f2)));
        }
    }
    self_.state.stack.push(Value::undefined())
}

pub unsafe fn function_prototype_apply(callobj: CallObject, args: Vec<Value>, self_: &mut VM) {
    let arg_this = args[0].clone();
    let arg = match args[1].val {
        ValueBase::Array(aryval) => {
            let aryval = &*aryval;
            let mut elems = vec![];
            for i in 0..aryval.length {
                elems.push(aryval.elems[i].clone());
            }
            elems
        }
        ValueBase::Arguments => {
            let mut elems = vec![];
            let callobj = &**self_.state.scope.last().unwrap();
            let length = callobj.get_arguments_length();
            for i in 0..length {
                elems.push(callobj.get_arguments_nth_value(i).unwrap());
            }
            elems
        }
        _ => vec![],
    };
    let callee = *callobj.this;

    match callee.val {
        ValueBase::BuiltinFunction(box (id, _, callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            callobj.vals = gc::new(FxHashMap::default());
            self_.builtin_functions[id](callobj, arg, self_);
        }
        ValueBase::Function(box (id, ref iseq, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            callobj.vals = gc::new(FxHashMap::default());
            call_function(self_, id, iseq, arg, callobj).unwrap();
        }
        _ => self_.state.stack.push(Value::undefined()),
    }
}

pub unsafe fn function_prototype_call(callobj: CallObject, args: Vec<Value>, self_: &mut VM) {
    let callee = *callobj.this;
    let arg_this = args[0].clone();
    match callee.val {
        ValueBase::BuiltinFunction(box (id, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            callobj.vals = gc::new(FxHashMap::default());
            self_.builtin_functions[id](callobj, args[1..].to_vec(), self_);
        }
        ValueBase::Function(box (id, ref iseq, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            callobj.vals = gc::new(FxHashMap::default());
            call_function(self_, id, iseq, args[1..].to_vec(), callobj).unwrap();
        }
        _ => self_.state.stack.push(Value::undefined()),
    }
}

pub unsafe fn require(_callobj: CallObject, args: Vec<Value>, self_: &mut VM) {
    // TODO: REFINE CODE!!!!
    use ansi_term::Colour;
    use extract_anony_func;
    use parser;
    use parser::Error::*;
    use std::ffi::CString;
    use std::fs::OpenOptions;
    use std::io::prelude::*;
    use vm;
    use vm_codegen;

    let file_name = match args[0].val {
        ValueBase::String(ref s) => s.to_str().unwrap().clone(),
        _ => panic!(),
    };

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

    let mut node = match parser.parse_all() {
        Ok(ok) => ok,
        Err(NormalEOF) => unreachable!(),
        Err(Expect(pos, kind, msg))
        | Err(UnexpectedEOF(pos, kind, msg))
        | Err(UnexpectedToken(pos, kind, msg)) => {
            parser.show_error_at(pos, kind, msg.as_str());
            self_.state.stack.push(Value::undefined());
            return;
        }
        Err(UnsupportedFeature(pos)) => {
            parser.enhanced_show_error_at(pos, "unsupported feature");
            self_.state.stack.push(Value::undefined());
            return;
        }
    };

    extract_anony_func::AnonymousFunctionExtractor::new().run_toplevel(&mut node);

    let mut vm_codegen = vm_codegen::VMCodeGen::new();
    let mut iseq = vec![];
    vm_codegen.bytecode_gen.const_table = self_.const_table.clone();
    vm_codegen.compile(&node, &mut iseq, false);
    self_.const_table = vm_codegen.bytecode_gen.const_table.clone();

    let mut vm = vm::VM::new(vm_codegen.global_varmap);
    vm.const_table = vm_codegen.bytecode_gen.const_table;
    // TODO: Do not unwrap
    vm.run(iseq).unwrap();

    let module_exports = (**vm.state.scope.last().unwrap())
        .get_value(&"module".to_string())
        .unwrap()
        .get_property(Value::string(CString::new("exports").unwrap()).val, None);
    self_.state.stack.push(module_exports);
}
