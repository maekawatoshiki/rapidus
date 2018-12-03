use libc;
use llvm::core::*;
use rustc_hash::FxHashMap;
use std::ffi::CString;

use super::{
    callobj::{CallObject, CallObjectRef},
    error::*,
    value::{ArrayValue, FuncId, Value, ValueBase},
};
use builtin;
use builtin::BuiltinJITFuncInfo;
use builtins::math;
use bytecode_gen::{ByteCode, VMInst};
use gc;
use jit::TracingJit;

pub struct VM {
    pub jit: TracingJit,
    pub state: VMState,
    pub const_table: ConstantTable,
    pub cur_func_id: FuncId, // id == 0: main
    pub op_table: [fn(&mut VM, &ByteCode) -> Result<(), RuntimeError>; 51],
}

pub struct VMState {
    pub stack: Vec<Value>,
    pub scope: Vec<CallObjectRef>,
    pub pc: isize,
    pub history: Vec<(usize, isize)>, // sp, return_pc
}

#[derive(Debug, Clone)]
pub struct ConstantTable {
    pub value: Vec<Value>,
    pub string: Vec<String>,
}

impl ConstantTable {
    pub fn new() -> ConstantTable {
        ConstantTable {
            value: vec![],
            string: vec![],
        }
    }
}

impl VM {
    pub fn new(global_vals: CallObjectRef) -> VM {
        let jit = unsafe { TracingJit::new() };

        // TODO: Support for 'require' is not enough.
        unsafe {
            (*global_vals).set_value(
                "require".to_string(),
                Value::builtin_function(builtin::require, CallObject::new(Value::undefined())),
            );

            let module_exports = Value::object(gc::new(FxHashMap::default()));
            (*global_vals).set_value("module".to_string(), {
                make_object!(exports: module_exports.clone())
            });
            (*global_vals).set_value("exports".to_string(), module_exports);
        }

        unsafe {
            (*global_vals).set_value("console".to_string(), {
                let mut map = FxHashMap::default();
                map.insert(
                    "log".to_string(),
                    Value::builtin_function_with_jit(
                        builtin::console_log,
                        BuiltinJITFuncInfo::ConsoleLog {
                            bool: (
                                builtin::jit_console_log_bool as *mut libc::c_void,
                                LLVMAddFunction(
                                    jit.module,
                                    CString::new("jit_console_log_bool").unwrap().as_ptr(),
                                    LLVMFunctionType(
                                        LLVMVoidType(),
                                        vec![LLVMInt1TypeInContext(jit.context)]
                                            .as_mut_slice()
                                            .as_mut_ptr(),
                                        1,
                                        0,
                                    ),
                                ),
                            ),
                            f64: (
                                builtin::jit_console_log_f64 as *mut libc::c_void,
                                LLVMAddFunction(
                                    jit.module,
                                    CString::new("jit_console_log_f64").unwrap().as_ptr(),
                                    LLVMFunctionType(
                                        LLVMVoidType(),
                                        vec![LLVMDoubleTypeInContext(jit.context)]
                                            .as_mut_slice()
                                            .as_mut_ptr(),
                                        1,
                                        0,
                                    ),
                                ),
                            ),
                            string: (
                                builtin::jit_console_log_string as *mut libc::c_void,
                                LLVMAddFunction(
                                    jit.module,
                                    CString::new("jit_console_log_string").unwrap().as_ptr(),
                                    LLVMFunctionType(
                                        LLVMVoidType(),
                                        vec![LLVMPointerType(
                                            LLVMInt8TypeInContext(jit.context),
                                            0,
                                        )]
                                        .as_mut_slice()
                                        .as_mut_ptr(),
                                        1,
                                        0,
                                    ),
                                ),
                            ),
                            newline: (
                                builtin::jit_console_log_newline as *mut libc::c_void,
                                LLVMAddFunction(
                                    jit.module,
                                    CString::new("jit_console_log_newline").unwrap().as_ptr(),
                                    LLVMFunctionType(LLVMVoidType(), vec![].as_mut_ptr(), 0, 0),
                                ),
                            ),
                        },
                        CallObject::new(Value::undefined()),
                    ),
                );
                Value::object(gc::new(map))
            });
        }

        unsafe {
            let llvm_process_stdout_write = LLVMAddFunction(
                jit.module,
                CString::new("process_stdout_write").unwrap().as_ptr(),
                LLVMFunctionType(
                    LLVMVoidType(),
                    vec![LLVMPointerType(LLVMInt8TypeInContext(jit.context), 0)]
                        .as_mut_slice()
                        .as_mut_ptr(),
                    1,
                    0,
                ),
            );
            (*global_vals).set_value(
                "process".to_string(),
                make_object!(
                    stdout:
                        make_object!(write:
                            Value::builtin_function_with_jit(
                                builtin::process_stdout_write,
                                BuiltinJITFuncInfo::Normal {
                                    func: builtin::jit_process_stdout_write as *mut libc::c_void,
                                    llvm_func: llvm_process_stdout_write,
                                },
                                CallObject::new(Value::undefined()),
                            )
                        )
                ),
            );
        }

        unsafe {
            use builtins::array::ARRAY_OBJ;
            (*global_vals).set_value("Array".to_string(), ARRAY_OBJ.with(|x| x.clone()));
        }

        unsafe {
            use builtins::function::FUNCTION_OBJ;
            (*global_vals).set_value("Function".to_string(), FUNCTION_OBJ.with(|x| x.clone()));
        }

        unsafe {
            (*global_vals).set_value(
                "Math".to_string(),
                make_object!(
                    PI:     Value::number(::std::f64::consts::PI),
                    abs:    Value::default_builtin_function(math::math_abs),
                    acos:   Value::default_builtin_function(math::math_acos),
                    acosh:  Value::default_builtin_function(math::math_acosh),
                    asin:   Value::default_builtin_function(math::math_asin),
                    asinh:  Value::default_builtin_function(math::math_asinh),
                    atan:   Value::default_builtin_function(math::math_atan),
                    atanh:  Value::default_builtin_function(math::math_atanh),
                    atan2:  Value::default_builtin_function(math::math_atan2),
                    cbrt:   Value::default_builtin_function(math::math_cbrt),
                    ceil:   Value::default_builtin_function(math::math_ceil),
                    clz32:  Value::default_builtin_function(math::math_clz32),
                    cos:    Value::default_builtin_function(math::math_cos),
                    cosh:   Value::default_builtin_function(math::math_cosh),
                    exp:    Value::default_builtin_function(math::math_exp),
                    expm1:  Value::default_builtin_function(math::math_expm1),
                    fround: Value::default_builtin_function(math::math_fround),
                    hypot:  Value::default_builtin_function(math::math_hypot),
                    log:    Value::default_builtin_function(math::math_log),
                    log1p:  Value::default_builtin_function(math::math_log1p),
                    log10:  Value::default_builtin_function(math::math_log10),
                    log2:   Value::default_builtin_function(math::math_log2),
                    max:    Value::default_builtin_function(math::math_max),
                    min:    Value::default_builtin_function(math::math_min),
                    round:  Value::default_builtin_function(math::math_round),
                    sign:   Value::default_builtin_function(math::math_sign),
                    sin:    Value::default_builtin_function(math::math_sin),
                    sinh:   Value::default_builtin_function(math::math_sinh),
                    sqrt:   Value::default_builtin_function(math::math_sqrt),
                    tan:    Value::default_builtin_function(math::math_tan),
                    tanh:   Value::default_builtin_function(math::math_tanh),
                    trunc:  Value::default_builtin_function(math::math_trunc),
                    floor: {
                        let llvm_func = LLVMAddFunction(
                            jit.module,
                            CString::new("jit_math_floor").unwrap().as_ptr(),
                            LLVMFunctionType(
                                LLVMDoubleTypeInContext(jit.context),
                                vec![LLVMDoubleTypeInContext(jit.context)]
                                    .as_mut_slice().as_mut_ptr(),
                                1, 0
                            )
                        );
                        Value::builtin_function_with_jit(
                            math::math_floor,
                            BuiltinJITFuncInfo::Normal {
                                func: math::jit_math_floor as *mut libc::c_void,
                                llvm_func,
                            },
                            CallObject::new(Value::undefined()),
                        )
                    },
                    random: {
                        let llvm_func = LLVMAddFunction(
                            jit.module,
                            CString::new("jit_math_random").unwrap().as_ptr(),
                            LLVMFunctionType(
                                LLVMDoubleTypeInContext(jit.context),
                                vec![].as_mut_slice().as_mut_ptr(),
                                0,
                                0,
                            ),
                        );
                        Value::builtin_function_with_jit(
                            math::math_random,
                            BuiltinJITFuncInfo::Normal {
                                func: math::jit_math_random as *mut libc::c_void,
                                llvm_func                        },
                            CallObject::new(Value::undefined()),
                        )
                    },
                    pow: {
                        let llvm_func = LLVMAddFunction(
                            jit.module,
                            CString::new("jit_math_pow").unwrap().as_ptr(),
                            LLVMFunctionType(
                                LLVMDoubleTypeInContext(jit.context),
                                vec![
                                    LLVMDoubleTypeInContext(jit.context),
                                    LLVMDoubleTypeInContext(jit.context),
                                ].as_mut_slice()
                                    .as_mut_ptr(),
                                2,
                                0,
                            ),
                        );
                        Value::builtin_function_with_jit(
                            math::math_pow,
                            BuiltinJITFuncInfo::Normal {
                                func: math::jit_math_pow as *mut libc::c_void,
                                llvm_func
                            },
                            CallObject::new(Value::undefined()),
                        )
                    }
                ),
            );
        }

        VM {
            jit: jit,
            state: VMState {
                stack: { Vec::with_capacity(128) },
                scope: vec![global_vals],
                history: {
                    let mut s = Vec::with_capacity(128);
                    s.push((0, 0));
                    s
                },
                pc: 0isize,
            },
            const_table: ConstantTable::new(),
            cur_func_id: 0, // 0 is main
            op_table: [
                end,
                create_context,
                construct,
                create_object,
                create_array,
                push_int8,
                push_int32,
                push_false,
                push_true,
                push_const,
                push_this,
                push_arguments,
                push_undefined,
                lnot,
                posi,
                neg,
                add,
                sub,
                mul,
                div,
                rem,
                lt,
                gt,
                le,
                ge,
                eq,
                ne,
                seq,
                sne,
                and,
                or,
                xor,
                shl,
                shr,
                zfshr,
                get_member,
                set_member,
                jmp_if_false,
                jmp,
                call,
                return_,
                double,
                pop,
                land,
                lor,
                set_cur_callobj,
                get_name,
                set_name,
                decl_var,
                cond_op,
                loop_start,
            ],
        }
    }
}

impl VM {
    pub fn run(&mut self, iseq: ByteCode) -> Result<(), RuntimeError> {
        // self.iseq = iseq;
        // Unlock the mutex and start the profiler
        // PROFILER
        //     .lock()
        //     .unwrap()
        //     .start("./my-prof.profile")
        //     .expect("Couldn't start");

        self.do_run(&iseq)

        // Unwrap the mutex and stop the profiler
        // PROFILER.lock().unwrap().stop().expect("Couldn't stop");
    }

    pub fn do_run(&mut self, iseq: &ByteCode) -> Result<(), RuntimeError> {
        // let id = self.cur_func_id;
        loop {
            let code = iseq[self.state.pc as usize];
            self.op_table[code as usize](self, iseq)?;
            if code == VMInst::RETURN || code == VMInst::END {
                break;
            }
            // println!("stack trace: {:?} - {}", self.stack, *pc);
        }

        Ok(())
    }
}

macro_rules! get_int8 {
    ($self:ident, $iseq:ident, $var:ident, $ty:ty) => {
        let $var = $iseq[$self.state.pc as usize] as $ty;
        $self.state.pc += 1;
    };
}

macro_rules! get_int32 {
    ($self:ident, $iseq:ident, $var:ident, $ty:ty) => {
        let $var = (($iseq[$self.state.pc as usize + 3] as $ty) << 24)
            + (($iseq[$self.state.pc as usize + 2] as $ty) << 16)
            + (($iseq[$self.state.pc as usize + 1] as $ty) << 8)
            + ($iseq[$self.state.pc as usize + 0] as $ty);
        $self.state.pc += 4;
    };
}

fn end(_self: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    Ok(())
}

fn create_context(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // create_context
    Ok(())
}

fn construct(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // construct
    get_int32!(self_, iseq, argc, usize);

    let callee = self_.state.stack.pop().unwrap();

    let mut args = vec![];
    for _ in 0..argc {
        args.push(self_.state.stack.pop().unwrap());
    }

    match callee.val.clone() {
        ValueBase::BuiltinFunction(box (x, obj, mut callobj)) => {
            let new_this = {
                let mut map = FxHashMap::default();
                map.insert("__proto__".to_string(), unsafe {
                    (*obj)
                        .get("prototype")
                        .unwrap_or(&Value::undefined())
                        .clone()
                });
                gc::new(map)
            };

            *callobj.this = Value::object(new_this);

            (x.func)(self_, &args, &callobj);
        }
        ValueBase::Function(box (id, iseq, obj, mut callobj)) => {
            // similar code is used some times. should make it a function.
            let new_this = {
                let mut map = FxHashMap::default();
                map.insert("__proto__".to_string(), unsafe {
                    (*obj)
                        .get("prototype")
                        .unwrap_or(&Value::undefined())
                        .clone()
                });
                gc::new(map)
            };

            callobj.clear_args_vals();
            callobj.vals = gc::new(unsafe { (*callobj.vals).clone() });
            callobj.apply_arguments(&args);

            *callobj.this = Value::object(new_this);
            self_.state.scope.push(gc::new(callobj));
            self_
                .state
                .history
                .push((self_.state.stack.len(), self_.state.pc));
            self_.state.pc = 0;
            let save_id = self_.cur_func_id;
            self_.cur_func_id = id;

            self_.do_run(&iseq)?;

            self_.cur_func_id = save_id;
            self_.state.scope.pop();

            let ret = self_.state.stack.last_mut().unwrap();
            match &ret.val {
                &ValueBase::Object(_)
                | &ValueBase::Array(_)
                | &ValueBase::Function(_)
                | &ValueBase::BuiltinFunction(_) => {}
                _ => *ret = Value::object(new_this),
            };
        }
        c => {
            return Err(RuntimeError::Type(format!(
                "type error(pc:{}): '{:?}' is not a constructor",
                self_.state.pc, c
            )));
        }
    };

    Ok(())
}

fn create_object(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // create_object
    get_int32!(self_, iseq, len, usize);

    let mut map = FxHashMap::default();
    for _ in 0..len {
        let name = if let ValueBase::String(name) = self_.state.stack.pop().unwrap().val {
            name.into_string().unwrap()
        } else {
            unreachable!()
        };
        let val = self_.state.stack.pop().unwrap();
        map.insert(name, val.clone());
    }

    self_.state.stack.push(Value::object(gc::new(map)));

    gc::mark_and_sweep(&self_.state);

    Ok(())
}

fn create_array(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // create_array
    get_int32!(self_, iseq, len, usize);

    let mut arr = vec![];
    for _ in 0..len {
        let val = self_.state.stack.pop().unwrap();
        arr.push(val);
    }

    self_
        .state
        .stack
        .push(Value::array(gc::new(ArrayValue::new(arr))));

    gc::mark_and_sweep(&self_.state);

    Ok(())
}

fn push_int8(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // push_int
    get_int8!(self_, iseq, n, i8);
    self_.state.stack.push(Value::number(n as f64));
    Ok(())
}

fn push_int32(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // push_int
    get_int32!(self_, iseq, n, i32);
    self_.state.stack.push(Value::number(n as f64));
    Ok(())
}

fn push_false(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // push_false
    self_.state.stack.push(Value::bool(false));
    Ok(())
}

fn push_true(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // push_true
    self_.state.stack.push(Value::bool(true));
    Ok(())
}

fn push_const(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // push_const
    get_int32!(self_, iseq, n, usize);
    self_.state.stack.push(self_.const_table.value[n].clone());
    Ok(())
}

fn push_this(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // push_this
    let this = unsafe { *(**self_.state.scope.last().unwrap()).this.clone() };
    self_.state.stack.push(this);
    Ok(())
}

fn push_arguments(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // push_arguments
    self_.state.stack.push(Value::arguments());
    Ok(())
}

fn push_undefined(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // push_defined
    self_.state.stack.push(Value::undefined());
    Ok(())
}

fn lnot(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // lnot
    let expr = self_.state.stack.last_mut().unwrap();
    expr.val = ValueBase::Bool(!expr.val.to_boolean());
    Ok(())
}

fn posi(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // posi
    let expr = self_.state.stack.last_mut().unwrap();
    expr.val = ValueBase::Number(expr.val.to_number());
    Ok(())
}

fn neg(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // neg
    let expr = self_.state.stack.last_mut().unwrap();
    match &mut expr.val {
        &mut ValueBase::Number(ref mut n) => *n = -*n,
        _ => return Err(RuntimeError::Unimplemented),
    };
    Ok(())
}

fn add(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number(l + r),
        (ValueBase::Bool(false), ValueBase::Number(x))
        | (ValueBase::Number(x), ValueBase::Bool(false)) => Value::number(x),
        (ValueBase::Bool(true), ValueBase::Number(x))
        | (ValueBase::Number(x), ValueBase::Bool(true)) => Value::number(x + 1.0),
        // TODO: We need the correct implementation.
        (ValueBase::Undefined, _) | (_, ValueBase::Undefined) => Value::number(::std::f64::NAN),
        (l, r) => Value::string(CString::new(l.to_string() + r.to_string().as_str()).unwrap()),
    });
    Ok(())
}

fn sub(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number(l - r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(())
}

fn mul(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number(l * r),
        (ValueBase::String(l), ValueBase::Number(r)) => {
            Value::string(CString::new(l.to_str().unwrap().repeat(r as usize)).unwrap())
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(())
}

fn div(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number(l / r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(())
}

fn rem(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::number((l as i64 % r as i64) as f64),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(())
}

fn lt(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::bool(l < r),
        (ValueBase::String(l), ValueBase::String(r)) => Value::bool(l < r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(())
}

fn gt(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::bool(l > r),
        (ValueBase::String(l), ValueBase::String(r)) => Value::bool(l > r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(())
}

fn le(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::bool(l <= r),
        (ValueBase::String(l), ValueBase::String(r)) => Value::bool(l <= r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(())
}

fn ge(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => Value::bool(l >= r),
        (ValueBase::String(l), ValueBase::String(r)) => Value::bool(l >= r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(())
}

// TODO: Need more precise implemention
fn eq(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_
        .state
        .stack
        .push(Value::bool(lhs.val.abstract_equal(rhs.val)?));
    Ok(())
}

// TODO: Need more precise implemention
fn ne(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_
        .state
        .stack
        .push(Value::bool(lhs.val.abstract_equal(rhs.val)?));
    Ok(())
}

// TODO: Need more precise implemention
fn seq(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_
        .state
        .stack
        .push(Value::bool(lhs.val.strict_equal(rhs.val)?));
    Ok(())
}

// TODO: Need more precise implemention
fn sne(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_
        .state
        .stack
        .push(Value::bool(!lhs.val.strict_equal(rhs.val)?));
    Ok(())
}

fn and(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => {
            Value::number(((l as i64 as i32) & (r as i64 as i32)) as f64)
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(())
}

fn or(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => {
            Value::number(((l as i64 as i32) | (r as i64 as i32)) as f64)
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(())
}

fn xor(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => {
            Value::number(((l as i64 as i32) ^ (r as i64 as i32)) as f64)
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(())
}

fn shl(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => {
            Value::number(((l as i64 as i32) << (r as i64 as i32)) as f64)
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(())
}

fn shr(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => {
            Value::number(((l as i64 as i32) >> (r as i64 as i32)) as f64)
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(())
}

fn zfshr(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs.val, rhs.val) {
        (ValueBase::Number(l), ValueBase::Number(r)) => {
            Value::number(((l as u64 as u32) >> (r as u64 as u32)) as f64)
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(())
}

fn get_member(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // get_global
    let member = self_.state.stack.pop().unwrap();
    let parent = self_.state.stack.pop().unwrap();
    let val = parent.get_property(member.val, Some(self_.state.scope.last().unwrap()));
    self_.state.stack.push(val);
    Ok(())
}

fn set_member(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // get_global
    let member = self_.state.stack.pop().unwrap();
    let parent = self_.state.stack.pop().unwrap();
    let val = self_.state.stack.pop().unwrap();
    // TODO: The following code should be a function (like Value::set_property).
    match parent.val {
        ValueBase::Object(map)
        | ValueBase::Function(box (_, _, map, _))
        | ValueBase::BuiltinFunction(box (_, map, _)) => unsafe {
            *(*map)
                .entry(member.to_string())
                .or_insert_with(|| Value::undefined()) = val;
        },
        ValueBase::Array(map) => unsafe {
            fn set_by_idx(map: &mut ArrayValue, n: usize, val: Value) {
                if n >= map.length as usize {
                    map.length = n + 1;
                    while map.elems.len() < n + 1 {
                        map.elems.push(Value::empty());
                    }
                }
                map.elems[n] = val;
            };

            let mut map = &mut *map;

            match member.val {
                // Index
                ValueBase::Number(n) if n - n.floor() == 0.0 && n >= 0.0 => {
                    set_by_idx(map, n as usize, val)
                }
                ValueBase::String(ref s) if s.to_str().unwrap() == "length" => match val.val {
                    ValueBase::Number(n) if n - n.floor() == 0.0 && n >= 0.0 => {
                        map.length = n as usize;
                        while map.elems.len() < n as usize + 1 {
                            map.elems.push(Value::empty());
                        }
                    }
                    _ => {}
                },
                // https://www.ecma-international.org/ecma-262/9.0/index.html#sec-array-exotic-objects
                ValueBase::String(ref s)
                    if Value::number(member.val.to_uint32()).to_string() == s.to_str().unwrap() =>
                {
                    let num = member.val.to_uint32();
                    set_by_idx(map, num as usize, val)
                }
                _ => {
                    *map.obj
                        .entry(member.to_string())
                        .or_insert_with(|| Value::undefined()) = val
                }
            }
        },
        ValueBase::Arguments => {
            match member.val {
                // Index
                ValueBase::Number(n) if n - n.floor() == 0.0 => unsafe {
                    (**self_.state.scope.last().unwrap()).set_arguments_nth_value(n as usize, val);
                },
                // TODO: 'length'
                _ => {}
            }
        }
        _ => {}
    };

    Ok(())
}

fn jmp(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // jmp
    get_int32!(self_, iseq, dst, i32);
    self_.state.pc += dst as isize;
    Ok(())
}

fn jmp_if_false(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // jmp_if_false
    get_int32!(self_, iseq, dst, i32);
    let cond = self_.state.stack.pop().unwrap();
    if let ValueBase::Bool(false) = cond.val {
        self_.state.pc += dst as isize
    }
    Ok(())
}

pub fn call_function(
    self_: &mut VM,
    id: FuncId,
    iseq: &ByteCode,
    args: &Vec<Value>,
    mut callobj: CallObject,
) -> Result<(), RuntimeError> {
    let argc = args.len();
    let args_all_numbers = args.iter().all(|Value { val, .. }| match val {
        ValueBase::Number(_) => true,
        _ => false,
    });

    callobj.clear_args_vals();
    callobj.vals = gc::new(unsafe { (*callobj.vals).clone() });
    callobj.apply_arguments(args);

    self_.state.scope.push(gc::new(callobj));

    if args_all_numbers {
        let scope = (*self_.state.scope.last().unwrap()).clone();
        if let Some(f) = unsafe {
            self_
                .jit
                .can_jit(id, iseq, &*scope, &self_.const_table, argc)
        } {
            self_
                .state
                .stack
                .push(unsafe { self_.jit.run_llvm_func(id, f, &args) });
            self_.state.scope.pop();
            return Ok(());
        }
    }

    self_
        .state
        .history
        .push((self_.state.stack.len(), self_.state.pc));
    self_.state.pc = 0;

    let save_id = self_.cur_func_id;
    self_.cur_func_id = id;

    self_.do_run(iseq)?;

    self_.cur_func_id = save_id;
    self_.state.scope.pop();

    self_
        .jit
        .record_function_return_type(id, self_.state.stack.last().unwrap());

    Ok(())
}

fn call(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // Call
    get_int32!(self_, iseq, argc, usize);

    let callee = self_.state.stack.pop().unwrap();

    let mut args = vec![];
    for _ in 0..argc {
        args.push(self_.state.stack.pop().unwrap());
    }

    match callee.val {
        ValueBase::BuiltinFunction(box (ref info, _, ref callobj)) => {
            (info.func)(self_, &args, callobj);
        }
        ValueBase::Function(box (id, ref iseq, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            call_function(self_, id, iseq, &args, callobj)?;
        }
        c => {
            return Err(RuntimeError::Type(format!(
                "type error(pc:{}): '{:?}' is not a function but called",
                self_.state.pc, c
            )));
        }
    };

    Ok(())
}

fn return_(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    let len = self_.state.stack.len();
    if let Some((previous_sp, return_pc)) = self_.state.history.pop() {
        self_.state.stack.drain(previous_sp..len - 1);
        self_.state.pc = return_pc;
    } else {
        unreachable!()
    }
    Ok(())
}

fn double(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // double
    let stack_top_val = self_.state.stack.last().unwrap().clone();
    self_.state.stack.push(stack_top_val);
    Ok(())
}

fn pop(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // double
    self_.state.stack.pop();
    Ok(())
}

// 'land' and 'lor' are for JIT compiler. Nope for VM.

fn land(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // land
    Ok(())
}

fn lor(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1; // lor
    Ok(())
}

fn set_cur_callobj(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1;
    if let Some(Value {
        val: ValueBase::Function(box (_, _, _, ref mut callobj)),
        ..
    }) = self_.state.stack.last_mut()
    {
        callobj.parent = Some(self_.state.scope.last().unwrap().clone());
    }
    Ok(())
}

fn get_name(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, iseq, name_id, usize);
    let name = &self_.const_table.string[name_id];
    let val = unsafe { (**self_.state.scope.last().unwrap()).get_value(name)? };
    self_.state.stack.push(val);
    Ok(())
}

fn set_name(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, iseq, name_id, usize);
    let name = self_.const_table.string[name_id].clone();
    let mut val = self_.state.stack.pop().unwrap();

    // We have to change cobj.this to the current scope one. (./examples/this.js)
    if let ValueBase::Function(box (_, _, _, ref mut cobj))
    | ValueBase::BuiltinFunction(box (_, _, ref mut cobj)) = &mut val.val
    {
        unsafe {
            cobj.this = (**self_.state.scope.last().unwrap()).this.clone();
        }
    }

    unsafe { (**self_.state.scope.last().unwrap()).set_value_if_exist(name, val) };

    Ok(())
}

fn decl_var(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, iseq, name_id, usize);
    let name = self_.const_table.string[name_id].clone();
    unsafe { (**self_.state.scope.last().unwrap()).set_value(name, Value::undefined()) };
    Ok(())
}

// 'cond_op' is for JIT compiler. Nope for VM.
fn cond_op(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1;
    Ok(())
}

fn loop_start(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
    let loop_start = self_.state.pc as usize;

    self_.state.pc += 1;
    get_int32!(self_, iseq, relatinal_loop_end, usize);
    let loop_end = loop_start + relatinal_loop_end;

    let id = self_.cur_func_id;

    if let Some(pc) = unsafe {
        self_.jit.can_loop_jit(
            id,
            &iseq,
            &self_.const_table,
            &mut self_.state,
            loop_start,
            loop_end,
        )
    } {
        self_.state.pc = pc;
    }

    Ok(())
}
