use chrono::Utc;
use libc;
use llvm::core::*;
use rustc_hash::FxHashMap;
use std::{ffi::CString, thread, time};

use super::{
    callobj::{CallObject, CallObjectRef},
    error::*,
    task::{Task, TaskManager, TimerKind},
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
    pub op_table: [fn(&mut VM, &ByteCode) -> Result<(), RuntimeError>; 52],
    pub task_mgr: TaskManager,
}

pub struct VMState {
    pub stack: Vec<Value>,
    pub scope: Vec<CallObjectRef>,
    pub pc: isize,
    pub history: Vec<(usize, isize, FuncId)>, // sp, return_pc
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

        let global_vals = unsafe { &mut *global_vals };

        // TODO: Support for 'require' is not enough.
        global_vals.set_value(
            "require".to_string(),
            Value::builtin_function(builtin::require, CallObject::new(Value::undefined())),
        );

        let module_exports = Value::object(gc::new(FxHashMap::default()));
        global_vals.set_value("module".to_string(), {
            make_object!(exports: module_exports.clone())
        });
        global_vals.set_value("exports".to_string(), module_exports);

        unsafe {
            global_vals.set_value("console".to_string(), {
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

        let llvm_process_stdout_write = unsafe {
            LLVMAddFunction(
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
            )
        };

        global_vals.set_value(
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

        global_vals.set_value(
            "setTimeout".to_string(),
            Value::default_builtin_function(builtin::set_timeout),
        );

        global_vals.set_value(
            "setInterval".to_string(),
            Value::default_builtin_function(builtin::set_interval),
        );

        global_vals.set_value(
            "clearInterval".to_string(),
            Value::default_builtin_function(builtin::clear_timer),
        );

        global_vals.set_value(
            "clearTimeout".to_string(),
            Value::default_builtin_function(builtin::clear_timer),
        );

        use builtins::object::OBJECT_OBJ;
        global_vals.set_value("Object".to_string(), OBJECT_OBJ.with(|x| x.clone()));

        use builtins::array::ARRAY_OBJ;
        global_vals.set_value("Array".to_string(), ARRAY_OBJ.with(|x| x.clone()));

        use builtins::function::FUNCTION_OBJ;
        global_vals.set_value("Function".to_string(), FUNCTION_OBJ.with(|x| x.clone()));

        use builtins::date::DATE_OBJ;
        global_vals.set_value("Date".to_string(), DATE_OBJ.with(|x| x.clone()));

        global_vals.set_value(
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
                    let llvm_func = unsafe { LLVMAddFunction(
                        jit.module,
                        CString::new("jit_math_floor").unwrap().as_ptr(),
                      LLVMFunctionType(
                            LLVMDoubleTypeInContext(jit.context),
                            vec![LLVMDoubleTypeInContext(jit.context)]
                                .as_mut_slice().as_mut_ptr(),
                            1, 0
                        )
                    )};
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
                    let llvm_func = unsafe { LLVMAddFunction(
                        jit.module,
                        CString::new("jit_math_random").unwrap().as_ptr(),
                        LLVMFunctionType(
                            LLVMDoubleTypeInContext(jit.context),
                            vec![].as_mut_slice().as_mut_ptr(),
                            0,
                            0,
                        ),
                    )};
                    Value::builtin_function_with_jit(
                        math::math_random,
                        BuiltinJITFuncInfo::Normal {
                            func: math::jit_math_random as *mut libc::c_void,
                            llvm_func                        },
                        CallObject::new(Value::undefined()),
                    )
                },
                pow: {
                    let llvm_func = unsafe { LLVMAddFunction(
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
                    )};
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

        VM {
            jit: jit,
            state: VMState {
                stack: { Vec::with_capacity(128) },
                scope: vec![global_vals],
                history: vec![(0, 0, 0)],
                pc: 0isize,
            },
            const_table: ConstantTable::new(),
            cur_func_id: 0, // 0 is main
            task_mgr: TaskManager::new(),
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
                update_parent_scope,
                get_value,
                set_value,
                decl_var,
                cond_op,
                loop_start,
                throw,
            ],
        }
    }
}

impl VM {
    pub fn run(&mut self, iseq: ByteCode) -> Result<(), RuntimeError> {
        let res = self.do_run(&iseq);

        loop {
            if self.task_mgr.no_tasks() {
                break;
            }

            let now = Utc::now().timestamp_millis();

            while let Some(task) = self.task_mgr.get_task() {
                match task {
                    Task::Timer {
                        ref callback,
                        ref args,
                        kind:
                            TimerKind::Timeout {
                                now: task_now,
                                timeout,
                            },
                        ..
                    } if now - task_now > timeout => {
                        self.call_function_simply(callback, args)?;
                        self.state.stack.pop(); // return value is not used
                    }
                    Task::Timer {
                        id,
                        ref callback,
                        ref args,
                        kind: TimerKind::Interval { previous, interval },
                    } if now - previous > interval => {
                        self.call_function_simply(callback, args)?;
                        self.state.stack.pop(); // return value is not used

                        self.task_mgr.retain_task(Task::Timer {
                            id,
                            callback: callback.clone(),
                            args: args.clone(),
                            kind: TimerKind::Interval {
                                previous: Utc::now().timestamp_millis(),
                                interval,
                            },
                        })
                    }
                    _ => self.task_mgr.retain_task(task),
                }
            }

            thread::sleep(time::Duration::from_millis(1));
        }

        res
    }

    pub fn do_run(&mut self, iseq: &ByteCode) -> Result<(), RuntimeError> {
        loop {
            let code = iseq[self.state.pc as usize];
            self.op_table[code as usize](self, iseq)?;
            if code == VMInst::RETURN || code == VMInst::END {
                break;
            }
        }

        Ok(())
    }

    pub fn set_return_value(&mut self, val: Value) {
        self.state.stack.push(val);
    }

    pub fn call_function_simply(
        &mut self,
        callee: &Value,
        args: &Vec<Value>,
    ) -> Result<(), RuntimeError> {
        match callee.val {
            ValueBase::BuiltinFunction(box (ref info, _, ref callobj)) => {
                (info.func)(self, args, &callobj)
            }
            ValueBase::Function(box (id, ref iseq, _, ref callobj)) => {
                call_function(self, id, iseq, args, callobj.clone())
            }
            ref e => Err(RuntimeError::Type(format!(
                "type error: {:?} is not function",
                e
            ))),
        }
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
            *callobj.this = make_object!(
                __proto__: unsafe { &*obj }
                                .get("prototype")
                                .unwrap_or(&Value::undefined())
                                .clone()
            );

            // https://tc39.github.io/ecma262/#sec-date-constructor
            // > The Date constructor returns a String representing the current time (UTC) when
            // > called as a function rather than as a constructor.
            use builtins::date::{date, date_new};
            (if x.func as *const u8 == date as *const u8 {
                date_new
            } else {
                x.func
            })(self_, &args, &callobj)?;
        }
        ValueBase::Function(box (id, iseq, obj, mut callobj)) => {
            // similar code is used some times. should make it a function.
            let new_this = gc::new(make_hashmap!(
                __proto__: unsafe { &*obj }
                                .get("prototype")
                                .unwrap_or(&Value::undefined())
                                .clone()
            ));

            callobj.clear_args_vals();
            callobj.vals = gc::new(unsafe { (*callobj.vals).clone() });
            callobj.apply_arguments(&args);

            *callobj.this = Value::object(new_this);
            self_.state.scope.push(gc::new(callobj));
            self_
                .state
                .history
                .push((self_.state.stack.len(), self_.state.pc, id));
            self_.state.pc = 0;

            self_.do_run(&iseq)?;

            let ret = self_.state.stack.last_mut().unwrap();
            match &ret.val {
                &ValueBase::Object(_)
                | &ValueBase::Array(_)
                | &ValueBase::Date(_)
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
        (l, r) => Value::string(l.to_string() + r.to_string().as_str()),
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
            Value::string(l.to_str().unwrap().repeat(r as usize))
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
    parent.set_property(member, val, Some(self_.state.scope.last().unwrap()));
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
        .push((self_.state.stack.len(), self_.state.pc, id));
    self_.state.pc = 0;

    self_.do_run(iseq)?;

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

    self_.call_function_simply(&callee, &args)?;

    Ok(())
}

fn unwind_state(vm: &mut VM) {
    let len = vm.state.stack.len();
    if let Some((previous_sp, return_pc, func_id)) = vm.state.history.pop() {
        vm.state.stack.drain(previous_sp..len - 1);
        vm.state.pc = return_pc;
        vm.cur_func_id = func_id;
    } else {
        unreachable!()
    }
}

fn return_(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    unwind_state(self_);
    Ok(())
}

fn throw(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
    let val = self_.state.stack.last().unwrap().clone();
    //TODO:
    // if an exception was thrown in try clause, go to the corresponding catch clause.
    // if in catch or finally clause, go to catch clause of outer try-catch statement.
    // if not in try-catch statement, terminate vm with uncaught exception.
    Err(RuntimeError::Exception(val))
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

fn update_parent_scope(self_: &mut VM, _iseq: &ByteCode) -> Result<(), RuntimeError> {
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

fn get_value(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, iseq, name_id, usize);
    let name = &self_.const_table.string[name_id];
    let val = unsafe { (**self_.state.scope.last().unwrap()).get_value(name)? };
    self_.state.stack.push(val);
    Ok(())
}

fn set_value(self_: &mut VM, iseq: &ByteCode) -> Result<(), RuntimeError> {
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
