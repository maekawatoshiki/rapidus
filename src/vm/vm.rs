use chrono::Utc;
use libc;
use llvm::core::*;
use rustc_hash::FxHashMap;
use std::{ffi::CString, thread, time};

use super::{
    callobj::{CallObject, CallObjectRef},
    error::*,
    task::{Task, TaskManager, TimerKind},
    value::{FuncId, Property, Value},
};
use builtin;
use builtin::BuiltinJITFuncInfo;
use builtins;
//use bytecode_gen;
use bytecode_gen::ByteCode;
use gc;
use jit::TracingJit;

pub struct VM {
    pub jit: TracingJit,
    pub state: VMState,
    pub trystate_stack: Vec<TryState>,
    pub const_table: ConstantTable,
    pub cur_func_id: FuncId, // id == 0: main
    pub op_table: [fn(&mut VM, &ByteCode) -> Result<bool, RuntimeError>; 59],
    pub task_mgr: TaskManager,
}

pub struct VMState {
    pub stack: Vec<Value>,
    pub scope: Vec<CallObjectRef>,
    pub pc: isize,
    pub history: Vec<(usize, isize, FuncId)>, // sp, return_pc
}

#[derive(Clone, Debug, PartialEq)]
pub enum TryReturn {
    Value(Value),
    Error(RuntimeError),
    None,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TryState {
    Try(isize, isize, TryReturn), //position of (CATCH, FINALLY)
    Catch(isize, TryReturn),      //postion of (FINALLY)
    Finally(TryReturn),
    None,
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
            Value::default_builtin_function(builtin::require),
        );

        let module_exports = Value::object(gc::new(FxHashMap::default()));
        global_vals.set_value("module".to_string(), {
            Value::object_from_nvp(&vec![("exports", module_exports.clone())])
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
                    )
                    .to_property(),
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
            Value::object_from_nvp(&vec![(
                "stdout",
                Value::object_from_nvp(&vec![(
                    "write",
                    Value::builtin_function_with_jit(
                        builtin::process_stdout_write,
                        BuiltinJITFuncInfo::Normal {
                            func: builtin::jit_process_stdout_write as *mut libc::c_void,
                            llvm_func: llvm_process_stdout_write,
                        },
                    ),
                )]),
            )]),
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

        global_vals.set_value("Error".to_string(), builtins::error::init());

        use builtins::array::ARRAY_OBJ;
        global_vals.set_value("Array".to_string(), ARRAY_OBJ.with(|x| x.clone()));

        global_vals.set_value("Function".to_string(), builtins::function::init());

        use builtins::date::DATE_OBJ;
        global_vals.set_value("Date".to_string(), DATE_OBJ.with(|x| x.clone()));

        global_vals.set_value("Math".to_string(), builtins::math::init(jit.clone()));

        VM {
            jit: jit,
            state: VMState {
                stack: { Vec::with_capacity(128) },
                scope: vec![global_vals],
                history: vec![(0, 0, 0)],
                pc: 0isize,
            },
            trystate_stack: vec![TryState::None],
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
                enter_try,
                leave_try,
                catch,
                finally,
                return_try,
                push_scope,
                pop_scope,
            ],
        }
    }
}

impl VM {
    pub fn run(&mut self, iseq: ByteCode) -> Result<bool, RuntimeError> {
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

    fn do_run(&mut self, iseq: &ByteCode) -> Result<bool, RuntimeError> {
        self.trystate_stack.push(TryState::None);

        loop {
            let code = iseq[self.state.pc as usize];
            //bytecode_gen::show_inst(iseq, self.state.pc as usize);
            let res = self.op_table[code as usize](self, iseq);

            if res == Ok(false) {
                // END or RETURN or throw occurs outer try-catch.
                break;
            }

            let mut error: Option<RuntimeError> = None;
            if let Err(err) = res {
                let trystate = self.trystate_stack.last_mut().unwrap();
                match trystate.clone() {
                    TryState::Try(to_catch, to_finally, ret) => {
                        self.state.pc = to_catch;
                        // push error object to exec stack.
                        let err_obj = match err {
                            RuntimeError::Exception(v) => v,
                            RuntimeError::Type(s) => Value::string(s),
                            RuntimeError::General(s) => Value::string(s),
                            RuntimeError::Reference(s) => Value::string(s),
                            RuntimeError::Unimplemented => {
                                Value::string("Unimplemented".to_string())
                            }
                            RuntimeError::Unknown => Value::string("Unknown".to_string()),
                        };
                        self.state.stack.push(err_obj);
                        *trystate = TryState::Catch(to_finally, ret);
                    }
                    TryState::Catch(to_finally, ret) => {
                        assert_eq!(ret, TryReturn::None);
                        self.state.pc = to_finally;
                        *trystate = TryState::Finally(TryReturn::Error(err));
                        self.state.scope.pop();
                    }
                    TryState::None | TryState::Finally(_) => {
                        error = Some(err);
                    }
                };
            }

            if let Some(err) = error {
                match self.trystate_stack.pop().unwrap() {
                    TryState::Finally(_) => {}
                    x => self.trystate_stack.push(x),
                }
                self.trystate_stack.pop().unwrap();
                // must push return value to exec stack.
                self.state.stack.push(Value::undefined());
                unwind_state(self);
                return Err(err);
            }
        }

        self.trystate_stack.pop().unwrap();

        Ok(true)
    }

    pub fn set_return_value(&mut self, val: Value) {
        self.state.stack.push(val);
    }

    pub fn call_function_simply(
        &mut self,
        callee: &Value,
        args: &Vec<Value>,
    ) -> Result<bool, RuntimeError> {
        match callee {
            Value::BuiltinFunction(box (ref info, _, ref callobj)) => {
                (info.func)(self, args, &callobj)?;
                return Ok(true);
            }
            Value::Function(box (id, ref iseq, _, ref callobj)) => {
                call_function(self, *id, iseq, args, callobj.clone())
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

fn end(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.stack.push(Value::Undefined);
    unwind_state(self_);
    Ok(false)
}

fn create_context(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // create_context
    Ok(true)
}

fn construct(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // construct
    get_int32!(self_, iseq, argc, usize);

    let callee = self_.state.stack.pop().unwrap();

    let mut args = vec![];
    for _ in 0..argc {
        args.push(self_.state.stack.pop().unwrap());
    }

    match callee.clone() {
        Value::BuiltinFunction(box (ref x, ref obj, ref mut callobj)) => {
            *callobj.this = Value::object_from_nvp(&vec![("__proto__", unsafe {
                (**obj)
                    .get("prototype")
                    .unwrap_or(&Property::new(Value::undefined()))
                    .val
                    .clone()
            })]);

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
        Value::Function(box (id, iseq, obj, mut callobj)) => {
            // similar code is used some times. should make it a function.
            let new_this = unsafe {
                Value::object_from_nvp(&vec![(
                    "__proto__",
                    (*obj)
                        .get("prototype")
                        .unwrap_or(&Property::new(Value::undefined()))
                        .val
                        .clone(),
                )])
            };
            callobj.clear_args_vals();
            callobj.vals = gc::new(unsafe { (*callobj.vals).clone() });
            callobj.apply_arguments(&args);

            *callobj.this = new_this.clone();
            self_.state.scope.push(gc::new(callobj));
            self_
                .state
                .history
                .push((self_.state.stack.len(), self_.state.pc, id));
            self_.state.pc = 0;

            let res = self_.do_run(&iseq);

            self_.state.scope.pop();
            let ret = self_.state.stack.last_mut().unwrap();
            match &ret {
                &Value::Object(_)
                | &Value::Array(_)
                | &Value::Date(_)
                | &Value::Function(_)
                | &Value::BuiltinFunction(_) => {}
                _ => *ret = new_this,
            };
            return res;
        }
        c => {
            return Err(RuntimeError::Type(format!(
                "type error(pc:{}): '{:?}' is not a constructor",
                self_.state.pc, c
            )));
        }
    };

    Ok(true)
}

fn create_object(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // create_object
    get_int32!(self_, iseq, len, usize);

    let mut map = FxHashMap::default();
    for _ in 0..len {
        let name = if let Value::String(name) = self_.state.stack.pop().unwrap() {
            name.into_string().unwrap()
        } else {
            unreachable!()
        };
        let val = self_.state.stack.pop().unwrap();
        map.insert(name, Property::new(val.clone()));
    }

    self_.state.stack.push(Value::object(gc::new(map)));

    gc::mark_and_sweep(&self_.state);

    Ok(true)
}

fn create_array(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // create_array
    get_int32!(self_, iseq, len, usize);

    let mut arr = vec![];
    for _ in 0..len {
        let val = self_.state.stack.pop().unwrap();
        arr.push(val);
    }

    self_.state.stack.push(Value::array_from_elems(arr));

    gc::mark_and_sweep(&self_.state);

    Ok(true)
}

fn push_int8(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_int
    get_int8!(self_, iseq, n, i8);
    self_.state.stack.push(Value::number(n as f64));
    Ok(true)
}

fn push_int32(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_int
    get_int32!(self_, iseq, n, i32);
    self_.state.stack.push(Value::number(n as f64));
    Ok(true)
}

fn push_false(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_false
    self_.state.stack.push(Value::bool(false));
    Ok(true)
}

fn push_true(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_true
    self_.state.stack.push(Value::bool(true));
    Ok(true)
}

fn push_const(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_const
    get_int32!(self_, iseq, n, usize);
    self_.state.stack.push(self_.const_table.value[n].clone());
    Ok(true)
}

fn push_this(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_this
    let this = unsafe { *(**self_.state.scope.last().unwrap()).this.clone() };
    self_.state.stack.push(this);
    Ok(true)
}

fn push_arguments(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_arguments
    self_.state.stack.push(Value::arguments());
    Ok(true)
}

fn push_undefined(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_defined
    self_.state.stack.push(Value::undefined());
    Ok(true)
}

fn lnot(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // lnot
    let expr = self_.state.stack.last_mut().unwrap();
    *expr = Value::Bool(!expr.to_boolean());
    Ok(true)
}

fn posi(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // posi
    let expr = self_.state.stack.last_mut().unwrap();
    *expr = Value::Number(expr.to_number());
    Ok(true)
}

fn neg(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // neg
    let expr = self_.state.stack.last_mut().unwrap();
    *expr = match *expr {
        Value::Number(n) => Value::number(-n),
        _ => return Err(RuntimeError::Unimplemented),
    };
    Ok(true)
}

fn add(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::number(l + r),
        (Value::Bool(false), Value::Number(x)) | (Value::Number(x), Value::Bool(false)) => {
            Value::number(x)
        }
        (Value::Bool(true), Value::Number(x)) | (Value::Number(x), Value::Bool(true)) => {
            Value::number(x + 1.0)
        }
        // TODO: We need the correct implementation.
        (Value::Undefined, _) | (_, Value::Undefined) => Value::number(::std::f64::NAN),
        (l, r) => Value::string(l.to_string() + r.to_string().as_str()),
    });
    Ok(true)
}

fn sub(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::number(l - r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn mul(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::number(l * r),
        (Value::String(l), Value::Number(r)) => {
            Value::string(l.to_str().unwrap().repeat(r as usize))
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn div(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::number(l / r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn rem(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::number((l as i64 % r as i64) as f64),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn lt(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::bool(l < r),
        (Value::String(l), Value::String(r)) => Value::bool(l < r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn gt(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::bool(l > r),
        (Value::String(l), Value::String(r)) => Value::bool(l > r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn le(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::bool(l <= r),
        (Value::String(l), Value::String(r)) => Value::bool(l <= r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn ge(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::bool(l >= r),
        (Value::String(l), Value::String(r)) => Value::bool(l >= r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

// TODO: Need more precise implemention
fn eq(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_
        .state
        .stack
        .push(Value::bool(lhs.abstract_equal(rhs)?));
    Ok(true)
}

// TODO: Need more precise implemention
fn ne(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_
        .state
        .stack
        .push(Value::bool(lhs.abstract_equal(rhs)?));
    Ok(true)
}

// TODO: Need more precise implemention
fn seq(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(Value::bool(lhs.strict_equal(rhs)?));
    Ok(true)
}

// TODO: Need more precise implemention
fn sne(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(Value::bool(!lhs.strict_equal(rhs)?));
    Ok(true)
}

fn and(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => {
            Value::number(((l as i64 as i32) & (r as i64 as i32)) as f64)
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn or(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => {
            Value::number(((l as i64 as i32) | (r as i64 as i32)) as f64)
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn xor(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => {
            Value::number(((l as i64 as i32) ^ (r as i64 as i32)) as f64)
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn shl(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => {
            Value::number(((l as i64 as i32) << (r as i64 as i32)) as f64)
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn shr(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => {
            Value::number(((l as i64 as i32) >> (r as i64 as i32)) as f64)
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn zfshr(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => {
            Value::number(((l as u64 as u32) >> (r as u64 as u32)) as f64)
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn get_member(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // get_global
    let member = self_.state.stack.pop().unwrap();
    let parent = self_.state.stack.pop().unwrap();
    let val = parent.get_property(member, Some(self_.state.scope.last().unwrap()));
    self_.state.stack.push(val);
    Ok(true)
}

fn set_member(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // get_global
    let member = self_.state.stack.pop().unwrap();
    let parent = self_.state.stack.pop().unwrap();
    let val = self_.state.stack.pop().unwrap();
    parent.set_property(member, val, Some(self_.state.scope.last().unwrap()));
    Ok(true)
}

fn jmp(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // jmp
    get_int32!(self_, iseq, dst, i32);
    self_.state.pc += dst as isize;
    Ok(true)
}

fn jmp_if_false(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // jmp_if_false
    get_int32!(self_, iseq, dst, i32);
    let cond = self_.state.stack.pop().unwrap();
    if let Value::Bool(false) = cond {
        self_.state.pc += dst as isize
    }
    Ok(true)
}

pub fn call_function(
    self_: &mut VM,
    id: FuncId,
    iseq: &ByteCode,
    args: &Vec<Value>,
    mut callobj: CallObject,
) -> Result<bool, RuntimeError> {
    let argc = args.len();
    let args_all_numbers = args.iter().all(|val| match val {
        Value::Number(_) => true,
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
            return Ok(true);
        }
    }

    self_
        .state
        .history
        .push((self_.state.stack.len(), self_.state.pc, id));
    self_.state.pc = 0;

    let res = self_.do_run(iseq);

    self_.state.scope.pop();
    self_
        .jit
        .record_function_return_type(id, self_.state.stack.last().unwrap());

    res
}

fn call(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // Call
    get_int32!(self_, iseq, argc, usize);

    let callee = self_.state.stack.pop().unwrap();

    let mut args = vec![];
    for _ in 0..argc {
        args.push(self_.state.stack.pop().unwrap());
    }

    self_.call_function_simply(&callee, &args)?;

    Ok(true)
}

fn unwind_state(vm: &mut VM) {
    //let len = vm.state.stack.len();
    if let Some((previous_sp, return_pc, func_id)) = vm.state.history.pop() {
        vm.state.stack.truncate(previous_sp + 1);
        vm.state.pc = return_pc;
        vm.cur_func_id = func_id;
    } else {
        unreachable!("history stack exhaust.")
    }
}

fn return_(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    unwind_state(self_);
    Ok(false)
}

fn throw(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    let val = self_.state.stack.last().unwrap().clone();
    Err(RuntimeError::Exception(val))
}

fn enter_try(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    let pc = self_.state.pc;
    self_.state.pc += 1;
    get_int32!(self_, iseq, to_catch, isize);
    get_int32!(self_, iseq, to_finally, isize);
    self_.trystate_stack.push(TryState::Try(
        pc + to_catch,
        pc + to_finally,
        TryReturn::None,
    ));
    Ok(true)
}

fn leave_try(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    fn finally_return(self_: &mut VM, return_val: Value) -> Result<bool, RuntimeError> {
        if self_.trystate_stack.len() == 0 || self_.trystate_stack.last() == Some(&TryState::None) {
            self_.state.stack.push(return_val);
            unwind_state(self_);
            return Ok(false);
        }

        let trystate = self_.trystate_stack.last_mut().unwrap();
        match trystate.clone() {
            TryState::Try(_, to_finally, _) | TryState::Catch(to_finally, _) => {
                self_.state.pc = to_finally;
                *trystate = TryState::Finally(TryReturn::Value(return_val));
            }
            TryState::Finally(ref mut ret) => *ret = TryReturn::Value(return_val),
            _ => unreachable!(),
        };

        Ok(true)
    };

    self_.state.pc += 1;
    let trystate = self_.trystate_stack.pop().unwrap();
    match trystate {
        TryState::Finally(TryReturn::Value(val)) => finally_return(self_, val),
        TryState::Finally(TryReturn::Error(err)) => Err(err.clone()),
        _ => Ok(true),
    }
}

fn catch(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    let trystate = self_.trystate_stack.last_mut().unwrap();
    match trystate {
        TryState::Catch(_, _) => {}
        _ => unreachable!("catch(): invalid trystate."),
    };

    Ok(true)
}

fn finally(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    let trystate = self_.trystate_stack.last_mut().unwrap();
    match trystate.clone() {
        TryState::Finally(_) => {}
        TryState::Try(_, _, x) => {
            *trystate = TryState::Finally(x.clone());
        }
        TryState::Catch(_, x) => {
            *trystate = TryState::Finally(x.clone());
        }
        _ => unreachable!("finally(): invalid trystate."),
    };
    Ok(true)
}

fn return_try(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    let pc = self_.state.pc;
    self_.state.pc += 1;
    get_int32!(self_, iseq, to_finally, isize);
    let val = self_.state.stack.pop().unwrap();
    let trystate = self_.trystate_stack.last_mut().unwrap();
    match trystate {
        TryState::Finally(_) => {}
        TryState::Try(_, _, _) | TryState::Catch(_, _) => {
            *trystate = TryState::Finally(TryReturn::Value(val));
        }
        _ => unreachable!(),
    };
    self_.state.pc = pc + to_finally;
    Ok(true)
}

fn push_scope(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    let &base_callobj = self_.state.scope.last().unwrap();
    let co = unsafe { (*base_callobj).clone() };
    let mut callobj = CallObject::new(Value::object_from_nvp(&vec![]));
    callobj.parent = Some(base_callobj);
    callobj.params = co.params;
    callobj.arg_rest_vals = co.arg_rest_vals;
    callobj.this = co.this;
    self_.state.scope.push(gc::new(callobj));
    Ok(true)
}

fn pop_scope(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    self_.state.scope.pop();
    Ok(true)
}

fn double(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // double
    let stack_top_val = self_.state.stack.last().unwrap().clone();
    self_.state.stack.push(stack_top_val);
    Ok(true)
}

fn pop(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // double
    self_.state.stack.pop();
    Ok(true)
}

// 'land' and 'lor' are for JIT compiler. Nope for VM.

fn land(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // land
    Ok(true)
}

fn lor(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // lor
    Ok(true)
}

fn update_parent_scope(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    if let Some(Value::Function(box (_, _, _, ref mut callobj))) = self_.state.stack.last_mut() {
        callobj.parent = Some(self_.state.scope.last().unwrap().clone());
    }
    Ok(true)
}

fn get_value(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, iseq, name_id, usize);
    let name = &self_.const_table.string[name_id];
    let val = unsafe { (**self_.state.scope.last().unwrap()).get_value(name)? };
    self_.state.stack.push(val);
    Ok(true)
}

fn set_value(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, iseq, name_id, usize);
    let name = self_.const_table.string[name_id].clone();
    let mut val = self_.state.stack.pop().unwrap();

    // We have to change cobj.this to the current scope one. (./examples/this.js)
    if let Value::Function(box (_, _, _, ref mut cobj))
    | Value::BuiltinFunction(box (_, _, ref mut cobj)) = &mut val
    {
        unsafe {
            cobj.this = (**self_.state.scope.last().unwrap()).this.clone();
        }
    }

    unsafe { (**self_.state.scope.last().unwrap()).set_value_if_exist(name, val) };

    Ok(true)
}

fn decl_var(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, iseq, name_id, usize);
    let name = self_.const_table.string[name_id].clone();
    unsafe { (**self_.state.scope.last().unwrap()).set_value(name, Value::undefined()) };
    Ok(true)
}

// 'cond_op' is for JIT compiler. Nope for VM.
fn cond_op(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    Ok(true)
}

fn loop_start(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
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

    Ok(true)
}
