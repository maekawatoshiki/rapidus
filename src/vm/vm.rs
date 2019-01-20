use chrono::Utc;
use libc;
use llvm::core::*;
use std::{ffi::CString, thread, time};

use super::{
    callobj::CallObject,
    error::*,
    task::{Task, TaskManager, TimerKind},
    value::*,
};
use builtin;
use builtin::BuiltinJITFuncInfo;
use builtins;
use bytecode_gen;
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
    pub is_debug: bool,
    pub jit_on: bool,
    pub gc_on: bool,
}

pub struct VMState {
    pub stack: Vec<Value>,
    pub scope: Vec<CallObjectRef>,
    pub pc: isize,
    pub history: Vec<(usize, isize)>, // sp, return_pc
}

#[derive(Clone, Debug, PartialEq)]
/// Value or Runtime error to be returned after finally clause.
pub enum TryReturn {
    Value(Value),
    Error(RuntimeError),
    None,
}

impl TryReturn {
    pub fn to_string(&self) -> String {
        match self {
            TryReturn::Value(val) => val.to_string(),
            TryReturn::Error(err) => err.to_value().to_string(),
            TryReturn::None => "None".to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TryState {
    Try(isize, isize, TryReturn), //position of (CATCH, FINALLY)
    Catch(isize, TryReturn),      //postion of (FINALLY)
    Finally(TryReturn),
    None,
}

impl TryState {
    pub fn to_string(&self) -> (String, String) {
        match &self {
            TryState::None => ("None".to_string(), "".to_string()),
            TryState::Try(_, _, tryreturn) => ("Try".to_string(), tryreturn.to_string()),
            TryState::Catch(_, tryreturn) => ("Catch".to_string(), tryreturn.to_string()),
            TryState::Finally(tryreturn) => ("Finally".to_string(), tryreturn.to_string()),
        }
    }
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
        let mut global_vals = global_vals.clone();

        // TODO: Support for 'require' is not enough.
        global_vals.set_value(
            "require".to_string(),
            Value::default_builtin_function(builtin::require),
        );

        let module_exports = Value::object_from_npp(&vec![]);
        global_vals.set_value("module".to_string(), {
            make_object!(
                exports:    module_exports.clone()
            )
        });
        global_vals.set_value("exports".to_string(), module_exports);

        global_vals.set_value("console".to_string(), {
            let func_log = Value::builtin_function_with_jit(
                builtin::console_log,
                BuiltinJITFuncInfo::ConsoleLog {
                    bool: (builtin::jit_console_log_bool as *mut libc::c_void, unsafe {
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
                        )
                    }),
                    f64: (builtin::jit_console_log_f64 as *mut libc::c_void, unsafe {
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
                        )
                    }),
                    string: (
                        builtin::jit_console_log_string as *mut libc::c_void,
                        unsafe {
                            LLVMAddFunction(
                                jit.module,
                                CString::new("jit_console_log_string").unwrap().as_ptr(),
                                LLVMFunctionType(
                                    LLVMVoidType(),
                                    vec![LLVMPointerType(LLVMInt8TypeInContext(jit.context), 0)]
                                        .as_mut_slice()
                                        .as_mut_ptr(),
                                    1,
                                    0,
                                ),
                            )
                        },
                    ),
                    newline: (
                        builtin::jit_console_log_newline as *mut libc::c_void,
                        unsafe {
                            LLVMAddFunction(
                                jit.module,
                                CString::new("jit_console_log_newline").unwrap().as_ptr(),
                                LLVMFunctionType(LLVMVoidType(), vec![].as_mut_ptr(), 0, 0),
                            )
                        },
                    ),
                },
            );
            let npp = make_npp!(log: func_log);
            Value::object_from_npp(&npp)
        });

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
                    Value::object_from_npp(
                        &make_npp!(
                             write:  Value::builtin_function_with_jit(
                                 builtin::process_stdout_write,
                                 BuiltinJITFuncInfo::Normal {
                                     func: builtin::jit_process_stdout_write as *mut libc::c_void,
                                     llvm_func: llvm_process_stdout_write,
                                 },
                             )
                         ),
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

        global_vals.set_value(
            "__enableJit".to_string(),
            Value::default_builtin_function(builtin::enable_jit),
        );

        global_vals.set_value(
            "__assert".to_string(),
            Value::default_builtin_function(builtin::assert_seq),
        );

        global_vals.set_value("Object".to_string(), builtins::object::init());
        global_vals.set_value("Error".to_string(), builtins::error::init());
        global_vals.set_value("Function".to_string(), builtins::function::init());
        global_vals.set_value("Array".to_string(), builtins::array::init());
        use builtins::date::DATE_OBJ;
        global_vals.set_value("Date".to_string(), DATE_OBJ.with(|x| x.clone()));
        global_vals.set_value("Math".to_string(), builtins::math::init(jit.clone()));
        /*
                println!(
                    "CallObject:{} Value:{} PropMapRef:{} ArrayValue:{}",
                    std::mem::size_of::<CallObject>(),
                    std::mem::size_of::<Value>(),
                    std::mem::size_of::<PropMapRef>(),
                    std::mem::size_of::<ArrayValue>()
                );
        */
        VM {
            jit: jit,
            state: VMState {
                stack: { Vec::with_capacity(128) },
                scope: vec![global_vals],
                history: vec![(0, 0)],
                pc: 0isize,
            },
            trystate_stack: vec![TryState::None],
            const_table: ConstantTable::new(),
            cur_func_id: 0, // 0 is main
            task_mgr: TaskManager::new(),
            is_debug: false,
            jit_on: true,
            gc_on: true,
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
        if self.is_debug {
            println!(
                " {:<8} {:<8} {:<5} {:<5} {:<16} {:<4} {}",
                "tryst".to_string(),
                "tryret".to_string(),
                "scope".to_string(),
                "stack".to_string(),
                "  top".to_string(),
                "PC".to_string(),
                "INST".to_string(),
            );
        }
        //self.store_state();
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

        gc::free_all();

        res
    }

    /// push vm.state.history
    fn store_state(&mut self) {
        self.state
            .history
            .push((self.state.stack.len(), self.state.pc));
        self.state.pc = 0;
    }

    /// pop vm.state.history
    fn restore_state(&mut self) {
        if let Some((previous_sp, return_pc)) = self.state.history.pop() {
            if self.is_debug {
                print!("stack trace: ");
                for (n, v) in self.state.stack.iter().enumerate() {
                    if n == 0 {
                        print!("{}", v.format(1, false));
                    } else {
                        print!(" | {}", v.format(1, false));
                    }
                }
                println!();
            }
            let top = self.state.stack.pop();
            self.state.stack.truncate(previous_sp);
            if let Some(top) = top {
                self.state.stack.push(top);
            }
            self.state.pc = return_pc;
        } else {
            unreachable!("history stack abnormaly exhaust.")
        }
    }

    /// main execution loop
    fn do_run(&mut self, iseq: &ByteCode) -> Result<bool, RuntimeError> {
        self.store_state();
        self.trystate_stack.push(TryState::None);
        //let mut count = 0;
        loop {
            //count += 1;
            //if count % 1000 == 0 {
            //    gc::mark_and_sweep(&mut self.state)
            //};
            let code = iseq[self.state.pc as usize];
            if self.is_debug {
                let trystate = self.trystate_stack.last().unwrap();
                let scopelen = self.state.scope.len();
                let stacklen = self.state.stack.len();
                let stacktop = self.state.stack.last();
                print!(
                    " {:<8} {:<8} {:<5} {:<5} {:<16} ",
                    trystate.to_string().0,
                    trystate.to_string().1,
                    scopelen,
                    stacklen,
                    match stacktop {
                        Some(val) => val.format(0, false),
                        None => "".to_string(),
                    },
                );
                bytecode_gen::show_inst(iseq, self.state.pc as usize, &self.const_table);
                println!();
            }
            match self.op_table[code as usize](self, iseq) {
                Ok(true) => {
                    continue;
                }

                Ok(false) => {
                    // END or RETURN or, THROW occurs outer try-catch.
                    self.trystate_stack.pop().unwrap();
                    self.restore_state();
                    return Ok(true);
                }

                Err(err) => {
                    // Runtime error or THROW in try-catch.
                    let mut error: Option<RuntimeError> = None;
                    {
                        let trystate = self.trystate_stack.last_mut().unwrap();
                        match trystate.clone() {
                            TryState::Try(to_catch, to_finally, ret) => {
                                self.state.pc = to_catch;
                                // push error object to exec stack.
                                let err_obj = err.to_value();
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
                        self.set_return_value(Value::Undefined);
                        self.restore_state();
                        return Err(err);
                    }
                }
            }
        }
    }

    pub fn set_return_value(&mut self, val: Value) {
        self.state.stack.push(val);
    }

    pub fn call_function_simply(
        &mut self,
        callee: &Value,
        args: &Vec<Value>,
    ) -> Result<bool, RuntimeError> {
        //println!("{}", callee.format(1, true));
        match callee {
            Value::Object(_, ObjectKind::BuiltinFunction(box (ref info, callobj))) => {
                (info.func)(self, args, callobj.clone())?;
                return Ok(true);
            }
            Value::Object(_, ObjectKind::Function(box (func_info, callobject))) => {
                //println!("call this:{}", callobject.this.clone().format(1, true));
                call_function(self, func_info.clone(), &mut callobject.clone(), args)
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

fn end(_self: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
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
        Value::Object(map, ObjectKind::BuiltinFunction(box (x, mut callobj))) => {
            *callobj.this = Value::object_from_npp(&vec![(
                "__proto__".to_string(),
                Property::new(
                    map.get("prototype")
                        .unwrap_or(&Value::Undefined.to_property())
                        .val
                        .clone(),
                ),
            )]);

            // https://tc39.github.io/ecma262/#sec-date-constructor
            // > The Date constructor returns a String representing the current time (UTC) when
            // > called as a function rather than as a constructor.
            use builtins::date::{date, date_new};
            (if x.func as *const u8 == date as *const u8 {
                date_new
            } else {
                x.func
            })(self_, &args, callobj)?;
        }
        Value::Object(map, ObjectKind::Function(box (func_info, callobj))) => {
            // similar code is used some times. should make it a function.
            let new_this = Value::object_from_npp(&vec![(
                "__proto__".to_string(),
                Property::new(
                    map.get("prototype")
                        .unwrap_or(&Property::new(Value::Undefined))
                        .val
                        .clone(),
                ),
            )]);
            let callobj =
                callobj.new_callobj_from_func(func_info.clone(), &args, Some(new_this.clone()));

            self_.state.scope.push(callobj);

            let res = self_.do_run(&func_info.iseq);

            self_.state.scope.pop();
            let ret = self_.state.stack.last_mut().unwrap();
            match &ret {
                &Value::Object(_, _) => {}
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

/// invoke JS function.
/// 1)apply arguments, 2)execute bytecode.
pub fn call_function(
    self_: &mut VM,
    func_info: FuncInfo,
    callobj: &mut CallObject,
    args: &Vec<Value>,
) -> Result<bool, RuntimeError> {
    let argc = args.len();
    let args_all_numbers = args.iter().all(|val| match val {
        Value::Number(_) => true,
        _ => false,
    });

    let callobj = callobj.new_callobj_from_func(func_info.clone(), &args, None);
    self_.state.scope.push(callobj);

    let FuncInfo { id, iseq, .. } = func_info.clone();

    if args_all_numbers && self_.jit_on {
        let scope = (*self_.state.scope.last().unwrap()).clone();
        if let Some(f) = unsafe {
            self_
                .jit
                .can_jit(func_info, scope, &self_.const_table, argc)
        } {
            self_
                .state
                .stack
                .push(unsafe { self_.jit.run_llvm_func(id, f, &args) });
            self_.state.scope.pop();
            return Ok(true);
        }
    }

    let res = self_.do_run(&iseq);

    self_.state.scope.pop();
    if self_.jit_on {
        self_
            .jit
            .record_function_return_type(id, self_.state.stack.last().unwrap());
    };
    res
}

fn create_object(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // create_object
    get_int32!(self_, iseq, len, usize);
    let mut npp = vec![];
    for _ in 0..len {
        let name = if let Value::String(name) = self_.state.stack.pop().unwrap() {
            name.into_string().unwrap()
        } else {
            unreachable!()
        };
        let val = self_.state.stack.pop().unwrap();
        npp.push((name, Property::new(val.clone())));
    }

    self_.state.stack.push(Value::object_from_npp(&npp));

    gc::mark_and_sweep(self_);

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

    gc::mark_and_sweep(self_);

    Ok(true)
}

fn push_int8(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_int
    get_int8!(self_, iseq, n, i8);
    self_.state.stack.push(Value::Number(n as f64));
    Ok(true)
}

fn push_int32(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_int
    get_int32!(self_, iseq, n, i32);
    self_.state.stack.push(Value::Number(n as f64));
    Ok(true)
}

fn push_false(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_false
    self_.state.stack.push(Value::Bool(false));
    Ok(true)
}

fn push_true(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_true
    self_.state.stack.push(Value::Bool(true));
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
    let this = self_.state.scope.last().unwrap().this.clone();
    self_.state.stack.push(*this);
    Ok(true)
}

fn push_arguments(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_arguments
    let callobj = self_.state.scope.last_mut().unwrap().clone();
    self_.state.stack.push(Value::arguments(callobj));
    Ok(true)
}

fn push_undefined(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_defined
    self_.state.stack.push(Value::Undefined);
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
        Value::Number(n) => Value::Number(-n),
        _ => return Err(RuntimeError::Unimplemented),
    };
    Ok(true)
}

fn add(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number(l + r),
        (Value::Bool(false), Value::Number(x)) | (Value::Number(x), Value::Bool(false)) => {
            Value::Number(x)
        }
        (Value::Bool(true), Value::Number(x)) | (Value::Number(x), Value::Bool(true)) => {
            Value::Number(x + 1.0)
        }
        // TODO: We need the correct implementation.
        (Value::Undefined, _) | (_, Value::Undefined) => Value::Number(::std::f64::NAN),
        (l, r) => Value::string(l.to_string() + r.to_string().as_str()),
    });
    Ok(true)
}

fn sub(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number(l - r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn mul(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number(l * r),
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
        (Value::Number(l), Value::Number(r)) => Value::Number(l / r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn rem(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number((l as i64 % r as i64) as f64),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn lt(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Bool(l < r),
        (Value::String(l), Value::String(r)) => Value::Bool(l < r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn gt(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Bool(l > r),
        (Value::String(l), Value::String(r)) => Value::Bool(l > r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn le(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Bool(l <= r),
        (Value::String(l), Value::String(r)) => Value::Bool(l <= r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn ge(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Bool(l >= r),
        (Value::String(l), Value::String(r)) => Value::Bool(l >= r),
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
        .push(Value::Bool(lhs.abstract_equal(rhs)?));
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
        .push(Value::Bool(!lhs.abstract_equal(rhs)?));
    Ok(true)
}

// TODO: Need more precise implemention
fn seq(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(Value::Bool(lhs.strict_equal(rhs)?));
    Ok(true)
}

// TODO: Need more precise implemention
fn sne(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(Value::Bool(!lhs.strict_equal(rhs)?));
    Ok(true)
}

fn and(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => {
            Value::Number(((l as i64 as i32) & (r as i64 as i32)) as f64)
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
            Value::Number(((l as i64 as i32) | (r as i64 as i32)) as f64)
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
            Value::Number(((l as i64 as i32) ^ (r as i64 as i32)) as f64)
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
            Value::Number(((l as i64 as i32) << (r as i64 as i32)) as f64)
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
            Value::Number(((l as i64 as i32) >> (r as i64 as i32)) as f64)
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
            Value::Number(((l as u64 as u32) >> (r as u64 as u32)) as f64)
        }
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn get_member(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // get_global
    let member = self_.state.stack.pop().unwrap();
    let parent = self_.state.stack.pop().unwrap();
    match member.clone() {
        Value::String(ref s) if s.to_str().unwrap() == "toString" => {}
        _ => {}
    };
    let val = parent.get_property(member, Some(self_.state.scope.last().unwrap().clone()));
    self_.state.stack.push(val);
    Ok(true)
}

fn set_member(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // get_global
    let member = self_.state.stack.pop().unwrap();
    let mut parent = self_.state.stack.pop().unwrap().clone();
    let val = self_.state.stack.pop().unwrap();
    parent.set_property(member, val, Some(self_.state.scope.last().unwrap().clone()));
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

fn return_(_self: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    Ok(false)
}

fn throw(self_: &mut VM, _iseq: &ByteCode) -> Result<bool, RuntimeError> {
    let val = self_.state.stack.pop().unwrap().clone();
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
    let mut callobj = CallObject::new_with_this(Value::object_from_npp(&vec![]));

    let base_callobj = self_.state.scope.last().unwrap().clone();

    callobj.parent = Some(base_callobj.clone());
    callobj.this = base_callobj.this.clone();

    self_.state.scope.push(callobj);
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
    if let Some(Value::Object(_, ObjectKind::Function(box (_, ref mut callobj)))) =
        self_.state.stack.last_mut()
    {
        callobj.parent = Some(self_.state.scope.last().unwrap().clone());
    }
    Ok(true)
}

fn get_value(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, iseq, name_id, usize);
    let name = &self_.const_table.string[name_id];
    let val = self_.state.scope.last().unwrap().get_value(name)?;
    self_.state.stack.push(val);
    Ok(true)
}

fn set_value(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, iseq, name_id, usize);
    let name = self_.const_table.string[name_id].clone();
    let mut val = self_.state.stack.pop().unwrap();

    // We have to change cobj.this to the current scope one. (./examples/this.js)
    if let Value::Object(_, ObjectKind::Function(box (_, ref mut cobj)))
    | Value::Object(_, ObjectKind::BuiltinFunction(box (_, ref mut cobj))) = &mut val
    {
        cobj.this = self_.state.scope.last().unwrap().this.clone();
    }

    self_
        .state
        .scope
        .last_mut()
        .unwrap()
        .set_value_if_exist(name, val);

    Ok(true)
}

fn decl_var(self_: &mut VM, iseq: &ByteCode) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, iseq, name_id, usize);
    let name = self_.const_table.string[name_id].clone();
    (*self_.state.scope.last_mut().unwrap()).set_value(name, Value::Undefined);
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

    if self_.jit_on {
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
    }

    Ok(true)
}
