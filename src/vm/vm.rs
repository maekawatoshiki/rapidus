use super::{
    callobj::CallObject,
    codegen,
    codegen::CodeGenerator,
    constant,
    error::*,
    frame,
    jsvalue::function::DestinationKind,
    jsvalue::prototype::ObjectPrototypes,
    jsvalue::value::*,
    task::{Task, TaskManager, TimerKind},
    value::*,
};
use builtin;
use builtin::BuiltinJITFuncInfo;
use builtins;
use bytecode_gen;
use bytecode_gen::ByteCode;
use bytecode_gen::VMInst;
use chrono::Utc;
use gc;
use jit::TracingJit;
use libc;
use llvm::core::*;
use rustc_hash::FxHashMap;
use std::{ffi::CString, thread, time};
use vm_codegen;

// New VM

pub type VMResult = Result<(), RuntimeError>;

pub struct VM2<'a> {
    pub global_environment: frame::LexicalEnvironmentRef,
    pub code_generator: CodeGenerator<'a>,
    pub stack: Vec<BoxedValue>,
    pub saved_frame: Vec<frame::Frame>,
}

macro_rules! memory_allocator {
    ($vm:ident) => {{
        &mut $vm.code_generator.memory_allocator
    }};
}

macro_rules! constant_table {
    ($vm:ident) => {{
        &$vm.code_generator.bytecode_generator.constant_table
    }};
}
macro_rules! object_prototypes {
    ($vm:ident) => {{
        &$vm.code_generator.object_prototypes
    }};
}

impl<'a> VM2<'a> {
    pub fn new(
        global_environment: frame::LexicalEnvironmentRef,
        constant_table: &'a mut constant::ConstantTable,
        memory_allocator: &'a mut gc::MemoryAllocator,
        object_prototypes: &'a ObjectPrototypes,
    ) -> Self {
        VM2 {
            global_environment,
            code_generator: CodeGenerator::new(constant_table, memory_allocator, object_prototypes),
            stack: vec![],
            saved_frame: vec![],
        }
    }

    pub fn run_global(&mut self, global_info: codegen::FunctionInfo, iseq: ByteCode) -> VMResult {
        let global_env_ref = self.global_environment;

        let var_env = memory_allocator!(self).alloc(frame::LexicalEnvironment {
            record: frame::EnvironmentRecord::Declarative({
                let mut record = FxHashMap::default();
                for name in global_info.var_names {
                    record.insert(name, Value2::undefined());
                }
                record
            }),
            outer: Some(global_env_ref),
        });

        let lex_env = memory_allocator!(self).alloc(frame::LexicalEnvironment {
            record: frame::EnvironmentRecord::Declarative({
                let mut record = FxHashMap::default();
                for name in global_info.lex_names {
                    record.insert(name, Value2::uninitialized());
                }
                record
            }),
            outer: Some(var_env),
        });

        for val in global_info.func_decls {
            let mut val = val.copy_object(memory_allocator!(self));
            let name = val.as_function().name.clone().unwrap();
            val.set_function_outer_environment(lex_env);
            unsafe { &mut *lex_env }.set_value(name, val)?;
        }

        let exec_ctx = frame::ExecutionContext {
            variable_environment: var_env,
            lexical_environment: lex_env,
            saved_lexical_environment: vec![],
        };

        let frame = frame::Frame::new(
            exec_ctx,
            iseq,
            global_info.exception_table,
            Some(unsafe { &*global_env_ref }.get_global_object()),
            false,
        );

        self.run(frame)?;

        Ok(())
    }
}

macro_rules! read_int8 {
    ($iseq:expr, $pc:expr, $var:ident, $ty:ty) => {
        let $var = $iseq[$pc] as $ty;
        $pc += 1;
    };
}

macro_rules! read_int32 {
    ($iseq:expr, $pc:expr, $var:ident, $ty:ty) => {
        let $var = (($iseq[$pc as usize + 3] as $ty) << 24)
            + (($iseq[$pc as usize + 2] as $ty) << 16)
            + (($iseq[$pc as usize + 1] as $ty) << 8)
            + ($iseq[$pc as usize + 0] as $ty);
        $pc += 4;
    };
}

impl<'a> VM2<'a> {
    pub fn run(&mut self, mut cur_frame: frame::Frame) -> VMResult {
        #[derive(Debug, Clone)]
        enum SubroutineKind {
            Ordinary(usize),
            Throw,
            Return,
        }

        let mut subroutine_stack: Vec<SubroutineKind> = vec![];

        macro_rules! exception {
            () => {{
                let mut exception_found = false;
                let mut outer_break = false;

                loop {
                    for exception in &cur_frame.exception_table {
                        let in_range =
                            exception.start <= cur_frame.pc && cur_frame.pc < exception.end;
                        if !in_range {
                            continue;
                        }
                        // TODO
                        match exception.dst_kind {
                            DestinationKind::Catch => cur_frame.pc = exception.end,
                            DestinationKind::Finally => {
                                subroutine_stack.push(SubroutineKind::Throw);
                                cur_frame.pc = exception.end
                            }
                        }
                        exception_found = true;
                        outer_break = true;
                        break;
                    }

                    if outer_break {
                        break;
                    }

                    if self.saved_frame.len() == 0 {
                        break;
                    }

                    if !exception_found {
                        self.unwind_frame_saving_stack_top(&mut cur_frame);
                    }
                }

                if !exception_found {
                    let val: Value2 = self.stack.pop().unwrap().into();
                    return Err(RuntimeError::Exception2(val));
                }
            }};
        }

        macro_rules! etry {
            ($val:expr) => {{
                match $val {
                    Ok(ok) => ok,
                    Err(err) => {
                        let val = err.to_value2(memory_allocator!(self));
                        self.stack.push(val.into());
                        exception!();
                        continue;
                    }
                }
            }};
        }

        loop {
            match cur_frame.bytecode[cur_frame.pc] {
                // TODO: Macro for bin ops?
                VMInst::ADD => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.add(rhs).into());
                }
                VMInst::SUB => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.sub(rhs).into());
                }
                VMInst::MUL => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.mul(rhs).into());
                }
                VMInst::EQ => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.eq(rhs).into());
                }
                VMInst::LT => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.lt(rhs).into());
                }
                VMInst::PUSH_INT8 => {
                    cur_frame.pc += 1;
                    read_int8!(cur_frame.bytecode, cur_frame.pc, num, f64);
                    self.stack.push(Value2::Number(num).into());
                }
                VMInst::PUSH_CONST => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, id, usize);
                    let val = *constant_table!(self).get(id).as_value();
                    self.stack.push(val.into());
                }
                VMInst::PUSH_THIS => {
                    cur_frame.pc += 1;
                    self.stack.push(cur_frame.this.unwrap().into());
                }
                VMInst::GET_MEMBER => {
                    cur_frame.pc += 1;
                    let property: Value2 = self.stack.pop().unwrap().into();
                    let parent: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(parent.get_property(property).into());
                }
                VMInst::SET_MEMBER => {
                    cur_frame.pc += 1;
                    let property: Value2 = self.stack.pop().unwrap().into();
                    let parent: Value2 = self.stack.pop().unwrap().into();
                    let val: Value2 = self.stack.pop().unwrap().into();
                    parent.set_property(property, val);
                }
                VMInst::SET_VALUE => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, name_id, usize);
                    let val = self.stack.pop().unwrap();
                    let name = constant_table!(self).get(name_id).as_string().clone();
                    etry!(cur_frame.lex_env().set_value(name, val.into()));
                }
                VMInst::GET_VALUE => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, name_id, usize);
                    let val = etry!(cur_frame
                        .lex_env()
                        .get_value(constant_table!(self).get(name_id).as_string()));
                    self.stack.push(val.into());
                }
                VMInst::CONSTRUCT => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, argc, usize);
                    let callee: Value2 = self.stack.pop().unwrap().into();
                    let mut args: Vec<Value2> = vec![];
                    for _ in 0..argc {
                        args.push(self.stack.pop().unwrap().into());
                    }
                    self.call_constructor(callee, args, &mut cur_frame)?;
                }
                VMInst::CALL => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, argc, usize);
                    let callee: Value2 = self.stack.pop().unwrap().into();
                    let mut args: Vec<Value2> = vec![];
                    for _ in 0..argc {
                        args.push(self.stack.pop().unwrap().into());
                    }
                    self.call_function(callee, args, None, &mut cur_frame, false)?;
                }
                VMInst::CALL_METHOD => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, argc, usize);
                    let parent: Value2 = self.stack.pop().unwrap().into();
                    let method: Value2 = self.stack.pop().unwrap().into();
                    let mut args: Vec<Value2> = vec![];
                    for _ in 0..argc {
                        args.push(self.stack.pop().unwrap().into());
                    }
                    self.call_function(
                        parent.get_property(method),
                        args,
                        Some(parent),
                        &mut cur_frame,
                        false,
                    )?;
                }
                VMInst::SET_OUTER_ENV => {
                    cur_frame.pc += 1;
                    let func_template: Value2 = self.stack.pop().unwrap().into();
                    let mut func = func_template.copy_object(memory_allocator!(self));
                    func.set_function_outer_environment(
                        cur_frame.execution_context.lexical_environment,
                    );
                    self.stack.push(func.into());
                }
                VMInst::CREATE_OBJECT => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, len, usize);
                    self.create_object(len)?;
                }
                VMInst::PUSH_ENV => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, id, usize);
                    let names = constant_table!(self).get(id).as_lex_env_info().clone();
                    self.push_env(names, &mut cur_frame)?;
                }
                VMInst::POP_ENV => {
                    cur_frame.pc += 1;
                    let lex_env = unsafe {
                        &*cur_frame
                            .execution_context
                            .saved_lexical_environment
                            .pop()
                            .unwrap()
                    };
                    unsafe {
                        *cur_frame.execution_context.lexical_environment = lex_env.clone();
                    }
                }
                VMInst::POP => {
                    cur_frame.pc += 1;
                    self.stack.pop();
                }
                VMInst::PUSH_UNDEFINED => {
                    cur_frame.pc += 1;
                    self.stack.push(Value2::undefined().into());
                }
                VMInst::JMP_IF_FALSE => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, dst, i32);
                    let cond_boxed = self.stack.pop().unwrap();
                    let cond: Value2 = cond_boxed.into();
                    if !cond.to_boolean() {
                        cur_frame.pc = (cur_frame.pc as isize + dst as isize) as usize;
                    }
                }
                VMInst::JMP => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, dst, i32);
                    cur_frame.pc = (cur_frame.pc as isize + dst as isize) as usize;
                }
                VMInst::JMP_SUB => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, dst, i32);
                    subroutine_stack.push(SubroutineKind::Ordinary(cur_frame.pc));
                    cur_frame.pc = (cur_frame.pc as isize + dst as isize) as usize;
                }
                VMInst::RETURN_TRY => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, dst, i32);
                    cur_frame.pc = (cur_frame.pc as isize + dst as isize) as usize;
                    subroutine_stack.push(SubroutineKind::Return);
                }
                VMInst::RETURN_SUB => {
                    cur_frame.pc += 1;
                    match subroutine_stack.pop().unwrap() {
                        SubroutineKind::Ordinary(pos) => cur_frame.pc = pos,
                        SubroutineKind::Throw => exception!(),
                        SubroutineKind::Return => {
                            self.unwind_frame_saving_stack_top(&mut cur_frame);
                        }
                    }
                }
                VMInst::THROW => {
                    cur_frame.pc += 1;
                    exception!();
                }
                VMInst::RETURN => {
                    cur_frame.pc += 1;
                    self.unwind_frame_saving_stack_top(&mut cur_frame);
                }
                VMInst::END => break,
                e => unimplemented!("code: {}", e),
            }
        }

        Ok(())
    }

    pub fn unwind_frame_saving_stack_top(&mut self, cur_frame: &mut frame::Frame) {
        let ret_val_boxed = self.stack.pop().unwrap();
        let ret_val: Value2 = ret_val_boxed.into();
        let frame = self.saved_frame.pop().unwrap();
        unsafe { self.stack.set_len(frame.saved_stack_len) };
        if cur_frame.constructor_call && !ret_val.is_object() {
            self.stack.push(cur_frame.this.unwrap().into());
        } else {
            self.stack.push(ret_val_boxed);
        }
        *cur_frame = frame;
    }

    pub fn unwind_frame(&mut self, cur_frame: &mut frame::Frame) {
        let frame = self.saved_frame.pop().unwrap();
        unsafe { self.stack.set_len(frame.saved_stack_len) };
        *cur_frame = frame;
    }

    fn push_env(&mut self, lex_names: Vec<String>, cur_frame: &mut frame::Frame) -> VMResult {
        let mut record = FxHashMap::default();
        for name in lex_names {
            record.insert(name, Value2::uninitialized());
        }

        let lex_env = memory_allocator!(self).alloc(frame::LexicalEnvironment {
            record: frame::EnvironmentRecord::Declarative(record),
            outer: Some(cur_frame.execution_context.lexical_environment),
        });

        cur_frame
            .execution_context
            .saved_lexical_environment
            .push(cur_frame.execution_context.lexical_environment);
        cur_frame.execution_context.lexical_environment = lex_env;

        Ok(())
    }

    fn create_object(&mut self, len: usize) -> VMResult {
        let mut properties = FxHashMap::default();
        for _ in 0..len {
            let prop: Value2 = self.stack.pop().unwrap().into();
            let name = prop.to_string();
            let val: Value2 = self.stack.pop().unwrap().into();
            properties.insert(
                name,
                Property2 {
                    val,
                    // TODO
                    writable: true,
                    enumerable: true,
                    configurable: true,
                },
            );
        }

        let obj = Value2::object(
            memory_allocator!(self),
            object_prototypes!(self),
            properties,
        );
        self.stack.push(obj.into());
        Ok(())
    }

    fn call_constructor(
        &mut self,
        callee: Value2,
        args: Vec<Value2>,
        cur_frame: &mut frame::Frame,
    ) -> VMResult {
        let properties = match callee
            .get_property_by_str_key("prototype")
            .get_object_properties()
        {
            Some(properties) => properties.clone(),
            None => return Err(RuntimeError::Type("not a constructor".to_string())),
        };

        let this = Value2::object(
            memory_allocator!(self),
            object_prototypes!(self),
            properties
                .into_iter()
                .map(|(key, _)| {
                    (
                        key,
                        Property2 {
                            val: Value2::undefined(),
                            writable: true,
                            enumerable: true,
                            configurable: true,
                        },
                    )
                })
                .collect(),
        );

        let info = callee.as_function();
        match info.kind {
            FunctionObjectKind::Builtin(func) => func(self, &args, cur_frame),
            FunctionObjectKind::User(ref user_func) => {
                self.call_user_function(user_func.clone(), args, Some(this), cur_frame, true)
            }
        }
    }

    fn call_function(
        &mut self,
        callee: Value2,
        args: Vec<Value2>,
        this: Option<Value2>,
        cur_frame: &mut frame::Frame,
        constructor_call: bool,
    ) -> VMResult {
        let info = callee.as_function();
        match info.kind {
            FunctionObjectKind::Builtin(func) => func(self, &args, cur_frame),
            FunctionObjectKind::User(ref user_func) => {
                self.call_user_function(user_func.clone(), args, this, cur_frame, constructor_call)
            }
        }
    }

    fn call_user_function(
        &mut self,
        user_func: UserFunctionInfo,
        args: Vec<Value2>,
        this: Option<Value2>,
        cur_frame: &mut frame::Frame,
        constructor_call: bool,
    ) -> VMResult {
        self.saved_frame.push({
            let mut cur_frame = cur_frame.clone();
            cur_frame.saved_stack_len = self.stack.len();
            cur_frame
        });

        let var_env = memory_allocator!(self).alloc(frame::LexicalEnvironment {
            record: frame::EnvironmentRecord::Declarative({
                let mut record = FxHashMap::default();

                for name in user_func.var_names {
                    record.insert(name, Value2::undefined());
                }

                // TODO: rest parameter
                for (FunctionParameter { name, .. }, arg) in user_func.params.iter().zip(args) {
                    record.insert(name.clone(), arg);
                }

                record
            }),
            outer: user_func.outer,
        });

        let lex_env = memory_allocator!(self).alloc(frame::LexicalEnvironment {
            record: frame::EnvironmentRecord::Declarative({
                let mut record = FxHashMap::default();
                for name in user_func.lex_names {
                    record.insert(name, Value2::uninitialized());
                }
                record
            }),
            outer: Some(var_env),
        });

        for func in user_func.func_decls {
            let mut func = func.copy_object(memory_allocator!(self));
            let name = func.as_function().name.clone().unwrap();
            func.set_function_outer_environment(lex_env);
            unsafe { &mut *lex_env }.set_value(name, func)?;
        }

        let exec_ctx = frame::ExecutionContext {
            variable_environment: var_env,
            lexical_environment: lex_env,
            saved_lexical_environment: vec![],
        };

        let frame = frame::Frame::new(
            exec_ctx,
            user_func.code,
            user_func.exception_table,
            this,
            constructor_call,
        );

        *cur_frame = frame;

        Ok(())
    }

    #[inline]
    pub fn constant_table(&self) -> &constant::ConstantTable {
        &self.code_generator.bytecode_generator.constant_table
    }

    #[inline]
    pub fn memory_allocator(&mut self) -> &mut gc::MemoryAllocator {
        self.code_generator.memory_allocator
    }

    #[inline]
    pub fn object_prototypes(&self) -> &ObjectPrototypes {
        self.code_generator.object_prototypes
    }
}

// Old VM below

pub struct VM {
    pub jit: TracingJit,
    pub state: VMState,
    pub context_stack: Vec<VMState>,
    pub op_table: [fn(&mut VM) -> Result<bool, RuntimeError>; 63],
    pub task_mgr: TaskManager,
    pub is_debug: bool,
    pub jit_on: bool,
    pub gc_on: bool,
    pub codegen: vm_codegen::VMCodeGen,
}

#[derive(Clone, PartialEq, Debug)]
pub struct VMState {
    /// execution stack.
    pub stack: Vec<Value>,
    /// scope stack.
    pub scope: Vec<CallObjectRef>,
    /// try-catch state stack. (TryState, scope_level)
    pub trystate: Vec<(TryState, usize)>,
    /// program counter on the current bytecode.
    pub pc: isize,
    /// bytecode instruction sequence.
    pub iseq: ByteCode,
    /// current FuncId.
    pub cur_func_id: FuncId, // id == 0: main
    /// a name of rest parameters. (if the function has no rest parameters, None.)
    pub rest_params: Option<String>,
    /// a set of the names of parameters corresponding to applied arguments when the function was invoked.
    pub arguments: Vec<(Option<String>, Value)>,
}

impl VMState {
    /// get the value of nth argument in the current context.
    pub fn get_arguments_nth_value(&self, n: usize) -> Result<Value, RuntimeError> {
        let callobj = self.scope.first().unwrap();
        if n < self.arguments.len() {
            match self.arguments[n].0.clone() {
                Some(name) => callobj.get_value(&name),
                None => Ok(self.arguments[n].1.clone()),
            }
        } else {
            Ok(Value::Undefined)
        }
    }

    /// set the nth argument in the current context to val:Value.
    pub fn set_arguments_nth_value(&mut self, n: usize, val: Value) -> Result<(), RuntimeError> {
        let callobj = self.scope.first_mut().unwrap();
        if n < self.arguments.len() {
            let param_name = self.arguments[n].0.clone();
            if let Some(param_name) = param_name {
                callobj.set_local_value(param_name, val)?;
            } else {
                self.arguments[n].1 = val;
            }
        }
        Ok(())
    }

    /// get length of the arguments.
    pub fn get_arguments_length(&self) -> usize {
        self.arguments.len()
    }

    /// get the name of the nth element of func_info.params.
    pub fn get_parameter_nth_name(&self, func_info: FuncInfo, n: usize) -> Option<String> {
        if n < func_info.params.len() {
            return Some(func_info.params[n].0.clone());
        }
        None
    }
}

impl VMState {
    pub fn apply_arguments(
        &mut self,
        func_info: FuncInfo,
        args: &Vec<Value>,
    ) -> Result<(), RuntimeError> {
        for (name, _) in &func_info.params {
            let callobj = self.scope.last_mut().unwrap();
            callobj.set_local_value(name.to_string(), Value::Undefined)?;
            //println!("apply {}", name.to_string());
        }
        let mut rest_args = vec![];
        let mut rest_param_name = None;
        self.arguments.clear();

        for (i, arg) in args.iter().enumerate() {
            if let Some(name) = self.get_parameter_nth_name(func_info.clone(), i) {
                // When rest parameter. TODO: More features of rest parameter
                if func_info.params[i].1 {
                    self.arguments.push((None, arg.clone()));
                    rest_param_name = Some(name);
                    rest_args.push(arg.clone());
                } else {
                    self.arguments.push((Some(name.clone()), arg.clone()));
                    let callobj = self.scope.last_mut().unwrap();
                    callobj.set_local_value(name.clone(), arg.clone())?;
                }
            } else {
                self.arguments.push((None, arg.clone()));
                rest_args.push(arg.clone());
            }
        }

        if let Some(rest_param_name) = rest_param_name {
            let callobj = self.scope.last_mut().unwrap();
            callobj.set_local_value(rest_param_name.clone(), Value::array_from_elems(rest_args))?;
            self.rest_params = Some(rest_param_name);
        };
        Ok(())
    }
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
            TryReturn::Value(val) => {
                let format = val.format(0, false);
                if format.len() > 7 {
                    val.kind()
                } else {
                    format
                }
            }
            TryReturn::Error(err) => err.to_value().to_string(),
            TryReturn::None => "None".to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TryState {
    Try(isize, isize, TryReturn), //position of (CATCH, FINALLY), basal scope_level
    Catch(isize, TryReturn),      //postion of (FINALLY), basal scope_level
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
    pub fn new() -> VM {
        let jit = unsafe { TracingJit::new() };
        let mut global_vals = CallObject::new_global();

        // TODO: Support for 'require' is not enough.
        global_vals
            .set_local_value(
                "require".to_string(),
                Value::default_builtin_function(builtin::require),
            )
            .unwrap();

        let module_exports = Value::object_from_npp(&vec![]);
        global_vals
            .set_local_value("module".to_string(), {
                make_object!(
                    exports:    module_exports.clone()
                )
            })
            .unwrap();
        global_vals
            .set_local_value("exports".to_string(), module_exports)
            .unwrap();

        global_vals
            .set_local_value("console".to_string(), {
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
                                        vec![LLVMPointerType(
                                            LLVMInt8TypeInContext(jit.context),
                                            0,
                                        )]
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
            })
            .unwrap();

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

        global_vals.set_local_value(
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
        ).unwrap();

        global_vals
            .set_local_value(
                "setTimeout".to_string(),
                Value::default_builtin_function(builtin::set_timeout),
            )
            .unwrap();

        global_vals
            .set_local_value(
                "setInterval".to_string(),
                Value::default_builtin_function(builtin::set_interval),
            )
            .unwrap();

        global_vals
            .set_local_value(
                "clearInterval".to_string(),
                Value::default_builtin_function(builtin::clear_timer),
            )
            .unwrap();

        global_vals
            .set_local_value(
                "clearTimeout".to_string(),
                Value::default_builtin_function(builtin::clear_timer),
            )
            .unwrap();

        global_vals
            .set_local_value(
                "__enableJit".to_string(),
                Value::default_builtin_function(builtin::enable_jit),
            )
            .unwrap();

        global_vals
            .set_local_value(
                "__assert".to_string(),
                Value::default_builtin_function(builtin::assert_seq),
            )
            .unwrap();

        global_vals
            .set_local_value("Object".to_string(), builtins::object::init())
            .unwrap();
        global_vals
            .set_local_value("Error".to_string(), builtins::error::init())
            .unwrap();
        global_vals
            .set_local_value("Function".to_string(), builtins::function::init())
            .unwrap();
        global_vals
            .set_local_value("Array".to_string(), builtins::array::init())
            .unwrap();
        use builtins::date::DATE_OBJ;
        global_vals
            .set_local_value("Date".to_string(), DATE_OBJ.with(|x| x.clone()))
            .unwrap();
        global_vals
            .set_local_value("Math".to_string(), builtins::math::init(jit.clone()))
            .unwrap();
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
                stack: Vec::with_capacity(128),
                scope: vec![global_vals.clone()],
                trystate: vec![(TryState::None, 1)],
                cur_func_id: 0,
                pc: 0isize,
                iseq: vec![],
                rest_params: None,
                arguments: vec![],
            },
            context_stack: vec![],
            task_mgr: TaskManager::new(),
            is_debug: false,
            jit_on: true,
            gc_on: true,
            codegen: vm_codegen::VMCodeGen::new(global_vals.clone()),
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
                decl_const,
                decl_let,
                not,
                jmp_unwind,
            ],
        }
    }
}

impl VM {
    pub fn run(&mut self, iseq: ByteCode) -> Result<(), RuntimeError> {
        self.state.iseq = iseq;
        self.state.pc = 0;
        let res = self.do_run();
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

    /// store vm.state to the context_stack, and initialize vm.state.
    pub fn store_state(&mut self) {
        self.context_stack.push(self.state.clone());
        self.state = VMState {
            stack: vec![],
            scope: vec![],
            trystate: vec![],
            pc: 0,
            iseq: vec![],
            cur_func_id: 0,
            rest_params: None,
            arguments: vec![],
        };
    }

    /// restore vm.state from the context_stack.
    /// push the returned value to the execute stack of the restored vm.state.
    pub fn restore_state(&mut self) {
        if let Some(state) = self.context_stack.pop() {
            if self.is_debug {
                print!("stack trace: ");
                for (n, v) in self.state.stack.iter().enumerate() {
                    let format = v.format(1, false);
                    let format = if format.len() > 20 { v.kind() } else { format };
                    if n == 0 {
                        print!("{}", format);
                    } else {
                        print!(" | {}", format);
                    }
                }
                println!();
            }
            let top = self.state.stack.pop();
            self.state = state;
            if let Some(top) = top {
                self.state.stack.push(top);
            } else {
                self.state.stack.push(Value::Undefined);
            }
        } else {
            unreachable!("context stack abnormaly exhaust.")
        }
    }

    /// main execution loop
    pub fn do_run(&mut self) -> Result<(), RuntimeError> {
        if self.is_debug {
            println!("ENTER NEW CONTEXT");
            println!(
                " {:<4} {:<25} {:<8} {:<8} {:<5} {:<5} {:<5} {:<16}",
                "PC".to_string(),
                "INST".to_string(),
                "tryst".to_string(),
                "tryret".to_string(),
                "ctx".to_string(),
                "scope".to_string(),
                "stack".to_string(),
                "top".to_string(),
            );
        }
        self.state
            .trystate
            .push((TryState::None, self.state.scope.len()));
        loop {
            let code = self.state.iseq[self.state.pc as usize];
            if self.is_debug {
                let trystate = self.state.trystate.last().unwrap();
                let ctxlen = self.context_stack.len();
                let scopelen = self.state.scope.len();
                let stacklen = self.state.stack.len();
                let stacktop = self.state.stack.last();
                print!(" ");
                bytecode_gen::show_inst(
                    &self.state.iseq,
                    self.state.pc as usize,
                    &self.codegen.bytecode_gen.const_table,
                );
                print!(
                    " {:<8} {:<8} {:<5} {:<5} {:<5} {:<16} ",
                    trystate.0.to_string().0,
                    trystate.0.to_string().1,
                    ctxlen,
                    scopelen,
                    stacklen,
                    match stacktop {
                        Some(val) => {
                            let format = val.format(0, false);
                            if format.len() > 15 {
                                val.kind()
                            } else {
                                format
                            }
                        }
                        None => "".to_string(),
                    },
                );
                println!();
            }
            match self.op_table[code as usize](self) {
                Ok(true) => {
                    continue;
                }

                Ok(false) => {
                    // END or RETURN or, LEAVE_TRY with no error.
                    self.state.trystate.pop().unwrap();
                    if self.is_debug {
                        println!("EXIT CONTEXT");
                    };
                    return Ok(());
                }

                Err(err) => {
                    // Runtime error or THROW in try-catch.
                    let mut error: Option<RuntimeError> = None;
                    {
                        let trystate = self.state.trystate.last_mut().unwrap();
                        match trystate.clone() {
                            (TryState::Try(to_catch, to_finally, ret), scope_level) => {
                                self.state.pc = to_catch;
                                self.state.scope.truncate(scope_level);
                                // push error object to exec stack.
                                let err_obj = err.to_value();
                                self.state.stack.push(err_obj);
                                *trystate = (TryState::Catch(to_finally, ret), scope_level);
                            }
                            (TryState::Catch(to_finally, ret), scope_level) => {
                                assert_eq!(ret, TryReturn::None);
                                self.state.pc = to_finally;
                                self.state.scope.truncate(scope_level);
                                *trystate = (TryState::Finally(TryReturn::Error(err)), scope_level);
                            }
                            (TryState::None, _) | (TryState::Finally(_), _) => {
                                error = Some(err);
                            }
                        };
                    }
                    if let Some(err) = error {
                        match self.state.trystate.pop().unwrap() {
                            (TryState::Finally(_), _) => {}
                            x => self.state.trystate.push(x),
                        }
                        self.state.trystate.pop().unwrap();
                        // must push return value to exec stack.
                        self.set_return_value(Value::Undefined);
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
    ) -> Result<(), RuntimeError> {
        //println!("{}", callee.format(1, true));
        match callee {
            Value::Object(_, ObjectKind::BuiltinFunction(box (ref info, callobj))) => {
                (info.func)(self, args, callobj.clone())?;
                return Ok(());
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
    ($self:ident, $var:ident, $ty:ty) => {
        let iseq = &$self.state.iseq;
        let $var = iseq[$self.state.pc as usize] as $ty;
        $self.state.pc += 1;
    };
}

macro_rules! get_int32 {
    ($self:ident, $var:ident, $ty:ty) => {
        let $var = (($self.state.iseq[$self.state.pc as usize + 3] as $ty) << 24)
            + (($self.state.iseq[$self.state.pc as usize + 2] as $ty) << 16)
            + (($self.state.iseq[$self.state.pc as usize + 1] as $ty) << 8)
            + ($self.state.iseq[$self.state.pc as usize + 0] as $ty);
        $self.state.pc += 4;
    };
}

fn end(_self: &mut VM) -> Result<bool, RuntimeError> {
    Ok(false)
}

fn create_context(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // create_context
    Ok(true)
}

fn construct(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // construct
    let callee = self_.state.stack.pop().unwrap();
    let mut args = vec![];
    {
        get_int32!(self_, argc, usize);
        for _ in 0..argc {
            args.push(self_.state.stack.pop().unwrap());
        }
    };
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
            let callobj = callobj.new_callobj_from_func(Some(new_this.clone()));
            self_.store_state();
            self_.state.scope.push(callobj);
            self_.state.apply_arguments(func_info.clone(), &args)?;
            self_.state.iseq = func_info.iseq;
            let res = match self_.do_run() {
                Ok(()) => Ok(true),
                Err(err) => Err(err),
            };

            {
                let ret = self_.state.stack.last_mut().unwrap();
                match &ret {
                    &Value::Object(_, _) => {}
                    _ => *ret = new_this,
                };
            };
            self_.restore_state();
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

fn call(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // Call
    let callee = self_.state.stack.pop().unwrap();
    let mut args = vec![];
    {
        get_int32!(self_, argc, usize);
        for _ in 0..argc {
            args.push(self_.state.stack.pop().unwrap());
        }
    };
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
) -> Result<(), RuntimeError> {
    let argc = args.len();
    let args_all_numbers = args.iter().all(|val| match val {
        Value::Number(_) => true,
        _ => false,
    });

    let callobj = callobj.new_callobj_from_func(None);
    self_.store_state();
    self_.state.scope.push(callobj);
    self_.state.apply_arguments(func_info.clone(), args)?;

    let FuncInfo { func_id, iseq, .. } = func_info.clone();

    if args_all_numbers && self_.jit_on {
        if let Some(f) = unsafe {
            self_.jit.can_jit(
                func_info,
                self_.state.clone(),
                &self_.codegen.bytecode_gen.const_table,
                argc,
            )
        } {
            self_
                .state
                .stack
                .push(unsafe { self_.jit.run_llvm_func(func_id, f, &args) });
            self_.restore_state();
            return Ok(());
        }
    }
    self_.state.iseq = iseq;
    let res = self_.do_run();

    if self_.jit_on {
        self_
            .jit
            .record_function_return_type(func_id, self_.state.stack.last().unwrap());
    };
    self_.restore_state();
    res
}

fn create_object(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // create_object
    let mut npp = vec![];
    {
        get_int32!(self_, len, usize);
        for _ in 0..len {
            let name = if let Value::String(name) = self_.state.stack.pop().unwrap() {
                name.into_string().unwrap()
            } else {
                unreachable!()
            };
            let val = self_.state.stack.pop().unwrap();
            npp.push((name, Property::new(val.clone())));
        }
    };
    self_.state.stack.push(Value::object_from_npp(&npp));

    gc::mark_and_sweep(self_);

    Ok(true)
}

fn create_array(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // create_array
    let mut arr = vec![];

    get_int32!(self_, len, usize);
    for _ in 0..len {
        let val = self_.state.stack.pop().unwrap();
        arr.push(val);
    }

    self_.state.stack.push(Value::array_from_elems(arr));

    gc::mark_and_sweep(self_);

    Ok(true)
}

fn push_int8(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_int
    get_int8!(self_, n, i8);
    self_.state.stack.push(Value::Number(n as f64));
    Ok(true)
}

fn push_int32(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_int
    get_int32!(self_, n, i32);
    self_.state.stack.push(Value::Number(n as f64));
    Ok(true)
}

fn push_false(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_false
    self_.state.stack.push(Value::Bool(false));
    Ok(true)
}

fn push_true(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_true
    self_.state.stack.push(Value::Bool(true));
    Ok(true)
}

fn push_const(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_const
    get_int32!(self_, n, usize);
    self_
        .state
        .stack
        .push(self_.codegen.bytecode_gen.const_table.value[n].clone());
    Ok(true)
}

fn push_this(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_this
    let this = self_.state.scope.last().unwrap().this.clone();
    self_.state.stack.push(*this);
    Ok(true)
}

fn push_arguments(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_arguments
    let state = self_.state.clone();
    self_.state.stack.push(Value::arguments(state));
    Ok(true)
}

fn push_undefined(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // push_defined
    self_.state.stack.push(Value::Undefined);
    Ok(true)
}

fn lnot(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // lnot
    let expr = self_.state.stack.last_mut().unwrap();
    *expr = Value::Bool(!expr.to_boolean());
    Ok(true)
}

fn posi(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // posi
    let expr = self_.state.stack.last_mut().unwrap();
    *expr = Value::Number(expr.to_number());
    Ok(true)
}

fn neg(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // neg
    let expr = self_.state.stack.last_mut().unwrap();
    *expr = match *expr {
        Value::Number(n) => Value::Number(-n),
        _ => return Err(RuntimeError::Unimplemented),
    };
    Ok(true)
}

fn add(self_: &mut VM) -> Result<bool, RuntimeError> {
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

fn sub(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number(l - r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn mul(self_: &mut VM) -> Result<bool, RuntimeError> {
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

fn div(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number(l / r),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn rem(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match (lhs, rhs) {
        (Value::Number(l), Value::Number(r)) => Value::Number((l as i64 % r as i64) as f64),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn lt(self_: &mut VM) -> Result<bool, RuntimeError> {
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

fn gt(self_: &mut VM) -> Result<bool, RuntimeError> {
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

fn le(self_: &mut VM) -> Result<bool, RuntimeError> {
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

fn ge(self_: &mut VM) -> Result<bool, RuntimeError> {
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
fn eq(self_: &mut VM) -> Result<bool, RuntimeError> {
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
fn ne(self_: &mut VM) -> Result<bool, RuntimeError> {
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
fn seq(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(Value::Bool(lhs.strict_equal(rhs)?));
    Ok(true)
}

// TODO: Need more precise implemention
fn sne(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let rhs = self_.state.stack.pop().unwrap();
    let lhs = self_.state.stack.pop().unwrap();
    self_.state.stack.push(Value::Bool(!lhs.strict_equal(rhs)?));
    Ok(true)
}

fn and(self_: &mut VM) -> Result<bool, RuntimeError> {
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

fn not(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // $name
    let expr = self_.state.stack.pop().unwrap();
    self_.state.stack.push(match expr {
        Value::Number(expr) => Value::Number(!(expr as i64 as i32) as f64),
        _ => return Err(RuntimeError::Unimplemented),
    });
    Ok(true)
}

fn or(self_: &mut VM) -> Result<bool, RuntimeError> {
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

fn xor(self_: &mut VM) -> Result<bool, RuntimeError> {
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

fn shl(self_: &mut VM) -> Result<bool, RuntimeError> {
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

fn shr(self_: &mut VM) -> Result<bool, RuntimeError> {
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

fn zfshr(self_: &mut VM) -> Result<bool, RuntimeError> {
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

fn get_member(self_: &mut VM) -> Result<bool, RuntimeError> {
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

fn set_member(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // get_global
    let member = self_.state.stack.pop().unwrap();
    let mut parent = self_.state.stack.pop().unwrap().clone();
    let val = self_.state.stack.pop().unwrap();
    parent.set_property(member, val, Some(self_.state.scope.last().unwrap().clone()))?;
    Ok(true)
}

fn jmp(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // jmp
    get_int32!(self_, dst, i32);
    self_.state.pc += dst as isize;
    Ok(true)
}

fn jmp_if_false(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // jmp_if_false
    get_int32!(self_, dst, i32);
    let cond = self_.state.stack.pop().unwrap().to_boolean();
    if !cond {
        self_.state.pc += dst as isize
    }
    Ok(true)
}

fn return_(_self: &mut VM) -> Result<bool, RuntimeError> {
    Ok(false)
}

fn throw(self_: &mut VM) -> Result<bool, RuntimeError> {
    let val = self_.state.stack.pop().unwrap().clone();
    Err(RuntimeError::Exception(val))
}

fn enter_try(self_: &mut VM) -> Result<bool, RuntimeError> {
    let pc = self_.state.pc;
    self_.state.pc += 1;
    get_int32!(self_, to_catch, isize);
    get_int32!(self_, to_finally, isize);
    get_int32!(self_, scope_level, usize);
    self_.state.trystate.push((
        TryState::Try(pc + to_catch, pc + to_finally, TryReturn::None),
        scope_level,
    ));
    Ok(true)
}

fn leave_try(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    let trystate = self_.state.trystate.pop().unwrap();
    match trystate {
        (TryState::Finally(TryReturn::Error(err)), _) => Err(err.clone()),
        (TryState::Finally(TryReturn::Value(val)), _) => {
            if self_.state.trystate.len() == 0 {
                self_.state.stack.push(val);
                return Ok(false);
            };
            if let Some((TryState::None, _)) = self_.state.trystate.last() {
                self_.state.stack.push(val);
                return Ok(false);
            };
            let trystate = self_.state.trystate.last_mut().unwrap();
            match trystate.0.clone() {
                TryState::Try(_, to_finally, _) | TryState::Catch(to_finally, _) => {
                    self_.state.pc = to_finally;
                    *trystate = (TryState::Finally(TryReturn::Value(val)), trystate.1);
                }
                TryState::Finally(ref mut ret) => *ret = TryReturn::Value(val),
                _ => unreachable!(),
            };
            Ok(true)
        }
        _ => Ok(true),
    }
}

fn catch(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    let trystate = self_.state.trystate.last_mut().unwrap();
    match trystate.0 {
        TryState::Catch(_, _) => {}
        _ => unreachable!("catch(): invalid trystate."),
    };

    Ok(true)
}

fn finally(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    let trystate = self_.state.trystate.last_mut().unwrap();
    match trystate.0.clone() {
        TryState::Finally(_) => {}
        TryState::Try(_, _, x) => {
            *trystate = (TryState::Finally(x.clone()), trystate.1);
        }
        TryState::Catch(_, x) => {
            *trystate = (TryState::Finally(x.clone()), trystate.1);
        }
        _ => unreachable!("finally(): invalid trystate."),
    };
    Ok(true)
}

/// return in try-catch. return after execute finally clause.
fn return_try(self_: &mut VM) -> Result<bool, RuntimeError> {
    let pc = self_.state.pc;
    self_.state.pc += 1;
    get_int32!(self_, to_finally, isize);
    let val = self_.state.stack.pop().unwrap();
    let trystate = self_.state.trystate.last_mut().unwrap();
    match trystate.0 {
        TryState::Finally(_) => {}
        TryState::Try(_, _, _) | TryState::Catch(_, _) => {
            *trystate = (TryState::Finally(TryReturn::Value(val)), trystate.1);
        }
        _ => unreachable!(),
    };
    self_.state.scope.truncate(trystate.1);
    self_.state.pc = pc + to_finally;
    Ok(true)
}

fn push_scope(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    let mut callobj = CallObject::new_with_this(Value::object_from_npp(&vec![]));

    let base_callobj = self_.state.scope.last().unwrap().clone();

    callobj.parent = Some(base_callobj.clone());
    callobj.this = base_callobj.this.clone();

    self_.state.scope.push(callobj);
    Ok(true)
}

fn pop_scope(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    self_.state.scope.pop();
    Ok(true)
}

fn double(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // double
    let stack_top_val = self_.state.stack.last().unwrap().clone();
    self_.state.stack.push(stack_top_val);
    Ok(true)
}

fn pop(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // double
    self_.state.stack.pop();
    Ok(true)
}

// 'land' and 'lor' are for JIT compiler. Nope for VM.

fn land(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // land
    Ok(true)
}

fn lor(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // lor
    Ok(true)
}

fn update_parent_scope(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    if let Some(Value::Object(_, ObjectKind::Function(box (_, ref mut callobj)))) =
        self_.state.stack.last_mut()
    {
        callobj.parent = Some(self_.state.scope.last().unwrap().clone());
    }
    Ok(true)
}

fn get_value(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, name_id, usize);
    let name = &self_.codegen.bytecode_gen.const_table.string[name_id];
    let val = self_.state.scope.last().unwrap().get_value(name)?;
    if val == Value::Uninitialized {
        return Err(RuntimeError::Reference(format!(
            "reference error: '{}' is not defined",
            name
        )));
    }
    self_.state.stack.push(val);
    Ok(true)
}

fn set_value(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, name_id, usize);
    let name = self_.codegen.bytecode_gen.const_table.string[name_id].clone();
    let mut val = self_.state.stack.pop().unwrap();

    // We have to change cobj.this to the current scope one. (./examples/this.js)
    if let Value::Object(_, ObjectKind::Function(box (_, ref mut cobj)))
    | Value::Object(_, ObjectKind::BuiltinFunction(box (_, ref mut cobj))) = &mut val
    {
        cobj.this = self_.state.scope.last().unwrap().this.clone();
    }

    self_.state.scope.last_mut().unwrap().set_value(name, val)?;

    Ok(true)
}

fn decl_var(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, name_id, usize);
    let name = self_.codegen.bytecode_gen.const_table.string[name_id].clone();
    (*self_.state.scope.last_mut().unwrap()).set_local_value(name, Value::Undefined)?;
    Ok(true)
}

fn decl_const(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, name_id, usize);
    let name = self_.codegen.bytecode_gen.const_table.string[name_id].clone();
    let env = &mut self_.state.scope.last_mut().unwrap();
    env.set_local_value(name.clone(), Value::Uninitialized)?;
    env.set_mutability(name, false);
    Ok(true)
}

fn decl_let(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    get_int32!(self_, name_id, usize);
    let name = self_.codegen.bytecode_gen.const_table.string[name_id].clone();
    (*self_.state.scope.last_mut().unwrap()).set_local_value(name, Value::Uninitialized)?;
    Ok(true)
}

// 'cond_op' is for JIT compiler. Nope for VM.
fn cond_op(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1;
    Ok(true)
}

fn loop_start(self_: &mut VM) -> Result<bool, RuntimeError> {
    let loop_start = self_.state.pc as usize;

    self_.state.pc += 1;
    get_int32!(self_, relatinal_loop_end, usize);
    let loop_end = loop_start + relatinal_loop_end;

    let id = self_.state.cur_func_id;

    if self_.jit_on {
        if let Some(pc) = unsafe {
            self_.jit.can_loop_jit(
                id,
                &self_.codegen.bytecode_gen.const_table,
                &mut self_.state,
                loop_start,
                loop_end,
            )?
        } {
            self_.state.pc = pc;
        }
    }

    Ok(true)
}

fn jmp_unwind(self_: &mut VM) -> Result<bool, RuntimeError> {
    self_.state.pc += 1; // jmp_unwind
    get_int32!(self_, dst, i32);
    let pc = self_.state.pc;
    get_int32!(self_, scope_count, u32);
    get_int32!(self_, try_count, u32);
    for _ in 0..scope_count {
        self_.state.scope.pop();
    }
    for _ in 0..try_count {
        self_.state.trystate.pop();
    }
    self_.state.pc = pc + dst as isize;
    Ok(true)
}
