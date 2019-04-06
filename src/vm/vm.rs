use super::super::node::Node;
use super::{
    callobj::CallObject,
    codegen,
    codegen::CodeGenerator,
    constant,
    error::*,
    frame,
    jsvalue::function::{DestinationKind, ThisMode},
    jsvalue::prototype::ObjectPrototypes,
    jsvalue::symbol::GlobalSymbolRegistry,
    jsvalue::value::*,
    task::{Task, TaskManager, TimerKind},
    value::*,
};
use builtin;
use builtin::BuiltinJITFuncInfo;
use builtins;
use bytecode_gen;
use bytecode_gen::show_inst2;
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

pub struct VM2 {
    pub global_environment: frame::LexicalEnvironmentRef,
    pub memory_allocator: gc::MemoryAllocator,
    pub object_prototypes: ObjectPrototypes,
    pub constant_table: constant::ConstantTable,
    pub global_symbol_registry: GlobalSymbolRegistry,
    pub stack: Vec<BoxedValue>,
    pub saved_frame: Vec<frame::Frame>,
}

macro_rules! gc_lock {
    ($vm:ident, $targets:expr, $($body:tt)*) => { {
        for arg in $targets { $vm.memory_allocator.lock(*arg) }
        let ret = { $($body)* };
        for arg in $targets { $vm.memory_allocator.unlock(*arg) }
        ret
    } }
}

impl VM2 {
    pub fn new() -> Self {
        let mut memory_allocator = gc::MemoryAllocator::new();
        let object_prototypes = ObjectPrototypes::new(&mut memory_allocator);
        let global_env = frame::LexicalEnvironment::new_global_initialized(
            &mut memory_allocator,
            &object_prototypes,
        );
        let global_environment = memory_allocator.alloc(global_env);
        VM2 {
            global_environment,
            memory_allocator,
            object_prototypes,
            constant_table: constant::ConstantTable::new(),
            global_symbol_registry: GlobalSymbolRegistry::new(),
            stack: vec![],
            saved_frame: vec![],
        }
    }

    pub fn compile(
        &mut self,
        node: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> Result<codegen::FunctionInfo, codegen::Error> {
        let mut code_generator = CodeGenerator::new(
            &mut self.constant_table,
            &mut self.memory_allocator,
            &self.object_prototypes,
        );
        code_generator.compile(node, iseq, use_value)
    }

    pub fn create_global_frame(
        &mut self,
        global_info: codegen::FunctionInfo,
        iseq: ByteCode,
    ) -> frame::Frame {
        let global_env_ref = self.global_environment;

        let var_env = self.memory_allocator.alloc(frame::LexicalEnvironment {
            record: frame::EnvironmentRecord::Declarative({
                let mut record = FxHashMap::default();
                for name in global_info.var_names {
                    record.insert(name, Value2::undefined());
                }
                record
            }),
            outer: Some(global_env_ref),
        });

        let lex_env = self.memory_allocator.alloc(frame::LexicalEnvironment {
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
            let mut val = val.copy_object(&mut self.memory_allocator);
            let name = val.as_function().name.clone().unwrap();
            val.set_function_outer_environment(lex_env);
            unsafe { &mut *lex_env }.set_value(name, val).unwrap();
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
            unsafe { &*global_env_ref }.get_global_object(),
            false,
        );

        frame
    }

    pub fn run_global(&mut self, global_info: codegen::FunctionInfo, iseq: ByteCode) -> VMResult {
        let global_env_ref = self.global_environment;

        let var_env = self.memory_allocator.alloc(frame::LexicalEnvironment {
            record: frame::EnvironmentRecord::Declarative({
                let mut record = FxHashMap::default();
                for name in global_info.var_names {
                    record.insert(name, Value2::undefined());
                }
                record
            }),
            outer: Some(global_env_ref),
        });

        let lex_env = self.memory_allocator.alloc(frame::LexicalEnvironment {
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
            let mut val = val.copy_object(&mut self.memory_allocator);
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
            unsafe { &*global_env_ref }.get_global_object(),
            false,
        );

        self.run(frame)?;

        Ok(())
    }

    pub fn call_function(
        &mut self,
        callee: Value2,
        args: &[Value2],
        this: Value2,
        cur_frame: &frame::Frame,
    ) -> VMResult {
        if !callee.is_function_object() {
            return Err(RuntimeError::Type("Not a function".to_string()));
        }

        let info = callee.as_function();

        match info.kind {
            FunctionObjectKind::Builtin(func) => gc_lock!(
                self,
                args,
                func(self, args, &frame::Frame::new_empty_with_this(this, false))
            ),
            FunctionObjectKind::User(ref user_func) => {
                self.call_user_function(user_func, args, this, cur_frame, false)
            }
        }
    }

    fn call_user_function(
        &mut self,
        user_func: &UserFunctionInfo,
        args: &[Value2],
        this: Value2,
        cur_frame: &frame::Frame,
        constructor_call: bool,
    ) -> VMResult {
        self.saved_frame
            .push(cur_frame.clone().saved_stack_len(self.stack.len()));

        let this = if user_func.this_mode == ThisMode::Lexical {
            // Arrow function
            unsafe { &*user_func.outer.unwrap() }.get_this_binding()
        } else {
            this
        };

        let var_env_ref = self.create_function_environment(
            |vm, record| {
                for name in &user_func.var_names {
                    record.insert(name.clone(), Value2::undefined());
                }

                for (i, FunctionParameter { name, rest_param }) in
                    user_func.params.iter().enumerate()
                {
                    record.insert(
                        name.clone(),
                        if *rest_param {
                            Value2::array(
                                &mut vm.memory_allocator,
                                &vm.object_prototypes,
                                (*args)
                                    .get(i..)
                                    .unwrap_or(&vec![])
                                    .iter()
                                    .map(|elem| Property2::new_data_simple(*elem))
                                    .collect::<Vec<Property2>>(),
                            )
                        } else {
                            *args.get(i).unwrap_or(&Value2::undefined())
                        },
                    );
                }
            },
            this,
            user_func.outer,
        );

        let lex_env_ref = self.create_declarative_environment(
            |_, record| {
                for name in &user_func.lex_names {
                    record.insert(name.clone(), Value2::uninitialized());
                }
            },
            Some(var_env_ref),
        );

        for func in &user_func.func_decls {
            let mut func = func.copy_object(&mut self.memory_allocator);
            let name = func.as_function().name.clone().unwrap();
            func.set_function_outer_environment(lex_env_ref);
            unsafe { &mut *lex_env_ref }.set_value(name, func)?;
        }

        let exec_ctx = frame::ExecutionContext {
            variable_environment: var_env_ref,
            lexical_environment: lex_env_ref,
            saved_lexical_environment: vec![],
        };

        let frame = frame::Frame::new(
            exec_ctx,
            user_func.code.clone(),
            user_func.exception_table.clone(),
            this,
            constructor_call,
        )
        .escape();

        self.run(frame)
    }

    fn get_property_to_stack_top(
        &mut self,
        parent: Value2,
        key: Value2,
        cur_frame: &mut frame::Frame,
    ) -> VMResult {
        let val = parent.get_property(&mut self.memory_allocator, &self.object_prototypes, key)?;
        match val {
            Property2::Data(DataProperty { val, .. }) => {
                self.stack.push(val.into());
                Ok(())
            }
            Property2::Accessor(AccessorProperty { get, .. }) => {
                if get.is_undefined() {
                    self.stack.push(Value2::undefined().into());
                    return Ok(());
                }
                self.enter_function(get, &[], parent, cur_frame, false)
            }
        }
    }

    pub fn get_property(
        &mut self,
        parent: Value2,
        key: Value2,
        cur_frame: &frame::Frame,
    ) -> Result<Value2, RuntimeError> {
        let val = parent.get_property(&mut self.memory_allocator, &self.object_prototypes, key)?;
        match val {
            Property2::Data(DataProperty { val, .. }) => Ok(val),
            Property2::Accessor(AccessorProperty { get, .. }) => {
                if get.is_undefined() {
                    return Ok(Value2::undefined());
                }
                self.call_function(get, &[], parent, cur_frame)?;
                Ok(self.stack.pop().unwrap().into(): Value2)
            }
        }
    }

    pub fn set_property(
        &mut self,
        parent: Value2,
        key: Value2,
        val: Value2,
        cur_frame: &frame::Frame,
    ) -> VMResult {
        let maybe_setter = parent.set_property(key, val)?;
        if let Some(setter) = maybe_setter {
            self.call_function(setter, &[val], parent, cur_frame)?;
            self.stack.pop().unwrap(); // Pop undefined (setter's return value)
        }
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

impl VM2 {
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

        macro_rules! type_error {
            ($msg:expr) => {{
                let val =
                    RuntimeError::Type($msg.to_string()).to_value2(&mut self.memory_allocator);
                self.stack.push(val.into());
                exception!();
                continue;
            }};
        }

        macro_rules! etry {
            ($val:expr) => {{
                match $val {
                    Ok(ok) => ok,
                    Err(err) => {
                        let val = err.to_value2(&mut self.memory_allocator);
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
                    self.stack
                        .push(lhs.add(&mut self.memory_allocator, rhs).into());
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
                VMInst::DIV => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.div(rhs).into());
                }
                VMInst::REM => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.rem(rhs).into());
                }
                VMInst::EQ => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.eq(rhs).into());
                }
                VMInst::SEQ => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.strict_eq(rhs).into());
                }
                VMInst::NE => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.ne(rhs).into());
                }
                VMInst::SNE => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.strict_ne(rhs).into());
                }
                VMInst::LT => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.lt(rhs).into());
                }
                VMInst::LE => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.le(rhs).into());
                }
                VMInst::GT => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(rhs.lt(lhs).into());
                }
                VMInst::GE => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(rhs.le(lhs).into());
                }
                VMInst::AND => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(rhs.and(lhs).into());
                }
                VMInst::OR => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(rhs.or(lhs).into());
                }
                VMInst::XOR => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(rhs.xor(lhs).into());
                }
                VMInst::NOT => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(rhs.not().into());
                }
                VMInst::SHL => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.shift_l(rhs).into());
                }
                VMInst::SHR => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.shift_r(rhs).into());
                }
                VMInst::ZFSHR => {
                    cur_frame.pc += 1;
                    let rhs: Value2 = self.stack.pop().unwrap().into();
                    let lhs: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.z_shift_r(rhs).into());
                }
                VMInst::NEG => {
                    cur_frame.pc += 1;
                    let val: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(val.minus().into());
                }
                VMInst::POSI => {
                    cur_frame.pc += 1;
                    let val: Value2 = self.stack.pop().unwrap().into();
                    self.stack.push(val.positive().into());
                }
                VMInst::LNOT => {
                    cur_frame.pc += 1;
                    let val: Value2 = self.stack.pop().unwrap().into();
                    let res = Value2::bool(!val.to_boolean());
                    self.stack.push(res.into());
                }
                VMInst::PUSH_INT8 => {
                    cur_frame.pc += 1;
                    read_int8!(cur_frame.bytecode, cur_frame.pc, num, f64);
                    self.stack.push(Value2::Number(num).into());
                }
                VMInst::PUSH_INT32 => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, num, i32);
                    self.stack.push(Value2::Number(num as f64).into());
                }
                VMInst::PUSH_CONST => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, id, usize);
                    let val = *self.constant_table.get(id).as_value();
                    self.stack.push(val.into());
                }
                VMInst::PUSH_NULL => {
                    cur_frame.pc += 1;
                    self.stack.push(Value2::null().into());
                }
                VMInst::PUSH_UNDEFINED => {
                    cur_frame.pc += 1;
                    self.stack.push(Value2::undefined().into());
                }
                VMInst::PUSH_THIS => {
                    cur_frame.pc += 1;
                    self.stack.push(cur_frame.this.into());
                }
                VMInst::PUSH_FALSE => {
                    cur_frame.pc += 1;
                    self.stack.push(Value2::Bool(0).into());
                }
                VMInst::PUSH_TRUE => {
                    cur_frame.pc += 1;
                    self.stack.push(Value2::Bool(1).into());
                }
                VMInst::GET_MEMBER => {
                    cur_frame.pc += 1;
                    let property: Value2 = self.stack.pop().unwrap().into();
                    let parent: Value2 = self.stack.pop().unwrap().into();
                    etry!(self.get_property_to_stack_top(parent, property, &mut cur_frame))
                }
                VMInst::SET_MEMBER => {
                    cur_frame.pc += 1;
                    let property: Value2 = self.stack.pop().unwrap().into();
                    let parent: Value2 = self.stack.pop().unwrap().into();
                    let val: Value2 = self.stack.pop().unwrap().into();
                    etry!(self.set_property(parent, property, val, &cur_frame))
                }
                VMInst::SET_VALUE => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, name_id, usize);
                    let val = self.stack.pop().unwrap();
                    let name = self.constant_table.get(name_id).as_string().clone();
                    etry!(cur_frame.lex_env_mut().set_value(name, val.into()));
                }
                VMInst::GET_VALUE => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, name_id, usize);
                    let val = etry!(cur_frame
                        .lex_env()
                        .get_value(self.constant_table.get(name_id).as_string()));
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
                    self.enter_constructor(callee, &args, &mut cur_frame)?;
                }
                VMInst::CALL => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, argc, usize);
                    let callee: Value2 = self.stack.pop().unwrap().into();
                    let mut args: Vec<Value2> = vec![];
                    for _ in 0..argc {
                        args.push(self.stack.pop().unwrap().into());
                    }
                    etry!(self.enter_function(callee, &args, cur_frame.this, &mut cur_frame, false))
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
                    let callee = match etry!(parent.get_property(
                        &mut self.memory_allocator,
                        &self.object_prototypes,
                        method
                    )) {
                        Property2::Data(DataProperty { val, .. }) => val,
                        _ => type_error!("Not a function"),
                    };
                    etry!(self.enter_function(callee, &args, parent, &mut cur_frame, false))
                }
                VMInst::SET_OUTER_ENV => {
                    cur_frame.pc += 1;
                    let func_template: Value2 = self.stack.pop().unwrap().into();
                    let mut func = func_template.copy_object(&mut self.memory_allocator);
                    func.set_function_outer_environment(
                        cur_frame.execution_context.lexical_environment,
                    );
                    self.stack.push(func.into());
                }
                VMInst::CREATE_OBJECT => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, id, usize);
                    self.create_object(id)?;
                    self.memory_allocator.mark(
                        self.global_environment,
                        &self.object_prototypes,
                        &self.constant_table,
                        &self.stack,
                        &cur_frame,
                        &self.saved_frame,
                    );
                }
                VMInst::CREATE_ARRAY => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, len, usize);
                    self.create_array(len)?;
                    self.memory_allocator.mark(
                        self.global_environment,
                        &self.object_prototypes,
                        &self.constant_table,
                        &self.stack,
                        &cur_frame,
                        &self.saved_frame,
                    );
                }
                VMInst::DOUBLE => {
                    cur_frame.pc += 1;
                    let val = *self.stack.last().unwrap();
                    self.stack.push(val);
                }
                VMInst::PUSH_ENV => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, id, usize);
                    self.push_env(id, &mut cur_frame)?;
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
                    let escape = cur_frame.escape;
                    self.unwind_frame_saving_stack_top(&mut cur_frame);
                    // TODO: GC schedule
                    self.memory_allocator.mark(
                        self.global_environment,
                        &self.object_prototypes,
                        &self.constant_table,
                        &self.stack,
                        &cur_frame,
                        &self.saved_frame,
                    );
                    if escape {
                        break;
                    }
                }
                VMInst::TYPEOF => {
                    cur_frame.pc += 1;
                    let val: Value2 = self.stack.pop().unwrap().into();
                    let type_str = val.type_of();
                    let type_str_val =
                        Value2::string(&mut self.memory_allocator, type_str.to_string());
                    self.stack.push(type_str_val.into());
                }
                VMInst::END => break,
                _ => {
                    print!("Not yet implemented VMInst: ");
                    show_inst2(&cur_frame.bytecode, cur_frame.pc, &self.constant_table);
                    println!();
                    unimplemented!();
                }
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
            self.stack.push(cur_frame.this.into());
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

    fn push_env(&mut self, id: usize, cur_frame: &mut frame::Frame) -> VMResult {
        let lex_names = self.constant_table.get(id).as_lex_env_info();
        let mut record = FxHashMap::default();
        for name in lex_names {
            record.insert(name.clone(), Value2::uninitialized());
        }

        let lex_env = self.memory_allocator.alloc(frame::LexicalEnvironment {
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

    fn create_object(&mut self, id: usize) -> VMResult {
        let (len, special_properties) = self.constant_table.get(id).as_object_literal_info();
        let mut properties = FxHashMap::default();

        for i in 0..len {
            let prop: Value2 = self.stack.pop().unwrap().into();
            let name = prop.to_string();
            let val: Value2 = self.stack.pop().unwrap().into();
            if let Some(kind) = special_properties.get(&i) {
                let AccessorProperty { get, set, .. } = properties
                    .entry(name)
                    .or_insert(Property2::Accessor(AccessorProperty {
                        get: Value2::undefined(),
                        set: Value2::undefined(),
                        // TODO
                        enumerable: true,
                        configurable: true,
                    }))
                    .as_accessor_mut();
                match kind {
                    constant::SpecialPropertyKind::Getter => *get = val,
                    constant::SpecialPropertyKind::Setter => *set = val,
                }
            } else {
                properties.insert(
                    name,
                    Property2::Data(DataProperty {
                        val,
                        // TODO
                        writable: true,
                        enumerable: true,
                        configurable: true,
                    }),
                );
            }
        }

        let obj = Value2::object(
            &mut self.memory_allocator,
            &self.object_prototypes,
            properties,
        );
        self.stack.push(obj.into());

        Ok(())
    }

    fn create_array(&mut self, len: usize) -> VMResult {
        let mut elems = vec![];
        for _ in 0..len {
            let val: Value2 = self.stack.pop().unwrap().into();
            elems.push(Property2::Data(DataProperty {
                val,
                writable: true,
                enumerable: true,
                configurable: true,
            }));
        }

        let ary = Value2::array(&mut self.memory_allocator, &self.object_prototypes, elems);
        self.stack.push(ary.into());

        Ok(())
    }

    fn enter_constructor(
        &mut self,
        callee: Value2,
        args: &[Value2],
        cur_frame: &mut frame::Frame,
    ) -> VMResult {
        let this = Value2::Object(self.memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Ordinary,
            prototype: callee.get_property_by_str_key("prototype"),
            property: FxHashMap::default(),
            sym_property: FxHashMap::default(),
        }));

        if !callee.is_function_object() {
            return Err(RuntimeError::Type("Not a function".to_string()));
        }

        let info = callee.as_function();

        match info.kind {
            FunctionObjectKind::Builtin(func) => {
                func(self, args, &frame::Frame::new_empty_with_this(this, true))
            }
            FunctionObjectKind::User(ref user_func) => {
                self.enter_user_function(user_func.clone(), args, this, cur_frame, true)
            }
        }
    }

    fn enter_function(
        &mut self,
        callee: Value2,
        args: &[Value2],
        this: Value2,
        cur_frame: &mut frame::Frame,
        constructor_call: bool,
    ) -> VMResult {
        if !callee.is_function_object() {
            return Err(RuntimeError::Type("Not a function".to_string()));
        }

        let info = callee.as_function();

        match info.kind {
            FunctionObjectKind::Builtin(func) => gc_lock!(
                self,
                args,
                func(self, args, &frame::Frame::new_empty_with_this(this, false))
            ),
            FunctionObjectKind::User(ref user_func) => {
                self.enter_user_function(user_func.clone(), args, this, cur_frame, constructor_call)
            }
        }
    }

    fn create_declarative_environment<F>(
        &mut self,
        f: F,
        outer: Option<frame::LexicalEnvironmentRef>,
    ) -> frame::LexicalEnvironmentRef
    where
        F: Fn(&mut VM2, &mut FxHashMap<String, Value2>),
    {
        let env = frame::LexicalEnvironment {
            record: frame::EnvironmentRecord::Declarative({
                let mut record = FxHashMap::default();
                f(self, &mut record);
                record
            }),
            outer,
        };
        self.memory_allocator.alloc(env)
    }

    fn create_function_environment<F>(
        &mut self,
        f: F,
        this: Value2,
        outer: Option<frame::LexicalEnvironmentRef>,
    ) -> frame::LexicalEnvironmentRef
    where
        F: Fn(&mut VM2, &mut FxHashMap<String, Value2>),
    {
        let env = frame::LexicalEnvironment {
            record: frame::EnvironmentRecord::Function {
                record: {
                    let mut record = FxHashMap::default();
                    f(self, &mut record);
                    record
                },
                this,
            },
            outer,
        };
        self.memory_allocator.alloc(env)
    }

    fn enter_user_function(
        &mut self,
        user_func: UserFunctionInfo,
        args: &[Value2],
        this: Value2,
        cur_frame: &mut frame::Frame,
        constructor_call: bool,
    ) -> VMResult {
        if !user_func.constructible && constructor_call {
            return Err(RuntimeError::Type("Not a constructor".to_string()));
        }

        self.saved_frame
            .push(cur_frame.clone().saved_stack_len(self.stack.len()));

        let this = if user_func.this_mode == ThisMode::Lexical {
            // Arrow function
            unsafe { &*user_func.outer.unwrap() }.get_this_binding()
        } else {
            this
        };

        let var_env_ref = self.create_function_environment(
            |vm, record| {
                for name in &user_func.var_names {
                    record.insert(name.clone(), Value2::undefined());
                }

                for (i, FunctionParameter { name, rest_param }) in
                    user_func.params.iter().enumerate()
                {
                    record.insert(
                        name.clone(),
                        if *rest_param {
                            Value2::array(
                                &mut vm.memory_allocator,
                                &vm.object_prototypes,
                                (*args)
                                    .get(i..)
                                    .unwrap_or(&vec![])
                                    .iter()
                                    .map(|elem| Property2::new_data_simple(*elem))
                                    .collect::<Vec<Property2>>(),
                            )
                        } else {
                            *args.get(i).unwrap_or(&Value2::undefined())
                        },
                    );
                }
            },
            this,
            user_func.outer,
        );

        let lex_env_ref = self.create_declarative_environment(
            |_, record| {
                for name in &user_func.lex_names {
                    record.insert(name.clone(), Value2::uninitialized());
                }
            },
            Some(var_env_ref),
        );

        for func in user_func.func_decls {
            let mut func = func.copy_object(&mut self.memory_allocator);
            let name = func.as_function().name.clone().unwrap();
            func.set_function_outer_environment(lex_env_ref);
            unsafe { &mut *lex_env_ref }.set_value(name, func)?;
        }

        let exec_ctx = frame::ExecutionContext {
            variable_environment: var_env_ref,
            lexical_environment: lex_env_ref,
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
