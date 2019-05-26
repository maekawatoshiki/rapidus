use crate::bytecode_gen::{show_inst, ByteCode, VMInst};
use crate::gc;
use crate::node::Node;
use crate::vm::{
    codegen,
    codegen::CodeGenerator,
    constant,
    error::*,
    frame,
    jsvalue::function::{DestinationKind, ThisMode},
    jsvalue::prototype::ObjectPrototypes,
    jsvalue::symbol::GlobalSymbolRegistry,
    jsvalue::value::*,
};
use rustc_hash::FxHashMap;

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
    pub to_source_map: FxHashMap<usize, codegen::ToSourcePos>,
    is_trace: bool,
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
        let global_environment = frame::LexicalEnvironmentRef(memory_allocator.alloc(global_env));
        VM2 {
            global_environment,
            memory_allocator,
            object_prototypes,
            constant_table: constant::ConstantTable::new(),
            global_symbol_registry: GlobalSymbolRegistry::new(),
            stack: vec![],
            saved_frame: vec![],
            to_source_map: FxHashMap::default(),
            is_trace: false,
        }
    }

    pub fn trace(mut self) -> Self {
        self.is_trace = true;
        self
    }

    pub fn compile(
        &mut self,
        node: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> Result<codegen::FunctionInfo, codegen::Error> {
        let mut code_generator = CodeGenerator::new(
            // &parser,
            &mut self.constant_table,
            &mut self.memory_allocator,
            &self.object_prototypes,
        );
        let res = code_generator.compile(node, iseq, use_value);
        self.to_source_map = code_generator.to_source_map;
        res
    }

    pub fn create_global_frame(
        &mut self,
        global_info: codegen::FunctionInfo,
        iseq: ByteCode,
    ) -> frame::Frame {
        let global_env_ref = self.global_environment;

        let var_env = self.create_variable_environment(&global_info.var_names, global_env_ref);

        let mut lex_env = self.create_lexical_environment(&global_info.lex_names, var_env);

        for val in global_info.func_decls {
            let mut val = val.copy_object(&mut self.memory_allocator);
            let name = val.as_function().name.clone().unwrap();
            val.set_function_outer_environment(lex_env);
            lex_env.set_value(name, val).unwrap();
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
            global_env_ref.get_global_object(),
            false,
        );

        frame
    }

    pub fn run_global(&mut self, global_info: codegen::FunctionInfo, iseq: ByteCode) -> VMResult {
        let global_frame = self.create_global_frame(global_info, iseq);

        self.run(global_frame)?;

        Ok(())
    }

    pub fn call_function(
        &mut self,
        callee: Value,
        args: &[Value],
        this: Value,
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
        args: &[Value],
        this: Value,
        cur_frame: &frame::Frame,
        constructor_call: bool,
    ) -> VMResult {
        let frame = self
            .prepare_frame_for_function_invokation(
                user_func,
                args,
                this,
                cur_frame,
                constructor_call,
            )?
            .escape();

        self.run(frame)
    }

    fn get_property_to_stack_top(
        &mut self,
        parent: Value,
        key: Value,
        cur_frame: &mut frame::Frame,
    ) -> VMResult {
        let val = parent.get_property(&mut self.memory_allocator, &self.object_prototypes, key)?;
        match val {
            Property::Data(DataProperty { val, .. }) => {
                self.stack.push(val.into());
                Ok(())
            }
            Property::Accessor(AccessorProperty { get, .. }) => {
                if get.is_undefined() {
                    self.stack.push(Value::undefined().into());
                    return Ok(());
                }
                self.enter_function(get, &[], parent, cur_frame, false)
            }
        }
    }

    pub fn get_property(
        &mut self,
        parent: Value,
        key: Value,
        cur_frame: &frame::Frame,
    ) -> Result<Value, RuntimeError> {
        let val = parent.get_property(&mut self.memory_allocator, &self.object_prototypes, key)?;
        match val {
            Property::Data(DataProperty { val, .. }) => Ok(val),
            Property::Accessor(AccessorProperty { get, .. }) => {
                if get.is_undefined() {
                    return Ok(Value::undefined());
                }
                self.call_function(get, &[], parent, cur_frame)?;
                Ok(self.stack.pop().unwrap().into(): Value)
            }
        }
    }

    pub fn set_property(
        &mut self,
        parent: Value,
        key: Value,
        val: Value,
        cur_frame: &frame::Frame,
    ) -> VMResult {
        let maybe_setter = parent.set_property(&mut self.memory_allocator, key, val)?;
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
        let is_trace = self.is_trace;

        loop {
            let current_inst_pc = cur_frame.pc;

            macro_rules! exception {
                () => {{
                    let mut exception_found = false;
                    let mut outer_break = false;

                    let node_pos = self
                        .to_source_map
                        .get(&cur_frame.id)
                        .unwrap()
                        .get_node_pos(current_inst_pc);

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
                        let val: Value = self.stack.pop().unwrap().into();
                        return Err(RuntimeError::Exception2(val, node_pos));
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

            if is_trace {
                crate::bytecode_gen::show_inst(
                    &cur_frame.bytecode,
                    current_inst_pc,
                    &self.constant_table,
                );
                match self.stack.last() {
                    None => {
                        println!("<empty>");
                    }
                    Some(val) => {
                        println!("{:10}", (*val).into(): Value);
                    }
                }
            }

            match cur_frame.bytecode[cur_frame.pc] {
                // TODO: Macro for bin ops?
                VMInst::ADD => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(lhs.add(&mut self.memory_allocator, rhs).into());
                }
                VMInst::SUB => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.sub(rhs).into());
                }
                VMInst::MUL => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.mul(rhs).into());
                }
                VMInst::DIV => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.div(rhs).into());
                }
                VMInst::REM => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.rem(rhs).into());
                }
                VMInst::EXP => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(lhs.exp(&mut self.memory_allocator, rhs).into());
                }
                VMInst::EQ => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(lhs.eq(&mut self.memory_allocator, rhs).into());
                }
                VMInst::SEQ => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.strict_eq(rhs).into());
                }
                VMInst::NE => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(lhs.ne(&mut self.memory_allocator, rhs).into());
                }
                VMInst::SNE => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.strict_ne(rhs).into());
                }
                VMInst::LT => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(lhs.lt(&mut self.memory_allocator, rhs).into());
                }
                VMInst::LE => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(lhs.le(&mut self.memory_allocator, rhs).into());
                }
                VMInst::GT => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(rhs.lt(&mut self.memory_allocator, lhs).into());
                }
                VMInst::GE => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(rhs.le(&mut self.memory_allocator, lhs).into());
                }
                VMInst::AND => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(rhs.and(&mut self.memory_allocator, lhs).into());
                }
                VMInst::OR => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(rhs.or(&mut self.memory_allocator, lhs).into());
                }
                VMInst::XOR => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(rhs.xor(&mut self.memory_allocator, lhs).into());
                }
                VMInst::NOT => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(rhs.not(&mut self.memory_allocator).into());
                }
                VMInst::SHL => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(lhs.shift_l(&mut self.memory_allocator, rhs).into());
                }
                VMInst::SHR => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(lhs.shift_r(&mut self.memory_allocator, rhs).into());
                }
                VMInst::ZFSHR => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(lhs.z_shift_r(&mut self.memory_allocator, rhs).into());
                }
                VMInst::NEG => {
                    cur_frame.pc += 1;
                    let val: Value = self.stack.pop().unwrap().into();
                    self.stack.push(val.minus().into());
                }
                VMInst::POSI => {
                    cur_frame.pc += 1;
                    let val: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(val.positive(&mut self.memory_allocator).into());
                }
                VMInst::LNOT => {
                    cur_frame.pc += 1;
                    let val: Value = self.stack.pop().unwrap().into();
                    let res = Value::bool(!val.to_boolean());
                    self.stack.push(res.into());
                }
                VMInst::PUSH_INT8 => {
                    cur_frame.pc += 1;
                    read_int8!(cur_frame.bytecode, cur_frame.pc, num, f64);
                    self.stack.push(Value::Number(num).into());
                }
                VMInst::PUSH_INT32 => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, num, i32);
                    self.stack.push(Value::Number(num as f64).into());
                }
                VMInst::PUSH_CONST => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, id, usize);
                    let val = *self.constant_table.get(id).as_value();
                    self.stack.push(val.into());
                }
                VMInst::PUSH_NULL => {
                    cur_frame.pc += 1;
                    self.stack.push(Value::null().into());
                }
                VMInst::PUSH_UNDEFINED => {
                    cur_frame.pc += 1;
                    self.stack.push(Value::undefined().into());
                }
                VMInst::PUSH_THIS => {
                    cur_frame.pc += 1;
                    self.stack.push(cur_frame.this.into());
                }
                VMInst::PUSH_FALSE => {
                    cur_frame.pc += 1;
                    self.stack.push(Value::Bool(0).into());
                }
                VMInst::PUSH_TRUE => {
                    cur_frame.pc += 1;
                    self.stack.push(Value::Bool(1).into());
                }
                VMInst::GET_MEMBER => {
                    cur_frame.pc += 1;
                    let property: Value = self.stack.pop().unwrap().into();
                    let parent: Value = self.stack.pop().unwrap().into();
                    etry!(self.get_property_to_stack_top(parent, property, &mut cur_frame))
                }
                VMInst::SET_MEMBER => {
                    cur_frame.pc += 1;
                    let property: Value = self.stack.pop().unwrap().into();
                    let parent: Value = self.stack.pop().unwrap().into();
                    let val: Value = self.stack.pop().unwrap().into();
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
                    let callee: Value = self.stack.pop().unwrap().into();
                    let mut args: Vec<Value> = vec![];
                    for _ in 0..argc {
                        args.push(self.stack.pop().unwrap().into());
                    }
                    if is_trace {
                        println!("--> call constructor")
                    };
                    self.enter_constructor(callee, &args, &mut cur_frame)?;
                }
                VMInst::CALL => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, argc, usize);
                    let callee: Value = self.stack.pop().unwrap().into();
                    let mut args: Vec<Value> = vec![];
                    for _ in 0..argc {
                        args.push(self.stack.pop().unwrap().into());
                    }
                    if is_trace {
                        println!("--> call function")
                    };
                    etry!(self.enter_function(callee, &args, cur_frame.this, &mut cur_frame, false))
                }
                VMInst::CALL_METHOD => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, argc, usize);
                    let parent: Value = self.stack.pop().unwrap().into();
                    let method: Value = self.stack.pop().unwrap().into();
                    let mut args: Vec<Value> = vec![];
                    for _ in 0..argc {
                        args.push(self.stack.pop().unwrap().into());
                    }
                    let callee = match etry!(parent.get_property(
                        &mut self.memory_allocator,
                        &self.object_prototypes,
                        method
                    )) {
                        Property::Data(DataProperty { val, .. }) => val,
                        _ => type_error!("Not a function"),
                    };
                    if is_trace {
                        println!("--> call function")
                    };
                    etry!(self.enter_function(callee, &args, parent, &mut cur_frame, false))
                }
                VMInst::SET_OUTER_ENV => {
                    cur_frame.pc += 1;
                    let func_template: Value = self.stack.pop().unwrap().into();
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
                    let lex_env = cur_frame
                        .execution_context
                        .saved_lexical_environment
                        .pop()
                        .unwrap();
                    cur_frame.execution_context.lexical_environment = lex_env;
                }
                VMInst::POP => {
                    cur_frame.pc += 1;
                    self.stack.pop();
                }
                VMInst::JMP_IF_FALSE => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, dst, i32);
                    let cond_boxed = self.stack.pop().unwrap();
                    let cond: Value = cond_boxed.into();
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
                    if is_trace {
                        println!("<-- return")
                    };
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
                    let val: Value = self.stack.pop().unwrap().into();
                    let type_str = val.type_of();
                    let type_str_val =
                        Value::string(&mut self.memory_allocator, type_str.to_string());
                    self.stack.push(type_str_val.into());
                }
                VMInst::END => break,
                _ => {
                    print!("Not yet implemented VMInst: ");
                    show_inst(&cur_frame.bytecode, cur_frame.pc, &self.constant_table);
                    println!();
                    unimplemented!();
                }
            }
        }

        Ok(())
    }

    pub fn unwind_frame_saving_stack_top(&mut self, cur_frame: &mut frame::Frame) {
        let ret_val_boxed = self.stack.pop().unwrap();
        let ret_val: Value = ret_val_boxed.into();
        let frame = self.saved_frame.pop().unwrap();
        self.stack.truncate(frame.saved_stack_len);
        if cur_frame.constructor_call && !ret_val.is_object() {
            self.stack.push(cur_frame.this.into());
        } else {
            self.stack.push(ret_val_boxed);
        }
        *cur_frame = frame;
    }

    pub fn unwind_frame(&mut self, cur_frame: &mut frame::Frame) {
        let frame = self.saved_frame.pop().unwrap();
        self.stack.truncate(frame.saved_stack_len);
        *cur_frame = frame;
    }

    fn push_env(&mut self, id: usize, cur_frame: &mut frame::Frame) -> VMResult {
        let lex_names = self.constant_table.get(id).as_lex_env_info().clone();
        let outer = cur_frame.execution_context.lexical_environment;

        let lex_env = self.create_lexical_environment(&lex_names, outer);

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
            let prop: Value = self.stack.pop().unwrap().into();
            let name = prop.to_string();
            let val: Value = self.stack.pop().unwrap().into();
            if let Some(kind) = special_properties.get(&i) {
                let AccessorProperty { get, set, .. } = properties
                    .entry(name)
                    .or_insert(Property::Accessor(AccessorProperty {
                        get: Value::undefined(),
                        set: Value::undefined(),
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
                    Property::Data(DataProperty {
                        val,
                        // TODO
                        writable: true,
                        enumerable: true,
                        configurable: true,
                    }),
                );
            }
        }

        let obj = Value::object(
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
            let val: Value = self.stack.pop().unwrap().into();
            elems.push(Property::Data(DataProperty {
                val,
                writable: true,
                enumerable: true,
                configurable: true,
            }));
        }

        let ary = Value::array(&mut self.memory_allocator, &self.object_prototypes, elems);
        self.stack.push(ary.into());

        Ok(())
    }

    fn enter_constructor(
        &mut self,
        callee: Value,
        args: &[Value],
        cur_frame: &mut frame::Frame,
    ) -> VMResult {
        let this = Value::Object(self.memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Ordinary,
            prototype: callee.get_property_by_str_key("prototype"),
            property: FxHashMap::default(),
            sym_property: FxHashMap::default(),
        }));

        self.enter_function(callee, args, this, cur_frame, true)
    }

    fn enter_function(
        &mut self,
        callee: Value,
        args: &[Value],
        this: Value,
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
        F: Fn(&mut VM2, &mut FxHashMap<String, Value>),
    {
        let env = frame::LexicalEnvironment {
            record: frame::EnvironmentRecord::Declarative({
                let mut record = FxHashMap::default();
                f(self, &mut record);
                record
            }),
            outer,
        };

        frame::LexicalEnvironmentRef(self.memory_allocator.alloc(env))
    }

    fn create_variable_environment(
        &mut self,
        var_names: &Vec<String>,
        outer_env_ref: frame::LexicalEnvironmentRef,
    ) -> frame::LexicalEnvironmentRef {
        self.create_declarative_environment(
            |_, record| {
                for name in var_names {
                    record.insert(name.clone(), Value::undefined());
                }
            },
            Some(outer_env_ref),
        )
    }

    fn create_lexical_environment(
        &mut self,
        lex_names: &Vec<String>,
        outer_env_ref: frame::LexicalEnvironmentRef,
    ) -> frame::LexicalEnvironmentRef {
        self.create_declarative_environment(
            |_, record| {
                for name in lex_names {
                    record.insert(name.clone(), Value::uninitialized());
                }
            },
            Some(outer_env_ref),
        )
    }

    fn create_function_environment(
        &mut self,
        var_names: &Vec<String>,
        params: &Vec<FunctionParameter>,
        args: &[Value],
        this: Value,
        outer: Option<frame::LexicalEnvironmentRef>,
    ) -> frame::LexicalEnvironmentRef {
        let env = frame::LexicalEnvironment {
            record: frame::EnvironmentRecord::Function {
                record: {
                    let mut record = FxHashMap::default();
                    for name in var_names {
                        record.insert(name.clone(), Value::undefined());
                    }
                    for (i, FunctionParameter { name, rest_param }) in params.iter().enumerate() {
                        record.insert(
                            name.clone(),
                            if *rest_param {
                                Value::array(
                                    &mut self.memory_allocator,
                                    &self.object_prototypes,
                                    (*args)
                                        .get(i..)
                                        .unwrap_or(&vec![])
                                        .iter()
                                        .map(|elem| Property::new_data_simple(*elem))
                                        .collect::<Vec<Property>>(),
                                )
                            } else {
                                *args.get(i).unwrap_or(&Value::undefined())
                            },
                        );
                    }
                    record
                },
                this,
            },
            outer,
        };

        frame::LexicalEnvironmentRef(self.memory_allocator.alloc(env))
    }

    fn prepare_frame_for_function_invokation(
        &mut self,
        user_func: &UserFunctionInfo,
        args: &[Value],
        this: Value,
        cur_frame: &frame::Frame,
        constructor_call: bool,
    ) -> Result<frame::Frame, RuntimeError> {
        self.saved_frame
            .push(cur_frame.clone().saved_stack_len(self.stack.len()));

        let this = if user_func.this_mode == ThisMode::Lexical {
            // Arrow function
            user_func.outer.unwrap().get_this_binding()
        } else {
            this
        };

        let var_env_ref = self.create_function_environment(
            &user_func.var_names,
            &user_func.params,
            args,
            this,
            user_func.outer,
        );

        let mut lex_env_ref = self.create_lexical_environment(&user_func.lex_names, var_env_ref);

        for func in &user_func.func_decls {
            let mut func = func.copy_object(&mut self.memory_allocator);
            let name = func.as_function().name.clone().unwrap();
            func.set_function_outer_environment(lex_env_ref);
            lex_env_ref.set_value(name, func)?;
        }

        let exec_ctx = frame::ExecutionContext {
            variable_environment: var_env_ref,
            lexical_environment: lex_env_ref,
            saved_lexical_environment: vec![],
        };

        let user_func = user_func.clone();

        Ok(frame::Frame::new(
            exec_ctx,
            user_func.code,
            user_func.exception_table,
            this,
            constructor_call,
        )
        .id(user_func.id))
    }

    fn enter_user_function(
        &mut self,
        user_func: UserFunctionInfo,
        args: &[Value],
        this: Value,
        cur_frame: &mut frame::Frame,
        constructor_call: bool,
    ) -> VMResult {
        if !user_func.constructible && constructor_call {
            return Err(RuntimeError::Type("Not a constructor".to_string()));
        }

        let frame = self.prepare_frame_for_function_invokation(
            &user_func,
            args,
            this,
            cur_frame,
            constructor_call,
        )?;

        *cur_frame = frame;

        Ok(())
    }
}
