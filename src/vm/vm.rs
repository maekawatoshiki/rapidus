use super::super::node::Node;
use super::{
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
use builtin::BuiltinFuncTy2;
use bytecode_gen::show_inst2;
use bytecode_gen::ByteCode;
use bytecode_gen::VMInst;
use gc;
use rustc_hash::FxHashMap;

// New VM

pub type VMResult = Result<(), RuntimeError>;

#[derive(Debug)]
pub struct VM2 {
    pub global_environment: frame::LexicalEnvironmentRef,
    pub factory: Factory,
    pub constant_table: constant::ConstantTable,
    pub global_symbol_registry: GlobalSymbolRegistry,
    pub stack: Vec<BoxedValue>,
    pub saved_frame: Vec<frame::Frame>,
    pub to_source_map: FxHashMap<usize, codegen::ToSourcePos>,
}

#[derive(Debug)]
pub struct Factory {
    pub memory_allocator: gc::MemoryAllocator,
    pub object_prototypes: ObjectPrototypes,
}

macro_rules! gc_lock {
    ($vm:ident, $targets:expr, $($body:tt)*) => { {
        for arg in $targets { $vm.factory.memory_allocator.lock(*arg) }
        let ret = { $($body)* };
        for arg in $targets { $vm.factory.memory_allocator.unlock(*arg) }
        ret
    } }
}

impl VM2 {
    pub fn new() -> Self {
        let mut memory_allocator = gc::MemoryAllocator::new();
        let object_prototypes = ObjectPrototypes::new(&mut memory_allocator);
        let global_env =
            frame::LexicalEnvironment::new_global(&mut memory_allocator, &object_prototypes);
        let global_environment = frame::LexicalEnvironmentRef(memory_allocator.alloc(global_env));
        let factory = Factory {
            memory_allocator,
            object_prototypes,
        };
        let mut vm = VM2 {
            global_environment,
            factory,
            constant_table: constant::ConstantTable::new(),
            global_symbol_registry: GlobalSymbolRegistry::new(),
            stack: vec![],
            saved_frame: vec![],
            to_source_map: FxHashMap::default(),
        };

        use builtin::parse_float;
        use builtins;

        let log = vm
            .factory
            .builtin_function("log".to_string(), builtins::console::console_log);
        let parse_float = vm
            .factory
            .builtin_function("parseFloat".to_string(), parse_float);
        let console = make_normal_object!(vm.factory,
            log => true, false, true: log
        );
        let object_constructor = builtins::object::object(&mut vm);
        let function_constructor = builtins::function::function(&mut vm);
        let array_constructor = builtins::array::array(&mut vm);
        let symbol_constructor = builtins::symbol::symbol(&mut vm);
        let math_object = builtins::math::math(&mut vm);

        let global_object = vm.global_environment.get_global_object();
        let global_objectinfo = global_object.get_object_info();
        add_property!(global_objectinfo,
            undefined  => false,false,false: Value::undefined(),
            NaN        => false,false,false: Value::Number(::std::f64::NAN),
            Infinity   => false,false,false: Value::Number(::std::f64::INFINITY),
            parseFloat => true, false, true: parse_float,
            console    => true, false, true: console,
            Object     => true, false, true: object_constructor,
            Function   => true, false, true: function_constructor,
            Array      => true, false, true: array_constructor,
            Symbol     => true, false, true: symbol_constructor,
            Math       => true, false, true: math_object
        );

        vm
    }

    pub fn compile(
        &mut self,
        node: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
    ) -> Result<codegen::FunctionInfo, codegen::Error> {
        let (res, source_map) = {
            let mut code_generator = CodeGenerator::new(self);
            code_generator.compile(node, iseq, use_value)?
        };
        self.to_source_map = source_map;
        Ok(res)
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
            let mut val = val.copy_object(&mut self.factory);
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
        let global_env_ref = self.global_environment;

        let var_env = self.create_variable_environment(&global_info.var_names, global_env_ref);

        let mut lex_env = self.create_lexical_environment(&global_info.lex_names, var_env);

        for val in global_info.func_decls {
            let mut val = val.copy_object(&mut self.factory);
            let name = val.as_function().name.clone().unwrap();
            val.set_function_outer_environment(lex_env);
            lex_env.set_value(name, val)?;
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

        self.run(frame)?;

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
                self.saved_frame
                    .push(cur_frame.clone().saved_stack_len(self.stack.len()));

                let frame = self
                    .prepare_frame_for_function_invokation(user_func, args, this, false)?
                    .escape();

                self.run(frame)
                //self.call_user_function(user_func, args, this, cur_frame, false)
            }
        }
    }
    /*
    fn call_user_function(
        &mut self,
        user_func: &UserFunctionInfo,
        args: &[Value],
        this: Value,
        cur_frame: &frame::Frame,
        constructor_call: bool,
    ) -> VMResult {
        self.saved_frame
            .push(cur_frame.clone().saved_stack_len(self.stack.len()));

        let frame = self
            .prepare_frame_for_function_invokation(user_func, args, this, constructor_call)?
            .escape();

        self.run(frame)
    }
    */

    fn get_property_to_stack_top(
        &mut self,
        parent: Value,
        key: Value,
        cur_frame: &mut frame::Frame,
    ) -> VMResult {
        let val = parent.get_property(&mut self.factory, key)?;
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
        let val = parent.get_property(&mut self.factory, key)?;
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
        let maybe_setter = parent.set_property(&mut self.factory, key, val)?;
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

                let node_pos = self
                    .to_source_map
                    .get(&cur_frame.id)
                    .unwrap()
                    .get_node_pos(cur_frame.pc);

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
                let val = RuntimeError::Type($msg.to_string()).to_value2(&mut self.factory);
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
                        let val = err.to_value2(&mut self.factory);
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
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.add(&mut self.factory, rhs).into());
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
                VMInst::EQ => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.eq(&mut self.factory, rhs).into());
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
                    self.stack.push(lhs.ne(&mut self.factory, rhs).into());
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
                    self.stack.push(lhs.lt(&mut self.factory, rhs).into());
                }
                VMInst::LE => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.le(&mut self.factory, rhs).into());
                }
                VMInst::GT => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(rhs.lt(&mut self.factory, lhs).into());
                }
                VMInst::GE => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(rhs.le(&mut self.factory, lhs).into());
                }
                VMInst::AND => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(rhs.and(&mut self.factory, lhs).into());
                }
                VMInst::OR => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(rhs.or(&mut self.factory, lhs).into());
                }
                VMInst::XOR => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(rhs.xor(&mut self.factory, lhs).into());
                }
                VMInst::NOT => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(rhs.not(&mut self.factory).into());
                }
                VMInst::SHL => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.shift_l(&mut self.factory, rhs).into());
                }
                VMInst::SHR => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(lhs.shift_r(&mut self.factory, rhs).into());
                }
                VMInst::ZFSHR => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(lhs.z_shift_r(&mut self.factory, rhs).into());
                }
                VMInst::NEG => {
                    cur_frame.pc += 1;
                    let val: Value = self.stack.pop().unwrap().into();
                    self.stack.push(val.minus().into());
                }
                VMInst::POSI => {
                    cur_frame.pc += 1;
                    let val: Value = self.stack.pop().unwrap().into();
                    self.stack.push(val.positive(&mut self.factory).into());
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
                    let callee = match etry!(parent.get_property(&mut self.factory, method)) {
                        Property::Data(DataProperty { val, .. }) => val,
                        _ => type_error!("Not a function"),
                    };
                    etry!(self.enter_function(callee, &args, parent, &mut cur_frame, false))
                }
                VMInst::SET_OUTER_ENV => {
                    cur_frame.pc += 1;
                    let func_template: Value = self.stack.pop().unwrap().into();
                    let mut func = func_template.copy_object(&mut self.factory);
                    func.set_function_outer_environment(
                        cur_frame.execution_context.lexical_environment,
                    );
                    self.stack.push(func.into());
                }
                VMInst::CREATE_OBJECT => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, id, usize);
                    self.create_object(id)?;
                    self.factory.memory_allocator.mark(
                        self.global_environment,
                        &self.factory.object_prototypes,
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
                    self.factory.memory_allocator.mark(
                        self.global_environment,
                        &self.factory.object_prototypes,
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
                    // TODO: GC schedule
                    self.factory.memory_allocator.mark(
                        self.global_environment,
                        &self.factory.object_prototypes,
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
                    let type_str_val = Value::string(&mut self.factory, type_str.to_string());
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
        let lex_env = self.create_lexical_environment(
            &lex_names,
            cur_frame.execution_context.lexical_environment,
        );

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

        let obj = Value::object(&mut self.factory, properties);
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

        let ary = self.factory.array(elems);
        self.stack.push(ary.into());

        Ok(())
    }

    fn enter_constructor(
        &mut self,
        callee: Value,
        args: &[Value],
        cur_frame: &mut frame::Frame,
    ) -> VMResult {
        let this = Value::Object(self.factory.memory_allocator.alloc(ObjectInfo {
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
                func(
                    self,
                    args,
                    &frame::Frame::new_empty_with_this(this, constructor_call)
                )
            ),
            FunctionObjectKind::User(ref user_func) => {
                self.enter_user_function(user_func, args, this, cur_frame, constructor_call)
            }
        }
    }

    fn enter_user_function(
        &mut self,
        user_func: &UserFunctionInfo,
        args: &[Value],
        this: Value,
        cur_frame: &mut frame::Frame,
        constructor_call: bool,
    ) -> VMResult {
        if !user_func.constructible && constructor_call {
            return Err(RuntimeError::Type("Not a constructor".to_string()));
        }
        self.saved_frame
            .push(cur_frame.clone().saved_stack_len(self.stack.len()));

        let frame =
            self.prepare_frame_for_function_invokation(user_func, args, this, constructor_call)?;

        *cur_frame = frame;

        Ok(())
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

        frame::LexicalEnvironmentRef(self.factory.memory_allocator.alloc(env))
    }

    fn create_variable_environment(
        &mut self,
        names: &Vec<String>,
        outer: frame::LexicalEnvironmentRef,
    ) -> frame::LexicalEnvironmentRef {
        self.create_declarative_environment(
            |_, record| {
                for name in names {
                    record.insert(name.clone(), Value::undefined());
                }
            },
            Some(outer),
        )
    }

    fn create_lexical_environment(
        &mut self,
        names: &Vec<String>,
        outer: frame::LexicalEnvironmentRef,
    ) -> frame::LexicalEnvironmentRef {
        self.create_declarative_environment(
            |_, record| {
                for name in names {
                    record.insert(name.clone(), Value::uninitialized());
                }
            },
            Some(outer),
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
                                self.factory.array(
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

        frame::LexicalEnvironmentRef(self.factory.memory_allocator.alloc(env))
    }

    /// Create new frame for function invokation.
    fn prepare_frame_for_function_invokation(
        &mut self,
        user_func: &UserFunctionInfo,
        args: &[Value],
        this: Value,
        constructor_call: bool,
    ) -> Result<frame::Frame, RuntimeError> {
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
            let mut func = func.copy_object(&mut self.factory);
            let name = func.as_function().name.clone().unwrap();
            func.set_function_outer_environment(lex_env_ref);
            lex_env_ref.set_value(name, func)?;
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
        .id(user_func.id);

        Ok(frame)
    }
}

impl Factory {
    pub fn function(&mut self, name: Option<String>, info: UserFunctionInfo) -> Value {
        let name_prop = Value::string(self, name.clone().unwrap_or("".to_string()));
        let prototype = Value::object(self, FxHashMap::default());

        let f = Value::Object(self.memory_allocator.alloc(ObjectInfo {
            prototype: self.object_prototypes.function,
            property: make_property_map!(
                length    => false, false, true : Value::Number(info.params.len() as f64), /* TODO: rest param */
                name      => false, false, true : name_prop,
                prototype => true , false, false: prototype
            ),
            kind: ObjectKind2::Function(FunctionObjectInfo {
                name: name,
                kind: FunctionObjectKind::User(info)
            }),
            sym_property: FxHashMap::default(),
        }));

        f.get_property_by_str_key("prototype")
            .get_object_info()
            .property
            .insert("constructor".to_string(), Property::new_data_simple(f));

        f
    }

    pub fn builtin_function(&mut self, name: String, func: BuiltinFuncTy2) -> Value {
        let name_prop = Value::string(self, name.clone());
        Value::Object(self.memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Function(FunctionObjectInfo {
                name: Some(name),
                kind: FunctionObjectKind::Builtin(func),
            }),
            prototype: self.object_prototypes.function,
            property: make_property_map!(
                length => false, false, true : Value::Number(0.0),
                name   => false, false, true : name_prop
            ),
            sym_property: FxHashMap::default(),
        }))
    }

    pub fn array(&mut self, elems: Vec<Property>) -> Value {
        Value::Object(self.memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Array(ArrayObjectInfo { elems }),
            prototype: self.object_prototypes.array,
            property: make_property_map!(),
            sym_property: FxHashMap::default(),
        }))
    }

    pub fn symbol(&mut self, description: Option<String>) -> Value {
        Value::Object(self.memory_allocator.alloc(ObjectInfo {
            kind: ObjectKind2::Symbol(SymbolInfo {
                id: ::id::get_unique_id(),
                description,
            }),
            prototype: self.object_prototypes.symbol,
            property: make_property_map!(),
            sym_property: FxHashMap::default(),
        }))
    }
}

impl VM2 {
    pub fn symbol_registory_for_(&mut self, key: String) -> Value {
        if let Some((_, sym)) = self
            .global_symbol_registry
            .list
            .iter()
            .find(|(key_, _)| key == *key_)
        {
            return *sym;
        }

        let sym = self.factory.symbol(Some(key.clone()));
        self.global_symbol_registry.list.push((key, sym));

        sym
    }
}
