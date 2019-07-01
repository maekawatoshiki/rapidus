use crate::builtins::console::debug_print;
use crate::bytecode_gen::{inst_to_inst_name, show_inst, VMInst};
use crate::gc;
use crate::node::Node;
use crate::parser::ScriptInfo;
pub use crate::vm::factory::{Factory, FunctionId};
use crate::vm::{
    codegen,
    codegen::CodeGenerator,
    constant,
    error::*,
    exec_context::{EnvironmentRecord, ExecContext, LexicalEnvironment, LexicalEnvironmentRef},
    jsvalue::function::{DestinationKind, ThisMode},
    jsvalue::prototype::ObjectPrototypes,
    jsvalue::symbol::GlobalSymbolRegistry,
    jsvalue::value::*,
};
use rustc_hash::FxHashMap;
use std::time::{Duration, Instant};

pub type VMResult = Result<(), RuntimeError>;
/// Ok(Value::Other(Empty)) means mudule call.
pub type VMValueResult = Result<Value, RuntimeError>;

pub struct VM {
    pub factory: Factory,
    pub global_environment: LexicalEnvironmentRef,
    pub constant_table: constant::ConstantTable,
    pub global_symbol_registry: GlobalSymbolRegistry,
    pub current_context: ExecContext,
    pub saved_context: Vec<ExecContext>,
    ///func_id, ToSourcePos
    pub to_source_map: FxHashMap<FunctionId, codegen::ToSourcePos>,
    pub is_profile: bool,
    pub is_trace: bool,
    ///(func_id, script_info)
    pub script_info: Vec<(FunctionId, ScriptInfo)>,
    pub profile: Profiler,
}

pub struct Profiler {
    instant: Instant,
    prev_time: Duration,
    current_inst: u8,
    inst_profile: [(usize, Duration); 100],
    gc_profile: [(usize, Duration); 3],
    gc_stop_time: Duration,
    trace_string: String,
    start_flag: bool,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CallMode {
    OrdinaryCall,
    ModuleCall,
    FromNative,
}

macro_rules! gc_lock {
    ($vm:ident, $targets:expr, $($body:tt)*) => { {
        for arg in $targets { $vm.factory.memory_allocator.lock(*arg) }
        let ret = { $($body)* };
        for arg in $targets { $vm.factory.memory_allocator.unlock(*arg) }
        ret
    } }
}

impl VM {
    pub fn new() -> Self {
        let mut memory_allocator = gc::MemoryAllocator::new();
        let object_prototypes = ObjectPrototypes::new(&mut memory_allocator);
        let mut factory = Factory::new(memory_allocator, object_prototypes);
        let global_env = LexicalEnvironment::new_global_initialized(&mut factory);
        let global_environment = LexicalEnvironmentRef(factory.alloc(global_env));
        VM {
            global_environment,
            factory,
            constant_table: constant::ConstantTable::new(),
            global_symbol_registry: GlobalSymbolRegistry::new(),
            current_context: ExecContext::empty(),
            saved_context: vec![],
            to_source_map: FxHashMap::default(),
            is_profile: false,
            is_trace: false,
            script_info: vec![],
            profile: Profiler {
                instant: Instant::now(),
                prev_time: Duration::from_secs(0),
                gc_stop_time: Duration::from_secs(0),
                gc_profile: [(0, Duration::from_secs(0)); 3],
                current_inst: 255,
                trace_string: "".to_string(),
                inst_profile: [(0, Duration::from_micros(0)); 100],
                start_flag: false,
            },
        }
    }

    pub fn profile(mut self) -> Self {
        self.is_profile = true;
        self
    }

    pub fn trace(mut self) -> Self {
        self.is_trace = true;
        self
    }

    pub fn gc_mark(&mut self) {
        let time_before_gc = self.profile.instant.elapsed();
        let gc_mode = self.factory.memory_allocator.state;
        self.factory.memory_allocator.mark(
            self.global_environment,
            &self.factory.object_prototypes,
            &self.constant_table,
            &self.current_context,
            &self.saved_context,
        );
        let i = match gc_mode {
            gc::GCState::Initial => 0,
            gc::GCState::Marking => 1,
            gc::GCState::ReadyToSweep => 2,
        };
        let stop_time = self.profile.instant.elapsed() - time_before_gc;
        self.profile.gc_stop_time += stop_time;
        self.profile.gc_profile[i].0 += 1;
        self.profile.gc_profile[i].1 += stop_time;
    }

    pub fn compile(
        &mut self,
        node: &Node,
        use_value: bool,
    ) -> Result<UserFunctionInfo, codegen::Error> {
        let func_id = self.factory.new_func_id();
        let mut code_generator =
            CodeGenerator::new(&mut self.constant_table, &mut self.factory, func_id);
        let res = code_generator.compile(node, use_value);
        for (func_id, list) in code_generator.to_source_map {
            self.to_source_map.insert(func_id, list);
        }
        res
    }

    pub fn create_global_context(&mut self, global_info: UserFunctionInfo) -> ExecContext {
        let global_env_ref = self.global_environment;

        let var_env = self.create_variable_environment(&global_info.var_names, global_env_ref);

        let mut lex_env = self.create_lexical_environment(&global_info.lex_names, var_env);

        for func_info in global_info.func_decls {
            let name = func_info.func_name.clone().unwrap();
            let mut val = self
                .factory
                .function(func_info.func_name.clone(), func_info);
            val.set_function_outer_environment(lex_env);
            lex_env.set_value(name, val).unwrap();
        }

        let context = ExecContext::new(
            global_info.code,
            var_env,
            lex_env,
            global_info.exception_table,
            global_env_ref.get_global_object(),
            CallMode::OrdinaryCall,
        );

        context
    }

    pub fn run_global(&mut self, func_info: UserFunctionInfo) -> VMResult {
        self.current_context = self.create_global_context(func_info);
        self.run()?;

        Ok(())
    }

    pub fn call_function(&mut self, callee: Value, args: &[Value], this: Value) -> VMValueResult {
        if !callee.is_function_object() {
            return Err(self.current_context.error_type("Not a function"));
        }

        let info = callee.as_function();

        match info.kind {
            FunctionObjectKind::Builtin(func) => gc_lock!(self, args, func(self, args, this)),
            FunctionObjectKind::User {
                ref info,
                outer_env,
            } => self.call_user_function(info, outer_env, args, this, false),
        }
    }

    fn call_user_function(
        &mut self,
        user_func: &UserFunctionInfo,
        outer_env: Option<LexicalEnvironmentRef>,
        args: &[Value],
        this: Value,
        constructor_call: bool,
    ) -> VMValueResult {
        self.prepare_context_for_function_invokation(
            user_func,
            outer_env,
            args,
            this,
            CallMode::FromNative,
            constructor_call,
        )?;

        self.run()
    }

    fn get_property_to_stack_top(&mut self, parent: Value, key: Value) -> VMResult {
        let val = parent.get_property_by_value(&mut self.factory, key)?;
        match val {
            Property::Data(DataProperty { val, .. }) => {
                self.current_context.stack.push(val.into());
                Ok(())
            }
            Property::Accessor(AccessorProperty { get, .. }) => {
                if get.is_undefined() {
                    self.current_context.stack.push(Value::undefined().into());
                    return Ok(());
                }
                self.enter_function(get, &[], parent, false)
            }
        }
    }

    pub fn get_property_by_value(
        &mut self,
        parent: Value,
        key: Value,
    ) -> Result<Value, RuntimeError> {
        let val = parent.get_property_by_value(&mut self.factory, key)?;
        match val {
            Property::Data(DataProperty { val, .. }) => Ok(val),
            Property::Accessor(AccessorProperty { get, .. }) => {
                if get.is_undefined() {
                    return Ok(Value::undefined());
                }
                self.call_function(get, &[], parent)?;
                Ok(self.current_context.stack.pop().unwrap().into(): Value)
            }
        }
    }

    pub fn set_property_by_value(&mut self, parent: Value, key: Value, val: Value) -> VMResult {
        let maybe_setter =
            parent.set_property_by_value(&mut self.factory.memory_allocator, key, val)?;
        if let Some(setter) = maybe_setter {
            self.call_function(setter, &[val], parent)?;
            self.current_context.stack.pop().unwrap(); // Pop undefined (setter's return value)
        }
        Ok(())
    }
}

impl VM {
    pub fn show_error_message(&self, error: RuntimeError) {
        match &error.kind {
            ErrorKind::Unknown => runtime_error("UnknownError"),
            ErrorKind::Unimplemented => runtime_error("Unimplemented feature"),
            ErrorKind::Reference(msg) => runtime_error(format!("ReferenceError: {}", msg)),
            ErrorKind::Type(msg) => runtime_error(format!("TypeError: {}", msg)),
            ErrorKind::General(msg) => runtime_error(format!("Error: {}", msg)),
            ErrorKind::Exception(ref val) => {
                runtime_error("Uncaught Exception");
                let pos_in_script = self
                    .to_source_map
                    .get(&error.func_id)
                    .unwrap()
                    .get_node_pos(error.inst_pc);
                let module_func_id = error.module_func_id;
                let info = &self
                    .script_info
                    .iter()
                    .find(|info| info.0 == module_func_id)
                    .unwrap()
                    .1;
                if let Some(pos) = pos_in_script {
                    let (msg, _, line) = get_code_around_err_point(info, pos);
                    println!("line: {}", line);
                    println!("{}", msg);
                }
                debug_print(val, false);
                println!();
            }
        }

        pub fn get_code_around_err_point(info: &ScriptInfo, pos: usize) -> (String, usize, usize) {
            let code = info.code.as_bytes();
            let iter = info.pos_line_list.iter();
            let (start_pos, line) = iter.take_while(|x| x.0 <= pos).last().unwrap();

            let mut iter = info.pos_line_list.iter();
            let end_pos = match iter
                .find(|x| x.0 > pos)
                .unwrap_or(info.pos_line_list.last().unwrap())
                .0
            {
                x if x == 0 => 0,
                x => x - 1,
            };
            let surrounding_code = String::from_utf8(code[*start_pos..end_pos].to_vec())
                .unwrap()
                .to_string();
            let err_point = format!("{}{}", " ".repeat(pos - start_pos), "^",);
            (surrounding_code + "\n" + err_point.as_str(), pos, *line)
        }
    }
}

macro_rules! read_int8 {
    ($vm:expr, $var:ident, $ty:ty) => {
        let $var = $vm.current_context.bytecode[$vm.current_context.pc] as $ty;
        $vm.current_context.pc += 1;
    };
}

macro_rules! read_int32 {
    ($vm:expr, $var:ident, $ty:ty) => {
        let $var = {
            let iseq = &$vm.current_context.bytecode;
            let pc = $vm.current_context.pc;
            ((iseq[pc as usize + 3] as $ty) << 24)
                + ((iseq[pc as usize + 2] as $ty) << 16)
                + ((iseq[pc as usize + 1] as $ty) << 8)
                + (iseq[pc as usize + 0] as $ty)
        };
        $vm.current_context.pc += 4;
    };
}

impl VM {
    pub fn run(&mut self) -> VMValueResult {
        #[derive(Debug, Clone)]
        enum SubroutineKind {
            Ordinary(usize),
            Throw,
            Return,
        }

        fn handle_exception(vm: &mut VM, subroutine_stack: &mut Vec<SubroutineKind>) -> VMResult {
            let mut trycatch_found = false;
            let save_error_info = vm.current_context.error_unknown();
            loop {
                for exception in &vm.current_context.exception_table {
                    let in_range = exception.start <= vm.current_context.pc
                        && vm.current_context.pc < exception.end;
                    if !in_range {
                        continue;
                    }
                    match exception.dst_kind {
                        DestinationKind::Catch => vm.current_context.pc = exception.end,
                        DestinationKind::Finally => {
                            subroutine_stack.push(SubroutineKind::Throw);
                            vm.current_context.pc = exception.end
                        }
                    }

                    trycatch_found = true;
                    break;
                }

                if trycatch_found {
                    break;
                }

                if vm.saved_context.len() == 0 {
                    break;
                }
                vm.unwind_context();
            }

            if !trycatch_found {
                let val: Value = vm.current_context.stack.pop().unwrap().into();
                let mut err = save_error_info;
                err.kind = ErrorKind::Exception(val);
                return Err(err);
            } else {
                Ok(())
            }
        }

        let mut subroutine_stack: Vec<SubroutineKind> = vec![];
        self.profile.trace_string = "".to_string();

        loop {
            self.current_context.current_inst_pc = self.current_context.pc;
            if self.is_profile || self.is_trace {
                self.trace_print();
            }

            macro_rules! type_error {
                ($msg:expr) => {{
                    let val = self
                        .current_context
                        .error_type($msg)
                        .to_value(&mut self.factory);
                    self.current_context.stack.push(val.into());
                    handle_exception(self, &mut subroutine_stack)?;
                    continue;
                }};
            }

            macro_rules! etry {
                ($val:expr) => {{
                    match $val {
                        Ok(ok) => ok,
                        Err(err) => {
                            let err = err.error_add_info(&self.current_context);
                            let val = err.to_value(&mut self.factory);
                            self.current_context.stack.push(val.into());
                            handle_exception(self, &mut subroutine_stack)?;
                            continue;
                        }
                    }
                }};
            }

            let inst = self.current_context.bytecode[self.current_context.pc];
            self.profile.current_inst = inst;
            match inst {
                // TODO: Macro for bin ops?
                VMInst::ADD => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.add(&mut self.factory.memory_allocator, rhs).into());
                }
                VMInst::SUB => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.sub(&mut self.factory.memory_allocator, rhs).into());
                }
                VMInst::MUL => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context.stack.push(lhs.mul(rhs).into());
                }
                VMInst::DIV => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context.stack.push(lhs.div(rhs).into());
                }
                VMInst::REM => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context.stack.push(lhs.rem(rhs).into());
                }
                VMInst::EXP => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.exp(&mut self.factory.memory_allocator, rhs).into());
                }
                VMInst::EQ => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.eq(&mut self.factory.memory_allocator, rhs).into());
                }
                VMInst::SEQ => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context.stack.push(lhs.strict_eq(rhs).into());
                }
                VMInst::NE => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.ne(&mut self.factory.memory_allocator, rhs).into());
                }
                VMInst::SNE => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context.stack.push(lhs.strict_ne(rhs).into());
                }
                VMInst::LT => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.lt(&mut self.factory.memory_allocator, rhs).into());
                }
                VMInst::LE => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.le(&mut self.factory.memory_allocator, rhs).into());
                }
                VMInst::GT => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(rhs.lt(&mut self.factory.memory_allocator, lhs).into());
                }
                VMInst::GE => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(rhs.le(&mut self.factory.memory_allocator, lhs).into());
                }
                VMInst::AND => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(rhs.and(&mut self.factory.memory_allocator, lhs).into());
                }
                VMInst::OR => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(rhs.or(&mut self.factory.memory_allocator, lhs).into());
                }
                VMInst::XOR => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(rhs.xor(&mut self.factory.memory_allocator, lhs).into());
                }
                VMInst::NOT => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(rhs.not(&mut self.factory.memory_allocator).into());
                }
                VMInst::SHL => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.shift_l(&mut self.factory.memory_allocator, rhs).into());
                }
                VMInst::SHR => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.shift_r(&mut self.factory.memory_allocator, rhs).into());
                }
                VMInst::ZFSHR => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context.stack.push(
                        lhs.z_shift_r(&mut self.factory.memory_allocator, rhs)
                            .into(),
                    );
                }
                VMInst::NEG => {
                    self.current_context.pc += 1;
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context.stack.push(val.minus().into());
                }
                VMInst::POSI => {
                    self.current_context.pc += 1;
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(val.positive(&mut self.factory.memory_allocator).into());
                }
                VMInst::LNOT => {
                    self.current_context.pc += 1;
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    let res = Value::bool(!val.to_boolean());
                    self.current_context.stack.push(res.into());
                }
                VMInst::PUSH_INT8 => {
                    self.current_context.pc += 1;
                    read_int8!(self, num, f64);
                    self.current_context.stack.push(Value::Number(num).into());
                }
                VMInst::PUSH_INT32 => {
                    self.current_context.pc += 1;
                    read_int32!(self, num, i32);
                    self.current_context
                        .stack
                        .push(Value::Number(num as f64).into());
                }
                VMInst::PUSH_CONST => {
                    self.current_context.pc += 1;
                    read_int32!(self, id, usize);
                    let val = *self.constant_table.get(id).as_value();
                    self.current_context.stack.push(val.into());
                }
                VMInst::PUSH_NULL => {
                    self.current_context.pc += 1;
                    self.current_context.stack.push(Value::null().into());
                }
                VMInst::PUSH_UNDEFINED => {
                    self.current_context.pc += 1;
                    self.current_context.stack.push(Value::undefined().into());
                }
                VMInst::PUSH_THIS => {
                    self.current_context.pc += 1;
                    self.current_context
                        .stack
                        .push(self.current_context.this.into());
                }
                VMInst::PUSH_FALSE => {
                    self.current_context.pc += 1;
                    self.current_context.stack.push(Value::Bool(0).into());
                }
                VMInst::PUSH_TRUE => {
                    self.current_context.pc += 1;
                    self.current_context.stack.push(Value::Bool(1).into());
                }
                VMInst::GET_MEMBER => {
                    self.current_context.pc += 1;
                    let property: Value = self.current_context.stack.pop().unwrap().into();
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    etry!(self.get_property_to_stack_top(parent, property))
                }
                VMInst::SET_MEMBER => {
                    self.current_context.pc += 1;
                    let property: Value = self.current_context.stack.pop().unwrap().into();
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    etry!(self.set_property_by_value(parent, property, val))
                }
                VMInst::SET_VALUE => {
                    self.current_context.pc += 1;
                    read_int32!(self, name_id, usize);
                    let val = self.current_context.stack.pop().unwrap();
                    let name = self.constant_table.get(name_id).as_string().clone();
                    etry!(self
                        .current_context
                        .lex_env_mut()
                        .set_value(name, val.into()));
                }
                VMInst::GET_VALUE => {
                    self.current_context.pc += 1;
                    read_int32!(self, name_id, usize);
                    let string = self.constant_table.get(name_id).as_string();
                    let val = etry!(self.current_context.lex_env().get_value(string.clone()));
                    self.current_context.stack.push(val.into());
                }
                VMInst::CONSTRUCT => {
                    self.current_context.pc += 1;
                    read_int32!(self, argc, usize);
                    let callee: Value = self.current_context.stack.pop().unwrap().into();
                    let mut args: Vec<Value> = vec![];
                    for _ in 0..argc {
                        args.push(self.current_context.stack.pop().unwrap().into());
                    }
                    self.enter_constructor(callee, &args)?;
                }
                VMInst::CALL => {
                    self.current_context.pc += 1;
                    read_int32!(self, argc, usize);
                    let callee: Value = self.current_context.stack.pop().unwrap().into();
                    let mut args: Vec<Value> = vec![];
                    for _ in 0..argc {
                        args.push(self.current_context.stack.pop().unwrap().into());
                    }
                    etry!(self.enter_function(callee, &args, self.current_context.this, false))
                }
                VMInst::CALL_METHOD => {
                    self.current_context.pc += 1;
                    read_int32!(self, argc, usize);
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    let method: Value = self.current_context.stack.pop().unwrap().into();
                    let mut args: Vec<Value> = vec![];
                    for _ in 0..argc {
                        args.push(self.current_context.stack.pop().unwrap().into());
                    }
                    let callee = match etry!(parent.get_property_by_value(&mut self.factory, method))
                    {
                        Property::Data(DataProperty { val, .. }) => val,
                        _ => type_error!("Not a function"),
                    };
                    etry!(self.enter_function(callee, &args, parent, false))
                }
                VMInst::SET_OUTER_ENV => {
                    self.current_context.pc += 1;
                    let func_template: Value = self.current_context.stack.pop().unwrap().into();
                    let mut func = func_template.copy_object(&mut self.factory.memory_allocator);
                    func.set_function_outer_environment(self.current_context.lexical_environment);
                    self.current_context.stack.push(func.into());
                }
                VMInst::CREATE_OBJECT => {
                    self.current_context.pc += 1;
                    read_int32!(self, id, usize);
                    self.create_object(id)?;
                    self.gc_mark();
                }
                VMInst::CREATE_ARRAY => {
                    self.current_context.pc += 1;
                    read_int32!(self, len, usize);
                    self.create_array(len)?;
                    self.gc_mark();
                }
                VMInst::DOUBLE => {
                    self.current_context.pc += 1;
                    let val = *self.current_context.stack.last().unwrap();
                    self.current_context.stack.push(val);
                }
                VMInst::PUSH_ENV => {
                    self.current_context.pc += 1;
                    read_int32!(self, id, usize);
                    self.push_env(id)?;
                }
                VMInst::POP_ENV => {
                    self.current_context.pc += 1;
                    let lex_env = self
                        .current_context
                        .saved_lexical_environment
                        .pop()
                        .unwrap();
                    self.current_context.lexical_environment = lex_env;
                }
                VMInst::POP => {
                    self.current_context.pc += 1;
                    self.current_context.stack.pop();
                }
                VMInst::JMP_IF_FALSE => {
                    self.current_context.pc += 1;
                    read_int32!(self, dst, i32);
                    let cond_boxed = self.current_context.stack.pop().unwrap();
                    let cond: Value = cond_boxed.into();
                    if !cond.to_boolean() {
                        self.current_context.pc =
                            (self.current_context.pc as isize + dst as isize) as usize;
                    }
                }
                VMInst::JMP => {
                    self.current_context.pc += 1;
                    read_int32!(self, dst, i32);
                    self.current_context.pc =
                        (self.current_context.pc as isize + dst as isize) as usize;
                }
                VMInst::JMP_SUB => {
                    self.current_context.pc += 1;
                    read_int32!(self, dst, i32);
                    subroutine_stack.push(SubroutineKind::Ordinary(self.current_context.pc));
                    self.current_context.pc =
                        (self.current_context.pc as isize + dst as isize) as usize;
                }
                VMInst::RETURN_TRY => {
                    self.current_context.pc += 1;
                    read_int32!(self, dst, i32);
                    self.current_context.pc =
                        (self.current_context.pc as isize + dst as isize) as usize;
                    subroutine_stack.push(SubroutineKind::Return);
                }
                VMInst::RETURN_SUB => {
                    self.current_context.pc += 1;
                    match subroutine_stack.pop().unwrap() {
                        SubroutineKind::Ordinary(pos) => self.current_context.pc = pos,
                        SubroutineKind::Throw => handle_exception(self, &mut subroutine_stack)?,
                        SubroutineKind::Return => {
                            self.unwind_context();
                        }
                    }
                }
                VMInst::THROW => {
                    self.current_context.pc += 1;
                    handle_exception(self, &mut subroutine_stack)?;
                }
                VMInst::RETURN => {
                    self.current_context.pc += 1;
                    let call_mode = self.current_context.call_mode;
                    if self.saved_context.len() == 0 {
                        break;
                    };
                    self.unwind_context();
                    // TODO: GC schedule
                    self.gc_mark();
                    if call_mode == CallMode::FromNative {
                        break;
                    }

                    if self.is_trace {
                        self.profile.trace_string = format!(
                            "{}\n<-- return\n  module_id:{:?} func_id:{:?}",
                            self.profile.trace_string,
                            self.current_context.module_func_id,
                            self.current_context.func_id
                        );
                    };
                }
                VMInst::TYPEOF => {
                    self.current_context.pc += 1;
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    let type_str = val.type_of();
                    let type_str_val = self.factory.string(type_str.to_string());
                    self.current_context.stack.push(type_str_val.into());
                }
                VMInst::END => break,
                _ => {
                    print!("Not yet implemented VMInst: ");
                    show_inst(
                        &self.current_context.bytecode,
                        self.current_context.pc,
                        &self.constant_table,
                    );
                    println!();
                    unimplemented!();
                }
            }
        }

        if self.is_profile || self.is_trace {
            self.trace_print();
        };
        if self.is_profile {
            self.print_profile();
        };

        let val = match self.current_context.stack.pop() {
            None => Value::undefined(),
            Some(val) => val.into(),
        };

        Ok(val)
    }

    pub fn trace_print(&mut self) {
        if self.profile.start_flag {
            let duration =
                self.profile.instant.elapsed() - self.profile.prev_time - self.profile.gc_stop_time;
            let mut inst_profile =
                &mut self.profile.inst_profile[self.profile.current_inst as usize];
            (*inst_profile).1 += duration;
            (*inst_profile).0 += 1;
            if self.is_trace {
                println!(
                    "{:6}n {:6}n {}",
                    duration.as_nanos(),
                    self.profile.gc_stop_time.as_nanos(),
                    self.profile.trace_string,
                );
            }
        } else {
            let duration =
                self.profile.instant.elapsed() - self.profile.prev_time - self.profile.gc_stop_time;
            println!("VM start up time {} microsec", duration.as_micros());
        }
        self.profile.start_flag = true;

        if self.is_trace {
            self.profile.trace_string = format!(
                "{} {}",
                crate::bytecode_gen::show_inst(
                    &self.current_context.bytecode,
                    self.current_context.current_inst_pc,
                    &self.constant_table,
                ),
                match self.current_context.stack.last() {
                    None => format!("<empty>"),
                    Some(val) => format!("{:10}", (*val).into(): Value),
                }
            );
        }

        self.profile.prev_time = self.profile.instant.elapsed();
        self.profile.gc_stop_time = Duration::from_secs(0);
    }

    pub fn print_profile(&mut self) {
        println!("# performance analysis");

        let total_inst_time = self
            .profile
            .inst_profile
            .iter()
            .fold(0, |acc, x| acc + x.1.as_micros()) as f64;

        let total_gc_time = self
            .profile
            .gc_profile
            .iter()
            .fold(0, |acc, x| acc + x.1.as_micros()) as f64;

        let total_time = total_gc_time + total_inst_time;
        println!("total execution time: {:>10} microsecs", total_time);
        println!("gc time:              {:>10} microsecs", total_gc_time);

        println!("Inst          total %    ave.time / inst");
        for (i, prof) in self.profile.inst_profile.iter().enumerate() {
            if prof.0 != 0 {
                println!(
                    "{:12} {:>6.2} % {:>10.0} nsecs",
                    inst_to_inst_name(i as u8),
                    prof.1.as_micros() as f64 / total_time * 100.0,
                    prof.1.as_nanos() as f64 / prof.0 as f64
                );
            }
        }
        println!("# GC performance");
        println!(
            "total allocated:    {:>8} bytes",
            self.factory.memory_allocator.total_allocated_size
        );
        println!(
            "total collected:    {:>8} bytes",
            self.factory.memory_allocator.total_collected_size
        );
        println!(
            "currently allocated:{:>8} bytes",
            self.factory.memory_allocator.allocated_size
        );
        println!("State    count        total time");
        let prof = self.profile.gc_profile;
        println!(
            "Init  {:>8}  {:>10.2} millisecs",
            prof[0].0,
            prof[0].1.as_millis() as f64
        );
        println!(
            "Mark  {:>8}  {:>10.2} millisecs",
            prof[1].0,
            prof[1].1.as_millis() as f64
        );
        println!(
            "Sweep {:>8}  {:>10.2} millisecs",
            prof[2].0,
            prof[2].1.as_millis() as f64
        );
    }

    /// Return from JS function.
    /// 1. Pop a Value from the stack of the current execution context.
    /// 2. Pop an ExecContext from the context stack.
    /// 3. Set current execution context to the ExecContext.
    /// 4. Push the Value to the stack of new current execution context.
    pub fn unwind_context(&mut self) {
        let prev_context = self.saved_context.pop().unwrap();
        let return_value = if self.current_context.call_mode == CallMode::ModuleCall {
            self.current_context
                .lex_env()
                .get_value("module")
                .unwrap()
                .get_object_info()
                .get_property("exports")
        } else {
            let ret_val_boxed = self.current_context.stack.pop().unwrap();
            let ret_val: Value = ret_val_boxed.into();
            if self.current_context.constructor_call && !ret_val.is_object() {
                self.current_context.this
            } else {
                ret_val
            }
        };
        self.current_context = prev_context;
        self.current_context.stack.push(return_value.into());
    }

    fn push_env(&mut self, id: usize) -> VMResult {
        let lex_names = self.constant_table.get(id).as_lex_env_info().clone();
        let outer = self.current_context.lexical_environment;

        let lex_env = self.create_lexical_environment(&lex_names, outer);

        self.current_context
            .saved_lexical_environment
            .push(self.current_context.lexical_environment);
        self.current_context.lexical_environment = lex_env;

        Ok(())
    }

    fn create_object(&mut self, id: usize) -> VMResult {
        let (len, special_properties) = self.constant_table.get(id).as_object_literal_info();
        let mut properties = FxHashMap::default();

        for i in 0..len {
            let prop: Value = self.current_context.stack.pop().unwrap().into();
            let name = prop.to_string();
            let val: Value = self.current_context.stack.pop().unwrap().into();
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

        let obj = self.factory.object(properties);
        self.current_context.stack.push(obj.into());

        Ok(())
    }

    fn create_array(&mut self, len: usize) -> VMResult {
        let mut elems = vec![];
        for _ in 0..len {
            let val: Value = self.current_context.stack.pop().unwrap().into();
            elems.push(Property::Data(DataProperty {
                val,
                writable: true,
                enumerable: true,
                configurable: true,
            }));
        }

        let ary = self.factory.array(elems);
        self.current_context.stack.push(ary.into());

        Ok(())
    }

    fn enter_constructor(&mut self, callee: Value, args: &[Value]) -> VMResult {
        let this = Value::Object(self.factory.alloc(ObjectInfo {
            kind: ObjectKind::Ordinary,
            prototype: callee.get_property("prototype"),
            property: FxHashMap::default(),
            sym_property: FxHashMap::default(),
        }));

        self.enter_function(callee, args, this, true)
    }

    fn enter_function(
        &mut self,
        callee: Value,
        args: &[Value],
        this: Value,
        constructor_call: bool,
    ) -> VMResult {
        if !callee.is_function_object() {
            return Err(self.current_context.error_type("Not a function"));
        }

        let info = callee.as_function();
        let ret = match info.kind {
            FunctionObjectKind::Builtin(func) => gc_lock!(self, args, {
                let val = func(self, args, this)?;
                self.current_context.stack.push(val.into());
                Ok(())
            }),
            FunctionObjectKind::User {
                ref info,
                outer_env,
            } => {
                if self.is_trace {
                    self.profile.trace_string = format!(
                        "{}\n--> call {}\n  module_id:{:?} func_id:{:?}",
                        self.profile.trace_string,
                        if constructor_call {
                            "constructor"
                        } else {
                            "function"
                        },
                        self.current_context.module_func_id,
                        self.current_context.func_id
                    );
                };
                self.enter_user_function(info.clone(), outer_env, args, this, constructor_call)
            }
        };
        ret
    }

    fn create_declarative_environment<F>(
        &mut self,
        f: F,
        outer: Option<LexicalEnvironmentRef>,
    ) -> LexicalEnvironmentRef
    where
        F: Fn(&mut VM, &mut FxHashMap<String, Value>),
    {
        let env = LexicalEnvironment {
            record: EnvironmentRecord::Declarative({
                let mut record = FxHashMap::default();
                f(self, &mut record);
                record
            }),
            outer,
        };

        LexicalEnvironmentRef(self.factory.alloc(env))
    }

    fn create_variable_environment(
        &mut self,
        var_names: &Vec<String>,
        outer_env_ref: LexicalEnvironmentRef,
    ) -> LexicalEnvironmentRef {
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
        outer_env_ref: LexicalEnvironmentRef,
    ) -> LexicalEnvironmentRef {
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
        user_func: &UserFunctionInfo,
        outer_env: Option<LexicalEnvironmentRef>,
        args: &[Value],
        this: Value,
    ) -> LexicalEnvironmentRef {
        let env = LexicalEnvironment {
            record: EnvironmentRecord::Function {
                record: {
                    let mut record = FxHashMap::default();
                    for name in &user_func.var_names {
                        record.insert(name.clone(), Value::undefined());
                    }
                    for (i, FunctionParameter { name, rest_param }) in
                        user_func.params.iter().enumerate()
                    {
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
            outer: outer_env,
        };

        LexicalEnvironmentRef(self.factory.alloc(env))
    }

    /// Prepare a new context before invoking function.
    /// 1. Push current context to the context stack.
    /// 2. Set `this`.
    /// 3. Create a new function environment.
    /// 4. Prepare function objects defined inner the function, and register them to the lexical environment.
    /// 5. Generate a new context, and set the current context (running execution context) to it.
    pub fn prepare_context_for_function_invokation(
        &mut self,
        user_func: &UserFunctionInfo,
        outer_env: Option<LexicalEnvironmentRef>,
        args: &[Value],
        this: Value,
        mode: CallMode,
        constructor_call: bool,
    ) -> Result<(), RuntimeError> {
        let context = std::mem::replace(&mut self.current_context, ExecContext::empty());
        self.saved_context.push(context);

        let this = if user_func.this_mode == ThisMode::Lexical {
            // Arrow function
            outer_env.unwrap().get_this_binding()
        } else {
            this
        };

        let var_env_ref = self.create_function_environment(&user_func, outer_env, args, this);

        let mut lex_env_ref = self.create_lexical_environment(&user_func.lex_names, var_env_ref);

        for func_info in &user_func.func_decls {
            let name = func_info.func_name.clone().unwrap();
            let mut func = self
                .factory
                .function(func_info.func_name.clone(), func_info.clone());
            func.set_function_outer_environment(lex_env_ref);
            lex_env_ref.set_value(name, func)?;
        }

        let context = ExecContext::new(
            user_func.code.clone(),
            var_env_ref,
            lex_env_ref,
            user_func.exception_table.clone(),
            this,
            mode,
        )
        .func_id(user_func.func_id)
        .module_func_id(user_func.module_func_id)
        .constructor_call(constructor_call);
        self.current_context = context;
        Ok(())
    }

    fn enter_user_function(
        &mut self,
        user_func: UserFunctionInfo,
        outer_env: Option<LexicalEnvironmentRef>,
        args: &[Value],
        this: Value,
        constructor_call: bool,
    ) -> VMResult {
        if !user_func.constructible && constructor_call {
            return Err(self.current_context.error_type("Not a constructor"));
        }

        self.prepare_context_for_function_invokation(
            &user_func,
            outer_env,
            args,
            this,
            CallMode::OrdinaryCall,
            constructor_call,
        )?;

        Ok(())
    }
}
