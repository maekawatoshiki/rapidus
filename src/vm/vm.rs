use crate::builtin::BuiltinFuncTy;
use crate::builtins::console::debug_print;
use crate::bytecode_gen::{show_inst, ByteCode, VMInst};
use crate::gc;
use crate::node::Node;
use crate::parser::ScriptInfo;
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

pub type VMResult = Result<(), RuntimeError>;
/// Ok(Value::Other(Empty)) means mudule call.
pub type VMValueResult = Result<Value, RuntimeError>;

#[derive(Debug)]
pub struct Factory {
    pub memory_allocator: gc::MemoryAllocator,
    pub object_prototypes: ObjectPrototypes,
}

pub struct VM {
    pub factory: Factory,
    pub global_environment: frame::LexicalEnvironmentRef,
    pub constant_table: constant::ConstantTable,
    pub global_symbol_registry: GlobalSymbolRegistry,
    pub stack: Vec<BoxedValue>,
    pub saved_frame: Vec<frame::Frame>,
    ///func_id, ToSourcePos
    pub to_source_map: FxHashMap<usize, codegen::ToSourcePos>,
    pub is_trace: bool,
    ///(func_id, script_info)
    pub script_info: Vec<(usize, ScriptInfo)>,
}

macro_rules! gc_lock {
    ($vm:ident, $targets:expr, $($body:tt)*) => { {
        for arg in $targets { $vm.factory.memory_allocator.lock(*arg) }
        let ret = { $($body)* };
        for arg in $targets { $vm.factory.memory_allocator.unlock(*arg) }
        ret
    } }
}

impl Factory {
    pub fn new(memory_allocator: gc::MemoryAllocator, object_prototypes: ObjectPrototypes) -> Self {
        Factory {
            memory_allocator,
            object_prototypes,
        }
    }

    pub fn alloc<T: gc::GcTarget + 'static>(&mut self, data: T) -> *mut T {
        self.memory_allocator.alloc(data)
    }

    /// Generate Value for a string.
    pub fn string(&mut self, body: impl Into<String>) -> Value {
        Value::String(self.alloc(std::ffi::CString::new(body.into()).unwrap()))
    }

    /// Generate Value for an object.
    pub fn object(&mut self, property: FxHashMap<String, Property>) -> Value {
        Value::Object(self.alloc(ObjectInfo {
            kind: ObjectKind::Ordinary,
            prototype: self.object_prototypes.object,
            property,
            sym_property: FxHashMap::default(),
        }))
    }

    /// Generate Value for a JS function.
    pub fn function(&mut self, name: Option<String>, info: UserFunctionInfo) -> Value {
        let name_prop = self.string(name.clone().unwrap_or("".to_string()));
        let prototype = self.object(FxHashMap::default());

        let f = Value::Object(self.alloc(ObjectInfo {
            prototype: self.object_prototypes.function,
            property: make_property_map!(
                length    => false, false, true : Value::Number(info.params.len() as f64), /* TODO: rest param */
                name      => false, false, true : name_prop,
                prototype => true , false, false: prototype
            ),
            kind: ObjectKind::Function(FunctionObjectInfo {
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

    /// Generate Value for a built-in (native) function.
    pub fn builtin_function(
        &mut self,
        name: impl Into<String>,
        func: crate::builtin::BuiltinFuncTy,
    ) -> Value {
        let name: String = name.into();
        let name_prop = self.string(name.clone());
        Value::Object(self.alloc(ObjectInfo {
            kind: ObjectKind::Function(FunctionObjectInfo {
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
        Value::Object(self.alloc(ObjectInfo {
            kind: ObjectKind::Array(ArrayObjectInfo { elems }),
            prototype: self.object_prototypes.array,
            property: make_property_map!(),
            sym_property: FxHashMap::default(),
        }))
    }

    pub fn symbol(&mut self, description: Option<String>) -> Value {
        Value::Object(self.alloc(ObjectInfo {
            kind: ObjectKind::Symbol(SymbolInfo {
                id: crate::id::get_unique_id(),
                description,
            }),
            prototype: self.object_prototypes.symbol,
            property: make_property_map!(),
            sym_property: FxHashMap::default(),
        }))
    }

    pub fn error(&mut self, message: impl Into<String>) -> Value {
        let message = self.string(message.into());
        Value::Object(self.alloc(ObjectInfo {
            kind: ObjectKind::Error(ErrorObjectInfo::new()),
            prototype: self.object_prototypes.error,
            property: make_property_map!(
                message => true, false, true: message
            ),
            sym_property: FxHashMap::default(),
        }))
    }

    pub fn generate_builtin_constructor(
        &mut self,
        constructor_name: impl Into<String>,
        constructor_func: BuiltinFuncTy,
        prototype: Value,
    ) -> Value {
        let ary = self.builtin_function(constructor_name, constructor_func);
        ary.set_property_by_string_key("prototype", prototype);
        ary.get_property_by_str_key("prototype")
            .set_constructor(ary);
        ary
    }
}

impl VM {
    pub fn new() -> Self {
        let mut memory_allocator = gc::MemoryAllocator::new();
        let object_prototypes = ObjectPrototypes::new(&mut memory_allocator);
        let mut factory = Factory::new(memory_allocator, object_prototypes);
        let global_env = frame::LexicalEnvironment::new_global_initialized(&mut factory);
        let global_environment = frame::LexicalEnvironmentRef(factory.alloc(global_env));
        VM {
            global_environment,
            factory,
            constant_table: constant::ConstantTable::new(),
            global_symbol_registry: GlobalSymbolRegistry::new(),
            stack: vec![],
            saved_frame: vec![],
            to_source_map: FxHashMap::default(),
            is_trace: false,
            script_info: vec![],
        }
    }

    pub fn trace(mut self) -> Self {
        self.is_trace = true;
        self
    }

    pub fn gc_mark(&mut self, cur_frame: &frame::Frame) {
        self.factory.memory_allocator.mark(
            self.global_environment,
            &self.factory.object_prototypes,
            &self.constant_table,
            &self.stack,
            cur_frame,
            &self.saved_frame,
        );
    }
    pub fn compile(
        &mut self,
        node: &Node,
        iseq: &mut ByteCode,
        use_value: bool,
        id: usize,
    ) -> Result<codegen::FunctionInfo, codegen::Error> {
        let mut code_generator =
            CodeGenerator::new(&mut self.constant_table, &mut self.factory, id);
        let res = code_generator.compile(node, iseq, use_value, id);
        for (id, list) in code_generator.to_source_map {
            self.to_source_map.insert(id, list);
        }
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
            let mut val = val.copy_object(&mut self.factory.memory_allocator);
            let name = val.as_function().name.clone().unwrap();
            val.set_function_outer_environment(lex_env);
            lex_env.set_value(name, val).unwrap();
        }

        let frame = frame::Frame::new(
            iseq,
            var_env,
            lex_env,
            global_info.exception_table,
            global_env_ref.get_global_object(),
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
        cur_frame: &mut frame::Frame,
    ) -> VMValueResult {
        if !callee.is_function_object() {
            return Err(cur_frame.error_type("Not a function"));
        }

        let info = callee.as_function();

        match info.kind {
            FunctionObjectKind::Builtin(func) => {
                gc_lock!(self, args, func(self, args, this, cur_frame))
            }
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
    ) -> VMValueResult {
        let frame = self
            .prepare_frame_for_function_invokation(user_func, args, this, cur_frame)?
            .constructor_call(constructor_call)
            .escape();

        self.run(frame)
    }

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
        cur_frame: &mut frame::Frame,
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
        cur_frame: &mut frame::Frame,
    ) -> VMResult {
        let maybe_setter = parent.set_property(&mut self.factory.memory_allocator, key, val)?;
        if let Some(setter) = maybe_setter {
            self.call_function(setter, &[val], parent, cur_frame)?;
            self.stack.pop().unwrap(); // Pop undefined (setter's return value)
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

impl VM {
    pub fn run(&mut self, mut cur_frame: frame::Frame) -> VMValueResult {
        #[derive(Debug, Clone)]
        enum SubroutineKind {
            Ordinary(usize),
            Throw,
            Return,
        }

        fn handle_exception(
            vm: &mut VM,
            cur_frame: &mut frame::Frame,
            subroutine_stack: &mut Vec<SubroutineKind>,
        ) -> VMResult {
            let mut trycatch_found = false;
            //TODO: too expensive!
            let old_frame = cur_frame.clone();
            loop {
                for exception in &cur_frame.exception_table {
                    let in_range = exception.start <= cur_frame.pc && cur_frame.pc < exception.end;
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

                    trycatch_found = true;
                    break;
                }

                if trycatch_found {
                    break;
                }

                if vm.saved_frame.len() == 0 {
                    break;
                }
                vm.unwind_frame_saving_stack_top(cur_frame);
            }

            if !trycatch_found {
                let val: Value = vm.stack.pop().unwrap().into();
                return Err(old_frame.error_exception(val));
            } else {
                Ok(())
            }
        }

        let mut subroutine_stack: Vec<SubroutineKind> = vec![];
        let is_trace = self.is_trace;

        loop {
            cur_frame.current_inst_pc = cur_frame.pc;

            macro_rules! type_error {
                ($msg:expr) => {{
                    let val = cur_frame.error_type($msg).to_value(&mut self.factory);
                    self.stack.push(val.into());
                    handle_exception(self, &mut cur_frame, &mut subroutine_stack)?;
                    continue;
                }};
            }

            macro_rules! etry {
                ($val:expr) => {{
                    match $val {
                        Ok(ok) => ok,
                        Err(err) => {
                            let err = err.error_add_info(&cur_frame);
                            let val = err.to_value(&mut self.factory);
                            self.stack.push(val.into());
                            handle_exception(self, &mut cur_frame, &mut subroutine_stack)?;
                            continue;
                        }
                    }
                }};
            }

            if is_trace {
                crate::bytecode_gen::show_inst(
                    &cur_frame.bytecode,
                    cur_frame.current_inst_pc,
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
                        .push(lhs.add(&mut self.factory.memory_allocator, rhs).into());
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
                        .push(lhs.exp(&mut self.factory.memory_allocator, rhs).into());
                }
                VMInst::EQ => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(lhs.eq(&mut self.factory.memory_allocator, rhs).into());
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
                        .push(lhs.ne(&mut self.factory.memory_allocator, rhs).into());
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
                        .push(lhs.lt(&mut self.factory.memory_allocator, rhs).into());
                }
                VMInst::LE => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(lhs.le(&mut self.factory.memory_allocator, rhs).into());
                }
                VMInst::GT => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(rhs.lt(&mut self.factory.memory_allocator, lhs).into());
                }
                VMInst::GE => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(rhs.le(&mut self.factory.memory_allocator, lhs).into());
                }
                VMInst::AND => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(rhs.and(&mut self.factory.memory_allocator, lhs).into());
                }
                VMInst::OR => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(rhs.or(&mut self.factory.memory_allocator, lhs).into());
                }
                VMInst::XOR => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(rhs.xor(&mut self.factory.memory_allocator, lhs).into());
                }
                VMInst::NOT => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(rhs.not(&mut self.factory.memory_allocator).into());
                }
                VMInst::SHL => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(lhs.shift_l(&mut self.factory.memory_allocator, rhs).into());
                }
                VMInst::SHR => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack
                        .push(lhs.shift_r(&mut self.factory.memory_allocator, rhs).into());
                }
                VMInst::ZFSHR => {
                    cur_frame.pc += 1;
                    let rhs: Value = self.stack.pop().unwrap().into();
                    let lhs: Value = self.stack.pop().unwrap().into();
                    self.stack.push(
                        lhs.z_shift_r(&mut self.factory.memory_allocator, rhs)
                            .into(),
                    );
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
                        .push(val.positive(&mut self.factory.memory_allocator).into());
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
                    etry!(self.set_property(parent, property, val, &mut cur_frame))
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
                    let string = self.constant_table.get(name_id).as_string();
                    let val = etry!(cur_frame.lex_env().get_value(string.clone()));
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
                    let mut func = func_template.copy_object(&mut self.factory.memory_allocator);
                    func.set_function_outer_environment(cur_frame.lexical_environment);
                    self.stack.push(func.into());
                }
                VMInst::CREATE_OBJECT => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, id, usize);
                    self.create_object(id)?;
                    self.gc_mark(&cur_frame);
                }
                VMInst::CREATE_ARRAY => {
                    cur_frame.pc += 1;
                    read_int32!(cur_frame.bytecode, cur_frame.pc, len, usize);
                    self.create_array(len)?;
                    self.gc_mark(&cur_frame);
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
                    let lex_env = cur_frame.saved_lexical_environment.pop().unwrap();
                    cur_frame.lexical_environment = lex_env;
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
                        SubroutineKind::Throw => {
                            handle_exception(self, &mut cur_frame, &mut subroutine_stack)?
                        }
                        SubroutineKind::Return => {
                            self.unwind_frame_saving_stack_top(&mut cur_frame);
                        }
                    }
                }
                VMInst::THROW => {
                    cur_frame.pc += 1;
                    handle_exception(self, &mut cur_frame, &mut subroutine_stack)?;
                }
                VMInst::RETURN => {
                    cur_frame.pc += 1;
                    let escape = cur_frame.escape;
                    if self.saved_frame.len() == 0 {
                        break;
                    };
                    self.unwind_frame_saving_stack_top(&mut cur_frame);
                    // TODO: GC schedule
                    self.gc_mark(&cur_frame);
                    if escape {
                        break;
                    }
                    if is_trace {
                        println!("<-- return");
                        println!(
                            "  module_id:{} func_id:{}",
                            cur_frame.module_func_id, cur_frame.func_id
                        );
                    };
                }
                VMInst::TYPEOF => {
                    cur_frame.pc += 1;
                    let val: Value = self.stack.pop().unwrap().into();
                    let type_str = val.type_of();
                    let type_str_val = self.factory.string(type_str.to_string());
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

        let val = match self.stack.pop() {
            None => Value::undefined(),
            Some(val) => val.into(),
        };
        println!("{}", val);
        Ok(val)
    }

    pub fn unwind_frame_saving_stack_top(&mut self, cur_frame: &mut frame::Frame) {
        let ret_val_boxed = self.stack.pop().unwrap();
        let ret_val: Value = ret_val_boxed.into();
        let frame = self.saved_frame.pop().unwrap();
        self.stack.truncate(frame.saved_stack_len);
        let return_value = if cur_frame.module_call {
            cur_frame
                .lex_env()
                .get_value("module")
                .unwrap()
                .get_object_info()
                .get_property_by_str_key("exports")
                .into()
        } else if cur_frame.constructor_call && !ret_val.is_object() {
            cur_frame.this.into()
        } else {
            ret_val_boxed
        };
        self.stack.push(return_value);
        *cur_frame = frame;
    }

    pub fn unwind_frame(&mut self, cur_frame: &mut frame::Frame) {
        let frame = self.saved_frame.pop().unwrap();
        self.stack.truncate(frame.saved_stack_len);
        *cur_frame = frame;
    }

    fn push_env(&mut self, id: usize, cur_frame: &mut frame::Frame) -> VMResult {
        let lex_names = self.constant_table.get(id).as_lex_env_info().clone();
        let outer = cur_frame.lexical_environment;

        let lex_env = self.create_lexical_environment(&lex_names, outer);

        cur_frame
            .saved_lexical_environment
            .push(cur_frame.lexical_environment);
        cur_frame.lexical_environment = lex_env;

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

        let obj = self.factory.object(properties);
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
        let this = Value::Object(self.factory.alloc(ObjectInfo {
            kind: ObjectKind::Ordinary,
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
            return Err(cur_frame.error_type("Not a function"));
        }

        let info = callee.as_function();
        let ret = match info.kind {
            FunctionObjectKind::Builtin(func) => gc_lock!(self, args, {
                let val = func(self, args, this, cur_frame)?;
                self.stack.push(val.into());
                Ok(())
            }),
            FunctionObjectKind::User(ref user_func) => {
                if self.is_trace {
                    println!("--> call function",);
                    println!(
                        "  module_id:{} func_id:{}",
                        user_func.module_func_id, user_func.id
                    );
                };
                self.enter_user_function(user_func.clone(), args, this, cur_frame, constructor_call)
            }
        };
        ret
    }

    fn create_declarative_environment<F>(
        &mut self,
        f: F,
        outer: Option<frame::LexicalEnvironmentRef>,
    ) -> frame::LexicalEnvironmentRef
    where
        F: Fn(&mut VM, &mut FxHashMap<String, Value>),
    {
        let env = frame::LexicalEnvironment {
            record: frame::EnvironmentRecord::Declarative({
                let mut record = FxHashMap::default();
                f(self, &mut record);
                record
            }),
            outer,
        };

        frame::LexicalEnvironmentRef(self.factory.alloc(env))
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

        frame::LexicalEnvironmentRef(self.factory.alloc(env))
    }

    /// Prepare a new frame before invoking function.
    /// 1. save current frame to the frame stack.
    /// 2. set `this`.
    /// 3. create a new function environment.
    /// 4. prepare function objects defined inner the function, and register them to the lexical environment.
    /// 5. generate a new frame, and return it.
    pub fn prepare_frame_for_function_invokation(
        &mut self,
        user_func: &UserFunctionInfo,
        args: &[Value],
        this: Value,
        cur_frame: &frame::Frame,
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
            let mut func = func.copy_object(&mut self.factory.memory_allocator);
            let name = func.as_function().name.clone().unwrap();
            func.set_function_outer_environment(lex_env_ref);
            lex_env_ref.set_value(name, func)?;
        }

        let user_func = user_func.clone();

        Ok(frame::Frame::new(
            user_func.code,
            var_env_ref,
            lex_env_ref,
            user_func.exception_table,
            this,
        )
        .func_id(user_func.id)
        .module_func_id(user_func.module_func_id))
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
            return Err(cur_frame.error_type("Not a constructor"));
        }

        let frame = self
            .prepare_frame_for_function_invokation(&user_func, args, this, cur_frame)?
            .constructor_call(constructor_call);

        *cur_frame = frame;

        Ok(())
    }
}
