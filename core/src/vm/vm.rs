use crate::builtins::console::debug_print;
use crate::bytecode_gen::{inst_to_inst_name, show_inst, VMInst};
use crate::gc;
pub use crate::vm::exec_context::{
    EnvironmentRecord, ExecContext, LexicalEnvironment, LexicalEnvironmentRef, PendingReference,
};
pub use crate::vm::factory::{Factory, FunctionId};
pub use crate::vm::jsvalue::function::{DestinationKind, FunctionParameter, ThisMode};
use crate::vm::{
    codegen,
    codegen::CodeGenerator,
    constant,
    error::*,
    jsvalue::prototype::ObjectPrototypes,
    jsvalue::symbol::{GlobalSymbolRegistry, SYMBOL_TO_PRIMITIVE_ID},
    jsvalue::value::*,
    promise_job::PromiseJob,
};
use rapidus_ast::Node;
use rapidus_lexer::get_error_line;
use rapidus_parser::script::ScriptInfo;
use rustc_hash::FxHashMap;
use std::collections::VecDeque;
use std::time::{Duration, Instant};

pub type VMResult = Result<(), RuntimeError>;
/// Ok(Value::Other(Empty)) means mudule call.
pub type VMValueResult = Result<Value, RuntimeError>;

fn array_index_key(key: &str) -> Option<u32> {
    let index = key.parse::<u32>().ok()?;
    if key == index.to_string() && index != u32::MAX {
        Some(index)
    } else {
        None
    }
}

fn proxy_target(value: Value) -> Option<Value> {
    match value {
        Value::Object(info) => match ObjectRef(info).kind {
            ObjectKind::Proxy(ref proxy) => Some(proxy.target),
            _ => None,
        },
        _ => None,
    }
}

fn proxy_info(value: Value) -> Option<ProxyObjectInfo> {
    match value {
        Value::Object(info) => match ObjectRef(info).kind {
            ObjectKind::Proxy(ref proxy) => Some(proxy.clone()),
            _ => None,
        },
        _ => None,
    }
}

fn property_key_same_value(lhs: Value, rhs: Value) -> bool {
    if lhs.is_symbol() || rhs.is_symbol() {
        return lhs.is_symbol()
            && rhs.is_symbol()
            && lhs.get_symbol_info().id == rhs.get_symbol_info().id;
    }
    lhs.to_string() == rhs.to_string()
}

pub struct VM {
    pub factory: Factory,
    pub global_environment: LexicalEnvironmentRef,
    pub constant_table: constant::ConstantTable,
    pub global_symbol_registry: GlobalSymbolRegistry,
    pub current_context: ExecContext,
    pub saved_context: Vec<ExecContext>,
    pub is_called_from_native: bool,
    pub builtin_constructor_call: bool,
    pub builtin_callee: Value,
    pub builtin_new_target: Value,
    pub direct_eval_call: bool,
    pub generator_yielded: bool,
    ///func_id, ToSourcePos
    pub to_source_map: FxHashMap<FunctionId, codegen::ToSourcePos>,
    pub is_profile: bool,
    pub is_trace: bool,
    pub script_info: FxHashMap<FunctionId, ScriptInfo>,
    promise_jobs: VecDeque<PromiseJob>,
    pub profile: Profiler,
}

pub struct Profiler {
    instant: Instant,
    prev_time: Duration,
    current_inst: u8,
    inst_profile: [(usize, Duration); 256],
    gc_profile: [(usize, Duration); 3],
    gc_stop_time: Duration,
    trace_string: String,
    start_flag: bool,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CallMode {
    Ordinary,
    Module,
    Native,
}

impl VM {
    pub fn new() -> Self {
        let memory_allocator = gc::MemoryAllocator::new();
        let mut factory = Factory::new(memory_allocator, ObjectPrototypes::dummy());
        factory.object_prototypes = ObjectPrototypes::new(&mut factory);
        let global_env = LexicalEnvironment::new_global_initialized(&mut factory);
        let global_environment = LexicalEnvironmentRef(factory.alloc(global_env));
        VM {
            global_environment,
            factory,
            constant_table: constant::ConstantTable::new(),
            global_symbol_registry: GlobalSymbolRegistry::new(),
            current_context: ExecContext::empty(),
            saved_context: vec![],
            is_called_from_native: false,
            builtin_constructor_call: false,
            builtin_callee: Value::undefined(),
            builtin_new_target: Value::undefined(),
            direct_eval_call: false,
            generator_yielded: false,
            to_source_map: FxHashMap::default(),
            is_profile: false,
            is_trace: false,
            script_info: FxHashMap::default(),
            promise_jobs: VecDeque::new(),
            profile: Profiler {
                instant: Instant::now(),
                prev_time: Duration::from_secs(0),
                gc_stop_time: Duration::from_secs(0),
                gc_profile: [(0, Duration::from_secs(0)); 3],
                current_inst: 255,
                trace_string: "".to_string(),
                inst_profile: [(0, Duration::from_micros(0)); 256],
                start_flag: false,
            },
        }
    }

    fn to_number(&mut self, value: Value) -> Result<f64, RuntimeError> {
        if value.is_symbol() || value.is_bigint() {
            return Err(self.current_context.error_type("Cannot convert to Number"));
        }
        if !value.is_object() {
            return Ok(value.to_number(&mut self.factory.memory_allocator));
        }
        if let Some(primitive) = self.call_to_primitive(value, "number")? {
            if primitive.is_symbol() || primitive.is_bigint() {
                return Err(self.current_context.error_type("Cannot convert to Number"));
            }
            return Ok(primitive.to_number(&mut self.factory.memory_allocator));
        }
        for method_name in ["valueOf", "toString"] {
            let key = self.factory.string(method_name);
            let method = self.get_property_by_value(value, key)?;
            if method.is_function_object() {
                let primitive = self.call_function(method, &[], value)?;
                if primitive.is_symbol() || primitive.is_bigint() {
                    return Err(self.current_context.error_type("Cannot convert to Number"));
                }
                if !primitive.is_object() {
                    return Ok(primitive.to_number(&mut self.factory.memory_allocator));
                }
            }
        }
        Err(self
            .current_context
            .error_type("Cannot convert object to Number"))
    }

    pub(crate) fn to_primitive(&mut self, value: Value, hint: &str) -> Result<Value, RuntimeError> {
        if !value.is_object() {
            return Ok(value);
        }
        if let Some(primitive) = self.call_to_primitive(value, hint)? {
            return Ok(primitive);
        }
        let method_names = if hint == "string" {
            ["toString", "valueOf"]
        } else {
            ["valueOf", "toString"]
        };
        for method_name in method_names {
            let key = self.factory.string(method_name);
            let method = self.get_property_by_value(value, key)?;
            if self.is_callable(method) {
                let primitive = self.call_function(method, &[], value)?;
                if !primitive.is_object() || primitive.is_symbol() || primitive.is_bigint() {
                    return Ok(primitive);
                }
            }
        }
        Err(self
            .current_context
            .error_type("Cannot convert object to primitive"))
    }

    pub fn to_string(&mut self, value: Value) -> Result<String, RuntimeError> {
        if value.is_symbol() {
            return Err(self
                .current_context
                .error_type("Cannot convert Symbol to string"));
        }
        let primitive = if value.is_object() {
            self.to_primitive(value, "string")?
        } else {
            value
        };
        if primitive.is_symbol() {
            return Err(self
                .current_context
                .error_type("Cannot convert Symbol to string"));
        }
        Ok(primitive.to_string())
    }

    fn add(&mut self, lhs: Value, rhs: Value) -> VMValueResult {
        let lhs = self.to_primitive(lhs, "default")?;
        let rhs = self.to_primitive(rhs, "default")?;
        if lhs.is_string() || rhs.is_string() {
            let lhs = self.to_string(lhs)?;
            let rhs = self.to_string(rhs)?;
            return Ok(self.factory.string(format!("{}{}", lhs, rhs)));
        }
        if lhs.is_bigint() && rhs.is_bigint() {
            return Ok(lhs.add(&mut self.factory.memory_allocator, rhs));
        }
        if lhs.is_bigint() || rhs.is_bigint() {
            return Err(self
                .current_context
                .error_type("Cannot mix BigInt and other types"));
        }
        if lhs.is_symbol() || rhs.is_symbol() {
            return Err(self
                .current_context
                .error_type("Cannot convert Symbol to Number"));
        }
        Ok(Value::Number(
            lhs.to_number(&mut self.factory.memory_allocator)
                + rhs.to_number(&mut self.factory.memory_allocator),
        ))
    }

    fn call_to_primitive(
        &mut self,
        value: Value,
        hint: &str,
    ) -> Result<Option<Value>, RuntimeError> {
        let key = self.factory.symbol_with_id(
            SYMBOL_TO_PRIMITIVE_ID,
            Some("Symbol.toPrimitive".to_string()),
        );
        let method = self.get_property_by_value(value, key)?;
        if method.is_null() || method.is_undefined() {
            return Ok(None);
        }
        if !method.is_function_object() {
            return Err(self.current_context.error_type("Symbol.toPrimitive"));
        }
        let hint = self.factory.string(hint);
        let primitive = self.call_function(method, &[hint], value)?;
        if primitive.is_object() && !primitive.is_symbol() && !primitive.is_bigint() {
            return Err(self
                .current_context
                .error_type("Cannot convert object to primitive"));
        }
        Ok(Some(primitive))
    }

    pub fn profile(mut self) -> Self {
        self.is_profile = true;
        self
    }

    pub fn trace(mut self) -> Self {
        self.is_trace = true;
        self
    }

    // GC mark is temporarily disabled (the early `return` short-circuits the body).
    // The implementation below is kept for future re-enabling.
    #[allow(unreachable_code)]
    pub fn gc_mark(&mut self) {
        return;
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

    pub fn compile(&mut self, node: &Node, use_value: bool) -> Result<FuncInfoRef, codegen::Error> {
        let func_id = self.factory.new_func_id();
        let mut code_generator =
            CodeGenerator::new(&mut self.constant_table, &mut self.factory, func_id);
        let res = code_generator.compile(node, use_value);
        for (func_id, list) in code_generator.to_source_map {
            self.to_source_map.insert(func_id, list);
        }
        res
    }

    pub fn create_global_context(&mut self, global_info: FuncInfoRef) -> ExecContext {
        let mut global_env_ref = self.global_environment;

        for name in &global_info.var_names {
            global_env_ref
                .set_own_value(name.clone(), Value::undefined())
                .unwrap();
        }

        let var_env = global_env_ref;

        let mut lex_env = self.factory.create_lexical_environment(
            &global_info.lex_names,
            &global_info.const_names,
            var_env,
        );

        for info in &global_info.func_decls {
            let name = info.func_name.as_ref().unwrap().as_str();
            let val = self.factory.function(*info, lex_env);
            lex_env.set_value(name, val).unwrap();
        }

        let context = ExecContext::new(
            var_env,
            lex_env,
            global_info,
            global_env_ref.get_global_object(),
            CallMode::Ordinary,
            Value::undefined(),
            Value::undefined(),
        );

        context
    }

    pub fn run_global(&mut self, func_info: FuncInfoRef) -> VMResult {
        self.current_context = self.create_global_context(func_info);
        self.run()?;
        self.drain_promise_jobs()?;

        Ok(())
    }

    pub fn enqueue_promise_reaction(
        &mut self,
        state: i32,
        result: Value,
        child: Value,
        on_fulfilled: Value,
        on_rejected: Value,
    ) {
        self.promise_jobs.push_back(PromiseJob::Reaction {
            state,
            result,
            child,
            on_fulfilled,
            on_rejected,
        });
    }

    pub fn enqueue_promise_thenable(
        &mut self,
        promise: Value,
        thenable: Value,
        then_action: Value,
    ) {
        self.promise_jobs.push_back(PromiseJob::ResolveThenable {
            promise,
            thenable,
            then_action,
        });
    }

    pub fn drain_promise_jobs(&mut self) -> VMResult {
        while let Some(job) = self.promise_jobs.pop_front() {
            match job {
                PromiseJob::Reaction {
                    state,
                    result,
                    child,
                    on_fulfilled,
                    on_rejected,
                } => crate::builtins::promise::run_reaction(
                    self,
                    state,
                    result,
                    child,
                    on_fulfilled,
                    on_rejected,
                )?,
                PromiseJob::ResolveThenable {
                    promise,
                    thenable,
                    then_action,
                } => {
                    crate::builtins::promise::run_thenable_job(
                        self,
                        promise,
                        thenable,
                        then_action,
                    )?;
                }
            }
        }
        Ok(())
    }

    pub fn call_function(&mut self, callee: Value, args: &[Value], this: Value) -> VMValueResult {
        if let Some(proxy) = proxy_info(callee) {
            return self.proxy_apply(proxy, this, args);
        }
        if !callee.is_function_object() {
            return Err(self.current_context.error_type("Not a function"));
        }

        if let Some((bound_target, bound_this, bound_args)) =
            self.bound_function_parts(callee, args)
        {
            return self.call_function(bound_target, &bound_args, bound_this);
        }

        let info = callee.as_function();

        match info.kind {
            FunctionObjectKind::Builtin(func) => {
                let old_constructor_call = self.builtin_constructor_call;
                self.builtin_constructor_call = false;
                let result = func(self, args, this);
                self.builtin_constructor_call = old_constructor_call;
                result
            }
            FunctionObjectKind::User { info, outer_env } => {
                if info.generator {
                    return self.create_generator_object(callee, info, outer_env, args, this);
                }
                if info.async_function {
                    return self.call_async_user_function(callee, info, outer_env, args, this);
                }
                self.call_user_function(
                    callee,
                    info,
                    outer_env,
                    args,
                    this,
                    Value::undefined(),
                    false,
                )
            }
        }
    }

    fn bound_function_parts(
        &mut self,
        callee: Value,
        args: &[Value],
    ) -> Option<(Value, Value, Vec<Value>)> {
        let bound_target = callee.get_property("__bound_target");
        if !bound_target.is_function_object() {
            return None;
        }

        let bound_this = callee.get_property("__bound_this");
        let bound_arg_count = callee
            .get_property("__bound_arg_count")
            .to_number(&mut self.factory.memory_allocator) as usize;
        let mut bound_args = Vec::with_capacity(bound_arg_count + args.len());
        for index in 0..bound_arg_count {
            bound_args.push(callee.get_property(&format!("__bound_arg_{}", index)));
        }
        bound_args.extend_from_slice(args);
        Some((bound_target, bound_this, bound_args))
    }

    pub fn is_callable(&self, callee: Value) -> bool {
        if let Some(target) = proxy_target(callee) {
            return self.is_callable(target);
        }
        callee.is_function_object()
    }

    pub fn is_constructor(&self, callee: Value) -> bool {
        if let Some(target) = proxy_target(callee) {
            return self.is_constructor(target);
        }
        if !callee.is_function_object() {
            return false;
        }
        let bound_target = callee.get_property("__bound_target");
        if bound_target.is_function_object() {
            return self.is_constructor(bound_target);
        }

        let info = callee.as_function();
        match info.kind {
            FunctionObjectKind::User { info, .. } => info.constructible,
            FunctionObjectKind::Builtin(_) => match info.name.as_ref().map(|name| name.as_str()) {
                Some("Array")
                | Some("ArrayBuffer")
                | Some("AggregateError")
                | Some("BigInt")
                | Some("Boolean")
                | Some("DataView")
                | Some("Date")
                | Some("Error")
                | Some("EvalError")
                | Some("FinalizationRegistry")
                | Some("Function")
                | Some("Collator")
                | Some("DateTimeFormat")
                | Some("DisplayNames")
                | Some("DurationFormat")
                | Some("ListFormat")
                | Some("Locale")
                | Some("Map")
                | Some("Number")
                | Some("NumberFormat")
                | Some("Object")
                | Some("PluralRules")
                | Some("Promise")
                | Some("Proxy")
                | Some("RangeError")
                | Some("RelativeTimeFormat")
                | Some("ReferenceError")
                | Some("RegExp")
                | Some("Segmenter")
                | Some("Set")
                | Some("ShadowRealm")
                | Some("SharedArrayBuffer")
                | Some("String")
                | Some("SyntaxError")
                | Some("Calendar")
                | Some("Duration")
                | Some("Instant")
                | Some("Iterator")
                | Some("PlainDate")
                | Some("PlainDateTime")
                | Some("PlainMonthDay")
                | Some("PlainTime")
                | Some("PlainYearMonth")
                | Some("TimeZone")
                | Some("ZonedDateTime")
                | Some("TypedArray")
                | Some("TypeError")
                | Some("URIError") => true,
                Some("WeakMap")
                | Some("WeakRef")
                | Some("WeakSet")
                | Some("Int8Array")
                | Some("Uint8Array")
                | Some("Uint8ClampedArray")
                | Some("Int16Array")
                | Some("Uint16Array")
                | Some("Int32Array")
                | Some("Uint32Array")
                | Some("Float32Array")
                | Some("Float64Array")
                | Some("BigInt64Array")
                | Some("BigUint64Array") => true,
                _ => false,
            },
        }
    }

    pub fn construct_function(&mut self, callee: Value, args: &[Value]) -> VMValueResult {
        self.construct_function_with_new_target(callee, args, callee)
    }

    pub fn construct_function_with_new_target(
        &mut self,
        callee: Value,
        args: &[Value],
        new_target: Value,
    ) -> VMValueResult {
        if let Some(proxy) = proxy_info(callee) {
            return self.proxy_construct(proxy, new_target, args);
        }
        if !self.is_constructor(callee) {
            return Err(self.current_context.error_type("Not a constructor"));
        }
        if let Some((bound_target, _, bound_args)) = self.bound_function_parts(callee, args) {
            return self.construct_function_with_new_target(bound_target, &bound_args, new_target);
        }

        let default_prototype = self.constructor_default_prototype(callee);
        let prototype = self.get_constructor_prototype(new_target, default_prototype)?;

        let this = Value::Object(self.factory.alloc(Object {
            kind: ObjectKind::Ordinary,
            prototype,
            property: FxHashMap::default(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }));

        let info = callee.as_function();
        match info.kind {
            FunctionObjectKind::Builtin(func) => {
                let old_constructor_call = self.builtin_constructor_call;
                let old_builtin_callee = self.builtin_callee;
                let old_builtin_new_target = self.builtin_new_target;
                self.builtin_constructor_call = true;
                self.builtin_callee = callee;
                self.builtin_new_target = new_target;
                let val = func(self, args, this);
                self.builtin_constructor_call = old_constructor_call;
                self.builtin_callee = old_builtin_callee;
                self.builtin_new_target = old_builtin_new_target;
                let val = val?;
                if val.is_object() {
                    let prototype =
                        self.get_constructor_prototype(new_target, default_prototype)?;
                    val.get_object_info().prototype = prototype;
                    Ok(val)
                } else {
                    Ok(this)
                }
            }
            FunctionObjectKind::User { info, outer_env } => {
                self.call_user_function(callee, info, outer_env, args, this, new_target, true)
            }
        }
    }

    pub fn super_construct(&mut self, args: &[Value]) -> VMValueResult {
        if !self.current_context.constructor_call
            || self.current_context.func_ref.this_mode != ThisMode::Derived
        {
            return Err(self.current_context.error_reference("super"));
        }
        if self.current_context.this != Value::uninitialized() {
            return Err(self.current_context.error_reference("super"));
        }

        let callee = self.current_context.callee;
        let super_constructor = if callee.is_function_object() {
            callee
                .as_function()
                .super_constructor
                .unwrap_or_else(Value::undefined)
        } else {
            Value::undefined()
        };
        if !self.is_constructor(super_constructor) {
            return Err(self.current_context.error_type("Super constructor"));
        }

        let this = self.construct_function_with_new_target(
            super_constructor,
            args,
            self.current_context.new_target,
        )?;
        self.current_context.this = this;
        self.current_context
            .variable_environment
            .set_this_binding(this)?;
        Ok(this)
    }

    fn constructor_default_prototype(&mut self, constructor: Value) -> Value {
        let prototype = constructor.get_property("prototype");
        if prototype.is_object() && !prototype.is_symbol() && !prototype.is_bigint() {
            prototype
        } else {
            self.factory.object_prototypes.object
        }
    }

    fn get_constructor_prototype(
        &mut self,
        new_target: Value,
        default_prototype: Value,
    ) -> Result<Value, RuntimeError> {
        let key = self.factory.string("prototype");
        let prototype = self.get_property_by_value(new_target, key)?;
        if prototype.is_object() && !prototype.is_symbol() && !prototype.is_bigint() {
            Ok(prototype)
        } else {
            Ok(default_prototype)
        }
    }

    fn call_user_function(
        &mut self,
        callee: Value,
        user_func: FuncInfoRef,
        outer_env: Option<LexicalEnvironmentRef>,
        args: &[Value],
        this: Value,
        new_target: Value,
        constructor_call: bool,
    ) -> VMValueResult {
        self.prepare_context_for_function_invokation(
            callee,
            user_func,
            outer_env,
            args,
            this,
            new_target,
            CallMode::Native,
            constructor_call,
        )?;
        // if called from builtin func, do not GC.
        let save = self.is_called_from_native;
        self.is_called_from_native = true;
        let res = self.run();
        self.is_called_from_native = save;
        if res.is_err() && !self.saved_context.is_empty() {
            self.current_context = self.saved_context.pop().unwrap();
        }
        res
    }

    fn get_property_to_stack_top(&mut self, parent: Value, key: Value) -> VMResult {
        if proxy_info(parent).is_some() {
            let val = self.get_property_by_value(parent, key)?;
            self.current_context.stack.push(val.into());
            return Ok(());
        }
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

    fn get_private_property_to_stack_top(&mut self, parent: Value, key: Value) -> VMResult {
        let key = key.to_string();
        let Value::Object(info) = parent else {
            return Err(self
                .current_context
                .error_type(format!("Cannot read private member '{}'", key)));
        };
        let Some(prop) = ObjectRef(info).get_private_element(&key) else {
            return Err(self
                .current_context
                .error_type(format!("Cannot read private member '{}'", key)));
        };
        match prop {
            Property::Data(DataProperty { val, .. }) => {
                self.current_context.stack.push(val.into());
                Ok(())
            }
            Property::Accessor(AccessorProperty { get, .. }) => {
                if get.is_undefined() {
                    return Err(self
                        .current_context
                        .error_type(format!("Cannot read private member '{}'", key)));
                }
                self.enter_function(get, &[], parent, false)
            }
        }
    }

    fn get_value_to_stack_top(&mut self, name: &str, keep_ref: bool) -> VMResult {
        let mut env_ref = self.current_context.lexical_environment;
        loop {
            let env = unsafe { &*env_ref.0 };
            match env.record {
                EnvironmentRecord::Function { ref record, .. }
                | EnvironmentRecord::Module { ref record, .. }
                | EnvironmentRecord::Declarative(ref record) => match record.get(name) {
                    Some(binding) if binding == &Value::uninitialized() => {
                        return Err(self
                            .current_context
                            .error_reference(format!("'{}' is not defined", name)));
                    }
                    Some(binding) => {
                        if keep_ref {
                            self.current_context.pending_reference =
                                Some(PendingReference::Binding {
                                    name: name.to_string(),
                                    env: env_ref,
                                });
                        }
                        self.current_context.stack.push((*binding).into());
                        return Ok(());
                    }
                    None => {}
                },
                EnvironmentRecord::Global(obj) | EnvironmentRecord::Object(obj) => {
                    if obj.has_own_property(name) {
                        if keep_ref {
                            let key = self.factory.string(name.to_string());
                            self.current_context.pending_reference =
                                Some(PendingReference::Object { key, object: obj });
                        }
                        let key = self.factory.string(name.to_string());
                        match obj.get_property_by_value(&mut self.factory, key)? {
                            Property::Data(DataProperty { val, .. }) => {
                                if val == Value::uninitialized() {
                                    return Err(self
                                        .current_context
                                        .error_reference(format!("'{}' is not defined", name)));
                                }
                                self.current_context.stack.push(val.into());
                            }
                            Property::Accessor(AccessorProperty { get, .. }) => {
                                if get.is_undefined() {
                                    self.current_context.stack.push(Value::undefined().into());
                                } else {
                                    self.enter_function(get, &[], obj, false)?;
                                }
                            }
                        }
                        return Ok(());
                    }
                }
            }

            if let Some(outer) = env.outer {
                env_ref = outer;
            } else {
                return Err(self
                    .current_context
                    .error_reference(format!("'{}' is not defined", name)));
            }
        }
    }

    fn set_value_keep_ref(&mut self, name: &str, val: Value) -> VMResult {
        let pending = self.current_context.pending_reference.take();
        let expected_key = self.factory.string(name.to_string());
        match pending {
            Some(PendingReference::Binding {
                name: ref saved,
                mut env,
            }) if saved == name => env.set_value(name, val),
            Some(PendingReference::Object { key, object })
                if property_key_same_value(key, expected_key) =>
            {
                self.set_property_by_value(object, key, val)
            }
            _ => self.current_context.lex_env_mut().set_value(name, val),
        }
    }

    fn set_pending_reference(&mut self, val: Value) -> VMResult {
        let pending = self.current_context.pending_reference.take();
        match pending {
            Some(PendingReference::Binding { name, mut env }) => {
                if env.binding_is_uninitialized(&name) {
                    return Err(self
                        .current_context
                        .error_reference(format!("'{}' is not defined", name)));
                }
                env.set_value(&name, val)
            }
            Some(PendingReference::Object { key, object }) => {
                self.set_property_by_value(object, key, val)
            }
            Some(PendingReference::Unresolvable { name }) => {
                if self.current_context.func_ref.this_mode == ThisMode::Strict {
                    return Err(self
                        .current_context
                        .error_reference(format!("'{}' is not defined", name)));
                }
                self.global_environment
                    .get_global_object()
                    .set_property(name, val);
                Ok(())
            }
            None => Err(self
                .current_context
                .error_reference("Invalid left-hand side")),
        }
    }

    fn make_binding_reference(&mut self, name: &str) -> VMResult {
        let mut env_ref = self.current_context.lexical_environment;
        loop {
            let env = unsafe { &*env_ref.0 };
            match env.record {
                EnvironmentRecord::Function { ref record, .. }
                | EnvironmentRecord::Module { ref record, .. }
                | EnvironmentRecord::Declarative(ref record)
                    if record.contains_key(name) =>
                {
                    self.current_context.pending_reference = Some(PendingReference::Binding {
                        name: name.to_string(),
                        env: env_ref,
                    });
                    return Ok(());
                }
                EnvironmentRecord::Global(obj) | EnvironmentRecord::Object(obj)
                    if obj.has_own_property(name) =>
                {
                    let key = self.factory.string(name.to_string());
                    self.current_context.pending_reference =
                        Some(PendingReference::Object { key, object: obj });
                    return Ok(());
                }
                _ => {}
            }

            if let Some(outer) = env.outer {
                env_ref = outer;
            } else {
                self.current_context.pending_reference = Some(PendingReference::Unresolvable {
                    name: name.to_string(),
                });
                return Ok(());
            }
        }
    }

    pub fn get_property_by_value(
        &mut self,
        parent: Value,
        key: Value,
    ) -> Result<Value, RuntimeError> {
        if let Some(proxy) = proxy_info(parent) {
            return self.proxy_get(proxy, parent, key);
        }
        let val = parent.get_property_by_value(&mut self.factory, key)?;
        match val {
            Property::Data(DataProperty { val, .. }) => Ok(val),
            Property::Accessor(AccessorProperty { get, .. }) => {
                if get.is_undefined() {
                    return Ok(Value::undefined());
                }
                Ok(self.call_function(get, &[], parent)?)
            }
        }
    }

    pub fn set_property_by_value(&mut self, parent: Value, key: Value, val: Value) -> VMResult {
        if parent.is_symbol() || parent.is_bigint() {
            if self.current_context.func_ref.this_mode == ThisMode::Strict {
                return Err(self
                    .current_context
                    .error_type(format!("Cannot set property '{}'", key.to_string())));
            }
            return Ok(());
        }
        if let Some(proxy) = proxy_info(parent) {
            return self.proxy_set(proxy, parent, key, val);
        }
        let key_string = key.to_string();
        let (maybe_setter, success) =
            parent.set_property_by_value(&mut self.factory.memory_allocator, key, val)?;
        if let Some(setter) = maybe_setter {
            self.call_function(setter, &[val], parent)?;
        }
        if !success {
            if self.current_context.func_ref.this_mode == ThisMode::Strict {
                return Err(self
                    .current_context
                    .error_type(format!("Cannot set property '{}'", key_string)));
            }
            return Ok(());
        }
        let mapped = parent
            .get_object_properties()
            .and_then(|props| props.get(&key_string))
            .and_then(|prop| prop.get_data())
            .map(|data| data.writable)
            .unwrap_or(false);
        if mapped {
            self.set_mapped_argument(parent, &key_string, val)?;
        }
        Ok(())
    }

    pub fn set_property_by_value_or_throw(
        &mut self,
        parent: Value,
        key: Value,
        val: Value,
    ) -> VMResult {
        let key_string = key.to_string();
        if parent.is_symbol() || parent.is_bigint() {
            return Err(self
                .current_context
                .error_type(format!("Cannot set property '{}'", key_string)));
        }
        if let Some(proxy) = proxy_info(parent) {
            let Some(trap) = self.proxy_trap(proxy.handler, "set")? else {
                return self.set_property_by_value_or_throw(proxy.target, key, val);
            };
            let success = self
                .call_function(trap, &[proxy.target, key, val, parent], proxy.handler)?
                .to_boolean();
            if !success {
                return Err(self
                    .current_context
                    .error_type(format!("Cannot set property '{}'", key_string)));
            }
            return Ok(());
        }

        let (maybe_setter, success) =
            parent.set_property_by_value(&mut self.factory.memory_allocator, key, val)?;
        if let Some(setter) = maybe_setter {
            self.call_function(setter, &[val], parent)?;
        }
        if !success {
            return Err(self
                .current_context
                .error_type(format!("Cannot set property '{}'", key_string)));
        }
        let mapped = parent
            .get_object_properties()
            .and_then(|props| props.get(&key_string))
            .and_then(|prop| prop.get_data())
            .map(|data| data.writable)
            .unwrap_or(false);
        if mapped {
            self.set_mapped_argument(parent, &key_string, val)?;
        }
        Ok(())
    }

    fn proxy_get(
        &mut self,
        proxy: ProxyObjectInfo,
        receiver: Value,
        key: Value,
    ) -> Result<Value, RuntimeError> {
        let Some(trap) = self.proxy_trap(proxy.handler, "get")? else {
            return self.get_property_by_value(proxy.target, key);
        };
        self.call_function(trap, &[proxy.target, key, receiver], proxy.handler)
    }

    fn proxy_set(
        &mut self,
        proxy: ProxyObjectInfo,
        receiver: Value,
        key: Value,
        value: Value,
    ) -> VMResult {
        let key_string = key.to_string();
        let Some(trap) = self.proxy_trap(proxy.handler, "set")? else {
            return self.set_property_by_value(proxy.target, key, value);
        };
        let success = self
            .call_function(trap, &[proxy.target, key, value, receiver], proxy.handler)?
            .to_boolean();
        if !success && self.current_context.func_ref.this_mode == ThisMode::Strict {
            return Err(self
                .current_context
                .error_type(format!("Cannot set property '{}'", key_string)));
        }
        Ok(())
    }

    fn proxy_apply(
        &mut self,
        proxy: ProxyObjectInfo,
        this: Value,
        args: &[Value],
    ) -> VMValueResult {
        if !self.is_callable(proxy.target) {
            return Err(self.current_context.error_type("Not a function"));
        }
        let Some(trap) = self.proxy_trap(proxy.handler, "apply")? else {
            return self.call_function(proxy.target, args, this);
        };
        let arg_array = self.argument_array(args);
        self.call_function(trap, &[proxy.target, this, arg_array], proxy.handler)
    }

    fn proxy_construct(
        &mut self,
        proxy: ProxyObjectInfo,
        new_target: Value,
        args: &[Value],
    ) -> VMValueResult {
        if !self.is_constructor(proxy.target) {
            return Err(self.current_context.error_type("Not a constructor"));
        }
        let Some(trap) = self.proxy_trap(proxy.handler, "construct")? else {
            return self.construct_function_with_new_target(proxy.target, args, new_target);
        };
        let arg_array = self.argument_array(args);
        let result =
            self.call_function(trap, &[proxy.target, arg_array, new_target], proxy.handler)?;
        if !result.is_object() {
            return Err(self.current_context.error_type("Proxy construct result"));
        }
        Ok(result)
    }

    fn argument_array(&mut self, args: &[Value]) -> Value {
        let elems = args
            .iter()
            .copied()
            .map(Property::new_data_simple)
            .collect();
        self.factory.array(elems)
    }

    fn proxy_trap(&mut self, handler: Value, name: &str) -> Result<Option<Value>, RuntimeError> {
        if handler.is_null() {
            return Err(self.current_context.error_type("Proxy is revoked"));
        }
        let key = self.factory.string(name);
        let trap = self.get_property_by_value(handler, key)?;
        if trap.is_null() || trap.is_undefined() {
            return Ok(None);
        }
        if !trap.is_function_object() {
            return Err(self
                .current_context
                .error_type("Proxy trap is not callable"));
        }
        Ok(Some(trap))
    }

    pub fn set_private_property_by_value(
        &mut self,
        parent: Value,
        key: Value,
        val: Value,
    ) -> VMResult {
        let key = key.to_string();
        let Value::Object(info) = parent else {
            return Err(self
                .current_context
                .error_type(format!("Cannot write private member '{}'", key)));
        };
        let mut object = ObjectRef(info);
        let maybe_setter = object.update_private_element(&key, val).map_err(|_| {
            self.current_context
                .error_type(format!("Cannot write private member '{}'", key))
        })?;
        if let Some(setter) = maybe_setter {
            self.call_function(setter, &[val], parent)?;
        }
        Ok(())
    }

    pub fn define_private_property_by_value(
        &mut self,
        parent: Value,
        key: Value,
        val: Value,
    ) -> VMResult {
        let key = key.to_string();
        let Value::Object(info) = parent else {
            return Err(self
                .current_context
                .error_type(format!("Cannot define private member '{}'", key)));
        };
        ObjectRef(info)
            .define_private_element(key.clone(), Property::new_data_simple(val))
            .map_err(|_| {
                self.current_context
                    .error_type(format!("Cannot define private member '{}'", key))
            })
    }

    pub fn define_private_method_by_value(
        &mut self,
        parent: Value,
        key: Value,
        val: Value,
    ) -> VMResult {
        let key = key.to_string();
        let Value::Object(info) = parent else {
            return Err(self
                .current_context
                .error_type(format!("Cannot define private member '{}'", key)));
        };
        ObjectRef(info)
            .define_private_element(key.clone(), Property::new_data(DataProperty::new(val)))
            .map_err(|_| {
                self.current_context
                    .error_type(format!("Cannot define private member '{}'", key))
            })
    }

    pub fn define_private_accessor_by_value(
        &mut self,
        parent: Value,
        key: Value,
        val: Value,
        is_getter: bool,
    ) -> VMResult {
        let key = key.to_string();
        let Value::Object(info) = parent else {
            return Err(self
                .current_context
                .error_type(format!("Cannot define private member '{}'", key)));
        };
        ObjectRef(info)
            .define_private_accessor(key.clone(), val, is_getter)
            .map_err(|_| {
                self.current_context
                    .error_type(format!("Cannot define private member '{}'", key))
            })
    }

    fn set_function_name_if_anonymous(&mut self, value: Value, name: String) {
        if !value.is_function_object() {
            return;
        }
        let mut object = value.get_object_info();
        let should_set = object
            .property
            .get("name")
            .and_then(|prop| prop.get_data())
            .map(|data| data.val)
            .filter(|val| val.is_string())
            .map(|val| {
                let current = val.to_string();
                current.is_empty() || current == "__Class"
            })
            .unwrap_or(false);
        if should_set {
            let name = self.factory.string(name);
            object.insert_property(
                "name".to_string(),
                Property::new_data(DataProperty::new(name).set_configurable()),
            );
        }
    }

    pub fn delete_property_by_value(
        &mut self,
        parent: Value,
        key: Value,
        strict: bool,
    ) -> VMResult {
        let key_string = key.to_string();
        let deleted = parent.delete_property_by_value(&mut self.factory.memory_allocator, key)?;
        if deleted {
            self.delete_mapped_argument(parent, &key_string);
        } else if strict || self.current_context.func_ref.this_mode == ThisMode::Strict {
            return Err(self
                .current_context
                .error_type(format!("Cannot delete property '{}'", key_string)));
        }
        self.current_context.stack.push(Value::bool(deleted).into());
        Ok(())
    }

    pub fn set_mapped_argument(&mut self, object: Value, key: &str, val: Value) -> VMResult {
        if !object.is_object() {
            return Ok(());
        }
        let parameter_name = {
            let obj = object.get_object_info();
            match obj.kind {
                ObjectKind::Arguments(ref info) => info.parameter_map.get(key).cloned(),
                _ => None,
            }
        };
        if let Some(name) = parameter_name {
            self.current_context.lex_env_mut().set_value(&name, val)?;
        }
        Ok(())
    }

    pub fn delete_mapped_argument(&mut self, object: Value, key: &str) {
        if !object.is_object() {
            return;
        }
        let mut obj = object.get_object_info();
        if let ObjectKind::Arguments(ref mut info) = obj.kind {
            info.parameter_map.remove(key);
        }
    }
}

impl VM {
    pub fn show_error_message(&self, error: RuntimeError) {
        match &error.kind {
            ErrorKind::Unknown => runtime_error("UnknownError"),
            ErrorKind::Unimplemented => runtime_error("Unimplemented feature"),
            ErrorKind::Reference(msg) => runtime_error(format!("ReferenceError: {}", msg)),
            ErrorKind::Type(msg) => runtime_error(format!("TypeError: {}", msg)),
            ErrorKind::Range(msg) => runtime_error(format!("RangeError: {}", msg)),
            ErrorKind::Syntax(msg) => runtime_error(format!("SyntaxError: {}", msg)),
            ErrorKind::Uri(msg) => runtime_error(format!("URIError: {}", msg)),
            ErrorKind::General(msg) => runtime_error(format!("Error: {}", msg)),
            ErrorKind::Exception(ref val) => {
                let loc_in_script = self
                    .to_source_map
                    .get(&error.func_id)
                    .unwrap()
                    .get_node_loc(error.inst_pc);
                let module_func_id = error.module_func_id;
                let info = &self.script_info[&module_func_id];
                if let Some(loc) = loc_in_script {
                    runtime_error(format!(
                        "{}:{}:{}: Uncaught Exception",
                        info.file_path().to_string_lossy(),
                        loc.line,
                        loc.column
                    ));
                    let msg = get_error_line(&info.code(), loc);
                    println!("{}", msg);
                } else {
                    runtime_error("Uncaught Exception");
                }
                debug_print(val, false);
                println!();
            }
        }
    }
}

macro_rules! read_int8 {
    ($vm:expr, $var:ident, $ty:ty) => {
        let $var = $vm.current_context.func_ref.code[$vm.current_context.pc] as $ty;
        $vm.current_context.pc += 1;
    };
}

macro_rules! read_int32 {
    ($vm:expr, $var:ident, $ty:ty) => {
        let $var = {
            let iseq = &$vm.current_context.func_ref.code;
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
                for exception in &vm.current_context.func_ref.exception_table {
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

                if vm.saved_context.len() == 0 || vm.is_called_from_native {
                    break;
                }
                if let Err(error) = vm.unwind_context() {
                    let val = error.to_value(&mut vm.factory);
                    vm.current_context.stack.push(val.into());
                } else {
                    vm.current_context.pc = vm.current_context.current_inst_pc;
                    vm.close_pending_iterators_for_throw_completion();
                }
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

        'vm_loop: loop {
            if self
                .current_context
                .run_until_pc
                .map(|limit| self.current_context.pc >= limit)
                .unwrap_or(false)
            {
                self.current_context.run_until_pc = None;
                return Ok(Value::undefined());
            }
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
                    continue 'vm_loop;
                }};
            }

            macro_rules! etry {
                ($val:expr) => {{
                    match $val {
                        Ok(ok) => ok,
                        Err(err) => {
                            let err = self
                                .close_pending_iterators_on_error(err)
                                .unwrap_or_else(|close_err| close_err);
                            let err = err.error_add_info(&self.current_context);
                            let val = err.to_value(&mut self.factory);
                            self.current_context.stack.push(val.into());
                            handle_exception(self, &mut subroutine_stack)?;
                            continue 'vm_loop;
                        }
                    }
                }};
            }

            let inst = self.current_context.func_ref.code[self.current_context.pc];
            self.profile.current_inst = inst;
            match inst {
                // TODO: Macro for bin ops?
                VMInst::ADD => {
                    self.current_context.pc += 1;
                    let rhs = self.current_context.stack.pop().unwrap();
                    let lhs = self.current_context.stack.pop().unwrap();
                    /*
                    let res: f64 =
                        unsafe { std::mem::transmute(rhs): f64 + std::mem::transmute(lhs): f64 };

                    if !res.is_nan() {
                        self.current_context
                            .stack
                            .push(unsafe { std::mem::transmute(res) });
                    } else {
                        */
                    let rhs_val: Value = rhs.into();
                    let lhs_val: Value = lhs.into();
                    let result = etry!(self.add(lhs_val, rhs_val));
                    self.current_context.stack.push(result.into());
                    //}
                    self.gc_mark();
                }
                VMInst::SUB => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.sub(&mut self.factory.memory_allocator, rhs).into());
                    self.gc_mark();
                }
                VMInst::MUL => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.mul(&mut self.factory.memory_allocator, rhs).into());
                    self.gc_mark();
                }
                VMInst::DIV => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.div(&mut self.factory.memory_allocator, rhs).into());
                    self.gc_mark();
                }
                VMInst::REM => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.rem(&mut self.factory.memory_allocator, rhs).into());
                    self.gc_mark();
                }
                VMInst::EXP => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.exp(&mut self.factory.memory_allocator, rhs).into());
                    self.gc_mark();
                }
                VMInst::EQ => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.eq(&mut self.factory.memory_allocator, rhs).into());
                    self.gc_mark();
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
                    self.gc_mark();
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
                    self.gc_mark();
                }
                VMInst::LE => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.le(&mut self.factory.memory_allocator, rhs).into());
                    self.gc_mark();
                }
                VMInst::GT => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(rhs.lt(&mut self.factory.memory_allocator, lhs).into());
                    self.gc_mark();
                }
                VMInst::GE => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(rhs.le(&mut self.factory.memory_allocator, lhs).into());
                    self.gc_mark();
                }
                VMInst::INSTANCEOF => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    let val = self.instanceof(lhs, rhs);
                    self.current_context.stack.push(val.into());
                    self.gc_mark();
                }
                VMInst::IN => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    let val = etry!(self.has_property(lhs, rhs));
                    self.current_context.stack.push(val.into());
                    self.gc_mark();
                }
                VMInst::AND => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(rhs.and(&mut self.factory.memory_allocator, lhs).into());
                    self.gc_mark();
                }
                VMInst::OR => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(rhs.or(&mut self.factory.memory_allocator, lhs).into());
                    self.gc_mark();
                }
                VMInst::XOR => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(rhs.xor(&mut self.factory.memory_allocator, lhs).into());
                    self.gc_mark();
                }
                VMInst::NOT => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(rhs.not(&mut self.factory.memory_allocator).into());
                    self.gc_mark();
                }
                VMInst::SHL => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.shift_l(&mut self.factory.memory_allocator, rhs).into());
                    self.gc_mark();
                }
                VMInst::SHR => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context
                        .stack
                        .push(lhs.shift_r(&mut self.factory.memory_allocator, rhs).into());
                    self.gc_mark();
                }
                VMInst::ZFSHR => {
                    self.current_context.pc += 1;
                    let rhs: Value = self.current_context.stack.pop().unwrap().into();
                    let lhs: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context.stack.push(
                        lhs.z_shift_r(&mut self.factory.memory_allocator, rhs)
                            .into(),
                    );
                    self.gc_mark();
                }
                VMInst::NEG => {
                    self.current_context.pc += 1;
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    let val = if val.is_bigint() {
                        val.minus(&mut self.factory.memory_allocator)
                    } else {
                        let number = -self.to_number(val)?;
                        Value::Number(if number.is_nan() { f64::NAN } else { number })
                    };
                    self.current_context.stack.push(val.into());
                    self.gc_mark();
                }
                VMInst::POSI => {
                    self.current_context.pc += 1;
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    let number = self.to_number(val)?;
                    self.current_context
                        .stack
                        .push(Value::Number(number).into());
                    self.gc_mark();
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
                VMInst::PUSH_SEPERATOR => {
                    self.current_context.pc += 1;
                    self.current_context.stack.push(Value::seperator().into());
                }
                VMInst::PUSH_THIS => {
                    self.current_context.pc += 1;
                    if self.current_context.this == Value::uninitialized() {
                        return Err(self.current_context.error_reference("this"));
                    }
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
                VMInst::SPREAD_ARRAY => {
                    self.current_context.pc += 1;
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    let values = etry!(self.collect_iterable_values(val));
                    for value in values.into_iter().rev() {
                        self.current_context.stack.push(value.into());
                    }
                }
                VMInst::GET_MEMBER => {
                    self.current_context.pc += 1;
                    let property: Value = self.current_context.stack.pop().unwrap().into();
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    etry!(self.get_property_to_stack_top(parent, property))
                }
                VMInst::GET_METHOD_KEEP_THIS => {
                    self.current_context.pc += 1;
                    let property: Value = self.current_context.stack.pop().unwrap().into();
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    let callee =
                        match etry!(parent.get_property_by_value(&mut self.factory, property)) {
                            Property::Data(DataProperty { val, .. }) => val,
                            Property::Accessor(AccessorProperty { get, .. }) => {
                                if get.is_undefined() {
                                    Value::undefined()
                                } else {
                                    etry!(self.call_function(get, &[], parent));
                                    self.current_context.stack.pop().unwrap().into()
                                }
                            }
                        };
                    self.current_context.stack.push(parent.into());
                    self.current_context.stack.push(callee.into());
                }
                VMInst::SET_MEMBER => {
                    self.current_context.pc += 1;
                    let property: Value = self.current_context.stack.pop().unwrap().into();
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    etry!(self.set_property_by_value(parent, property, val))
                }
                VMInst::GET_PRIVATE_MEMBER => {
                    self.current_context.pc += 1;
                    let property: Value = self.current_context.stack.pop().unwrap().into();
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    etry!(self.get_private_property_to_stack_top(parent, property))
                }
                VMInst::SET_PRIVATE_MEMBER => {
                    self.current_context.pc += 1;
                    let property: Value = self.current_context.stack.pop().unwrap().into();
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    etry!(self.set_private_property_by_value(parent, property, val))
                }
                VMInst::DEFINE_PRIVATE_MEMBER => {
                    self.current_context.pc += 1;
                    let property: Value = self.current_context.stack.pop().unwrap().into();
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    etry!(self.define_private_property_by_value(parent, property, val))
                }
                VMInst::DEFINE_PRIVATE_METHOD => {
                    self.current_context.pc += 1;
                    let property: Value = self.current_context.stack.pop().unwrap().into();
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    etry!(self.define_private_method_by_value(parent, property, val))
                }
                VMInst::DEFINE_PRIVATE_GETTER => {
                    self.current_context.pc += 1;
                    let property: Value = self.current_context.stack.pop().unwrap().into();
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    etry!(self.define_private_accessor_by_value(parent, property, val, true))
                }
                VMInst::DEFINE_PRIVATE_SETTER => {
                    self.current_context.pc += 1;
                    let property: Value = self.current_context.stack.pop().unwrap().into();
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    etry!(self.define_private_accessor_by_value(parent, property, val, false))
                }
                VMInst::DELETE_MEMBER => {
                    self.current_context.pc += 1;
                    let property: Value = self.current_context.stack.pop().unwrap().into();
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    etry!(self.delete_property_by_value(parent, property, false))
                }
                VMInst::DELETE_MEMBER_STRICT => {
                    self.current_context.pc += 1;
                    let property: Value = self.current_context.stack.pop().unwrap().into();
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    etry!(self.delete_property_by_value(parent, property, true))
                }
                VMInst::SET_VALUE => {
                    self.current_context.pc += 1;
                    read_int32!(self, name_id, usize);
                    let val = self.current_context.stack.pop().unwrap();
                    let name = self.constant_table.get(name_id).as_string().as_str();
                    etry!(self
                        .current_context
                        .lex_env_mut()
                        .set_value(name, val.into()));
                }
                VMInst::SET_VALUE_KEEP_REF => {
                    self.current_context.pc += 1;
                    read_int32!(self, name_id, usize);
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    let name = self
                        .constant_table
                        .get(name_id)
                        .as_string()
                        .as_str()
                        .to_string();
                    etry!(self.set_value_keep_ref(&name, val));
                }
                VMInst::MAKE_OBJECT_REFERENCE => {
                    self.current_context.pc += 1;
                    let key: Value = self.current_context.stack.pop().unwrap().into();
                    let object: Value = self.current_context.stack.pop().unwrap().into();
                    self.current_context.pending_reference =
                        Some(PendingReference::Object { key, object });
                }
                VMInst::MAKE_BINDING_REFERENCE => {
                    self.current_context.pc += 1;
                    read_int32!(self, name_id, usize);
                    let name = self
                        .constant_table
                        .get(name_id)
                        .as_string()
                        .as_str()
                        .to_string();
                    etry!(self.make_binding_reference(&name));
                }
                VMInst::SET_PENDING_REFERENCE => {
                    self.current_context.pc += 1;
                    let val: Value = self.current_context.stack.pop().unwrap().into();
                    etry!(self.set_pending_reference(val));
                }
                VMInst::GET_VALUE => {
                    self.current_context.pc += 1;
                    read_int32!(self, name_id, usize);
                    let name = self
                        .constant_table
                        .get(name_id)
                        .as_string()
                        .as_str()
                        .to_string();
                    etry!(self.get_value_to_stack_top(&name, false));
                }
                VMInst::GET_VALUE_KEEP_REF => {
                    self.current_context.pc += 1;
                    read_int32!(self, name_id, usize);
                    let name = self
                        .constant_table
                        .get(name_id)
                        .as_string()
                        .as_str()
                        .to_string();
                    etry!(self.get_value_to_stack_top(&name, true));
                }
                VMInst::CONSTRUCT => {
                    self.current_context.pc += 1;
                    read_int32!(self, argc, usize);
                    let Some(callee) = self.current_context.stack.pop() else {
                        etry!(Err(self.current_context.error_type("Not a constructor")));
                        continue;
                    };
                    let callee: Value = callee.into();
                    let mut args: Vec<Value> = vec![];
                    for _ in 0..argc {
                        let Some(arg) = self.current_context.stack.pop() else {
                            etry!(Err(self.current_context.error_type("Not a constructor")));
                            continue;
                        };
                        args.push(arg.into());
                    }
                    etry!(self.enter_constructor(callee, &args));
                }
                VMInst::CALL => {
                    self.current_context.pc += 1;
                    read_int32!(self, argc, usize);
                    let Some(callee) = self.current_context.stack.pop() else {
                        etry!(Err(self.current_context.error_type("Not a function")));
                        continue;
                    };
                    let callee: Value = callee.into();
                    let mut args: Vec<Value> = vec![];
                    for _ in 0..argc {
                        let Some(arg) = self.current_context.stack.pop() else {
                            etry!(Err(self.current_context.error_type("Not a function")));
                            continue;
                        };
                        args.push(arg.into());
                    }
                    etry!(self.enter_function(callee, &args, self.current_context.this, false))
                }
                VMInst::CALL_SPREAD => {
                    self.current_context.pc += 1;
                    let callee: Value = match self.current_context.stack.pop() {
                        Some(callee) => callee.into(),
                        None => type_error!("Not a function"),
                    };
                    let mut args: Vec<Value> = vec![];
                    loop {
                        let arg: Value = match self.current_context.stack.pop() {
                            Some(arg) => arg.into(),
                            None => type_error!("Not a function"),
                        };
                        if arg.is_seperator() {
                            break;
                        }
                        args.push(arg);
                    }
                    etry!(self.enter_function(callee, &args, self.current_context.this, false))
                }
                VMInst::CALL_VALUE => {
                    self.current_context.pc += 1;
                    read_int32!(self, argc, usize);
                    let mut args: Vec<Value> = vec![];
                    for _ in 0..argc {
                        let Some(arg) = self.current_context.stack.pop() else {
                            etry!(Err(self.current_context.error_type("Not a function")));
                            continue;
                        };
                        args.push(arg.into());
                    }
                    let Some(callee) = self.current_context.stack.pop() else {
                        etry!(Err(self.current_context.error_type("Not a function")));
                        continue;
                    };
                    etry!(self.enter_function(
                        callee.into(),
                        &args,
                        self.current_context.this,
                        false,
                    ))
                }
                VMInst::CALL_VALUE_SPREAD => {
                    self.current_context.pc += 1;
                    let mut args: Vec<Value> = vec![];
                    loop {
                        let arg: Value = match self.current_context.stack.pop() {
                            Some(arg) => arg.into(),
                            None => type_error!("Not a function"),
                        };
                        if arg.is_seperator() {
                            break;
                        }
                        args.push(arg);
                    }
                    let callee: Value = match self.current_context.stack.pop() {
                        Some(callee) => callee.into(),
                        None => type_error!("Not a function"),
                    };
                    etry!(self.enter_function(callee, &args, self.current_context.this, false))
                }
                VMInst::CALL_VALUE_WITH_THIS => {
                    self.current_context.pc += 1;
                    read_int32!(self, argc, usize);
                    let mut args: Vec<Value> = vec![];
                    for _ in 0..argc {
                        let Some(arg) = self.current_context.stack.pop() else {
                            etry!(Err(self.current_context.error_type("Not a function")));
                            continue;
                        };
                        args.push(arg.into());
                    }
                    let Some(callee) = self.current_context.stack.pop() else {
                        etry!(Err(self.current_context.error_type("Not a function")));
                        continue;
                    };
                    let Some(this) = self.current_context.stack.pop() else {
                        etry!(Err(self.current_context.error_type("Not a function")));
                        continue;
                    };
                    etry!(self.enter_function(callee.into(), &args, this.into(), false))
                }
                VMInst::CALL_VALUE_WITH_THIS_SPREAD => {
                    self.current_context.pc += 1;
                    let mut args: Vec<Value> = vec![];
                    loop {
                        let arg: Value = match self.current_context.stack.pop() {
                            Some(arg) => arg.into(),
                            None => type_error!("Not a function"),
                        };
                        if arg.is_seperator() {
                            break;
                        }
                        args.push(arg);
                    }
                    let callee: Value = match self.current_context.stack.pop() {
                        Some(callee) => callee.into(),
                        None => type_error!("Not a function"),
                    };
                    let this: Value = match self.current_context.stack.pop() {
                        Some(this) => this.into(),
                        None => type_error!("Not a function"),
                    };
                    etry!(self.enter_function(callee, &args, this, false))
                }
                VMInst::CALL_DIRECT_EVAL => {
                    self.current_context.pc += 1;
                    read_int32!(self, argc, usize);
                    let callee: Value = self.current_context.stack.pop().unwrap().into();
                    let mut args: Vec<Value> = vec![];
                    for _ in 0..argc {
                        args.push(self.current_context.stack.pop().unwrap().into());
                    }
                    let old_direct_eval_call = self.direct_eval_call;
                    self.direct_eval_call = true;
                    let result =
                        self.enter_function(callee, &args, self.current_context.this, false);
                    self.direct_eval_call = old_direct_eval_call;
                    etry!(result)
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
                        Property::Accessor(AccessorProperty { get, .. }) => {
                            if get.is_undefined() {
                                type_error!("Not a function")
                            }
                            etry!(self.call_function(get, &[], parent))
                        }
                    };
                    etry!(self.enter_function(callee, &args, parent, false))
                }
                VMInst::CALL_SUPER_METHOD => {
                    self.current_context.pc += 1;
                    read_int32!(self, argc, usize);
                    let method: Value = self.current_context.stack.pop().unwrap().into();
                    let mut args: Vec<Value> = vec![];
                    for _ in 0..argc {
                        args.push(self.current_context.stack.pop().unwrap().into());
                    }
                    let this = self.current_context.this;
                    if !this.is_object() {
                        return Err(self.current_context.error_reference("super"));
                    }
                    let proto = this.get_object_info().prototype;
                    if !proto.is_object() {
                        return Err(self.current_context.error_reference("super"));
                    }
                    let super_base = proto.get_object_info().prototype;
                    if super_base.is_null() || super_base.is_undefined() {
                        return Err(self.current_context.error_reference("super"));
                    }
                    let callee =
                        match etry!(super_base.get_property_by_value(&mut self.factory, method)) {
                            Property::Data(DataProperty { val, .. }) => val,
                            Property::Accessor(AccessorProperty { get, .. }) => {
                                if get.is_undefined() {
                                    type_error!("Not a function")
                                }
                                etry!(self.call_function(get, &[], this))
                            }
                        };
                    etry!(self.enter_function(callee, &args, this, false))
                }
                VMInst::CALL_SUPER_METHOD_SPREAD => {
                    self.current_context.pc += 1;
                    let method: Value = match self.current_context.stack.pop() {
                        Some(method) => method.into(),
                        None => type_error!("Not a function"),
                    };
                    let mut args: Vec<Value> = vec![];
                    loop {
                        let arg: Value = match self.current_context.stack.pop() {
                            Some(arg) => arg.into(),
                            None => type_error!("Not a function"),
                        };
                        if arg.is_seperator() {
                            break;
                        }
                        args.push(arg);
                    }
                    let this = self.current_context.this;
                    if !this.is_object() {
                        return Err(self.current_context.error_reference("super"));
                    }
                    let proto = this.get_object_info().prototype;
                    if !proto.is_object() {
                        return Err(self.current_context.error_reference("super"));
                    }
                    let super_base = proto.get_object_info().prototype;
                    if super_base.is_null() || super_base.is_undefined() {
                        return Err(self.current_context.error_reference("super"));
                    }
                    let callee =
                        match etry!(super_base.get_property_by_value(&mut self.factory, method)) {
                            Property::Data(DataProperty { val, .. }) => val,
                            Property::Accessor(AccessorProperty { get, .. }) => {
                                if get.is_undefined() {
                                    type_error!("Not a function")
                                }
                                etry!(self.call_function(get, &[], this))
                            }
                        };
                    etry!(self.enter_function(callee, &args, this, false))
                }
                VMInst::CALL_METHOD_SPREAD => {
                    self.current_context.pc += 1;
                    let parent: Value = match self.current_context.stack.pop() {
                        Some(parent) => parent.into(),
                        None => type_error!("Not a function"),
                    };
                    let method: Value = match self.current_context.stack.pop() {
                        Some(method) => method.into(),
                        None => type_error!("Not a function"),
                    };
                    let mut args: Vec<Value> = vec![];
                    loop {
                        let arg: Value = match self.current_context.stack.pop() {
                            Some(arg) => arg.into(),
                            None => type_error!("Not a function"),
                        };
                        if arg.is_seperator() {
                            break;
                        }
                        args.push(arg);
                    }
                    let callee = match etry!(parent.get_property_by_value(&mut self.factory, method))
                    {
                        Property::Data(DataProperty { val, .. }) => val,
                        Property::Accessor(AccessorProperty { get, .. }) => {
                            if get.is_undefined() {
                                type_error!("Not a function")
                            }
                            etry!(self.call_function(get, &[], parent))
                        }
                    };
                    etry!(self.enter_function(callee, &args, parent, false))
                }
                VMInst::CALL_PRIVATE_METHOD => {
                    self.current_context.pc += 1;
                    read_int32!(self, argc, usize);
                    let parent: Value = self.current_context.stack.pop().unwrap().into();
                    let method: Value = self.current_context.stack.pop().unwrap().into();
                    let mut args: Vec<Value> = vec![];
                    for _ in 0..argc {
                        args.push(self.current_context.stack.pop().unwrap().into());
                    }
                    let key = method.to_string();
                    let Value::Object(info) = parent else {
                        type_error!("Cannot call private method")
                    };
                    let callee = match ObjectRef(info).get_private_element(&key) {
                        Some(Property::Data(DataProperty { val, .. })) => val,
                        _ => type_error!("Cannot call private method"),
                    };
                    etry!(self.enter_function(callee, &args, parent, false))
                }
                VMInst::CALL_PRIVATE_METHOD_SPREAD => {
                    self.current_context.pc += 1;
                    let parent: Value = match self.current_context.stack.pop() {
                        Some(parent) => parent.into(),
                        None => type_error!("Cannot call private method"),
                    };
                    let method: Value = match self.current_context.stack.pop() {
                        Some(method) => method.into(),
                        None => type_error!("Cannot call private method"),
                    };
                    let mut args: Vec<Value> = vec![];
                    loop {
                        let arg: Value = match self.current_context.stack.pop() {
                            Some(arg) => arg.into(),
                            None => type_error!("Cannot call private method"),
                        };
                        if arg.is_seperator() {
                            break;
                        }
                        args.push(arg);
                    }
                    let key = method.to_string();
                    let Value::Object(info) = parent else {
                        type_error!("Cannot call private method")
                    };
                    let callee = match ObjectRef(info).get_private_element(&key) {
                        Some(Property::Data(DataProperty { val, .. })) => val,
                        _ => type_error!("Cannot call private method"),
                    };
                    etry!(self.enter_function(callee, &args, parent, false))
                }
                VMInst::SET_OUTER_ENV => {
                    self.current_context.pc += 1;
                    let func_template: Value = self.current_context.stack.pop().unwrap().into();
                    let mut func = func_template.copy_object(&mut self.factory.memory_allocator);
                    func.set_function_outer_environment(self.current_context.lexical_environment);
                    self.current_context.stack.push(func.into());
                    self.gc_mark();
                }
                VMInst::SET_FUNCTION_NAME => {
                    self.current_context.pc += 1;
                    read_int32!(self, name_id, i32);
                    let name = self
                        .constant_table
                        .get(name_id as usize)
                        .as_string()
                        .clone();
                    let value: Value = self.current_context.stack.last().unwrap().clone().into();
                    self.set_function_name_if_anonymous(value, name);
                }
                VMInst::CREATE_OBJECT => {
                    self.current_context.pc += 1;
                    read_int32!(self, id, usize);
                    self.create_object(id)?;
                    self.gc_mark();
                }
                VMInst::CREATE_ARRAY => {
                    self.current_context.pc += 1;
                    self.create_array()?;
                    self.gc_mark();
                }
                VMInst::FOR_IN_ENUMERATE => {
                    self.current_context.pc += 1;
                    self.for_in_enumerate()?;
                    self.gc_mark();
                }
                VMInst::FOR_OF_ENUMERATE => {
                    self.current_context.pc += 1;
                    etry!(self.for_of_enumerate());
                    self.gc_mark();
                }
                VMInst::FOR_IN_NEXT => {
                    self.current_context.pc += 1;
                    read_int32!(self, name_id, i32);
                    read_int32!(self, dst, i32);
                    let name = self
                        .constant_table
                        .get(name_id as usize)
                        .as_string()
                        .clone();
                    if etry!(self.for_in_next(&name)) {
                        self.current_context.pc =
                            (self.current_context.pc as isize + dst as isize) as usize;
                    }
                }
                VMInst::FOR_OF_NEXT => {
                    self.current_context.pc += 1;
                    read_int32!(self, name_id, i32);
                    read_int32!(self, dst, i32);
                    let name = self
                        .constant_table
                        .get(name_id as usize)
                        .as_string()
                        .clone();
                    if etry!(self.for_of_next(&name)) {
                        self.current_context.pc =
                            (self.current_context.pc as isize + dst as isize) as usize;
                    }
                }
                VMInst::FOR_OF_NEXT_VALUE => {
                    self.current_context.pc += 1;
                    read_int32!(self, dst, i32);
                    if let Some(value) = etry!(self.iterator_next_value()) {
                        self.current_context.stack.push(value.into());
                    } else {
                        self.current_context.pc =
                            (self.current_context.pc as isize + dst as isize) as usize;
                    }
                }
                VMInst::ITERATOR_NEXT => {
                    self.current_context.pc += 1;
                    let value = etry!(self.iterator_next_value()).unwrap_or_else(Value::undefined);
                    self.current_context.stack.push(value.into());
                }
                VMInst::ITERATOR_REST_ARRAY => {
                    self.current_context.pc += 1;
                    let mut elems = vec![];
                    loop {
                        let next = match self.iterator_next_value() {
                            Ok(next) => next,
                            Err(err) => {
                                self.current_context.stack.pop();
                                let err = err.error_add_info(&self.current_context);
                                let val = err.to_value(&mut self.factory);
                                self.current_context.stack.push(val.into());
                                handle_exception(self, &mut subroutine_stack)?;
                                continue 'vm_loop;
                            }
                        };
                        match next {
                            Some(value) => elems.push(Property::new_data_simple(value)),
                            None => break,
                        }
                    }
                    let array = self.factory.array(elems);
                    self.current_context.stack.push(array.into());
                    self.gc_mark();
                }
                VMInst::ITERATOR_CLOSE => {
                    self.current_context.pc += 1;
                    etry!(self.iterator_close());
                    self.gc_mark();
                }
                VMInst::PUSH_PENDING_ITERATOR_CLOSE => {
                    self.current_context.pc += 1;
                    let state: Value = (*self.current_context.stack.last().ok_or_else(|| {
                        self.current_context
                            .error_general("for-of iterator missing")
                    })?)
                    .into();
                    self.current_context
                        .pending_iterator_close_stack
                        .push(state);
                    self.gc_mark();
                }
                VMInst::POP_PENDING_ITERATOR_CLOSE => {
                    self.current_context.pc += 1;
                    self.current_context.pending_iterator_close_stack.pop();
                }
                VMInst::REQUIRE_OBJECT_COERCIBLE => {
                    self.current_context.pc += 1;
                    let value: Value = (*self.current_context.stack.last().ok_or_else(|| {
                        self.current_context
                            .error_general("object coercible value missing")
                    })?)
                    .into();
                    if value.is_null() || value.is_undefined() {
                        type_error!("Cannot destructure null or undefined")
                    }
                }
                VMInst::OBJECT_REST_EXCLUSION => {
                    self.current_context.pc += 1;
                    let key: Value = (*self.current_context.stack.last().ok_or_else(|| {
                        self.current_context
                            .error_general("object rest exclusion key missing")
                    })?)
                    .into();
                    self.current_context.object_rest_exclusion_stack.push(key);
                    self.gc_mark();
                }
                VMInst::OBJECT_REST => {
                    self.current_context.pc += 1;
                    read_int32!(self, excluded_count, usize);
                    etry!(self.object_rest(excluded_count));
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
                VMInst::PUSH_OBJECT_ENV => {
                    self.current_context.pc += 1;
                    let object = self
                        .current_context
                        .stack
                        .pop()
                        .map(Into::into)
                        .unwrap_or_else(Value::undefined);
                    self.push_object_env(object)?;
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
                VMInst::JMP_IF_TRUE => {
                    self.current_context.pc += 1;
                    read_int32!(self, dst, i32);
                    let cond_boxed = self.current_context.stack.pop().unwrap();
                    let cond: Value = cond_boxed.into();
                    if cond.to_boolean() {
                        self.current_context.pc =
                            (self.current_context.pc as isize + dst as isize) as usize;
                    }
                }
                VMInst::JMP_IF_NOT_NULLISH => {
                    self.current_context.pc += 1;
                    read_int32!(self, dst, i32);
                    let cond_boxed = self.current_context.stack.pop().unwrap();
                    let cond: Value = cond_boxed.into();
                    if !cond.is_null() && !cond.is_undefined() {
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
                            if let Err(error) = self.unwind_context() {
                                let val = error.to_value(&mut self.factory);
                                self.current_context.stack.push(val.into());
                                handle_exception(self, &mut subroutine_stack)?;
                            }
                        }
                    }
                }
                VMInst::THROW => {
                    self.current_context.pc += 1;
                    let pending =
                        std::mem::take(&mut self.current_context.pending_iterator_close_stack);
                    for state in pending.into_iter().rev() {
                        let _ = self.close_iterator_state(state);
                    }
                    handle_exception(self, &mut subroutine_stack)?;
                }
                VMInst::RETURN => {
                    self.current_context.pc += 1;
                    let call_mode = self.current_context.call_mode;
                    if self.saved_context.len() == 0 {
                        break;
                    };
                    if let Err(error) = self.unwind_context() {
                        let val = error.to_value(&mut self.factory);
                        self.current_context.stack.push(val.into());
                        handle_exception(self, &mut subroutine_stack)?;
                    }

                    if call_mode == CallMode::Native {
                        break;
                    }
                    // If call from built-in func, do not GC.
                    if !self.is_called_from_native {
                        // self.gc_mark()
                    };

                    if self.is_trace {
                        self.profile.trace_string = format!(
                            "{}\n<-- return\n  module_id:{:?} func_id:{:?}",
                            self.profile.trace_string,
                            self.current_context.func_ref.module_func_id,
                            self.current_context.func_ref.func_id
                        );
                    };
                }
                VMInst::YIELD => {
                    self.current_context.pc += 1;
                    self.generator_yielded = true;
                    let value: Value = self
                        .current_context
                        .stack
                        .pop()
                        .map(Into::into)
                        .unwrap_or_else(Value::undefined);
                    self.current_context.stack.push(Value::undefined().into());
                    return Ok(value);
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
                        &self.current_context.func_ref.code,
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
            let inst_profile = &mut self.profile.inst_profile[self.profile.current_inst as usize];
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
                    &self.current_context.func_ref.code,
                    self.current_context.current_inst_pc,
                    &self.constant_table,
                ),
                match self.current_context.stack.last() {
                    None => format!("<empty>"),
                    Some(val) => {
                        let val: Value = (*val).into();
                        format!("{:10}", val)
                    }
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
        println!(
            "total execution time: {:>10} secs",
            total_time / 1000.0 / 1000.0
        );
        println!(
            "gc time:              {:>10} secs",
            total_gc_time / 1000.0 / 1000.0
        );

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
            "total allocated:  {:>12} bytes",
            self.factory.memory_allocator.allocated_size
                + self.factory.memory_allocator.collected_size
        );
        println!(
            "total collected:  {:>12} bytes",
            self.factory.memory_allocator.collected_size
        );
        println!(
            "finally allocated:{:>12} bytes",
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
    pub fn unwind_context(&mut self) -> VMResult {
        let prev_context = self.saved_context.pop().unwrap();
        let return_value = if self.current_context.call_mode == CallMode::Module {
            self.current_context
                .lex_env()
                .get_value("module")
                .unwrap()
                .get_object_info()
                .get_property("exports")
        } else {
            let ret_val: Value = self
                .current_context
                .stack
                .pop()
                .map(Into::into)
                .unwrap_or_else(Value::undefined);
            if self.current_context.constructor_call
                && self.current_context.func_ref.this_mode == ThisMode::Derived
            {
                if ret_val.is_object() {
                    ret_val
                } else if ret_val.is_undefined() {
                    if self.current_context.this == Value::uninitialized() {
                        let error = self.current_context.error_reference("this");
                        self.current_context = prev_context;
                        return Err(error);
                    }
                    self.current_context.this
                } else {
                    let error = self
                        .current_context
                        .error_type("Derived constructor return");
                    self.current_context = prev_context;
                    return Err(error);
                }
            } else if self.current_context.constructor_call && !ret_val.is_object() {
                self.current_context.this
            } else {
                ret_val
            }
        };
        self.current_context = prev_context;
        self.current_context.stack.push(return_value.into());
        Ok(())
    }

    fn push_env(&mut self, id: usize) -> VMResult {
        let lex_names = self.constant_table.get(id).as_lex_env_info();
        let outer = self.current_context.lexical_environment;

        let lex_env = self
            .factory
            .create_lexical_environment(lex_names, &Vec::new(), outer);

        self.current_context
            .saved_lexical_environment
            .push(self.current_context.lexical_environment);
        self.current_context.lexical_environment = lex_env;

        Ok(())
    }

    fn push_object_env(&mut self, object: Value) -> VMResult {
        if object.is_null() || object.is_undefined() {
            return Err(self
                .current_context
                .error_type("Cannot convert undefined or null to object"));
        }
        let object = if object.is_object() {
            object
        } else {
            self.factory.object(FxHashMap::default())
        };
        let outer = self.current_context.lexical_environment;
        let lex_env = LexicalEnvironmentRef(
            self.factory
                .alloc(LexicalEnvironment::new_object(object, Some(outer))),
        );
        self.current_context
            .saved_lexical_environment
            .push(self.current_context.lexical_environment);
        self.current_context.lexical_environment = lex_env;
        Ok(())
    }

    fn create_object(&mut self, id: usize) -> VMResult {
        let special_properties = self.constant_table.get(id).as_object_literal_info();
        let mut properties = FxHashMap::default();
        let mut property_order = Vec::new();
        let mut sym_properties = FxHashMap::default();
        let mut sym_property_order = Vec::new();

        let mut i = 0;
        loop {
            let prop: Value = self.current_context.stack.pop().unwrap().into();
            if prop.is_seperator() {
                break;
            }
            let val: Value = self.current_context.stack.pop().unwrap().into();
            use constant::SpecialPropertyKind::*;
            if let Some(kind) = special_properties.get(&i) {
                if *kind == Spread {
                    self.copy_data_properties_to_maps(
                        val,
                        &[],
                        &mut properties,
                        &mut property_order,
                        &mut sym_properties,
                        &mut sym_property_order,
                    )?;
                } else {
                    let property = if prop.is_symbol() {
                        let id = prop.get_symbol_info().id;
                        if !sym_properties.contains_key(&id) {
                            sym_property_order.push(prop);
                        }
                        sym_properties
                            .entry(id)
                            .or_insert(Property::Accessor(AccessorProperty {
                                get: Value::undefined(),
                                set: Value::undefined(),
                                // TODO
                                enumerable: true,
                                configurable: true,
                            }))
                    } else {
                        let name = prop.to_string();
                        if !properties.contains_key(&name) {
                            property_order.push(name.clone());
                        }
                        properties
                            .entry(name)
                            .or_insert(Property::Accessor(AccessorProperty {
                                get: Value::undefined(),
                                set: Value::undefined(),
                                // TODO
                                enumerable: true,
                                configurable: true,
                            }))
                    };
                    let AccessorProperty { get, set, .. } = property.as_accessor_mut();
                    match kind {
                        Getter => *get = val,
                        Setter => *set = val,
                        Spread => {}
                    }
                }
            } else if prop.is_symbol() {
                let id = prop.get_symbol_info().id;
                if !sym_properties.contains_key(&id) {
                    sym_property_order.push(prop);
                }
                sym_properties.insert(
                    id,
                    Property::Data(DataProperty {
                        val,
                        // TODO
                        writable: true,
                        enumerable: true,
                        configurable: true,
                    }),
                );
            } else {
                let name = prop.to_string();
                if !properties.contains_key(&name) {
                    property_order.push(name.clone());
                }
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
            i += 1;
        }

        let obj = self
            .factory
            .object_with_property_order(properties, property_order);
        {
            let mut info = obj.get_object_info();
            info.sym_property = sym_properties;
            info.sym_property_order = sym_property_order;
        }
        self.current_context.stack.push(obj.into());

        Ok(())
    }

    fn create_array(&mut self) -> VMResult {
        let mut elems = vec![];
        loop {
            let val: Value = self.current_context.stack.pop().unwrap().into();
            if val.is_seperator() {
                break;
            }
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

    fn object_rest(&mut self, excluded_count: usize) -> VMResult {
        let mut excluded = vec![];
        for _ in 0..excluded_count {
            let key: Value = self
                .current_context
                .object_rest_exclusion_stack
                .pop()
                .ok_or_else(|| {
                    self.current_context
                        .error_general("object rest key missing")
                })?
                .into();
            excluded.push(key);
        }
        let source: Value = self
            .current_context
            .stack
            .pop()
            .ok_or_else(|| {
                self.current_context
                    .error_general("object rest source missing")
            })?
            .into();
        if source.is_null() || source.is_undefined() {
            return Err(self
                .current_context
                .error_type("Cannot destructure null or undefined"));
        }

        let mut properties = FxHashMap::default();
        let mut property_order = Vec::new();
        let mut sym_properties = FxHashMap::default();
        let mut sym_property_order = Vec::new();

        self.copy_data_properties_to_maps(
            source,
            &excluded,
            &mut properties,
            &mut property_order,
            &mut sym_properties,
            &mut sym_property_order,
        )?;

        let obj = self
            .factory
            .object_with_property_order(properties, property_order);
        {
            let mut info = obj.get_object_info();
            info.sym_property = sym_properties;
            info.sym_property_order = sym_property_order;
        }
        self.current_context.stack.push(obj.into());

        Ok(())
    }

    fn copy_data_properties_to_maps(
        &mut self,
        mut source: Value,
        excluded: &[Value],
        properties: &mut FxHashMap<String, Property>,
        property_order: &mut Vec<String>,
        sym_properties: &mut FxHashMap<usize, Property>,
        sym_property_order: &mut Vec<Value>,
    ) -> VMResult {
        if source.is_null() || source.is_undefined() {
            return Ok(());
        }
        if !source.is_object() {
            source = crate::builtins::helpers::to_object(self, source)?;
        }
        for key in self.own_property_keys(source)? {
            if excluded
                .iter()
                .any(|excluded| property_key_same_value(*excluded, key))
            {
                continue;
            }
            if !self.own_property_is_enumerable(source, key)? {
                continue;
            }
            let value = self.get_property_by_value(source, key)?;
            if key.is_symbol() {
                let id = key.get_symbol_info().id;
                if !sym_properties.contains_key(&id) {
                    sym_property_order.push(key);
                }
                sym_properties.insert(id, Property::new_data_simple(value));
            } else {
                let name = key.to_string();
                if !properties.contains_key(&name) {
                    property_order.push(name.clone());
                }
                properties.insert(name, Property::new_data_simple(value));
            }
        }
        Ok(())
    }

    fn own_property_keys(&mut self, source: Value) -> Result<Vec<Value>, RuntimeError> {
        if let Some(proxy) = proxy_info(source) {
            let Some(trap) = self.proxy_trap(proxy.handler, "ownKeys")? else {
                return self.own_property_keys(proxy.target);
            };
            let result = self.call_function(trap, &[proxy.target], proxy.handler)?;
            if !result.is_object() || !result.is_array_object() {
                return Err(self.current_context.error_type("Proxy ownKeys"));
            }
            let array = result.as_array_mut();
            let mut keys = Vec::new();
            for index in 0..array.get_length() {
                let key = array
                    .get_element(index)
                    .as_data()
                    .val
                    .to_undefined_if_empty();
                if !key.is_symbol() && !key.is_string() {
                    return Err(self.current_context.error_type("Proxy ownKeys"));
                }
                keys.push(key);
            }
            return Ok(keys);
        }

        if !source.is_object() {
            return Ok(Vec::new());
        }
        let info = source.get_object_info();
        let mut keys = info
            .own_string_property_keys()
            .into_iter()
            .map(|key| self.factory.string(key))
            .collect::<Vec<_>>();
        for symbol in &info.sym_property_order {
            if info.sym_property.contains_key(&symbol.get_symbol_info().id) {
                keys.push(*symbol);
            }
        }
        Ok(keys)
    }

    fn own_property_is_enumerable(
        &mut self,
        source: Value,
        key: Value,
    ) -> Result<bool, RuntimeError> {
        if let Some(proxy) = proxy_info(source) {
            let Some(trap) = self.proxy_trap(proxy.handler, "getOwnPropertyDescriptor")? else {
                return self.own_property_is_enumerable(proxy.target, key);
            };
            let desc = self.call_function(trap, &[proxy.target, key], proxy.handler)?;
            if desc.is_undefined() {
                return Ok(false);
            }
            if !desc.is_object() {
                return Err(self
                    .current_context
                    .error_type("Proxy getOwnPropertyDescriptor"));
            }
            let enumerable_key = self.factory.string("enumerable".to_string());
            return Ok(self
                .get_property_by_value(desc, enumerable_key)?
                .to_boolean());
        }

        Ok(self
            .own_property(source, key)
            .map(|property| property_is_enumerable(&property))
            .unwrap_or(false))
    }

    fn own_property(&mut self, source: Value, key: Value) -> Option<Property> {
        if !source.is_object() {
            return None;
        }
        let info = source.get_object_info();
        if key.is_symbol() {
            return info.sym_property.get(&key.get_symbol_info().id).copied();
        }

        let name = key.to_string();
        if let Some(index) = array_index_key(&name) {
            if source.is_array_object() {
                let array = source.as_array_mut();
                if (index as usize) < array.get_length() {
                    let property = array.get_element(index as usize);
                    if property
                        .get_data()
                        .map(|data| !data.val.is_empty())
                        .unwrap_or(true)
                    {
                        return Some(property);
                    }
                }
            }
        }
        if let Some(property) = info.property.get(&name).copied() {
            return Some(property);
        }
        if let Some(string) = info
            .property
            .get("__string_data")
            .and_then(|prop| prop.get_data())
            .map(|data| data.val)
            .filter(|value| value.is_string())
            .map(|value| value.to_string())
        {
            if let Some(index) = array_index_key(&name) {
                if let Some(ch) = string.chars().nth(index as usize) {
                    return Some(Property::new_data(DataProperty {
                        val: self.factory.string(ch.to_string()),
                        writable: false,
                        enumerable: true,
                        configurable: false,
                    }));
                }
            }
        }
        None
    }

    fn for_in_enumerate(&mut self) -> VMResult {
        let target: Value = self
            .current_context
            .stack
            .pop()
            .ok_or_else(|| self.current_context.error_general("for-in target missing"))?
            .into();
        let mut keys = vec![];

        if target.is_object() {
            let obj = target.get_object_info();
            for (name, prop) in &obj.property {
                let enumerable = match prop {
                    Property::Data(data) => data.enumerable,
                    Property::Accessor(accessor) => accessor.enumerable,
                };
                if enumerable {
                    keys.push(name.clone());
                }
            }

            if let ObjectKind::Array(ref info) = obj.kind {
                for (i, prop) in info.elems.iter().enumerate() {
                    let enumerable = match prop {
                        Property::Data(data) => data.enumerable && !data.val.is_empty(),
                        Property::Accessor(accessor) => accessor.enumerable,
                    };
                    if enumerable {
                        keys.push(i.to_string());
                    }
                }
            }

            if let Some(string) = obj
                .property
                .get("__string_data")
                .and_then(|prop| prop.get_data())
                .map(|data| data.val)
                .filter(|value| value.is_string())
                .map(|value| value.to_string())
            {
                for index in 0..string.chars().count() {
                    keys.push(index.to_string());
                }
            }
        }

        keys.sort_by(|a, b| match (array_index_key(a), array_index_key(b)) {
            (Some(a), Some(b)) => a.cmp(&b),
            (Some(_), None) => std::cmp::Ordering::Less,
            (None, Some(_)) => std::cmp::Ordering::Greater,
            (None, None) => a.cmp(b),
        });
        keys.dedup();

        let elems = keys
            .into_iter()
            .map(|key| {
                Property::Data(DataProperty {
                    val: self.factory.string(key),
                    writable: true,
                    enumerable: true,
                    configurable: true,
                })
            })
            .collect();
        let iterator = self.factory.array(elems);
        iterator.set_property("__rapidus_for_in_index", Value::Number(0.0));
        self.current_context.stack.push(iterator.into());

        Ok(())
    }

    fn for_of_enumerate(&mut self) -> VMResult {
        let target: Value = self
            .current_context
            .stack
            .pop()
            .ok_or_else(|| self.current_context.error_general("for-of target missing"))?
            .into();
        if let Some(iterator) = self.iterator_for(target)? {
            let state = self.factory.object(FxHashMap::default());
            state.set_property("__rapidus_for_of_iterator", iterator);
            self.current_context.stack.push(state.into());
            return Ok(());
        }

        let elems = if target.is_string() {
            target
                .to_string()
                .chars()
                .map(|ch| Property::new_data_simple(self.factory.string(ch.to_string())))
                .collect()
        } else if target.is_object() {
            let string_data = target
                .get_object_info()
                .property
                .get("__string_data")
                .and_then(|prop| prop.get_data())
                .map(|data| data.val)
                .filter(|value| value.is_string())
                .map(|value| value.to_string());
            if let Some(string) = string_data {
                string
                    .chars()
                    .map(|ch| Property::new_data_simple(self.factory.string(ch.to_string())))
                    .collect()
            } else {
                return Err(self.current_context.error_type("Value is not iterable"));
            }
        } else {
            return Err(self.current_context.error_type("Value is not iterable"));
        };

        let iterator = self.factory.array(elems);
        iterator.set_property("__rapidus_for_in_index", Value::Number(0.0));
        self.current_context.stack.push(iterator.into());

        Ok(())
    }

    fn iterator_for(&mut self, target: Value) -> Result<Option<Value>, RuntimeError> {
        if target.is_null() || target.is_undefined() {
            return Err(self.current_context.error_type("Value is not iterable"));
        }
        let iterator_key = self.factory.well_known_symbol(SYMBOL_ITERATOR_ID);
        let method = self.get_property_by_value(target, iterator_key)?;
        if method.is_function_object() {
            let iterator = self.call_function(method, &[], target)?;
            if iterator.is_object() {
                return Ok(Some(iterator));
            }
            return Err(self
                .current_context
                .error_type("@@iterator must return an object"));
        }

        let next_key = self.factory.string("next".to_string());
        let next = self.get_property_by_value(target, next_key)?;
        if next.is_function_object() {
            return Ok(Some(target));
        }
        Ok(None)
    }

    fn for_of_next(&mut self, name: &str) -> Result<bool, RuntimeError> {
        if let Some(value) = self.iterator_next_value()? {
            self.current_context.lex_env_mut().set_value(name, value)?;
            Ok(false)
        } else {
            Ok(true)
        }
    }

    fn iterator_next_value(&mut self) -> Result<Option<Value>, RuntimeError> {
        let state: Value = (*self.current_context.stack.last().ok_or_else(|| {
            self.current_context
                .error_general("for-of iterator missing")
        })?)
        .into();
        self.iterator_next_value_from_state(state)
    }

    fn iterator_next_value_from_state(
        &mut self,
        state: Value,
    ) -> Result<Option<Value>, RuntimeError> {
        let iterator = state.get_property("__rapidus_for_of_iterator");
        if iterator.is_undefined() {
            return self.indexed_iterator_next_value(state);
        }
        if state.get_property("__rapidus_for_of_done").to_boolean() {
            return Ok(None);
        }

        let next_key = self.factory.string("next".to_string());
        let next = match self.get_property_by_value(iterator, next_key) {
            Ok(next) => next,
            Err(err) => {
                state.set_property("__rapidus_for_of_done", Value::bool(true));
                return Err(err);
            }
        };
        if !next.is_function_object() {
            return Err(self
                .current_context
                .error_type("Iterator next is not callable"));
        }
        let result = match self.call_function(next, &[], iterator) {
            Ok(result) => result,
            Err(err) => {
                state.set_property("__rapidus_for_of_done", Value::bool(true));
                return Err(err);
            }
        };
        let result = self.unwrap_fulfilled_promise(result);
        if !result.is_object() {
            state.set_property("__rapidus_for_of_done", Value::bool(true));
            return Err(self
                .current_context
                .error_type("Iterator result is not an object"));
        }
        let done_key = self.factory.string("done".to_string());
        if self.get_property_by_value(result, done_key)?.to_boolean() {
            state.set_property("__rapidus_for_of_done", Value::bool(true));
            return Ok(None);
        }
        let value_key = self.factory.string("value".to_string());
        match self.get_property_by_value(result, value_key) {
            Ok(value) => Ok(Some(value)),
            Err(err) => {
                state.set_property("__rapidus_for_of_done", Value::bool(true));
                Err(err)
            }
        }
    }

    fn collect_iterable_values(&mut self, target: Value) -> Result<Vec<Value>, RuntimeError> {
        if let Some(iterator) = self.iterator_for(target)? {
            let state = self.factory.object(FxHashMap::default());
            state.set_property("__rapidus_for_of_iterator", iterator);
            let mut values = vec![];
            while let Some(value) = self.iterator_next_value_from_state(state)? {
                values.push(value);
            }
            return Ok(values);
        }

        if target.is_string() {
            return Ok(target
                .to_string()
                .chars()
                .map(|ch| self.factory.string(ch.to_string()))
                .collect());
        }

        if target.is_object() {
            if let Some(string) = target
                .get_object_info()
                .property
                .get("__string_data")
                .and_then(|prop| prop.get_data())
                .map(|data| data.val)
                .filter(|value| value.is_string())
                .map(|value| value.to_string())
            {
                return Ok(string
                    .chars()
                    .map(|ch| self.factory.string(ch.to_string()))
                    .collect());
            }
        }

        Err(self.current_context.error_type("Value is not iterable"))
    }

    fn iterator_close(&mut self) -> Result<(), RuntimeError> {
        let state: Value = (*self.current_context.stack.last().ok_or_else(|| {
            self.current_context
                .error_general("for-of iterator missing")
        })?)
        .into();
        self.close_iterator_state(state)
    }

    pub fn close_iterator_state(&mut self, state: Value) -> Result<(), RuntimeError> {
        let iterator = state.get_property("__rapidus_for_of_iterator");
        if iterator.is_undefined() {
            return Ok(());
        }
        if state.get_property("__rapidus_for_of_done").to_boolean() {
            return Ok(());
        }
        state.set_property("__rapidus_for_of_done", Value::bool(true));
        let return_key = self.factory.string("return".to_string());
        let return_method = self.get_property_by_value(iterator, return_key)?;
        if return_method.is_undefined() || return_method.is_null() {
            return Ok(());
        }
        if !return_method.is_function_object() {
            return Err(self
                .current_context
                .error_type("Iterator return is not callable"));
        }
        let result = self.call_function(return_method, &[], iterator)?;
        if !result.is_object() {
            return Err(self
                .current_context
                .error_type("Iterator close result is not object"));
        }
        Ok(())
    }

    fn close_pending_iterators_on_error(
        &mut self,
        err: RuntimeError,
    ) -> Result<RuntimeError, RuntimeError> {
        let pending = std::mem::take(&mut self.current_context.pending_iterator_close_stack);
        let is_throw_completion = matches!(err.kind, ErrorKind::Exception(_));
        for state in pending.into_iter().rev() {
            let stack_len = self.current_context.stack.len();
            if let Err(close_err) = self.close_iterator_state(state) {
                if !is_throw_completion {
                    return Err(close_err);
                }
            }
            if is_throw_completion {
                self.current_context.stack.truncate(stack_len);
            }
        }
        Ok(err)
    }

    fn close_pending_iterators_for_throw_completion(&mut self) {
        let pending = std::mem::take(&mut self.current_context.pending_iterator_close_stack);
        let completion = self.current_context.stack.pop();
        for state in pending.into_iter().rev() {
            let _ = self.close_iterator_state(state);
        }
        if let Some(completion) = completion {
            self.current_context.stack.push(completion);
        }
    }

    fn unwrap_fulfilled_promise(&self, value: Value) -> Value {
        if value.is_object()
            && matches!(
                value.get_property("__promise_state"),
                Value::Number(state) if state == 1.0
            )
        {
            value.get_property("__promise_result")
        } else {
            value
        }
    }

    fn indexed_iterator_next_value(
        &mut self,
        iterator: Value,
    ) -> Result<Option<Value>, RuntimeError> {
        if !iterator.is_array_object() {
            return Err(self
                .current_context
                .error_type("Iterator state is not an object"));
        }
        let index = iterator
            .get_property("__rapidus_for_in_index")
            .to_number(&mut self.factory.memory_allocator) as usize;
        let len = iterator.as_array_mut().get_length();
        if index >= len {
            return Ok(None);
        }

        let value = iterator
            .as_array_mut()
            .get_element(index)
            .as_data()
            .val
            .to_undefined_if_empty();
        iterator.set_property("__rapidus_for_in_index", Value::Number((index + 1) as f64));

        Ok(Some(value))
    }

    fn for_in_next(&mut self, name: &str) -> Result<bool, RuntimeError> {
        let iterator: Value = (*self.current_context.stack.last().ok_or_else(|| {
            self.current_context
                .error_general("for-in iterator missing")
        })?)
        .into();
        let index = iterator
            .get_property("__rapidus_for_in_index")
            .to_number(&mut self.factory.memory_allocator) as usize;
        let len = iterator.as_array_mut().get_length();
        if index >= len {
            return Ok(true);
        }

        let key = iterator.as_array_mut().get_element(index).as_data().val;
        iterator.set_property("__rapidus_for_in_index", Value::Number((index + 1) as f64));
        self.current_context.lex_env_mut().set_value(name, key)?;

        Ok(false)
    }

    fn instanceof(&self, lhs: Value, rhs: Value) -> Value {
        if !lhs.is_object() || !rhs.is_object() {
            return Value::bool(false);
        }

        let prototype = rhs.get_property("prototype");
        if !prototype.is_object() {
            return Value::bool(false);
        }

        let mut current = lhs.get_prototype();
        while current.is_object() {
            if current.strict_eq_bool(prototype) {
                return Value::bool(true);
            }
            current = current.get_prototype();
        }

        Value::bool(false)
    }

    pub fn has_property(&self, key: Value, object: Value) -> Result<Value, RuntimeError> {
        if !object.is_object() {
            return Err(self.current_context.error_type("right-hand side of 'in'"));
        }

        if key.is_symbol() {
            let id = key.get_symbol_info().id;
            let mut current = object;
            while current.is_object() {
                let info = current.get_object_info();
                if info.sym_property.contains_key(&id) {
                    return Ok(Value::bool(true));
                }
                current = info.prototype;
            }
            return Ok(Value::bool(false));
        }

        let key = key.to_string();
        let mut current = object;
        while current.is_object() {
            let info = current.get_object_info();
            if info.property.contains_key(&key) {
                return Ok(Value::bool(true));
            }
            if let ObjectKind::Array(ref array) = info.kind {
                if key == "length" {
                    return Ok(Value::bool(true));
                }
                if let Ok(index) = key.parse::<usize>() {
                    if index < array.elems.len()
                        && array.elems[index]
                            .get_data()
                            .map(|data| !data.val.is_empty())
                            .unwrap_or(true)
                    {
                        return Ok(Value::bool(true));
                    }
                }
            }
            current = current.get_prototype();
        }

        Ok(Value::bool(false))
    }

    fn enter_constructor(&mut self, callee: Value, args: &[Value]) -> VMResult {
        if !self.is_constructor(callee) {
            return Err(self.current_context.error_type("Not a constructor"));
        }
        if let Some((bound_target, _, bound_args)) = self.bound_function_parts(callee, args) {
            return self.enter_constructor(bound_target, &bound_args);
        }

        let this = Value::Object(self.factory.alloc(Object {
            kind: ObjectKind::Ordinary,
            prototype: callee.get_property("prototype"),
            property: FxHashMap::default(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
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
        if let Some((bound_target, bound_this, bound_args)) =
            self.bound_function_parts(callee, args)
        {
            let this = if constructor_call { this } else { bound_this };
            return self.enter_function(bound_target, &bound_args, this, constructor_call);
        }

        let info = callee.as_function();
        let ret = match info.kind {
            FunctionObjectKind::Builtin(func) => {
                let old_constructor_call = self.builtin_constructor_call;
                self.builtin_constructor_call = constructor_call;
                let val = func(self, args, this);
                self.builtin_constructor_call = old_constructor_call;
                let val = val?;
                let return_value = if constructor_call && !val.is_object() {
                    this
                } else {
                    val
                };
                self.current_context.stack.push(return_value.into());
                Ok(())
            }
            FunctionObjectKind::User {
                ref info,
                outer_env,
            } => {
                if info.generator {
                    if constructor_call {
                        return Err(self.current_context.error_type("Not a constructor"));
                    }
                    let generator =
                        self.create_generator_object(callee, *info, outer_env, args, this)?;
                    self.current_context.stack.push(generator.into());
                    return Ok(());
                }
                if info.async_function {
                    if constructor_call {
                        return Err(self.current_context.error_type("Not a constructor"));
                    }
                    let promise =
                        self.call_async_user_function(callee, *info, outer_env, args, this)?;
                    self.current_context.stack.push(promise.into());
                    return Ok(());
                }
                if self.is_trace {
                    self.profile.trace_string = format!(
                        "{}\n--> call {}\n  module_id:{:?} func_id:{:?}",
                        self.profile.trace_string,
                        if constructor_call {
                            "constructor"
                        } else {
                            "function"
                        },
                        self.current_context.func_ref.module_func_id,
                        self.current_context.func_ref.func_id
                    );
                };
                let new_target = if constructor_call {
                    callee
                } else {
                    Value::undefined()
                };
                self.enter_user_function(
                    callee,
                    *info,
                    outer_env,
                    args,
                    this,
                    new_target,
                    constructor_call,
                )
            }
        };
        ret
    }

    /// Prepare a new context before invoking function.
    /// 1. Push current context to the context stack.
    /// 2. Set `this`.
    /// 3. Create a new function environment.
    /// 4. Prepare function objects defined inner the function, and register them to the lexical environment.
    /// 5. Generate a new context, and set the current context (running execution context) to it.
    pub fn prepare_context_for_function_invokation(
        &mut self,
        callee: Value,
        user_func: FuncInfoRef,
        outer_env: Option<LexicalEnvironmentRef>,
        args: &[Value],
        this: Value,
        new_target: Value,
        mode: CallMode,
        constructor_call: bool,
    ) -> Result<(), RuntimeError> {
        let context = std::mem::replace(&mut self.current_context, ExecContext::empty());
        self.saved_context.push(context);

        let this = if user_func.this_mode == ThisMode::Lexical {
            // Arrow function
            outer_env.unwrap().get_this_binding()
        } else if user_func.this_mode == ThisMode::Derived {
            Value::uninitialized()
        } else if user_func.this_mode == ThisMode::Global && (this.is_undefined() || this.is_null())
        {
            self.global_environment.get_global_object()
        } else {
            this
        };

        let var_env_ref = self
            .factory
            .create_function_environment(callee, user_func, outer_env, args, this);

        let mut lex_env_ref = self.factory.create_lexical_environment(
            &user_func.lex_names,
            &user_func.const_names,
            var_env_ref,
        );

        for info in &user_func.func_decls {
            let name = info.func_name.as_ref().unwrap().as_str();
            let func = self.factory.function(*info, lex_env_ref);
            lex_env_ref.set_value(name, func)?;
        }

        let context = ExecContext::new(
            var_env_ref,
            lex_env_ref,
            user_func,
            this,
            mode,
            callee,
            new_target,
        )
        .constructor_call(constructor_call);
        self.current_context = context;
        Ok(())
    }

    fn create_generator_object(
        &mut self,
        callee: Value,
        user_func: FuncInfoRef,
        outer_env: Option<LexicalEnvironmentRef>,
        args: &[Value],
        this: Value,
    ) -> VMValueResult {
        let this = if user_func.this_mode == ThisMode::Lexical {
            outer_env.unwrap().get_this_binding()
        } else if user_func.this_mode == ThisMode::Global && (this.is_undefined() || this.is_null())
        {
            self.global_environment.get_global_object()
        } else {
            this
        };

        let var_env_ref = self
            .factory
            .create_function_environment(callee, user_func, outer_env, args, this);
        let mut lex_env_ref = self.factory.create_lexical_environment(
            &user_func.lex_names,
            &user_func.const_names,
            var_env_ref,
        );

        for info in &user_func.func_decls {
            let name = info.func_name.as_ref().unwrap().as_str();
            let func = self.factory.function(*info, lex_env_ref);
            lex_env_ref.set_value(name, func)?;
        }

        let mut context = ExecContext::new(
            var_env_ref,
            lex_env_ref,
            user_func,
            this,
            CallMode::Ordinary,
            callee,
            Value::undefined(),
        );
        if user_func.parameter_init_len > 0 {
            context.run_until_pc = Some(user_func.parameter_init_len);
            let outer_context = std::mem::replace(&mut self.current_context, context);
            let outer_saved_context = std::mem::take(&mut self.saved_context);
            let result = self.run();
            context = std::mem::replace(&mut self.current_context, outer_context);
            self.saved_context = outer_saved_context;
            result?;
            context.pc = user_func.parameter_init_len;
            context.run_until_pc = None;
        }
        let prototype = if user_func.async_function {
            self.factory.object_prototypes.async_generator
        } else {
            self.factory.object_prototypes.generator
        };
        let generator = Value::Object(self.factory.alloc(Object {
            kind: ObjectKind::Generator(GeneratorObjectInfo {
                context: Some(context),
                state: GeneratorState::SuspendedStart,
            }),
            prototype,
            property: FxHashMap::default(),
            property_order: Vec::new(),
            private_elements: rustc_hash::FxHashMap::default(),
            sym_property: FxHashMap::default(),
            sym_property_order: Vec::new(),
            extensible: true,
        }));
        Ok(generator)
    }

    fn call_async_user_function(
        &mut self,
        callee: Value,
        user_func: FuncInfoRef,
        outer_env: Option<LexicalEnvironmentRef>,
        args: &[Value],
        this: Value,
    ) -> VMValueResult {
        let saved_context_len = self.saved_context.len();
        match self.call_user_function(
            callee,
            user_func,
            outer_env,
            args,
            this,
            Value::undefined(),
            false,
        ) {
            Ok(value) => crate::builtins::promise::promise_resolve_direct(self, value),
            Err(error) => {
                while self.saved_context.len() > saved_context_len {
                    self.current_context = self.saved_context.pop().unwrap();
                }
                let reason = error.to_value(&mut self.factory);
                crate::builtins::promise::promise_reject_direct(self, reason)
            }
        }
    }

    fn enter_user_function(
        &mut self,
        callee: Value,
        user_func: FuncInfoRef,
        outer_env: Option<LexicalEnvironmentRef>,
        args: &[Value],
        this: Value,
        new_target: Value,
        constructor_call: bool,
    ) -> VMResult {
        if !user_func.constructible && constructor_call {
            return Err(self.current_context.error_type("Not a constructor"));
        }

        self.prepare_context_for_function_invokation(
            callee,
            user_func,
            outer_env,
            args,
            this,
            new_target,
            CallMode::Ordinary,
            constructor_call,
        )?;

        Ok(())
    }
}
