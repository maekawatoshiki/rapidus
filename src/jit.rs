use builtin::{BuiltinFuncInfo, BuiltinJITFuncInfo, BuiltinJITFuncTy};
use builtins::math;
use bytecode_gen::{ByteCode, VMInst};
use id::Id;
use vm;
use vm::{callobj::CallObject, value::FuncId};

use libc;
use llvm;
use llvm::core::*;
use llvm::prelude::*;
use rand::{random, thread_rng, RngCore};
use rustc_hash::{FxHashMap, FxHashSet};
use std::ffi::CString;
use std::mem::transmute;
use std::ptr;

const MAX_FUNCTION_PARAMS: usize = 3;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueType {
    Number,
    String,
    Bool,
}

trait CastIntoLLVMType {
    unsafe fn to_llvmty(&self, LLVMContextRef) -> LLVMTypeRef;
}

impl CastIntoLLVMType for ValueType {
    unsafe fn to_llvmty(&self, ctx: LLVMContextRef) -> LLVMTypeRef {
        match self {
            &ValueType::Number => LLVMDoubleTypeInContext(ctx),
            &ValueType::String => LLVMPointerType(LLVMInt8TypeInContext(ctx), 0),
            &ValueType::Bool => LLVMInt1TypeInContext(ctx),
        }
    }
}

fn get_value_type(val: &vm::value::Value) -> Option<ValueType> {
    match val.val {
        vm::value::ValueBase::Bool(_) => Some(ValueType::Bool),
        vm::value::ValueBase::Number(_) => Some(ValueType::Number),
        vm::value::ValueBase::String(_) => Some(ValueType::String),
        // TODO: Support more types.
        _ => None,
    }
}

macro_rules! get_int8 {
    ($iseq:ident, $pc:ident, $var:ident, $ty:ty) => {
        let $var = $iseq[$pc as usize] as $ty;
        $pc += 1;
    };
}

macro_rules! get_int32 {
    ($iseq:ident, $pc:ident, $var:ident, $ty:ty) => {
        let $var = (($iseq[$pc as usize + 3] as $ty) << 24)
            + (($iseq[$pc as usize + 2] as $ty) << 16)
            + (($iseq[$pc as usize + 1] as $ty) << 8)
            + ($iseq[$pc as usize + 0] as $ty);
        $pc += 4;
    };
}

macro_rules! try_opt {
    ($e:expr) => {
        match $e {
            Some(val) => val,
            None => return Err(()),
        }
    };
}

macro_rules! try_stack {
    ($e:expr) => {
        match $e {
            Some((val, None)) => val,
            _ => return Err(()),
        }
    };
}

#[derive(Debug, Clone)]
pub struct JITInfo {
    pub cannot_jit: bool,
}

#[derive(Debug, Clone)]
pub struct LoopInfo {
    raw_func: Option<fn(*mut f64) -> i32>,
    llvm_func: Option<LLVMValueRef>,
    local_vars: Vec<(usize, ValueType)>, // the (ids, types) of local variables used in this loop
    jit_info: JITInfo,
}

impl LoopInfo {
    pub fn new() -> LoopInfo {
        LoopInfo {
            raw_func: None,
            llvm_func: None,
            local_vars: vec![],
            jit_info: JITInfo { cannot_jit: false },
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncInfo {
    func_addr: Option<fn()>,
    llvm_func: Option<LLVMValueRef>,
    jit_info: JITInfo,
}

impl FuncInfo {
    pub fn new() -> FuncInfo {
        FuncInfo {
            func_addr: None,
            llvm_func: None,
            jit_info: JITInfo { cannot_jit: false },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UniquePosition {
    func_id: Id,
    pos: usize, // position in iseq
}

impl UniquePosition {
    pub fn new(func_id: Id, pos: usize) -> Self {
        UniquePosition {
            func_id: func_id,
            pos: pos,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TracingJit {
    pub loop_info: FxHashMap<UniquePosition, LoopInfo>,
    pub func_info: FxHashMap<FuncId, FuncInfo>,
    pub function_return_types: FxHashMap<usize, ValueType>,
    pub count: FxHashMap<UniquePosition, usize>,
    pub cur_func: Option<LLVMValueRef>,
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub pass_manager: LLVMPassManagerRef,
    pub used_builtin_funcs: FxHashSet<(BuiltinJITFuncTy, LLVMValueRef)>,
}

impl TracingJit {
    pub unsafe fn new() -> TracingJit {
        math::MATH_RAND_SEED = thread_rng().next_u64();

        llvm::target::LLVM_InitializeNativeTarget();
        llvm::target::LLVM_InitializeNativeAsmPrinter();
        llvm::target::LLVM_InitializeNativeAsmParser();
        llvm::target::LLVM_InitializeAllTargetMCs();
        llvm::execution_engine::LLVMLinkInMCJIT();

        let context = LLVMContextCreate();
        let module =
            LLVMModuleCreateWithNameInContext(CString::new("rapidus").unwrap().as_ptr(), context);

        let pm = LLVMCreatePassManager();
        llvm::transforms::scalar::LLVMAddReassociatePass(pm);
        llvm::transforms::scalar::LLVMAddGVNPass(pm);
        llvm::transforms::scalar::LLVMAddInstructionCombiningPass(pm);
        llvm::transforms::scalar::LLVMAddPromoteMemoryToRegisterPass(pm);
        llvm::transforms::scalar::LLVMAddTailCallEliminationPass(pm);
        llvm::transforms::scalar::LLVMAddJumpThreadingPass(pm);

        TracingJit {
            loop_info: FxHashMap::default(),
            func_info: FxHashMap::default(),
            function_return_types: FxHashMap::default(),
            count: FxHashMap::default(),
            context: context,
            module: module,
            builder: LLVMCreateBuilderInContext(context),
            pass_manager: pm,
            cur_func: None,
            used_builtin_funcs: FxHashSet::default(),
        }
    }
}

unsafe fn cur_bb_has_no_terminator(builder: LLVMBuilderRef) -> bool {
    LLVMIsATerminatorInst(LLVMGetLastInstruction(LLVMGetInsertBlock(builder))) == ptr::null_mut()
}

impl TracingJit {
    pub unsafe fn can_jit(
        &mut self,
        id: FuncId,
        iseq: &ByteCode,
        scope: &CallObject,
        const_table: &vm::vm::ConstantTable,
        argc: usize,
    ) -> Option<fn()> {
        if !self.func_is_called_enough_times(id, 0) {
            self.inc_count(id, 0);
            return None;
        }

        {
            let FuncInfo {
                func_addr,
                jit_info: JITInfo { cannot_jit },
                ..
            } = self.func_info.entry(id).or_insert(FuncInfo::new());
            if *cannot_jit {
                return None;
            }
            if let Some(func_addr) = func_addr {
                return Some(*func_addr);
            }
        }

        let name = format!("func-{}", random::<u32>());

        // If gen_code fails, it means the function can't be JIT-compiled and should never be
        // compiled. (cannot_jit = true)
        // llvm::execution_engine::LLVMAddModule(self.exec_engine, self.module);
        let llvm_func =
            match self.gen_code_for_func(name.clone(), iseq, scope, const_table, id, argc) {
                Ok(llvm_func) => llvm_func,
                Err(()) => {
                    self.func_info.get_mut(&id).unwrap().jit_info.cannot_jit = true;
                    return None;
                }
            };

        // LLVMDumpModule(self.module);

        // TODO: Is this REALLY the right way???
        let mut ee = 0 as llvm::execution_engine::LLVMExecutionEngineRef;
        let mut error = 0 as *mut i8;
        if llvm::execution_engine::LLVMCreateExecutionEngineForModule(
            &mut ee,
            self.module,
            &mut error,
        ) != 0
        {
            panic!("llvm error: failed to initialize execute engine")
        }

        for (func, llvm_func) in &self.used_builtin_funcs {
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *llvm_func,
                *func as *mut libc::c_void,
            );
        }

        let f_raw = llvm::execution_engine::LLVMGetFunctionAddress(
            ee,
            CString::new(name.as_str()).unwrap().as_ptr(),
        );
        let f = transmute::<u64, fn()>(f_raw);

        let info = self.func_info.get_mut(&id).unwrap();
        info.func_addr = Some(f);
        info.llvm_func = Some(llvm_func);

        Some(f)
    }

    unsafe fn gen_code_for_func(
        &mut self,
        name: String,
        iseq: &ByteCode,
        scope: &CallObject,
        const_table: &vm::vm::ConstantTable,
        func_id: FuncId,
        argc: usize,
    ) -> Result<LLVMValueRef, ()> {
        if argc > MAX_FUNCTION_PARAMS {
            return Err(());
        }

        let func_ret_ty = if let Some(ty) = self.function_return_types.get(&func_id) {
            ty.to_llvmty(self.context)
        } else {
            LLVMDoubleTypeInContext(self.context) // Assume as double
        };
        let func_ty = LLVMFunctionType(
            func_ret_ty,
            vec![LLVMDoubleTypeInContext(self.context)]
                .repeat(argc)
                .as_mut_slice()
                .as_mut_ptr(),
            argc as u32,
            0,
        );
        let func = LLVMAddFunction(
            self.module,
            CString::new(name.as_str()).unwrap().as_ptr(),
            func_ty,
        );
        let bb_entry = LLVMAppendBasicBlockInContext(
            self.context,
            func,
            CString::new("entry").unwrap().as_ptr(),
        );
        LLVMPositionBuilderAtEnd(self.builder, bb_entry);

        let mut env = FxHashMap::default();
        self.cur_func = Some(func);

        for i in 0..argc {
            LLVMBuildStore(
                self.builder,
                LLVMGetParam(func, i as u32),
                self.declare_local_var(scope.get_parameter_nth_name(i).unwrap(), &mut env),
            );
        }

        let mut compilation_failed = false;
        if let Err(_) = self.gen_body(
            iseq,
            scope,
            const_table,
            func_id,
            1, // 0 + 1(CreateContext)
            iseq.len(),
            true,
            &mut env,
        ) {
            compilation_failed = true;
        }

        let mut iter_bb = LLVMGetFirstBasicBlock(func);
        while iter_bb != ptr::null_mut() {
            if LLVMIsATerminatorInst(LLVMGetLastInstruction(iter_bb)) == ptr::null_mut() {
                let terminator_builder = LLVMCreateBuilderInContext(self.context);
                LLVMPositionBuilderAtEnd(terminator_builder, iter_bb);
                LLVMBuildRet(terminator_builder, LLVMConstNull(func_ret_ty));
            }
            iter_bb = LLVMGetNextBasicBlock(iter_bb);
        }

        // LLVMDumpValue(func);

        llvm::analysis::LLVMVerifyFunction(
            func,
            llvm::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction,
        );

        if compilation_failed {
            // Remove the unnecessary function.
            // TODO: Following code has a bug. Need fixing.
            //  ref. https://groups.google.com/forum/#!topic/llvm-dev/ovvfIe_zU3Y
            // LLVMReplaceAllUsesWith(func, LLVMGetUndef(LLVMTypeOf(func)));
            // LLVMInstructionEraseFromParent(func);
            return Err(());
        }

        LLVMRunPassManager(self.pass_manager, self.module);

        Ok(func)
    }

    pub unsafe fn can_loop_jit(
        &mut self,
        func_id: FuncId,
        iseq: &ByteCode,
        const_table: &vm::vm::ConstantTable,
        vm_state: &mut vm::vm::VMState,
        bgn: usize,
        end: usize,
    ) -> Option<isize> {
        if !self.loop_is_called_enough_times(func_id, bgn) {
            self.inc_count(func_id, bgn);
            return None;
        }

        {
            let LoopInfo {
                raw_func,
                local_vars,
                jit_info: JITInfo { cannot_jit },
                ..
            } = self
                .loop_info
                .entry(UniquePosition::new(func_id, bgn))
                .or_insert(LoopInfo::new());

            if *cannot_jit {
                return None;
            }

            if let Some(raw_func) = raw_func {
                return run_loop_llvm_func(*raw_func, vm_state, const_table, local_vars);
            }
        }

        let name = format!("loop-{}", random::<u32>());

        // If gen_code fails, it means the function can't be JIT-compiled and should never be
        // compiled. (cannot_jit = true)
        let (llvm_func, local_vars) =
            match self.gen_code_for_loop(name.clone(), vm_state, iseq, const_table, bgn, end) {
                Ok(info) => info,
                Err(()) => {
                    self.loop_info
                        .get_mut(&UniquePosition::new(func_id, bgn))
                        .unwrap()
                        .jit_info
                        .cannot_jit = true;
                    return None;
                }
            };

        // LLVMDumpModule(self.module);

        // TODO: Do we have to create exec engine every time?
        let mut ee = 0 as llvm::execution_engine::LLVMExecutionEngineRef;
        let mut error = 0 as *mut i8;
        if llvm::execution_engine::LLVMCreateExecutionEngineForModule(
            &mut ee,
            self.module,
            &mut error,
        ) != 0
        {
            panic!("llvm error: failed to initialize execute engine")
        }

        for (func, llvm_func) in &self.used_builtin_funcs {
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *llvm_func,
                *func as *mut libc::c_void,
            );
        }

        let raw_func =
            transmute::<u64, fn(*mut f64) -> i32>(llvm::execution_engine::LLVMGetFunctionAddress(
                ee,
                CString::new(name.as_str()).unwrap().as_ptr(),
            ));

        let info = self
            .loop_info
            .get_mut(&UniquePosition::new(func_id, bgn))
            .unwrap();

        info.raw_func = Some(raw_func);
        info.llvm_func = Some(llvm_func);
        info.local_vars = local_vars.clone();

        run_loop_llvm_func(raw_func, vm_state, const_table, &local_vars)
    }

    unsafe fn gen_code_for_loop(
        &mut self,
        name: String,
        vm_state: &mut vm::vm::VMState,
        iseq: &ByteCode,
        const_table: &vm::vm::ConstantTable,
        bgn: usize,
        end: usize,
    ) -> Result<(LLVMValueRef, Vec<(usize, ValueType)>), ()> {
        let local_vars = self.collect_local_variables(vm_state, iseq, const_table, bgn, end)?;

        let func_ret_ty = LLVMInt32TypeInContext(self.context);
        let func_ty = LLVMFunctionType(
            func_ret_ty,
            vec![LLVMPointerType(
                LLVMPointerType(LLVMInt8TypeInContext(self.context), 0),
                0,
            )]
            .as_mut_slice()
            .as_mut_ptr(),
            1,
            0,
        );
        let func = LLVMAddFunction(
            self.module,
            CString::new(name.as_str()).unwrap().as_ptr(),
            func_ty,
        );
        let bb_entry = LLVMAppendBasicBlockInContext(
            self.context,
            func,
            CString::new("entry").unwrap().as_ptr(),
        );
        LLVMPositionBuilderAtEnd(self.builder, bb_entry);

        let mut env = FxHashMap::default();
        self.cur_func = Some(func);

        let arg_0 = LLVMGetParam(func, 0);
        for i in 0..local_vars.len() {
            let local_var_name = const_table.string[local_vars[i].0].clone();
            let local_var_ref = LLVMBuildGEP(
                self.builder,
                arg_0,
                vec![LLVMConstInt(
                    LLVMInt32TypeInContext(self.context),
                    i as u64,
                    0,
                )]
                .as_mut_slice()
                .as_mut_ptr(),
                1,
                CString::new("").unwrap().as_ptr(),
            );
            let local_var_val = LLVMBuildLoad(
                self.builder,
                local_var_ref,
                CString::new("").unwrap().as_ptr(),
            );
            env.insert(
                local_var_name,
                LLVMBuildPointerCast(
                    self.builder,
                    local_var_val,
                    LLVMPointerType(local_vars[i].1.to_llvmty(self.context), 0),
                    CString::new("").unwrap().as_ptr(),
                ),
            );
        }

        let mut compilation_failed = false;
        if let Err(_) = self.gen_body(
            iseq,
            &**vm_state.scope.last().unwrap(),
            const_table,
            bgn,
            bgn,
            end,
            false,
            &mut env,
        ) {
            compilation_failed = true;
        }

        let mut iter_bb = LLVMGetFirstBasicBlock(func);
        while iter_bb != ptr::null_mut() {
            if LLVMIsATerminatorInst(LLVMGetLastInstruction(iter_bb)) == ptr::null_mut() {
                let terminator_builder = LLVMCreateBuilderInContext(self.context);
                LLVMPositionBuilderAtEnd(terminator_builder, iter_bb);
                LLVMBuildRet(
                    terminator_builder,
                    LLVMConstInt(LLVMInt32TypeInContext(self.context), end as u64, 0),
                );
            }
            iter_bb = LLVMGetNextBasicBlock(iter_bb);
        }

        llvm::analysis::LLVMVerifyFunction(
            func,
            llvm::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction,
        );

        // LLVMDumpValue(func);

        if compilation_failed {
            // Remove the unnecessary function.
            // TODO: Following code has a bug. Need fixing.
            //  ref. https://groups.google.com/forum/#!topic/llvm-dev/ovvfIe_zU3Y
            // LLVMReplaceAllUsesWith(func, LLVMGetUndef(LLVMTypeOf(func)));
            // LLVMInstructionEraseFromParent(func);
            return Err(());
        }

        LLVMRunPassManager(self.pass_manager, self.module);

        Ok((func, local_vars))
    }

    unsafe fn declare_local_var(
        &mut self,
        name: String,
        env: &mut FxHashMap<String, LLVMValueRef>,
    ) -> LLVMValueRef {
        if let Some(v) = env.get(&name) {
            return *v;
        }

        let func = self.cur_func.unwrap();
        let builder = LLVMCreateBuilderInContext(self.context);
        let entry_bb = LLVMGetEntryBasicBlock(func);
        let first_inst = LLVMGetFirstInstruction(entry_bb);
        // A variable is always declared at the first point of entry block
        if first_inst == ptr::null_mut() {
            LLVMPositionBuilderAtEnd(builder, entry_bb);
        } else {
            LLVMPositionBuilderBefore(builder, first_inst);
        }
        let var = LLVMBuildAlloca(
            builder,
            LLVMDoubleTypeInContext(self.context),
            CString::new("").unwrap().as_ptr(),
        );
        env.insert(name, var);
        var
    }

    unsafe fn collect_local_variables(
        &mut self,
        vm_state: &mut vm::vm::VMState,
        iseq: &ByteCode,
        const_table: &vm::vm::ConstantTable,
        mut pc: usize,
        end: usize,
    ) -> Result<Vec<(usize, ValueType)>, ()> {
        let mut local_vars = FxHashSet::default();
        let local_scope = &**vm_state.scope.last().unwrap();

        while pc < end {
            let inst_size = try_opt!(VMInst::get_inst_size(iseq[pc]));
            match iseq[pc] {
                VMInst::DECL_VAR | VMInst::SET_VALUE | VMInst::GET_VALUE => {
                    pc += 1;
                    get_int32!(iseq, pc, id, usize);
                    let name = &const_table.string[id];
                    if let Some(val) = (*local_scope.vals).get(name) {
                        let ty = if let Some(ty) = get_value_type(val) {
                            ty
                        } else {
                            continue;
                        };
                        local_vars.insert((id, ty));
                    }
                }
                _ => pc += inst_size,
            }
        }

        Ok(local_vars.iter().map(|x| x.clone()).collect())
    }

    unsafe fn gen_body(
        &mut self,
        iseq: &ByteCode,
        scope: &CallObject,
        const_table: &vm::vm::ConstantTable,
        func_id: FuncId,
        bgn: usize,
        end: usize,
        is_func_jit: bool,
        env: &mut FxHashMap<String, LLVMValueRef>,
    ) -> Result<(), ()> {
        let func = self.cur_func.unwrap();
        let mut stack: Vec<(LLVMValueRef, Option<vm::value::Value>)> = vec![];
        let mut labels: FxHashMap<usize, LabelKind> = FxHashMap::default();

        enum LabelKind {
            NotPositioned(LLVMBasicBlockRef),
            Positioned(LLVMBasicBlockRef),
        }

        fn label_retrieve(lk: &LabelKind) -> LLVMBasicBlockRef {
            match lk {
                &LabelKind::NotPositioned(x) | &LabelKind::Positioned(x) => x,
            }
        }

        unsafe fn infer_ty(
            llvm_val: LLVMValueRef,
            vm_val: &Option<vm::value::Value>,
        ) -> Result<ValueType, ()> {
            match vm_val {
                &Some(vm::value::Value {
                    val: vm::value::ValueBase::String(_),
                    ..
                }) => Ok(ValueType::String),
                _ => match LLVMGetTypeKind(LLVMTypeOf(llvm_val)) {
                    llvm::LLVMTypeKind::LLVMIntegerTypeKind
                        if LLVMGetIntTypeWidth(LLVMTypeOf(llvm_val)) == 1 =>
                    {
                        Ok(ValueType::Bool)
                    }
                    llvm::LLVMTypeKind::LLVMDoubleTypeKind => Ok(ValueType::Number),
                    _ => return Err(()),
                },
            }
        }

        unsafe fn call_builtin_function(
            self_: &mut TracingJit,
            builtin_func_info: BuiltinFuncInfo,
            args: Vec<(LLVMValueRef, ValueType)>,
            stack: &mut Vec<(LLVMValueRef, Option<vm::value::Value>)>,
        ) -> Option<()> {
            let jit_info = if let Some(jit_info) = builtin_func_info.jit_info {
                jit_info
            } else {
                return None;
            };

            match jit_info {
                BuiltinJITFuncInfo::ConsoleLog {
                    bool,
                    f64,
                    string,
                    newline,
                } => {
                    for (arg, ty) in args {
                        LLVMBuildCall(
                            self_.builder,
                            {
                                let ty = match ty {
                                    ValueType::Bool => bool,
                                    ValueType::Number => f64,
                                    ValueType::String => string,
                                };
                                self_.used_builtin_funcs.insert(ty);
                                ty.1
                            },
                            vec![arg].as_mut_ptr(),
                            1,
                            CString::new("").unwrap().as_ptr(),
                        );
                    }

                    self_.used_builtin_funcs.insert(newline);

                    LLVMBuildCall(
                        self_.builder,
                        newline.1,
                        vec![].as_mut_ptr(),
                        0,
                        CString::new("").unwrap().as_ptr(),
                    );
                }
                BuiltinJITFuncInfo::Normal { func, llvm_func } => {
                    self_.used_builtin_funcs.insert((func, llvm_func));

                    let result = LLVMBuildCall(
                        self_.builder,
                        llvm_func,
                        args.iter()
                            .map(|(x, _)| *x)
                            .collect::<Vec<LLVMValueRef>>()
                            .as_mut_ptr(),
                        args.len() as u32,
                        CString::new("").unwrap().as_ptr(),
                    );

                    stack.push((result, None));
                }
            }

            Some(())
        }

        unsafe fn val_to_bool(self_: &TracingJit, val: LLVMValueRef) -> LLVMValueRef {
            match LLVMGetTypeKind(LLVMTypeOf(val)) {
                llvm::LLVMTypeKind::LLVMDoubleTypeKind | llvm::LLVMTypeKind::LLVMFloatTypeKind => {
                    LLVMBuildFCmp(
                        self_.builder,
                        llvm::LLVMRealPredicate::LLVMRealONE,
                        val,
                        LLVMConstNull(LLVMTypeOf(val)),
                        CString::new("to_bool").unwrap().as_ptr(),
                    )
                }
                _ => LLVMBuildICmp(
                    self_.builder,
                    llvm::LLVMIntPredicate::LLVMIntNE,
                    val,
                    LLVMConstNull(LLVMTypeOf(val)),
                    CString::new("to_bool").unwrap().as_ptr(),
                ),
            }
        }

        // First of all, find JMP-related ops and record its destination.
        {
            let mut pc = bgn;
            while pc < end {
                let inst_size = try_opt!(VMInst::get_inst_size(iseq[pc]));
                match iseq[pc] {
                    VMInst::END => break,
                    VMInst::CREATE_CONTEXT if is_func_jit => break,
                    VMInst::JMP | VMInst::JMP_IF_FALSE => {
                        pc += 1;
                        get_int32!(iseq, pc, dst, i32);
                        // println!("pc: {}, dst: {}, = {}", pc, dst, pc as i32 + dst);
                        labels.insert(
                            (pc as i32 + dst) as usize,
                            LabelKind::NotPositioned(LLVMAppendBasicBlock(
                                func,
                                CString::new("").unwrap().as_ptr(),
                            )),
                        );
                    }
                    _ => pc += inst_size,
                }
            }
        }

        let mut logand: Vec<LLVMBasicBlockRef> = Vec::new();
        let mut logcom: Vec<LLVMBasicBlockRef> = Vec::new();
        let mut logor: Vec<LLVMBasicBlockRef> = Vec::new();

        let mut pc = bgn;
        while pc < end {
            if let Some(label_kind) = labels.get_mut(&pc) {
                let bb = if let LabelKind::NotPositioned(bb) = *label_kind {
                    bb
                } else {
                    unreachable!()
                };
                if cur_bb_has_no_terminator(self.builder) {
                    LLVMBuildBr(self.builder, bb);
                }
                LLVMPositionBuilderAtEnd(self.builder, bb);
                *label_kind = LabelKind::Positioned(bb);
            }

            match iseq[pc] {
                VMInst::END => break,
                VMInst::CREATE_CONTEXT => break,
                VMInst::LOOP_START => pc += 5,
                VMInst::CONSTRUCT | VMInst::CREATE_OBJECT | VMInst::CREATE_ARRAY => pc += 5,
                VMInst::JMP_IF_FALSE => {
                    pc += 1;
                    get_int32!(iseq, pc, dst, i32);
                    let bb_then = LLVMAppendBasicBlock(func, CString::new("").unwrap().as_ptr());
                    let bb_else =
                        label_retrieve(try_opt!(labels.get(&((pc as i32 + dst) as usize))));

                    logand.push(LLVMGetInsertBlock(self.builder));
                    logcom.push(bb_then);
                    logor.push(bb_else);

                    let cond_val = val_to_bool(self, try_stack!(stack.pop()));
                    LLVMBuildCondBr(self.builder, cond_val, bb_then, bb_else);
                    LLVMPositionBuilderAtEnd(self.builder, bb_then);
                }
                VMInst::JMP => {
                    pc += 1;
                    get_int32!(iseq, pc, dst, i32);
                    let bb = label_retrieve(try_opt!(labels.get(&((pc as i32 + dst) as usize))));
                    if cur_bb_has_no_terminator(self.builder) {
                        LLVMBuildBr(self.builder, bb);
                    }

                    let bb = LLVMAppendBasicBlock(func, CString::new("").unwrap().as_ptr());
                    LLVMPositionBuilderAtEnd(self.builder, bb);
                }
                VMInst::COND_OP => {
                    pc += 1;
                    let else_val = try_stack!(stack.pop());
                    let then_val = try_stack!(stack.pop());
                    let phi = LLVMBuildPhi(
                        self.builder,
                        LLVMTypeOf(then_val),
                        CString::new("condop").unwrap().as_ptr(),
                    );
                    LLVMAddIncoming(
                        phi,
                        vec![then_val].as_mut_slice().as_mut_ptr(),
                        vec![logcom.pop().unwrap()].as_mut_slice().as_mut_ptr(),
                        1,
                    );
                    LLVMAddIncoming(
                        phi,
                        vec![else_val].as_mut_slice().as_mut_ptr(),
                        vec![logor.pop().unwrap()].as_mut_slice().as_mut_ptr(),
                        1,
                    );

                    logand.pop();
                    if let Some(x) = logor.last_mut() {
                        *x = LLVMGetInsertBlock(self.builder);
                    }

                    stack.push((phi, None));
                }
                VMInst::LAND => {
                    pc += 1;
                    let phi = LLVMBuildPhi(
                        self.builder,
                        LLVMInt1TypeInContext(self.context),
                        CString::new("logand").unwrap().as_ptr(),
                    );
                    LLVMAddIncoming(
                        phi,
                        vec![LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0)]
                            .as_mut_slice()
                            .as_mut_ptr(),
                        vec![logand.pop().unwrap()].as_mut_slice().as_mut_ptr(),
                        1,
                    );
                    LLVMAddIncoming(
                        phi,
                        vec![try_stack!(stack.pop())].as_mut_slice().as_mut_ptr(),
                        vec![logcom.pop().unwrap()].as_mut_slice().as_mut_ptr(),
                        1,
                    );

                    logor.pop();
                    if let Some(x) = logor.last_mut() {
                        *x = LLVMGetInsertBlock(self.builder);
                    }

                    stack.push((phi, None));
                }
                VMInst::LOR => {
                    pc += 1;
                    let phi = LLVMBuildPhi(
                        self.builder,
                        LLVMInt1TypeInContext(self.context),
                        CString::new("logor").unwrap().as_ptr(),
                    );
                    LLVMAddIncoming(
                        phi,
                        vec![LLVMConstInt(LLVMInt1TypeInContext(self.context), 1, 0)]
                            .as_mut_slice()
                            .as_mut_ptr(),
                        vec![logcom.pop().unwrap()].as_mut_slice().as_mut_ptr(),
                        1,
                    );
                    LLVMAddIncoming(
                        phi,
                        vec![try_stack!(stack.pop())].as_mut_slice().as_mut_ptr(),
                        vec![logor.pop().unwrap()].as_mut_slice().as_mut_ptr(),
                        1,
                    );

                    logand.pop();
                    if let Some(x) = logcom.last_mut() {
                        *x = LLVMGetInsertBlock(self.builder);
                    }

                    stack.push((phi, None));
                }
                VMInst::ADD => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildFAdd(
                            self.builder,
                            lhs,
                            rhs,
                            CString::new("fadd").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::SUB => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildFSub(
                            self.builder,
                            lhs,
                            rhs,
                            CString::new("fsub").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::MUL => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildFMul(
                            self.builder,
                            lhs,
                            rhs,
                            CString::new("fmul").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::DIV => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildFDiv(
                            self.builder,
                            lhs,
                            rhs,
                            CString::new("fdiv").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::REM => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildSIToFP(
                            self.builder,
                            LLVMBuildSRem(
                                self.builder,
                                LLVMBuildFPToSI(
                                    self.builder,
                                    lhs,
                                    LLVMInt64TypeInContext(self.context),
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                LLVMBuildFPToSI(
                                    self.builder,
                                    rhs,
                                    LLVMInt64TypeInContext(self.context),
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                CString::new("frem").unwrap().as_ptr(),
                            ),
                            LLVMDoubleTypeInContext(self.context),
                            CString::new("").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::LT => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildFCmp(
                            self.builder,
                            llvm::LLVMRealPredicate::LLVMRealOLT,
                            lhs,
                            rhs,
                            CString::new("flt").unwrap().as_ptr(),
                        ),
                        None,
                    ))
                }
                VMInst::LE => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildFCmp(
                            self.builder,
                            llvm::LLVMRealPredicate::LLVMRealOLE,
                            lhs,
                            rhs,
                            CString::new("fle").unwrap().as_ptr(),
                        ),
                        None,
                    ))
                }
                VMInst::GT => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildFCmp(
                            self.builder,
                            llvm::LLVMRealPredicate::LLVMRealOGT,
                            lhs,
                            rhs,
                            CString::new("fgt").unwrap().as_ptr(),
                        ),
                        None,
                    ))
                }
                VMInst::GE => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildFCmp(
                            self.builder,
                            llvm::LLVMRealPredicate::LLVMRealOGE,
                            lhs,
                            rhs,
                            CString::new("fge").unwrap().as_ptr(),
                        ),
                        None,
                    ))
                }
                VMInst::EQ => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildFCmp(
                            self.builder,
                            llvm::LLVMRealPredicate::LLVMRealOEQ,
                            lhs,
                            rhs,
                            CString::new("feq").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::NE => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildFCmp(
                            self.builder,
                            llvm::LLVMRealPredicate::LLVMRealONE,
                            lhs,
                            rhs,
                            CString::new("fne").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::SEQ => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildFCmp(
                            self.builder,
                            llvm::LLVMRealPredicate::LLVMRealOEQ,
                            lhs,
                            rhs,
                            CString::new("feq").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::SNE => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildFCmp(
                            self.builder,
                            llvm::LLVMRealPredicate::LLVMRealONE,
                            lhs,
                            rhs,
                            CString::new("fne").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::NEG => {
                    pc += 1;
                    let val = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildFNeg(self.builder, val, CString::new("fneg").unwrap().as_ptr()),
                        None,
                    ));
                }
                VMInst::AND => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildSIToFP(
                            self.builder,
                            LLVMBuildAnd(
                                self.builder,
                                LLVMBuildFPToSI(
                                    self.builder,
                                    lhs,
                                    LLVMInt32TypeInContext(self.context),
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                LLVMBuildFPToSI(
                                    self.builder,
                                    rhs,
                                    LLVMInt32TypeInContext(self.context),
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                CString::new("and").unwrap().as_ptr(),
                            ),
                            LLVMDoubleTypeInContext(self.context),
                            CString::new("").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::OR => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildSIToFP(
                            self.builder,
                            LLVMBuildOr(
                                self.builder,
                                LLVMBuildFPToSI(
                                    self.builder,
                                    lhs,
                                    LLVMInt32TypeInContext(self.context),
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                LLVMBuildFPToSI(
                                    self.builder,
                                    rhs,
                                    LLVMInt32TypeInContext(self.context),
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                CString::new("or").unwrap().as_ptr(),
                            ),
                            LLVMDoubleTypeInContext(self.context),
                            CString::new("").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::XOR => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildSIToFP(
                            self.builder,
                            LLVMBuildXor(
                                self.builder,
                                LLVMBuildFPToSI(
                                    self.builder,
                                    lhs,
                                    LLVMInt32TypeInContext(self.context),
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                LLVMBuildFPToSI(
                                    self.builder,
                                    rhs,
                                    LLVMInt32TypeInContext(self.context),
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                CString::new("or").unwrap().as_ptr(),
                            ),
                            LLVMDoubleTypeInContext(self.context),
                            CString::new("").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::SHL => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildSIToFP(
                            self.builder,
                            LLVMBuildShl(
                                self.builder,
                                LLVMBuildTruncOrBitCast(
                                    self.builder,
                                    LLVMBuildFPToSI(
                                        self.builder,
                                        lhs,
                                        LLVMInt64TypeInContext(self.context),
                                        CString::new("").unwrap().as_ptr(),
                                    ),
                                    LLVMInt32TypeInContext(self.context),
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                LLVMBuildTruncOrBitCast(
                                    self.builder,
                                    LLVMBuildFPToSI(
                                        self.builder,
                                        rhs,
                                        LLVMInt64TypeInContext(self.context),
                                        CString::new("").unwrap().as_ptr(),
                                    ),
                                    LLVMInt32TypeInContext(self.context),
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                CString::new("or").unwrap().as_ptr(),
                            ),
                            LLVMDoubleTypeInContext(self.context),
                            CString::new("").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::SHR => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildSIToFP(
                            self.builder,
                            LLVMBuildAShr(
                                self.builder,
                                LLVMBuildTruncOrBitCast(
                                    self.builder,
                                    LLVMBuildFPToSI(
                                        self.builder,
                                        lhs,
                                        LLVMInt64TypeInContext(self.context),
                                        CString::new("").unwrap().as_ptr(),
                                    ),
                                    LLVMInt32TypeInContext(self.context),
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                LLVMBuildTruncOrBitCast(
                                    self.builder,
                                    LLVMBuildFPToSI(
                                        self.builder,
                                        rhs,
                                        LLVMInt64TypeInContext(self.context),
                                        CString::new("").unwrap().as_ptr(),
                                    ),
                                    LLVMInt32TypeInContext(self.context),
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                CString::new("or").unwrap().as_ptr(),
                            ),
                            LLVMDoubleTypeInContext(self.context),
                            CString::new("").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::ZFSHR => {
                    pc += 1;
                    let rhs = try_stack!(stack.pop());
                    let lhs = try_stack!(stack.pop());
                    stack.push((
                        LLVMBuildSIToFP(
                            self.builder,
                            LLVMBuildLShr(
                                self.builder,
                                LLVMBuildTruncOrBitCast(
                                    self.builder,
                                    LLVMBuildFPToSI(
                                        self.builder,
                                        lhs,
                                        LLVMInt64TypeInContext(self.context),
                                        CString::new("").unwrap().as_ptr(),
                                    ),
                                    LLVMInt32TypeInContext(self.context),
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                LLVMBuildTruncOrBitCast(
                                    self.builder,
                                    LLVMBuildFPToSI(
                                        self.builder,
                                        rhs,
                                        LLVMInt64TypeInContext(self.context),
                                        CString::new("").unwrap().as_ptr(),
                                    ),
                                    LLVMInt32TypeInContext(self.context),
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                CString::new("or").unwrap().as_ptr(),
                            ),
                            LLVMDoubleTypeInContext(self.context),
                            CString::new("").unwrap().as_ptr(),
                        ),
                        None,
                    ));
                }
                VMInst::GET_VALUE => {
                    pc += 1;
                    get_int32!(iseq, pc, id, usize);
                    let name = &const_table.string[id];
                    match env.get(name) {
                        Some(val) => {
                            stack.push((
                                LLVMBuildLoad(
                                    self.builder,
                                    *val,
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                None,
                            ));
                        }
                        None => match scope.get_value(name).unwrap().val {
                            vm::value::ValueBase::Function(box (id, _, _, _)) if id == func_id => {
                                stack.push((func, None));
                            }
                            vm::value::ValueBase::Function(box (id, _, _, _)) => {
                                match self.func_info.get(&id) {
                                    Some(FuncInfo { llvm_func, .. }) if llvm_func.is_some() => {
                                        stack.push((llvm_func.unwrap(), None));
                                    }
                                    _ => return Err(()),
                                }
                            }
                            // vm::value::ValueBase::BuiltinFunction(box (info, _, _)) => {
                            //     // TODO: BUG
                            //     match self.builtin_funcs.get(&info.id) {
                            //         Some(f) => stack.push((*f, None)),
                            //         _ => return Err(()),
                            //     }
                            // }
                            vm::value::ValueBase::Object(obj) => {
                                stack.push((ptr::null_mut(), Some(vm::value::Value::object(obj))))
                            }
                            _ => return Err(()),
                        },
                    }
                }
                VMInst::SET_VALUE => {
                    pc += 1;
                    get_int32!(iseq, pc, id, usize);
                    let name = &const_table.string[id];
                    let val = try_stack!(stack.pop());
                    LLVMBuildStore(self.builder, val, self.declare_local_var(name.clone(), env));
                }
                VMInst::DECL_VAR => pc += 5,
                VMInst::UPDATE_PARENT_SCOPE => pc += 1,
                VMInst::CALL => {
                    pc += 1;
                    get_int32!(iseq, pc, argc, usize);

                    let callee = try_opt!(stack.pop());

                    if let Some(callee) = callee.1 {
                        let mut args = vec![];
                        for _ in 0..argc {
                            let arg = try_opt!(stack.pop());
                            args.push((arg.0, infer_ty(arg.0, &arg.1)?));
                        }

                        try_opt!(call_builtin_function(
                            self,
                            if let vm::value::ValueBase::BuiltinFunction(box (info, _, _)) =
                                callee.val
                            {
                                info
                            } else {
                                return Err(());
                            },
                            args,
                            &mut stack,
                        ));
                    } else {
                        let mut llvm_args = vec![];
                        for _ in 0..argc {
                            llvm_args.push(try_opt!(stack.pop()).0);
                        }
                        stack.push((
                            LLVMBuildCall(
                                self.builder,
                                callee.0,
                                llvm_args.as_mut_ptr(),
                                llvm_args.len() as u32,
                                CString::new("").unwrap().as_ptr(),
                            ),
                            None,
                        ));
                    }
                }
                VMInst::GET_MEMBER => {
                    pc += 1; // get_member
                    let member = try_opt!(try_opt!(stack.pop()).1);
                    let parent = try_opt!(try_opt!(stack.pop()).1);
                    match parent.val {
                        vm::value::ValueBase::Object(map) => stack.push((
                            ptr::null_mut(),
                            Some(vm::value::obj_find_val(&*map, member.to_string().as_str())),
                        )),
                        _ => return Err(()),
                    }
                }
                VMInst::PUSH_CONST => {
                    pc += 1;
                    get_int32!(iseq, pc, n, usize);
                    match const_table.value[n].val {
                        vm::value::ValueBase::Bool(false) => stack.push((
                            LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0),
                            None,
                        )),
                        vm::value::ValueBase::Bool(true) => stack.push((
                            LLVMConstInt(LLVMInt1TypeInContext(self.context), 1, 0),
                            None,
                        )),
                        vm::value::ValueBase::Number(n) => stack.push((
                            LLVMConstReal(LLVMDoubleTypeInContext(self.context), n as f64),
                            None,
                        )),
                        vm::value::ValueBase::Function(box (id, _, _, _))
                            if is_func_jit && id == func_id =>
                        {
                            stack.push((func, None))
                        }
                        vm::value::ValueBase::Function(box (id, _, _, _)) => stack.push((
                            match self.func_info.get(&id) {
                                Some(FuncInfo { llvm_func, .. }) if llvm_func.is_some() => {
                                    llvm_func.unwrap()
                                }
                                _ => return Err(()),
                            },
                            None,
                        )),
                        vm::value::ValueBase::String(ref s) => stack.push((
                            LLVMBuildIntToPtr(
                                self.builder,
                                LLVMConstInt(
                                    LLVMInt64TypeInContext(self.context),
                                    s.as_ptr() as u64,
                                    0,
                                ),
                                LLVMPointerType(LLVMInt8TypeInContext(self.context), 0),
                                CString::new("").unwrap().as_ptr(),
                            ),
                            Some(const_table.value[n].clone()),
                        )),
                        vm::value::ValueBase::Object(_) => {
                            stack.push((ptr::null_mut(), Some(const_table.value[n].clone())))
                        }
                        // vm::value::ValueBase::BuiltinFunction(box (ref info, _, _)) => stack.push((
                        //         // TODO: BUG
                        //     if let Some(f) = self.builtin_funcs.get(&info.id) {
                        //         *f
                        //     } else {
                        //         return Err(());
                        //     },
                        //     None,
                        // )),
                        _ => return Err(()),
                    }
                }
                VMInst::PUSH_INT8 => {
                    pc += 1;
                    get_int8!(iseq, pc, n, isize);
                    stack.push((
                        LLVMConstReal(LLVMDoubleTypeInContext(self.context), n as f64),
                        None,
                    ));
                }
                VMInst::PUSH_INT32 => {
                    pc += 1;
                    get_int32!(iseq, pc, n, isize);
                    stack.push((
                        LLVMConstReal(LLVMDoubleTypeInContext(self.context), n as f64),
                        None,
                    ));
                }
                VMInst::PUSH_TRUE => {
                    pc += 1;
                    stack.push((
                        LLVMConstInt(LLVMInt1TypeInContext(self.context), 1, 0),
                        None,
                    ));
                }
                VMInst::PUSH_FALSE => {
                    pc += 1;
                    stack.push((
                        LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0),
                        None,
                    ));
                }
                VMInst::PUSH_THIS | VMInst::PUSH_ARGUMENTS | VMInst::SET_MEMBER => pc += 1,
                VMInst::POP => {
                    pc += 1;
                    stack.pop();
                }
                VMInst::DOUBLE => {
                    pc += 1;
                    let stack_top_val = stack.last().unwrap().clone();
                    stack.push(stack_top_val);
                }
                VMInst::RETURN if is_func_jit => {
                    pc += 1;
                    let val = try_stack!(stack.pop());
                    LLVMBuildRet(self.builder, val);
                }
                _ => return Err(()),
            }
        }

        if !is_func_jit {
            for (pos, label_kind) in labels {
                match label_kind {
                    LabelKind::NotPositioned(bb) => {
                        if cur_bb_has_no_terminator(self.builder) {
                            LLVMBuildBr(self.builder, bb);
                        }
                        LLVMPositionBuilderAtEnd(self.builder, bb);
                        LLVMBuildRet(
                            self.builder,
                            LLVMConstInt(LLVMInt32TypeInContext(self.context), pos as u64, 0),
                        );
                    }
                    LabelKind::Positioned(_) => {}
                }
            }
        }

        // LLVMDumpModule(self.module);

        Ok(())
    }

    pub fn record_function_return_type(&mut self, func_id: FuncId, val: &vm::value::Value) {
        if let Some(ty) = get_value_type(val) {
            self.function_return_types.insert(func_id, ty);
        }
    }

    pub unsafe fn run_llvm_func(
        &mut self,
        pc: usize,
        f: fn(),
        args: &Vec<vm::value::Value>,
    ) -> vm::value::Value {
        let mut llvm_args = vec![];
        for arg in args {
            llvm_args.push(match arg.val {
                vm::value::ValueBase::Number(f) => f,
                _ => unimplemented!(),
            });
        }

        let func_ret_ty = self
            .function_return_types
            .get(&pc)
            .unwrap_or(&ValueType::Number);

        // Because of a bug of LLVM, llvm::execution_engine::runFunction can not be used.
        // So, all I can do is this:
        // TODO: MAX_FUNCTION_PARAMS is too small?
        match func_ret_ty {
            &ValueType::Number => vm::value::Value::number(match llvm_args.len() {
                0 => transmute::<fn(), fn() -> f64>(f)(),
                1 => transmute::<fn(), fn(f64) -> f64>(f)(llvm_args[0]),
                2 => transmute::<fn(), fn(f64, f64) -> f64>(f)(llvm_args[0], llvm_args[1]),
                3 => transmute::<fn(), fn(f64, f64, f64) -> f64>(f)(
                    llvm_args[0],
                    llvm_args[1],
                    llvm_args[2],
                ),
                _ => unimplemented!("should be implemented.."),
            }),
            &ValueType::Bool => vm::value::Value::bool(match llvm_args.len() {
                0 => transmute::<fn(), fn() -> bool>(f)(),
                1 => transmute::<fn(), fn(f64) -> bool>(f)(llvm_args[0]),
                2 => transmute::<fn(), fn(f64, f64) -> bool>(f)(llvm_args[0], llvm_args[1]),
                3 => transmute::<fn(), fn(f64, f64, f64) -> bool>(f)(
                    llvm_args[0],
                    llvm_args[1],
                    llvm_args[2],
                ),
                _ => unimplemented!("should be implemented.."),
            }),
            &ValueType::String => unimplemented!(),
        }
    }
}

pub unsafe fn run_loop_llvm_func(
    f: fn(*mut f64) -> i32,
    vm_state: &mut vm::vm::VMState,
    const_table: &vm::vm::ConstantTable,
    local_vars: &Vec<(usize, ValueType)>,
) -> Option<isize> {
    let scope = *vm_state.scope.last().unwrap();
    let mut args_of_local_vars = vec![];

    for (id, _) in local_vars {
        let name = &const_table.string[*id];
        args_of_local_vars.push(match (*scope).get_value(name).unwrap().val {
            vm::value::ValueBase::Number(f) => Box::into_raw(Box::new(f)) as *mut libc::c_void,
            vm::value::ValueBase::Bool(b) => Box::into_raw(Box::new(b)) as *mut libc::c_void,
            _ => return None,
        });
    }

    // println!("before: farg[{:?}] local[{:?}]", args_of_arg_vars, args_of_local_vars);
    let pc = transmute::<fn(*mut f64) -> i32, fn(*mut *mut libc::c_void) -> i32>(f)(
        args_of_local_vars.as_mut_slice().as_mut_ptr(),
    );
    // println!("after:  farg[{:?}] local[{:?}]", args_of_arg_vars, args_of_local_vars);

    for (i, (id, ty)) in local_vars.iter().enumerate() {
        let name = const_table.string[*id].clone();
        (*scope).set_value_if_exist(
            name,
            match ty {
                ValueType::Number => vm::value::Value::number(*(args_of_local_vars[i] as *mut f64)),
                ValueType::Bool => vm::value::Value::bool(*(args_of_local_vars[i] as *mut bool)),
                _ => unimplemented!(),
            },
        );
        Box::from_raw(args_of_local_vars[i]);
    }

    Some(pc as isize)
}

impl TracingJit {
    #[inline]
    fn func_is_called_enough_times(&mut self, id: FuncId, pc: usize) -> bool {
        *self.count.entry(UniquePosition::new(id, pc)).or_insert(0) >= 5
    }

    #[inline]
    fn loop_is_called_enough_times(&mut self, id: FuncId, pc: usize) -> bool {
        *self.count.entry(UniquePosition::new(id, pc)).or_insert(0) >= 7
    }

    #[inline]
    fn inc_count(&mut self, id: FuncId, pc: usize) {
        *self.count.entry(UniquePosition::new(id, pc)).or_insert(0) += 1;
    }
}
