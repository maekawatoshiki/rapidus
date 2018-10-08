use builtin;
use bytecode_gen::VMInst;
use vm;
use vm::CallObject;

use rand::{random, thread_rng, RngCore};

use std::collections::{HashMap, HashSet};

use libc;
use llvm;
use llvm::core::*;
use llvm::prelude::*;

use std::ffi::CString;
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

fn get_value_type(val: &vm::Value) -> Option<ValueType> {
    match val {
        &vm::Value::Bool(_) => Some(ValueType::Bool),
        &vm::Value::Number(_) => Some(ValueType::Number),
        &vm::Value::String(_) => Some(ValueType::String),
        // TODO: Support more types.
        _ => None,
    }
}

macro_rules! get_int8 {
    ($insts:ident, $pc:ident, $var:ident, $ty:ty) => {
        let $var = $insts[$pc as usize] as $ty;
        $pc += 1;
    };
}

macro_rules! get_int32 {
    ($insts:ident, $pc:ident, $var:ident, $ty:ty) => {
        let $var = (($insts[$pc as usize + 3] as $ty) << 24)
            + (($insts[$pc as usize + 2] as $ty) << 16)
            + (($insts[$pc as usize + 1] as $ty) << 8)
            + ($insts[$pc as usize + 0] as $ty);
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
    func_addr: Option<fn(*mut f64) -> i32>,
    llvm_func: Option<LLVMValueRef>,
    local_vars_id: Vec<(usize, ValueType)>, // the (ids, types) of local variables used in this loop
    jit_info: JITInfo,
}

impl LoopInfo {
    pub fn new() -> LoopInfo {
        LoopInfo {
            func_addr: None,
            llvm_func: None,
            local_vars_id: vec![],
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

#[derive(Debug, Clone)]
pub struct TracingJit {
    loop_info: HashMap<usize, LoopInfo>, // <pos in bytecode, loop info>
    func_info: HashMap<usize, FuncInfo>, // <pos in bytecode, func info>
    return_ty_map: HashMap<usize, ValueType>,
    count: HashMap<usize, usize>,
    cur_func: Option<LLVMValueRef>,
    builtin_funcs: HashMap<usize, LLVMValueRef>,
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    pass_manager: LLVMPassManagerRef,
}

impl TracingJit {
    pub unsafe fn new() -> TracingJit {
        MATH_RAND_SEED = thread_rng().next_u64();

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
            loop_info: HashMap::new(),
            func_info: HashMap::new(),
            return_ty_map: HashMap::new(),
            count: HashMap::new(),
            context: context,
            module: module,
            builder: LLVMCreateBuilderInContext(context),
            pass_manager: pm,
            cur_func: None,
            builtin_funcs: {
                let mut hmap = HashMap::new();

                let f_console_log_string = LLVMAddFunction(
                    module,
                    CString::new("console_log_string").unwrap().as_ptr(),
                    LLVMFunctionType(
                        LLVMVoidType(),
                        vec![LLVMPointerType(LLVMInt8TypeInContext(context), 0)]
                            .as_mut_slice()
                            .as_mut_ptr(),
                        1,
                        0,
                    ),
                );
                hmap.insert(BUILTIN_CONSOLE_LOG_STRING, f_console_log_string);

                let f_console_log_bool = LLVMAddFunction(
                    module,
                    CString::new("console_log_bool").unwrap().as_ptr(),
                    LLVMFunctionType(
                        LLVMVoidType(),
                        vec![LLVMInt1TypeInContext(context)]
                            .as_mut_slice()
                            .as_mut_ptr(),
                        1,
                        0,
                    ),
                );
                hmap.insert(BUILTIN_CONSOLE_LOG_BOOL, f_console_log_bool);

                let f_console_log_f64 = LLVMAddFunction(
                    module,
                    CString::new("console_log_f64").unwrap().as_ptr(),
                    LLVMFunctionType(
                        LLVMVoidType(),
                        vec![LLVMDoubleTypeInContext(context)]
                            .as_mut_slice()
                            .as_mut_ptr(),
                        1,
                        0,
                    ),
                );
                hmap.insert(BUILTIN_CONSOLE_LOG_F64, f_console_log_f64);

                let f_console_log_newline = LLVMAddFunction(
                    module,
                    CString::new("console_log_newline").unwrap().as_ptr(),
                    LLVMFunctionType(LLVMVoidType(), vec![].as_mut_ptr(), 0, 0),
                );
                hmap.insert(BUILTIN_CONSOLE_LOG_NEWLINE, f_console_log_newline);

                let f_process_stdout_write = LLVMAddFunction(
                    module,
                    CString::new("process_stdout_write").unwrap().as_ptr(),
                    LLVMFunctionType(
                        LLVMVoidType(),
                        vec![LLVMPointerType(LLVMInt8TypeInContext(context), 0)]
                            .as_mut_slice()
                            .as_mut_ptr(),
                        1,
                        0,
                    ),
                );
                hmap.insert(BUILTIN_PROCESS_STDOUT_WRITE, f_process_stdout_write);

                let f_math_pow = LLVMAddFunction(
                    module,
                    CString::new("math_pow").unwrap().as_ptr(),
                    LLVMFunctionType(
                        LLVMDoubleTypeInContext(context),
                        vec![
                            LLVMDoubleTypeInContext(context),
                            LLVMDoubleTypeInContext(context),
                        ].as_mut_slice()
                            .as_mut_ptr(),
                        2,
                        0,
                    ),
                );
                hmap.insert(BUILTIN_MATH_POW, f_math_pow);

                let f_math_floor = LLVMAddFunction(
                    module,
                    CString::new("math_floor").unwrap().as_ptr(),
                    LLVMFunctionType(
                        LLVMDoubleTypeInContext(context),
                        vec![LLVMDoubleTypeInContext(context)]
                            .as_mut_slice()
                            .as_mut_ptr(),
                        1,
                        0,
                    ),
                );
                hmap.insert(BUILTIN_MATH_FLOOR, f_math_floor);

                let f_math_random = LLVMAddFunction(
                    module,
                    CString::new("math_random").unwrap().as_ptr(),
                    LLVMFunctionType(
                        LLVMDoubleTypeInContext(context),
                        vec![].as_mut_slice().as_mut_ptr(),
                        0,
                        0,
                    ),
                );
                hmap.insert(BUILTIN_MATH_RANDOM, f_math_random);

                hmap
            },
        }
    }
}

unsafe fn cur_bb_has_no_terminator(builder: LLVMBuilderRef) -> bool {
    LLVMIsATerminatorInst(LLVMGetLastInstruction(LLVMGetInsertBlock(builder))) == ptr::null_mut()
}

impl TracingJit {
    pub unsafe fn can_jit(
        &mut self,
        insts: &Vec<u8>,
        scope: &CallObject,
        const_table: &vm::ConstantTable,
        pc: usize,
        argc: usize,
    ) -> Option<fn()> {
        if !self.func_is_called_enough_times(pc) {
            self.inc_count(pc);
            return None;
        }

        {
            let FuncInfo {
                func_addr,
                jit_info: JITInfo { cannot_jit },
                ..
            } = self.func_info.entry(pc).or_insert(FuncInfo::new());
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
            match self.gen_code_for_func(name.clone(), insts, scope, const_table, pc, argc) {
                Ok(llvm_func) => llvm_func,
                Err(()) => {
                    self.func_info.get_mut(&pc).unwrap().jit_info.cannot_jit = true;
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
            panic!()
        }
        {
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self.builtin_funcs.get(&BUILTIN_CONSOLE_LOG_STRING).unwrap(),
                console_log_string as *mut libc::c_void,
            );
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self.builtin_funcs.get(&BUILTIN_CONSOLE_LOG_BOOL).unwrap(),
                console_log_bool as *mut libc::c_void,
            );
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self.builtin_funcs.get(&BUILTIN_CONSOLE_LOG_F64).unwrap(),
                console_log_f64 as *mut libc::c_void,
            );
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self
                    .builtin_funcs
                    .get(&BUILTIN_CONSOLE_LOG_NEWLINE)
                    .unwrap(),
                console_log_newline as *mut libc::c_void,
            );
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self
                    .builtin_funcs
                    .get(&BUILTIN_PROCESS_STDOUT_WRITE)
                    .unwrap(),
                process_stdout_write as *mut libc::c_void,
            );
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self.builtin_funcs.get(&BUILTIN_MATH_POW).unwrap(),
                math_pow as *mut libc::c_void,
            );
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self.builtin_funcs.get(&BUILTIN_MATH_FLOOR).unwrap(),
                math_floor as *mut libc::c_void,
            );
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self.builtin_funcs.get(&BUILTIN_MATH_RANDOM).unwrap(),
                math_random as *mut libc::c_void,
            );
        }
        let f_raw = llvm::execution_engine::LLVMGetFunctionAddress(
            ee,
            CString::new(name.as_str()).unwrap().as_ptr(),
        );
        let f = ::std::mem::transmute::<u64, fn()>(f_raw);

        let info = self.func_info.get_mut(&pc).unwrap();
        info.func_addr = Some(f);
        info.llvm_func = Some(llvm_func);

        Some(f)
    }

    unsafe fn gen_code_for_func(
        &mut self,
        name: String,
        insts: &Vec<u8>,
        scope: &CallObject,
        const_table: &vm::ConstantTable,
        mut pc: usize,
        argc: usize,
    ) -> Result<LLVMValueRef, ()> {
        if argc > MAX_FUNCTION_PARAMS {
            return Err(());
        }

        let func_ret_ty = if let Some(ty) = self.return_ty_map.get(&pc) {
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

        let mut env = HashMap::new();
        self.cur_func = Some(func);

        for i in 0..argc {
            LLVMBuildStore(
                self.builder,
                LLVMGetParam(func, i as u32),
                self.declare_local_var(scope.get_parameter_nth_name(i).unwrap(), &mut env),
            );
        }

        let func_pos = pc;
        pc += 1; // CreateContext
        pc += 4; // |- num_local_var

        let mut compilation_failed = false;
        if let Err(_) = self.gen_body(
            insts,
            scope,
            const_table,
            func_pos,
            pc,
            insts.len(),
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
        insts: &Vec<u8>,
        const_table: &vm::ConstantTable,
        vm_state: &mut vm::VMState,
        end: usize,
    ) -> Option<isize> {
        let bgn = vm_state.pc as usize;

        if !self.loop_is_called_enough_times(bgn) {
            self.inc_count(bgn);
            return None;
        }

        {
            let LoopInfo {
                func_addr,
                local_vars_id,
                jit_info: JITInfo { cannot_jit },
                ..
            } = self.loop_info.entry(bgn).or_insert(LoopInfo::new());
            if *cannot_jit {
                return None;
            }
            if let Some(func_addr) = func_addr {
                return run_loop_llvm_func(*func_addr, vm_state, const_table, local_vars_id.clone());
            }
        }

        let name = format!("loop-{}", random::<u32>());

        // If gen_code fails, it means the function can't be JIT-compiled and should never be
        // compiled. (cannot_jit = true)
        let (llvm_func, local_vars) =
            match self.gen_code_for_loop(name.clone(), vm_state, insts, const_table, bgn, end) {
                Ok(info) => info,
                Err(()) => {
                    self.loop_info.get_mut(&bgn).unwrap().jit_info.cannot_jit = true;
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
            panic!()
        }
        {
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self.builtin_funcs.get(&BUILTIN_CONSOLE_LOG_STRING).unwrap(),
                console_log_string as *mut libc::c_void,
            );
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self.builtin_funcs.get(&BUILTIN_CONSOLE_LOG_BOOL).unwrap(),
                console_log_bool as *mut libc::c_void,
            );
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self.builtin_funcs.get(&BUILTIN_CONSOLE_LOG_F64).unwrap(),
                console_log_f64 as *mut libc::c_void,
            );
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self
                    .builtin_funcs
                    .get(&BUILTIN_CONSOLE_LOG_NEWLINE)
                    .unwrap(),
                console_log_newline as *mut libc::c_void,
            );
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self
                    .builtin_funcs
                    .get(&BUILTIN_PROCESS_STDOUT_WRITE)
                    .unwrap(),
                process_stdout_write as *mut libc::c_void,
            );
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self.builtin_funcs.get(&BUILTIN_MATH_POW).unwrap(),
                math_pow as *mut libc::c_void,
            );
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self.builtin_funcs.get(&BUILTIN_MATH_FLOOR).unwrap(),
                math_floor as *mut libc::c_void,
            );
            llvm::execution_engine::LLVMAddGlobalMapping(
                ee,
                *self.builtin_funcs.get(&BUILTIN_MATH_RANDOM).unwrap(),
                math_random as *mut libc::c_void,
            );
        }
        let f_raw = llvm::execution_engine::LLVMGetFunctionAddress(
            ee,
            CString::new(name.as_str()).unwrap().as_ptr(),
        );
        let f = ::std::mem::transmute::<u64, fn(*mut f64) -> i32>(f_raw);

        let info = self.loop_info.get_mut(&bgn).unwrap();
        info.func_addr = Some(f);
        info.llvm_func = Some(llvm_func);
        info.local_vars_id = local_vars.clone();

        run_loop_llvm_func(f, vm_state, const_table, local_vars)
    }

    unsafe fn gen_code_for_loop(
        &mut self,
        name: String,
        vm_state: &mut vm::VMState,
        insts: &Vec<u8>,
        const_table: &vm::ConstantTable,
        bgn: usize,
        end: usize,
    ) -> Result<(LLVMValueRef, Vec<(usize, ValueType)>), ()> {
        let local_vars = self.collect_local_variables(vm_state, insts, const_table, bgn, end)?;

        let func_ret_ty = LLVMInt32TypeInContext(self.context);
        let func_ty = LLVMFunctionType(
            func_ret_ty,
            vec![LLVMPointerType(
                LLVMPointerType(LLVMInt8TypeInContext(self.context), 0),
                0,
            )].as_mut_slice()
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

        let mut env = HashMap::new();
        self.cur_func = Some(func);

        let arg_0 = LLVMGetParam(func, 0);
        for i in 0..local_vars.len() {
            env.insert(
                const_table.string[local_vars[i].0].clone(),
                LLVMBuildPointerCast(
                    self.builder,
                    LLVMBuildLoad(
                        self.builder,
                        LLVMBuildGEP(
                            self.builder,
                            arg_0,
                            vec![LLVMConstInt(
                                LLVMInt32TypeInContext(self.context),
                                i as u64,
                                0,
                            )].as_mut_slice()
                                .as_mut_ptr(),
                            1,
                            CString::new("").unwrap().as_ptr(),
                        ),
                        CString::new("").unwrap().as_ptr(),
                    ),
                    LLVMPointerType(local_vars[i].1.to_llvmty(self.context), 0),
                    CString::new("").unwrap().as_ptr(),
                ),
            );
        }

        let mut compilation_failed = false;
        if let Err(_) = self.gen_body(
            insts,
            &vm_state.scope.last().unwrap().borrow(),
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
        env: &mut HashMap<String, LLVMValueRef>,
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
        vm_state: &mut vm::VMState,
        insts: &Vec<u8>,
        const_table: &vm::ConstantTable,
        mut pc: usize,
        end: usize,
    ) -> Result<Vec<(usize, ValueType)>, ()> {
        let mut local_vars = HashSet::new();
        let local_scope = &vm_state.scope.last().unwrap().borrow();

        while pc < end {
            let inst_size = try_opt!(VMInst::get_inst_size(insts[pc]));
            match insts[pc] {
                VMInst::DECL_VAR | VMInst::SET_NAME | VMInst::GET_NAME => {
                    pc += 1;
                    get_int32!(insts, pc, id, usize);
                    let name = &const_table.string[id];
                    if let Some(val) = local_scope.vals.borrow().get(name) {
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
        insts: &Vec<u8>,
        scope: &CallObject,
        const_table: &vm::ConstantTable,
        func_pos: usize,
        bgn: usize,
        end: usize,
        is_func_jit: bool,
        env: &mut HashMap<String, LLVMValueRef>,
    ) -> Result<(), ()> {
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
            vm_val: &Option<vm::Value>,
        ) -> Result<ValueType, ()> {
            match vm_val {
                &Some(vm::Value::String(_)) => Ok(ValueType::String),
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

        let func = self.cur_func.unwrap();
        let mut stack: Vec<(LLVMValueRef, Option<vm::Value>)> = vec![];

        let mut labels: HashMap<usize, LabelKind> = HashMap::new();

        // First of all, find JMP-related ops and record its destination.
        {
            let mut pc = bgn;
            while pc < end {
                let inst_size = try_opt!(VMInst::get_inst_size(insts[pc]));
                match insts[pc] {
                    VMInst::END => break,
                    VMInst::CREATE_CONTEXT if is_func_jit => break,
                    VMInst::JMP | VMInst::JMP_IF_FALSE => {
                        pc += 1;
                        get_int32!(insts, pc, dst, i32);
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

        let mut land: Vec<LLVMBasicBlockRef> = Vec::new();
        let mut lcom: Vec<LLVMBasicBlockRef> = Vec::new();
        let mut lor: Vec<LLVMBasicBlockRef> = Vec::new();

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

            match insts[pc] {
                VMInst::END => break,
                VMInst::CREATE_CONTEXT => break,
                VMInst::ASG_FREST_PARAM => pc += 9,
                VMInst::CONSTRUCT
                | VMInst::CREATE_OBJECT
                | VMInst::SET_GLOBAL
                | VMInst::CREATE_ARRAY => pc += 5,
                VMInst::JMP_IF_FALSE => {
                    pc += 1;
                    get_int32!(insts, pc, dst, i32);
                    land.push(LLVMGetInsertBlock(self.builder));
                    let bb_then = LLVMAppendBasicBlock(func, CString::new("").unwrap().as_ptr());
                    lcom.push(bb_then);
                    let bb_else =
                        label_retrieve(try_opt!(labels.get(&((pc as i32 + dst) as usize))));
                    lor.push(bb_else);
                    let cond_val = try_stack!(stack.pop());
                    LLVMBuildCondBr(self.builder, cond_val, bb_then, bb_else);
                    LLVMPositionBuilderAtEnd(self.builder, bb_then);
                }
                VMInst::JMP => {
                    pc += 1;
                    get_int32!(insts, pc, dst, i32);
                    let bb = label_retrieve(try_opt!(labels.get(&((pc as i32 + dst) as usize))));
                    if cur_bb_has_no_terminator(self.builder) {
                        LLVMBuildBr(self.builder, bb);
                    }
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
                        vec![land.pop().unwrap()].as_mut_slice().as_mut_ptr(),
                        1,
                    );
                    LLVMAddIncoming(
                        phi,
                        vec![try_stack!(stack.pop())].as_mut_slice().as_mut_ptr(),
                        vec![lcom.pop().unwrap()].as_mut_slice().as_mut_ptr(),
                        1,
                    );
                    lor.pop();
                    if let Some(x) = lor.last_mut() {
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
                        vec![lcom.pop().unwrap()].as_mut_slice().as_mut_ptr(),
                        1,
                    );
                    LLVMAddIncoming(
                        phi,
                        vec![try_stack!(stack.pop())].as_mut_slice().as_mut_ptr(),
                        vec![lor.pop().unwrap()].as_mut_slice().as_mut_ptr(),
                        1,
                    );
                    land.pop();
                    if let Some(x) = lcom.last_mut() {
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
                VMInst::GET_NAME => {
                    pc += 1;
                    get_int32!(insts, pc, id, usize);
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
                        None => match scope.get_value(name) {
                            vm::Value::Function(pos, _, _) if pos == func_pos => {
                                stack.push((func, None));
                            }
                            vm::Value::Function(pos, _, _) => match self.func_info.get(&pos) {
                                Some(FuncInfo { llvm_func, .. }) if llvm_func.is_some() => {
                                    stack.push((llvm_func.unwrap(), None));
                                }
                                _ => return Err(()),
                            },
                            vm::Value::BuiltinFunction(n, _) => match self.builtin_funcs.get(&n) {
                                Some(f) => stack.push((*f, None)),
                                _ => return Err(()),
                            },
                            vm::Value::Object(obj) => {
                                stack.push((ptr::null_mut(), Some(vm::Value::Object(obj))))
                            }
                            _ => return Err(()),
                        },
                    }
                }
                VMInst::SET_NAME => {
                    pc += 1;
                    get_int32!(insts, pc, id, usize);
                    let name = &const_table.string[id];
                    let val = try_stack!(stack.pop());
                    LLVMBuildStore(self.builder, val, *try_opt!(env.get(name)));
                }
                VMInst::DECL_VAR => {
                    pc += 1;
                    get_int32!(insts, pc, id, usize);
                    let name = const_table.string[id].clone();
                    let dst = self.declare_local_var(name, env);
                    let src = try_stack!(stack.pop());
                    LLVMBuildStore(self.builder, src, dst);
                }
                VMInst::CALL => {
                    pc += 1;
                    get_int32!(insts, pc, argc, usize);

                    let callee = try_opt!(stack.pop());

                    if let Some(callee) = callee.1 {
                        let mut args = vec![];
                        for _ in 0..argc {
                            let arg = try_opt!(stack.pop());
                            args.push((arg.0, infer_ty(arg.0, &arg.1)?));
                        }
                        match callee {
                            vm::Value::BuiltinFunction(builtin::CONSOLE_LOG, _) => {
                                for (arg, ty) in args {
                                    LLVMBuildCall(
                                        self.builder,
                                        *self
                                            .builtin_funcs
                                            .get(&match ty {
                                                ValueType::Number => BUILTIN_CONSOLE_LOG_F64,
                                                ValueType::Bool => BUILTIN_CONSOLE_LOG_BOOL,
                                                ValueType::String => BUILTIN_CONSOLE_LOG_STRING,
                                            })
                                            .unwrap(),
                                        vec![arg].as_mut_ptr(),
                                        1,
                                        CString::new("").unwrap().as_ptr(),
                                    );
                                }
                                LLVMBuildCall(
                                    self.builder,
                                    *self
                                        .builtin_funcs
                                        .get(&BUILTIN_CONSOLE_LOG_NEWLINE)
                                        .unwrap(),
                                    vec![].as_mut_ptr(),
                                    0,
                                    CString::new("").unwrap().as_ptr(),
                                );
                            }
                            vm::Value::BuiltinFunction(builtin::PROCESS_STDOUT_WRITE, _) => {
                                for (arg, ty) in args {
                                    match ty {
                                        ValueType::String => LLVMBuildCall(
                                            self.builder,
                                            *self
                                                .builtin_funcs
                                                .get(&BUILTIN_PROCESS_STDOUT_WRITE)
                                                .unwrap(),
                                            vec![arg].as_mut_ptr(),
                                            1,
                                            CString::new("").unwrap().as_ptr(),
                                        ),
                                        _ => return Err(()),
                                    };
                                }
                            }
                            vm::Value::BuiltinFunction(builtin::MATH_FLOOR, _) => stack.push((
                                LLVMBuildCall(
                                    self.builder,
                                    *self.builtin_funcs.get(&BUILTIN_MATH_FLOOR).unwrap(),
                                    args.iter()
                                        .map(|(x, _)| *x)
                                        .collect::<Vec<LLVMValueRef>>()
                                        .as_mut_ptr(),
                                    1,
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                None,
                            )),
                            vm::Value::BuiltinFunction(builtin::MATH_RANDOM, _) => stack.push((
                                LLVMBuildCall(
                                    self.builder,
                                    *self.builtin_funcs.get(&BUILTIN_MATH_RANDOM).unwrap(),
                                    args.iter()
                                        .map(|(x, _)| *x)
                                        .collect::<Vec<LLVMValueRef>>()
                                        .as_mut_ptr(),
                                    0,
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                None,
                            )),
                            vm::Value::BuiltinFunction(builtin::MATH_POW, _) => stack.push((
                                LLVMBuildCall(
                                    self.builder,
                                    *self.builtin_funcs.get(&BUILTIN_MATH_POW).unwrap(),
                                    args.iter()
                                        .map(|(x, _)| *x)
                                        .collect::<Vec<LLVMValueRef>>()
                                        .as_mut_ptr(),
                                    2,
                                    CString::new("").unwrap().as_ptr(),
                                ),
                                None,
                            )),
                            _ => return Err(()),
                        }
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
                    match parent {
                        vm::Value::Object(map) => stack.push((
                            ptr::null_mut(),
                            Some(vm::obj_find_val(
                                &*map.borrow(),
                                member.to_string().as_str(),
                            )),
                        )),
                        _ => return Err(()),
                    }
                }
                VMInst::PUSH_CONST => {
                    pc += 1;
                    get_int32!(insts, pc, n, usize);
                    match const_table.value[n] {
                        vm::Value::Bool(false) => stack.push((
                            LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0),
                            None,
                        )),
                        vm::Value::Bool(true) => stack.push((
                            LLVMConstInt(LLVMInt1TypeInContext(self.context), 1, 0),
                            None,
                        )),
                        vm::Value::Number(n) => stack.push((
                            LLVMConstReal(LLVMDoubleTypeInContext(self.context), n as f64),
                            None,
                        )),
                        vm::Value::Function(pos, _, _) if is_func_jit && pos == func_pos => {
                            stack.push((func, None))
                        }
                        vm::Value::Function(pos, _, _) => stack.push((
                            match self.func_info.get(&pos) {
                                Some(FuncInfo { llvm_func, .. }) if llvm_func.is_some() => {
                                    llvm_func.unwrap()
                                }
                                _ => return Err(()),
                            },
                            None,
                        )),
                        vm::Value::String(ref s) => stack.push((
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
                        vm::Value::Object(_) => {
                            stack.push((ptr::null_mut(), Some(const_table.value[n].clone())))
                        }
                        vm::Value::BuiltinFunction(n, _) => stack.push((
                            if let Some(f) = self.builtin_funcs.get(&n) {
                                *f
                            } else {
                                return Err(());
                            },
                            None,
                        )),
                        _ => return Err(()),
                    }
                }
                VMInst::PUSH_INT8 => {
                    pc += 1;
                    get_int8!(insts, pc, n, isize);
                    stack.push((
                        LLVMConstReal(LLVMDoubleTypeInContext(self.context), n as f64),
                        None,
                    ));
                }
                VMInst::PUSH_INT32 => {
                    pc += 1;
                    get_int32!(insts, pc, n, isize);
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
                VMInst::GET_GLOBAL => pc += 5,
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

    pub fn register_return_type(&mut self, pc: usize, val: &vm::Value) {
        match val {
            &vm::Value::Number(_) => self.return_ty_map.insert(pc, ValueType::Number),
            &vm::Value::Bool(_) => self.return_ty_map.insert(pc, ValueType::Bool),
            _ => None,
        };
    }

    pub unsafe fn run_llvm_func(&mut self, pc: usize, f: fn(), args: Vec<vm::Value>) -> vm::Value {
        let mut llvm_args = vec![];
        for arg in args {
            llvm_args.push(match arg {
                vm::Value::Number(f) => f,
                _ => unimplemented!(),
            });
        }

        let func_ret_ty = self.return_ty_map.get(&pc).unwrap_or(&ValueType::Number);

        // Because of a bug of LLVM, llvm::execution_engine::runFunction can not be used.
        // So, all I can do is this:
        // TODO: MAX_FUNCTION_PARAMS is too small?
        match func_ret_ty {
            &ValueType::Number => vm::Value::Number(match llvm_args.len() {
                0 => ::std::mem::transmute::<fn(), fn() -> f64>(f)(),
                1 => ::std::mem::transmute::<fn(), fn(f64) -> f64>(f)(llvm_args[0]),
                2 => ::std::mem::transmute::<fn(), fn(f64, f64) -> f64>(f)(
                    llvm_args[0],
                    llvm_args[1],
                ),
                3 => ::std::mem::transmute::<fn(), fn(f64, f64, f64) -> f64>(f)(
                    llvm_args[0],
                    llvm_args[1],
                    llvm_args[2],
                ),
                _ => unimplemented!("should be implemented.."),
            }),
            &ValueType::Bool => vm::Value::Bool(match llvm_args.len() {
                0 => ::std::mem::transmute::<fn(), fn() -> bool>(f)(),
                1 => ::std::mem::transmute::<fn(), fn(f64) -> bool>(f)(llvm_args[0]),
                2 => ::std::mem::transmute::<fn(), fn(f64, f64) -> bool>(f)(
                    llvm_args[0],
                    llvm_args[1],
                ),
                3 => ::std::mem::transmute::<fn(), fn(f64, f64, f64) -> bool>(f)(
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
    vm_state: &mut vm::VMState,
    const_table: &vm::ConstantTable,
    local_vars: Vec<(usize, ValueType)>,
) -> Option<isize> {
    let mut scope = vm_state.scope.last().unwrap().borrow_mut();
    let mut args_of_local_vars = vec![];

    for (id, _) in &local_vars {
        let name = &const_table.string[*id];
        args_of_local_vars.push(match scope.get_value(name) {
            vm::Value::Number(f) => {
                let p = libc::calloc(1, ::std::mem::size_of::<f64>()) as *mut f64;
                *p = f;
                p as *mut libc::c_void
            }
            vm::Value::Bool(b) => {
                let p = libc::calloc(1, ::std::mem::size_of::<bool>()) as *mut bool;
                *p = b;
                p as *mut libc::c_void
            }
            _ => return None,
        });
    }

    // println!("before: farg[{:?}] local[{:?}]", args_of_arg_vars, args_of_local_vars);
    let pc = ::std::mem::transmute::<fn(*mut f64) -> i32, fn(*mut *mut libc::c_void) -> i32>(f)(
        args_of_local_vars.as_mut_slice().as_mut_ptr(),
    );
    // println!("after:  farg[{:?}] local[{:?}]", args_of_arg_vars, args_of_local_vars);

    for (i, (id, ty)) in local_vars.iter().enumerate() {
        let name = const_table.string[*id].clone();
        scope.set_value_if_exist(
            name,
            match ty {
                ValueType::Number => vm::Value::Number(*(args_of_local_vars[i] as *mut f64)),
                ValueType::Bool => vm::Value::Bool(*(args_of_local_vars[i] as *mut bool)),
                _ => unimplemented!(),
            },
        );
        libc::free(args_of_local_vars[i]);
    }

    Some(pc as isize)
}

impl TracingJit {
    #[inline]
    fn func_is_called_enough_times(&mut self, pc: usize) -> bool {
        *self.count.entry(pc).or_insert(0) >= 5
    }

    #[inline]
    fn loop_is_called_enough_times(&mut self, pc: usize) -> bool {
        *self.count.entry(pc).or_insert(0) >= 7
    }

    #[inline]
    fn inc_count(&mut self, pc: usize) {
        *self.count.entry(pc).or_insert(0) += 1;
    }
}

// Builtin functions

const BUILTIN_CONSOLE_LOG_F64: usize = 0;
const BUILTIN_CONSOLE_LOG_BOOL: usize = 1;
const BUILTIN_CONSOLE_LOG_STRING: usize = 2;
const BUILTIN_CONSOLE_LOG_NEWLINE: usize = 3;
const BUILTIN_PROCESS_STDOUT_WRITE: usize = 4;
const BUILTIN_MATH_POW: usize = 5;
const BUILTIN_MATH_FLOOR: usize = 6;
const BUILTIN_MATH_RANDOM: usize = 7;

#[no_mangle]
pub extern "C" fn console_log_string(s: vm::RawStringPtr) {
    unsafe {
        libc::printf(b"%s \0".as_ptr() as vm::RawStringPtr, s);
    }
}

#[no_mangle]
pub extern "C" fn console_log_bool(b: bool) {
    unsafe {
        if b {
            libc::printf(b"true \0".as_ptr() as vm::RawStringPtr);
        } else {
            libc::printf(b"false \0".as_ptr() as vm::RawStringPtr);
        }
    }
}

#[no_mangle]
pub extern "C" fn console_log_f64(n: f64) {
    unsafe {
        libc::printf(b"%.15g \0".as_ptr() as vm::RawStringPtr, n);
    }
}

#[no_mangle]
pub extern "C" fn console_log_newline() {
    unsafe {
        libc::printf(b"\n\0".as_ptr() as vm::RawStringPtr);
    }
}

#[no_mangle]
pub extern "C" fn process_stdout_write(s: vm::RawStringPtr) {
    unsafe {
        libc::printf(b"%s\0".as_ptr() as vm::RawStringPtr, s);
    }
}

#[no_mangle]
pub extern "C" fn math_floor(n: f64) -> f64 {
    n.floor()
}

// TODO: Find a better way for rand gen. (rand::random is slow)
static mut MATH_RAND_SEED: u64 = 0xf6d582196d588cac;
#[no_mangle]
pub extern "C" fn math_random() -> f64 {
    unsafe {
        MATH_RAND_SEED = MATH_RAND_SEED ^ (MATH_RAND_SEED << 13);
        MATH_RAND_SEED = MATH_RAND_SEED ^ (MATH_RAND_SEED >> 17);
        MATH_RAND_SEED = MATH_RAND_SEED ^ (MATH_RAND_SEED << 5);
        (MATH_RAND_SEED as f64) / ::std::u64::MAX as f64
    }
}

#[no_mangle]
pub extern "C" fn math_pow(x: f64, y: f64) -> f64 {
    x.powf(y)
}
