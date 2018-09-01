use vm;
use vm::{
    PUSH_INT32, PUSH_INT8, ADD, ASG_FREST_PARAM, CALL, CONSTRUCT, CREATE_ARRAY, CREATE_CONTEXT,
    CREATE_OBJECT, DIV, END, EQ, GE, GET_ARG_LOCAL, GET_GLOBAL, GET_LOCAL, GET_MEMBER, GT, JMP,
    JMP_IF_FALSE, LE, LT, MUL, NE, NEG, PUSH_ARGUMENTS, PUSH_CONST, PUSH_FALSE, PUSH_THIS,
    PUSH_TRUE, REM, RETURN, SET_ARG_LOCAL, SET_GLOBAL, SET_LOCAL, SET_MEMBER, SUB,
};

use rand::random;

use std::collections::{HashMap, HashSet};

// use libc;
use llvm;
use llvm::core::*;
use llvm::prelude::*;

use std::ffi::CString;
use std::ptr;

const MAX_FUNCTION_PARAMS: usize = 3;

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Number,
    Bool,
}

trait CastIntoLLVMType {
    unsafe fn to_llvmty(&self, LLVMContextRef) -> LLVMTypeRef;
}

impl CastIntoLLVMType for ValueType {
    unsafe fn to_llvmty(&self, ctx: LLVMContextRef) -> LLVMTypeRef {
        match self {
            &ValueType::Number => LLVMDoubleTypeInContext(ctx),
            &ValueType::Bool => LLVMInt1TypeInContext(ctx),
        }
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

#[derive(Debug, Clone)]
pub struct JITInfo {
    pub cannot_jit: bool,
}

#[derive(Debug, Clone)]
pub struct LoopInfo {
    func_addr: Option<fn(*mut f64, *mut f64) -> i32>,
    llvm_func: Option<LLVMValueRef>,
    arg_vars_id: Vec<usize>, // the ids of argument variables used in this loop
    local_vars_id: Vec<usize>, // the ids of local variables used in this loop
    jit_info: JITInfo,
}

impl LoopInfo {
    pub fn new() -> LoopInfo {
        LoopInfo {
            func_addr: None,
            llvm_func: None,
            arg_vars_id: vec![],
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
                let hmap = HashMap::new();
                // let f_math_floor = LLVMAddFunction(
                //     module,
                //     CString::new("math_floor").unwrap().as_ptr(),
                //     LLVMFunctionType(
                //         LLVMDoubleType(),
                //         vec![LLVMDoubleType()].as_mut_slice().as_mut_ptr(),
                //         1,
                //         0,
                //     ),
                // );
                // llvm::execution_engine::LLVMAddGlobalMapping(
                //     ee,
                //     f_math_floor,
                //     math_floor as *mut libc::c_void,
                // );
                // hmap.insert(3, f_math_floor);
                //
                // let f_math_random = LLVMAddFunction(
                //     module,
                //     CString::new("math_random").unwrap().as_ptr(),
                //     LLVMFunctionType(LLVMDoubleType(), vec![].as_mut_slice().as_mut_ptr(), 0, 0),
                // );
                // llvm::execution_engine::LLVMAddGlobalMapping(
                //     ee,
                //     f_math_random,
                //     math_random as *mut libc::c_void,
                // );
                // hmap.insert(4, f_math_random);
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

        let name = format!("func.{}", random::<u32>());

        // If gen_code fails, it means the function can't be JIT-compiled and should never be
        // compiled. (cannot_jit = true)
        // llvm::execution_engine::LLVMAddModule(self.exec_engine, self.module);
        let llvm_func = match self.gen_code(name.clone(), insts, const_table, pc, argc) {
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

        // By a bug of LLVM, llvm::execution_engine::runFunction can not be used.
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
        }
    }

    unsafe fn gen_code(
        &mut self,
        name: String,
        insts: &Vec<u8>,
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
                self.declare_local_var(i, true, &mut env),
            );
        }

        let func_pos = pc;
        pc += 1; // CreateContext
        pc += 4; // |- num_local_var

        let mut compilation_failed = false;
        if let Err(_) = self.gen(insts, const_table, func_pos, pc, &mut env) {
            compilation_failed = true;
        }

        let mut iter_bb = LLVMGetFirstBasicBlock(func);
        while iter_bb != ptr::null_mut() {
            if LLVMIsATerminatorInst(LLVMGetLastInstruction(iter_bb)) == ptr::null_mut() {
                let terminator_builder = LLVMCreateBuilderInContext(self.context);
                LLVMPositionBuilderAtEnd(terminator_builder, iter_bb);
                LLVMBuildRet(
                    terminator_builder,
                    LLVMConstNull(LLVMDoubleTypeInContext(self.context)),
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

        Ok(func)
    }

    unsafe fn declare_local_var(
        &mut self,
        id: usize,
        is_param: bool,
        env: &mut HashMap<(usize, bool), LLVMValueRef>,
    ) -> LLVMValueRef {
        if let Some(v) = env.get(&(id, is_param)) {
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
        env.insert((id, is_param), var);
        var
    }

    unsafe fn gen(
        &mut self,
        insts: &Vec<u8>,
        const_table: &vm::ConstantTable,
        func_pos: usize,
        mut pc: usize,
        env: &mut HashMap<(usize, bool), LLVMValueRef>,
    ) -> Result<(), ()> {
        let func = self.cur_func.unwrap();
        let mut stack: Vec<LLVMValueRef> = vec![];

        let mut labels: HashMap<usize, LLVMBasicBlockRef> = HashMap::new();
        // First of all, find JMP-related ops and record its destination.
        {
            let mut pc = pc;
            while pc < insts.len() {
                match insts[pc] {
                    END => break,
                    CREATE_CONTEXT => break,
                    RETURN => pc += 1,
                    ASG_FREST_PARAM => pc += 9,
                    CONSTRUCT | CREATE_OBJECT | PUSH_CONST | PUSH_INT32 | SET_GLOBAL
                    | GET_LOCAL | SET_ARG_LOCAL | GET_ARG_LOCAL | CREATE_ARRAY | SET_LOCAL
                    | CALL => pc += 5,
                    JMP | JMP_IF_FALSE => {
                        pc += 1;
                        get_int32!(insts, pc, dst, i32);
                        // println!("pc: {}, dst: {}, = {}", pc, dst, pc as i32 + dst);
                        labels.insert(
                            (pc as i32 + dst) as usize,
                            LLVMAppendBasicBlock(func, CString::new("").unwrap().as_ptr()),
                        );
                    }
                    PUSH_INT8 => pc += 2,
                    PUSH_FALSE | PUSH_TRUE | PUSH_THIS | ADD | SUB | MUL | DIV | REM | LT
                    | PUSH_ARGUMENTS | NEG | GT | LE | GE | EQ | NE | GET_MEMBER | SET_MEMBER => {
                        pc += 1
                    }
                    GET_GLOBAL => pc += 5,
                    _ => return Err(()),
                }
            }
        }

        while pc < insts.len() {
            if let Some(bb) = labels.get(&pc) {
                if cur_bb_has_no_terminator(self.builder) {
                    LLVMBuildBr(self.builder, *bb);
                }
                LLVMPositionBuilderAtEnd(self.builder, *bb);
            }

            match insts[pc] {
                END => break,
                CREATE_CONTEXT => break,
                ASG_FREST_PARAM => pc += 9,
                CONSTRUCT | CREATE_OBJECT | SET_GLOBAL | CREATE_ARRAY => pc += 5,
                JMP_IF_FALSE => {
                    pc += 1;
                    get_int32!(insts, pc, dst, i32);
                    let bb_then = LLVMAppendBasicBlock(func, CString::new("").unwrap().as_ptr());
                    let bb_else = try_opt!(labels.get(&((pc as i32 + dst) as usize)));
                    let cond_val = try_opt!(stack.pop());
                    LLVMBuildCondBr(self.builder, cond_val, bb_then, *bb_else);
                    LLVMPositionBuilderAtEnd(self.builder, bb_then);
                }
                JMP => {
                    pc += 1;
                    get_int32!(insts, pc, dst, i32);
                    let bb = try_opt!(labels.get(&((pc as i32 + dst) as usize)));
                    if cur_bb_has_no_terminator(self.builder) {
                        LLVMBuildBr(self.builder, *bb);
                    }
                }
                ADD => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFAdd(
                        self.builder,
                        lhs,
                        rhs,
                        CString::new("fadd").unwrap().as_ptr(),
                    ));
                }
                SUB => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFSub(
                        self.builder,
                        lhs,
                        rhs,
                        CString::new("fsub").unwrap().as_ptr(),
                    ));
                }
                MUL => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFMul(
                        self.builder,
                        lhs,
                        rhs,
                        CString::new("fmul").unwrap().as_ptr(),
                    ));
                }
                DIV => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFDiv(
                        self.builder,
                        lhs,
                        rhs,
                        CString::new("fdiv").unwrap().as_ptr(),
                    ));
                }
                REM => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildSIToFP(
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
                    ));
                }
                LT => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFCmp(
                        self.builder,
                        llvm::LLVMRealPredicate::LLVMRealOLT,
                        lhs,
                        rhs,
                        CString::new("flt").unwrap().as_ptr(),
                    ))
                }
                LE => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFCmp(
                        self.builder,
                        llvm::LLVMRealPredicate::LLVMRealOLE,
                        lhs,
                        rhs,
                        CString::new("fle").unwrap().as_ptr(),
                    ))
                }
                GT => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFCmp(
                        self.builder,
                        llvm::LLVMRealPredicate::LLVMRealOGT,
                        lhs,
                        rhs,
                        CString::new("fgt").unwrap().as_ptr(),
                    ))
                }
                GE => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFCmp(
                        self.builder,
                        llvm::LLVMRealPredicate::LLVMRealOGE,
                        lhs,
                        rhs,
                        CString::new("fge").unwrap().as_ptr(),
                    ))
                }
                EQ => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFCmp(
                        self.builder,
                        llvm::LLVMRealPredicate::LLVMRealOEQ,
                        lhs,
                        rhs,
                        CString::new("feq").unwrap().as_ptr(),
                    ));
                }
                NE => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFCmp(
                        self.builder,
                        llvm::LLVMRealPredicate::LLVMRealONE,
                        lhs,
                        rhs,
                        CString::new("fne").unwrap().as_ptr(),
                    ));
                }
                NEG => {
                    pc += 1;
                    let val = try_opt!(stack.pop());
                    stack.push(LLVMBuildFNeg(
                        self.builder,
                        val,
                        CString::new("fneg").unwrap().as_ptr(),
                    ));
                }
                GET_ARG_LOCAL => {
                    pc += 1;
                    get_int32!(insts, pc, n, usize);
                    stack.push(LLVMBuildLoad(
                        self.builder,
                        *try_opt!(env.get(&(n, true))),
                        CString::new("").unwrap().as_ptr(),
                    ));
                }
                // Rarely used?
                SET_ARG_LOCAL => {
                    pc += 1;
                    get_int32!(insts, pc, n, usize);
                    let src = try_opt!(stack.pop());
                    LLVMBuildStore(self.builder, src, *try_opt!(env.get(&(n, true))));
                }
                GET_LOCAL => {
                    pc += 1;
                    get_int32!(insts, pc, n, usize);
                    stack.push(LLVMBuildLoad(
                        self.builder,
                        self.declare_local_var(n, false, env),
                        CString::new("").unwrap().as_ptr(),
                    ));
                }
                SET_LOCAL => {
                    pc += 1;
                    get_int32!(insts, pc, n, usize);
                    let src = try_opt!(stack.pop());
                    LLVMBuildStore(self.builder, src, self.declare_local_var(n, false, env));
                }
                CALL => {
                    pc += 1;
                    get_int32!(insts, pc, argc, usize);
                    let callee = try_opt!(stack.pop());
                    let mut llvm_args = vec![];
                    for _ in 0..argc {
                        llvm_args.push(try_opt!(stack.pop()));
                    }
                    llvm_args.reverse();
                    stack.push(LLVMBuildCall(
                        self.builder,
                        callee,
                        llvm_args.as_mut_ptr(),
                        llvm_args.len() as u32,
                        CString::new("").unwrap().as_ptr(),
                    ));
                }
                PUSH_CONST => {
                    pc += 1;
                    get_int32!(insts, pc, n, usize);
                    stack.push(match const_table.value[n] {
                        vm::Value::Bool(false) => {
                            LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0)
                        }
                        vm::Value::Bool(true) => {
                            LLVMConstInt(LLVMInt1TypeInContext(self.context), 1, 0)
                        }
                        vm::Value::Number(n) => {
                            LLVMConstReal(LLVMDoubleTypeInContext(self.context), n as f64)
                        }
                        vm::Value::Function(pos, _) if pos == func_pos => func,
                        vm::Value::Function(pos, _) => match self.func_info.get(&pos) {
                            Some(FuncInfo { llvm_func, .. }) if llvm_func.is_some() => {
                                llvm_func.unwrap()
                            }
                            _ => return Err(()),
                        },
                        // TODO: Currently, this is unreachable because GET_MEMBER is
                        // unimplemented. (Most builtin/embedded functions involve it.)
                        vm::Value::EmbeddedFunction(n) => {
                            if let Some(f) = self.builtin_funcs.get(&n) {
                                *f
                            } else {
                                return Err(());
                            }
                        }
                        _ => return Err(()),
                    });
                }
                PUSH_INT8 => {
                    pc += 1;
                    get_int8!(insts, pc, n, isize);
                    stack.push(LLVMConstReal(
                        LLVMDoubleTypeInContext(self.context),
                        n as f64,
                    ));
                }
                PUSH_INT32 => {
                    pc += 1;
                    get_int32!(insts, pc, n, isize);
                    stack.push(LLVMConstReal(
                        LLVMDoubleTypeInContext(self.context),
                        n as f64,
                    ));
                }
                PUSH_TRUE => {
                    pc += 1;
                    stack.push(LLVMConstInt(LLVMInt1TypeInContext(self.context), 1, 0));
                }
                PUSH_FALSE => {
                    pc += 1;
                    stack.push(LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0));
                }
                PUSH_THIS | PUSH_ARGUMENTS | GET_MEMBER | SET_MEMBER => pc += 1,
                RETURN => {
                    pc += 1;
                    let val = try_opt!(stack.pop());
                    LLVMBuildRet(self.builder, val);
                }
                GET_GLOBAL => pc += 5,
                _ => return Err(()),
            }
        }

        Ok(())
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
                arg_vars_id,
                local_vars_id,
                jit_info: JITInfo { cannot_jit },
                ..
            } = self.loop_info.entry(bgn).or_insert(LoopInfo::new());
            if *cannot_jit {
                return None;
            }
            if let Some(func_addr) = func_addr {
                return run_loop_llvm_func(
                    *func_addr,
                    vm_state,
                    arg_vars_id.clone(),
                    local_vars_id.clone(),
                );
            }
        }

        let name = format!("func.{}", random::<u32>());

        // If gen_code fails, it means the function can't be JIT-compiled and should never be
        // compiled. (cannot_jit = true)
        let (llvm_func, arg_vars, local_vars) =
            match self.gen_loop_code(name.clone(), insts, const_table, bgn, end) {
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
        let f_raw = llvm::execution_engine::LLVMGetFunctionAddress(
            ee,
            CString::new(name.as_str()).unwrap().as_ptr(),
        );
        let f = ::std::mem::transmute::<u64, fn(*mut f64, *mut f64) -> i32>(f_raw);

        let info = self.loop_info.get_mut(&bgn).unwrap();
        info.func_addr = Some(f);
        info.llvm_func = Some(llvm_func);
        info.arg_vars_id = arg_vars.clone();
        info.local_vars_id = local_vars.clone();

        run_loop_llvm_func(f, vm_state, arg_vars, local_vars)
    }

    unsafe fn gen_loop_code(
        &mut self,
        name: String,
        insts: &Vec<u8>,
        const_table: &vm::ConstantTable,
        bgn: usize,
        end: usize,
    ) -> Result<(LLVMValueRef, Vec<usize>, Vec<usize>), ()> {
        let (arg_vars, local_vars) = self.collect_local_var(insts, bgn, end)?;

        let func_ret_ty = LLVMInt32TypeInContext(self.context);
        let func_ty = LLVMFunctionType(
            func_ret_ty,
            vec![
                LLVMPointerType(LLVMDoubleTypeInContext(self.context), 0),
                LLVMPointerType(LLVMDoubleTypeInContext(self.context), 0),
            ].as_mut_slice()
                .as_mut_ptr(),
            2,
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
        for i in 0..arg_vars.len() {
            env.insert(
                (arg_vars[i], true),
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
            );
        }
        let arg_1 = LLVMGetParam(func, 1);
        for i in 0..local_vars.len() {
            env.insert(
                (local_vars[i], false),
                LLVMBuildGEP(
                    self.builder,
                    arg_1,
                    vec![LLVMConstInt(
                        LLVMInt32TypeInContext(self.context),
                        i as u64,
                        0,
                    )].as_mut_slice()
                        .as_mut_ptr(),
                    1,
                    CString::new("").unwrap().as_ptr(),
                ),
            );
        }

        let mut compilation_failed = false;
        if let Err(_) = self.gen_loop(insts, const_table, bgn, end, &mut env) {
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

        Ok((func, arg_vars, local_vars))
    }

    unsafe fn collect_local_var(
        &mut self,
        insts: &Vec<u8>,
        mut pc: usize,
        end: usize,
    ) -> Result<(Vec<usize>, Vec<usize>), ()> {
        let mut arg_vars = HashSet::new();
        let mut local_vars = HashSet::new();

        while pc < end {
            match insts[pc] {
                END => pc += 1,
                CREATE_CONTEXT => pc += 5,
                RETURN => pc += 1,
                ASG_FREST_PARAM => pc += 9,
                CONSTRUCT | CREATE_OBJECT | PUSH_CONST | PUSH_INT32 | SET_GLOBAL | CREATE_ARRAY
                | CALL => pc += 5,
                SET_ARG_LOCAL | GET_ARG_LOCAL => {
                    pc += 1;
                    get_int32!(insts, pc, id, usize);
                    arg_vars.insert(id);
                }
                GET_LOCAL | SET_LOCAL => {
                    pc += 1;
                    get_int32!(insts, pc, id, usize);
                    local_vars.insert(id);
                }
                JMP | JMP_IF_FALSE => pc += 5,
                PUSH_INT8 => pc += 2,
                PUSH_FALSE | PUSH_TRUE | PUSH_THIS | ADD | SUB | MUL | DIV | REM | LT
                | PUSH_ARGUMENTS | NEG | GT | LE | GE | EQ | NE | GET_MEMBER | SET_MEMBER => {
                    pc += 1
                }
                GET_GLOBAL => pc += 5,
                _ => return Err(()),
            }
        }

        Ok((
            arg_vars.iter().map(|x| *x).collect(),
            local_vars.iter().map(|x| *x).collect(),
        ))
    }

    unsafe fn gen_loop(
        &mut self,
        insts: &Vec<u8>,
        const_table: &vm::ConstantTable,
        bgn: usize,
        end: usize,
        env: &mut HashMap<(usize, bool), LLVMValueRef>,
    ) -> Result<(), ()> {
        let func = self.cur_func.unwrap();
        let mut stack: Vec<LLVMValueRef> = vec![];

        let mut labels: HashMap<usize, LLVMBasicBlockRef> = HashMap::new();
        let mut positioned_labels: HashSet<usize> = HashSet::new();
        // First of all, find JMP-related ops and record its destination.
        {
            let mut pc = bgn;
            while pc < end {
                match insts[pc] {
                    END => pc += 1,
                    CREATE_CONTEXT => pc += 5,
                    RETURN => pc += 1,
                    ASG_FREST_PARAM => pc += 9,
                    CONSTRUCT | CREATE_OBJECT | PUSH_CONST | PUSH_INT32 | SET_GLOBAL
                    | GET_LOCAL | SET_ARG_LOCAL | GET_ARG_LOCAL | CREATE_ARRAY | SET_LOCAL
                    | CALL => pc += 5,
                    JMP | JMP_IF_FALSE => {
                        pc += 1;
                        get_int32!(insts, pc, dst, i32);
                        // println!("pc: {}, dst: {}, = {}", pc, dst, pc as i32 + dst);
                        labels.insert(
                            (pc as i32 + dst) as usize,
                            LLVMAppendBasicBlock(func, CString::new("").unwrap().as_ptr()),
                        );
                    }
                    PUSH_INT8 => pc += 2,
                    PUSH_FALSE | PUSH_TRUE | PUSH_THIS | ADD | SUB | MUL | DIV | REM | LT
                    | PUSH_ARGUMENTS | NEG | GT | LE | GE | EQ | NE | GET_MEMBER | SET_MEMBER => {
                        pc += 1
                    }
                    GET_GLOBAL => pc += 5,
                    _ => return Err(()),
                }
            }
        }

        let mut pc = bgn;
        while pc < end {
            if let Some(bb) = labels.get(&pc) {
                if cur_bb_has_no_terminator(self.builder) {
                    LLVMBuildBr(self.builder, *bb);
                }
                LLVMPositionBuilderAtEnd(self.builder, *bb);
                positioned_labels.insert(pc);
            }

            match insts[pc] {
                END => break,
                CREATE_CONTEXT => break,
                ASG_FREST_PARAM => pc += 9,
                CONSTRUCT | CREATE_OBJECT | SET_GLOBAL | CREATE_ARRAY => pc += 5,
                JMP_IF_FALSE => {
                    pc += 1;
                    get_int32!(insts, pc, dst, i32);
                    let bb_then = LLVMAppendBasicBlock(func, CString::new("").unwrap().as_ptr());
                    let bb_else = try_opt!(labels.get(&((pc as i32 + dst) as usize)));
                    let cond_val = try_opt!(stack.pop());
                    LLVMBuildCondBr(self.builder, cond_val, bb_then, *bb_else);
                    LLVMPositionBuilderAtEnd(self.builder, bb_then);
                }
                JMP => {
                    pc += 1;
                    get_int32!(insts, pc, dst, i32);
                    let bb = try_opt!(labels.get(&((pc as i32 + dst) as usize)));
                    if cur_bb_has_no_terminator(self.builder) {
                        LLVMBuildBr(self.builder, *bb);
                    }
                }
                ADD => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFAdd(
                        self.builder,
                        lhs,
                        rhs,
                        CString::new("fadd").unwrap().as_ptr(),
                    ));
                }
                SUB => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFSub(
                        self.builder,
                        lhs,
                        rhs,
                        CString::new("fsub").unwrap().as_ptr(),
                    ));
                }
                MUL => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFMul(
                        self.builder,
                        lhs,
                        rhs,
                        CString::new("fmul").unwrap().as_ptr(),
                    ));
                }
                DIV => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFDiv(
                        self.builder,
                        lhs,
                        rhs,
                        CString::new("fdiv").unwrap().as_ptr(),
                    ));
                }
                REM => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildSIToFP(
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
                    ));
                }
                LT => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFCmp(
                        self.builder,
                        llvm::LLVMRealPredicate::LLVMRealOLT,
                        lhs,
                        rhs,
                        CString::new("flt").unwrap().as_ptr(),
                    ))
                }
                LE => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFCmp(
                        self.builder,
                        llvm::LLVMRealPredicate::LLVMRealOLE,
                        lhs,
                        rhs,
                        CString::new("fle").unwrap().as_ptr(),
                    ))
                }
                GT => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFCmp(
                        self.builder,
                        llvm::LLVMRealPredicate::LLVMRealOGT,
                        lhs,
                        rhs,
                        CString::new("fgt").unwrap().as_ptr(),
                    ))
                }
                GE => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFCmp(
                        self.builder,
                        llvm::LLVMRealPredicate::LLVMRealOGE,
                        lhs,
                        rhs,
                        CString::new("fge").unwrap().as_ptr(),
                    ))
                }
                EQ => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFCmp(
                        self.builder,
                        llvm::LLVMRealPredicate::LLVMRealOEQ,
                        lhs,
                        rhs,
                        CString::new("feq").unwrap().as_ptr(),
                    ));
                }
                NE => {
                    pc += 1;
                    let rhs = try_opt!(stack.pop());
                    let lhs = try_opt!(stack.pop());
                    stack.push(LLVMBuildFCmp(
                        self.builder,
                        llvm::LLVMRealPredicate::LLVMRealONE,
                        lhs,
                        rhs,
                        CString::new("fne").unwrap().as_ptr(),
                    ));
                }
                NEG => {
                    pc += 1;
                    let val = try_opt!(stack.pop());
                    stack.push(LLVMBuildFNeg(
                        self.builder,
                        val,
                        CString::new("fneg").unwrap().as_ptr(),
                    ));
                }
                GET_ARG_LOCAL => {
                    pc += 1;
                    get_int32!(insts, pc, n, usize);
                    stack.push(LLVMBuildLoad(
                        self.builder,
                        *try_opt!(env.get(&(n, true))),
                        CString::new("").unwrap().as_ptr(),
                    ));
                }
                // Rarely used?
                SET_ARG_LOCAL => {
                    pc += 1;
                    get_int32!(insts, pc, n, usize);
                    let src = try_opt!(stack.pop());
                    LLVMBuildStore(self.builder, src, *try_opt!(env.get(&(n, true))));
                }
                GET_LOCAL => {
                    pc += 1;
                    get_int32!(insts, pc, n, usize);
                    stack.push(LLVMBuildLoad(
                        self.builder,
                        self.declare_local_var(n, false, env),
                        CString::new("").unwrap().as_ptr(),
                    ));
                }
                SET_LOCAL => {
                    pc += 1;
                    get_int32!(insts, pc, n, usize);
                    let src = try_opt!(stack.pop());
                    LLVMBuildStore(self.builder, src, self.declare_local_var(n, false, env));
                }
                CALL => {
                    pc += 1;
                    get_int32!(insts, pc, argc, usize);
                    let callee = try_opt!(stack.pop());
                    let mut llvm_args = vec![];
                    for _ in 0..argc {
                        llvm_args.push(try_opt!(stack.pop()));
                    }
                    llvm_args.reverse();
                    stack.push(LLVMBuildCall(
                        self.builder,
                        callee,
                        llvm_args.as_mut_ptr(),
                        llvm_args.len() as u32,
                        CString::new("").unwrap().as_ptr(),
                    ));
                }
                PUSH_CONST => {
                    pc += 1;
                    get_int32!(insts, pc, n, usize);
                    stack.push(match const_table.value[n] {
                        vm::Value::Bool(false) => {
                            LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0)
                        }
                        vm::Value::Bool(true) => {
                            LLVMConstInt(LLVMInt1TypeInContext(self.context), 1, 0)
                        }
                        vm::Value::Number(n) => {
                            LLVMConstReal(LLVMDoubleTypeInContext(self.context), n as f64)
                        }
                        vm::Value::Function(pos, _) => match self.func_info.get(&pos) {
                            Some(FuncInfo { llvm_func, .. }) if llvm_func.is_some() => {
                                llvm_func.unwrap()
                            }
                            _ => return Err(()),
                        },
                        // TODO: Currently, this is unreachable because GET_MEMBER is
                        // unimplemented. (Most builtin/embedded functions involve it.)
                        vm::Value::EmbeddedFunction(n) => {
                            if let Some(f) = self.builtin_funcs.get(&n) {
                                *f
                            } else {
                                return Err(());
                            }
                        }
                        _ => return Err(()),
                    });
                }
                PUSH_INT8 => {
                    pc += 1;
                    get_int8!(insts, pc, n, isize);
                    stack.push(LLVMConstReal(
                        LLVMDoubleTypeInContext(self.context),
                        n as f64,
                    ));
                }
                PUSH_INT32 => {
                    pc += 1;
                    get_int32!(insts, pc, n, isize);
                    stack.push(LLVMConstReal(
                        LLVMDoubleTypeInContext(self.context),
                        n as f64,
                    ));
                }
                PUSH_TRUE => {
                    pc += 1;
                    stack.push(LLVMConstInt(LLVMInt1TypeInContext(self.context), 1, 0));
                }
                PUSH_FALSE => {
                    pc += 1;
                    stack.push(LLVMConstInt(LLVMInt1TypeInContext(self.context), 0, 0));
                }
                PUSH_THIS | PUSH_ARGUMENTS | GET_MEMBER | SET_MEMBER => pc += 1,
                // RETURN => {
                //     pc += 1;
                //     let val = try_opt!(stack.pop());
                //     LLVMBuildRet(self.builder, val);
                // }
                GET_GLOBAL => pc += 5,
                _ => return Err(()),
            }
        }

        for (pos, bb) in labels {
            if !positioned_labels.contains(&pos) {
                if cur_bb_has_no_terminator(self.builder) {
                    LLVMBuildBr(self.builder, bb);
                }
                LLVMPositionBuilderAtEnd(self.builder, bb);
                LLVMBuildRet(
                    self.builder,
                    LLVMConstInt(LLVMInt32TypeInContext(self.context), pos as u64, 0),
                );
            }
        }

        Ok(())
    }
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

pub unsafe fn run_loop_llvm_func(
    f: fn(*mut f64, *mut f64) -> i32,
    vm_state: &mut vm::VMState,
    arg_vars: Vec<usize>,
    local_vars: Vec<usize>,
) -> Option<isize> {
    let mut args_of_arg_vars = vec![];
    let mut args_of_local_vars = vec![];
    for id in &arg_vars {
        args_of_arg_vars.push(match vm_state.stack[vm_state.bp + id].clone() {
            vm::Value::Number(f) => f,
            _ => return None,
        });
    }
    for id in &local_vars {
        args_of_local_vars.push(match vm_state.stack[vm_state.lp + id].clone() {
            vm::Value::Number(f) => f,
            _ => return None,
        });
    }

    // println!("before: farg[{:?}] local[{:?}]", args_of_arg_vars, args_of_local_vars);
    let pc = f(
        args_of_arg_vars.as_mut_slice().as_mut_ptr(),
        args_of_local_vars.as_mut_slice().as_mut_ptr(),
    );
    // println!("after:  farg[{:?}] local[{:?}]", args_of_arg_vars, args_of_local_vars);

    for (i, id) in arg_vars.iter().enumerate() {
        vm_state.stack[vm_state.bp + id] = vm::Value::Number(args_of_arg_vars[i]);
    }
    for (i, id) in local_vars.iter().enumerate() {
        vm_state.stack[vm_state.lp + id] = vm::Value::Number(args_of_local_vars[i]);
    }

    Some(pc as isize)
}

// Builtin functions

#[no_mangle]
pub extern "C" fn math_floor(n: f64) -> f64 {
    n.floor()
}

#[no_mangle]
pub extern "C" fn math_random() -> f64 {
    random::<f64>()
}
