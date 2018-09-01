use vm;
use vm::{
    PUSH_INT32, PUSH_INT8, ADD, ASG_FREST_PARAM, CALL, CONSTRUCT, CREATE_ARRAY, CREATE_CONTEXT,
    CREATE_OBJECT, DIV, END, EQ, GE, GET_ARG_LOCAL, GET_GLOBAL, GET_LOCAL, GET_MEMBER, GT, JMP,
    JMP_IF_FALSE, LE, LT, MUL, NE, NEG, PUSH_ARGUMENTS, PUSH_CONST, PUSH_FALSE, PUSH_THIS,
    PUSH_TRUE, REM, RETURN, SET_ARG_LOCAL, SET_GLOBAL, SET_LOCAL, SET_MEMBER, SUB,
};
use vm_codegen::FunctionInfoForJIT;

use rand::random;

use std::collections::HashMap;

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
pub struct TracingJit {
    func_pos_in_bytecode_and_its_entity: HashMap<usize, FunctionInfoForJIT>,
    func_pos_in_bytecode_and_its_addr_in_llvm: HashMap<usize, fn()>,
    func_pos_in_bytecode_and_its_llvm_val: HashMap<usize, LLVMValueRef>,
    return_ty_map: HashMap<usize, ValueType>,
    count: HashMap<usize, usize>,
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    pass_manager: LLVMPassManagerRef,
    cur_func: Option<LLVMValueRef>,
    builtin_funcs: HashMap<usize, LLVMValueRef>,
}

impl TracingJit {
    pub unsafe fn new(
        func_pos_in_bytecode_and_its_entity: HashMap<usize, FunctionInfoForJIT>,
    ) -> TracingJit {
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
            func_pos_in_bytecode_and_its_entity: func_pos_in_bytecode_and_its_entity,
            func_pos_in_bytecode_and_its_addr_in_llvm: HashMap::new(),
            func_pos_in_bytecode_and_its_llvm_val: HashMap::new(),
            return_ty_map: HashMap::new(),
            count: HashMap::new(),
            context: context,
            module: module,
            builder: LLVMCreateBuilderInContext(context),
            pass_manager: pm,
            cur_func: None,
            builtin_funcs: {
                let mut hmap = HashMap::new();
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
        if let Some(val) = self.func_pos_in_bytecode_and_its_addr_in_llvm.get(&pc) {
            return Some(val.clone());
        }

        self.inc_times_func_called(pc);

        if self.func_is_called_enough_times(pc) {
            let info = self
                .func_pos_in_bytecode_and_its_entity
                .get(&pc)
                .unwrap()
                .clone();

            if info.cannot_jit {
                return None;
            }

            let name = format!("func.{}", random::<u32>());

            // If gen_code fails, it means the function can't be JIT-compiled and should never be
            // compiled. (cannot_jit = true)
            // llvm::execution_engine::LLVMAddModule(self.exec_engine, self.module);
            match self.gen_code(name.clone(), insts, const_table, pc, argc) {
                Ok(val) => self.func_pos_in_bytecode_and_its_llvm_val.insert(pc, val),
                Err(()) => {
                    self.func_pos_in_bytecode_and_its_entity
                        .get_mut(&pc)
                        .unwrap()
                        .cannot_jit = true;
                    return None;
                }
            };

            LLVMDumpModule(self.module);

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

            self.func_pos_in_bytecode_and_its_addr_in_llvm.insert(pc, f);

            return Some(f);
        }

        None
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
                        vm::Value::Function(pos, _) => {
                            match self.func_pos_in_bytecode_and_its_llvm_val.get(&pos) {
                                Some(val) => *val,
                                None => return Err(()),
                            }
                        }
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
                PUSH_THIS | PUSH_ARGUMENTS | NEG | GET_MEMBER | SET_MEMBER => pc += 1,
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
}

impl TracingJit {
    #[inline]
    fn func_is_called_enough_times(&mut self, pc: usize) -> bool {
        *self.count.entry(pc).or_insert(0) >= 10
    }

    #[inline]
    fn inc_times_func_called(&mut self, pc: usize) {
        *self.count.entry(pc).or_insert(0) += 1;
    }
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
