use node::{BinOp, Node};
use vm;
use vm_codegen::FunctionInfoForJIT;

use std::collections::HashMap;

use llvm;
use llvm::core::*;
use llvm::prelude::*;

use std::ffi::CString;
use std::ptr;

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Number,
    Bool,
}

trait CastIntoLLVMType {
    unsafe fn to_llvmty(&self) -> LLVMTypeRef;
}

impl CastIntoLLVMType for ValueType {
    unsafe fn to_llvmty(&self) -> LLVMTypeRef {
        match self {
            &ValueType::Number => LLVMDoubleType(),
            &ValueType::Bool => LLVMInt1Type(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TracingJit {
    func_addr_in_bytecode_and_its_entity: HashMap<usize, FunctionInfoForJIT>,
    func_addr_in_bytecode_and_its_addr_in_llvm: HashMap<usize, fn(c: *mut f64) -> f64>,
    return_ty_map: HashMap<usize, ValueType>,
    count: HashMap<usize, usize>,
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    exec_engine: llvm::execution_engine::LLVMExecutionEngineRef,
    cur_func: Option<LLVMValueRef>,
}

impl TracingJit {
    pub unsafe fn new(
        func_addr_in_bytecode_and_its_entity: HashMap<usize, FunctionInfoForJIT>,
    ) -> TracingJit {
        llvm::execution_engine::LLVMLinkInMCJIT();
        llvm::target::LLVM_InitializeNativeTarget();
        llvm::target::LLVM_InitializeNativeAsmPrinter();
        llvm::target::LLVM_InitializeNativeAsmParser();
        llvm::target::LLVM_InitializeAllTargetMCs();

        let context = LLVMContextCreate();
        let module =
            LLVMModuleCreateWithNameInContext(CString::new("rapidus").unwrap().as_ptr(), context);

        let mut ee = 0 as llvm::execution_engine::LLVMExecutionEngineRef;
        let mut error = 0 as *mut i8;
        if llvm::execution_engine::LLVMCreateExecutionEngineForModule(&mut ee, module, &mut error)
            != 0
        {
            panic!()
        }

        TracingJit {
            func_addr_in_bytecode_and_its_entity: func_addr_in_bytecode_and_its_entity,
            func_addr_in_bytecode_and_its_addr_in_llvm: HashMap::new(),
            return_ty_map: HashMap::new(),
            count: HashMap::new(),
            context: context,
            module: module,
            builder: LLVMCreateBuilderInContext(context),
            exec_engine: ee,
            cur_func: None,
        }
    }
}

unsafe fn cur_bb_has_no_terminator(builder: LLVMBuilderRef) -> bool {
    LLVMIsATerminatorInst(LLVMGetLastInstruction(LLVMGetInsertBlock(builder))) == ptr::null_mut()
}

impl TracingJit {
    pub unsafe fn can_jit(&mut self, pc: usize) -> Option<fn(c: *mut f64) -> f64> {
        if *self.count.entry(pc).or_insert(0) >= 10 {
            let info = self
                .func_addr_in_bytecode_and_its_entity
                .get(&pc)
                .unwrap()
                .clone();

            if info.cannot_jit {
                return None;
            }

            if let Some(val) = self.func_addr_in_bytecode_and_its_addr_in_llvm.get(&pc) {
                return Some(val.clone());
            }

            if let Err(()) = self.gen_code(pc, &info) {
                self.func_addr_in_bytecode_and_its_entity
                    .get_mut(&pc)
                    .unwrap()
                    .cannot_jit = true;
                return None;
            }

            let f = ::std::mem::transmute::<u64, fn(c: *mut f64) -> f64>(
                llvm::execution_engine::LLVMGetFunctionAddress(
                    self.exec_engine,
                    CString::new(info.name.as_str()).unwrap().as_ptr(),
                ),
            );
            // LLVMDumpModule(self.module);
            self.func_addr_in_bytecode_and_its_addr_in_llvm
                .insert(pc, f);
            return Some(f);
        }

        *self.count.get_mut(&pc).unwrap() += 1;
        None
    }

    pub fn register_return_type(&mut self, pc: usize, val: &vm::Value) {
        match val {
            &vm::Value::Number(_) => self.return_ty_map.insert(pc, ValueType::Number),
            &vm::Value::Bool(_) => self.return_ty_map.insert(pc, ValueType::Bool),
            _ => None,
        };
    }

    pub unsafe fn run_llvm_func(
        &mut self,
        f: fn(c: *mut f64) -> f64,
        args: Vec<vm::Value>,
    ) -> vm::Value {
        let mut llvm_args = vec![];
        for arg in args {
            llvm_args.push(match arg {
                vm::Value::Number(f) => f,
                _ => unimplemented!(),
            });
        }

        // By a bug of LLVM, llvm::execution_engine::runFunction can not be used.
        // So, all I can do is this:
        let llvm_val = match llvm_args.len() {
            0 => ::std::mem::transmute::<fn(c: *mut f64) -> f64, fn() -> f64>(f)(),
            1 => {
                ::std::mem::transmute::<fn(c: *mut f64) -> f64, fn(a1: f64) -> f64>(f)(llvm_args[0])
            }
            2 => ::std::mem::transmute::<fn(c: *mut f64) -> f64, fn(f64, f64) -> f64>(f)(
                llvm_args[0],
                llvm_args[1],
            ),
            3 => ::std::mem::transmute::<fn(c: *mut f64) -> f64, fn(f64, f64, f64) -> f64>(f)(
                llvm_args[0],
                llvm_args[1],
                llvm_args[2],
            ),
            _ => unimplemented!("should be implemented.."),
        };

        let val = vm::Value::Number(llvm_val);

        val
    }

    unsafe fn gen_code(
        &mut self,
        pc: usize,
        info: &FunctionInfoForJIT,
    ) -> Result<LLVMValueRef, ()> {
        let func_ret_ty = if let Some(ty) = self.return_ty_map.get(&pc) {
            ty.to_llvmty()
        } else {
            LLVMDoubleType() // Assume as double
        };
        let func_ty = LLVMFunctionType(
            func_ret_ty,
            vec![LLVMDoubleType()]
                .repeat(info.params.len())
                .as_mut_slice()
                .as_mut_ptr(),
            info.params.len() as u32,
            0,
        );
        let func = LLVMAddFunction(
            self.module,
            CString::new(info.name.as_str()).unwrap().as_ptr(),
            func_ty,
        );
        let bb_entry = LLVMAppendBasicBlock(func, CString::new("entry").unwrap().as_ptr());
        LLVMPositionBuilderAtEnd(self.builder, bb_entry);

        let mut env = HashMap::new();
        env.insert(info.name.clone(), func);
        self.cur_func = Some(func);

        for (i, arg) in info.params.iter().enumerate() {
            LLVMBuildStore(
                self.builder,
                LLVMGetParam(func, i as u32),
                self.declare_local_var(&arg.name, &mut env),
            );
        }

        self.gen(&mut env, &info.body)?;

        let mut iter_bb = LLVMGetFirstBasicBlock(func);
        while iter_bb != ptr::null_mut() {
            if LLVMIsATerminatorInst(LLVMGetLastInstruction(iter_bb)) == ptr::null_mut() {
                let terminator_builder = LLVMCreateBuilderInContext(self.context);
                LLVMPositionBuilderAtEnd(terminator_builder, iter_bb);
                LLVMBuildRet(terminator_builder, LLVMConstNull(LLVMDoubleType()));
            }
            iter_bb = LLVMGetNextBasicBlock(iter_bb);
        }

        Ok(func)
    }

    unsafe fn declare_local_var(
        &mut self,
        name: &String,
        env: &mut HashMap<String, LLVMValueRef>,
    ) -> LLVMValueRef {
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
            LLVMDoubleType(),
            CString::new("").unwrap().as_ptr(),
        );
        env.insert(name.clone(), var);
        var
    }

    unsafe fn gen(
        &mut self,
        env: &mut HashMap<String, LLVMValueRef>,
        node: &Node,
    ) -> Result<LLVMValueRef, ()> {
        match node {
            &Node::StatementList(ref list) => {
                for elem in list {
                    self.gen(env, elem)?;
                }
                Ok(ptr::null_mut())
            }
            &Node::BinaryOp(box ref lhs, box ref rhs, ref op) => match op {
                &BinOp::Add => Ok(LLVMBuildFAdd(
                    self.builder,
                    self.gen(env, lhs)?,
                    self.gen(env, rhs)?,
                    CString::new("fadd").unwrap().as_ptr(),
                )),
                &BinOp::Sub => Ok(LLVMBuildFSub(
                    self.builder,
                    self.gen(env, lhs)?,
                    self.gen(env, rhs)?,
                    CString::new("fsub").unwrap().as_ptr(),
                )),
                &BinOp::Lt => Ok(LLVMBuildFCmp(
                    self.builder,
                    llvm::LLVMRealPredicate::LLVMRealOLT,
                    self.gen(env, lhs)?,
                    self.gen(env, rhs)?,
                    CString::new("flt").unwrap().as_ptr(),
                )),
                _ => panic!(),
            },
            &Node::If(box ref cond, box ref then, box ref else_) => {
                let cond_val_tmp = self.gen(env, cond)?;
                let cond_val = cond_val_tmp;

                let func = self.cur_func.unwrap();

                let bb_then = LLVMAppendBasicBlock(func, CString::new("then").unwrap().as_ptr());
                let bb_else = LLVMAppendBasicBlock(func, CString::new("else").unwrap().as_ptr());
                let bb_merge = LLVMAppendBasicBlock(func, CString::new("merge").unwrap().as_ptr());

                LLVMBuildCondBr(self.builder, cond_val, bb_then, bb_else);

                LLVMPositionBuilderAtEnd(self.builder, bb_then);

                self.gen(env, then)?;
                if cur_bb_has_no_terminator(self.builder) {
                    LLVMBuildBr(self.builder, bb_merge);
                }

                LLVMPositionBuilderAtEnd(self.builder, bb_else);

                self.gen(env, else_)?;
                if cur_bb_has_no_terminator(self.builder) {
                    LLVMBuildBr(self.builder, bb_merge);
                }

                LLVMPositionBuilderAtEnd(self.builder, bb_merge);

                Ok(ptr::null_mut())
            }
            &Node::Call(box ref callee, ref args) => {
                let callee = if let Node::Identifier(name) = callee {
                    *env.get(name).unwrap()
                } else {
                    unreachable!()
                };
                let mut llvm_args = vec![];
                for arg in args {
                    llvm_args.push(self.gen(env, arg)?);
                }
                Ok(LLVMBuildCall(
                    self.builder,
                    callee,
                    llvm_args.as_mut_ptr(),
                    llvm_args.len() as u32,
                    CString::new("").unwrap().as_ptr(),
                ))
            }
            &Node::Return(Some(box ref val)) => Ok(LLVMBuildRet(self.builder, self.gen(env, val)?)),
            &Node::Identifier(ref name) => Ok(LLVMBuildLoad(
                self.builder,
                *env.get(name).unwrap(),
                CString::new("").unwrap().as_ptr(),
            )),
            &Node::Number(f) => Ok(LLVMConstReal(LLVMDoubleType(), f)),
            &Node::Nope => Ok(ptr::null_mut()),
            _ => {
                // Unsupported features
                Err(())
            }
        }
    }
}
