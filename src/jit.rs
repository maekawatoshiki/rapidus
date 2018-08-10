use node::Node;
use vm;
use vm_codegen::FunctionInfoForJIT;

use std::collections::HashMap;

use llvm;
use llvm::core::*;
use llvm::prelude::*;

use std::ffi::CString;
use std::ptr;

#[derive(Debug, Clone)]
pub struct TracingJit {
    func_addr_in_bytecode_and_its_entity: HashMap<usize, FunctionInfoForJIT>,
    func_addr_in_bytecode_and_its_addr_in_llvm: HashMap<usize, fn(c: *mut f64) -> f64>,
    count: HashMap<usize, usize>,
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    exec_engine: llvm::execution_engine::LLVMExecutionEngineRef,
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
            count: HashMap::new(),
            context: context,
            module: module,
            builder: LLVMCreateBuilderInContext(context),
            exec_engine: ee,
        }
    }
}

impl TracingJit {
    pub unsafe fn can_jit(&mut self, pc: usize) -> Option<fn(c: *mut f64) -> f64> {
        if *self.count.entry(pc).or_insert(0) >= 10 {
            let info = self
                .func_addr_in_bytecode_and_its_entity
                .get(&pc)
                .unwrap()
                .clone();

            if let Some(val) = self.func_addr_in_bytecode_and_its_addr_in_llvm.get(&pc) {
                return Some(val.clone());
            }

            self.gen_code(&info);
            let f = ::std::mem::transmute::<u64, fn(c: *mut f64) -> f64>(
                llvm::execution_engine::LLVMGetFunctionAddress(
                    self.exec_engine,
                    CString::new(info.name.as_str()).unwrap().as_ptr(),
                ),
            );
            LLVMDumpModule(self.module);
            self.func_addr_in_bytecode_and_its_addr_in_llvm
                .insert(pc, f);
            return Some(f);
        } else {
            *self.count.get_mut(&pc).unwrap() += 1;
        }
        None
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

        let llvm_val = f(llvm_args.as_mut_slice().as_mut_ptr());

        let val = vm::Value::Number(llvm_val);

        val
    }

    unsafe fn gen_code(&mut self, info: &FunctionInfoForJIT) -> LLVMValueRef {
        let func_ty = LLVMFunctionType(
            LLVMDoubleType(),
            vec![LLVMPointerType(LLVMDoubleType(), 0)]
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
        for (i, arg) in info.params.iter().enumerate() {
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
            LLVMBuildStore(
                builder,
                LLVMBuildLoad(
                    builder,
                    LLVMGetParam(func, i as u32),
                    CString::new("").unwrap().as_ptr(),
                ),
                var,
            );
            env.insert(arg.name.clone(), var);
        }

        self.gen(&mut env, &info.body);

        let mut iter_bb = LLVMGetFirstBasicBlock(func);
        while iter_bb != ptr::null_mut() {
            if LLVMIsATerminatorInst(LLVMGetLastInstruction(iter_bb)) == ptr::null_mut() {
                let terminator_builder = LLVMCreateBuilderInContext(self.context);
                LLVMPositionBuilderAtEnd(terminator_builder, iter_bb);
                LLVMBuildRet(terminator_builder, LLVMConstNull(LLVMDoubleType()));
            }
            iter_bb = LLVMGetNextBasicBlock(iter_bb);
        }

        func
    }

    unsafe fn gen(&mut self, env: &mut HashMap<String, LLVMValueRef>, node: &Node) -> LLVMValueRef {
        match node {
            &Node::StatementList(ref list) => {
                for elem in list {
                    self.gen(env, elem);
                }
                ptr::null_mut()
            }
            &Node::Return(Some(box ref val)) => LLVMBuildRet(self.builder, self.gen(env, val)),
            &Node::Identifier(ref name) => LLVMBuildLoad(
                self.builder,
                *env.get(name).unwrap(),
                CString::new("").unwrap().as_ptr(),
            ),
            &Node::Number(f) => LLVMConstReal(LLVMDoubleType(), f),
            _ => ptr::null_mut(),
        }
    }
}
