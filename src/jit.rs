use vm_codegen::FunctionInfoForJIT;

use std::collections::HashMap;

// use llvm::core::*;
use llvm::prelude::*;

use std::ptr;

#[derive(Debug, Clone)]
pub struct TracingJit {
    func_addr_in_bytecode_and_its_entity: HashMap<usize, FunctionInfoForJIT>,
    count: HashMap<usize, usize>,
}

impl TracingJit {
    pub fn new(
        func_addr_in_bytecode_and_its_entity: HashMap<usize, FunctionInfoForJIT>,
    ) -> TracingJit {
        TracingJit {
            func_addr_in_bytecode_and_its_entity: func_addr_in_bytecode_and_its_entity,
            count: HashMap::new(),
        }
    }
}

impl TracingJit {
    pub unsafe fn can_jit(&mut self, pc: usize) -> Option<LLVMValueRef> {
        if *self.count.entry(pc).or_insert(0) >= 10 {
            let info = self.func_addr_in_bytecode_and_its_entity.get(&pc).unwrap().clone();
            return Some(self.gen_code(info));
        } else {
            *self.count.get_mut(&pc).unwrap() += 1;
        }
        None
    }

    unsafe fn gen_code(&mut self, info: FunctionInfoForJIT) -> LLVMValueRef {
        ptr::null_mut()
    }
}
