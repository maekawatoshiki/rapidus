#![feature(box_patterns)]
#![feature(tool_attributes)]

pub mod lexer;
pub mod node;
pub mod parser;
pub mod token;
pub mod vm;
pub mod vm_codegen;
pub mod jit;
pub mod bytecode_gen;
pub mod id;
pub mod fv_finder;
pub mod fv_solver;
pub mod extract_anony_func;

extern crate libc;
extern crate rand;
extern crate llvm_sys as llvm;
// extern crate cpuprofiler;
