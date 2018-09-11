#![feature(box_patterns)]
#![feature(tool_attributes)]
#![feature(repeat_generic_slice)]

pub mod bytecode_gen;
pub mod extract_anony_func;
pub mod fv_finder;
pub mod fv_solver;
pub mod id;
pub mod jit;
pub mod lexer;
pub mod node;
pub mod parser;
pub mod token;
pub mod vm;
pub mod vm_codegen;
pub mod builtin;

extern crate ansi_term;
extern crate encoding;
extern crate libc;
extern crate llvm_sys as llvm;
extern crate nix;
extern crate rand;
// extern crate cpuprofiler;
