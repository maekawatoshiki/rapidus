#![feature(box_patterns)]
#![feature(if_while_or_patterns)]
#![feature(repeat_generic_slice)]

pub mod builtin;
pub mod builtins;
pub mod bytecode_gen;
pub mod gc;
pub mod id;
pub mod jit;
pub mod lexer;
pub mod node;
pub mod parser;
pub mod token;
pub mod vm;
pub mod vm_codegen;

extern crate ansi_term;
extern crate chrono;
extern crate encoding;
extern crate libc;
extern crate libloading;
extern crate llvm_sys as llvm;
extern crate nix;
extern crate rand;
extern crate rustc_hash;
extern crate rustyline;
#[macro_use]
extern crate lazy_static;
// extern crate cpuprofiler;
