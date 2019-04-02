#![feature(box_patterns)]
#![feature(repeat_generic_slice)]
#![feature(type_ascription)]
#[macro_use]
pub mod util;
#[macro_use]
pub mod vm;
pub mod builtin;
pub mod builtins;
pub mod bytecode_gen;
pub mod gc;
pub mod id;
// pub mod jit;
pub mod lexer;
pub mod node;
pub mod parser;
pub mod test;
pub mod token;
pub mod vm_codegen;

extern crate ansi_term;
extern crate chrono;
extern crate encoding;
// extern crate libc;
// extern crate libloading;
// extern crate llvm_sys as llvm;
// extern crate nix;
extern crate rand;
extern crate rustc_hash;
// extern crate rustyline;
// extern crate stopwatch;
#[macro_use]
extern crate nanbox;
// extern crate cpuprofiler;

extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub extern "C" fn run_js(source: &str) {
    use vm::{jsvalue::value::Value2, vm::VM2};

    let mut parser = parser::Parser::new(source.to_string());

    println!("Parser:");
    let node = match parser.parse_all() {
        Ok(ok) => ok,
        Err(err) => {
            parser.handle_error(err);
            return;
        }
    };
    println!("{:?}", node);

    let mut vm = VM2::new();
    let mut iseq = vec![];
    let global_info = match vm.compile(&node, &mut iseq, false) {
        Ok(ok) => ok,
        Err(vm::codegen::Error { msg, token_pos, .. }) => {
            parser.show_error_at(token_pos, msg.as_str());
            return;
        }
    };

    println!("CodeGenerator generated:");
    bytecode_gen::show2(&iseq, &vm.constant_table);

    println!("Result:");
    if let Err(e) = vm.run_global(global_info, iseq) {
        e.show_error_message();
    }

    for (i, val_boxed) in vm.stack.iter().enumerate() {
        let val: Value2 = (*val_boxed).into();
        let s = val.debug_string(true);
    }
}
