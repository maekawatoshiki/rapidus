#![no_main]

use libfuzzer_sys::fuzz_target;
use rapidus_core::vm::vm::VM;
use rapidus_parser::Parser;

fuzz_target!(|data: &[u8]| {
    if let Ok(code) = std::str::from_utf8(data) {
        let mut parser = Parser::new("fuzz.js", code);
        if let Ok(node) = parser.parse_all() {
            let mut vm = VM::new();
            if let Ok(global_info) = vm.compile(&node, false) {
                let _ = vm.run_global(global_info);
            }
        }
    }
});