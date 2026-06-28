#![no_main]

use libfuzzer_sys::fuzz_target;
use rapidus_parser::Parser;

fuzz_target!(|data: &[u8]| {
    if let Ok(code) = std::str::from_utf8(data) {
        let mut parser = Parser::new("fuzz.js", code);
        let _ = parser.parse_all();
    }
});