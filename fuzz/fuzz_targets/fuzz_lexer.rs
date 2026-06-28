#![no_main]

use libfuzzer_sys::fuzz_target;
use rapidus_lexer::Lexer;

fuzz_target!(|data: &[u8]| {
    if let Ok(code) = std::str::from_utf8(data) {
        let mut lexer = Lexer::new(code.to_string());
        let _ = lexer.tokenize_all();
    }
});