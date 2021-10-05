#![feature(test)]
extern crate test;
use rapidus_core::vm;
use rapidus_core::vm::jsvalue::value::Value;
use rapidus_parser as parser;
use std::fs::OpenOptions;
use std::io::Read;

/// Load the file ("test/{file_name}.js"), execute the script,
/// and compare returned value and the given answer.
/// ### Panic
/// Panic if the returned value was different from the answer.

pub fn test_file(file_name: impl Into<String>, answer: impl Into<String>) {
    let file_name = file_name.into();
    println!("{}", format!("tests/test/{}.js", file_name));
    compare_scripts(load_file(file_name), answer.into());
}

/// Load the file ("test/{file_name}.js"), and execute the script.
/// ### Panic
/// Panic if the given code returned Err.
pub fn assert_file(file_name: &str) {
    println!("{}", format!("tests/test/{}.js", file_name));
    execute_script(load_file(file_name));
}

fn load_file(file_name: impl Into<String>) -> String {
    let mut file_body = String::new();
    match OpenOptions::new()
        .read(true)
        .open(format!("tests/test/{}.js", file_name.into()))
    {
        Ok(mut file) => match file.read_to_string(&mut file_body).ok() {
            Some(x) => x,
            None => panic!("Couldn't read the file"),
        },
        Err(_) => panic!("file not found"),
    };
    file_body
}

/// Execute the given code, and compare returned value and the given answer.
/// ### Panic
/// Panic if the returned value was different from the answer.
pub fn test_code(code: impl Into<String>, answer: impl Into<String>) {
    compare_scripts(code.into(), answer.into());
}

/// Execute the given code, and normally terminates only when an runtime error is returned.
/// ### Panic
/// Panic if the code returned a parse error, or terminated without error.
pub fn runtime_error(text: &str) -> String {
    let mut vm = vm::vm::VM::new();

    let mut parser = parser::Parser::new("test", text);
    let node = parser.parse_all().unwrap();

    let func_info = vm.compile(&node, true).unwrap();
    match vm.run_global(func_info) {
        Ok(()) => panic!(),
        Err(err) => return format!("{:?}", err),
    };
}

/// Execute the given code.
/// ### Panic
/// Panic if the given code returned Err.
pub fn execute_script(text: String) -> String {
    let mut vm = vm::vm::VM::new();

    let mut parser = parser::Parser::new("test", text);
    let node = parser.parse_all().unwrap();
    let func_info = vm.compile(&node, true).unwrap();
    vm.run_global(func_info).unwrap();
    let val: Value = vm
        .current_context
        .stack
        .pop()
        .unwrap_or(Value::undefined().into())
        .into();
    val.debug_string(true)
}

fn compare_scripts(text: String, answer: String) {
    let res_text = execute_script(text);
    println!("file: {}", res_text);

    let res_answer = execute_script(answer);
    println!("ans:  {}", res_answer);

    assert_eq!(res_text, res_answer);
}

#[test]
fn vm_test() {
    execute_script("for(var i = 0; i < 4; i++){ i }".to_string());
}

#[test]
fn string_test1() {
    test_code("'死して屍拾う者なし'[4]", "'拾'");
}

#[test]
fn string_test2() {
    test_code("'死して屍拾う者なし'.length", "9");
}
#[test]
fn operator_test() {
    test_code("+(5>3)+60%7+(3>=5)+!!5+(-6)", "0");
}

#[test]
fn operator_test2() {
    assert_file("operator");
}

#[test]
fn this_test() {
    test_file("this", "[1,101,124]");
}

#[test]
fn prototype_test() {
    assert_file("prototypes");
}

#[test]
fn accessor_property() {
    assert_file("accessor_property");
}

#[test]
fn trinity() {
    assert_file("trinity");
}

#[test]
fn closure() {
    assert_file("closure");
}

#[test]
fn trycatch() {
    assert_file("trycatch");
}

#[test]
fn r#typeof() {
    assert_file("typeof");
}

#[test]
fn exotic_cmp() {
    assert_file("exotic_cmp")
}

#[test]
fn r#while() {
    assert_file("while")
}

#[test]
fn r#for() {
    assert_file("for")
}

#[test]
fn r#if() {
    assert_file("if")
}

#[test]
fn arrow_function() {
    test_code("let f = (x) => { return x * x }; f(5)", "25");
    test_code("let f = x => { return x * x }; f(6)", "36");
}

#[test]
fn symbol() {
    assert_file("symbol")
}

#[test]
fn array() {
    assert_file("array")
}

#[test]
fn spread_op() {
    assert_file("spread_op")
}

#[test]
fn env() {
    assert_file("env")
}

#[test]
fn new_call_member() {
    assert_file("new_call_member")
}

#[test]
fn assert() {
    assert_file("assert")
}

#[test]
fn fibo() {
    assert_file("fibo")
}

#[test]
fn fact() {
    assert_file("fact")
}

#[test]
fn test_module() {
    assert_file("test_module_caller")
}

#[test]
fn function_methods() {
    assert_file("function_methods")
}

#[test]
fn string_methods() {
    assert_file("string_methods")
}

#[test]
fn runtime_error1() {
    runtime_error("let a = {}; a.b.c");
}

#[test]
fn runtime_error2() {
    runtime_error("let a = {}; a.b.c = 5");
}

#[test]
fn runtime_error3() {
    runtime_error("let a = {}; a(5)");
}

#[test]
fn runtime_error4() {
    runtime_error("let a = {}; a.b(5)");
}

#[test]
fn runtime_error5() {
    runtime_error("let a = {}; a(5)");
}

#[test]
fn runtime_error6() {
    runtime_error("let a = x => y; a(1)");
}

#[test]
fn runtime_error7() {
    runtime_error("let a = x => arguments[0]; a(1)");
}

use test::Bencher;
#[bench]
fn bench_fibo(b: &mut Bencher) {
    b.iter(|| assert_file("fibo"));
}
