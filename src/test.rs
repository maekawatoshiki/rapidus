use parser;
use std::fs::OpenOptions;
use std::io::Read;
use vm;
use vm_codegen::*;

pub fn test_file(file_name: String, answer: String) {
    let mut file_body = String::new();
    match OpenOptions::new()
        .read(true)
        .open(format!("test/{}.js", file_name.clone()))
    {
        Ok(mut file) => match file.read_to_string(&mut file_body).ok() {
            Some(x) => x,
            None => panic!("Couldn't read the file"),
        },
        Err(_) => panic!("file not found"),
    };
    println!("{}", format!("test/{}.js", file_name));
    execute_script(file_body, answer);
}

pub fn test_code(code: String, answer: String) {
    execute_script(code, answer);
}

fn execute_script(text: String, answer: String) {
    let mut vm_codegen = VMCodeGen::new();
    let global = vm_codegen.global_varmap;
    let mut vm = vm::vm::VM::new(global);

    let mut parser = parser::Parser::new(text);
    let node = parser.parse_all().unwrap();
    let mut iseq = vec![];

    vm_codegen.compile(&node, &mut iseq, true).unwrap();
    vm.const_table = vm_codegen.bytecode_gen.const_table;
    vm.is_debug = false;
    vm.run(iseq).unwrap();
    let res_text = vm.state.stack.pop().unwrap().clone().format(5, true);
    println!("file: {}", res_text);

    let mut vm_codegen = VMCodeGen::new();
    let global = vm_codegen.global_varmap;
    let mut vm = vm::vm::VM::new(global);

    let mut parser = parser::Parser::new(answer);
    let node = parser.parse_all().unwrap();
    let mut iseq = vec![];

    vm_codegen.compile(&node, &mut iseq, true).unwrap();
    vm.const_table = vm_codegen.bytecode_gen.const_table;
    vm.is_debug = false;
    vm.run(iseq).unwrap();
    let res_answer = vm.state.stack.pop().unwrap().clone().format(5, true);
    println!("ans:  {}", res_answer);

    assert_eq!(res_text, res_answer);
}
