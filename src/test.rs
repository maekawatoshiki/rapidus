use gc;
use parser;
use std::fs::OpenOptions;
use std::io::Read;
use vm;
use vm::value;
use vm_codegen::VMCodeGen;

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
    compare_scripts(file_body, answer);
}

pub fn test_code(code: String, answer: String) {
    compare_scripts(code, answer);
}

pub fn execute_script(text: String, debug: bool) -> String {
    let mut vm_codegen = VMCodeGen::new();
    let global = vm_codegen.global_varmap.clone();
    let mut vm = vm::vm::VM::new(global);

    let mut parser = parser::Parser::new(text);
    let node = parser.parse_all().unwrap();
    let mut iseq = vec![];

    vm_codegen.compile(&node, &mut iseq, true).unwrap();
    vm.const_table = vm_codegen.bytecode_gen.const_table;
    vm.is_debug = debug;
    vm.run(iseq).unwrap();
    vm.state
        .stack
        .pop()
        .unwrap_or(value::Value::Undefined)
        .clone()
        .format(5, true)
}

fn compare_scripts(text: String, answer: String) {
    let res_text = execute_script(text, false);
    println!("file: {}", res_text);

    let res_answer = execute_script(answer, false);
    println!("ans:  {}", res_answer);

    gc::free_all();
    assert_eq!(res_text, res_answer);
}
