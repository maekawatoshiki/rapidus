use parser;
use std::fs::OpenOptions;
use std::io::Read;
use vm;
use vm_codegen::*;

pub fn test_code(file_name: String, answer: String) {
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
    let value = execute_script(file_body);
    println!("file: {}", value);
    let answer = execute_script(answer);
    println!("ans:  {}", answer);
    assert_eq!(value, answer);

    fn execute_script(text: String) -> String {
        let mut parser = parser::Parser::new(text);
        let node = parser.parse_all().unwrap();
        let mut iseq = vec![];
        let mut vm_codegen = VMCodeGen::new();
        let global = vm_codegen.global_varmap;
        let mut vm = vm::vm::VM::new(global);
        vm_codegen.compile(&node, &mut iseq, true).unwrap();
        vm.const_table = vm_codegen.bytecode_gen.const_table.clone();
        vm.state.pc = 0;
        vm.is_debug = false;
        vm.run(iseq).unwrap();
        if vm.state.stack.len() == 0 {
            vm.state.stack.push(vm::value::Value::Undefined);
        };
        if vm.state.history.len() != 1 {
            panic!("length of history stack shoule be 1.")
        };
        vm.state.stack.pop().unwrap().clone().format(100, true)
    }
}
