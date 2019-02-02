extern crate rapidus;
use rapidus::bytecode_gen;
use rapidus::parser;
use rapidus::vm;
use rapidus::vm::value::Value;
use rapidus::vm::vm::VM;
use rapidus::vm_codegen;

extern crate libc;

extern crate rustyline;

extern crate clap;
use clap::{App, Arg};

extern crate nix;
use nix::sys::wait::*;
use nix::unistd::*;

extern crate ansi_term;
use ansi_term::Colour;

use std::fs::OpenOptions;
use std::io::prelude::*;

const VERSION_STR: &'static str = env!("CARGO_PKG_VERSION");

fn main() {
    let app = App::new("Rapidus")
        .version(VERSION_STR)
        .author("uint256_t")
        .about("A toy JavaScript engine")
        .arg(
            Arg::with_name("debug")
                .help("Show useful information for debugging")
                .long("debug"),
        )
        .arg(
            Arg::with_name("trace")
                .help("Trace bytecode execution for debugging")
                .long("trace"),
        )
        .arg(Arg::with_name("file").help("Input file name").index(1));
    let app_matches = app.clone().get_matches();

    let file_name = match app_matches.value_of("file") {
        Some(file_name) => file_name,
        None => {
            repl(app_matches.is_present("trace"));
            return;
        }
    };

    // Normally run the given code
    if !app_matches.is_present("debug") {
        run(file_name, app_matches.is_present("trace"));
        return;
    }

    // Show information for debugging

    let mut file_body = String::new();

    match OpenOptions::new().read(true).open(file_name) {
        Ok(mut ok) => ok
            .read_to_string(&mut file_body)
            .ok()
            .expect("cannot read file"),
        Err(e) => {
            println!("error: {}", e);
            return;
        }
    };

    let mut parser = parser::Parser::new(file_body);

    println!("Parser:");
    let node = match parser.parse_all() {
        Ok(ok) => ok,
        Err(err) => {
            parser.handle_error(err);
            return;
        }
    };
    println!("{:?}", node);

    let mut vm = VM::new();
    let mut iseq = vec![];
    vm.codegen.compile(&node, &mut iseq, false).unwrap();

    bytecode_gen::show(&iseq, &vm.codegen.bytecode_gen.const_table);

    // println!("Result:");
    // let mut vm = vm::VM::new();
    // vm.global_objects.extend(vm_codegen.global_varmap);
    // vm.run(iseq);
}

fn repl(trace: bool) {
    // TODO: REFINE CODE!!!!
    let mut vm = vm::vm::VM::new();
    vm.is_debug = trace;
    let mut rl = rustyline::Editor::<()>::new();

    loop {
        let mut parser;
        match rl.readline("> ") {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());
                let mut lines = line.clone() + "\n";
                loop {
                    parser = parser::Parser::new(lines.clone());
                    match parser.parse_all() {
                        Ok(node) => {
                            // compile and execute
                            let mut iseq = vec![];
                            match vm.codegen.compile(&node, &mut iseq, true) {
                                Ok(()) => {}
                                Err(vm_codegen::Error::General { msg, token_pos }) => {
                                    parser.show_error_at(token_pos, msg.as_str());
                                    break;
                                }
                                Err(vm_codegen::Error::Unimplemented { msg, token_pos }) => {
                                    parser.show_error_at(token_pos, msg.as_str());
                                    break;
                                }
                            };

                            let res = vm.run(iseq);
                            if vm.state.stack.len() == 0 {
                                vm.state.stack.push(vm::value::Value::Undefined);
                            };
                            if vm.context_stack.len() != 0 {
                                println!(
                                    "Warning: context length is {} (should be 0)",
                                    vm.context_stack.len()
                                );
                            };
                            match res {
                                Err(e) => {
                                    e.show_error_message();
                                }
                                _ => {
                                    // Show the evaluated result
                                    if let Some(value) = vm.state.stack.pop() {
                                        if value == Value::Undefined {
                                            print!(
                                                "{}",
                                                Colour::Fixed(8).paint(value.format(3, true))
                                            );
                                        } else {
                                            print!("{}", value.format(3, true));
                                        }

                                        println!();
                                        /*
                                        unsafe {
                                            builtin::debug_print(&value, true);
                                            libc::puts(b"\0".as_ptr() as *const i8);
                                        }
                                        */
                                    }
                                }
                            };
                            vm.state.stack.clear();
                            break;
                        }
                        Err(parser::Error::UnexpectedEOF(_)) => match rl.readline("... ") {
                            Ok(line) => {
                                rl.add_history_entry(line.as_ref());
                                lines += line.as_str();
                                lines += "\n";
                                continue;
                            }
                            Err(_) => break,
                        },
                        Err(_) => break,
                    };
                }
            }
            Err(_) => break,
        };
    }
}

fn run(file_name: &str, trace: bool) {
    match fork() {
        Ok(ForkResult::Parent { child, .. }) => {
            match waitpid(child, None) {
                Ok(ok) => match ok {
                    WaitStatus::Exited(_, status) => {
                        if status != 0 {
                            panic!("exited. status: {}", status)
                        }
                    }
                    WaitStatus::Signaled(pid, status, _) => {
                        // TODO: We can do anything (like calling destructors) here.
                        panic!("child: pid={:?}, status={:?}\nRapidus Internal Error: segmentation fault", pid, status);
                    }
                    e => panic!("Rapidus Internal Error: VM exited abnormally!: {:?}", e),
                },
                Err(e) => panic!("Rapidus Internal Error: waitpid failed: {:?}", e),
            }
        }
        Ok(ForkResult::Child) => {
            let mut file_body = String::new();

            match OpenOptions::new().read(true).open(file_name) {
                Ok(mut ok) => match ok.read_to_string(&mut file_body).ok() {
                    Some(x) => x,
                    None => {
                        eprintln!(
                            "{}: Couldn't read the file '{}'",
                            Colour::Red.bold().paint("error"),
                            file_name,
                        );
                        return;
                    }
                },
                Err(_e) => {
                    eprintln!(
                        "{}: No such file or directory '{}'",
                        Colour::Red.bold().paint("error"),
                        file_name,
                    );
                    return;
                }
            };

            if file_body.len() == 0 {
                return;
            }

            if file_body.as_bytes()[0] == b'#' {
                let first_ln = file_body.find('\n').unwrap_or(file_body.len());
                file_body.drain(..first_ln);
            }

            let mut parser = parser::Parser::new(file_body);

            let mut node = match parser.parse_all() {
                Ok(ok) => ok,
                Err(err) => {
                    parser.handle_error(err);
                    return;
                }
            };

            let mut vm = vm::vm::VM::new();
            let mut iseq = vec![];
            match vm.codegen.compile(&node, &mut iseq, false) {
                Ok(()) => {}
                Err(vm_codegen::Error::General { msg, token_pos }) => {
                    parser.show_error_at(token_pos, msg.as_str());
                    return;
                }
                Err(e) => panic!(e),
            }

            vm.is_debug = trace;

            if let Err(e) = vm.run(iseq) {
                e.show_error_message();
            }
        }
        Err(e) => panic!("Rapidus Internal Error: fork failed: {:?}", e),
    }
}

#[test]
fn vm_test() {
    // IMPORTANT: these tests should be run in a single thread.
    use rapidus::test::{assert_file, execute_script, test_code, test_file};
    execute_script("for(var i = 0; i < 4; i++){ i }".to_string(), true);
    assert_file("trinity".to_string());
    assert_file("closure".to_string());
    assert_file("fact".to_string());
    test_file(
        "array".to_string(),
        "'2,3,6,7,3,4,2,3,three1,5,4,1,2,three'".to_string(),
    );
    test_code("(100).toString(15)".to_string(), "'6a'".to_string());
    test_code(
        "'死して屍拾う者なし'[4]".to_string(),
        "'拾'".to_string(),
    );
    test_code(
        "'死して屍拾う者なし'.length".to_string(),
        "9".to_string(),
    );
    test_file(
        "label".to_string(),
        "[0,0,0,1,0,2,1,0,2,0,2,1,2,2]".to_string(),
    );
    test_file("this".to_string(), "[1,101,124]".to_string());
    test_file("trycatch".to_string(), "[ 0, 2, 1, 10110 ]".to_string());
    test_file(
        "prototypes".to_string(),
        "[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true]"
            .to_string());
    test_file(
        "qsort".to_string(),
        "[ 0, 0, 1, 3, 5, 7, 7, 10, 11, 12, 14, 14, 16, 17, 19 ]".to_string(),
    );
    test_file("arguments1".to_string(), "[[1,2,3,4,4],[1,2,[3,4]],[5,6,7,undefined,3],[5,6,[7]],[8,9,undefined,undefined,2],[8,9,undefined],[10,undefined,undefined,undefined,1],[10,undefined,undefined]]".to_string());
    test_file(
        "arguments2".to_string(),
        "[10,15,20,25,15,10,'OK',20,25,'OK',10,'NG',20,25,'NG']".to_string(),
    );
}
