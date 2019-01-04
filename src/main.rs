extern crate rapidus;
use rapidus::builtin;
use rapidus::bytecode_gen;
use rapidus::parser;
use rapidus::vm;
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

    let mut vm_codegen = vm_codegen::VMCodeGen::new();
    let mut iseq = vec![];
    vm_codegen.compile(&node, &mut iseq, false).unwrap();

    bytecode_gen::show(&iseq, &vm_codegen.bytecode_gen.const_table);

    // println!("Result:");
    // let mut vm = vm::VM::new();
    // vm.global_objects.extend(vm_codegen.global_varmap);
    // vm.run(iseq);

    // println!("VM CodeGen Test:");
    // vm_codegen::test();
}

fn repl(trace: bool) {
    // TODO: REFINE CODE!!!!
    let mut vm_codegen = vm_codegen::VMCodeGen::new();
    let global = vm_codegen.global_varmap;
    let mut vm = vm::vm::VM::new(global);

    let mut rl = rustyline::Editor::<()>::new();

    loop {
        let line = match rl.readline("> ") {
            Ok(line) => {
                let body = if line == ".." {
                    let mut lines = "".to_string();

                    while let Ok(line) = rl.readline(">> ") {
                        if line == ".." {
                            break;
                        }
                        lines += line.as_str();
                        lines += "\n";
                    }

                    lines
                } else {
                    line
                };

                rl.add_history_entry(body.as_ref());
                body
            }
            Err(_) => break,
        };

        let mut parser = parser::Parser::new(line);

        let node = match parser.parse_all() {
            Ok(ok) => ok,
            Err(err) => {
                parser.handle_error(err);
                continue;
            }
        };

        let mut iseq = vec![];
        match vm_codegen.compile(&node, &mut iseq, true) {
            Ok(()) => {}
            Err(vm_codegen::Error::General { msg, token_pos }) => {
                parser.show_error_at(token_pos, msg.as_str());
                continue;
            }
            Err(vm_codegen::Error::Unimplemented { msg, token_pos }) => {
                parser.show_error_at(token_pos, msg.as_str());
                continue;
            }
        };
        vm.const_table = vm_codegen.bytecode_gen.const_table.clone();
        vm.state.pc = 0;
        vm.is_debug = trace;
        let res = vm.run(iseq);
        if vm.state.stack.len() == 0 {
            vm.state.stack.push(vm::value::Value::Undefined);
        };
        if vm.state.history.len() != 1 {
            println!(
                "Warning: history length is {} (should be 1)",
                vm.state.history.len()
            );
        };
        match res {
            Err(e) => {
                e.show_error_message();
                vm.state.stack.pop();
            }
            _ => {
                // Show the evaluated result
                if let Some(value) = vm.state.stack.pop() {
                    //println!("{}", value.to_string());
                    unsafe {
                        builtin::debug_print(&value, true);
                        libc::puts(b"\0".as_ptr() as *const i8);
                    }
                }
            }
        }

        vm_codegen.bytecode_gen.const_table = vm.const_table.clone();
        //vm.state.stack.clear();
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

            let mut vm_codegen = vm_codegen::VMCodeGen::new();
            let mut iseq = vec![];
            match vm_codegen.compile(&node, &mut iseq, false) {
                Ok(()) => {}
                Err(vm_codegen::Error::General { msg, token_pos }) => {
                    parser.show_error_at(token_pos, msg.as_str());
                    return;
                }
                Err(e) => panic!(e),
            }

            let mut vm = vm::vm::VM::new(vm_codegen.global_varmap);
            vm.const_table = vm_codegen.bytecode_gen.const_table;
            vm.is_debug = trace;

            if let Err(e) = vm.run(iseq) {
                e.show_error_message();
            }
        }
        Err(e) => panic!("Rapidus Internal Error: fork failed: {:?}", e),
    }
}
