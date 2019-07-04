#![feature(test)]
//extern crate rapidus;
use rapidus::parser;
use rapidus::{vm, vm::exec_context, vm::vm::VM};
extern crate clap;
extern crate libc;
extern crate rustyline;
extern crate test;
use clap::{App, Arg};

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
            Arg::with_name("profile")
                .help("Collect and print performance profile")
                .long("profile"),
        )
        .arg(
            Arg::with_name("trace")
                .help("Tracing execution")
                .long("trace"),
        )
        .arg(Arg::with_name("file").help("Input file name").index(1));
    let app_matches = app.clone().get_matches();
    let is_debug = app_matches.is_present("debug");
    let is_profile = app_matches.is_present("profile");
    let is_trace = app_matches.is_present("trace");
    let file_name = match app_matches.value_of("file") {
        Some(file_name) => file_name,
        None => {
            repl(is_profile, is_trace);
            return;
        }
    };

    let mut parser = match parser::Parser::load_module(file_name.clone()) {
        Ok(ok) => ok,
        Err(_) => return,
    };

    let node = match parser.parse_all() {
        Ok(ok) => ok,
        Err(err) => {
            parser.handle_error(&err);
            return;
        }
    };
    if is_debug {
        println!("Parser:");
        println!("{:?}", node);
    };

    let mut vm = VM::new();
    if is_profile {
        vm = vm.profile();
    }
    if is_trace {
        vm = vm.trace();
    }

    let global_info = match vm.compile(&node, false) {
        Ok(ok) => ok,
        Err(vm::codegen::Error { msg, token_pos, .. }) => {
            parser.show_error_at(token_pos, msg);
            return;
        }
    };

    if is_debug {
        println!("Codegen:");
        rapidus::bytecode_gen::show_inst_seq(&global_info.code, &vm.constant_table);
    };

    let script_info = parser.into_script_info();
    vm.script_info
        .push((global_info.module_func_id, script_info));
    if let Err(e) = vm.run_global(global_info) {
        vm.show_error_message(e);
    }
}

fn repl(is_profile: bool, is_trace: bool) {
    let mut rl = rustyline::Editor::<()>::new();
    let mut vm = VM::new();
    if is_profile {
        vm = vm.profile();
    }
    if is_trace {
        vm = vm.trace();
    }
    let mut global_context: Option<exec_context::ExecContext> = None;

    loop {
        let mut parser;

        let line = if let Ok(line) = rl.readline("> ") {
            line
        } else {
            break;
        };

        rl.add_history_entry(line.clone());

        let mut lines = line + "\n";

        loop {
            parser = parser::Parser::new("REPL", lines.clone());
            match parser.parse_all() {
                Ok(node) => {
                    // compile and execute
                    let global_info = match vm.compile(&node, true) {
                        Ok(ok) => ok,
                        Err(vm::codegen::Error { msg, token_pos, .. }) => {
                            parser.show_error_at(token_pos, msg);
                            break;
                        }
                    };

                    match global_context {
                        Some(ref mut context) => {
                            context.append_from_function_info(&mut vm.factory, &global_info);
                            context.func_ref = global_info;
                        }
                        None => global_context = Some(vm.create_global_context(global_info)),
                    }

                    vm.current_context = global_context.clone().unwrap();
                    let script_info = parser.into_script_info();
                    vm.script_info = vec![(vm.current_context.func_ref.module_func_id, script_info)];

                    match vm.run() {
                        Ok(val) => println!("{}", val.debug_string(true)),
                        Err(e) => {
                            let val = e.to_value(&mut vm.factory);
                            if val.is_error_object() {
                                println!("Error: {}", val.get_property("message"));
                            } else {
                                println!("Thrown: {}", val.to_string())
                            };
                        }
                    }
                    break;
                }
                Err(parser::Error::UnexpectedEOF(_)) => match rl.readline("... ") {
                    Ok(line) => {
                        rl.add_history_entry(line.clone());
                        lines += line.as_str();
                        lines += "\n";
                        continue;
                    }
                    Err(_) => break,
                },
                Err(e) => {
                    parser.handle_error(&e);
                    break;
                }
            }
        }
    }
}
