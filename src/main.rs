extern crate rapidus;
use rapidus::builtin;
use rapidus::bytecode_gen;
use rapidus::extract_anony_func;
use rapidus::lexer;
use rapidus::parser;
use rapidus::vm;
use rapidus::vm_codegen;

use parser::Error::*;
use vm::RuntimeError;

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
    use parser::Error::*;

    let app = App::new("Rapidus")
        .version(VERSION_STR)
        .author("uint256_t")
        .about("A toy JavaScript engine")
        .arg(
            Arg::with_name("debug")
                .help("Show useful information for debugging")
                .long("debug"),
        )
        .arg(Arg::with_name("file").help("Input file name").index(1));
    let app_matches = app.clone().get_matches();

    let file_name = match app_matches.value_of("file") {
        Some(file_name) => file_name,
        None => {
            repl();
            return;
        }
    };

    // Normally run the given code
    if !app_matches.is_present("debug") {
        run(file_name);
        return;
    }

    // Show the information for debugging

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

    let mut lexer = lexer::Lexer::new(file_body.clone());

    println!("Lexer:");
    while let Ok(token) = lexer.next() {
        println!("{:?}", token);
    }

    let mut parser = parser::Parser::new(file_body);

    println!("Parser:");
    let mut node = match parser.parse_all() {
        Ok(ok) => ok,
        Err(NormalEOF) => unreachable!(),
        Err(Expect(pos, kind, msg))
        | Err(UnexpectedEOF(pos, kind, msg))
        | Err(UnexpectedToken(pos, kind, msg)) => {
            parser.show_error_at(pos, kind, msg.as_str());
            return;
        }
        Err(UnsupportedFeature(pos)) => {
            parser.enhanced_show_error_at(pos, "unsupported feature");
            return;
        }
    };
    println!("{:?}", node);

    extract_anony_func::AnonymousFunctionExtractor::new().run_toplevel(&mut node);
    // fv_finder::FreeVariableFinder::new().run_toplevel(&mut node);
    // println!("extract_anony_func, fv_finder:\n {:?}", node);
    // fv_solver::FreeVariableSolver::new().run_toplevel(&mut node);
    //
    println!("extract_anony_func:\n {:?}", node);

    let mut vm_codegen = vm_codegen::VMCodeGen::new();
    let mut iseq = vec![];
    vm_codegen.compile(&node, &mut iseq, false);

    bytecode_gen::show(&iseq);

    // println!("Result:");
    // let mut vm = vm::VM::new();
    // vm.global_objects.extend(vm_codegen.global_varmap);
    // vm.run(iseq);

    // println!("VM CodeGen Test:");
    // vm_codegen::test();
}

fn repl() {
    // TODO: REFINE CODE!!!!

    let mut vm_codegen = vm_codegen::VMCodeGen::new();
    let mut vm = vm::VM::new(vm_codegen.global_varmap);

    let mut rl = rustyline::Editor::<()>::new();

    loop {
        let line = match rl.readline("> ") {
            Ok(ref line) if line == ".." => {
                let mut lines = "".to_string();

                while let Ok(line) = rl.readline(">> ") {
                    if line == ".." {
                        break;
                    }
                    lines += line.as_str();
                }

                rl.add_history_entry(lines.as_ref());
                lines
            }
            Ok(line) => {
                rl.add_history_entry(line.as_ref());
                line
            }
            Err(_) => break,
        };

        let mut parser = parser::Parser::new(line);

        let mut node = match parser.parse_all() {
            Ok(ok) => ok,
            Err(NormalEOF) => unreachable!(),
            Err(Expect(pos, kind, msg))
            | Err(UnexpectedEOF(pos, kind, msg))
            | Err(UnexpectedToken(pos, kind, msg)) => {
                parser.show_error_at(pos, kind, msg.as_str());
                continue;
            }
            Err(UnsupportedFeature(pos)) => {
                parser.enhanced_show_error_at(pos, "unsupported feature");
                continue;
            }
        };

        extract_anony_func::AnonymousFunctionExtractor::new().run_toplevel(&mut node);

        let mut iseq = vec![];
        vm_codegen.compile(&node, &mut iseq, true);

        vm.const_table = vm_codegen.bytecode_gen.const_table.clone();
        vm.state.pc = 0;

        if let Err(e) = vm.run(iseq) {
            match e {
                RuntimeError::Unknown => vm::runtime_error("unknown error occurred"),
                RuntimeError::Unimplemented => vm::runtime_error("unimplemented feature"),
                RuntimeError::Reference(msg) | RuntimeError::Type(msg) => {
                    vm::runtime_error(msg.as_str())
                }
            }
            continue;
        }

        if let Some(value) = vm.state.stack.pop() {
            unsafe {
                builtin::debug_print(&value, false);
                libc::puts(b"\0".as_ptr() as *const i8);
            }
        }

        vm_codegen.bytecode_gen.const_table = vm.const_table.clone();
        vm.state.stack.clear();
    }
}

fn run(file_name: &str) {
    match fork() {
        Ok(ForkResult::Parent { child, .. }) => {
            match waitpid(child, None) {
                Ok(ok) => match ok {
                    WaitStatus::Exited(_, status) => if status != 0 {
                        panic!("exited. status: {}", status)
                    },
                    WaitStatus::Signaled(pid, status, _) => {
                        // We can do anything (like calling destructors) here.
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
                Err(NormalEOF) => unreachable!(),
                Err(Expect(pos, kind, msg))
                | Err(UnexpectedEOF(pos, kind, msg))
                | Err(UnexpectedToken(pos, kind, msg)) => {
                    parser.show_error_at(pos, kind, msg.as_str());
                    return;
                }
                Err(UnsupportedFeature(pos)) => {
                    parser.enhanced_show_error_at(pos, "unsupported feature");
                    return;
                }
            };

            extract_anony_func::AnonymousFunctionExtractor::new().run_toplevel(&mut node);

            let mut vm_codegen = vm_codegen::VMCodeGen::new();
            let mut iseq = vec![];
            vm_codegen.compile(&node, &mut iseq, false);

            let mut vm = vm::VM::new(vm_codegen.global_varmap);
            vm.const_table = vm_codegen.bytecode_gen.const_table;

            if let Err(e) = vm.run(iseq) {
                match e {
                    RuntimeError::Unknown => vm::runtime_error("unknown error occurred"),
                    RuntimeError::Unimplemented => vm::runtime_error("unimplemented feature"),
                    RuntimeError::Reference(msg) | RuntimeError::Type(msg) => {
                        vm::runtime_error(msg.as_str())
                    }
                }
            }
        }
        Err(e) => panic!("Rapidus Internal Error: fork failed: {:?}", e),
    }
}
