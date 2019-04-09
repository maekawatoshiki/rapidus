extern crate rapidus;
use rapidus::bytecode_gen;
use rapidus::parser;
use rapidus::{vm, vm::frame, vm::jsvalue::value::Value, vm::vm::VM2};

extern crate libc;

extern crate rustyline;

extern crate clap;
use clap::{App, Arg};

// extern crate nix;
// use nix::sys::wait::*;
// use nix::unistd::*;

// extern crate ansi_term;
// use ansi_term::Colour;

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
            repl();
            return;
        }
    };

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

    let mut vm = VM2::new();
    let mut iseq = vec![];
    let global_info = match vm.compile(&node, &mut iseq, false) {
        Ok(ok) => ok,
        Err(vm::codegen::Error { msg, token_pos, .. }) => {
            parser.show_error_at(token_pos, msg.as_str());
            return;
        }
    };

    println!("CodeGenerator generated:");
    bytecode_gen::show2(&iseq, &vm.constant_table);

    println!("Result:");
    if let Err(e) = vm.run_global(global_info, iseq) {
        e.show_error_message(Some(&parser.lexer));
    }

    for (i, val_boxed) in vm.stack.iter().enumerate() {
        let val: Value = (*val_boxed).into();
        println!("stack remaining: [{}]: {:?}", i, val);
    }
}

fn repl() {
    let mut rl = rustyline::Editor::<()>::new();
    let mut vm = VM2::new();
    let mut global_frame: Option<frame::Frame> = None;

    loop {
        let mut parser;

        let line = if let Ok(line) = rl.readline("> ") {
            line
        } else {
            break;
        };

        rl.add_history_entry(line.as_ref());

        let mut lines = line + "\n";

        loop {
            parser = parser::Parser::new(lines.clone());
            match parser.parse_all() {
                Ok(node) => {
                    // compile and execute
                    let mut iseq = vec![];
                    let global_info = match vm.compile(&node, &mut iseq, true) {
                        Ok(ok) => ok,
                        Err(vm::codegen::Error { msg, token_pos, .. }) => {
                            parser.show_error_at(token_pos, msg.as_str());
                            break;
                        }
                    };

                    match global_frame {
                        Some(ref mut frame) => {
                            frame.bytecode = iseq;
                            frame.exception_table = global_info.exception_table.clone();
                            frame.append_from_function_info(&mut vm.memory_allocator, &global_info)
                        }
                        None => global_frame = Some(vm.create_global_frame(global_info, iseq)),
                    }

                    if let Err(e) = vm.run(global_frame.clone().unwrap()) {
                        e.show_error_message(None);
                        break;
                    }

                    if vm.stack.len() != 0 {
                        let val: Value = vm.stack[0].into();
                        println!("{}", val.debug_string(true));
                        vm.stack = vec![];
                    }

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
                Err(e) => {
                    parser.handle_error(e);
                    break;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use rapidus::test::{assert_file, execute_script, test_code, test_file};

    #[test]
    fn vm_test() {
        // IMPORTANT: these tests should be run in a single thread.
        execute_script("for(var i = 0; i < 4; i++){ i }".to_string());
        // TODO: Following tests should run. Fix them ASAP.
        // assert_file("trinity".to_string());
        // assert_file("closure".to_string());
        // assert_file("fact".to_string());

        // assert_file("letconst".to_string());
        // assert_file("nested_block".to_string());
        // assert_file("nested_block2".to_string());
        // test_file(
        //     "array".to_string(),
        //     "'2,3,6,7,3,4,2,3,three1,5,4,1,2,three'".to_string(),
        // );
        // test_code("'true'*3".to_string(), "'truetruetrue'".to_string());
        // test_code("(100).toString(15)".to_string(), "'6a'".to_string());
        // test_file(
        //     "label".to_string(),
        //     "[ 0, 0, 0, 1, 0, 2, 1, 0, 2, 0, 3, 0, 3, 1, 4, 1, 4, 2, 0 ]".to_string(),
        // );

        // test_file("trycatch".to_string(), "[ 0, 2, 123, 10110 ]".to_string());
        //test_file(
        //    "qsort".to_string(),
        //    "[ 0, 0, 1, 3, 5, 7, 7, 10, 11, 12, 14, 14, 16, 17, 19 ]".to_string(),
        //);
        // test_file("arguments1".to_string(), "[[1,2,3,4,4],[1,2,[3,4]],[5,6,7,undefined,3],[5,6,[7]],[8,9,undefined,undefined,2],[8,9,undefined],[10,undefined,undefined,undefined,1],[10,undefined,undefined]]".to_string());
        // test_file(
        //     "arguments2".to_string(),
        //     "[10,15,20,25,15,10,'OK',20,25,'OK',10,'NG',20,25,'NG']".to_string(),
        // );
    }

    #[test]
    fn string_test1() {
        test_code(
            "'死して屍拾う者なし'[4]".to_string(),
            "'拾'".to_string(),
        );
    }

    #[test]
    fn string_test2() {
        test_code(
            "'死して屍拾う者なし'.length".to_string(),
            "9".to_string(),
        );
    }
    #[test]
    fn operator_test() {
        test_code("+(5>3)+60%7+(3>=5)+!!5+(-6)".to_string(), "0".to_string());
    }

    #[test]
    fn operator_test2() {
        assert_file("operator");
    }

    #[test]
    fn this_test() {
        test_file("this", "[1,101,124]".to_string());
    }

    #[test]
    fn prototype_test() {
        test_file(
            "prototypes",
            "[true,true,true,true,true,true,true,true,true,true]".to_string(),
        );
    }

    #[test]
    fn accessor_property() {
        test_file("accessor_property", "[0,123]".to_string())
    }

    #[test]
    fn trinity() {
        assert_file("trinity");
    }

    #[test]
    fn trycatch() {
        test_file("trycatch", "[0,2,123,10110]".to_string());
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
        test_code(
            "let f = (x) => { return x * x }; f(5)".to_string(),
            "25".to_string(),
        );
        test_code(
            "let f = x => { return x * x }; f(6)".to_string(),
            "36".to_string(),
        );
    }

    #[test]
    fn symbol() {
        assert_file("symbol")
    }

    #[test]
    fn array() {
        assert_file("array")
    }
}
