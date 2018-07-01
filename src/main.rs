extern crate rapidus;
use rapidus::lexer;
use rapidus::parser;
use rapidus::vm;

extern crate clap;
use clap::{App, Arg};

use std::fs::OpenOptions;
use std::io::prelude::*;

const VERSION_STR: &'static str = env!("CARGO_PKG_VERSION");

fn main() {
    let app = App::new("Rapidus")
        .version(VERSION_STR)
        .author("uint256_t")
        .about("A toy JavaScript engine")
        .arg(Arg::with_name("file").help("Input file name").index(1));
    let app_matches = app.clone().get_matches();

    if let Some(filename) = app_matches.value_of("file") {
        let mut file_body = String::new();

        match OpenOptions::new().read(true).open(filename) {
            Ok(mut ok) => ok.read_to_string(&mut file_body)
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
        while let Ok(node) = parser.next() {
            println!("{:?}", node);
        }

        vm::test();
    }
}
