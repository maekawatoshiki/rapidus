use std::path::PathBuf;

use rapidus_parser::{
    lexer::{Input, Lexer},
    source::{Source, SourceName},
};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "lex")]
pub struct Opt {
    #[structopt(parse(from_os_str))]
    pub file: PathBuf,
}

fn main() {
    env_logger::init();
    color_backtrace::install();

    let opt = Opt::from_args();
    let source = Source::new(
        SourceName::FileName(opt.file.into_os_string().into_string().unwrap()),
        r#"for (let i = 1; i < 20; i++) {
    if (i % 15 == 0) console.log("FizzBuzz");
    else if (i % 3 == 0) console.log("Fizz");
    else if (i % 5 == 0) console.log("Buzz");
    else console.log(i);
}"#,
    );
    let mut lexer = Lexer::new(Input::from(&source));

    while let Ok(Some(token)) = lexer.read_token() {
        log::info!("{:?}", token);
    }
}
