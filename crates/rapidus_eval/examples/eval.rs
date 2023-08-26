use std::{fs, path::PathBuf};

use ecow::EcoString;
use rapidus_eval::eval::EvalCtx;
use rapidus_parser::{
    error::{Error, SyntaxError},
    lexer::{Input, Lexer},
    parser::Parser,
    source::{Source, SourceName},
};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "compile")]
pub struct Opt {
    #[structopt(parse(from_os_str), help = "File path to evaluate")]
    pub filepath: Option<PathBuf>,
}

fn main() {
    env_logger::init();
    color_backtrace::install();

    let opt = Opt::from_args();

    if let Some(filepath) = opt.filepath {
        let src = fs::read_to_string(filepath).expect("Failed to read file");
        eval_str(src.into());
    } else {
        eval_str("1;".into());
    }
}

fn eval_str(text: EcoString) {
    let src = Source {
        name: SourceName::Custom("test".into()),
        text,
    };
    let lexer = Lexer::new(Input::from(&src));
    let module = match Parser::new(lexer).parse_module() {
        Ok(module) => module,
        Err(Error::SyntaxError(SyntaxError::UnexpectedToken(tok))) => {
            panic!("{}", src.error_message_at(tok.0, "Unexpected token"));
        }
        Err(e) => panic!("{e:?}"),
    };
    println!(
        "Result: {:#?}",
        EvalCtx::new().eval_module(&module).unwrap()
    );
}
