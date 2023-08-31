use std::{
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
};

use indicatif::{ProgressBar, ProgressStyle};
use rapidus_parser::{
    lexer::{Input, Lexer},
    parser::Parser,
    source::{Source, SourceName},
};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "compile")]
pub struct Opt {
    #[structopt(parse(from_os_str), help = "File path to lex")]
    pub parse_target: Option<PathBuf>,

    #[structopt(long, help = "File path to dump failed tests to")]
    pub dump_failed_tests: Option<PathBuf>,
}

fn main() {
    env_logger::init();
    color_backtrace::install();

    let opt = Opt::from_args();

    if let Some(filepath) = opt.parse_target {
        parse_file(filepath);
    } else {
        parse_test262(opt.dump_failed_tests);
    }
}

fn parse_file(filepath: PathBuf) {
    let filename = filepath.clone().into_os_string().into_string().unwrap();
    let content = fs::read_to_string(&filepath).expect("Failed to read file");
    let source = Source::new(SourceName::FileName(filename), content);
    let lexer = Lexer::new(Input::from(&source));
    let mut parser = Parser::new(lexer);

    match parser.parse_module() {
        Ok(module) => println!("\x1b[0;32mPASSED\x1b[0m: {module:#?}"),
        Err(e) => {
            println!("\x1b[0;31mFAILED\x1b[0m: {e:?}");
        }
    }
}

fn parse_test262(dump_failed_tests: Option<PathBuf>) {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../test262/test");
    let num_expected_tests = 45554; // Does not include tests that raise 'type: SyntaxError'
    let bar = ProgressBar::new(num_expected_tests);
    bar.set_style(
        ProgressStyle::default_bar()
            .template("{bar:60} {pos:>5}/{len:>5} {msg}")
            .unwrap(),
    );
    let mut dump_file = dump_failed_tests.map(|path| File::create(path).unwrap());

    let mut passed = 0;
    let mut failed = 0;
    let tests = glob::glob(format!("{}/**/*.js", root.display()).as_str())
        .unwrap()
        .filter_map(Result::ok);
    for i in tests {
        let filename = i.clone().into_os_string().into_string().unwrap();
        let content = fs::read_to_string(&i).unwrap();
        if content.contains("type: SyntaxError") {
            continue;
        }
        // let len = content.len();
        let source = Source::new(SourceName::FileName(filename), content);
        let lexer = Lexer::new(Input::from(&source));
        let mut parser = Parser::new(lexer);

        let module = parser.parse_module();
        if matches!(module, Ok(_)) {
            passed += 1;
        } else {
            let SourceName::FileName(filename) = source.name else {
                panic!()
            };
            if let Some(file) = dump_file.as_mut() {
                writeln!(file, "{}", filename).expect("Failed to write dump file");
            }
            failed += 1
        }

        bar.inc(1);
    }

    assert_eq!(passed + failed, num_expected_tests);

    println!(
        "passed: \x1b[0;32m{} ({}%)\x1b[0m, failed: \x1b[0;31m{} ({}%)\x1b[0m, total: {}",
        passed,
        passed * 100 / (passed + failed),
        failed,
        failed * 100 / (passed + failed),
        passed + failed
    );
}