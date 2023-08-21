use std::{fs, path::Path};

use indicatif::{ProgressBar, ProgressStyle};
use rapidus_parser::{
    lexer::{Input, Lexer},
    source::{Source, SourceName},
};

fn main() {
    env_logger::init();
    color_backtrace::install();

    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../test262/test");

    let num_expected_tests = 49795;
    let bar = ProgressBar::new(num_expected_tests);
    bar.set_style(
        ProgressStyle::default_bar()
            .template("{bar:60} {pos:>5}/{len:>5} {msg}")
            .unwrap(),
    );

    let mut passed = 0;
    let mut failed = 0;
    let tests = glob::glob(format!("{}/**/*.js", root.display()).as_str())
        .unwrap()
        .filter_map(Result::ok);
    for i in tests {
        let filename = i.clone().into_os_string().into_string().unwrap();
        let content = fs::read_to_string(&i).unwrap();
        let len = content.len();
        let source = Source::new(SourceName::FileName(filename), content);
        let mut lexer = Lexer::new(Input::from(&source));

        let mut num_tokens = 0;
        while let Ok(Some(_token)) = lexer.read_token() {
            if num_tokens > len {
                // This means that the lexer is in an infinite loop.
                break;
            }
            num_tokens += 1;
        }

        if matches!(lexer.read_token(), Ok(None)) {
            passed += 1;
        } else {
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
