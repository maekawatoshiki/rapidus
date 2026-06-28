use rapidus_core::{bytecode_gen, vm, vm::exec_context, vm::vm::VM};
use rapidus_parser as parser;
extern crate clap;
use clap::{App, Arg};
use nu_ansi_term::{Color, Style};
use reedline::{
    FileBackedHistory, Highlighter, Prompt, PromptEditMode, PromptHistorySearch,
    PromptHistorySearchStatus, PromptViMode, Reedline, Signal, StyledText,
};
use std::{
    borrow::Cow,
    collections::VecDeque,
    io::{self, IsTerminal, Read, Write},
    path::PathBuf,
};

const VERSION_STR: &'static str = env!("CARGO_PKG_VERSION");
const HISTORY_FILE: &str = ".rapidus_history";

fn main() {
    let _ = color_eyre::install();

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
        .arg(
            Arg::with_name("module")
                .help("Parse input as an ECMAScript module")
                .long("module"),
        )
        .arg(Arg::with_name("file").help("Input file name").index(1));
    let app_matches = app.clone().get_matches();
    let is_debug = app_matches.is_present("debug");
    let is_profile = app_matches.is_present("profile");
    let is_trace = app_matches.is_present("trace");
    let is_module = app_matches.is_present("module");
    let file_name = match app_matches.value_of("file") {
        Some(file_name) => file_name,
        None => {
            repl(is_profile, is_trace);
            return;
        }
    };

    let mut parser = match parser::Parser::load_module(file_name) {
        Ok(ok) => ok,
        Err(_) => return,
    };

    let node = match if is_module {
        parser.parse_module()
    } else {
        parser.parse_all()
    } {
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
        Err(vm::codegen::Error { msg, loc, .. }) => {
            parser.show_error_at(loc, msg);
            return;
        }
    };

    if is_debug {
        println!("Codegen:");
        bytecode_gen::show_inst_seq(&global_info.code, &vm.constant_table);
    };

    let script_info = parser.into_script_info();
    vm.script_info
        .insert(global_info.module_func_id, script_info);
    if let Err(e) = vm.run_global(global_info) {
        vm.show_error_message(e);
    }
}

fn repl(is_profile: bool, is_trace: bool) {
    let mut reader = ReplReader::new();
    let prompt = RapidusPrompt::new("rapidus", "❯");
    let continuation_prompt = RapidusPrompt::new("      ", "…");

    if reader.is_interactive() {
        print_banner();
    }

    let mut vm = VM::new();
    if is_profile {
        vm = vm.profile();
    }
    if is_trace {
        vm = vm.trace();
    }
    let mut global_context: Option<exec_context::ExecContext> = None;
    let mut session_history = Vec::new();

    'repl: loop {
        let mut parser;

        let line = match reader.read_line(&prompt) {
            ReadLineStatus::Line(line) => line,
            ReadLineStatus::Continue => continue,
            ReadLineStatus::Exit => break,
        };

        match handle_repl_command(&line, &session_history) {
            ReplCommand::NotCommand => {}
            ReplCommand::Continue => continue,
            ReplCommand::Exit => break,
            ReplCommand::Clear => continue,
        }

        session_history.push(line.clone());

        let mut lines = line + "\n";

        loop {
            parser = parser::Parser::new("REPL", lines.clone());
            match parser.parse_all() {
                Ok(node) => {
                    // compile and execute
                    let global_info = match vm.compile(&node, true) {
                        Ok(ok) => ok,
                        Err(vm::codegen::Error { msg, loc, .. }) => {
                            parser.show_error_at(loc, msg);
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
                    vm.script_info =
                        vec![(vm.current_context.func_ref.module_func_id, script_info)]
                            .into_iter()
                            .collect();

                    match vm.run() {
                        Ok(val) => print_result(val.debug_string(true)),
                        Err(e) => {
                            let val = e.to_value(&mut vm.factory);
                            if val.is_error_object() {
                                print_runtime_error(val.get_property("message").to_string());
                            } else {
                                print_thrown(val.to_string())
                            }
                        }
                    }
                    break;
                }
                Err(parser::Error::UnexpectedEOF(_)) => {
                    match reader.read_line(&continuation_prompt) {
                        ReadLineStatus::Line(line) => {
                            match handle_repl_command(&line, &session_history) {
                                ReplCommand::NotCommand => {
                                    session_history.push(line.clone());
                                    lines += line.as_str();
                                    lines += "\n";
                                    continue;
                                }
                                ReplCommand::Continue => continue,
                                ReplCommand::Exit => break 'repl,
                                ReplCommand::Clear => break,
                            }
                        }
                        ReadLineStatus::Continue => continue,
                        ReadLineStatus::Exit => break 'repl,
                    }
                }
                Err(e) => {
                    parser.handle_error(&e);
                    break;
                }
            }
        }
    }
}

struct RapidusPrompt {
    left: &'static str,
    indicator: &'static str,
}

impl RapidusPrompt {
    fn new(left: &'static str, indicator: &'static str) -> Self {
        Self { left, indicator }
    }
}

impl Prompt for RapidusPrompt {
    fn render_prompt_left(&self) -> Cow<'_, str> {
        Cow::Borrowed(self.left)
    }

    fn render_prompt_right(&self) -> Cow<'_, str> {
        Cow::Borrowed("")
    }

    fn render_prompt_indicator(&self, edit_mode: PromptEditMode) -> Cow<'_, str> {
        match edit_mode {
            PromptEditMode::Vi(PromptViMode::Insert) => Cow::Borrowed(": "),
            _ => Cow::Owned(format!(" {} ", self.indicator)),
        }
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<'_, str> {
        Cow::Borrowed("… ")
    }

    fn render_prompt_history_search_indicator(
        &self,
        history_search: PromptHistorySearch,
    ) -> Cow<'_, str> {
        let status = match history_search.status {
            PromptHistorySearchStatus::Passing => "search",
            PromptHistorySearchStatus::Failing => "search failed",
        };
        Cow::Owned(format!("({}: {}) ", status, history_search.term))
    }

    fn get_prompt_color(&self) -> reedline::Color {
        reedline::Color::Green
    }

    fn get_indicator_color(&self) -> reedline::Color {
        reedline::Color::Cyan
    }

    fn get_prompt_multiline_color(&self) -> nu_ansi_term::Color {
        nu_ansi_term::Color::Cyan
    }
}

enum ReadLineStatus {
    Line(String),
    Continue,
    Exit,
}

enum ReplCommand {
    NotCommand,
    Continue,
    Exit,
    Clear,
}

struct ReplReader {
    line_editor: Option<Reedline>,
    scripted_lines: VecDeque<String>,
    plain: bool,
}

impl ReplReader {
    fn new() -> Self {
        if io::stdin().is_terminal() {
            let mut line_editor = Reedline::create().with_highlighter(Box::new(JsHighlighter));
            if let Some(path) = history_path() {
                if let Ok(history) = FileBackedHistory::with_file(1000, path) {
                    line_editor = line_editor.with_history(Box::new(history));
                }
            }
            Self {
                line_editor: Some(line_editor),
                scripted_lines: VecDeque::new(),
                plain: false,
            }
        } else {
            let mut source = String::new();
            let _ = io::stdin().read_to_string(&mut source);
            Self {
                line_editor: None,
                scripted_lines: source.lines().map(ToOwned::to_owned).collect(),
                plain: false,
            }
        }
    }

    fn is_interactive(&self) -> bool {
        self.line_editor.is_some()
    }

    fn read_line(&mut self, prompt: &RapidusPrompt) -> ReadLineStatus {
        if let Some(line_editor) = self.line_editor.as_mut() {
            match line_editor.read_line(prompt) {
                Ok(Signal::Success(line)) => ReadLineStatus::Line(line),
                Ok(Signal::CtrlC) => {
                    println!("{}", Style::new().dimmed().paint("^C"));
                    ReadLineStatus::Continue
                }
                Ok(Signal::CtrlD) => ReadLineStatus::Exit,
                Ok(_) => ReadLineStatus::Continue,
                Err(err) => {
                    println!(
                        "{} {}",
                        Style::new().bold().fg(Color::Red).paint("REPL error:"),
                        err
                    );
                    println!(
                        "{}",
                        Style::new()
                            .dimmed()
                            .paint("Falling back to plain line input.")
                    );
                    self.line_editor = None;
                    self.plain = true;
                    self.read_line(prompt)
                }
            }
        } else if self.plain {
            print!(
                "{} {} ",
                Style::new().bold().fg(Color::Green).paint(prompt.left),
                Style::new().bold().fg(Color::Cyan).paint(prompt.indicator)
            );
            let _ = io::stdout().flush();
            let mut line = String::new();
            match io::stdin().read_line(&mut line) {
                Ok(0) | Err(_) => ReadLineStatus::Exit,
                Ok(_) => ReadLineStatus::Line(line.trim_end_matches(['\r', '\n']).to_string()),
            }
        } else if let Some(line) = self.scripted_lines.pop_front() {
            ReadLineStatus::Line(line)
        } else {
            ReadLineStatus::Exit
        }
    }
}

fn history_path() -> Option<PathBuf> {
    std::env::var_os("HOME").map(|home| PathBuf::from(home).join(HISTORY_FILE))
}

fn handle_repl_command(line: &str, history: &[String]) -> ReplCommand {
    match line.trim() {
        ".help" => {
            print_help();
            ReplCommand::Continue
        }
        ".exit" | ".quit" => ReplCommand::Exit,
        ".clear" => ReplCommand::Clear,
        ".history" => {
            if history.is_empty() {
                println!("{}", Style::new().dimmed().paint("No entries yet."));
            } else {
                for (index, entry) in history.iter().enumerate() {
                    println!(
                        "{}  {}",
                        Style::new().dimmed().paint(format!("{:>4}", index + 1)),
                        entry
                    );
                }
            }
            ReplCommand::Continue
        }
        command if command.starts_with('.') => {
            println!(
                "{} {}",
                Style::new().bold().fg(Color::Red).paint("Unknown command:"),
                command
            );
            println!("{}", Style::new().dimmed().paint("Type .help for help."));
            ReplCommand::Continue
        }
        _ => ReplCommand::NotCommand,
    }
}

fn print_banner() {
    println!(
        "{}",
        Style::new()
            .bold()
            .fg(Color::Cyan)
            .paint(format!("Rapidus {}", VERSION_STR))
    );
    println!(
        "{}",
        Style::new()
            .dimmed()
            .paint("JavaScript engine REPL — type .help for help, Ctrl-D to quit")
    );
}

fn print_help() {
    println!("{}", Style::new().bold().fg(Color::Cyan).paint("Commands"));
    println!("  {}      show this help", Color::Yellow.paint(".help"));
    println!("  {}      exit the REPL", Color::Yellow.paint(".exit"));
    println!("  {}      exit the REPL", Color::Yellow.paint(".quit"));
    println!(
        "  {}   show commands entered in this session",
        Color::Yellow.paint(".history")
    );
    println!(
        "  {}     discard the current multiline input",
        Color::Yellow.paint(".clear")
    );
}

fn print_result(value: String) {
    println!(
        "{} {}",
        Style::new().bold().fg(Color::Purple).paint("=>"),
        value
    );
}

fn print_runtime_error(message: String) {
    println!(
        "{} {}",
        Style::new().bold().fg(Color::Red).paint("Uncaught Error:"),
        message
    );
}

fn print_thrown(value: String) {
    println!(
        "{} {}",
        Style::new().bold().fg(Color::Red).paint("Uncaught thrown:"),
        value
    );
}

struct JsHighlighter;

impl Highlighter for JsHighlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
        highlight_js_line(line)
    }

    fn is_inside_string_literal(&self, line: &str, cursor: usize) -> bool {
        is_inside_js_string(line, cursor)
    }
}

fn highlight_js_line(line: &str) -> StyledText {
    let mut out = StyledText::new();
    if line.trim_start().starts_with('.') {
        push_span(&mut out, command_style(), line);
        return out;
    }

    let bytes = line.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let start = i;
        match bytes[i] {
            b'\'' | b'"' | b'`' => {
                let quote = bytes[i];
                i += 1;
                let mut escaped = false;
                while i < bytes.len() {
                    let byte = bytes[i];
                    i += 1;
                    if escaped {
                        escaped = false;
                    } else if byte == b'\\' {
                        escaped = true;
                    } else if byte == quote {
                        break;
                    }
                }
                push_span(&mut out, string_style(), &line[start..i]);
            }
            b'/' if i + 1 < bytes.len() && bytes[i + 1] == b'/' => {
                push_span(&mut out, comment_style(), &line[i..]);
                break;
            }
            b'/' if i + 1 < bytes.len() && bytes[i + 1] == b'*' => {
                i += 2;
                while i + 1 < bytes.len() && !(bytes[i] == b'*' && bytes[i + 1] == b'/') {
                    i += 1;
                }
                if i + 1 < bytes.len() {
                    i += 2;
                }
                push_span(&mut out, comment_style(), &line[start..i]);
            }
            byte if byte.is_ascii_digit() => {
                i = consume_number(bytes, i);
                push_span(&mut out, number_style(), &line[start..i]);
            }
            byte if is_ident_start(byte) => {
                i += 1;
                while i < bytes.len() && is_ident_continue(bytes[i]) {
                    i += 1;
                }
                let word = &line[start..i];
                push_span(
                    &mut out,
                    style_for_word(word).unwrap_or_else(plain_style),
                    word,
                );
            }
            byte if is_operator_byte(byte) => {
                i += 1;
                while i < bytes.len() && is_operator_byte(bytes[i]) {
                    i += 1;
                }
                push_span(&mut out, operator_style(), &line[start..i]);
            }
            _ => {
                let ch = line[i..].chars().next().unwrap();
                i += ch.len_utf8();
                push_span(&mut out, plain_style(), &line[start..i]);
            }
        }
    }

    out
}

fn push_span(out: &mut StyledText, style: Style, text: &str) {
    if !text.is_empty() {
        out.push((style, text.to_string()));
    }
}

fn plain_style() -> Style {
    Style::new()
}

fn keyword_style() -> Style {
    Style::new().bold().fg(Color::Yellow)
}

fn literal_style() -> Style {
    Style::new().fg(Color::Cyan)
}

fn string_style() -> Style {
    Style::new().fg(Color::Green)
}

fn number_style() -> Style {
    Style::new().fg(Color::Purple)
}

fn comment_style() -> Style {
    Style::new().dimmed()
}

fn operator_style() -> Style {
    Style::new().fg(Color::Cyan)
}

fn command_style() -> Style {
    Style::new().bold().fg(Color::Yellow)
}

fn style_for_word(word: &str) -> Option<Style> {
    match word {
        "true" | "false" | "null" | "undefined" | "NaN" | "Infinity" => Some(literal_style()),
        "async" | "await" | "break" | "case" | "catch" | "class" | "const" | "continue"
        | "debugger" | "default" | "delete" | "do" | "else" | "export" | "extends" | "finally"
        | "for" | "from" | "function" | "if" | "import" | "in" | "instanceof" | "let" | "new"
        | "of" | "return" | "super" | "switch" | "this" | "throw" | "try" | "typeof" | "var"
        | "void" | "while" | "with" | "yield" => Some(keyword_style()),
        _ => None,
    }
}

fn is_ident_start(byte: u8) -> bool {
    byte == b'_' || byte == b'$' || byte.is_ascii_alphabetic()
}

fn is_ident_continue(byte: u8) -> bool {
    is_ident_start(byte) || byte.is_ascii_digit()
}

fn is_operator_byte(byte: u8) -> bool {
    matches!(
        byte,
        b'+' | b'-'
            | b'*'
            | b'/'
            | b'%'
            | b'='
            | b'!'
            | b'<'
            | b'>'
            | b'&'
            | b'|'
            | b'^'
            | b'~'
            | b'?'
            | b':'
            | b'.'
            | b','
            | b';'
            | b'('
            | b')'
            | b'['
            | b']'
            | b'{'
            | b'}'
    )
}

fn consume_number(bytes: &[u8], mut i: usize) -> usize {
    if i + 1 < bytes.len() && bytes[i] == b'0' && matches!(bytes[i + 1], b'x' | b'X') {
        i += 2;
        while i < bytes.len() && (bytes[i].is_ascii_hexdigit() || bytes[i] == b'_') {
            i += 1;
        }
        return i;
    }

    while i < bytes.len() && (bytes[i].is_ascii_digit() || bytes[i] == b'_') {
        i += 1;
    }
    if i < bytes.len() && bytes[i] == b'.' {
        i += 1;
        while i < bytes.len() && (bytes[i].is_ascii_digit() || bytes[i] == b'_') {
            i += 1;
        }
    }
    if i < bytes.len() && matches!(bytes[i], b'e' | b'E') {
        let exp_start = i;
        i += 1;
        if i < bytes.len() && matches!(bytes[i], b'+' | b'-') {
            i += 1;
        }
        let digits_start = i;
        while i < bytes.len() && (bytes[i].is_ascii_digit() || bytes[i] == b'_') {
            i += 1;
        }
        if i == digits_start {
            i = exp_start;
        }
    }
    i
}

fn is_inside_js_string(line: &str, cursor: usize) -> bool {
    let cursor = cursor.min(line.len());
    let mut quote = None;
    let mut escaped = false;

    for (idx, ch) in line.char_indices() {
        if idx >= cursor {
            break;
        }
        if escaped {
            escaped = false;
            continue;
        }
        if ch == '\\' {
            escaped = true;
            continue;
        }
        match quote {
            Some(q) if ch == q => quote = None,
            Some(_) => {}
            None if ch == '\'' || ch == '"' || ch == '`' => quote = Some(ch),
            None => {}
        }
    }

    quote.is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn highlighted_text(line: &str) -> String {
        highlight_js_line(line)
            .buffer
            .into_iter()
            .map(|(_, text)| text)
            .collect()
    }

    #[test]
    fn highlighter_preserves_input_text() {
        let line = "let answer = 40 + 2 // comment";
        assert_eq!(highlighted_text(line), line);
    }

    #[test]
    fn string_detection_handles_escapes() {
        assert!(is_inside_js_string("'hello", 3));
        assert!(!is_inside_js_string("'hello'", 7));
        assert!(is_inside_js_string("'\\''", 3));
    }

    #[test]
    fn consume_number_handles_hex_and_float() {
        assert_eq!(consume_number(b"0xff + 1", 0), 4);
        assert_eq!(consume_number(b"12.34e-5;", 0), 8);
    }

    #[test]
    fn highlighter_uses_gruvbox_dark_palette() {
        assert!(keyword_style().paint("x").to_string().contains("[1;33m"));
        assert!(string_style().paint("x").to_string().contains("[32m"));
        assert!(number_style().paint("x").to_string().contains("[35m"));
        assert!(operator_style().paint("x").to_string().contains("[36m"));
    }
}
