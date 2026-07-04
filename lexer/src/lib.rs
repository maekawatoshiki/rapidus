pub mod token;

use crate::token::{convert_reserved_keyword, Keyword, Kind, Symbol, TemplatePart, Token};
use rapidus_ast::loc::SourceLoc;

use std::collections::VecDeque;

// TODO: Simplify
#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    NormalEOF,
    UnexpectedEOF(String),              // error msg
    UnexpectedToken(SourceLoc, String), // position, error msg
    UnsupportedFeature(SourceLoc),      // position
    Expect(SourceLoc, String),          // position, error msg
    InvalidToken(SourceLoc),
    General(SourceLoc, String),
}

#[derive(Clone, Debug)]
pub struct Lexer {
    pub code: String,

    /// Current positon in code.
    pub loc: SourceLoc,

    /// Hold all tokens
    pub buf: VecDeque<Token>,

    /// Current position in ``buf``.
    pub token_pos: usize,

    /// Previous position of ``token_pos``
    pub prev_token_pos: usize,

    /// Saved states
    pub states: Vec<usize>,

    /// Whether an Annex B HTML close comment may start at the current position.
    html_close_allowed: bool,
}

impl Lexer {
    pub fn new(code: String) -> Lexer {
        Lexer {
            code,
            loc: SourceLoc::default(),
            buf: VecDeque::new(),
            token_pos: 0,
            prev_token_pos: 0,
            states: vec![],
            html_close_allowed: false,
        }
    }

    /// Tokenize all the script
    pub fn tokenize_all(&mut self) -> Result<(), Error> {
        loop {
            match self.tokenize() {
                Ok(tok) => self.buf.push_back(tok),
                Err(Error::NormalEOF) => break,
                Err(err) => {
                    // When error occurs in tokenizer, pos_line_list is not completed.
                    // self.skip_char_while(|c| c != '\n')?;
                    // self.take_char().unwrap_or(' ');
                    return Err(err);
                }
            };
        }

        Ok(())
    }

    pub fn print_buf(&self) {
        for tok in &self.buf {
            println!("{:?}", tok);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.token_pos >= self.buf.len()
    }
}

impl Lexer {
    /// Get next token.
    /// No skipping line terminator.
    pub fn next(&mut self) -> Result<Token, Error> {
        self.prev_token_pos = self.token_pos;
        self.read_token()
    }

    /// Get the next token.
    /// Skipping line terminators.
    pub fn next_skip_lineterminator(&mut self) -> Result<Token, Error> {
        self.prev_token_pos = self.token_pos;
        loop {
            let tok = self.read_token()?;
            if tok.kind != Kind::LineTerminator {
                return Ok(tok);
            }
        }
    }

    /// Skip line terminators.
    /// Return Err(Error::NormalEOF) when reached EOF.
    pub fn skip_lineterminator(&mut self) -> Result<(), Error> {
        let len = self.buf.len();
        for i in self.token_pos..len {
            let tok = self.buf[i].clone();
            if tok.kind != Kind::LineTerminator {
                self.token_pos = i;
                return Ok(());
            }
        }
        Err(Error::NormalEOF)
    }

    /// Peek the next token.
    /// Skipping line terminators.
    pub fn peek_skip_lineterminator(&mut self) -> Result<Token, Error> {
        let len = self.buf.len();
        for i in self.token_pos..len {
            let tok = self.buf[i].clone();
            if tok.kind != Kind::LineTerminator {
                return Ok(tok);
            }
        }
        Err(Error::NormalEOF)
    }

    /// Peek the token specified by index.
    /// Return the next token when index = 0.
    pub fn peek(&mut self, index: usize) -> Result<Token, Error> {
        let index_in_buf = self.token_pos + index;
        if index_in_buf < self.buf.len() {
            Ok(self.buf[index_in_buf].clone())
        } else {
            Err(Error::NormalEOF)
        }
    }

    /// Peek the previous token
    pub fn peek_prev(&mut self) -> Token {
        let index_in_buf = self.token_pos - 1;
        self.buf[index_in_buf].clone()
    }

    /// Get char position in the script of the next token
    pub fn get_current_loc(&mut self) -> SourceLoc {
        if self.token_pos < self.buf.len() {
            self.buf[self.token_pos].loc
        } else {
            self.loc
        }
    }

    // /// Get char position in the script of previous token.
    // pub fn get_prev_loc(&mut self) -> SourceLoc {
    //     if self.token_pos < self.buf.len() {
    //         self.buf[self.token_pos].prev_pos
    //     } else {
    //         self.pos - 1
    //     }
    // }

    /// Skips the current token and return `Ok(true)` only if its kind is `kind`.
    /// Ignores `Kind::LineTerminator`.
    pub fn skip<K: Into<Kind>>(&mut self, kind: K) -> Result<bool, Error> {
        match self.peek_skip_lineterminator() {
            Ok(tok) => {
                let eq = tok.kind == kind.into();
                if eq {
                    self.next_skip_lineterminator()?;
                }
                Ok(eq)
            }
            Err(e) => Err(e),
        }
    }

    /// Skips the current token and return `Ok(true)` only if its kind is `kind`.
    /// Does not ignore `Kind::LineTerminator`.
    pub fn skip2<K: Into<Kind>>(&mut self, kind: K) -> Result<bool, Error> {
        match self.peek(0) {
            Ok(tok) => {
                let eq = tok.kind == kind.into();
                if eq {
                    self.read_token()?;
                }
                Ok(eq)
            }
            Err(e) => Err(e),
        }
    }

    /// Revert the previous ``next()`` or ``next_skip_lineterminator()``.
    /// Does not work for ``next_if()``.
    pub fn unget(&mut self) {
        self.token_pos = self.prev_token_pos;
    }

    /// Read token
    fn read_token(&mut self) -> Result<Token, Error> {
        if self.token_pos < self.buf.len() {
            let pos = self.token_pos;
            self.token_pos += 1;
            Ok(self.buf[pos].clone())
        } else {
            Err(Error::NormalEOF)
        }
    }
}

///
/// Tokenizer
///
impl Lexer {
    /// Tokenize and return the token
    fn tokenize(&mut self) -> Result<Token, Error> {
        loop {
            if self.loc.pos == 0 && self.starts_with("#!") {
                self.skip_line_comment()?;
            } else if self.starts_with("//") || self.starts_with("<!--") {
                self.skip_line_comment()?;
            } else if self.starts_with("/*") {
                self.skip_normal_comment()?;
            } else if self.starts_with("-->") && self.is_html_close_comment_start() {
                self.skip_line_comment()?;
            } else {
                break;
            }
        }

        let tok = match self.peek_char()? {
            c if is_identifier_start_char(c) || c == '\\' => self.read_identifier(),
            '0'..='9' => self.read_number(),
            '.' if self
                .code
                .get(self.loc.pos + 1..)
                .and_then(|rest| rest.chars().next())
                .map(|c| c.is_ascii_digit())
                .unwrap_or(false) =>
            {
                self.read_number()
            }
            '\'' | '\"' => self.read_string_literal(),
            '`' => self.read_template_literal(),
            c if is_line_terminator(c) => self.read_line_terminator(),
            '/' if self.regexp_literal_allowed() => {
                let loc = self.loc;
                self.take_char()?;
                let (pattern, flags) = self.read_regexp_literal_after_slash()?;
                Ok(Token::new_regexp(pattern, flags, loc))
            }
            c if is_whitespace(c) => {
                self.skip_whitespace()?;
                return self.tokenize();
            }
            _ => self.read_symbol(),
        }?;

        if tok.kind != Kind::LineTerminator {
            self.html_close_allowed = false;
        }

        Ok(tok)
    }
}

impl Lexer {
    fn is_html_close_comment_start(&self) -> bool {
        if self.html_close_allowed {
            return true;
        }

        let line_start = self.code[..self.loc.pos]
            .rfind('\n')
            .map(|i| i + 1)
            .unwrap_or(0);
        self.code[line_start..self.loc.pos]
            .chars()
            .all(|c| c == ' ' || c == '\t')
    }

    fn skip_line_comment(&mut self) -> Result<(), Error> {
        self.skip_char_while(|c| !is_line_terminator(c))
    }

    fn skip_normal_comment(&mut self) -> Result<(), Error> {
        let close_comment_allowed_before_comment = self.is_html_close_comment_start();
        let mut last_char = ' ';
        let mut line = self.loc.line;
        let mut has_line_terminator = false;
        self.skip_char_while(|c| {
            if is_line_terminator(c) {
                line += 1;
                has_line_terminator = true;
            }
            let end_of_comment = last_char == '*' && c == '/';
            last_char = c;
            !end_of_comment
        })?;
        self.loc.line = line;
        assert_eq!(self.take_char()?, '/');
        self.html_close_allowed = close_comment_allowed_before_comment || has_line_terminator;
        Ok(())
    }
}

impl Lexer {
    fn regexp_literal_allowed(&self) -> bool {
        let Some(tok) = self
            .buf
            .iter()
            .rev()
            .find(|tok| tok.kind != Kind::LineTerminator)
        else {
            return true;
        };
        match tok.kind {
            Kind::Identifier(_)
            | Kind::Number(_)
            | Kind::BigInt(_)
            | Kind::String(_)
            | Kind::RegExp(_, _) => false,
            Kind::Template(_) => false,
            Kind::Keyword(Keyword::This) => false,
            Kind::Symbol(Symbol::ClosingParen | Symbol::ClosingBoxBracket) => false,
            _ => true,
        }
    }
}

fn is_identifier_start_char(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c == '$' || c == '\u{2118}'
}

fn is_identifier_part_char(c: char) -> bool {
    is_identifier_start_char(c) || c.is_numeric() || c == '\u{200c}' || c == '\u{200d}'
}

fn is_line_terminator(c: char) -> bool {
    matches!(c, '\n' | '\r' | '\u{2028}' | '\u{2029}')
}

fn is_whitespace(c: char) -> bool {
    (c.is_whitespace() || c == '\u{feff}') && !is_line_terminator(c)
}

impl Lexer {
    fn read_identifier(&mut self) -> Result<Token, Error> {
        let loc = self.loc;

        let mut ident = String::new();
        let mut first = true;
        let mut contains_escape = false;
        while !self.eof() {
            let c = self.peek_char()?;
            let next = if c == '\\' {
                contains_escape = true;
                self.take_char()?;
                if self.peek_char()? != 'u' {
                    return Err(Error::General(
                        self.loc,
                        "invalid identifier escape sequence".to_string(),
                    ));
                }
                let chars = self.read_escaped_char()?;
                if chars.len() != 1 {
                    return Err(Error::General(
                        self.loc,
                        "invalid identifier escape sequence".to_string(),
                    ));
                }
                chars[0]
            } else if (first && is_identifier_start_char(c))
                || (!first && is_identifier_part_char(c))
            {
                self.take_char()?
            } else {
                break;
            };

            if (first && !is_identifier_start_char(next))
                || (!first && !is_identifier_part_char(next))
            {
                return Err(Error::General(
                    self.loc,
                    "invalid identifier escape sequence".to_string(),
                ));
            }
            ident.push(next);
            first = false;
        }

        if ident.is_empty() {
            return Err(Error::General(
                loc,
                "invalid identifier escape sequence".to_string(),
            ));
        }
        if contains_escape {
            Ok(Token::new_escaped_identifier(ident, loc))
        } else if let Some(keyword) = convert_reserved_keyword(ident.as_str()) {
            Ok(Token::new_keyword(keyword, loc))
        } else {
            Ok(Token::new_identifier(ident, loc))
        }
    }

    pub fn read_regexp_literal_after_slash(&mut self) -> Result<(String, String), Error> {
        let loc = self.loc;
        let mut pattern = String::new();
        let mut escaped = false;
        let mut in_class = false;

        while !self.eof() {
            let c = self.take_char()?;
            if matches!(c, '\n' | '\r' | '\u{2028}' | '\u{2029}') {
                return Err(Error::General(
                    loc,
                    "unterminated regular expression literal".to_string(),
                ));
            }
            if escaped {
                pattern.push(c);
                escaped = false;
                continue;
            }
            match c {
                '\\' => {
                    pattern.push(c);
                    escaped = true;
                }
                '[' => {
                    pattern.push(c);
                    in_class = true;
                }
                ']' => {
                    pattern.push(c);
                    in_class = false;
                }
                '/' if !in_class => {
                    let flags = self.take_char_while(is_identifier_part_char)?;
                    validate_regexp_flags(&flags, loc)?;
                    validate_regexp_named_groups(&pattern, loc)?;
                    validate_unicode_set_class_syntax(&pattern, &flags, loc)?;
                    validate_unicode_property_escapes(&pattern, &flags, loc)?;
                    return Ok((pattern, flags));
                }
                _ => pattern.push(c),
            }
        }

        Err(Error::General(
            loc,
            "unterminated regular expression literal".to_string(),
        ))
    }
}

fn validate_regexp_flags(flags: &str, loc: SourceLoc) -> Result<(), Error> {
    let mut seen = Vec::new();
    for flag in flags.chars() {
        if !matches!(flag, 'd' | 'g' | 'i' | 'm' | 's' | 'u' | 'v' | 'y') {
            return Err(Error::General(
                loc,
                "invalid regular expression flag".to_string(),
            ));
        }
        if seen.contains(&flag) {
            return Err(Error::General(
                loc,
                "duplicate regular expression flag".to_string(),
            ));
        }
        seen.push(flag);
    }
    if flags.contains('u') && flags.contains('v') {
        return Err(Error::General(
            loc,
            "invalid regular expression flags".to_string(),
        ));
    }
    Ok(())
}

fn validate_regexp_named_groups(pattern: &str, loc: SourceLoc) -> Result<(), Error> {
    let chars = pattern.chars().collect::<Vec<_>>();
    let mut group_names = Vec::new();
    let mut refs = Vec::new();
    let mut i = 0usize;
    let mut escaped = false;

    while i < chars.len() {
        let ch = chars[i];
        if escaped {
            if ch == 'k' {
                let (name, next) = read_regexp_group_name(&chars, i + 1, loc)?;
                refs.push(name);
                i = next;
            } else {
                i += 1;
            }
            escaped = false;
            continue;
        }

        if ch == '\\' {
            escaped = true;
            i += 1;
            continue;
        }

        if ch == '('
            && chars.get(i + 1) == Some(&'?')
            && chars.get(i + 2) == Some(&'<')
            && chars.get(i + 3) != Some(&'=')
            && chars.get(i + 3) != Some(&'!')
        {
            let (name, next) = read_regexp_group_name(&chars, i + 2, loc)?;
            if group_names.iter().any(|existing| existing == &name) {
                return Err(invalid_regexp_named_group(loc));
            }
            group_names.push(name);
            i = next;
            continue;
        }

        i += 1;
    }

    for name in refs {
        if !group_names.iter().any(|existing| existing == &name) {
            return Err(invalid_regexp_named_group(loc));
        }
    }
    Ok(())
}

fn validate_unicode_set_class_syntax(
    pattern: &str,
    flags: &str,
    loc: SourceLoc,
) -> Result<(), Error> {
    if !flags.contains('v') {
        return Ok(());
    }

    let chars = pattern.chars().collect::<Vec<_>>();
    let mut in_class = false;
    let mut escaped = false;
    let mut i = 0usize;
    while i < chars.len() {
        let ch = chars[i];
        if escaped {
            escaped = false;
            i += 1;
            continue;
        }
        if ch == '\\' {
            escaped = true;
            i += 1;
            continue;
        }
        if !in_class {
            if ch == '[' {
                in_class = true;
            }
            i += 1;
            continue;
        }
        if ch == ']' {
            in_class = false;
            i += 1;
            continue;
        }
        if matches!(ch, '(' | ')' | '[' | '{' | '}' | '/' | '-' | '|') {
            return Err(invalid_unicode_set_pattern(loc));
        }
        if is_unicode_set_reserved_double_punctuator(ch) && chars.get(i + 1).copied() == Some(ch) {
            return Err(invalid_unicode_set_pattern(loc));
        }
        i += 1;
    }
    Ok(())
}

fn is_unicode_set_reserved_double_punctuator(ch: char) -> bool {
    matches!(
        ch,
        '!' | '#'
            | '$'
            | '%'
            | '&'
            | '*'
            | '+'
            | ','
            | '.'
            | ':'
            | ';'
            | '<'
            | '='
            | '>'
            | '?'
            | '@'
            | '^'
            | '`'
            | '~'
    )
}

fn invalid_unicode_set_pattern(loc: SourceLoc) -> Error {
    Error::General(
        loc,
        "invalid regular expression unicode set pattern".to_string(),
    )
}

fn read_regexp_group_name(
    chars: &[char],
    start: usize,
    loc: SourceLoc,
) -> Result<(String, usize), Error> {
    if chars.get(start) != Some(&'<') {
        return Err(invalid_regexp_named_group(loc));
    }

    let mut name = String::new();
    let mut index = start + 1;
    while let Some(ch) = chars.get(index) {
        if *ch == '>' {
            if regexp_identifier_name_is_valid(&name) {
                return Ok((name, index + 1));
            }
            return Err(invalid_regexp_named_group(loc));
        }
        name.push(*ch);
        index += 1;
    }

    Err(invalid_regexp_named_group(loc))
}

fn regexp_identifier_name_is_valid(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !is_regexp_identifier_start(first) {
        return false;
    }
    chars.all(is_regexp_identifier_continue)
}

fn is_regexp_identifier_start(ch: char) -> bool {
    ch == '$' || ch == '_' || ch.is_alphabetic()
}

fn is_regexp_identifier_continue(ch: char) -> bool {
    is_regexp_identifier_start(ch) || ch.is_numeric() || ch == '\u{200c}' || ch == '\u{200d}'
}

fn invalid_regexp_named_group(loc: SourceLoc) -> Error {
    Error::General(loc, "invalid regular expression named group".to_string())
}

fn validate_unicode_property_escapes(
    pattern: &str,
    flags: &str,
    loc: SourceLoc,
) -> Result<(), Error> {
    let unicode = flags.contains('u') || flags.contains('v');
    if !unicode {
        return Ok(());
    }

    let unicode_sets = flags.contains('v');
    let chars = pattern.chars().collect::<Vec<_>>();
    let mut i = 0;
    let mut in_class = false;
    let mut class_negated = false;

    while i < chars.len() {
        match chars[i] {
            '[' => {
                in_class = true;
                class_negated = chars.get(i + 1) == Some(&'^');
                i += 1;
            }
            ']' => {
                in_class = false;
                class_negated = false;
                i += 1;
            }
            '\\' => {
                if chars.get(i + 1) == Some(&'\\')
                    && matches!(chars.get(i + 2), Some('p' | 'P'))
                    && chars.get(i + 3) == Some(&'{')
                {
                    let Some(end) = chars[i + 4..].iter().position(|&ch| ch == '}') else {
                        return invalid_regexp_property_escape(loc);
                    };
                    let end = i + 4 + end;
                    let expr = chars[i + 4..end].iter().collect::<String>();
                    if !is_valid_unicode_property_expression(&expr, unicode_sets, false, false) {
                        return invalid_regexp_property_escape(loc);
                    }
                    i += 2;
                    continue;
                }
                let Some(kind @ ('p' | 'P')) = chars.get(i + 1).copied() else {
                    i += 2;
                    continue;
                };
                if chars.get(i + 2) != Some(&'{') {
                    return invalid_regexp_property_escape(loc);
                }
                let Some(end) = chars[i + 3..].iter().position(|&ch| ch == '}') else {
                    return invalid_regexp_property_escape(loc);
                };
                let end = i + 3 + end;
                let expr = chars[i + 3..end].iter().collect::<String>();
                if property_escape_is_class_range_endpoint(&chars, i, end)
                    || !is_valid_unicode_property_expression(
                        &expr,
                        unicode_sets,
                        kind == 'P',
                        in_class && class_negated,
                    )
                {
                    return invalid_regexp_property_escape(loc);
                }
                i = end + 1;
            }
            _ => i += 1,
        }
    }

    Ok(())
}

fn invalid_regexp_property_escape<T>(loc: SourceLoc) -> Result<T, Error> {
    Err(Error::General(
        loc,
        "Invalid regular expression Unicode property escape".to_string(),
    ))
}

fn property_escape_is_class_range_endpoint(chars: &[char], start: usize, end: usize) -> bool {
    let range_start = chars.get(end + 1) == Some(&'-') && chars.get(end + 2) != Some(&']');
    let range_end = start >= 2
        && chars.get(start - 1) == Some(&'-')
        && chars.get(start - 2) != Some(&'[')
        && chars.get(start - 2) != Some(&'^');
    range_start || range_end
}

fn is_valid_unicode_property_expression(
    expr: &str,
    unicode_sets: bool,
    complement: bool,
    negated_class: bool,
) -> bool {
    if expr.is_empty()
        || !expr
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '=')
    {
        return false;
    }

    let mut parts = expr.split('=');
    let first = parts.next().unwrap();
    let second = parts.next();
    if parts.next().is_some() {
        return false;
    }

    if let Some(value) = second {
        if first.is_empty() || value.is_empty() {
            return false;
        }
        return match first {
            "General_Category" | "gc" => is_general_category_value(value),
            "Script" | "sc" | "Script_Extensions" | "scx" => is_script_value(value),
            _ => false,
        };
    }

    if matches!(
        expr,
        "General_Category" | "gc" | "Script" | "sc" | "Script_Extensions" | "scx"
    ) {
        return false;
    }

    if is_binary_property_of_strings(expr) {
        return unicode_sets && !complement && !negated_class;
    }

    is_binary_unicode_property(expr) || is_general_category_value(expr)
}

fn is_binary_property_of_strings(name: &str) -> bool {
    matches!(
        name,
        "Basic_Emoji"
            | "Emoji_Keycap_Sequence"
            | "RGI_Emoji"
            | "RGI_Emoji_Flag_Sequence"
            | "RGI_Emoji_Modifier_Sequence"
            | "RGI_Emoji_Tag_Sequence"
            | "RGI_Emoji_ZWJ_Sequence"
    )
}

fn is_binary_unicode_property(name: &str) -> bool {
    matches!(
        name,
        "ASCII"
            | "ASCII_Hex_Digit"
            | "AHex"
            | "Alphabetic"
            | "Alpha"
            | "Any"
            | "Assigned"
            | "Bidi_Control"
            | "Bidi_C"
            | "Bidi_Mirrored"
            | "Bidi_M"
            | "Case_Ignorable"
            | "CI"
            | "Cased"
            | "Changes_When_Casefolded"
            | "CWCF"
            | "Changes_When_Casemapped"
            | "CWCM"
            | "Changes_When_Lowercased"
            | "CWL"
            | "Changes_When_NFKC_Casefolded"
            | "CWKCF"
            | "Changes_When_Titlecased"
            | "CWT"
            | "Changes_When_Uppercased"
            | "CWU"
            | "Dash"
            | "Default_Ignorable_Code_Point"
            | "DI"
            | "Deprecated"
            | "Dep"
            | "Diacritic"
            | "Dia"
            | "Emoji"
            | "Emoji_Component"
            | "EComp"
            | "Emoji_Modifier"
            | "EMod"
            | "Emoji_Modifier_Base"
            | "EBase"
            | "Emoji_Presentation"
            | "EPres"
            | "Extended_Pictographic"
            | "ExtPict"
            | "Extender"
            | "Ext"
            | "Grapheme_Base"
            | "Gr_Base"
            | "Grapheme_Extend"
            | "Gr_Ext"
            | "Hex_Digit"
            | "Hex"
            | "IDS_Binary_Operator"
            | "IDSB"
            | "IDS_Trinary_Operator"
            | "IDST"
            | "ID_Continue"
            | "IDC"
            | "ID_Start"
            | "IDS"
            | "Ideographic"
            | "Ideo"
            | "Join_Control"
            | "Join_C"
            | "Logical_Order_Exception"
            | "LOE"
            | "Lowercase"
            | "Lower"
            | "Math"
            | "Noncharacter_Code_Point"
            | "NChar"
            | "Pattern_Syntax"
            | "Pat_Syn"
            | "Pattern_White_Space"
            | "Pat_WS"
            | "Quotation_Mark"
            | "QMark"
            | "Radical"
            | "Regional_Indicator"
            | "RI"
            | "Sentence_Terminal"
            | "STerm"
            | "Soft_Dotted"
            | "SD"
            | "Terminal_Punctuation"
            | "Term"
            | "Unified_Ideograph"
            | "UIdeo"
            | "Uppercase"
            | "Upper"
            | "Variation_Selector"
            | "VS"
            | "White_Space"
            | "space"
            | "XID_Continue"
            | "XIDC"
            | "XID_Start"
            | "XIDS"
    )
}

fn is_general_category_value(value: &str) -> bool {
    matches!(
        value,
        "C" | "Cc"
            | "Cf"
            | "Cn"
            | "Co"
            | "Cs"
            | "L"
            | "LC"
            | "Ll"
            | "Lm"
            | "Lo"
            | "Lt"
            | "Lu"
            | "M"
            | "Mc"
            | "Me"
            | "Mn"
            | "N"
            | "Nd"
            | "Nl"
            | "No"
            | "P"
            | "Pc"
            | "Pd"
            | "Pe"
            | "Pf"
            | "Pi"
            | "Po"
            | "Ps"
            | "S"
            | "Sc"
            | "Sk"
            | "Sm"
            | "So"
            | "Z"
            | "Zl"
            | "Zp"
            | "Zs"
            | "Cased_Letter"
            | "Close_Punctuation"
            | "Connector_Punctuation"
            | "Control"
            | "cntrl"
            | "Currency_Symbol"
            | "Dash_Punctuation"
            | "Decimal_Number"
            | "digit"
            | "Enclosing_Mark"
            | "Final_Punctuation"
            | "Format"
            | "Initial_Punctuation"
            | "Letter"
            | "Letter_Number"
            | "Line_Separator"
            | "Lowercase_Letter"
            | "Mark"
            | "Combining_Mark"
            | "Math_Symbol"
            | "Modifier_Letter"
            | "Modifier_Symbol"
            | "Nonspacing_Mark"
            | "Number"
            | "Open_Punctuation"
            | "Other"
            | "Other_Letter"
            | "Other_Number"
            | "Other_Punctuation"
            | "Other_Symbol"
            | "Paragraph_Separator"
            | "Private_Use"
            | "Punctuation"
            | "punct"
            | "Separator"
            | "Space_Separator"
            | "Spacing_Mark"
            | "Surrogate"
            | "Symbol"
            | "Titlecase_Letter"
            | "Unassigned"
            | "Uppercase_Letter"
    )
}

fn is_script_value(value: &str) -> bool {
    !matches!(value, "FooBarBazInvalid" | "H_e_h")
}

impl Lexer {
    fn read_number(&mut self) -> Result<Token, Error> {
        let loc = self.loc;
        #[derive(Debug, Clone, Copy, PartialEq)]
        enum NumLiteralKind {
            Hex,
            Dec,
            Oct,
            OldOct,
            Bin,
        }

        fn is_digit_for_radix(c: char, radix: u32) -> bool {
            c.to_digit(radix).is_some()
        }

        fn invalid_numeric_follower(c: char) -> bool {
            c == '_' || c == '$' || c.is_alphabetic()
        }

        fn parse_radix_literal(lit: &str, radix: u32) -> f64 {
            lit.chars()
                .filter_map(|c| c.to_digit(radix))
                .fold(0.0, |num, digit| num * radix as f64 + digit as f64)
        }

        fn decimal_mul_add(decimal: &str, radix: u32, digit: u32) -> String {
            let mut carry = digit;
            let mut out = Vec::with_capacity(decimal.len() + 1);
            for byte in decimal.bytes().rev() {
                let value = (byte - b'0') as u32 * radix + carry;
                out.push((b'0' + (value % 10) as u8) as char);
                carry = value / 10;
            }
            while carry != 0 {
                out.push((b'0' + (carry % 10) as u8) as char);
                carry /= 10;
            }
            out.iter().rev().collect()
        }

        fn normalize_bigint_literal(lit: &str, radix: u32) -> String {
            let mut decimal = "0".to_string();
            for digit in lit.chars().filter_map(|c| c.to_digit(radix)) {
                decimal = decimal_mul_add(decimal.as_str(), radix, digit);
            }
            let trimmed = decimal.trim_start_matches('0');
            if trimmed.is_empty() {
                "0".to_string()
            } else {
                trimmed.to_string()
            }
        }

        let read_digits = |lexer: &mut Lexer,
                           radix: u32,
                           mut seen_digit: bool,
                           require_new_digit: bool|
         -> Result<(String, bool), Error> {
            let mut digits = String::new();
            let mut saw_separator = false;
            let mut last_was_separator = false;
            let mut saw_new_digit = false;

            while !lexer.eof() {
                let c = lexer.peek_char()?;
                if is_digit_for_radix(c, radix) {
                    digits.push(lexer.take_char()?);
                    seen_digit = true;
                    saw_new_digit = true;
                    last_was_separator = false;
                } else if c == '_' {
                    if !seen_digit || last_was_separator {
                        return Err(Error::General(loc, "invalid token".to_string()));
                    }
                    digits.push(lexer.take_char()?);
                    saw_separator = true;
                    last_was_separator = true;
                } else {
                    break;
                }
            }

            if require_new_digit && !saw_new_digit {
                return Err(Error::General(loc, "invalid token".to_string()));
            }
            if last_was_separator {
                return Err(Error::General(loc, "invalid token".to_string()));
            }

            Ok((digits, saw_separator))
        };

        let mut kind = NumLiteralKind::Dec;
        let mut num_literal = String::new();
        let first = self.take_char()?;
        let mut integer_part = String::new();
        let mut integer_has_separator = false;

        if first == '.' {
            num_literal.push('0');
            num_literal.push('.');
            let (fraction, _) = read_digits(self, 10, false, true)?;
            num_literal.push_str(&fraction);
        } else if first == '0' && !self.eof() {
            match self.peek_char()? {
                'x' | 'X' => {
                    self.take_char()?;
                    kind = NumLiteralKind::Hex;
                    let (digits, _) = read_digits(self, 16, false, true)?;
                    num_literal = digits;
                }
                'b' | 'B' => {
                    self.take_char()?;
                    kind = NumLiteralKind::Bin;
                    let (digits, _) = read_digits(self, 2, false, true)?;
                    num_literal = digits;
                }
                'o' | 'O' => {
                    self.take_char()?;
                    kind = NumLiteralKind::Oct;
                    let (digits, _) = read_digits(self, 8, false, true)?;
                    num_literal = digits;
                }
                _ => {
                    integer_part.push(first);
                    let (rest, sep) = read_digits(self, 10, true, false)?;
                    integer_has_separator = sep;
                    integer_part.push_str(&rest);
                    num_literal.push_str(&integer_part);
                }
            }
        } else {
            integer_part.push(first);
            let (rest, sep) = read_digits(self, 10, true, false)?;
            integer_has_separator = sep;
            integer_part.push_str(&rest);
            num_literal.push_str(&integer_part);
        }

        if kind == NumLiteralKind::Dec {
            if integer_part.starts_with("0_")
                || (integer_part.starts_with('0') && integer_has_separator)
            {
                return Err(Error::General(loc, "invalid token".to_string()));
            }

            if !self.eof() && self.peek_char()? == '.' && first != '.' {
                num_literal.push(self.take_char()?);
                let (fraction, _) = read_digits(self, 10, false, false)?;
                num_literal.push_str(&fraction);
            }

            if !self.eof() && matches!(self.peek_char()?, 'e' | 'E') {
                num_literal.push(self.take_char()?);
                if !self.eof() && matches!(self.peek_char()?, '+' | '-') {
                    num_literal.push(self.take_char()?);
                }
                let (exponent, _) = read_digits(self, 10, false, true)?;
                num_literal.push_str(&exponent);
            }

            if integer_part.starts_with('0')
                && integer_part.len() > 1
                && !integer_part.contains(['8', '9'])
                && !integer_has_separator
                && !num_literal.contains('.')
                && !num_literal.contains('e')
                && !num_literal.contains('E')
            {
                kind = NumLiteralKind::OldOct;
            }
        }

        let num_literal = num_literal.replace('_', "");

        if !self.eof() && self.peek_char()? == 'n' {
            if kind == NumLiteralKind::OldOct
                || (kind == NumLiteralKind::Dec
                    && (num_literal.contains('.')
                        || num_literal.contains('e')
                        || num_literal.contains('E')
                        || (integer_part.starts_with('0') && integer_part.len() > 1)))
            {
                return Err(Error::General(loc, "invalid token".to_string()));
            }
            self.take_char()?;
            if !self.eof() && invalid_numeric_follower(self.peek_char()?) {
                return Err(Error::General(loc, "invalid token".to_string()));
            }
            let radix = match kind {
                NumLiteralKind::Hex => 16,
                NumLiteralKind::Oct => 8,
                NumLiteralKind::Bin => 2,
                NumLiteralKind::Dec => 10,
                NumLiteralKind::OldOct => unreachable!(),
            };
            return Ok(Token::new_bigint(
                normalize_bigint_literal(num_literal.as_str(), radix),
                loc,
            ));
        }

        if !self.eof() && invalid_numeric_follower(self.peek_char()?) {
            return Err(Error::General(loc, "invalid token".to_string()));
        }

        let num = match kind {
            NumLiteralKind::Dec => match num_literal.parse() {
                Ok(ok) => ok,
                Err(_) => {
                    return Err(Error::General(loc, "invalid token".to_string()));
                }
            },
            NumLiteralKind::Hex => parse_radix_literal(num_literal.as_str(), 16),
            NumLiteralKind::Oct | NumLiteralKind::OldOct => {
                parse_radix_literal(num_literal.as_str(), 8)
            }
            NumLiteralKind::Bin => parse_radix_literal(num_literal.as_str(), 2),
        };

        Ok(Token::new_number(num, loc))
    }

    fn read_hex_num(&mut self, num_literal: &str) -> Option<i64> {
        num_literal.chars().try_fold(0i64, |n, c| {
            let digit = match c.to_ascii_lowercase() {
                '0'..='9' | 'A'..='F' | 'a'..='f' => c.to_digit(16).unwrap() as i64,
                _ => return Some(n),
            };
            n.checked_mul(16)?.checked_add(digit)
        })
    }
}

impl Lexer {
    fn read_string_literal(&mut self) -> Result<Token, Error> {
        let loc = self.loc;
        let quote = self.take_char()?;
        let mut s = "".to_string();
        loop {
            match self.take_char()? {
                q if q == quote => break,
                '\\' => {
                    for c in self.read_escaped_char()? {
                        s.push(c)
                    }
                }
                c if is_line_terminator(c) => {
                    return Err(Error::General(
                        loc,
                        "unterminated string literal".to_string(),
                    ))
                }
                c => s.push(c),
            }
        }
        Ok(Token::new_string(s, loc))
    }

    fn read_template_literal(&mut self) -> Result<Token, Error> {
        let loc = self.loc;
        assert_eq!(self.take_char()?, '`');
        let mut parts = vec![];
        let mut cooked = String::new();

        loop {
            let c = self.take_char()?;
            match c {
                '`' => {
                    parts.push(TemplatePart { cooked, expr: None });
                    break;
                }
                '\\' => {
                    for c in self.read_escaped_char()? {
                        cooked.push(c)
                    }
                }
                '$' if self.take_char_if('{')? => {
                    let expr = self.read_template_expression()?;
                    parts.push(TemplatePart {
                        cooked,
                        expr: Some(expr),
                    });
                    cooked = String::new();
                }
                '\n' => {
                    self.loc.line += 1;
                    self.loc.column = 0;
                    cooked.push(c);
                }
                _ => cooked.push(c),
            }
        }

        Ok(Token::new_template(parts, loc))
    }

    fn read_template_expression(&mut self) -> Result<String, Error> {
        let mut expr = String::new();
        let mut depth = 1usize;
        let mut quote = None;

        while depth > 0 {
            let c = self.take_char()?;
            if let Some(q) = quote {
                if c == '\\' {
                    expr.push(c);
                    expr.push(self.take_char()?);
                    continue;
                }
                if c == q {
                    quote = None;
                }
                expr.push(c);
                continue;
            }

            match c {
                '\'' | '"' | '`' => {
                    quote = Some(c);
                    expr.push(c);
                }
                '/' if Self::template_slash_starts_regexp(&expr)
                    && !self.starts_with("/")
                    && !self.starts_with("*") =>
                {
                    expr.push(c);
                    self.read_template_regexp_literal(&mut expr)?;
                }
                '{' => {
                    depth += 1;
                    expr.push(c);
                }
                '}' => {
                    depth -= 1;
                    if depth > 0 {
                        expr.push(c);
                    }
                }
                '\n' => {
                    self.loc.line += 1;
                    self.loc.column = 0;
                    expr.push(c);
                }
                _ => expr.push(c),
            }
        }

        Ok(expr)
    }

    fn template_slash_starts_regexp(expr: &str) -> bool {
        match expr.chars().rev().find(|c| !c.is_whitespace()) {
            None => true,
            Some(
                '(' | '[' | '{' | '=' | ':' | ',' | ';' | '!' | '?' | '&' | '|' | '+' | '-' | '*'
                | '%' | '~' | '^' | '<' | '>',
            ) => true,
            _ => false,
        }
    }

    fn read_template_regexp_literal(&mut self, expr: &mut String) -> Result<(), Error> {
        let mut in_class = false;
        loop {
            let c = self.take_char()?;
            expr.push(c);
            match c {
                '\\' => expr.push(self.take_char()?),
                '[' if !in_class => in_class = true,
                ']' if in_class => in_class = false,
                '/' if !in_class => break,
                '\n' => {
                    self.loc.line += 1;
                    self.loc.column = 0;
                }
                _ => {}
            }
        }
        while !self.eof() && self.peek_char()?.is_ascii_alphabetic() {
            expr.push(self.take_char()?);
        }
        Ok(())
    }

    fn read_escaped_char(&mut self) -> Result<Vec<char>, Error> {
        fn is_high_surrogate(unit: u16) -> bool {
            (0xd800..=0xdbff).contains(&unit)
        }

        fn is_low_surrogate(unit: u16) -> bool {
            (0xdc00..=0xdfff).contains(&unit)
        }

        fn surrogate_pair_to_char(high: u16, low: u16) -> Option<char> {
            let code = 0x10000 + (((high as u32 - 0xd800) << 10) | (low as u32 - 0xdc00));
            char::from_u32(code)
        }

        let c = self.take_char()?;
        Ok(match c {
            c if is_line_terminator(c) => {
                if c == '\r' && !self.eof() && self.peek_char()? == '\n' {
                    self.take_char()?;
                }
                self.loc.line += 1;
                self.loc.column = 0;
                vec![]
            }
            '\'' | '"' | '?' | '\\' => vec![c],
            'a' => vec!['\x07'],
            'b' => vec!['\x08'],
            'f' => vec!['\x0c'],
            'n' => vec!['\x0a'],
            'r' => vec!['\x0d'],
            't' => vec!['\x09'],
            'v' => vec!['\x0b'],
            'x' => {
                let hex = self.take_hex_digits(2)?;
                vec![self.read_hex_num(hex.as_str()).unwrap() as u8 as char]
            }
            'u' => {
                if self.take_char_if('{')? {
                    let hex = self.take_char_while(|c| c.is_digit(16))?;
                    if hex.is_empty() || !self.take_char_if('}')? {
                        return Err(Error::General(
                            self.loc,
                            "invalid unicode escape sequence".to_string(),
                        ));
                    }
                    let code = self.read_hex_num(hex.as_str()).ok_or_else(|| {
                        Error::General(self.loc, "invalid unicode code point".to_string())
                    })? as u32;
                    vec![char::from_u32(code).ok_or_else(|| {
                        Error::General(self.loc, "invalid unicode code point".to_string())
                    })?]
                } else {
                    let hex = self.take_hex_digits(4)?;
                    let unit = self.read_hex_num(hex.as_str()).unwrap() as u16;
                    if is_high_surrogate(unit) && self.starts_with("\\u") {
                        let save_loc = self.loc;
                        self.take_char()?;
                        self.take_char()?;
                        let low_hex = self.take_hex_digits(4)?;
                        let low = self.read_hex_num(low_hex.as_str()).unwrap() as u16;
                        if is_low_surrogate(low) {
                            vec![surrogate_pair_to_char(unit, low).unwrap()]
                        } else {
                            self.loc = save_loc;
                            vec![char::REPLACEMENT_CHARACTER]
                        }
                    } else if is_high_surrogate(unit) || is_low_surrogate(unit) {
                        vec![char::REPLACEMENT_CHARACTER]
                    } else {
                        vec![char::from_u32(unit as u32).unwrap()]
                    }
                }
            }
            _ => vec![c],
        })
    }
}

impl Lexer {
    pub fn read_symbol(&mut self) -> Result<Token, Error> {
        let loc = self.loc;
        let mut symbol = Symbol::Hash;
        let c = self.take_char()?;
        match c {
            '+' | '-' => match self.peek_char()? {
                '=' => {
                    assert_eq!(self.take_char()?, '=');
                    if c == '+' {
                        symbol = Symbol::AssignAdd;
                    } else if c == '-' {
                        symbol = Symbol::AssignSub;
                    }
                }
                '>' => {
                    assert_eq!(self.take_char()?, '>');
                    if c == '-' {
                        symbol = Symbol::Arrow;
                    }
                }
                '+' => {
                    assert_eq!(self.take_char()?, '+');
                    if c == '+' {
                        symbol = Symbol::Inc;
                    }
                }
                '-' => {
                    assert_eq!(self.take_char()?, '-');
                    if c == '-' {
                        symbol = Symbol::Dec;
                    }
                }
                _ => {
                    if c == '+' {
                        symbol = Symbol::Add;
                    } else if c == '-' {
                        symbol = Symbol::Sub;
                    }
                }
            },
            '*' => {
                if self.take_char_if('=')? {
                    symbol = Symbol::AssignMul
                } else if self.take_char_if('*')? {
                    symbol = Symbol::Exp
                } else {
                    symbol = Symbol::Asterisk
                }
            }
            '/' => {
                if self.take_char_if('=')? {
                    symbol = Symbol::AssignDiv
                } else {
                    symbol = Symbol::Div
                }
            }
            '%' => {
                if self.take_char_if('=')? {
                    symbol = Symbol::AssignMod
                } else {
                    symbol = Symbol::Mod
                }
            }
            '=' => {
                if self.take_char_if('>')? {
                    symbol = Symbol::FatArrow
                } else if self.take_char_if('=')? {
                    symbol = if self.take_char_if('=')? {
                        Symbol::SEq
                    } else {
                        Symbol::Eq
                    }
                } else {
                    symbol = Symbol::Assign
                }
            }
            '^' => {
                if self.take_char_if('=')? {
                    symbol = Symbol::AssignXor
                } else {
                    symbol = Symbol::Xor
                }
            }
            '!' => {
                if self.take_char_if('=')? {
                    symbol = if self.take_char_if('=')? {
                        Symbol::SNe
                    } else {
                        Symbol::Ne
                    }
                } else {
                    symbol = Symbol::Not
                }
            }
            '<' | '>' | '&' | '|' => {
                let mut single = true;
                if self.take_char_if(c)? {
                    symbol = match c {
                        '<' => Symbol::Shl,
                        '>' => {
                            if self.take_char_if('>')? {
                                Symbol::ZFShr
                            } else {
                                Symbol::Shr
                            }
                        }
                        '&' => Symbol::LAnd,
                        '|' => Symbol::LOr,
                        _ => unreachable!(),
                    };
                    single = false;
                }
                if self.take_char_if('=')? {
                    symbol = match (c, symbol) {
                        ('<', Symbol::Shl) => Symbol::AssignShl,
                        ('<', _) => Symbol::Le,
                        ('>', Symbol::Shr) => Symbol::AssignShr,
                        ('>', Symbol::ZFShr) => Symbol::AssignZFShr,
                        ('>', _) => Symbol::Ge,
                        ('&', Symbol::LAnd) => Symbol::AssignLAnd,
                        ('&', _) => Symbol::AssignAnd,
                        ('|', Symbol::LOr) => Symbol::AssignLOr,
                        ('|', _) => Symbol::AssignOr,
                        _ => unreachable!(),
                    };
                    single = false;
                }
                if single {
                    symbol = match c {
                        '<' => Symbol::Lt,
                        '>' => Symbol::Gt,
                        '&' => Symbol::And,
                        '|' => Symbol::Or,
                        _ => unreachable!(),
                    };
                }
            }
            '(' => symbol = Symbol::OpeningParen,
            ')' => symbol = Symbol::ClosingParen,
            '[' => symbol = Symbol::OpeningBoxBracket,
            ']' => symbol = Symbol::ClosingBoxBracket,
            '{' => symbol = Symbol::OpeningBrace,
            '}' => symbol = Symbol::ClosingBrace,
            ',' => symbol = Symbol::Comma,
            ';' => symbol = Symbol::Semicolon,
            ':' => symbol = Symbol::Colon,
            '~' => symbol = Symbol::BitwiseNot,
            '?' => {
                if self.take_char_if('?')? {
                    symbol = if self.take_char_if('=')? {
                        Symbol::AssignCoalesce
                    } else {
                        Symbol::Coalesce
                    }
                } else if !self.eof()
                    && self.peek_char()? == '.'
                    && self
                        .code
                        .get(self.loc.pos + 1..)
                        .and_then(|rest| rest.chars().next())
                        .map_or(true, |c| !c.is_ascii_digit())
                {
                    self.take_char()?;
                    symbol = Symbol::OptionalChain
                } else {
                    symbol = Symbol::Question
                }
            }
            '#' => symbol = Symbol::Hash,
            '.' => {
                if self.take_char_if('.')? {
                    symbol = if self.take_char_if('.')? {
                        Symbol::Spread
                    } else {
                        return Err(Error::General(loc, "Invalid token".to_string()));
                    }
                } else {
                    symbol = Symbol::Point
                }
            }
            _ => {}
        };

        Ok(Token::new_symbol(symbol, loc))
    }
}

impl Lexer {
    /// Read line terminator. (if next char is not line terminator, panic.)
    fn read_line_terminator(&mut self) -> Result<Token, Error> {
        let loc = self.loc;
        let ch = self.take_char()?;
        assert!(is_line_terminator(ch));
        if ch == '\r' && !self.eof() && self.peek_char()? == '\n' {
            self.take_char()?;
        }
        self.loc.line += 1;
        self.loc.column = 0;
        self.html_close_allowed = true;
        Ok(Token::new_line_terminator(loc))
    }
}

impl Lexer {
    /// Skip whitespace and tabs
    fn skip_whitespace(&mut self) -> Result<(), Error> {
        self.take_char_while(is_whitespace).and(Ok(()))
    }

    /// While ``f(char)`` is true, read chars, and move cursor next.
    /// Return all chars as String.
    fn take_char_while<F>(&mut self, mut f: F) -> Result<String, Error>
    where
        F: FnMut(char) -> bool,
    {
        let mut s = "".to_string();
        while !self.eof() && f(self.peek_char()?) {
            s.push(self.take_char()?);
        }
        Ok(s)
    }

    fn take_hex_digits(&mut self, len: usize) -> Result<String, Error> {
        let loc = self.loc;
        let mut s = String::new();
        for _ in 0..len {
            let c = self.take_char()?;
            if !c.is_digit(16) {
                return Err(Error::General(
                    loc,
                    "invalid hexadecimal escape sequence".to_string(),
                ));
            }
            s.push(c);
        }
        Ok(s)
    }

    /// While ``f(char)`` is true and not reached EOF, move cursor next
    fn skip_char_while<F>(&mut self, mut f: F) -> Result<(), Error>
    where
        F: FnMut(char) -> bool,
    {
        while !self.eof() && f(self.peek_char()?) {
            self.take_char()?;
        }
        Ok(())
    }

    /// Read next char, and move cursor next
    fn take_char(&mut self) -> Result<char, Error> {
        let mut iter = self.code[self.loc.pos..].char_indices();
        let (_, cur_char) = iter.next().ok_or(Error::NormalEOF)?;
        let (next_pos, _) = iter.next().unwrap_or((cur_char.len_utf8(), ' '));
        self.loc.pos += next_pos;
        self.loc.column += next_pos;
        Ok(cur_char)
    }

    /// If the next char is ``c``, move cursor next, return true.
    /// If not (include EOF), return false.
    fn take_char_if(&mut self, c: char) -> Result<bool, Error> {
        let f = !self.eof() && self.peek_char()? == c;
        if f {
            assert_eq!(self.take_char()?, c);
        }
        Ok(f)
    }

    /// If chars start with ``s``, return true
    fn starts_with(&self, s: &str) -> bool {
        self.code[self.loc.pos..].starts_with(s)
    }

    /// peek next char. if eof, raise Err(Error::NormalEOF)
    fn peek_char(&self) -> Result<char, Error> {
        self.code[self.loc.pos..]
            .chars()
            .next()
            .ok_or(Error::NormalEOF)
    }

    fn eof(&self) -> bool {
        self.loc.pos >= self.code.len()
    }
}

pub fn get_error_line<T: AsRef<str>>(code: T, loc: SourceLoc) -> String {
    let code = code.as_ref();
    let mut pos = loc.pos.min(code.len());
    while pos > 0 && !code.is_char_boundary(pos) {
        pos -= 1;
    }

    let start = code[..pos].rfind('\n').map(|i| i + 1).unwrap_or(0);
    let end = code[pos..]
        .find('\n')
        .map(|i| pos + i)
        .unwrap_or(code.len());

    let surrounding_code = code[start..end].to_string();
    let err_point = format!("{}{}", " ".repeat(code[start..pos].chars().count()), '^');
    surrounding_code + "\n" + err_point.as_str()
}

#[test]
fn number() {
    let mut lexer = Lexer::new("1 2 0x34 056 7.89 0b10 5e3 5e+3 5e-3 0999 0O123".to_string());
    lexer.tokenize_all().unwrap();
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(1.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(2.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(52.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(46.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(7.89));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(2.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(5e3));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(5e+3));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(5e-3));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(999.0));
    assert_eq!(lexer.next().unwrap().kind, Kind::Number(0o123 as f64));
}

#[test]
fn identifier() {
    let mut lexer = Lexer::new("console log".to_string());
    lexer.tokenize_all().unwrap();
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Identifier("console".to_string())
    );
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Identifier("log".to_string())
    );
}

#[test]
fn string() {
    let mut lexer = Lexer::new("'aaa' \"bbb\"".to_string());
    lexer.tokenize_all().unwrap();
    assert_eq!(lexer.next().unwrap().kind, Kind::String("aaa".to_string()));
    assert_eq!(lexer.next().unwrap().kind, Kind::String("bbb".to_string()));
}

#[test]
fn keyword() {
    use crate::token::Keyword;

    let mut lexer = Lexer::new(
        "break case catch continue debugger default \
         delete do else finally for function if in instanceof \
         new return switch this throw try typeof \
         var void while with"
            .to_string(),
    );
    lexer.tokenize_all().unwrap();
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Break,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Case,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Catch,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Keyword(Keyword::Continue,)
    );
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Keyword(Keyword::Debugger,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Default,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Delete,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Do,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Else,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Finally,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::For,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Keyword(Keyword::Function,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::If,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::In,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Keyword(Keyword::Instanceof,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::New,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Return,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Switch,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::This,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Throw,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Try,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Typeof,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Var,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::Void,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::While,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Keyword(Keyword::With,));
}

#[test]
fn symbol() {
    let mut lexer = Lexer::new(
        "() {} [] , ; : . -> ++ -- + - * % **\
         ! ~ << >> >>> < <= > >= == != === !== & | ^ && || \
         ? ?. ?? = += -= *= %= <<= >>= >>>= &= |= ^= \
         &&= ||= ??= #"
            .to_string(),
    );
    lexer.tokenize_all().unwrap();

    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::OpeningParen,)
    );
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::ClosingParen,)
    );
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::OpeningBrace,)
    );
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::ClosingBrace,)
    );
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::OpeningBoxBracket,)
    );
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::ClosingBoxBracket,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Comma,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Semicolon,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Colon,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Point,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Arrow,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Inc,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Dec,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Add,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Sub,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Asterisk,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Mod,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Exp,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Not,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::BitwiseNot,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Shl,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Shr,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::ZFShr,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Lt,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Le,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Gt,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Ge,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Eq,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Ne,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::SEq,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::SNe,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::And,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Or,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Xor,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::LAnd,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::LOr,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Question,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::OptionalChain,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Coalesce,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Assign,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignAdd,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignSub,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignMul,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignMod,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignShl,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignShr,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::AssignZFShr,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignAnd,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignOr,));
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignXor,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::AssignLAnd,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignLOr,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Symbol(Symbol::AssignCoalesce,)
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Hash,));
}

#[test]
fn division_symbols() {
    // `/` and `/=` are only division operators when they follow an operand;
    // otherwise they begin a regular expression literal.
    let mut lexer = Lexer::new("a / b /= c".to_string());
    lexer.tokenize_all().unwrap();
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Identifier("a".to_string())
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::Div,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Identifier("b".to_string())
    );
    assert_eq!(lexer.next().unwrap().kind, Kind::Symbol(Symbol::AssignDiv,));
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Identifier("c".to_string())
    );
}

#[test]
fn line_terminator() {
    let mut lexer = Lexer::new("hello\nworld".to_string());
    lexer.tokenize_all().unwrap();
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Identifier("hello".to_string())
    );
    assert_eq!(lexer.read_token().unwrap().kind, Kind::LineTerminator);
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::Identifier("world".to_string())
    );
}

#[test]
fn escape_seq() {
    let mut lexer = Lexer::new(
        "\"\\' \\\" \\\\ \\a \\b \\f \\n \\r \\t \\v \\x12 \\uD867\\uDE3D\"".to_string(),
    );
    lexer.tokenize_all().unwrap();
    assert_eq!(
        lexer.next().unwrap().kind,
        Kind::String("\' \" \\ \x07 \x08 \x0c \n \r \t \x0b \x12 𩸽".to_string())
    );
}

#[test]
fn escaped_unicode_code_point_limit() {
    let expected = char::from_u32(0x10ffff).unwrap().to_string();
    let mut lexer = Lexer::new("\"\\u{10ffff}\"".to_string());
    lexer.tokenize_all().unwrap();
    assert_eq!(lexer.next().unwrap().kind, Kind::String(expected));
}

#[test]
fn escaped_unicode_code_point_overflow() {
    let mut lexer = Lexer::new("\\u{77777777777777777}".to_string());
    assert!(lexer.tokenize_all().is_err());
}

#[test]
fn comment() {
    let mut lexer = Lexer::new(
        "x; // line comment
                               /* multi-line
                                * comment 
                                */
                               y"
        .to_string(),
    );
    lexer.tokenize_all().unwrap();
    assert_eq!(
        lexer.next_skip_lineterminator().unwrap().kind,
        Kind::Identifier("x".to_string())
    );
    assert_eq!(
        lexer.next_skip_lineterminator().unwrap().kind,
        Kind::Symbol(Symbol::Semicolon)
    );
    assert_eq!(
        lexer.next_skip_lineterminator().unwrap().kind,
        Kind::Identifier("y".to_string())
    );
}
