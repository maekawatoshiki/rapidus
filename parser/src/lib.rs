mod for_;
mod if_;
mod return_;
mod while_;

pub mod script;

use ansi_term::Colour;
use rapidus_ast::{
    loc::SourceLoc, ArrayPatternElement, BinOp, FormalParameter, FormalParameters,
    MethodDefinitionKind, Node, NodeBase, ObjectPatternProperty, PropertyDefinition, UnaryOp,
    VarKind,
};
use rapidus_lexer::token::{Keyword, Kind, Symbol, Token};
pub use rapidus_lexer::Error;
use rapidus_lexer::{get_error_line, Lexer};
use script::ScriptInfo;
use std::collections::{HashMap, HashSet};
use std::fs::OpenOptions;
use std::io::Read;
use std::path::{Path, PathBuf};

#[macro_export]
macro_rules! expect {
    ($self:ident, $kind:expr, $msg:expr) => {{
        let tok = $self.lexer.next_skip_lineterminator()?;
        if tok.kind != $kind {
            return Err(Error::Expect(tok.loc, $msg.to_string()));
        }
    }};
}

macro_rules! expect_no_lineterminator {
    ($self:ident, $kind:expr, $msg:expr) => {{
        let tok = $self.lexer.next()?;
        if tok.kind != $kind {
            return Err(Error::Expect(tok.loc, $msg.to_string()));
        }
    }};
}

#[derive(Clone, Debug)]
pub struct Parser {
    pub file_name: PathBuf,
    pub lexer: Lexer,
    module: bool,
    allow_await: bool,
    allow_in: bool,
    private_name_stack: Vec<HashMap<String, u8>>,
}

struct ClassElementName {
    func_name: Option<String>,
    property_key: Node,
    is_private: bool,
}

const PRIVATE_NAME_GET: u8 = 1;
const PRIVATE_NAME_SET: u8 = 2;
const PRIVATE_NAME_OTHER: u8 = 4;

impl Parser {
    pub fn new(file_name: impl Into<PathBuf>, code: impl Into<String>) -> Parser {
        Parser {
            file_name: file_name.into(),
            lexer: Lexer::new(code.into()),
            module: false,
            allow_await: false,
            allow_in: true,
            private_name_stack: Vec::new(),
        }
    }

    pub(crate) fn with_allow_in<T>(
        &mut self,
        allow_in: bool,
        f: impl FnOnce(&mut Parser) -> Result<T, Error>,
    ) -> Result<T, Error> {
        let prev_allow_in = self.allow_in;
        self.allow_in = allow_in;
        let result = f(self);
        self.allow_in = prev_allow_in;
        result
    }

    /// Load file and generate Parser from the file.
    /// ## Arguments
    /// * `file_name` - A module file name.
    pub fn load_module(file_name: impl Into<PathBuf>) -> Result<Parser, Error> {
        let file_name = file_name.into();
        let path = Path::new(&file_name).with_extension("js");
        let absolute_path = match path.canonicalize() {
            Ok(path) => path,
            Err(ioerr) => {
                // TODO: Error::General may not be suitable. We need a richer way to represent this error.
                return Err(Error::General(SourceLoc::default(), ioerr.to_string()));
            }
        };

        let mut file_body = String::new();

        match OpenOptions::new().read(true).open(&absolute_path) {
            Ok(mut ok) => ok
                .read_to_string(&mut file_body)
                .ok()
                .expect("cannot read file"),
            Err(ioerr) => {
                // TODO: Error::General may not be suitable. We need a richer way to represent this error.
                return Err(Error::General(SourceLoc::default(), ioerr.to_string()));
            }
        };

        Ok(Self::new(absolute_path, file_body))
    }

    pub fn into_script_info(self) -> ScriptInfo {
        ScriptInfo::new(self.file_name, self.lexer.code)
    }

    /// Display error position in the source script.
    /// ## Arguments
    /// * `loc` - Source location in the source script.
    /// * `msg` - Error message text.
    pub fn show_error_at(&self, loc: SourceLoc, msg: impl Into<String>) {
        let err_line = get_error_line(&self.lexer.code, loc);
        eprintln!(
            "{}: {}:{}:{}: {}\n{}",
            Colour::Red.bold().paint("Syntax error"),
            self.file_name.to_string_lossy(),
            loc.line,
            loc.column,
            msg.into(),
            err_line,
        );
    }

    /// Display syntax error message.
    /// ## Arguments
    /// * `err` - parser::Error.
    pub fn handle_error(&self, err: &Error) {
        match err {
            Error::NormalEOF => unreachable!(),
            Error::Expect(loc, msg)
            | Error::General(loc, msg)
            | Error::UnexpectedToken(loc, msg) => {
                self.show_error_at(*loc, msg.clone());
            }
            Error::UnexpectedEOF(msg) => {
                self.show_error_at(self.lexer.loc, format!("unexpected EOF. {}", msg))
            }
            Error::InvalidToken(loc) => self.show_error_at(*loc, "Invalid token."),
            Error::UnsupportedFeature(loc) => {
                self.show_error_at(*loc, "Unsupported feature.");
            }
        }
    }
}

impl Parser {
    pub fn parse_all(&mut self) -> Result<Node, Error> {
        self.lexer.tokenize_all()?;
        let script = self.read_script()?;
        Self::validate_early_errors(&script, FunctionBody::Yes, false)?;
        Ok(script)
    }

    pub fn parse_module(&mut self) -> Result<Node, Error> {
        self.module = true;
        self.allow_await = true;
        self.lexer.tokenize_all()?;
        let script = self.read_script()?;
        Self::validate_early_errors(&script, FunctionBody::Yes, false)?;
        Ok(script)
    }
}

#[derive(Clone, Copy, PartialEq)]
enum FunctionBody {
    No,
    Yes,
    DerivedConstructor,
}

impl FunctionBody {
    fn statement_context(self) -> Self {
        match self {
            FunctionBody::DerivedConstructor => FunctionBody::DerivedConstructor,
            _ => FunctionBody::No,
        }
    }
}

impl Parser {
    fn read_script(&mut self) -> Result<Node, Error> {
        self.read_statement_list()
    }
}

impl Parser {
    fn read_statement_list(&mut self) -> Result<Node, Error> {
        self.read_statements(false, false, false)
    }

    fn read_block_statement(&mut self) -> Result<Node, Error> {
        self.read_statements(true, true, false)
    }

    fn read_block(&mut self) -> Result<Node, Error> {
        self.read_statements(true, false, false)
    }

    fn read_statements(
        &mut self,
        break_when_closingbrase: bool,
        is_block_statement: bool,
        accept_case_label: bool,
    ) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        let mut items = vec![];

        loop {
            let loc = self.lexer.get_current_loc();
            match self.lexer.skip(Symbol::ClosingBrace) {
                Ok(true) => {
                    if break_when_closingbrase {
                        break;
                    } else {
                        return Err(Error::UnexpectedToken(
                            loc,
                            "unexpected token '}'.".to_string(),
                        ));
                    }
                }
                Ok(false) => {}
                Err(Error::NormalEOF) => {}
                Err(e) => return Err(e),
            }

            if self.lexer.peek_skip_lineterminator() == Err(Error::NormalEOF) {
                if break_when_closingbrase {
                    return Err(Error::UnexpectedEOF("expected '}'.".to_string()));
                } else {
                    break;
                }
            }

            match self.read_statement_list_item(accept_case_label) {
                Ok(ok) => items.push(ok),
                Err(Error::NormalEOF) => {
                    return Err(Error::UnexpectedEOF("".to_string()));
                }
                Err(e) => return Err(e),
            }

            while match self.lexer.skip(Symbol::Semicolon) {
                Ok(succ) => succ,
                Err(Error::NormalEOF) => false,
                Err(e) => return Err(e),
            } {}
        }

        if is_block_statement {
            Ok(Node::new(NodeBase::Block(items), loc))
        } else {
            Ok(Node::new(NodeBase::StatementList(items), loc))
        }
    }

    /// https://tc39.github.io/ecma262/#prod-StatementListItem
    fn read_statement_list_item(&mut self, accept_case_label: bool) -> Result<Node, Error> {
        if let Ok(tok) = self.lexer.peek_skip_lineterminator() {
            match tok.kind {
                Kind::Identifier(ref name) if self.module && name == "export" => {
                    self.read_export_declaration()
                }
                Kind::Identifier(ref name) if self.module && name == "import" => {
                    let save_pos = self.lexer.token_pos;
                    self.lexer.next_skip_lineterminator()?;
                    if matches!(
                        self.lexer.peek_skip_lineterminator()?.kind,
                        Kind::Symbol(Symbol::OpeningParen)
                    ) {
                        self.lexer.token_pos = save_pos;
                        self.read_statement_sub(accept_case_label)
                    } else {
                        self.skip_module_declaration_tail(tok.loc)
                    }
                }
                Kind::Keyword(Keyword::Function) => self.read_declaration(),
                Kind::Keyword(Keyword::Const) => self.read_declaration(),
                Kind::Keyword(Keyword::Let) => self.read_declaration(),
                Kind::Identifier(ref name) if name == "async" => {
                    let save_pos = self.lexer.token_pos;
                    self.lexer.next_skip_lineterminator()?;
                    if matches!(
                        self.lexer.next(),
                        Ok(Token {
                            kind: Kind::Keyword(Keyword::Function),
                            ..
                        })
                    ) {
                        self.read_function_declaration_inner(true)
                    } else {
                        self.lexer.token_pos = save_pos;
                        self.read_statement_sub(accept_case_label)
                    }
                }
                Kind::Identifier(ref name) if name == "class" => self.read_declaration(),
                _ => self.read_statement_sub(accept_case_label),
            }
        } else {
            Err(Error::NormalEOF)
        }
    }

    fn read_export_declaration(&mut self) -> Result<Node, Error> {
        let tok = self.lexer.next_skip_lineterminator()?;
        let loc = tok.loc;
        if self.peek_identifier("default") {
            self.lexer.next_skip_lineterminator()?;
            let default_decl = match self.lexer.peek_skip_lineterminator()?.kind {
                Kind::Keyword(Keyword::Function) => true,
                Kind::Identifier(ref name) => name == "class" || name == "async",
                _ => false,
            };
            if default_decl {
                return self.read_statement_list_item(false);
            }
            let expr = self.read_assignment_expression()?;
            let _ = self.lexer.skip(Symbol::Semicolon);
            return Ok(expr);
        }
        match self.lexer.peek_skip_lineterminator()?.kind {
            Kind::Symbol(Symbol::OpeningBrace) | Kind::Symbol(Symbol::Asterisk) => {
                self.skip_module_declaration_tail(loc)
            }
            _ => self.read_statement_list_item(false),
        }
    }

    fn skip_module_declaration_tail(&mut self, loc: SourceLoc) -> Result<Node, Error> {
        let mut paren_depth = 0usize;
        let mut brace_depth = 0usize;
        let mut bracket_depth = 0usize;
        loop {
            let tok = self.lexer.next_skip_lineterminator()?;
            match tok.kind {
                Kind::Symbol(Symbol::OpeningParen) => paren_depth += 1,
                Kind::Symbol(Symbol::ClosingParen) => paren_depth = paren_depth.saturating_sub(1),
                Kind::Symbol(Symbol::OpeningBrace) => brace_depth += 1,
                Kind::Symbol(Symbol::ClosingBrace) => brace_depth = brace_depth.saturating_sub(1),
                Kind::Symbol(Symbol::OpeningBoxBracket) => bracket_depth += 1,
                Kind::Symbol(Symbol::ClosingBoxBracket) => {
                    bracket_depth = bracket_depth.saturating_sub(1)
                }
                Kind::Symbol(Symbol::Semicolon)
                    if paren_depth == 0 && brace_depth == 0 && bracket_depth == 0 =>
                {
                    break
                }
                Kind::EOF => break,
                _ => {}
            }
        }
        Ok(Node::new(NodeBase::Nope, loc))
    }

    /// http://www.ecma-international.org/ecma-262/9.0/index.html#prod-Statement
    fn read_statement(&mut self) -> Result<Node, Error> {
        self.read_statement_sub(false)
    }

    fn read_statement_sub(&mut self, accept_case_label: bool) -> Result<Node, Error> {
        let tok = self.lexer.next_skip_lineterminator()?;

        // Case label
        if accept_case_label && matches!(tok.kind, Kind::Keyword(Keyword::Case)) {
            let val = self.read_assignment_expression()?;
            let maybe_colon = self.lexer.peek_skip_lineterminator();
            if let Ok(Token {
                kind: Kind::Symbol(Symbol::Colon),
                ..
            }) = maybe_colon
            {
                assert!(self.lexer.next_skip_lineterminator().is_ok());
                return Ok(Node::new(NodeBase::CaseLabel(Box::new(val)), tok.loc));
            }
        }
        if accept_case_label && matches!(tok.kind, Kind::Keyword(Keyword::Default)) {
            expect!(self, Kind::Symbol(Symbol::Colon), "expect ':'");
            return Ok(Node::new(NodeBase::DefaultLabel, tok.loc));
        }

        // Label
        if let Kind::Identifier(ref name) = tok.kind {
            let maybe_colon = self.lexer.peek_skip_lineterminator();
            if let Ok(Token {
                kind: Kind::Symbol(Symbol::Colon),
                ..
            }) = maybe_colon
            {
                if self.allow_await && name == "await" {
                    return Err(Error::UnexpectedToken(
                        tok.loc,
                        "await cannot be used as a label in an async function".to_string(),
                    ));
                }
                assert_eq!(
                    self.lexer.next_skip_lineterminator()?.kind,
                    Kind::Symbol(Symbol::Colon)
                );
                let labeled_item = self.read_statement_list_item(false)?;
                return Ok(Node::new(
                    NodeBase::Label(name.clone(), Box::new(labeled_item)),
                    tok.loc,
                ));
            }
        }

        let mut is_expression_statement = false;
        let stmt = match tok.kind {
            Kind::Keyword(Keyword::If) => self.read_if_statement(tok.loc),
            Kind::Keyword(Keyword::Switch) => self.read_switch_statement(tok.loc),
            Kind::Keyword(Keyword::Var) => self.read_variable_statement(tok.loc),
            Kind::Keyword(Keyword::While) => self.read_while_statement(tok.loc),
            Kind::Keyword(Keyword::With) => self.read_with_statement(tok.loc),
            Kind::Keyword(Keyword::Do) => self.read_do_while_statement(tok.loc),
            Kind::Keyword(Keyword::For) => self.read_for_statement(tok.loc),
            Kind::Keyword(Keyword::Return) => self.read_return_statement(tok.loc),
            Kind::Keyword(Keyword::Break) => self.read_break_statement(),
            Kind::Keyword(Keyword::Continue) => self.read_continue_statement(),
            Kind::Keyword(Keyword::Try) => self.read_try_statement(),
            Kind::Keyword(Keyword::Throw) => self.read_throw_statement(),
            Kind::Keyword(Keyword::Function) => self.read_function_declaration(),
            Kind::Identifier(ref name) if name == "class" => self.read_class_declaration(tok.loc),
            Kind::Symbol(Symbol::OpeningBrace) => self.read_block_statement(),
            Kind::Symbol(Symbol::Semicolon) => return Ok(Node::new(NodeBase::Nope, tok.loc)),
            _ => {
                self.lexer.unget();
                is_expression_statement = true;
                self.read_expression_statement()
            }
        };

        match self.lexer.skip(Symbol::Semicolon) {
            Ok(true) | Err(Error::NormalEOF) => {}
            Ok(false) => {
                if is_expression_statement {
                    match self.lexer.peek(0)?.kind {
                        Kind::LineTerminator | Kind::Symbol(Symbol::ClosingBrace) => {}
                        _ => {
                            return Err(Error::UnexpectedToken(
                                self.lexer.get_current_loc(),
                                format!("unexpected token."),
                            ));
                        }
                    }
                }
            }
            Err(e) => return Err(e),
        }

        stmt
    }
}

impl Parser {
    fn read_with_statement(&mut self, loc: SourceLoc) -> Result<Node, Error> {
        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");
        let object = self.read_expression()?;
        expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");
        let body = self.read_statement()?;
        Ok(Node::new(
            NodeBase::With(Box::new(object), Box::new(body)),
            loc,
        ))
    }

    /// https://tc39.github.io/ecma262/#prod-VariableStatement
    fn read_variable_statement(&mut self, loc: SourceLoc) -> Result<Node, Error> {
        self.read_variable_declaration_list(loc)
    }

    /// https://tc39.github.io/ecma262/#prod-VariableDeclarationList
    fn read_variable_declaration_list(&mut self, loc: SourceLoc) -> Result<Node, Error> {
        let mut list = vec![];

        loop {
            list.push(self.read_variable_declaration()?);
            if !self.variable_declaration_continuation()? {
                break;
            }
        }

        Ok(Node::new(NodeBase::StatementList(list), loc))
    }

    fn variable_declaration_continuation(&mut self) -> Result<bool, Error> {
        let mut newline_found = false;

        for i in 0.. {
            match self.lexer.peek(i) {
                Ok(tok) => match tok.kind {
                    Kind::LineTerminator => newline_found = true,
                    Kind::Symbol(Symbol::Semicolon) => {
                        return Ok(false);
                    }
                    Kind::Symbol(Symbol::Comma) => {
                        for _ in 0..=i {
                            self.lexer.next()?;
                        }
                        return Ok(true);
                    }
                    _ if newline_found => return Ok(false),
                    _ => break,
                },
                Err(_) => return Ok(false),
            }
        }

        Err(Error::Expect(
            self.lexer.get_current_loc(),
            "expect ';' or line terminator".to_string(),
        ))
    }

    /// https://tc39.github.io/ecma262/#prod-VariableDeclaration
    fn read_variable_declaration(&mut self) -> Result<Node, Error> {
        self.read_variable_declaration_with_kind(VarKind::Var)
    }

    fn read_variable_declaration_with_kind(&mut self, var_kind: VarKind) -> Result<Node, Error> {
        self.read_variable_declaration_with_kind_allow_pattern_without_init(var_kind, false)
    }

    fn read_variable_declaration_with_kind_allow_pattern_without_init(
        &mut self,
        var_kind: VarKind,
        allow_pattern_without_init: bool,
    ) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        if matches!(
            self.lexer.peek_skip_lineterminator()?.kind,
            Kind::Symbol(Symbol::OpeningBrace) | Kind::Symbol(Symbol::OpeningBoxBracket)
        ) {
            let pattern = self.read_binding_pattern()?;
            let init = if self.lexer.skip(Symbol::Assign).unwrap_or(false) {
                Some(Box::new(self.read_initializer()?))
            } else {
                None
            };
            if init.is_none() && !allow_pattern_without_init {
                return Err(Error::Expect(
                    loc,
                    "destructuring declaration requires initializer".to_string(),
                ));
            }
            return Ok(Node::new(
                NodeBase::VarDeclPattern(Box::new(pattern), init, var_kind),
                loc,
            ));
        }

        let name = match self.lexer.next_skip_lineterminator()?.kind {
            Kind::Identifier(name) => name,
            _ => {
                return Err(Error::UnexpectedToken(
                    loc,
                    "Expect identifier.".to_string(),
                ));
            }
        };
        if self.allow_await && name == "await" {
            return Err(Error::UnexpectedToken(
                loc,
                "await cannot be used as a binding identifier in an async function".to_string(),
            ));
        }

        if self.lexer.skip(Symbol::Assign).unwrap_or(false) {
            Ok(Node::new(
                NodeBase::VarDecl(name, Some(Box::new(self.read_initializer()?)), var_kind),
                loc,
            ))
        } else {
            Ok(Node::new(NodeBase::VarDecl(name, None, var_kind), loc))
        }
    }

    /// https://tc39.github.io/ecma262/#prod-Initializer
    fn read_initializer(&mut self) -> Result<Node, Error> {
        self.read_assignment_expression()
    }

    fn read_binding_pattern(&mut self) -> Result<Node, Error> {
        match self.lexer.peek_skip_lineterminator()?.kind {
            Kind::Symbol(Symbol::OpeningBrace) => self.read_object_binding_pattern(),
            Kind::Symbol(Symbol::OpeningBoxBracket) => self.read_array_binding_pattern(),
            _ => Err(Error::Expect(
                self.lexer.get_current_loc(),
                "expect binding pattern".to_string(),
            )),
        }
    }

    fn read_binding_target(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        match self.lexer.peek_skip_lineterminator()?.kind {
            Kind::Symbol(Symbol::OpeningBrace) | Kind::Symbol(Symbol::OpeningBoxBracket) => {
                self.read_binding_pattern()
            }
            Kind::Identifier(_) => match self.lexer.next_skip_lineterminator()?.kind {
                Kind::Identifier(name) => Ok(Node::new(NodeBase::Identifier(name), loc)),
                _ => unreachable!(),
            },
            _ => Err(Error::Expect(loc, "expect binding identifier".to_string())),
        }
    }

    fn read_binding_element(&mut self) -> Result<(Node, Option<Node>), Error> {
        let target = self.read_binding_target()?;
        let init = if self.lexer.skip(Symbol::Assign)? {
            Some(self.read_initializer()?)
        } else {
            None
        };
        Ok((target, init))
    }

    fn read_array_binding_pattern(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        expect!(self, Kind::Symbol(Symbol::OpeningBoxBracket), "expect '['");
        let mut elements = vec![];

        loop {
            if self.lexer.skip(Symbol::ClosingBoxBracket)? {
                break;
            }
            if self.lexer.skip(Symbol::Comma)? {
                elements.push(ArrayPatternElement::Elision);
                continue;
            }
            if self.lexer.skip(Symbol::Spread)? {
                let target = self.read_binding_target()?;
                elements.push(ArrayPatternElement::Rest(target));
                expect!(
                    self,
                    Kind::Symbol(Symbol::ClosingBoxBracket),
                    "rest element must be last"
                );
                break;
            }

            let (target, init) = self.read_binding_element()?;
            elements.push(ArrayPatternElement::Element(target, init));
            if self.lexer.skip(Symbol::ClosingBoxBracket)? {
                break;
            }
            expect!(self, Kind::Symbol(Symbol::Comma), "expect ',' or ']'");
        }

        Ok(Node::new(NodeBase::ArrayPattern(elements), loc))
    }

    fn read_object_binding_pattern(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");
        let mut properties = vec![];

        loop {
            if self.lexer.skip(Symbol::ClosingBrace)? {
                break;
            }
            if self.lexer.skip(Symbol::Spread)? {
                let target = self.read_binding_target()?;
                properties.push(ObjectPatternProperty::Rest(target));
                expect!(
                    self,
                    Kind::Symbol(Symbol::ClosingBrace),
                    "rest property must be last"
                );
                break;
            }

            let tok = self.lexer.next_skip_lineterminator()?;
            if tok.kind == Kind::Symbol(Symbol::OpeningBoxBracket) {
                let key = self.read_assignment_expression()?;
                expect!(self, Kind::Symbol(Symbol::ClosingBoxBracket), "expect ']'");
                expect!(self, Kind::Symbol(Symbol::Colon), "expect ':'");
                let (target, init) = self.read_binding_element()?;
                properties.push(ObjectPatternProperty::ComputedProperty(key, target, init));
            } else {
                let key = Self::property_name_to_string(tok.kind.clone(), tok.loc)?;
                if self.lexer.skip(Symbol::Colon)? {
                    let (target, init) = self.read_binding_element()?;
                    properties.push(ObjectPatternProperty::Property(key, target, init));
                } else {
                    let name = match tok.kind {
                        Kind::Identifier(name) => name,
                        _ => {
                            return Err(Error::Expect(
                                tok.loc,
                                "expect binding identifier".to_string(),
                            ))
                        }
                    };
                    let init = if self.lexer.skip(Symbol::Assign)? {
                        Some(self.read_initializer()?)
                    } else {
                        None
                    };
                    let target = Node::new(NodeBase::Identifier(name), tok.loc);
                    properties.push(ObjectPatternProperty::Property(key, target, init));
                }
            }

            if self.lexer.skip(Symbol::ClosingBrace)? {
                break;
            }
            expect!(self, Kind::Symbol(Symbol::Comma), "expect ',' or '}'");
        }

        Ok(Node::new(NodeBase::ObjectPattern(properties), loc))
    }

    fn property_name_to_string(kind: Kind, loc: SourceLoc) -> Result<String, Error> {
        match kind {
            Kind::Identifier(name) => Ok(name),
            Kind::Keyword(keyword) => Ok(keyword.to_str().to_string()),
            Kind::Number(n) => Ok(format!("{}", n)),
            Kind::String(s) => Ok(s),
            _ => Err(Error::UnexpectedToken(
                loc,
                "invalid property name.".to_string(),
            )),
        }
    }
}

impl Parser {
    fn read_switch_statement(&mut self, loc: SourceLoc) -> Result<Node, Error> {
        let oparen = self.lexer.next_skip_lineterminator()?;
        if oparen.kind != Kind::Symbol(Symbol::OpeningParen) {
            return Err(Error::Expect(oparen.loc, "expect '('".to_string()));
        }
        let val = self.read_expression()?;
        let cparen = self.lexer.next_skip_lineterminator()?;
        if cparen.kind != Kind::Symbol(Symbol::ClosingParen) {
            return Err(Error::Expect(cparen.loc, "expect ')'".to_string()));
        }
        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");
        let block = self.read_statements(true, true, true)?;
        if let NodeBase::Block(ref items) | NodeBase::StatementList(ref items) = block.base {
            if items
                .iter()
                .filter(|item| matches!(item.base, NodeBase::DefaultLabel))
                .count()
                > 1
            {
                return Err(Error::UnexpectedToken(
                    loc,
                    "switch statement must not have multiple default clauses".to_string(),
                ));
            }
        }
        Ok(Node::new(
            NodeBase::Switch(Box::new(val), Box::new(block)),
            loc,
        ))
    }
}

impl Parser {
    fn read_break_statement(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::LineTerminator
            | Kind::Symbol(Symbol::Semicolon)
            | Kind::Symbol(Symbol::ClosingBrace) => {
                self.lexer.unget();
                Ok(Node::new(NodeBase::Break(None), loc))
            }
            Kind::Identifier(name) => Ok(Node::new(NodeBase::Break(Some(name)), loc)),
            _ => Err(Error::UnexpectedToken(
                tok.loc,
                "expected ';', identifier or line terminator".to_string(),
            )),
        }
    }

    fn read_continue_statement(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::LineTerminator
            | Kind::Symbol(Symbol::Semicolon)
            | Kind::Symbol(Symbol::ClosingBrace) => {
                self.lexer.unget();
                Ok(Node::new(NodeBase::Continue(None), loc))
            }
            Kind::Identifier(name) => Ok(Node::new(NodeBase::Continue(Some(name)), loc)),
            _ => Err(Error::UnexpectedToken(
                tok.loc,
                "expected ';', identifier or line terminator".to_string(),
            )),
        }
    }
}

macro_rules! expression { ( $name:ident, $lower:ident, [ $( $op:path ),* ] ) => {
    fn $name (&mut self) -> Result<Node, Error> {
        let mut lhs = self. $lower ()?;
        while let Ok(tok) = self.lexer.peek_skip_lineterminator() {
            match tok.kind {
                Kind::Symbol(ref op) if $( op == &$op )||* => {
                    self.lexer.next_skip_lineterminator().unwrap();
                    let loc = self.lexer.get_current_loc();
                    lhs = Node::new(NodeBase::BinaryOp(
                        Box::new(lhs),
                        Box::new(self. $lower ()?),
                        op.as_binop().unwrap(),
                    ), loc);
                }
                _ => break
            }
        }
        Ok(lhs)
    }
} }

impl Parser {
    fn read_expression_statement(&mut self) -> Result<Node, Error> {
        self.read_expression()
    }

    // https://tc39.github.io/ecma262/#prod-Expression
    expression!(read_expression, read_assignment_expression, [Symbol::Comma]);

    /// https://tc39.github.io/ecma262/#prod-AssignmentExpression
    // TODO: Implement all features.
    fn read_assignment_expression(&mut self) -> Result<Node, Error> {
        self.lexer.skip_lineterminator()?;
        let loc = self.lexer.get_current_loc();

        if self.peek_identifier("yield") {
            self.lexer.next_skip_lineterminator()?;
            let is_yield_star = if matches!(self.lexer.peek(0)?.kind, Kind::LineTerminator) {
                false
            } else {
                self.lexer.skip(Symbol::Asterisk)?
            };
            let expr = if !is_yield_star && self.is_yield_without_operand() {
                None
            } else {
                Some(Box::new(self.read_assignment_expression()?))
            };
            return Ok(Node::new(NodeBase::Yield(expr, is_yield_star), loc));
        }

        if self.peek_identifier("async") {
            let save_pos = self.lexer.token_pos;
            let f = self.read_async_arrow_function();
            if f.is_err() {
                self.lexer.token_pos = save_pos;
            } else {
                return f;
            }
        }

        // Arrow function
        let next_token = self.lexer.peek(0)?;
        match next_token.kind {
            // (a,b)=>{}
            Kind::Symbol(Symbol::OpeningParen) => {
                let save_pos = self.lexer.token_pos;
                let f = self.read_arrow_function(true);
                if f.is_err() {
                    self.lexer.token_pos = save_pos;
                } else {
                    return f;
                }
            }
            // a=>{}
            Kind::Identifier(_) => match self.lexer.peek(1) {
                Ok(tok) => {
                    if tok.kind == Kind::Symbol(Symbol::FatArrow) {
                        return self.read_arrow_function(false);
                    }
                }
                _ => {}
            },
            _ => {}
        }

        let mut lhs = self.read_conditional_expression()?;

        if let Ok(tok) = self.lexer.next_skip_lineterminator() {
            macro_rules! assignop {
                ($op:ident) => {{
                    let lhs_loc = lhs.loc;
                    lhs = Node::new(
                        NodeBase::AssignOp(
                            Box::new(lhs.clone()),
                            Box::new(self.read_assignment_expression()?),
                            BinOp::$op,
                        ),
                        lhs_loc,
                    );
                }};
            }
            match tok.kind {
                Kind::Symbol(Symbol::Assign) => {
                    let lhs_loc = lhs.loc;
                    let dst = self.expression_to_assignment_pattern(lhs)?;
                    lhs = Node::new(
                        NodeBase::Assign(
                            Box::new(dst),
                            Box::new(self.read_assignment_expression()?),
                        ),
                        lhs_loc,
                    )
                }
                Kind::Symbol(Symbol::AssignAdd) => assignop!(Add),
                Kind::Symbol(Symbol::AssignSub) => assignop!(Sub),
                Kind::Symbol(Symbol::AssignMul) => assignop!(Mul),
                Kind::Symbol(Symbol::AssignDiv) => assignop!(Div),
                Kind::Symbol(Symbol::AssignMod) => assignop!(Rem),
                Kind::Symbol(Symbol::AssignShl) => assignop!(Shl),
                Kind::Symbol(Symbol::AssignShr) => assignop!(Shr),
                Kind::Symbol(Symbol::AssignZFShr) => assignop!(ZFShr),
                Kind::Symbol(Symbol::AssignAnd) => assignop!(And),
                Kind::Symbol(Symbol::AssignOr) => assignop!(Or),
                Kind::Symbol(Symbol::AssignXor) => assignop!(Xor),
                Kind::Symbol(Symbol::AssignLAnd) => assignop!(LAnd),
                Kind::Symbol(Symbol::AssignLOr) => assignop!(LOr),
                Kind::Symbol(Symbol::AssignCoalesce) => assignop!(Coalesce),
                _ => self.lexer.unget(),
            }
        }
        Ok(lhs)
    }

    fn is_yield_without_operand(&mut self) -> bool {
        match self.lexer.peek(0) {
            Ok(tok) => matches!(
                tok.kind,
                Kind::LineTerminator
                    | Kind::EOF
                    | Kind::Symbol(Symbol::Semicolon)
                    | Kind::Symbol(Symbol::ClosingBrace)
                    | Kind::Symbol(Symbol::ClosingParen)
                    | Kind::Symbol(Symbol::ClosingBoxBracket)
                    | Kind::Symbol(Symbol::Comma)
            ),
            Err(_) => true,
        }
    }

    fn expression_to_assignment_pattern(&self, node: Node) -> Result<Node, Error> {
        match node.base {
            NodeBase::Array(elements) => {
                let loc = node.loc;
                let mut pattern_elements = vec![];
                let element_count = elements.len();
                for (index, element) in elements.into_iter().enumerate() {
                    let element_loc = element.loc;
                    match element.base {
                        NodeBase::Nope => pattern_elements.push(ArrayPatternElement::Elision),
                        NodeBase::Spread(inner) => {
                            if index + 1 != element_count {
                                return Err(Error::UnexpectedToken(
                                    element_loc,
                                    "rest element must be last".to_string(),
                                ));
                            }
                            pattern_elements.push(ArrayPatternElement::Rest(
                                self.expression_to_assignment_pattern(*inner)?,
                            ));
                        }
                        NodeBase::Assign(target, init) => {
                            pattern_elements.push(ArrayPatternElement::Element(
                                self.expression_to_assignment_pattern(*target)?,
                                Some(*init),
                            ));
                        }
                        _ => pattern_elements.push(ArrayPatternElement::Element(
                            self.expression_to_assignment_pattern(element)?,
                            None,
                        )),
                    }
                }
                Ok(Node::new(NodeBase::ArrayPattern(pattern_elements), loc))
            }
            NodeBase::Object(properties) => {
                let loc = node.loc;
                let mut pattern_properties = vec![];
                for property in properties {
                    match property {
                        PropertyDefinition::IdentifierReference(name) => {
                            let target = Node::new(NodeBase::Identifier(name.clone()), loc);
                            pattern_properties
                                .push(ObjectPatternProperty::Property(name, target, None));
                        }
                        PropertyDefinition::Property(name, value) => match value.base {
                            NodeBase::Assign(target, init) => {
                                pattern_properties.push(ObjectPatternProperty::Property(
                                    name,
                                    self.expression_to_assignment_pattern(*target)?,
                                    Some(*init),
                                ));
                            }
                            base => {
                                let value = Node::new(base, value.loc);
                                pattern_properties.push(ObjectPatternProperty::Property(
                                    name,
                                    self.expression_to_assignment_pattern(value)?,
                                    None,
                                ));
                            }
                        },
                        PropertyDefinition::ComputedProperty(key, value) => match value.base {
                            NodeBase::Assign(target, init) => {
                                pattern_properties.push(ObjectPatternProperty::ComputedProperty(
                                    key,
                                    self.expression_to_assignment_pattern(*target)?,
                                    Some(*init),
                                ));
                            }
                            base => {
                                let value = Node::new(base, value.loc);
                                pattern_properties.push(ObjectPatternProperty::ComputedProperty(
                                    key,
                                    self.expression_to_assignment_pattern(value)?,
                                    None,
                                ));
                            }
                        },
                        PropertyDefinition::CoverInitializedName(name, init) => {
                            let target = Node::new(NodeBase::Identifier(name.clone()), loc);
                            pattern_properties.push(ObjectPatternProperty::Property(
                                name,
                                target,
                                Some(init),
                            ));
                        }
                        PropertyDefinition::SpreadObject(value) => {
                            pattern_properties.push(ObjectPatternProperty::Rest(
                                self.expression_to_assignment_pattern(value)?,
                            ));
                        }
                        PropertyDefinition::MethodDefinition(_, _, _)
                        | PropertyDefinition::ComputedMethodDefinition(_, _, _) => {
                            return Err(Error::UnexpectedToken(
                                loc,
                                "invalid destructuring assignment target".to_string(),
                            ));
                        }
                    }
                }
                Ok(Node::new(NodeBase::ObjectPattern(pattern_properties), loc))
            }
            _ => Ok(node),
        }
    }

    /// https://tc39.github.io/ecma262/#prod-ConditionalExpression
    fn read_conditional_expression(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();

        let lhs = self.read_short_circuit_expression()?;

        if let Ok(tok) = self.lexer.next_skip_lineterminator() {
            match tok.kind {
                Kind::Symbol(Symbol::Question) => {
                    let _ = self.lexer.skip_lineterminator();
                    let then_ = self.read_assignment_expression()?;
                    expect!(self, Kind::Symbol(Symbol::Colon), "expect ':'");
                    let _ = self.lexer.skip_lineterminator();
                    let else_ = self.read_assignment_expression()?;
                    return Ok(Node::new(
                        NodeBase::TernaryOp(Box::new(lhs), Box::new(then_), Box::new(else_)),
                        loc,
                    ));
                }
                _ => self.lexer.unget(),
            }
        }

        Ok(lhs)
    }

    fn read_short_circuit_expression(&mut self) -> Result<Node, Error> {
        let lhs = self.read_bitwise_or_expression()?;

        if let Ok(tok) = self.lexer.peek_skip_lineterminator() {
            if tok.kind == Kind::Symbol(Symbol::Coalesce) {
                return self.read_coalesce_tail(lhs);
            }
        }

        let lhs = self.read_logical_and_tail(lhs)?;
        let lhs = self.read_logical_or_tail(lhs)?;

        if let Ok(tok) = self.lexer.peek_skip_lineterminator() {
            if tok.kind == Kind::Symbol(Symbol::Coalesce) {
                return Err(Error::UnexpectedToken(
                    tok.loc,
                    "cannot mix '??' with '&&' or '||' without parentheses".to_string(),
                ));
            }
        }

        Ok(lhs)
    }

    fn read_coalesce_tail(&mut self, mut lhs: Node) -> Result<Node, Error> {
        while let Ok(tok) = self.lexer.peek_skip_lineterminator() {
            if tok.kind != Kind::Symbol(Symbol::Coalesce) {
                break;
            }
            self.lexer.next_skip_lineterminator().unwrap();
            let loc = self.lexer.get_current_loc();
            lhs = Node::new(
                NodeBase::BinaryOp(
                    Box::new(lhs),
                    Box::new(self.read_bitwise_or_expression()?),
                    BinOp::Coalesce,
                ),
                loc,
            );
        }
        Ok(lhs)
    }

    fn read_logical_or_tail(&mut self, mut lhs: Node) -> Result<Node, Error> {
        while let Ok(tok) = self.lexer.peek_skip_lineterminator() {
            if tok.kind != Kind::Symbol(Symbol::LOr) {
                break;
            }
            self.lexer.next_skip_lineterminator().unwrap();
            let loc = self.lexer.get_current_loc();
            lhs = Node::new(
                NodeBase::BinaryOp(
                    Box::new(lhs),
                    Box::new(self.read_logical_and_expression()?),
                    BinOp::LOr,
                ),
                loc,
            );
        }
        Ok(lhs)
    }

    fn read_logical_and_expression(&mut self) -> Result<Node, Error> {
        let lhs = self.read_bitwise_or_expression()?;
        self.read_logical_and_tail(lhs)
    }

    fn read_logical_and_tail(&mut self, mut lhs: Node) -> Result<Node, Error> {
        while let Ok(tok) = self.lexer.peek_skip_lineterminator() {
            if tok.kind != Kind::Symbol(Symbol::LAnd) {
                break;
            }
            self.lexer.next_skip_lineterminator().unwrap();
            let loc = self.lexer.get_current_loc();
            lhs = Node::new(
                NodeBase::BinaryOp(
                    Box::new(lhs),
                    Box::new(self.read_bitwise_or_expression()?),
                    BinOp::LAnd,
                ),
                loc,
            );
        }
        Ok(lhs)
    }

    // https://tc39.github.io/ecma262/#prod-BitwiseORExpression
    expression!(
        read_bitwise_or_expression,
        read_bitwise_xor_expression,
        [Symbol::Or]
    );

    // https://tc39.github.io/ecma262/#prod-BitwiseXORExpression
    expression!(
        read_bitwise_xor_expression,
        read_bitwise_and_expression,
        [Symbol::Xor]
    );

    // https://tc39.github.io/ecma262/#prod-BitwiseANDExpression
    expression!(
        read_bitwise_and_expression,
        read_equality_expression,
        [Symbol::And]
    );

    // https://tc39.github.io/ecma262/#prod-EqualityExpression
    expression!(
        read_equality_expression,
        read_relational_expression,
        [Symbol::Eq, Symbol::Ne, Symbol::SEq, Symbol::SNe]
    );

    // https://tc39.github.io/ecma262/#prod-RelationalExpression
    fn read_relational_expression(&mut self) -> Result<Node, Error> {
        let mut lhs = self.read_shift_expression()?;
        while let Ok(tok) = self.lexer.peek_skip_lineterminator() {
            let op = match tok.kind {
                Kind::Symbol(ref op)
                    if op == &Symbol::Lt
                        || op == &Symbol::Gt
                        || op == &Symbol::Le
                        || op == &Symbol::Ge =>
                {
                    op.as_binop()
                }
                Kind::Keyword(Keyword::Instanceof) => Some(BinOp::Instanceof),
                Kind::Keyword(Keyword::In) if self.allow_in => Some(BinOp::In),
                _ => None,
            };
            let Some(op) = op else {
                break;
            };
            self.lexer.next_skip_lineterminator().unwrap();
            let loc = self.lexer.get_current_loc();
            lhs = Node::new(
                NodeBase::BinaryOp(Box::new(lhs), Box::new(self.read_shift_expression()?), op),
                loc,
            );
        }
        Ok(lhs)
    }

    // https://tc39.github.io/ecma262/#prod-ShiftExpression
    expression!(
        read_shift_expression,
        read_additive_expression,
        [Symbol::Shl, Symbol::Shr, Symbol::ZFShr]
    );

    // https://tc39.github.io/ecma262/#prod-AdditiveExpression
    expression!(
        read_additive_expression,
        read_multiplicate_expression,
        [Symbol::Add, Symbol::Sub]
    );

    // https://tc39.github.io/ecma262/#prod-MultiplicativeExpression
    expression!(
        read_multiplicate_expression,
        read_exponentiation_expression,
        [Symbol::Asterisk, Symbol::Div, Symbol::Mod]
    );

    /// https://tc39.github.io/ecma262/#prod-ExponentiationExpression
    fn read_exponentiation_expression(&mut self) -> Result<Node, Error> {
        if self.is_unary_expression() {
            return self.read_unary_expression();
        }
        let loc = self.lexer.get_current_loc();
        let lhs = self.read_update_expression()?;
        if let Ok(tok) = self.lexer.next() {
            if let Kind::Symbol(Symbol::Exp) = tok.kind {
                return Ok(Node::new(
                    NodeBase::BinaryOp(
                        Box::new(lhs),
                        Box::new(self.read_exponentiation_expression()?),
                        BinOp::Exp,
                    ),
                    loc,
                ));
            } else {
                self.lexer.unget();
            }
        }
        Ok(lhs)
    }

    fn is_unary_expression(&mut self) -> bool {
        match self.lexer.peek_skip_lineterminator() {
            Ok(ok) => match ok.kind {
                Kind::Keyword(Keyword::Delete)
                | Kind::Keyword(Keyword::Void)
                | Kind::Keyword(Keyword::Typeof)
                | Kind::Symbol(Symbol::Add)
                | Kind::Symbol(Symbol::Sub)
                | Kind::Symbol(Symbol::BitwiseNot)
                | Kind::Symbol(Symbol::Not) => true,
                Kind::Identifier(ref name) if self.allow_await && name == "await" => true,
                _ => false,
            },
            Err(_) => false,
        }
    }

    /// https://tc39.github.io/ecma262/#prod-UnaryExpression
    fn read_unary_expression(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        let tok = self.lexer.next_skip_lineterminator()?;
        match tok.kind {
            Kind::Keyword(Keyword::Delete) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.read_unary_expression()?), UnaryOp::Delete),
                loc,
            )),
            Kind::Keyword(Keyword::Void) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.read_unary_expression()?), UnaryOp::Void),
                loc,
            )),
            Kind::Keyword(Keyword::Typeof) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.read_unary_expression()?), UnaryOp::Typeof),
                loc,
            )),
            Kind::Symbol(Symbol::Add) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.read_unary_expression()?), UnaryOp::Plus),
                loc,
            )),
            Kind::Symbol(Symbol::Sub) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.read_unary_expression()?), UnaryOp::Minus),
                loc,
            )),
            Kind::Symbol(Symbol::BitwiseNot) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.read_unary_expression()?), UnaryOp::BitwiseNot),
                loc,
            )),
            Kind::Symbol(Symbol::Not) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.read_unary_expression()?), UnaryOp::Not),
                loc,
            )),
            Kind::Identifier(ref name) if self.allow_await && name == "await" => Ok(Node::new(
                NodeBase::Await(Box::new(self.read_unary_expression()?)),
                loc,
            )),
            _ => {
                self.lexer.unget();
                self.read_update_expression()
            }
        }
    }

    /// https://tc39.github.io/ecma262/#prod-UpdateExpression
    // TODO: Implement all features.
    fn read_update_expression(&mut self) -> Result<Node, Error> {
        let tok = self.lexer.peek_skip_lineterminator()?;
        match tok.kind {
            Kind::Symbol(Symbol::Inc) => {
                self.lexer.next_skip_lineterminator().unwrap();
                return Ok(Node::new(
                    NodeBase::UnaryOp(
                        Box::new(self.read_left_hand_side_expression()?),
                        UnaryOp::PrInc,
                    ),
                    tok.loc,
                ));
            }
            Kind::Symbol(Symbol::Dec) => {
                self.lexer.next_skip_lineterminator().unwrap();
                return Ok(Node::new(
                    NodeBase::UnaryOp(
                        Box::new(self.read_left_hand_side_expression()?),
                        UnaryOp::PrDec,
                    ),
                    tok.loc,
                ));
            }
            _ => {}
        }

        let loc = self.lexer.get_current_loc();
        let e = self.read_left_hand_side_expression()?;
        if let Ok(tok) = self.lexer.peek(0) {
            match tok.kind {
                Kind::Symbol(Symbol::Inc) => {
                    self.lexer.next().unwrap();
                    return Ok(Node::new(
                        NodeBase::UnaryOp(Box::new(e), UnaryOp::PoInc),
                        loc,
                    ));
                }
                Kind::Symbol(Symbol::Dec) => {
                    self.lexer.next().unwrap();
                    return Ok(Node::new(
                        NodeBase::UnaryOp(Box::new(e), UnaryOp::PoDec),
                        loc,
                    ));
                }
                _ => {}
            }
        }

        Ok(e)
    }

    /// https://tc39.github.io/ecma262/#prod-LeftHandSideExpression
    /// TODO: Implement NewExpression: new MemberExpression
    fn read_left_hand_side_expression(&mut self) -> Result<Node, Error> {
        let lhs = self.read_member_expression()?;
        match self.lexer.peek_skip_lineterminator() {
            Ok(ref tok) if tok.kind == Kind::Symbol(Symbol::OpeningParen) => {
                self.read_call_expression(lhs)
            }
            _ => self.read_new_expression(lhs),
        }
    }

    /// https://tc39.github.io/ecma262/#prod-NewExpression
    /// TODO: Implement NewExpression: new MemberExpression
    fn read_new_expression(&mut self, first_member_expr: Node) -> Result<Node, Error> {
        Ok(first_member_expr)
    }

    /// https://tc39.github.io/ecma262/#prod-CallExpression
    // TODO: Implement all features.
    fn read_call_expression(&mut self, first_member_expr: Node) -> Result<Node, Error> {
        let loc = first_member_expr.loc;

        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let args = self.read_arguments()?;
        let mut lhs = if matches!(first_member_expr.base, NodeBase::Identifier(ref name) if name == "super")
        {
            Node::new(NodeBase::SuperCall(args), loc)
        } else {
            Node::new(NodeBase::Call(Box::new(first_member_expr), args), loc)
        };

        while let Ok(tok) = self.lexer.next_skip_lineterminator() {
            let loc_ = self.lexer.get_current_loc();
            match tok.kind {
                Kind::Symbol(Symbol::OpeningParen) => {
                    let args = self.read_arguments()?;
                    lhs = Node::new(NodeBase::Call(Box::new(lhs), args), loc)
                }
                Kind::Template(parts) => {
                    let template = self.read_template_literal(parts, tok.loc)?;
                    lhs = Node::new(NodeBase::Call(Box::new(lhs), vec![template]), loc)
                }
                Kind::Symbol(Symbol::Point) => {
                    let prop_tok = self.lexer.next_skip_lineterminator()?;
                    match prop_tok.kind {
                        Kind::Identifier(name) => {
                            lhs = Node::new(NodeBase::Member(Box::new(lhs), name), loc)
                        }
                        Kind::Keyword(kw) => {
                            lhs = Node::new(
                                NodeBase::Member(Box::new(lhs), kw.to_str().to_owned()),
                                loc,
                            )
                        }
                        Kind::Symbol(Symbol::Hash) => {
                            let name = self.read_private_identifier_name(prop_tok.loc)?;
                            lhs = Node::new(NodeBase::PrivateMember(Box::new(lhs), name), loc)
                        }
                        _ => {
                            return Err(Error::Expect(loc_, "expect identifier".to_string()));
                        }
                    }
                }
                Kind::Symbol(Symbol::OpeningBoxBracket) => {
                    let idx = self.read_expression()?;
                    if !self.lexer.skip(Symbol::ClosingBoxBracket).unwrap_or(false) {
                        return Err(Error::Expect(
                            self.lexer.get_current_loc(),
                            "expect ']'".to_string(),
                        ));
                    }
                    lhs = Node::new(NodeBase::Index(Box::new(lhs), Box::new(idx)), loc);
                }
                _ => {
                    self.lexer.unget();
                    break;
                }
            }
        }

        Ok(lhs)
    }

    /// https://tc39.github.io/ecma262/#prod-CallExpression
    // TODO: Implement all features.
    fn read_member_expression(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        let mut lhs = if self.lexer.peek_skip_lineterminator()?.kind == Kind::Keyword(Keyword::New)
        {
            self.lexer.next_skip_lineterminator()?;
            let call_loc = self.lexer.get_current_loc();
            if matches!(
                self.lexer.peek_skip_lineterminator(),
                Ok(Token {
                    kind: Kind::Identifier(ref name),
                    ..
                }) if name == "import"
            ) && matches!(
                self.lexer.peek(1),
                Ok(Token {
                    kind: Kind::Symbol(Symbol::OpeningParen),
                    ..
                })
            ) {
                return Err(Error::UnexpectedToken(
                    call_loc,
                    "import call cannot be used as a constructor".to_string(),
                ));
            }
            let lhs = self.read_member_expression()?;
            let args = if matches!(
                self.lexer.peek_skip_lineterminator(),
                Ok(ref tok) if tok.kind == Kind::Symbol(Symbol::OpeningParen)
            ) {
                self.lexer.next_skip_lineterminator()?;
                self.read_arguments()?
            } else {
                vec![]
            };
            let call_node = Node::new(NodeBase::Call(Box::new(lhs), args), call_loc);
            let new_node = Node::new(NodeBase::New(Box::new(call_node)), loc);
            new_node
        } else {
            self.read_primary_expression()?
        };
        while let Ok(tok) = self.lexer.next_skip_lineterminator() {
            let loc_ = self.lexer.get_current_loc();
            match tok.kind {
                Kind::Symbol(Symbol::Point) => {
                    let prop_tok = self.lexer.next_skip_lineterminator()?;
                    match prop_tok.kind {
                        Kind::Identifier(name) => {
                            lhs = Node::new(NodeBase::Member(Box::new(lhs), name), loc)
                        }
                        Kind::Keyword(kw) => {
                            lhs = Node::new(
                                NodeBase::Member(Box::new(lhs), kw.to_str().to_owned()),
                                loc,
                            )
                        }
                        Kind::Symbol(Symbol::Hash) => {
                            let name = self.read_private_identifier_name(prop_tok.loc)?;
                            lhs = Node::new(NodeBase::PrivateMember(Box::new(lhs), name), loc)
                        }
                        _ => {
                            return Err(Error::Expect(loc_, "expect identifier".to_string()));
                        }
                    }
                }
                Kind::Symbol(Symbol::OpeningBoxBracket) => {
                    let idx = self.read_expression()?;
                    if !self
                        .lexer
                        .skip2(Kind::Symbol(Symbol::ClosingBoxBracket))
                        .unwrap_or(false)
                    {
                        return Err(Error::Expect(
                            self.lexer.get_current_loc(),
                            "expect ']'".to_string(),
                        ));
                    }
                    lhs = Node::new(NodeBase::Index(Box::new(lhs), Box::new(idx)), loc);
                }
                Kind::Template(parts) => {
                    let template = self.read_template_literal(parts, tok.loc)?;
                    lhs = Node::new(NodeBase::Call(Box::new(lhs), vec![template]), loc);
                }
                _ => {
                    self.lexer.unget();
                    break;
                }
            }
        }

        Ok(lhs)
    }

    fn read_arguments(&mut self) -> Result<Vec<Node>, Error> {
        let mut args = vec![];
        loop {
            match self.lexer.next_skip_lineterminator() {
                Ok(ref tok) if tok.kind == Kind::Symbol(Symbol::ClosingParen) => break,
                Ok(ref tok) if tok.kind == Kind::Symbol(Symbol::Comma) => {
                    if args.len() == 0 {
                        return Err(Error::UnexpectedToken(
                            tok.loc,
                            "Unexpected token.".to_string(),
                        ));
                    }
                    if self.lexer.skip(Symbol::ClosingParen)? {
                        break;
                    }
                }
                Ok(ref tok) => {
                    if args.len() != 0 {
                        return Err(Error::Expect(tok.loc, "expect ',' or ')'.".to_string()));
                    } else {
                        self.lexer.unget();
                    }
                }
                Err(_) => {
                    return Err(Error::UnexpectedEOF("".to_string()));
                }
            }
            let loc = self.lexer.get_current_loc();
            if self.lexer.skip(Symbol::Spread)? {
                let arg = self.read_assignment_expression()?;
                args.push(Node::new(NodeBase::Spread(Box::new(arg)), loc));
            } else {
                args.push(self.read_assignment_expression()?);
            }
        }

        Ok(args)
    }

    /// https://tc39.github.io/ecma262/#prod-PrimaryExpression
    fn read_primary_expression(&mut self) -> Result<Node, Error> {
        let tok = self.lexer.next_skip_lineterminator()?;

        match tok.kind {
            Kind::Keyword(Keyword::This) => Ok(Node::new(NodeBase::This, tok.loc)),
            // Kind::Keyword(Keyword::Arguments) => Ok(Node::new(NodeBase::Arguments, tok.pos)),
            Kind::Keyword(Keyword::Function) => self.read_function_expression(),
            Kind::Identifier(ref i)
                if i == "async"
                    && matches!(
                        self.lexer.peek(0),
                        Ok(Token {
                            kind: Kind::Keyword(Keyword::Function),
                            ..
                        })
                    ) =>
            {
                self.lexer.next_skip_lineterminator()?;
                self.read_function_expression_inner(true)
            }
            Kind::Identifier(ref i) if i == "class" => self.read_class_expression(tok.loc),
            Kind::Symbol(Symbol::OpeningParen) => {
                let expr = self.read_expression();
                expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");
                expr
            }
            Kind::Symbol(Symbol::OpeningBoxBracket) => self.read_array_literal(),
            Kind::Symbol(Symbol::OpeningBrace) => self.read_object_literal(),
            Kind::Identifier(ref i) if i == "true" => {
                Ok(Node::new(NodeBase::Boolean(true), tok.loc))
            }
            Kind::Identifier(ref i) if i == "false" => {
                Ok(Node::new(NodeBase::Boolean(false), tok.loc))
            }
            // Kind::Identifier(ref i) if i == "undefined" => {
            //     Ok(Node::new(NodeBase::Undefined, tok.pos))
            // }
            Kind::Identifier(ref i) if i == "null" => Ok(Node::new(NodeBase::Null, tok.loc)),
            Kind::Identifier(ref i)
                if i == "import"
                    && matches!(
                        self.lexer.peek_skip_lineterminator(),
                        Ok(Token {
                            kind: Kind::Symbol(Symbol::OpeningParen),
                            ..
                        })
                    ) =>
            {
                expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");
                let args = self.read_arguments()?;
                if args.is_empty()
                    || args.len() > 2
                    || args
                        .iter()
                        .any(|arg| matches!(arg.base, NodeBase::Spread(_)))
                {
                    return Err(Error::UnexpectedToken(
                        tok.loc,
                        "invalid import call arguments".to_string(),
                    ));
                }
                Ok(Node::new(
                    NodeBase::Call(
                        Box::new(Node::new(
                            NodeBase::Identifier("import".to_string()),
                            tok.loc,
                        )),
                        args,
                    ),
                    tok.loc,
                ))
            }
            Kind::Identifier(ident) => Ok(Node::new(NodeBase::Identifier(ident), tok.loc)),
            Kind::String(s) => Ok(Node::new(NodeBase::String(s), tok.loc)),
            Kind::RegExp(pattern, flags) => Ok(Node::new(
                NodeBase::New(Box::new(Node::new(
                    NodeBase::Call(
                        Box::new(Node::new(
                            NodeBase::Identifier("RegExp".to_string()),
                            tok.loc,
                        )),
                        vec![
                            Node::new(NodeBase::String(pattern), tok.loc),
                            Node::new(NodeBase::String(flags), tok.loc),
                        ],
                    ),
                    tok.loc,
                ))),
                tok.loc,
            )),
            Kind::Template(parts) => self.read_template_literal(parts, tok.loc),
            Kind::Number(num) => Ok(Node::new(NodeBase::Number(num), tok.loc)),
            Kind::BigInt(num) => Ok(Node::new(NodeBase::BigInt(num), tok.loc)),
            _ => Err(Error::UnexpectedToken(
                tok.loc,
                format!("unexpected token."),
            )),
        }
    }

    fn read_class_expression(&mut self, loc: SourceLoc) -> Result<Node, Error> {
        let mut is_anonymous = false;
        let class_name = match self.lexer.peek_skip_lineterminator() {
            Ok(Token {
                kind: Kind::Identifier(ref name),
                ..
            }) if name != "extends" => {
                let Kind::Identifier(name) = self.lexer.next_skip_lineterminator()?.kind else {
                    unreachable!();
                };
                name
            }
            _ => {
                is_anonymous = true;
                "__Class".to_string()
            }
        };

        let heritage = if self.peek_identifier("extends") {
            self.lexer.next_skip_lineterminator()?;
            self.reject_arrow_class_heritage()?;
            Some(self.read_left_hand_side_expression()?)
        } else {
            None
        };

        let mut body_items = self.read_class_body_items(&class_name, heritage, loc)?;
        body_items.push(Node::new(
            NodeBase::Return(Some(Box::new(Node::new(
                NodeBase::Identifier(class_name),
                loc,
            )))),
            loc,
        ));
        let body = Node::new(NodeBase::StatementList(body_items), loc);
        let class_expr = Node::new(
            NodeBase::Call(
                Box::new(Node::new(
                    NodeBase::FunctionExpr(None, vec![], Box::new(body)),
                    loc,
                )),
                vec![],
            ),
            loc,
        );
        if is_anonymous {
            Ok(Node::new(
                NodeBase::AnonymousClassExpr(Box::new(class_expr)),
                loc,
            ))
        } else {
            Ok(class_expr)
        }
    }

    fn read_template_literal(
        &mut self,
        parts: Vec<rapidus_lexer::token::TemplatePart>,
        loc: SourceLoc,
    ) -> Result<Node, Error> {
        fn concat(left: Node, right: Node, loc: SourceLoc) -> Node {
            Node::new(
                NodeBase::BinaryOp(Box::new(left), Box::new(right), BinOp::Add),
                loc,
            )
        }

        fn parse_template_expr(source: String, loc: SourceLoc) -> Result<Node, Error> {
            let mut parser = Parser::new("template", source);
            let node = parser.parse_all()?;
            match node.base {
                NodeBase::StatementList(mut list) if list.len() == 1 => Ok(list.remove(0)),
                _ => Err(Error::UnexpectedToken(
                    loc,
                    "expected template expression".to_string(),
                )),
            }
        }

        let mut node = Node::new(NodeBase::String(String::new()), loc);
        for part in parts {
            if !part.cooked.is_empty() {
                node = concat(node, Node::new(NodeBase::String(part.cooked), loc), loc);
            }
            if let Some(expr) = part.expr {
                node = concat(node, parse_template_expr(expr, loc)?, loc);
            }
        }
        Ok(node)
    }

    /// https://www.ecma-international.org/ecma-262/6.0/#sec-arrow-function-definitions
    fn read_arrow_function(&mut self, is_parenthesized_param: bool) -> Result<Node, Error> {
        let params;
        let params_loc = self.lexer.get_current_loc();
        if is_parenthesized_param {
            expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");
            params = self.read_formal_parameters()?;
        } else {
            let param_name = match self.lexer.next()?.kind {
                Kind::Identifier(s) => s,
                _ => unreachable!(),
            };
            params = vec![FormalParameter {
                init: None,
                name: param_name,
                pattern: None,
                is_rest_param: false,
            }];
        }
        expect_no_lineterminator!(self, Kind::Symbol(Symbol::FatArrow), "expect '=>'");
        let body = if self.lexer.skip(Symbol::OpeningBrace)? {
            self.read_block()?
        } else {
            let loc = self.lexer.get_current_loc();
            Node::new(
                NodeBase::Return(Some(Box::new(self.read_assignment_expression()?))),
                loc,
            )
        };
        Self::validate_formal_parameters(&params, &body, params_loc, true)?;
        Ok(Node::new(
            NodeBase::ArrowFunction(params, Box::new(body)),
            params_loc,
        ))
    }

    fn read_async_arrow_function(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        let async_tok = self.lexer.next()?;
        if !matches!(async_tok.kind, Kind::Identifier(ref name) if name == "async") {
            return Err(Error::UnexpectedToken(
                async_tok.loc,
                "expect async".to_string(),
            ));
        }

        let prev_allow_await = self.allow_await;
        self.allow_await = true;
        let params = match self.lexer.peek(0)?.kind {
            Kind::Symbol(Symbol::OpeningParen) => {
                self.lexer.next()?;
                self.read_formal_parameters()?
            }
            Kind::Identifier(_) => {
                let param_tok = self.lexer.next()?;
                let Kind::Identifier(param_name) = param_tok.kind else {
                    unreachable!();
                };
                if param_name == "await" {
                    self.allow_await = prev_allow_await;
                    return Err(Error::UnexpectedToken(
                        param_tok.loc,
                        "await cannot be used as a binding identifier in an async function"
                            .to_string(),
                    ));
                }
                vec![FormalParameter {
                    init: None,
                    name: param_name,
                    pattern: None,
                    is_rest_param: false,
                }]
            }
            _ => {
                self.allow_await = prev_allow_await;
                return Err(Error::UnexpectedToken(
                    async_tok.loc,
                    "expect async arrow parameters".to_string(),
                ));
            }
        };

        expect_no_lineterminator!(self, Kind::Symbol(Symbol::FatArrow), "expect '=>'");
        let body = if self.lexer.skip(Symbol::OpeningBrace)? {
            self.read_block()?
        } else {
            let loc = self.lexer.get_current_loc();
            Node::new(
                NodeBase::Return(Some(Box::new(self.read_assignment_expression()?))),
                loc,
            )
        };
        self.allow_await = prev_allow_await;
        Self::validate_formal_parameters(&params, &body, loc, true)?;
        Ok(Node::new(
            NodeBase::AsyncArrowFunction(params, Box::new(body)),
            loc,
        ))
    }

    /// https://tc39.github.io/ecma262/#prod-FunctionDeclaration
    fn read_function_expression(&mut self) -> Result<Node, Error> {
        self.read_function_expression_inner(false)
    }

    fn read_function_expression_inner(&mut self, is_async: bool) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        let is_generator = self.lexer.skip(Symbol::Asterisk)?;
        let name = if let Kind::Identifier(name) = self.lexer.peek(0)?.kind {
            self.lexer.next()?;
            Some(name)
        } else {
            None
        };

        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let params = self.read_formal_parameters()?;

        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");

        let prev_allow_await = self.allow_await;
        self.allow_await = is_async;
        let body = self.read_block();
        self.allow_await = prev_allow_await;
        let body = body?;
        Self::validate_formal_parameters(&params, &body, loc, is_async || is_generator)?;
        Self::validate_formal_parameter_context(&params, loc, is_generator, is_async)?;
        if let Some(ref name) = name {
            Self::validate_strict_function_name(name, &body, loc)?;
        }

        Ok(Node::new(
            if is_async && is_generator {
                NodeBase::AsyncGeneratorFunctionExpr(name, params, Box::new(body))
            } else if is_async {
                NodeBase::AsyncFunctionExpr(name, params, Box::new(body))
            } else if is_generator {
                NodeBase::GeneratorFunctionExpr(name, params, Box::new(body))
            } else {
                NodeBase::FunctionExpr(name, params, Box::new(body))
            },
            loc,
        ))
    }

    /// https://tc39.github.io/ecma262/#prod-ArrayLiteral
    fn read_array_literal(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        let mut elements = vec![];

        loop {
            // TODO: Support all features.
            while self
                .lexer
                .skip(Kind::Symbol(Symbol::Comma))
                .unwrap_or(false)
            {
                elements.push(Node::new(NodeBase::Nope, loc));
            }

            if self
                .lexer
                .skip(Kind::Symbol(Symbol::ClosingBoxBracket))
                .unwrap_or(false)
            {
                break;
            }

            if self.lexer.is_empty() {
                return Err(Error::UnexpectedEOF("']' may be needed".to_string()));
            }

            if self.lexer.skip(Symbol::Spread)? {
                let node = self.read_assignment_expression()?;
                let loc = node.loc;
                elements.push(Node::new(NodeBase::Spread(Box::new(node)), loc));
                if self.lexer.skip(Kind::Symbol(Symbol::Comma))? {
                    if self.lexer.peek(0)?.kind == Kind::Symbol(Symbol::ClosingBoxBracket) {
                        return Err(Error::UnexpectedToken(
                            loc,
                            "rest element must be last".to_string(),
                        ));
                    }
                }
            } else {
                elements.push(self.read_assignment_expression()?);
                self.lexer.skip(Kind::Symbol(Symbol::Comma))?;
            }
        }

        Ok(Node::new(NodeBase::Array(elements), loc))
    }

    /// https://tc39.github.io/ecma262/#prod-ObjectLiteral
    fn read_object_literal(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        let mut elements = vec![];

        loop {
            if self.lexer.skip(Symbol::ClosingBrace)? {
                break;
            }

            elements.push(self.read_property_definition()?);

            if self.lexer.skip(Symbol::ClosingBrace)? {
                break;
            }

            if !self.lexer.skip(Symbol::Comma)? {
                return Err(Error::Expect(
                    self.lexer.get_current_loc(),
                    "expect ',' or '}'.".to_string(),
                ));
            }
        }

        Ok(Node::new(NodeBase::Object(elements), loc))
    }

    /// https://tc39.github.io/ecma262/#prod-PropertyDefinition
    fn read_property_definition(&mut self) -> Result<PropertyDefinition, Error> {
        fn to_string(kind: Kind, loc: SourceLoc) -> Result<String, Error> {
            match kind {
                Kind::Identifier(name) => Ok(name),
                Kind::Keyword(keyword) => Ok(keyword.to_str().to_string()),
                Kind::Number(n) => Ok(format!("{}", n)),
                Kind::String(s) => Ok(s),
                _ => Err(Error::UnexpectedToken(
                    loc,
                    "invalid property name.".to_string(),
                )),
            }
        }

        if self.lexer.skip(Symbol::Spread)? {
            let node = self.read_assignment_expression()?;
            return Ok(PropertyDefinition::SpreadObject(node));
        }

        if self.lexer.skip(Symbol::Asterisk)? {
            let name_tok = self.lexer.next_skip_lineterminator()?;
            if name_tok.kind == Kind::Symbol(Symbol::OpeningBoxBracket) {
                let key = self.read_assignment_expression()?;
                expect!(self, Kind::Symbol(Symbol::ClosingBoxBracket), "expect ']'");
                expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");
                let params = self.read_formal_parameters()?;
                expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");
                let body = self.read_block()?;
                Self::validate_formal_parameters(&params, &body, name_tok.loc, true)?;
                Self::validate_formal_parameter_context(&params, name_tok.loc, true, false)?;
                let func = Node::new(
                    NodeBase::GeneratorFunctionExpr(None, params, Box::new(body)),
                    name_tok.loc,
                );
                return Ok(PropertyDefinition::ComputedMethodDefinition(
                    MethodDefinitionKind::Ordinary,
                    key,
                    func,
                ));
            }

            let name = to_string(name_tok.kind, name_tok.loc)?;
            expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");
            let params = self.read_formal_parameters()?;
            expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");
            let body = self.read_block()?;
            Self::validate_formal_parameters(&params, &body, name_tok.loc, true)?;
            Self::validate_formal_parameter_context(&params, name_tok.loc, true, false)?;
            let func = Node::new(
                NodeBase::GeneratorFunctionExpr(Some(name.clone()), params, Box::new(body)),
                name_tok.loc,
            );
            return Ok(PropertyDefinition::MethodDefinition(
                MethodDefinitionKind::Ordinary,
                name,
                func,
            ));
        }

        if self.peek_identifier("async") {
            let save_pos = self.lexer.token_pos;
            let async_tok = self.lexer.next_skip_lineterminator()?;
            if matches!(self.lexer.peek(0)?.kind, Kind::LineTerminator) {
                self.lexer.token_pos = save_pos;
            } else {
                match self.lexer.peek_skip_lineterminator()?.kind {
                    Kind::Symbol(
                        Symbol::OpeningParen
                        | Symbol::Colon
                        | Symbol::Assign
                        | Symbol::Comma
                        | Symbol::ClosingBrace,
                    ) => {
                        self.lexer.token_pos = save_pos;
                    }
                    Kind::Symbol(Symbol::Asterisk) => {
                        self.lexer.next_skip_lineterminator()?;
                        let name_tok = self.lexer.next_skip_lineterminator()?;
                        if name_tok.kind == Kind::Symbol(Symbol::OpeningBoxBracket) {
                            let key = self.read_assignment_expression()?;
                            expect!(self, Kind::Symbol(Symbol::ClosingBoxBracket), "expect ']'");
                            expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");
                            let params = self.read_formal_parameters()?;
                            expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");
                            let prev_allow_await = self.allow_await;
                            self.allow_await = true;
                            let body = self.read_block();
                            self.allow_await = prev_allow_await;
                            let body = body?;
                            Self::validate_formal_parameters(&params, &body, async_tok.loc, true)?;
                            Self::validate_formal_parameter_context(
                                &params,
                                async_tok.loc,
                                true,
                                true,
                            )?;
                            let func = Node::new(
                                NodeBase::AsyncGeneratorFunctionExpr(None, params, Box::new(body)),
                                async_tok.loc,
                            );
                            return Ok(PropertyDefinition::ComputedMethodDefinition(
                                MethodDefinitionKind::Ordinary,
                                key,
                                func,
                            ));
                        }

                        let name = to_string(name_tok.kind, name_tok.loc)?;
                        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");
                        let params = self.read_formal_parameters()?;
                        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");
                        let prev_allow_await = self.allow_await;
                        self.allow_await = true;
                        let body = self.read_block();
                        self.allow_await = prev_allow_await;
                        let body = body?;
                        Self::validate_formal_parameters(&params, &body, async_tok.loc, true)?;
                        Self::validate_formal_parameter_context(
                            &params,
                            async_tok.loc,
                            true,
                            true,
                        )?;
                        let func = Node::new(
                            NodeBase::AsyncGeneratorFunctionExpr(
                                Some(name.clone()),
                                params,
                                Box::new(body),
                            ),
                            async_tok.loc,
                        );
                        return Ok(PropertyDefinition::MethodDefinition(
                            MethodDefinitionKind::Ordinary,
                            name,
                            func,
                        ));
                    }
                    Kind::Symbol(Symbol::OpeningBoxBracket) => {
                        self.lexer.next_skip_lineterminator()?;
                        let key = self.read_assignment_expression()?;
                        expect!(self, Kind::Symbol(Symbol::ClosingBoxBracket), "expect ']'");
                        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");
                        let params = self.read_formal_parameters()?;
                        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");
                        let prev_allow_await = self.allow_await;
                        self.allow_await = true;
                        let body = self.read_block();
                        self.allow_await = prev_allow_await;
                        let body = body?;
                        Self::validate_unique_formal_parameters(&params, &body, async_tok.loc)?;
                        Self::validate_formal_parameter_context(
                            &params,
                            async_tok.loc,
                            false,
                            true,
                        )?;
                        let func = Node::new(
                            NodeBase::AsyncFunctionExpr(None, params, Box::new(body)),
                            async_tok.loc,
                        );
                        return Ok(PropertyDefinition::ComputedMethodDefinition(
                            MethodDefinitionKind::Ordinary,
                            key,
                            func,
                        ));
                    }
                    Kind::Identifier(_) | Kind::Keyword(_) | Kind::Number(_) | Kind::String(_) => {
                        let name_tok = self.lexer.next_skip_lineterminator()?;
                        let name = to_string(name_tok.kind, name_tok.loc)?;
                        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");
                        let params = self.read_formal_parameters()?;
                        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");
                        let prev_allow_await = self.allow_await;
                        self.allow_await = true;
                        let body = self.read_block();
                        self.allow_await = prev_allow_await;
                        let body = body?;
                        Self::validate_unique_formal_parameters(&params, &body, async_tok.loc)?;
                        Self::validate_formal_parameter_context(
                            &params,
                            async_tok.loc,
                            false,
                            true,
                        )?;
                        let func = Node::new(
                            NodeBase::AsyncFunctionExpr(Some(name.clone()), params, Box::new(body)),
                            async_tok.loc,
                        );
                        return Ok(PropertyDefinition::MethodDefinition(
                            MethodDefinitionKind::Ordinary,
                            name,
                            func,
                        ));
                    }
                    _ => {
                        self.lexer.token_pos = save_pos;
                    }
                }
            }
        }

        let tok = self.lexer.next_skip_lineterminator()?;
        let tok_contains_escape = tok.contains_escape;

        if tok.kind == Kind::Symbol(Symbol::OpeningBoxBracket) {
            let key = self.read_assignment_expression()?;
            expect!(self, Kind::Symbol(Symbol::ClosingBoxBracket), "expect ']'");
            if self.lexer.skip(Symbol::Colon)? {
                let val = self.read_assignment_expression()?;
                return Ok(PropertyDefinition::ComputedProperty(key, val));
            }
            if self.lexer.skip(Symbol::OpeningParen)? {
                let params = self.read_formal_parameters()?;
                expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");
                let body = self.read_block()?;
                Self::validate_formal_parameters(&params, &body, tok.loc, true)?;
                let func = Node::new(
                    NodeBase::FunctionExpr(None, params, Box::new(body)),
                    tok.loc,
                );
                return Ok(PropertyDefinition::ComputedMethodDefinition(
                    MethodDefinitionKind::Ordinary,
                    key,
                    func,
                ));
            }
            return Err(Error::Expect(
                self.lexer.get_current_loc(),
                "expect ':' or '('.".to_string(),
            ));
        }

        if self.lexer.skip(Symbol::Colon)? {
            let val = self.read_assignment_expression()?;
            return Ok(PropertyDefinition::Property(
                to_string(tok.kind, tok.loc)?,
                val,
            ));
        }

        if self.lexer.skip(Symbol::OpeningParen)? {
            let name = to_string(tok.kind, tok.loc)?;
            let params = self.read_formal_parameters()?;
            expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");
            let body = self.read_block()?;
            Self::validate_formal_parameters(&params, &body, tok.loc, true)?;
            let func = Node::new(
                NodeBase::FunctionExpr(Some(name.clone()), params, Box::new(body)),
                tok.loc,
            );
            return Ok(PropertyDefinition::MethodDefinition(
                MethodDefinitionKind::Ordinary,
                name,
                func,
            ));
        }

        if let Kind::Identifier(name) = tok.kind {
            if !tok_contains_escape && (name == "get" || name == "set") {
                let may_identifier = self.lexer.peek_skip_lineterminator();
                if self.lexer.skip(Symbol::OpeningBoxBracket)? {
                    let key = self.read_assignment_expression()?;
                    expect!(self, Kind::Symbol(Symbol::ClosingBoxBracket), "expect ']'");
                    expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");
                    let params = self.read_formal_parameters()?;
                    expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");
                    let body = self.read_block()?;
                    Self::validate_formal_parameters(&params, &body, tok.loc, true)?;
                    let func = Node::new(
                        NodeBase::FunctionExpr(None, params, Box::new(body)),
                        tok.loc,
                    );
                    return Ok(PropertyDefinition::ComputedMethodDefinition(
                        if name == "get" {
                            MethodDefinitionKind::Get
                        } else {
                            MethodDefinitionKind::Set
                        },
                        key,
                        func,
                    ));
                }
                if let Ok(property_name) = may_identifier {
                    if matches!(
                        property_name.kind,
                        Kind::Identifier(_) | Kind::Keyword(_) | Kind::Number(_) | Kind::String(_)
                    ) {
                        let property_name = self.lexer.next_skip_lineterminator()?;
                        let func_name = to_string(property_name.kind, property_name.loc)?;
                        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");
                        let params = self.read_formal_parameters()?;
                        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");
                        let body = self.read_block()?;
                        Self::validate_formal_parameters(&params, &body, tok.loc, true)?;
                        let f = Node::new(
                            NodeBase::FunctionExpr(Some(func_name.clone()), params, Box::new(body)),
                            tok.loc,
                        );
                        return Ok(PropertyDefinition::MethodDefinition(
                            if name == "get" {
                                MethodDefinitionKind::Get
                            } else {
                                MethodDefinitionKind::Set
                            },
                            func_name,
                            f,
                        ));
                    }
                }
            }

            if self.lexer.skip(Symbol::Assign)? {
                let init = self.read_assignment_expression()?;
                return Ok(PropertyDefinition::CoverInitializedName(name, init));
            }

            return Ok(PropertyDefinition::IdentifierReference(name));
        }

        Err(Error::Expect(
            tok.loc,
            "Expect property definition.".to_string(),
        ))
    }
}

macro_rules! skip_symbol_or_error {
    ($lexer: expr, $symbol: path) => {
        if !$lexer.skip($symbol)? {
            return Err(Error::UnexpectedToken($lexer.get_current_loc(), {
                let name: String = $symbol.into();
                format!("expected {}", name)
            }));
        };
    };
}

impl Parser {
    /// http://www.ecma-international.org/ecma-262/9.0/index.html#sec-try-statement
    fn read_try_statement(&mut self) -> Result<Node, Error> {
        let loc_try = self.lexer.get_current_loc();
        skip_symbol_or_error!(self.lexer, Symbol::OpeningBrace);
        let try_clause = self.read_block_statement()?;
        let is_catch = self.lexer.skip(Keyword::Catch).unwrap_or(false);
        let loc_catch = self.lexer.get_current_loc();
        let (catch, param) = if is_catch {
            let catch_param = if self.lexer.skip(Symbol::OpeningParen)? {
                let catch_param = self.read_binding_target()?;
                skip_symbol_or_error!(self.lexer, Symbol::ClosingParen);
                catch_param
            } else {
                Node::new(NodeBase::Nope, loc_catch)
            };
            skip_symbol_or_error!(self.lexer, Symbol::OpeningBrace);
            (self.read_block()?, catch_param)
        } else {
            (
                Node::new(NodeBase::Nope, loc_catch),
                Node::new(NodeBase::Nope, loc_catch),
            )
        };
        let is_finally = self.lexer.skip(Keyword::Finally).unwrap_or(false);
        let loc_finally = self.lexer.get_current_loc();
        let finally = if is_finally {
            skip_symbol_or_error!(self.lexer, Symbol::OpeningBrace);
            self.read_block_statement()?
        } else {
            Node::new(NodeBase::Nope, loc_finally)
        };

        Ok(Node::new(
            NodeBase::Try(
                Box::new(try_clause),
                Box::new(catch),
                Box::new(param),
                Box::new(finally),
            ),
            loc_try,
        ))
    }
}

impl Parser {
    /// https://tc39.github.io/ecma262/#prod-ThrowStatement
    fn read_throw_statement(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();

        // no LineTerminator here
        if self.lexer.skip2(Kind::LineTerminator).unwrap_or(false) {
            return Err(Error::General(
                loc,
                "Illegal new line after 'throw'".to_string(),
            ));
        }

        if self
            .lexer
            .skip2(Kind::Symbol(Symbol::Semicolon))
            .unwrap_or(false)
        {
            return Err(Error::UnexpectedToken(
                loc,
                "Unexpected token ;".to_string(),
            ));
        }

        if self.lexer.peek(0)?.kind == Kind::Symbol(Symbol::ClosingBrace) {
            return Err(Error::UnexpectedToken(
                loc,
                "Unexpected token }".to_string(),
            ));
        }

        let expr = self.read_expression()?;
        let _ = self.lexer.skip(Symbol::Semicolon);

        Ok(Node::new(NodeBase::Throw(Box::new(expr)), loc))
    }
}

impl Parser {
    fn read_declaration(&mut self) -> Result<Node, Error> {
        let tok = self.lexer.next_skip_lineterminator()?;
        match tok.kind {
            Kind::Keyword(Keyword::Function) => self.read_function_declaration(),
            Kind::Keyword(Keyword::Const) => self.read_lexical_declaration(true),
            Kind::Keyword(Keyword::Let) => self.read_lexical_declaration(false),
            Kind::Identifier(ref name) if name == "class" => self.read_class_declaration(tok.loc),
            _ => unreachable!(),
        }
    }

    fn read_class_declaration(&mut self, loc: SourceLoc) -> Result<Node, Error> {
        let name = match self.lexer.next_skip_lineterminator()?.kind {
            Kind::Identifier(name) => name,
            _ => return Err(Error::Expect(loc, "expect class name".to_string())),
        };

        let heritage = if self.peek_identifier("extends") {
            self.lexer.next_skip_lineterminator()?;
            self.reject_arrow_class_heritage()?;
            Some(self.read_left_hand_side_expression()?)
        } else {
            None
        };
        let items = self.read_class_body_items(&name, heritage, loc)?;
        Ok(Node::new(NodeBase::StatementList(items), loc))
    }

    fn read_class_body_items(
        &mut self,
        class_name: &str,
        heritage: Option<Node>,
        loc: SourceLoc,
    ) -> Result<Vec<Node>, Error> {
        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");

        self.private_name_stack.push(HashMap::new());
        let has_heritage = heritage.is_some();
        let mut constructor = None;
        let mut instance_fields = vec![];
        let mut items = vec![];
        while !self.lexer.skip(Symbol::ClosingBrace)? {
            if self.lexer.skip(Symbol::Semicolon)? {
                continue;
            }
            if let Some(item) = self.read_class_element(class_name, has_heritage)? {
                if Self::is_instance_field_initializer(&item) {
                    instance_fields.push(item);
                } else {
                    match item.base {
                        NodeBase::FunctionDecl(_, _, _)
                        | NodeBase::DerivedConstructorDecl(_, _, _) => {
                            if constructor.is_some() {
                                return Err(Error::UnexpectedToken(
                                    item.loc,
                                    "duplicate constructor".to_string(),
                                ));
                            }
                            constructor = Some(item);
                        }
                        _ => items.push(item),
                    }
                }
            }
        }

        let constructor = constructor.unwrap_or_else(|| {
            let base = if has_heritage {
                let super_call = Node::new(NodeBase::SuperCallFromArguments, loc);
                NodeBase::DerivedConstructorDecl(
                    class_name.to_string(),
                    vec![],
                    Box::new(Node::new(
                        NodeBase::Block(vec![Node::new(
                            NodeBase::Return(Some(Box::new(super_call))),
                            loc,
                        )]),
                        loc,
                    )),
                )
            } else {
                NodeBase::FunctionDecl(
                    class_name.to_string(),
                    vec![],
                    Box::new(Node::new(NodeBase::Block(vec![]), loc)),
                )
            };
            Node::new(base, loc)
        });
        let constructor = Self::prepend_instance_fields(constructor, instance_fields);
        items.insert(0, constructor);
        if let Some(heritage) = heritage {
            items.insert(
                1,
                Node::new(
                    NodeBase::ClassHeritageSetup(class_name.to_string(), Box::new(heritage)),
                    loc,
                ),
            );
        }
        self.private_name_stack.pop();
        Ok(items)
    }

    fn is_instance_field_initializer(node: &Node) -> bool {
        let NodeBase::Assign(ref dst, _) = node.base else {
            return false;
        };
        match dst.base {
            NodeBase::Member(ref parent, _) | NodeBase::Index(ref parent, _) => {
                parent.base == NodeBase::This
            }
            NodeBase::PrivateMemberInit(ref parent, _, _)
            | NodeBase::PrivateAccessorInit(ref parent, _, _) => parent.base == NodeBase::This,
            _ => false,
        }
    }

    fn prepend_instance_fields(constructor: Node, mut fields: Vec<Node>) -> Node {
        if fields.is_empty() {
            return constructor;
        }

        let Node { base, loc } = constructor;
        let (name, params, body, derived) = match base {
            NodeBase::FunctionDecl(name, params, body) => (name, params, body, false),
            NodeBase::DerivedConstructorDecl(name, params, body) => (name, params, body, true),
            _ => return Node::new(base, loc),
        };

        let mut body_items = match body.base {
            NodeBase::Block(items) => items,
            _ => vec![*body],
        };
        fields.append(&mut body_items);

        let base = if derived {
            NodeBase::DerivedConstructorDecl(
                name,
                params,
                Box::new(Node::new(NodeBase::Block(fields), loc)),
            )
        } else {
            NodeBase::FunctionDecl(
                name,
                params,
                Box::new(Node::new(NodeBase::Block(fields), loc)),
            )
        };
        Node::new(base, loc)
    }

    fn read_class_element(
        &mut self,
        class_name: &str,
        class_has_heritage: bool,
    ) -> Result<Option<Node>, Error> {
        let loc = self.lexer.get_current_loc();
        let mut is_static = false;
        let mut is_async = false;
        let mut is_generator = false;

        if self.peek_identifier("static") {
            let save_pos = self.lexer.token_pos;
            self.lexer.next_skip_lineterminator()?;
            if !matches!(
                self.lexer.peek_skip_lineterminator()?.kind,
                Kind::Symbol(Symbol::OpeningParen)
                    | Kind::Symbol(Symbol::Assign)
                    | Kind::Symbol(Symbol::Semicolon)
                    | Kind::Symbol(Symbol::ClosingBrace)
            ) {
                is_static = true;
            } else {
                self.lexer.token_pos = save_pos;
            }
        }

        if self.lexer.skip(Symbol::Asterisk)? {
            is_generator = true;
        }

        if is_static
            && matches!(
                self.lexer.peek_skip_lineterminator()?.kind,
                Kind::Symbol(Symbol::OpeningBrace)
            )
        {
            expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");
            return Ok(Some(self.read_block_statement()?));
        }

        let mut accessor = None;
        if self.peek_identifier("get") || self.peek_identifier("set") {
            let save_pos = self.lexer.token_pos;
            let keyword = match self.lexer.next_skip_lineterminator()?.kind {
                Kind::Identifier(name) => name,
                _ => unreachable!(),
            };
            if !matches!(
                self.lexer.peek_skip_lineterminator()?.kind,
                Kind::Symbol(Symbol::OpeningParen)
            ) {
                accessor = Some(keyword);
            } else {
                self.lexer.token_pos = save_pos;
            }
        }

        if self.peek_identifier("async") {
            let save_pos = self.lexer.token_pos;
            self.lexer.next_skip_lineterminator()?;
            if matches!(self.lexer.peek(0)?.kind, Kind::LineTerminator) {
                self.lexer.token_pos = save_pos;
            } else {
                match self.lexer.peek_skip_lineterminator()?.kind {
                    Kind::Symbol(
                        Symbol::OpeningParen
                        | Symbol::Assign
                        | Symbol::Semicolon
                        | Symbol::ClosingBrace,
                    ) => {
                        self.lexer.token_pos = save_pos;
                    }
                    Kind::Symbol(Symbol::Asterisk) => {
                        self.lexer.next_skip_lineterminator()?;
                        is_async = true;
                        is_generator = true;
                    }
                    _ => {
                        is_async = true;
                    }
                }
            }
        }

        let Some(element_name) = self.read_class_element_name()? else {
            return Err(Error::UnsupportedFeature(loc));
        };
        let ClassElementName {
            func_name,
            property_key,
            is_private,
        } = element_name;

        if !self.lexer.skip(Symbol::OpeningParen)? {
            if is_private {
                self.register_private_name(
                    func_name.as_deref().expect("private class element name"),
                    PRIVATE_NAME_OTHER,
                    loc,
                )?;
            }
            if accessor.is_some() {
                return Err(Error::UnsupportedFeature(loc));
            }
            if !is_private
                && (func_name.as_deref() == Some("constructor")
                    || (is_static && func_name.as_deref() == Some("prototype")))
            {
                return Err(Error::UnexpectedToken(
                    loc,
                    "invalid class field name".to_string(),
                ));
            }

            let (init, field_end_line) = if self.lexer.skip(Symbol::Assign)? {
                let init = self.read_assignment_expression()?;
                let field_end_line = init.loc.line;
                (init, field_end_line)
            } else {
                (
                    Node::new(NodeBase::Identifier("undefined".to_string()), loc),
                    property_key.loc.line,
                )
            };
            if Self::contains_identifier_reference(&init, "arguments")
                || Self::contains_super_call(&init)
            {
                return Err(Error::UnexpectedToken(
                    init.loc,
                    "invalid class field initializer".to_string(),
                ));
            }
            if !self.lexer.skip(Symbol::Semicolon)? {
                let next = self.lexer.peek(0)?;
                if !matches!(
                    next.kind,
                    Kind::LineTerminator | Kind::Symbol(Symbol::ClosingBrace) | Kind::EOF
                ) && next.loc.line == field_end_line
                {
                    return Err(Error::UnexpectedToken(
                        next.loc,
                        "invalid class field".to_string(),
                    ));
                }
            }
            let target = if is_static {
                Node::new(NodeBase::Identifier(class_name.to_string()), loc)
            } else {
                Node::new(NodeBase::This, loc)
            };
            let lhs = match (is_private, func_name) {
                (true, Some(name)) => Node::new(
                    NodeBase::PrivateMemberInit(Box::new(target), name, true),
                    loc,
                ),
                (false, Some(name)) => Node::new(NodeBase::Member(Box::new(target), name), loc),
                (true, None) => unreachable!(),
                (false, None) => Node::new(
                    NodeBase::Index(Box::new(target), Box::new(property_key)),
                    loc,
                ),
            };
            return Ok(Some(Node::new(
                NodeBase::Assign(Box::new(lhs), Box::new(init)),
                loc,
            )));
        }

        let params = self.read_formal_parameters()?;
        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");
        let prev_allow_await = self.allow_await;
        self.allow_await = is_async;
        let body = self.read_block();
        self.allow_await = prev_allow_await;
        let body = body?;
        Self::validate_unique_formal_parameters(&params, &body, loc)?;
        Self::validate_formal_parameter_context(&params, loc, is_generator, is_async)?;

        if !is_private
            && !is_static
            && func_name.as_deref() == Some("constructor")
            && (is_async || is_generator || accessor.is_some())
        {
            return Err(Error::UnexpectedToken(
                loc,
                "constructor cannot be a special method".to_string(),
            ));
        }
        if !is_private && is_static && func_name.as_deref() == Some("prototype") {
            return Err(Error::UnexpectedToken(
                loc,
                "static class method cannot be named prototype".to_string(),
            ));
        }
        if is_private {
            let private_kind = match accessor.as_deref() {
                Some("get") => PRIVATE_NAME_GET,
                Some("set") => PRIVATE_NAME_SET,
                _ => PRIVATE_NAME_OTHER,
            };
            self.register_private_name(
                func_name.as_deref().expect("private class element name"),
                private_kind,
                loc,
            )?;
        }

        if !is_private
            && func_name.as_deref() == Some("constructor")
            && !is_static
            && !is_async
            && accessor.is_none()
            && !is_generator
        {
            let base = if class_has_heritage {
                NodeBase::DerivedConstructorDecl(class_name.to_string(), params, Box::new(body))
            } else {
                NodeBase::FunctionDecl(class_name.to_string(), params, Box::new(body))
            };
            return Ok(Some(Node::new(base, loc)));
        }

        let func = Node::new(
            if is_async && is_generator {
                NodeBase::AsyncGeneratorFunctionExpr(func_name.clone(), params, Box::new(body))
            } else if is_async {
                NodeBase::AsyncFunctionExpr(func_name.clone(), params, Box::new(body))
            } else if is_generator {
                NodeBase::GeneratorFunctionExpr(func_name.clone(), params, Box::new(body))
            } else {
                NodeBase::FunctionExpr(func_name.clone(), params, Box::new(body))
            },
            loc,
        );
        if is_private {
            let target = if is_static {
                Node::new(NodeBase::Identifier(class_name.to_string()), loc)
            } else {
                Node::new(NodeBase::This, loc)
            };
            let init = match accessor.as_deref() {
                Some("get") => NodeBase::PrivateAccessorInit(
                    Box::new(target),
                    func_name.expect("private class element name"),
                    true,
                ),
                Some("set") => NodeBase::PrivateAccessorInit(
                    Box::new(target),
                    func_name.expect("private class element name"),
                    false,
                ),
                _ => NodeBase::PrivateMemberInit(
                    Box::new(target),
                    func_name.expect("private class element name"),
                    false,
                ),
            };
            return Ok(Some(Node::new(
                NodeBase::Assign(Box::new(Node::new(init, loc)), Box::new(func)),
                loc,
            )));
        }
        let target = if is_static {
            Node::new(NodeBase::Identifier(class_name.to_string()), loc)
        } else {
            Node::new(
                NodeBase::Member(
                    Box::new(Node::new(NodeBase::Identifier(class_name.to_string()), loc)),
                    "prototype".to_string(),
                ),
                loc,
            )
        };
        let descriptor = match accessor.as_deref() {
            Some("get") => Node::new(
                NodeBase::Object(vec![
                    PropertyDefinition::Property("get".to_string(), func),
                    PropertyDefinition::Property(
                        "configurable".to_string(),
                        Node::new(NodeBase::Boolean(true), loc),
                    ),
                ]),
                loc,
            ),
            Some("set") => Node::new(
                NodeBase::Object(vec![
                    PropertyDefinition::Property("set".to_string(), func),
                    PropertyDefinition::Property(
                        "configurable".to_string(),
                        Node::new(NodeBase::Boolean(true), loc),
                    ),
                ]),
                loc,
            ),
            _ => Node::new(
                NodeBase::Object(vec![
                    PropertyDefinition::Property("value".to_string(), func),
                    PropertyDefinition::Property(
                        "writable".to_string(),
                        Node::new(NodeBase::Boolean(true), loc),
                    ),
                    PropertyDefinition::Property(
                        "configurable".to_string(),
                        Node::new(NodeBase::Boolean(true), loc),
                    ),
                ]),
                loc,
            ),
        };
        Ok(Some(Node::new(
            NodeBase::Call(
                Box::new(Node::new(
                    NodeBase::Member(
                        Box::new(Node::new(NodeBase::Identifier("Object".to_string()), loc)),
                        "defineProperty".to_string(),
                    ),
                    loc,
                )),
                vec![target, property_key, descriptor],
            ),
            loc,
        )))
    }

    fn register_private_name(&mut self, name: &str, kind: u8, loc: SourceLoc) -> Result<(), Error> {
        if name == "constructor" {
            return Err(Error::UnexpectedToken(
                loc,
                "invalid private identifier".to_string(),
            ));
        }

        let Some(names) = self.private_name_stack.last_mut() else {
            return Ok(());
        };
        match names.get_mut(name) {
            Some(existing) if *existing == PRIVATE_NAME_GET && kind == PRIVATE_NAME_SET => {
                *existing |= PRIVATE_NAME_SET;
                Ok(())
            }
            Some(existing) if *existing == PRIVATE_NAME_SET && kind == PRIVATE_NAME_GET => {
                *existing |= PRIVATE_NAME_GET;
                Ok(())
            }
            Some(_) => Err(Error::UnexpectedToken(
                loc,
                format!("duplicate private name '{}'", name),
            )),
            None => {
                names.insert(name.to_string(), kind);
                Ok(())
            }
        }
    }

    fn validate_unique_formal_parameters(
        params: &FormalParameters,
        body: &Node,
        loc: SourceLoc,
    ) -> Result<(), Error> {
        Self::validate_formal_parameters(params, body, loc, true)
    }

    fn validate_formal_parameters(
        params: &FormalParameters,
        body: &Node,
        loc: SourceLoc,
        duplicates_always_forbidden: bool,
    ) -> Result<(), Error> {
        Self::validate_formal_parameters_in_context(
            params,
            body,
            loc,
            duplicates_always_forbidden,
            Self::body_contains_use_strict(body),
        )
    }

    fn validate_formal_parameter_context(
        params: &FormalParameters,
        loc: SourceLoc,
        forbid_yield: bool,
        forbid_await: bool,
    ) -> Result<(), Error> {
        if forbid_yield && Self::formal_parameters_contain_yield(params) {
            return Err(Error::UnexpectedToken(
                loc,
                "yield cannot be used in generator parameters".to_string(),
            ));
        }
        if forbid_await && Self::formal_parameters_contain_identifier_reference(params, "await") {
            return Err(Error::UnexpectedToken(
                loc,
                "await cannot be used in async function parameters".to_string(),
            ));
        }
        Ok(())
    }

    fn validate_formal_parameters_in_context(
        params: &FormalParameters,
        body: &Node,
        loc: SourceLoc,
        duplicates_always_forbidden: bool,
        strict: bool,
    ) -> Result<(), Error> {
        let is_simple = Self::formal_parameters_are_simple(params);
        if !is_simple && strict {
            return Err(Error::UnexpectedToken(
                loc,
                "non-simple parameters cannot contain a use strict directive".to_string(),
            ));
        }

        let bound_names = Self::formal_parameter_bound_names(params);
        for name in &bound_names {
            if Self::is_reserved_identifier_name(name) {
                return Err(Error::UnexpectedToken(
                    loc,
                    format!("'{}' is not a valid binding identifier", name),
                ));
            }
        }
        if strict {
            for name in &bound_names {
                if name == "eval" || name == "arguments" {
                    return Err(Error::UnexpectedToken(
                        loc,
                        format!(
                            "'{}' cannot be used as a binding identifier in strict mode",
                            name
                        ),
                    ));
                }
            }
        }

        let lexical_names = Self::function_body_lexically_declared_names(body);
        for name in &bound_names {
            if lexical_names.iter().any(|lex_name| lex_name == name) {
                return Err(Error::UnexpectedToken(
                    loc,
                    format!("Identifier '{}' has already been declared", name),
                ));
            }
        }

        if duplicates_always_forbidden || !is_simple || strict {
            let mut names = HashSet::new();
            for name in bound_names {
                if !names.insert(name) {
                    return Err(Error::UnexpectedToken(
                        loc,
                        "duplicate formal parameter name".to_string(),
                    ));
                }
            }
        }
        Ok(())
    }

    fn function_body_lexically_declared_names(body: &Node) -> Vec<String> {
        match body.base {
            NodeBase::StatementList(ref list) | NodeBase::Block(ref list) => {
                Self::lexically_declared_names(list, FunctionBody::Yes)
            }
            _ => vec![],
        }
    }

    fn formal_parameters_are_simple(params: &FormalParameters) -> bool {
        params
            .iter()
            .all(|param| param.pattern.is_none() && param.init.is_none() && !param.is_rest_param)
    }

    fn validate_strict_function_name(name: &str, body: &Node, loc: SourceLoc) -> Result<(), Error> {
        Self::validate_strict_function_name_in_context(
            name,
            loc,
            Self::body_contains_use_strict(body),
        )
    }

    fn validate_strict_function_name_in_context(
        name: &str,
        loc: SourceLoc,
        strict: bool,
    ) -> Result<(), Error> {
        if strict && (name == "eval" || name == "arguments") {
            return Err(Error::UnexpectedToken(
                loc,
                format!(
                    "'{}' cannot be used as a binding identifier in strict mode",
                    name
                ),
            ));
        }
        Ok(())
    }

    fn formal_parameter_bound_names(params: &FormalParameters) -> Vec<String> {
        let mut names = vec![];
        for param in params {
            if let Some(ref pattern) = param.pattern {
                names.extend(Self::pattern_bound_names(pattern));
            } else {
                names.push(param.name.clone());
            }
        }
        names
    }

    fn pattern_bound_names(pattern: &Node) -> Vec<String> {
        let mut names = vec![];
        match pattern.base {
            NodeBase::Identifier(ref name) => names.push(name.clone()),
            NodeBase::ArrayPattern(ref elements) => {
                for element in elements {
                    match element {
                        ArrayPatternElement::Element(target, _)
                        | ArrayPatternElement::Rest(target) => {
                            names.extend(Self::pattern_bound_names(target));
                        }
                        ArrayPatternElement::Elision => {}
                    }
                }
            }
            NodeBase::ObjectPattern(ref properties) => {
                for property in properties {
                    match property {
                        ObjectPatternProperty::Property(_, target, _)
                        | ObjectPatternProperty::ComputedProperty(_, target, _)
                        | ObjectPatternProperty::Rest(target) => {
                            names.extend(Self::pattern_bound_names(target));
                        }
                    }
                }
            }
            _ => {}
        }
        names
    }

    fn formal_parameters_contain_identifier_reference(
        params: &FormalParameters,
        expected: &str,
    ) -> bool {
        params.iter().any(|param| {
            param.name == expected
                || param
                    .pattern
                    .as_ref()
                    .map(|pattern| Self::contains_identifier_reference(pattern, expected))
                    .unwrap_or(false)
                || param
                    .init
                    .as_ref()
                    .map(|init| Self::contains_identifier_reference(init, expected))
                    .unwrap_or(false)
        })
    }

    fn formal_parameters_contain_yield(params: &FormalParameters) -> bool {
        Self::formal_parameter_bound_names(params)
            .iter()
            .any(|name| name == "yield")
            || params.iter().any(|param| {
                param
                    .init
                    .as_ref()
                    .map(Self::contains_yield_expression)
                    .unwrap_or(false)
            })
    }

    fn contains_yield_expression(node: &Node) -> bool {
        match node.base {
            NodeBase::Yield(_, _) => true,
            NodeBase::StatementList(ref list) | NodeBase::Block(ref list) => {
                list.iter().any(Self::contains_yield_expression)
            }
            NodeBase::FunctionDecl(_, _, _)
            | NodeBase::DerivedConstructorDecl(_, _, _)
            | NodeBase::FunctionExpr(_, _, _)
            | NodeBase::GeneratorFunctionDecl(_, _, _)
            | NodeBase::GeneratorFunctionExpr(_, _, _)
            | NodeBase::AsyncFunctionDecl(_, _, _)
            | NodeBase::AsyncFunctionExpr(_, _, _)
            | NodeBase::AsyncGeneratorFunctionDecl(_, _, _)
            | NodeBase::AsyncGeneratorFunctionExpr(_, _, _)
            | NodeBase::ArrowFunction(_, _)
            | NodeBase::AsyncArrowFunction(_, _) => false,
            NodeBase::If(ref cond, ref then, ref else_) => {
                Self::contains_yield_expression(cond)
                    || Self::contains_yield_expression(then)
                    || Self::contains_yield_expression(else_)
            }
            NodeBase::While(ref cond, ref body) | NodeBase::DoWhile(ref body, ref cond) => {
                Self::contains_yield_expression(cond) || Self::contains_yield_expression(body)
            }
            NodeBase::With(ref object, ref body)
            | NodeBase::ForIn(ref object, _, ref body)
            | NodeBase::ForOf(ref object, _, ref body) => {
                Self::contains_yield_expression(object) || Self::contains_yield_expression(body)
            }
            NodeBase::For(ref init, ref cond, ref step, ref body) => {
                Self::contains_yield_expression(init)
                    || Self::contains_yield_expression(cond)
                    || Self::contains_yield_expression(step)
                    || Self::contains_yield_expression(body)
            }
            NodeBase::New(ref init)
            | NodeBase::UnaryOp(ref init, _)
            | NodeBase::Return(Some(ref init))
            | NodeBase::Throw(ref init)
            | NodeBase::Await(ref init)
            | NodeBase::Spread(ref init)
            | NodeBase::Label(_, ref init)
            | NodeBase::AnonymousClassExpr(ref init) => Self::contains_yield_expression(init),
            NodeBase::Member(ref object, _)
            | NodeBase::PrivateMember(ref object, _)
            | NodeBase::PrivateMemberInit(ref object, _, _)
            | NodeBase::PrivateAccessorInit(ref object, _, _) => {
                Self::contains_yield_expression(object)
            }
            NodeBase::Index(ref left, ref right)
            | NodeBase::Assign(ref left, ref right)
            | NodeBase::AssignOp(ref left, ref right, _)
            | NodeBase::BinaryOp(ref left, ref right, _) => {
                Self::contains_yield_expression(left) || Self::contains_yield_expression(right)
            }
            NodeBase::TernaryOp(ref cond, ref then, ref else_) => {
                Self::contains_yield_expression(cond)
                    || Self::contains_yield_expression(then)
                    || Self::contains_yield_expression(else_)
            }
            NodeBase::Call(ref callee, ref args) => {
                Self::contains_yield_expression(callee)
                    || args.iter().any(Self::contains_yield_expression)
            }
            NodeBase::Array(ref items) => items.iter().any(Self::contains_yield_expression),
            NodeBase::Object(ref properties) => properties.iter().any(|property| match property {
                PropertyDefinition::Property(_, value)
                | PropertyDefinition::CoverInitializedName(_, value)
                | PropertyDefinition::SpreadObject(value)
                | PropertyDefinition::MethodDefinition(_, _, value) => {
                    Self::contains_yield_expression(value)
                }
                PropertyDefinition::ComputedProperty(key, value)
                | PropertyDefinition::ComputedMethodDefinition(_, key, value) => {
                    Self::contains_yield_expression(key) || Self::contains_yield_expression(value)
                }
                PropertyDefinition::IdentifierReference(_) => false,
            }),
            _ => false,
        }
    }

    fn body_contains_use_strict(body: &Node) -> bool {
        let list = match body.base {
            NodeBase::StatementList(ref list) | NodeBase::Block(ref list) => list,
            _ => return false,
        };
        for node in list {
            match node.base {
                NodeBase::String(ref string) if string == "use strict" => return true,
                NodeBase::String(_) => {}
                _ => return false,
            }
        }
        false
    }

    fn validate_early_errors(
        node: &Node,
        function_body: FunctionBody,
        strict_context: bool,
    ) -> Result<(), Error> {
        Self::validate_early_errors_with_yield(node, function_body, strict_context, false)
    }

    fn validate_early_errors_with_yield(
        node: &Node,
        function_body: FunctionBody,
        strict_context: bool,
        allow_yield: bool,
    ) -> Result<(), Error> {
        match node.base {
            NodeBase::StatementList(ref list) => {
                let strict_context = strict_context || Self::body_contains_use_strict(node);
                Self::validate_statement_list(node.loc, list, function_body)?;
                let statement_context = function_body.statement_context();
                for item in list {
                    Self::validate_early_errors_with_yield(
                        item,
                        statement_context,
                        strict_context,
                        allow_yield,
                    )?;
                }
            }
            NodeBase::Block(ref list) => {
                let strict_context = strict_context || Self::body_contains_use_strict(node);
                Self::validate_statement_list(node.loc, list, function_body)?;
                let statement_context = function_body.statement_context();
                for item in list {
                    Self::validate_early_errors_with_yield(
                        item,
                        statement_context,
                        strict_context,
                        allow_yield,
                    )?;
                }
            }
            NodeBase::DerivedConstructorDecl(_, _, ref body) => {
                Self::validate_function_early_errors(node, strict_context)?;
                let strict_context = strict_context || Self::body_contains_use_strict(body);
                Self::validate_early_errors_with_yield(
                    body,
                    FunctionBody::DerivedConstructor,
                    strict_context,
                    false,
                )?;
            }
            NodeBase::FunctionDecl(_, _, ref body)
            | NodeBase::FunctionExpr(_, _, ref body)
            | NodeBase::AsyncFunctionDecl(_, _, ref body)
            | NodeBase::AsyncFunctionExpr(_, _, ref body)
            | NodeBase::ArrowFunction(_, ref body)
            | NodeBase::AsyncArrowFunction(_, ref body) => {
                Self::validate_function_early_errors(node, strict_context)?;
                let strict_context = strict_context || Self::body_contains_use_strict(body);
                Self::validate_early_errors_with_yield(
                    body,
                    FunctionBody::Yes,
                    strict_context,
                    false,
                )?;
            }
            NodeBase::GeneratorFunctionDecl(_, _, ref body)
            | NodeBase::AsyncGeneratorFunctionDecl(_, _, ref body)
            | NodeBase::AsyncGeneratorFunctionExpr(_, _, ref body)
            | NodeBase::GeneratorFunctionExpr(_, _, ref body) => {
                Self::validate_function_early_errors(node, strict_context)?;
                let strict_context = strict_context || Self::body_contains_use_strict(body);
                Self::validate_early_errors_with_yield(
                    body,
                    FunctionBody::Yes,
                    strict_context,
                    true,
                )?;
            }
            NodeBase::If(_, ref then, ref else_) => {
                Self::validate_early_errors_with_yield(
                    then,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
                Self::validate_early_errors_with_yield(
                    else_,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::While(_, ref body)
            | NodeBase::DoWhile(ref body, _)
            | NodeBase::With(_, ref body) => {
                Self::validate_early_errors_with_yield(
                    body,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::Label(ref name, ref body) => {
                if allow_yield && name == "yield" {
                    return Err(Error::UnexpectedToken(
                        node.loc,
                        "yield cannot be used as a label in a generator".to_string(),
                    ));
                }
                Self::validate_early_errors_with_yield(
                    body,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::For(ref init, ref cond, ref step, ref body) => {
                Self::validate_early_errors_with_yield(
                    init,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
                Self::validate_early_errors_with_yield(
                    cond,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
                Self::validate_early_errors_with_yield(
                    step,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
                Self::validate_early_errors_with_yield(
                    body,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::ForIn(ref init, ref value, ref body)
            | NodeBase::ForOf(ref init, ref value, ref body) => {
                Self::validate_for_iteration_head(init, strict_context, allow_yield)?;
                Self::validate_early_errors_with_yield(
                    value,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
                Self::validate_early_errors_with_yield(
                    body,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::Try(ref try_, ref catch, _, ref finally) => {
                Self::validate_early_errors_with_yield(
                    try_,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
                Self::validate_early_errors_with_yield(
                    catch,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
                Self::validate_early_errors_with_yield(
                    finally,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::Switch(_, ref cases) => {
                Self::validate_early_errors_with_yield(
                    cases,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::AnonymousClassExpr(ref body) => {
                Self::validate_early_errors_with_yield(
                    body,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::VarDecl(ref name, ref init, _) => {
                Self::validate_binding_identifier(name, strict_context, allow_yield, node.loc)?;
                if let Some(init) = init {
                    Self::validate_early_errors_with_yield(
                        init,
                        FunctionBody::No,
                        strict_context,
                        allow_yield,
                    )?;
                }
            }
            NodeBase::VarDeclPattern(ref pattern, ref init, _) => {
                Self::validate_binding_pattern(pattern, strict_context, allow_yield)?;
                if let Some(init) = init {
                    Self::validate_early_errors_with_yield(
                        init,
                        FunctionBody::No,
                        strict_context,
                        allow_yield,
                    )?;
                }
            }
            NodeBase::New(ref init)
            | NodeBase::UnaryOp(ref init, _)
            | NodeBase::Return(Some(ref init))
            | NodeBase::Throw(ref init)
            | NodeBase::Await(ref init)
            | NodeBase::Spread(ref init) => {
                Self::validate_early_errors_with_yield(
                    init,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::Yield(ref init, _) => {
                if strict_context && !allow_yield {
                    return Err(Error::UnexpectedToken(
                        node.loc,
                        "yield expression is not allowed here".to_string(),
                    ));
                }
                if let Some(init) = init {
                    Self::validate_early_errors_with_yield(
                        init,
                        FunctionBody::No,
                        strict_context,
                        allow_yield,
                    )?;
                }
            }
            NodeBase::Member(ref object, _)
            | NodeBase::PrivateMember(ref object, _)
            | NodeBase::PrivateMemberInit(ref object, _, _)
            | NodeBase::PrivateAccessorInit(ref object, _, _) => {
                if !matches!(object.base, NodeBase::Identifier(ref name) if name == "super") {
                    Self::validate_early_errors_with_yield(
                        object,
                        FunctionBody::No,
                        strict_context,
                        allow_yield,
                    )?;
                }
            }
            NodeBase::Assign(ref left, ref right) => {
                Self::validate_assignment_target(left, strict_context, allow_yield)?;
                Self::validate_early_errors_with_yield(
                    right,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::Index(ref left, ref right) => {
                if !matches!(left.base, NodeBase::Identifier(ref name) if name == "super") {
                    Self::validate_early_errors_with_yield(
                        left,
                        FunctionBody::No,
                        strict_context,
                        allow_yield,
                    )?;
                }
                Self::validate_early_errors_with_yield(
                    right,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::AssignOp(ref left, ref right, _) => {
                Self::validate_assignment_target(left, strict_context, allow_yield)?;
                Self::validate_early_errors_with_yield(
                    right,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::BinaryOp(ref left, ref right, _) => {
                Self::validate_early_errors_with_yield(
                    left,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
                Self::validate_early_errors_with_yield(
                    right,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::Call(ref callee, ref args) => {
                let callee_is_super =
                    matches!(callee.base, NodeBase::Identifier(ref name) if name == "super");
                if callee_is_super {
                    if function_body != FunctionBody::DerivedConstructor {
                        return Err(Error::UnexpectedToken(
                            node.loc,
                            "super call is not allowed here".to_string(),
                        ));
                    }
                } else if !matches!(callee.base, NodeBase::Identifier(ref name) if name == "import")
                {
                    Self::validate_early_errors_with_yield(
                        callee,
                        FunctionBody::No,
                        strict_context,
                        allow_yield,
                    )?;
                }
                for arg in args {
                    Self::validate_early_errors_with_yield(
                        arg,
                        FunctionBody::No,
                        strict_context,
                        allow_yield,
                    )?;
                }
            }
            NodeBase::Array(ref items) => {
                for item in items {
                    Self::validate_early_errors_with_yield(
                        item,
                        FunctionBody::No,
                        strict_context,
                        allow_yield,
                    )?;
                }
            }
            NodeBase::Object(ref properties) => {
                for property in properties {
                    Self::validate_property_early_errors(property, strict_context, allow_yield)?;
                }
            }
            NodeBase::TernaryOp(ref cond, ref then, ref else_) => {
                Self::validate_early_errors_with_yield(
                    cond,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
                Self::validate_early_errors_with_yield(
                    then,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
                Self::validate_early_errors_with_yield(
                    else_,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::Identifier(ref name) => {
                if Self::is_reserved_identifier_name(name) || (allow_yield && name == "yield") {
                    return Err(Error::UnexpectedToken(
                        node.loc,
                        format!("'{}' cannot be used as an identifier", name),
                    ));
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn validate_assignment_target(
        target: &Node,
        strict_context: bool,
        allow_yield: bool,
    ) -> Result<(), Error> {
        match target.base {
            NodeBase::Identifier(ref name) => {
                if (strict_context && (name == "eval" || name == "arguments"))
                    || Self::is_reserved_identifier_name(name)
                    || Self::is_reserved_assignment_identifier(name, strict_context)
                    || (allow_yield && name == "yield")
                {
                    return Err(Error::UnexpectedToken(
                        target.loc,
                        format!("'{}' is not a valid assignment target", name),
                    ));
                }
            }
            NodeBase::Member(ref object, _)
            | NodeBase::PrivateMember(ref object, _)
            | NodeBase::PrivateMemberInit(ref object, _, _)
            | NodeBase::PrivateAccessorInit(ref object, _, _) => {
                if !matches!(object.base, NodeBase::Identifier(ref name) if name == "super") {
                    Self::validate_early_errors_with_yield(
                        object,
                        FunctionBody::No,
                        strict_context,
                        allow_yield,
                    )?;
                }
            }
            NodeBase::Index(ref object, ref index) => {
                if !matches!(object.base, NodeBase::Identifier(ref name) if name == "super") {
                    Self::validate_early_errors_with_yield(
                        object,
                        FunctionBody::No,
                        strict_context,
                        allow_yield,
                    )?;
                }
                Self::validate_early_errors_with_yield(
                    index,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            NodeBase::ArrayPattern(ref elements) => {
                for (index, element) in elements.iter().enumerate() {
                    match element {
                        ArrayPatternElement::Elision => {}
                        ArrayPatternElement::Element(target, init) => {
                            Self::validate_assignment_target(target, strict_context, allow_yield)?;
                            if let Some(init) = init {
                                Self::validate_early_errors_with_yield(
                                    init,
                                    FunctionBody::No,
                                    strict_context,
                                    allow_yield,
                                )?;
                            }
                        }
                        ArrayPatternElement::Rest(target) => {
                            if index + 1 != elements.len() {
                                return Err(Error::UnexpectedToken(
                                    target.loc,
                                    "rest element must be last".to_string(),
                                ));
                            }
                            Self::validate_assignment_target(target, strict_context, allow_yield)?;
                        }
                    }
                }
            }
            NodeBase::ObjectPattern(ref properties) => {
                for (index, property) in properties.iter().enumerate() {
                    match property {
                        ObjectPatternProperty::Property(_, target, init)
                        | ObjectPatternProperty::ComputedProperty(_, target, init) => {
                            Self::validate_assignment_target(target, strict_context, allow_yield)?;
                            if let Some(init) = init {
                                Self::validate_early_errors_with_yield(
                                    init,
                                    FunctionBody::No,
                                    strict_context,
                                    allow_yield,
                                )?;
                            }
                        }
                        ObjectPatternProperty::Rest(target) => {
                            if index + 1 != properties.len() {
                                return Err(Error::UnexpectedToken(
                                    target.loc,
                                    "rest property must be last".to_string(),
                                ));
                            }
                            Self::validate_assignment_target(target, strict_context, allow_yield)?;
                        }
                    }
                }
            }
            _ => {
                return Err(Error::UnexpectedToken(
                    target.loc,
                    "invalid assignment target".to_string(),
                ));
            }
        }
        Ok(())
    }

    fn validate_for_iteration_head(
        target: &Node,
        strict_context: bool,
        allow_yield: bool,
    ) -> Result<(), Error> {
        match target.base {
            NodeBase::VarDecl(_, _, _) | NodeBase::VarDeclPattern(_, _, _) => {
                Self::validate_early_errors_with_yield(
                    target,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )
            }
            _ => Self::validate_assignment_target(target, strict_context, allow_yield),
        }
    }

    fn validate_binding_pattern(
        pattern: &Node,
        strict_context: bool,
        allow_yield: bool,
    ) -> Result<(), Error> {
        match pattern.base {
            NodeBase::Identifier(ref name) => {
                Self::validate_binding_identifier(name, strict_context, allow_yield, pattern.loc)?;
            }
            NodeBase::ArrayPattern(ref elements) => {
                for element in elements {
                    match element {
                        ArrayPatternElement::Element(target, init) => {
                            Self::validate_binding_pattern(target, strict_context, allow_yield)?;
                            if let Some(init) = init {
                                Self::validate_early_errors_with_yield(
                                    init,
                                    FunctionBody::No,
                                    strict_context,
                                    allow_yield,
                                )?;
                            }
                        }
                        ArrayPatternElement::Rest(target) => {
                            Self::validate_binding_pattern(target, strict_context, allow_yield)?;
                        }
                        ArrayPatternElement::Elision => {}
                    }
                }
            }
            NodeBase::ObjectPattern(ref properties) => {
                for property in properties {
                    match property {
                        ObjectPatternProperty::Property(_, target, init)
                        | ObjectPatternProperty::ComputedProperty(_, target, init) => {
                            Self::validate_binding_pattern(target, strict_context, allow_yield)?;
                            if let Some(init) = init {
                                Self::validate_early_errors_with_yield(
                                    init,
                                    FunctionBody::No,
                                    strict_context,
                                    allow_yield,
                                )?;
                            }
                        }
                        ObjectPatternProperty::Rest(target) => {
                            Self::validate_binding_pattern(target, strict_context, allow_yield)?;
                        }
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn validate_binding_identifier(
        name: &str,
        strict_context: bool,
        allow_yield: bool,
        loc: SourceLoc,
    ) -> Result<(), Error> {
        if (strict_context && (name == "eval" || name == "arguments"))
            || Self::is_reserved_identifier_name(name)
            || Self::is_reserved_assignment_identifier(name, strict_context)
            || (allow_yield && name == "yield")
        {
            return Err(Error::UnexpectedToken(
                loc,
                format!("'{}' is not a valid binding identifier", name),
            ));
        }
        Ok(())
    }

    fn is_reserved_assignment_identifier(name: &str, strict_context: bool) -> bool {
        matches!(
            name,
            "class" | "const" | "enum" | "export" | "extends" | "import" | "super"
        ) || (strict_context
            && matches!(
                name,
                "implements"
                    | "interface"
                    | "let"
                    | "package"
                    | "private"
                    | "protected"
                    | "public"
                    | "static"
                    | "yield"
            ))
    }

    fn is_reserved_identifier_name(name: &str) -> bool {
        matches!(
            name,
            "abstract"
                | "break"
                | "case"
                | "catch"
                | "class"
                | "const"
                | "continue"
                | "debugger"
                | "default"
                | "delete"
                | "do"
                | "else"
                | "enum"
                | "export"
                | "extends"
                | "false"
                | "finally"
                | "for"
                | "function"
                | "if"
                | "import"
                | "in"
                | "instanceof"
                | "let"
                | "new"
                | "null"
                | "return"
                | "super"
                | "switch"
                | "this"
                | "throw"
                | "true"
                | "try"
                | "typeof"
                | "var"
                | "void"
                | "while"
                | "with"
        )
    }

    fn validate_property_early_errors(
        property: &PropertyDefinition,
        strict_context: bool,
        allow_yield: bool,
    ) -> Result<(), Error> {
        match property {
            PropertyDefinition::Property(_, value)
            | PropertyDefinition::CoverInitializedName(_, value)
            | PropertyDefinition::SpreadObject(value) => {
                Self::validate_early_errors_with_yield(
                    value,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            PropertyDefinition::ComputedProperty(key, value) => {
                Self::validate_early_errors_with_yield(
                    key,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
                Self::validate_early_errors_with_yield(
                    value,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            PropertyDefinition::MethodDefinition(_, _, func) => {
                Self::validate_early_errors_with_yield(
                    func,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            PropertyDefinition::ComputedMethodDefinition(_, key, func) => {
                Self::validate_early_errors_with_yield(
                    key,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
                Self::validate_early_errors_with_yield(
                    func,
                    FunctionBody::No,
                    strict_context,
                    allow_yield,
                )?;
            }
            PropertyDefinition::IdentifierReference(name) => {
                if allow_yield && name == "yield" {
                    return Err(Error::UnexpectedToken(
                        SourceLoc::default(),
                        "yield cannot be used as an identifier in a generator".to_string(),
                    ));
                }
            }
        }
        Ok(())
    }

    fn validate_function_early_errors(node: &Node, strict_context: bool) -> Result<(), Error> {
        let (name, params, body, duplicates_always_forbidden, allow_super_call) = match node.base {
            NodeBase::FunctionDecl(ref name, ref params, ref body)
            | NodeBase::FunctionExpr(Some(ref name), ref params, ref body) => {
                (Some(name.as_str()), params, body, false, false)
            }
            NodeBase::DerivedConstructorDecl(ref name, ref params, ref body) => {
                (Some(name.as_str()), params, body, false, true)
            }
            NodeBase::FunctionExpr(None, ref params, ref body) => {
                (None, params, body, false, false)
            }
            NodeBase::GeneratorFunctionDecl(ref name, ref params, ref body)
            | NodeBase::GeneratorFunctionExpr(Some(ref name), ref params, ref body)
            | NodeBase::AsyncFunctionDecl(ref name, ref params, ref body)
            | NodeBase::AsyncFunctionExpr(Some(ref name), ref params, ref body)
            | NodeBase::AsyncGeneratorFunctionDecl(ref name, ref params, ref body)
            | NodeBase::AsyncGeneratorFunctionExpr(Some(ref name), ref params, ref body) => {
                (Some(name.as_str()), params, body, true, false)
            }
            NodeBase::GeneratorFunctionExpr(None, ref params, ref body)
            | NodeBase::AsyncFunctionExpr(None, ref params, ref body)
            | NodeBase::AsyncGeneratorFunctionExpr(None, ref params, ref body) => {
                (None, params, body, true, false)
            }
            NodeBase::ArrowFunction(ref params, ref body)
            | NodeBase::AsyncArrowFunction(ref params, ref body) => {
                (None, params, body, true, false)
            }
            _ => return Ok(()),
        };
        let strict = strict_context || Self::body_contains_use_strict(body);
        Self::validate_formal_parameters_in_context(
            params,
            body,
            node.loc,
            duplicates_always_forbidden,
            strict,
        )?;
        if Self::formal_parameters_contain_super_call(params)
            || (!allow_super_call && Self::contains_super_call(body))
        {
            return Err(Error::UnexpectedToken(
                node.loc,
                "super call is not allowed here".to_string(),
            ));
        }
        if let Some(name) = name {
            Self::validate_strict_function_name_in_context(name, node.loc, strict)?;
        }
        Ok(())
    }

    fn formal_parameters_contain_super_call(params: &FormalParameters) -> bool {
        params.iter().any(|param| {
            param
                .init
                .as_ref()
                .map(Self::contains_super_call)
                .unwrap_or(false)
        })
    }

    fn contains_super_call(node: &Node) -> bool {
        match node.base {
            NodeBase::SuperCall(_) | NodeBase::SuperCallFromArguments => true,
            NodeBase::Call(ref callee, ref args) => {
                if matches!(callee.base, NodeBase::Identifier(ref name) if name == "super") {
                    return true;
                }
                Self::contains_super_call(callee) || args.iter().any(Self::contains_super_call)
            }
            NodeBase::StatementList(ref list) | NodeBase::Block(ref list) => {
                list.iter().any(Self::contains_super_call)
            }
            NodeBase::If(ref cond, ref then, ref else_) => {
                Self::contains_super_call(cond)
                    || Self::contains_super_call(then)
                    || Self::contains_super_call(else_)
            }
            NodeBase::While(ref cond, ref body) | NodeBase::DoWhile(ref body, ref cond) => {
                Self::contains_super_call(cond) || Self::contains_super_call(body)
            }
            NodeBase::With(ref object, ref body)
            | NodeBase::ForIn(ref object, _, ref body)
            | NodeBase::ForOf(ref object, _, ref body) => {
                Self::contains_super_call(object) || Self::contains_super_call(body)
            }
            NodeBase::For(ref init, ref cond, ref step, ref body) => {
                Self::contains_super_call(init)
                    || Self::contains_super_call(cond)
                    || Self::contains_super_call(step)
                    || Self::contains_super_call(body)
            }
            NodeBase::Try(ref try_, ref catch, ref param, ref finally) => {
                Self::contains_super_call(try_)
                    || Self::contains_super_call(catch)
                    || Self::contains_super_call(param)
                    || Self::contains_super_call(finally)
            }
            NodeBase::Switch(ref value, ref cases) => {
                Self::contains_super_call(value) || Self::contains_super_call(cases)
            }
            NodeBase::VarDecl(_, Some(ref init), _)
            | NodeBase::VarDeclPattern(_, Some(ref init), _)
            | NodeBase::ArrowFunction(_, ref init)
            | NodeBase::AsyncArrowFunction(_, ref init)
            | NodeBase::New(ref init)
            | NodeBase::UnaryOp(ref init, _)
            | NodeBase::Return(Some(ref init))
            | NodeBase::Throw(ref init)
            | NodeBase::Yield(Some(ref init), _)
            | NodeBase::Await(ref init)
            | NodeBase::Spread(ref init)
            | NodeBase::Label(_, ref init)
            | NodeBase::AnonymousClassExpr(ref init) => Self::contains_super_call(init),
            NodeBase::Member(ref object, _)
            | NodeBase::PrivateMember(ref object, _)
            | NodeBase::PrivateMemberInit(ref object, _, _)
            | NodeBase::PrivateAccessorInit(ref object, _, _) => Self::contains_super_call(object),
            NodeBase::Index(ref left, ref right)
            | NodeBase::Assign(ref left, ref right)
            | NodeBase::AssignOp(ref left, ref right, _)
            | NodeBase::BinaryOp(ref left, ref right, _) => {
                Self::contains_super_call(left) || Self::contains_super_call(right)
            }
            NodeBase::TernaryOp(ref cond, ref then, ref else_) => {
                Self::contains_super_call(cond)
                    || Self::contains_super_call(then)
                    || Self::contains_super_call(else_)
            }
            NodeBase::Array(ref items) => items.iter().any(Self::contains_super_call),
            NodeBase::Object(ref properties) => properties.iter().any(|property| match property {
                PropertyDefinition::Property(_, value)
                | PropertyDefinition::CoverInitializedName(_, value)
                | PropertyDefinition::SpreadObject(value) => Self::contains_super_call(value),
                PropertyDefinition::ComputedProperty(key, value)
                | PropertyDefinition::ComputedMethodDefinition(_, key, value) => {
                    Self::contains_super_call(key) || Self::contains_super_call(value)
                }
                PropertyDefinition::MethodDefinition(_, _, value) => {
                    Self::contains_super_call(value)
                }
                PropertyDefinition::IdentifierReference(_) => false,
            }),
            _ => false,
        }
    }

    fn contains_identifier_reference(node: &Node, expected: &str) -> bool {
        match node.base {
            NodeBase::Identifier(ref name) => name == expected,
            NodeBase::StatementList(ref list) | NodeBase::Block(ref list) => list
                .iter()
                .any(|item| Self::contains_identifier_reference(item, expected)),
            NodeBase::FunctionDecl(_, ref params, ref body)
            | NodeBase::DerivedConstructorDecl(_, ref params, ref body)
            | NodeBase::FunctionExpr(_, ref params, ref body)
            | NodeBase::GeneratorFunctionDecl(_, ref params, ref body)
            | NodeBase::GeneratorFunctionExpr(_, ref params, ref body)
            | NodeBase::AsyncFunctionDecl(_, ref params, ref body)
            | NodeBase::AsyncFunctionExpr(_, ref params, ref body)
            | NodeBase::AsyncGeneratorFunctionDecl(_, ref params, ref body)
            | NodeBase::AsyncGeneratorFunctionExpr(_, ref params, ref body)
            | NodeBase::ArrowFunction(ref params, ref body)
            | NodeBase::AsyncArrowFunction(ref params, ref body) => {
                params.iter().any(|param| {
                    param
                        .pattern
                        .as_ref()
                        .map(|pattern| Self::contains_identifier_reference(pattern, expected))
                        .unwrap_or(false)
                        || param
                            .init
                            .as_ref()
                            .map(|init| Self::contains_identifier_reference(init, expected))
                            .unwrap_or(false)
                }) || Self::contains_identifier_reference(body, expected)
            }
            NodeBase::If(ref cond, ref then, ref else_) => {
                Self::contains_identifier_reference(cond, expected)
                    || Self::contains_identifier_reference(then, expected)
                    || Self::contains_identifier_reference(else_, expected)
            }
            NodeBase::While(ref cond, ref body) | NodeBase::DoWhile(ref body, ref cond) => {
                Self::contains_identifier_reference(cond, expected)
                    || Self::contains_identifier_reference(body, expected)
            }
            NodeBase::With(ref object, ref body)
            | NodeBase::ForIn(ref object, _, ref body)
            | NodeBase::ForOf(ref object, _, ref body) => {
                Self::contains_identifier_reference(object, expected)
                    || Self::contains_identifier_reference(body, expected)
            }
            NodeBase::For(ref init, ref cond, ref step, ref body) => {
                Self::contains_identifier_reference(init, expected)
                    || Self::contains_identifier_reference(cond, expected)
                    || Self::contains_identifier_reference(step, expected)
                    || Self::contains_identifier_reference(body, expected)
            }
            NodeBase::Try(ref try_, ref catch, ref param, ref finally) => {
                Self::contains_identifier_reference(try_, expected)
                    || Self::contains_identifier_reference(catch, expected)
                    || Self::contains_identifier_reference(param, expected)
                    || Self::contains_identifier_reference(finally, expected)
            }
            NodeBase::Switch(ref value, ref cases) => {
                Self::contains_identifier_reference(value, expected)
                    || Self::contains_identifier_reference(cases, expected)
            }
            NodeBase::VarDecl(_, Some(ref init), _)
            | NodeBase::VarDeclPattern(_, Some(ref init), _)
            | NodeBase::New(ref init)
            | NodeBase::UnaryOp(ref init, _)
            | NodeBase::Return(Some(ref init))
            | NodeBase::Throw(ref init)
            | NodeBase::Yield(Some(ref init), _)
            | NodeBase::Await(ref init)
            | NodeBase::Spread(ref init)
            | NodeBase::Label(_, ref init)
            | NodeBase::AnonymousClassExpr(ref init) => {
                Self::contains_identifier_reference(init, expected)
            }
            NodeBase::Member(ref object, _)
            | NodeBase::PrivateMember(ref object, _)
            | NodeBase::PrivateMemberInit(ref object, _, _)
            | NodeBase::PrivateAccessorInit(ref object, _, _) => {
                Self::contains_identifier_reference(object, expected)
            }
            NodeBase::Index(ref left, ref right)
            | NodeBase::Assign(ref left, ref right)
            | NodeBase::AssignOp(ref left, ref right, _)
            | NodeBase::BinaryOp(ref left, ref right, _) => {
                Self::contains_identifier_reference(left, expected)
                    || Self::contains_identifier_reference(right, expected)
            }
            NodeBase::TernaryOp(ref cond, ref then, ref else_) => {
                Self::contains_identifier_reference(cond, expected)
                    || Self::contains_identifier_reference(then, expected)
                    || Self::contains_identifier_reference(else_, expected)
            }
            NodeBase::Call(ref callee, ref args) => {
                Self::contains_identifier_reference(callee, expected)
                    || args
                        .iter()
                        .any(|arg| Self::contains_identifier_reference(arg, expected))
            }
            NodeBase::Array(ref items) => items
                .iter()
                .any(|item| Self::contains_identifier_reference(item, expected)),
            NodeBase::Object(ref properties) => properties.iter().any(|property| match property {
                PropertyDefinition::IdentifierReference(name) => name == expected,
                PropertyDefinition::Property(_, value)
                | PropertyDefinition::CoverInitializedName(_, value)
                | PropertyDefinition::SpreadObject(value)
                | PropertyDefinition::MethodDefinition(_, _, value) => {
                    Self::contains_identifier_reference(value, expected)
                }
                PropertyDefinition::ComputedProperty(key, value)
                | PropertyDefinition::ComputedMethodDefinition(_, key, value) => {
                    Self::contains_identifier_reference(key, expected)
                        || Self::contains_identifier_reference(value, expected)
                }
            }),
            NodeBase::ArrayPattern(ref elements) => elements.iter().any(|element| match element {
                ArrayPatternElement::Element(target, init) => {
                    Self::contains_identifier_reference(target, expected)
                        || init
                            .as_ref()
                            .map(|init| Self::contains_identifier_reference(init, expected))
                            .unwrap_or(false)
                }
                ArrayPatternElement::Rest(target) => {
                    Self::contains_identifier_reference(target, expected)
                }
                ArrayPatternElement::Elision => false,
            }),
            NodeBase::ObjectPattern(ref properties) => {
                properties.iter().any(|property| match property {
                    ObjectPatternProperty::Property(_, target, init)
                    | ObjectPatternProperty::ComputedProperty(_, target, init) => {
                        Self::contains_identifier_reference(target, expected)
                            || init
                                .as_ref()
                                .map(|init| Self::contains_identifier_reference(init, expected))
                                .unwrap_or(false)
                    }
                    ObjectPatternProperty::Rest(target) => {
                        Self::contains_identifier_reference(target, expected)
                    }
                })
            }
            _ => false,
        }
    }

    fn validate_statement_list(
        loc: SourceLoc,
        list: &[Node],
        function_body: FunctionBody,
    ) -> Result<(), Error> {
        let lexical_names = Self::lexically_declared_names(list, function_body);
        let var_names = Self::var_declared_names(list, function_body);
        Self::ensure_no_duplicates(loc, &lexical_names)?;
        for name in lexical_names {
            if var_names.iter().any(|var_name| var_name == &name) {
                return Err(Error::UnexpectedToken(
                    loc,
                    format!("Identifier '{}' has already been declared", name),
                ));
            }
        }
        Ok(())
    }

    fn ensure_no_duplicates(loc: SourceLoc, names: &[String]) -> Result<(), Error> {
        let mut seen: Vec<&String> = Vec::new();
        for name in names {
            if seen.iter().any(|seen_name| *seen_name == name) {
                return Err(Error::UnexpectedToken(
                    loc,
                    format!("Identifier '{}' has already been declared", name),
                ));
            }
            seen.push(name);
        }
        Ok(())
    }

    fn lexically_declared_names(list: &[Node], function_body: FunctionBody) -> Vec<String> {
        let mut names = Vec::new();
        for item in list {
            Self::direct_lexically_declared_names(item, function_body, &mut names);
        }
        names
    }

    fn direct_lexically_declared_names(
        node: &Node,
        function_body: FunctionBody,
        names: &mut Vec<String>,
    ) {
        match node.base {
            NodeBase::VarDecl(ref name, _, VarKind::Let | VarKind::Const) => {
                names.push(name.clone());
            }
            NodeBase::VarDeclPattern(ref pattern, _, VarKind::Let | VarKind::Const) => {
                names.extend(Self::pattern_bound_names(pattern));
            }
            NodeBase::StatementList(ref list) => {
                if let Some(name) = Self::class_declaration_name(list) {
                    names.push(name);
                } else {
                    for item in list {
                        Self::direct_lexically_declared_names(item, function_body, names);
                    }
                }
            }
            NodeBase::FunctionDecl(ref name, _, _)
            | NodeBase::DerivedConstructorDecl(ref name, _, _)
            | NodeBase::GeneratorFunctionDecl(ref name, _, _)
            | NodeBase::AsyncFunctionDecl(ref name, _, _)
            | NodeBase::AsyncGeneratorFunctionDecl(ref name, _, _)
                if function_body == FunctionBody::No =>
            {
                names.push(name.clone());
            }
            _ => {}
        }
    }

    fn class_declaration_name(list: &[Node]) -> Option<String> {
        let Some(first) = list.first() else {
            return None;
        };
        match first.base {
            NodeBase::FunctionDecl(ref name, _, _)
            | NodeBase::DerivedConstructorDecl(ref name, _, _) => Some(name.clone()),
            _ => None,
        }
    }

    fn var_declared_names(list: &[Node], function_body: FunctionBody) -> Vec<String> {
        let mut names = Vec::new();
        for item in list {
            Self::collect_var_declared_names(item, function_body, &mut names);
        }
        names
    }

    fn collect_var_declared_names(
        node: &Node,
        function_body: FunctionBody,
        names: &mut Vec<String>,
    ) {
        match node.base {
            NodeBase::VarDecl(ref name, _, VarKind::Var) => names.push(name.clone()),
            NodeBase::VarDeclPattern(ref pattern, _, VarKind::Var) => {
                names.extend(Self::pattern_bound_names(pattern));
            }
            NodeBase::FunctionDecl(ref name, _, _)
            | NodeBase::DerivedConstructorDecl(ref name, _, _)
            | NodeBase::GeneratorFunctionDecl(ref name, _, _)
            | NodeBase::AsyncFunctionDecl(ref name, _, _)
            | NodeBase::AsyncGeneratorFunctionDecl(ref name, _, _)
                if function_body == FunctionBody::Yes =>
            {
                names.push(name.clone());
            }
            NodeBase::StatementList(ref list) => {
                if Self::class_declaration_name(list).is_none() {
                    for item in list {
                        Self::collect_var_declared_names(item, function_body, names);
                    }
                }
            }
            NodeBase::Block(ref list) => {
                for item in list {
                    Self::collect_var_declared_names(item, FunctionBody::No, names);
                }
            }
            NodeBase::If(_, ref then, ref else_) => {
                Self::collect_var_declared_names(then, FunctionBody::No, names);
                Self::collect_var_declared_names(else_, FunctionBody::No, names);
            }
            NodeBase::While(_, ref body)
            | NodeBase::DoWhile(ref body, _)
            | NodeBase::With(_, ref body)
            | NodeBase::Label(_, ref body) => {
                Self::collect_var_declared_names(body, FunctionBody::No, names);
            }
            NodeBase::For(ref init, _, _, ref body)
            | NodeBase::ForIn(ref init, _, ref body)
            | NodeBase::ForOf(ref init, _, ref body) => {
                Self::collect_var_declared_names(init, FunctionBody::No, names);
                Self::collect_var_declared_names(body, FunctionBody::No, names);
            }
            NodeBase::Try(ref try_, ref catch, _, ref finally) => {
                Self::collect_var_declared_names(try_, FunctionBody::No, names);
                Self::collect_var_declared_names(catch, FunctionBody::No, names);
                Self::collect_var_declared_names(finally, FunctionBody::No, names);
            }
            NodeBase::Switch(_, ref cases) => {
                Self::collect_var_declared_names(cases, FunctionBody::No, names);
            }
            _ => {}
        }
    }

    fn read_private_identifier_name(&mut self, loc: SourceLoc) -> Result<String, Error> {
        let tok = self.lexer.next_skip_lineterminator()?;
        if tok.loc.line != loc.line || tok.loc.pos != loc.pos + 1 {
            return Err(Error::UnexpectedToken(
                loc,
                "invalid private identifier".to_string(),
            ));
        }
        match tok.kind {
            Kind::Identifier(name) => Ok(name),
            Kind::Keyword(keyword) => Ok(keyword.to_str().to_string()),
            _ => Err(Error::UnexpectedToken(
                loc,
                "invalid private identifier".to_string(),
            )),
        }
    }

    fn read_class_element_name(&mut self) -> Result<Option<ClassElementName>, Error> {
        let tok = self.lexer.next_skip_lineterminator()?;
        match tok.kind {
            Kind::Identifier(name) => Ok(Some(ClassElementName {
                func_name: Some(name.clone()),
                property_key: Node::new(NodeBase::String(name), tok.loc),
                is_private: false,
            })),
            Kind::Keyword(keyword) => {
                let name = keyword.to_str().to_string();
                Ok(Some(ClassElementName {
                    func_name: Some(name.clone()),
                    property_key: Node::new(NodeBase::String(name), tok.loc),
                    is_private: false,
                }))
            }
            Kind::String(name) => Ok(Some(ClassElementName {
                func_name: Some(name.clone()),
                property_key: Node::new(NodeBase::String(name), tok.loc),
                is_private: false,
            })),
            Kind::Number(number) => {
                let name = format!("{}", number);
                Ok(Some(ClassElementName {
                    func_name: Some(name.clone()),
                    property_key: Node::new(NodeBase::String(name), tok.loc),
                    is_private: false,
                }))
            }
            Kind::Symbol(Symbol::OpeningBoxBracket) => {
                let key = self.read_assignment_expression()?;
                expect!(self, Kind::Symbol(Symbol::ClosingBoxBracket), "expect ']'");
                Ok(Some(ClassElementName {
                    func_name: None,
                    property_key: key,
                    is_private: false,
                }))
            }
            Kind::Symbol(Symbol::Hash) => {
                let name = self.read_private_identifier_name(tok.loc)?;
                Ok(Some(ClassElementName {
                    func_name: Some(name.clone()),
                    property_key: Node::new(NodeBase::String(name), tok.loc),
                    is_private: true,
                }))
            }
            _ => Err(Error::UnexpectedToken(
                tok.loc,
                "invalid class element name".to_string(),
            )),
        }
    }

    fn peek_identifier(&mut self, expected: &str) -> bool {
        matches!(
            self.lexer.peek_skip_lineterminator(),
            Ok(Token {
                kind: Kind::Identifier(ref name),
                contains_escape: false,
                ..
            }) if name == expected
        )
    }

    fn reject_arrow_class_heritage(&mut self) -> Result<(), Error> {
        let save_pos = self.lexer.token_pos;
        if self.peek_identifier("async") {
            self.lexer.next_skip_lineterminator()?;
        }
        if matches!(
            self.lexer.peek_skip_lineterminator()?.kind,
            Kind::Symbol(Symbol::OpeningParen)
        ) {
            let mut depth = 0usize;
            loop {
                let tok = self.lexer.next_skip_lineterminator()?;
                match tok.kind {
                    Kind::Symbol(Symbol::OpeningParen) => depth += 1,
                    Kind::Symbol(Symbol::ClosingParen) => {
                        depth = depth.saturating_sub(1);
                        if depth == 0 {
                            break;
                        }
                    }
                    Kind::EOF => break,
                    _ => {}
                }
            }
            if matches!(
                self.lexer.peek_skip_lineterminator()?.kind,
                Kind::Symbol(Symbol::FatArrow)
            ) {
                return Err(Error::UnexpectedToken(
                    self.lexer.get_current_loc(),
                    "class heritage cannot be an arrow function".to_string(),
                ));
            }
        }
        self.lexer.token_pos = save_pos;
        Ok(())
    }

    /// https://tc39.github.io/ecma262/#prod-LexicalDeclaration
    fn read_lexical_declaration(&mut self, is_const: bool) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        let var_kind = if is_const {
            VarKind::Const
        } else {
            VarKind::Let
        };

        let mut list = vec![];

        loop {
            let decl = self.read_variable_declaration_with_kind(var_kind)?;
            if is_const && matches!(decl.base, NodeBase::VarDecl(_, None, _)) {
                return Err(Error::UnexpectedToken(
                    decl.loc,
                    "const declaration requires initializer.".to_string(),
                ));
            }
            list.push(decl);

            if !self.variable_declaration_continuation()? {
                break;
            }
        }

        Ok(Node::new(NodeBase::StatementList(list), loc))
    }

    /// https://tc39.github.io/ecma262/#prod-FunctionDeclaration
    fn read_function_declaration(&mut self) -> Result<Node, Error> {
        self.read_function_declaration_inner(false)
    }

    fn read_function_declaration_inner(&mut self, is_async: bool) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        let is_generator = self.lexer.skip(Symbol::Asterisk)?;
        let name = if let Kind::Identifier(name) = self.lexer.next_skip_lineterminator()?.kind {
            name
        } else {
            return Err(Error::Expect(loc, "expect function name".to_string()));
        };

        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let params = self.read_formal_parameters()?;

        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");

        let prev_allow_await = self.allow_await;
        self.allow_await = is_async;
        let body = self.read_block();
        self.allow_await = prev_allow_await;
        let body = body?;
        Self::validate_formal_parameters(&params, &body, loc, is_async || is_generator)?;
        Self::validate_formal_parameter_context(&params, loc, is_generator, is_async)?;
        Self::validate_strict_function_name(&name, &body, loc)?;

        Ok(Node::new(
            if is_async && is_generator {
                NodeBase::AsyncGeneratorFunctionDecl(name, params, Box::new(body))
            } else if is_async {
                NodeBase::AsyncFunctionDecl(name, params, Box::new(body))
            } else if is_generator {
                NodeBase::GeneratorFunctionDecl(name, params, Box::new(body))
            } else {
                NodeBase::FunctionDecl(name, params, Box::new(body))
            },
            loc,
        ))
    }

    fn read_formal_parameters(&mut self) -> Result<FormalParameters, Error> {
        if self.lexer.skip(Symbol::ClosingParen)? {
            return Ok(vec![]);
        }

        let mut params = vec![];

        loop {
            let mut rest_param = false;

            let index = params.len();
            params.push(if self.lexer.skip(Symbol::Spread)? {
                rest_param = true;
                self.read_function_rest_parameter(index)?
            } else {
                self.read_formal_parameter(index)?
            });

            if self.lexer.skip(Symbol::ClosingParen).unwrap_or(false) {
                break;
            }

            if rest_param {
                return Err(Error::UnexpectedToken(
                    self.lexer.get_current_loc(),
                    "rest parameter must be the last formal parameter".to_string(),
                ));
            }

            expect!(self, Kind::Symbol(Symbol::Comma), "expect ','");
            if self.lexer.skip(Symbol::ClosingParen)? {
                break;
            }
        }

        Ok(params)
    }

    // TODO: Support all features: https://tc39.github.io/ecma262/#prod-FormalParameter
    fn read_formal_parameter(&mut self, index: usize) -> Result<FormalParameter, Error> {
        let loc = self.lexer.get_current_loc();
        if matches!(
            self.lexer.peek_skip_lineterminator()?.kind,
            Kind::Symbol(Symbol::OpeningBrace) | Kind::Symbol(Symbol::OpeningBoxBracket)
        ) {
            let pattern = self.read_binding_pattern()?;
            let init = if self.lexer.skip(Symbol::Assign)? {
                Some(self.read_assignment_expression()?)
            } else {
                None
            };
            return Ok(FormalParameter::new_pattern(
                format!("__rapidus_param_{}", index),
                pattern,
                init,
                false,
            ));
        }
        let name = if let Kind::Identifier(name) = self.lexer.next_skip_lineterminator()?.kind {
            name
        } else {
            return Err(Error::Expect(
                loc,
                "expect identifier (unsupported feature)".to_string(),
            ));
        };
        if self.allow_await && name == "await" {
            return Err(Error::UnexpectedToken(
                loc,
                "await cannot be used as a binding identifier in an async function".to_string(),
            ));
        }
        let init = if self.lexer.skip(Symbol::Assign)? {
            Some(self.read_assignment_expression()?)
        } else {
            None
        };
        Ok(FormalParameter::new(name, init, false))
    }

    fn read_function_rest_parameter(&mut self, index: usize) -> Result<FormalParameter, Error> {
        let loc = self.lexer.get_current_loc();
        if matches!(
            self.lexer.peek_skip_lineterminator()?.kind,
            Kind::Symbol(Symbol::OpeningBrace) | Kind::Symbol(Symbol::OpeningBoxBracket)
        ) {
            let pattern = self.read_binding_pattern()?;
            return Ok(FormalParameter::new_pattern(
                format!("__rapidus_param_{}", index),
                pattern,
                None,
                true,
            ));
        }
        let name = if let Kind::Identifier(name) = self.lexer.next()?.kind {
            name
        } else {
            return Err(Error::Expect(
                loc,
                "rest params: expect identifier".to_string(),
            ));
        };
        if self.allow_await && name == "await" {
            return Err(Error::UnexpectedToken(
                loc,
                "await cannot be used as a binding identifier in an async function".to_string(),
            ));
        }
        Ok(FormalParameter::new(name, None, true))
    }
}

#[test]
fn number() {
    let mut parser = Parser::new("test", "12345".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn string() {
    let mut parser = Parser::new("test", "\"aaa\"".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn boolean() {
    let mut parser = Parser::new("test", "true; false".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn identifier() {
    let mut parser = Parser::new("test", "variable".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn array1() {
    let mut parser = Parser::new("test", "[1, 2]".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn array2() {
    let mut parser = Parser::new("test", "[]".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn array3() {
    let mut parser = Parser::new("test", "[,,]".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn array4() {
    let mut parser = Parser::new("test", "[1,2,".to_string());
    parser.parse_all().expect_err("should be error");
}

#[test]
fn object1() {
    let mut parser = Parser::new("test", "a = {x: 123, 1.2: 456}".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn object2() {
    for input in ["a = {}", "a = {b}"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().unwrap();
    }
    for input in ["a = {b:6 c}", "a = {b:6, 777}"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err(input);
    }
}

#[test]
fn simple_expr_5arith() {
    let mut parser = Parser::new("test", "31 + 26 / 3 - 1 * 20 % 3".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_eq1() {
    let mut parser = Parser::new("test", "1 + 2 == 3".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_eq2() {
    let mut parser = Parser::new("test", "1 + 2 != 3".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_eq3() {
    let mut parser = Parser::new("test", "1 + 2 === 3".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_eq4() {
    let mut parser = Parser::new("test", "1 + 2 !== 3".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_rel1() {
    let mut parser = Parser::new("test", "1 + 2 < 3".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_rel2() {
    let mut parser = Parser::new("test", "1 + 2 > 3".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_rel3() {
    let mut parser = Parser::new("test", "1 + 2 <= 3".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_rel4() {
    let mut parser = Parser::new("test", "1 + 2 >= 3".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_cond() {
    let mut parser = Parser::new("test", "n == 1 ? 2 : max".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_logical_or1() {
    let mut parser = Parser::new("test", "1 || 0".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}
#[test]
fn simple_expr_logical_or2() {
    let mut parser = Parser::new("test", "1 && 0".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_bitwise_and1() {
    let mut parser = Parser::new("test", "1 & 3".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_bitwise_and2() {
    let mut parser = Parser::new("test", "1 ^ 3".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_bitwise_and3() {
    let mut parser = Parser::new("test", "1 | 3".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_shift1() {
    let mut parser = Parser::new("test", "1 << 2".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_shift2() {
    let mut parser = Parser::new("test", "1 >> 2".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_shift3() {
    let mut parser = Parser::new("test", "1 >>> 2".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_exp() {
    let mut parser = Parser::new("test", "20**50**70".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn expression_statement() {
    for input in ["1 2 3"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
    for input in ["for(;false;){} 4"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().unwrap();
    }
}

#[test]
fn simple_expr_unary1() {
    let mut parser = Parser::new("test", "delete a".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_unary2() {
    let mut parser = Parser::new("test", "void a".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_unary3() {
    let mut parser = Parser::new("test", "typeof a".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_unary4() {
    let mut parser = Parser::new("test", "+a".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_unary5() {
    let mut parser = Parser::new("test", "-a".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_unary6() {
    let mut parser = Parser::new("test", "~a".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_unary7() {
    let mut parser = Parser::new("test", "!a".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_unary8() {
    let mut parser = Parser::new("test", "++a".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_unary9() {
    let mut parser = Parser::new("test", "--a".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_unary10() {
    let mut parser = Parser::new("test", "a++".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_unary11() {
    let mut parser = Parser::new("test", "a--".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_assign1() {
    let mut parser = Parser::new("test", "v = 1".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_assign2() {
    let mut parser = Parser::new("test", "v += 1".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}
#[test]
fn simple_expr_assign3() {
    let mut parser = Parser::new("test", "v -= 1".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}
#[test]
fn simple_expr_assign4() {
    let mut parser = Parser::new("test", "v *= 1".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}
#[test]
fn simple_expr_assign5() {
    let mut parser = Parser::new("test", "v /= 1".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_assign6() {
    let mut parser = Parser::new("test", "v %= 1".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_new() {
    let mut parser = Parser::new("test", "new f(1)".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn simple_expr_new_without_args() {
    let mut parser = Parser::new("test", "new f".to_string());
    parser.parse_all().unwrap();
}

#[test]
fn simple_expr_parentheses() {
    let mut parser = Parser::new("test", "2 * (1 + 3)".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn call1() {
    let mut parser = Parser::new("test", "f(1, 2,)".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn call2() {
    let mut parser = Parser::new("test", "f()".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn call3() {
    let mut parser = Parser::new("test", "f(1, 2, 3)".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn call4() {
    for input in ["f(,)", "f(", "f(1", "f(1,", "f.7", "f[5", "f(1 a)"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
    for input in ["f[3]()"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().unwrap();
    }
}

#[test]
fn member1() {
    let mut parser = Parser::new("test", "a.b.c".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn member2() {
    let mut parser = Parser::new("test", "console.log".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn var_decl1() {
    let mut parser = Parser::new("test", "var a, b = 21".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn var_decl2() {
    for input in ["var 7"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn block1() {
    let mut parser = Parser::new("test", "{ a=1 }".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn block2() {
    for input in ["{", "{ a", "{ a=", "{ a=1", "}", "{ 7z }", "{a=0 8k}"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn break1() {
    let mut parser = Parser::new("test", "while(1){break}".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn break2() {
    for input in ["while(1){break 7}"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn continue1() {
    let mut parser = Parser::new("test", "while(1){continue}".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn continue2() {
    for input in ["while(1){continue 825}"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn return1() {
    let mut parser = Parser::new("test", "return 1".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn return2() {
    let mut parser = Parser::new("test", "return;".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn if1() {
    let mut parser = Parser::new(
        "test",
        "if (x <= 2)
            then_stmt
        else
            else_stmt"
            .to_string(),
    );
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn if2() {
    let mut parser = Parser::new("test", "if (x <= 2) then_stmt ".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn if3() {
    for input in ["if(", "if()else", "if(true){} 8j"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn switch1() {
    let mut parser = Parser::new(
        "test",
        "switch(1) {
            case 1:
        }"
        .to_string(),
    );
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn while_() {
    let mut parser = Parser::new("test", "while (true) { }".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn for1() {
    let mut parser = Parser::new("test", "for (;;) { }".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn for_etc() {
    for input in [
        "for(){}",
        "for(;){}",
        "for(;;;){}",
        "for(var){}",
        "for(var a=1){}",
        "for(a=1){}",
        "for(a=1;a<8)",
        "for(a=1;a<8;a++",
    ]
    .iter()
    {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn function_decl1() {
    let mut parser = Parser::new(
        "test",
        "function
            f
            (
            )
            {
            }"
        .to_string(),
    );
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn function_decl2() {
    let mut parser = Parser::new("test", "function f() { return }".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn function_decl3() {
    let mut parser = Parser::new(
        "test",
        "function f(x, y, ...z) { return x + y }".to_string(),
    );
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn function_decl_etc() {
    for input in [
        "function",
        "function ()",
        "function f(x",
        "function f(x,",
        "function f(,){}",
        "function f(x,.y){}",
        "function f(x,..y){}",
        "function f(x,...y,z){}",
        "function f(x,...7){}",
    ]
    .iter()
    {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err(input);
    }
    for input in ["a = function(x,y){b=1}"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().unwrap();
    }
}

#[test]
fn arrow_function1() {
    let mut parser = Parser::new("test", "(a, b) => { return a + b }".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn arrow_function2() {
    let mut parser = Parser::new("test", "(a, b, ...c) => a".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn arrow_function_etc() {
    for input in [
        "()
        =>{}",
        "(a)=>{ return 7",
        "(...a, b)=>{ }",
    ]
    .iter()
    {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err(input);
    }
    for input in ["a = (x,y) => x + y", "a = x => x * x"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().unwrap();
    }
}

#[test]
fn asi1() {
    let mut parser = Parser::new(
        "test",
        "function f()
         {
             return
             {};
         }"
        .to_string(),
    );
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn asi2() {
    let mut parser = Parser::new(
        "test",
        "
        b = a
        ++b
        "
        .to_string(),
    );
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn throw() {
    let mut parser = Parser::new("test", "throw 10".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
    for input in [
        "throw",
        "throw
    100",
        "throw;",
        "throw}",
    ]
    .iter()
    {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn try_catch1() {
    let mut parser = Parser::new("test", "try {} catch(e){} finally{}".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());

    for input in ["try {} catch", "try {} catch(7)"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn try_catch2() {
    let mut parser = Parser::new("test", "try {} catch(e){}".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}

#[test]
fn try_catch3() {
    let mut parser = Parser::new("test", "try {} finally {}".to_string());
    insta::assert_debug_snapshot!(parser.parse_all().unwrap());
}
