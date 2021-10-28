mod for_;
mod if_;
mod return_;
mod while_;

pub mod script;

use ansi_term::Colour;
use rapidus_ast::{
    loc::SourceLoc, BinOp, FormalParameter, FormalParameters, MethodDefinitionKind, Node, NodeBase,
    PropertyDefinition, UnaryOp, VarKind,
};
use rapidus_lexer::token::{Keyword, Kind, Symbol, Token};
pub use rapidus_lexer::Error;
use rapidus_lexer::{get_error_line, Lexer};
use script::ScriptInfo;
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
}

impl Parser {
    pub fn new(file_name: impl Into<PathBuf>, code: impl Into<String>) -> Parser {
        Parser {
            file_name: file_name.into(),
            lexer: Lexer::new(code.into()),
        }
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
        self.read_script()
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
                Kind::Keyword(Keyword::Function) => self.read_declaration(),
                Kind::Keyword(Keyword::Const) => self.read_declaration(),
                Kind::Keyword(Keyword::Let) => self.read_declaration(),
                _ => self.read_statement_sub(accept_case_label),
            }
        } else {
            Err(Error::NormalEOF)
        }
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

        // Label
        if let Kind::Identifier(ref name) = tok.kind {
            let maybe_colon = self.lexer.peek_skip_lineterminator();
            if let Ok(Token {
                kind: Kind::Symbol(Symbol::Colon),
                ..
            }) = maybe_colon
            {
                assert_eq!(
                    self.lexer.next_skip_lineterminator()?.kind,
                    Kind::Symbol(Symbol::Colon)
                );
                // TODO: https://tc39.github.io/ecma262/#prod-LabelledStatement
                // let labeled_item = self.read_statement_list_item()?;
                return Ok(Node::new(NodeBase::Label(name.clone()), tok.loc));
            }
        }

        let mut is_expression_statement = false;
        let stmt = match tok.kind {
            Kind::Keyword(Keyword::If) => self.read_if_statement(tok.loc),
            Kind::Keyword(Keyword::Switch) => self.read_switch_statement(tok.loc),
            Kind::Keyword(Keyword::Var) => self.read_variable_statement(tok.loc),
            Kind::Keyword(Keyword::While) => self.read_while_statement(tok.loc),
            Kind::Keyword(Keyword::For) => self.read_for_statement(tok.loc),
            Kind::Keyword(Keyword::Return) => self.read_return_statement(tok.loc),
            Kind::Keyword(Keyword::Break) => self.read_break_statement(),
            Kind::Keyword(Keyword::Continue) => self.read_continue_statement(),
            Kind::Keyword(Keyword::Try) => self.read_try_statement(),
            Kind::Keyword(Keyword::Throw) => self.read_throw_statement(),
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
                        self.lexer.next()?;
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
        let loc = self.lexer.get_current_loc();
        let name = match self.lexer.next_skip_lineterminator()?.kind {
            Kind::Identifier(name) => name,
            _ => {
                return Err(Error::UnexpectedToken(
                    loc,
                    "Expect identifier.".to_string(),
                ));
            }
        };

        if self.lexer.skip(Symbol::Assign).unwrap_or(false) {
            Ok(Node::new(
                NodeBase::VarDecl(name, Some(Box::new(self.read_initializer()?)), VarKind::Var),
                loc,
            ))
        } else {
            Ok(Node::new(NodeBase::VarDecl(name, None, VarKind::Var), loc))
        }
    }

    /// https://tc39.github.io/ecma262/#prod-Initializer
    fn read_initializer(&mut self) -> Result<Node, Error> {
        self.read_assignment_expression()
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

        if let Ok(tok) = self.lexer.next() {
            macro_rules! assignop {
                ($op:ident) => {{
                    let lhs_loc = lhs.loc;
                    lhs = Node::new(
                        NodeBase::Assign(
                            Box::new(lhs.clone()),
                            Box::new(Node::new(
                                NodeBase::BinaryOp(
                                    Box::new(lhs),
                                    Box::new(self.read_assignment_expression()?),
                                    BinOp::$op,
                                ),
                                loc,
                            )),
                        ),
                        lhs_loc,
                    );
                }};
            }
            match tok.kind {
                Kind::Symbol(Symbol::Assign) => {
                    let lhs_loc = lhs.loc;
                    lhs = Node::new(
                        NodeBase::Assign(
                            Box::new(lhs),
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
                _ => self.lexer.unget(),
            }
        }
        Ok(lhs)
    }

    /// https://tc39.github.io/ecma262/#prod-ConditionalExpression
    fn read_conditional_expression(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();

        let lhs = self.read_logical_or_expression()?;

        if let Ok(tok) = self.lexer.next() {
            match tok.kind {
                Kind::Symbol(Symbol::Question) => {
                    let then_ = self.read_assignment_expression()?;
                    expect!(self, Kind::Symbol(Symbol::Colon), "expect ':'");
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

    // https://tc39.github.io/ecma262/#prod-LogicalORExpression
    expression!(
        read_logical_or_expression,
        read_logical_and_expression,
        [Symbol::LOr]
    );

    // https://tc39.github.io/ecma262/#prod-LogicalANDExpression
    expression!(
        read_logical_and_expression,
        read_bitwise_or_expression,
        [Symbol::LAnd]
    );

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
    expression!(
        read_relational_expression,
        read_shift_expression,
        [Symbol::Lt, Symbol::Gt, Symbol::Le, Symbol::Ge]
    );

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
        match self.lexer.peek(0) {
            Ok(ok) => match ok.kind {
                Kind::Keyword(Keyword::Delete)
                | Kind::Keyword(Keyword::Void)
                | Kind::Keyword(Keyword::Typeof)
                | Kind::Symbol(Symbol::Add)
                | Kind::Symbol(Symbol::Sub)
                | Kind::Symbol(Symbol::BitwiseNot)
                | Kind::Symbol(Symbol::Not) => true,
                _ => false,
            },
            Err(_) => false,
        }
    }

    /// https://tc39.github.io/ecma262/#prod-UnaryExpression
    fn read_unary_expression(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        let tok = self.lexer.next()?;
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
        let mut lhs = Node::new(NodeBase::Call(Box::new(first_member_expr), args), loc);

        while let Ok(tok) = self.lexer.next_skip_lineterminator() {
            let loc_ = self.lexer.get_current_loc();
            match tok.kind {
                Kind::Symbol(Symbol::OpeningParen) => {
                    let args = self.read_arguments()?;
                    lhs = Node::new(NodeBase::Call(Box::new(lhs), args), loc)
                }
                Kind::Symbol(Symbol::Point) => match self.lexer.next_skip_lineterminator()?.kind {
                    Kind::Identifier(name) => {
                        lhs = Node::new(NodeBase::Member(Box::new(lhs), name), loc)
                    }
                    Kind::Keyword(kw) => {
                        lhs =
                            Node::new(NodeBase::Member(Box::new(lhs), kw.to_str().to_owned()), loc)
                    }
                    _ => {
                        return Err(Error::Expect(loc_, "expect identifier".to_string()));
                    }
                },
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
            let lhs = self.read_member_expression()?;
            expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('.");
            let args = self.read_arguments()?;
            let call_node = Node::new(NodeBase::Call(Box::new(lhs), args), call_loc);
            let new_node = Node::new(NodeBase::New(Box::new(call_node)), loc);
            new_node
        } else {
            self.read_primary_expression()?
        };
        while let Ok(tok) = self.lexer.next_skip_lineterminator() {
            let loc_ = self.lexer.get_current_loc();
            match tok.kind {
                Kind::Symbol(Symbol::Point) => match self.lexer.next_skip_lineterminator()?.kind {
                    Kind::Identifier(name) => {
                        lhs = Node::new(NodeBase::Member(Box::new(lhs), name), loc)
                    }
                    Kind::Keyword(kw) => {
                        lhs =
                            Node::new(NodeBase::Member(Box::new(lhs), kw.to_str().to_owned()), loc)
                    }
                    _ => {
                        return Err(Error::Expect(loc_, "expect identifier".to_string()));
                    }
                },
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
            args.push(self.read_assignment_expression()?);
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
            Kind::Identifier(ident) => Ok(Node::new(NodeBase::Identifier(ident), tok.loc)),
            Kind::String(s) => Ok(Node::new(NodeBase::String(s), tok.loc)),
            Kind::Number(num) => Ok(Node::new(NodeBase::Number(num), tok.loc)),
            _ => Err(Error::UnexpectedToken(
                tok.loc,
                format!("unexpected token."),
            )),
        }
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
        Ok(Node::new(
            NodeBase::ArrowFunction(params, Box::new(body)),
            params_loc,
        ))
    }

    /// https://tc39.github.io/ecma262/#prod-FunctionDeclaration
    fn read_function_expression(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        let name = if let Kind::Identifier(name) = self.lexer.peek(0)?.kind {
            self.lexer.next()?;
            Some(name)
        } else {
            None
        };

        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let params = self.read_formal_parameters()?;

        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");

        let body = self.read_block()?;

        Ok(Node::new(
            NodeBase::FunctionExpr(name, params, Box::new(body)),
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
            } else {
                elements.push(self.read_assignment_expression()?);
            }
            self.lexer.skip(Kind::Symbol(Symbol::Comma))?;
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
        fn to_string(kind: Kind) -> String {
            match kind {
                Kind::Identifier(name) => name,
                Kind::Number(n) => format!("{}", n),
                Kind::String(s) => s,
                _ => unimplemented!(),
            }
        }

        if self.lexer.skip(Symbol::Spread)? {
            let node = self.read_assignment_expression()?;
            return Ok(PropertyDefinition::SpreadObject(node));
        }

        let tok = self.lexer.next_skip_lineterminator()?;

        if self.lexer.skip(Symbol::Colon)? {
            let val = self.read_assignment_expression()?;
            return Ok(PropertyDefinition::Property(to_string(tok.kind), val));
        }

        if let Kind::Identifier(name) = tok.kind {
            if name == "get" || name == "set" {
                let may_identifier = self.lexer.peek_skip_lineterminator();
                if may_identifier.is_ok() && may_identifier.unwrap().is_identifier() {
                    let f = self.read_function_expression()?;
                    let func_name = if let NodeBase::FunctionExpr(ref name, _, _) = f.base {
                        name.clone().unwrap()
                    } else {
                        panic!()
                    };
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
            skip_symbol_or_error!(self.lexer, Symbol::OpeningParen);
            // TODO: should accept BindingPattern
            let loc_param = self.lexer.get_current_loc();
            let catch_param = match self.lexer.next()?.kind {
                Kind::Identifier(s) => Node::new(NodeBase::Identifier(s), loc_param),
                _ => {
                    return Err(Error::UnexpectedToken(
                        loc_param,
                        "expected identifier.".to_string(),
                    ));
                }
            };
            skip_symbol_or_error!(self.lexer, Symbol::ClosingParen);
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
            _ => unreachable!(),
        }
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
            let loc = self.lexer.get_current_loc();
            let name = match self.lexer.next_skip_lineterminator()?.kind {
                Kind::Identifier(name) => name,
                _ => {
                    return Err(Error::UnexpectedToken(
                        loc,
                        "Expect identifier.".to_string(),
                    ));
                }
            };

            if self.lexer.skip(Symbol::Assign)? {
                let init = Some(Box::new(self.read_initializer()?));
                let decl = NodeBase::VarDecl(name, init, var_kind);
                list.push(Node::new(decl, loc))
            } else {
                list.push(Node::new(NodeBase::VarDecl(name, None, var_kind), loc))
            }

            if !self.variable_declaration_continuation()? {
                break;
            }
        }

        Ok(Node::new(NodeBase::StatementList(list), loc))
    }

    /// https://tc39.github.io/ecma262/#prod-FunctionDeclaration
    fn read_function_declaration(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();
        let name = if let Kind::Identifier(name) = self.lexer.next_skip_lineterminator()?.kind {
            name
        } else {
            return Err(Error::Expect(loc, "expect function name".to_string()));
        };

        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let params = self.read_formal_parameters()?;

        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");

        let body = self.read_block()?;

        Ok(Node::new(
            NodeBase::FunctionDecl(name, params, Box::new(body)),
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

            params.push(if self.lexer.skip(Symbol::Spread)? {
                rest_param = true;
                self.read_function_rest_parameter()?
            } else {
                self.read_formal_parameter()?
            });

            if self
                .lexer
                .skip2(Kind::Symbol(Symbol::ClosingParen))
                .unwrap_or(false)
            {
                break;
            }

            if rest_param {
                return Err(Error::UnexpectedToken(
                    self.lexer.get_current_loc(),
                    "rest parameter must be the last formal parameter".to_string(),
                ));
            }

            expect!(self, Kind::Symbol(Symbol::Comma), "expect ','");
        }

        Ok(params)
    }

    // TODO: Support all features: https://tc39.github.io/ecma262/#prod-FormalParameter
    fn read_formal_parameter(&mut self) -> Result<FormalParameter, Error> {
        let loc = self.lexer.get_current_loc();
        let name = if let Kind::Identifier(name) = self.lexer.next_skip_lineterminator()?.kind {
            name
        } else {
            return Err(Error::Expect(
                loc,
                "expect identifier (unsupported feature)".to_string(),
            ));
        };
        // TODO: Implement initializer.
        Ok(FormalParameter::new(name, None, false))
    }

    fn read_function_rest_parameter(&mut self) -> Result<FormalParameter, Error> {
        let loc = self.lexer.get_current_loc();
        Ok(FormalParameter::new(
            if let Kind::Identifier(name) = self.lexer.next()?.kind {
                name
            } else {
                return Err(Error::Expect(
                    loc,
                    "rest params: expect identifier".to_string(),
                ));
            },
            None,
            true,
        ))
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

    for input in ["try {} catch", "try {} catch {}", "try {} catch(7)"].iter() {
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
