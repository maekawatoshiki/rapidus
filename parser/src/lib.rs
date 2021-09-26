pub mod lexer;
pub mod token;

use ansi_term::Colour;
use rapidus_ast::{
    BinOp, FormalParameter, FormalParameters, MethodDefinitionKind, Node, NodeBase,
    PropertyDefinition, UnaryOp, VarKind,
};
use std::fs::OpenOptions;
use std::io::Read;
use std::path::Path;
use token::{get_string_for_symbol, Keyword, Kind, Symbol, Token};

macro_rules! expect {
    ($self:ident, $kind:expr, $msg:expr) => {{
        let tok = $self.lexer.next_skip_lineterminator()?;
        if tok.kind != $kind {
            return Err(Error::Expect(tok.pos, $msg.to_string()));
        }
    }};
}

macro_rules! expect_no_lineterminator {
    ($self:ident, $kind:expr, $msg:expr) => {{
        let tok = $self.lexer.next()?;
        if tok.kind != $kind {
            return Err(Error::Expect(tok.pos, $msg.to_string()));
        }
    }};
}

// TODO: It's dirty. Make it simpler.
#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    NormalEOF,
    UnexpectedEOF(String),          // error msg
    UnexpectedToken(usize, String), // position, error msg
    UnsupportedFeature(usize),      // position
    Expect(usize, String),          // position, error msg
    InvalidToken(usize),
    General(usize, String),
}

#[derive(Clone, Debug)]
pub struct Parser {
    pub file_name: String,
    pub lexer: lexer::Lexer,
}

#[derive(Clone, Debug)]
/// Information about a script (module), e.g. file name, source text, and correspondence between char postions and line numbers.
pub struct ScriptInfo {
    /// File name with Absolute path.
    pub file_name: String,
    /// Script text.
    pub code: String,
    /// Correspondence between char postions and line numbers.
    pub pos_line_list: Vec<(usize, usize)>,
}

impl Parser {
    pub fn new(file_name: impl Into<String>, code: impl Into<String>) -> Parser {
        Parser {
            file_name: file_name.into(),
            lexer: lexer::Lexer::new(code.into()),
        }
    }

    /// Load file and generate Parser from the file.
    /// ## Arguments
    /// * `file_name` - A module file name.
    pub fn load_module(file_name: impl Into<String>) -> Result<Parser, Error> {
        let file_name = file_name.into();
        let path = Path::new(&file_name).with_extension("js");
        let absolute_path = match path.canonicalize() {
            Ok(path) => path,
            Err(ioerr) => {
                let msg = format!("{}", ioerr);
                println!("Error: Cannot find module file. '{}'", &file_name);
                println!("{}", msg);
                return Err(Error::General(0, msg));
            }
        };

        let mut file_body = String::new();

        match OpenOptions::new().read(true).open(&absolute_path) {
            Ok(mut ok) => ok
                .read_to_string(&mut file_body)
                .ok()
                .expect("cannot read file"),
            Err(ioerr) => {
                let msg = format!("{}", ioerr);
                println!("Error: Cannot find module file. '{}'", &file_name);
                println!("{}", msg);
                return Err(Error::General(0, msg));
            }
        };

        Ok(Parser::new(absolute_path.to_string_lossy(), file_body))
    }

    pub fn into_script_info(self) -> ScriptInfo {
        ScriptInfo {
            file_name: self.file_name,
            code: self.lexer.code,
            pos_line_list: self.lexer.pos_line_list,
        }
    }

    /// Display error position in the source script.
    /// ## Arguments
    /// * `pos` - A char position in the source script.
    /// * `msg` - An error message text.
    pub fn show_error_at(&self, pos: usize, msg: impl Into<String>) {
        let (source_at_err_point, _pos, line) = self.lexer.get_code_around_err_point(pos);
        eprintln!(
            "{}: line {}: {}\n{}",
            Colour::Red.bold().paint("SyntaxError"),
            line,
            msg.into(),
            source_at_err_point,
        );
    }

    /// Display syntax error message.
    /// ## Arguments
    /// * `err` - parser::Error.
    pub fn handle_error(&self, err: &Error) {
        match err {
            Error::NormalEOF => unreachable!(),
            Error::Expect(pos, msg)
            | Error::General(pos, msg)
            | Error::UnexpectedToken(pos, msg) => {
                self.show_error_at(*pos, msg.clone());
            }
            Error::UnexpectedEOF(msg) => {
                self.show_error_at(self.lexer.pos, format!("unexpected EOF. {}", msg))
            }
            Error::InvalidToken(pos) => self.show_error_at(*pos, "Invalid token."),
            Error::UnsupportedFeature(pos) => {
                self.show_error_at(*pos, "Unsupported feature.");
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
        self.read_statements(false, false)
    }

    fn read_block_statement(&mut self) -> Result<Node, Error> {
        self.read_statements(true, true)
    }

    fn read_block(&mut self) -> Result<Node, Error> {
        self.read_statements(true, false)
    }

    fn read_statements(
        &mut self,
        break_when_closingbrase: bool,
        is_block_statement: bool,
    ) -> Result<Node, Error> {
        let pos = if break_when_closingbrase {
            self.lexer.get_prev_pos()
        } else {
            self.lexer.get_current_pos()
        };

        let mut items = vec![];

        loop {
            match self
                .lexer
                .next_if_skip_lineterminator(Kind::Symbol(Symbol::ClosingBrace))
            {
                Ok(true) => {
                    if break_when_closingbrase {
                        break;
                    } else {
                        return Err(Error::UnexpectedToken(
                            self.lexer.get_prev_pos(),
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

            match self.read_statement_list_item() {
                Ok(ok) => items.push(ok),
                Err(Error::NormalEOF) => {
                    return Err(Error::UnexpectedEOF("".to_string()));
                }
                Err(e) => return Err(e),
            }

            while match self
                .lexer
                .next_if_skip_lineterminator(Kind::Symbol(Symbol::Semicolon))
            {
                Ok(succ) => succ,
                Err(Error::NormalEOF) => false,
                Err(e) => return Err(e),
            } {}
        }

        if is_block_statement {
            Ok(Node::new(NodeBase::Block(items), pos))
        } else {
            Ok(Node::new(NodeBase::StatementList(items), pos))
        }
    }

    /// https://tc39.github.io/ecma262/#prod-StatementListItem
    fn read_statement_list_item(&mut self) -> Result<Node, Error> {
        if let Ok(tok) = self.lexer.peek_skip_lineterminator() {
            match tok.kind {
                Kind::Keyword(Keyword::Function) => self.read_declaration(),
                Kind::Keyword(Keyword::Const) => self.read_declaration(),
                Kind::Keyword(Keyword::Let) => self.read_declaration(),
                _ => self.read_statement(),
            }
        } else {
            Err(Error::NormalEOF)
        }
    }

    /// http://www.ecma-international.org/ecma-262/9.0/index.html#prod-Statement
    fn read_statement(&mut self) -> Result<Node, Error> {
        let tok = self.lexer.next_skip_lineterminator()?;

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
                let labeled_item = self.read_statement_list_item()?;
                return Ok(Node::new(
                    NodeBase::Label(name.clone(), Box::new(labeled_item)),
                    tok.pos,
                ));
            }
        }
        let mut is_expression_statement = false;
        let stmt = match tok.kind {
            Kind::Keyword(Keyword::If) => self.read_if_statement(),
            Kind::Keyword(Keyword::Var) => self.read_variable_statement(),
            Kind::Keyword(Keyword::While) => self.read_while_statement(),
            Kind::Keyword(Keyword::For) => self.read_for_statement(),
            Kind::Keyword(Keyword::Return) => self.read_return_statement(),
            Kind::Keyword(Keyword::Break) => self.read_break_statement(),
            Kind::Keyword(Keyword::Continue) => self.read_continue_statement(),
            Kind::Keyword(Keyword::Try) => self.read_try_statement(),
            Kind::Keyword(Keyword::Throw) => self.read_throw_statement(),
            Kind::Symbol(Symbol::OpeningBrace) => self.read_block_statement(),
            Kind::Symbol(Symbol::Semicolon) => return Ok(Node::new(NodeBase::Nope, tok.pos)),
            _ => {
                self.lexer.unget();
                is_expression_statement = true;
                self.read_expression_statement()
            }
        };

        match self
            .lexer
            .next_if_skip_lineterminator(Kind::Symbol(Symbol::Semicolon))
        {
            Ok(true) | Err(Error::NormalEOF) => {}
            Ok(false) => {
                if is_expression_statement {
                    match self.lexer.peek(0)?.kind {
                        Kind::LineTerminator | Kind::Symbol(Symbol::ClosingBrace) => {}
                        _ => {
                            return Err(Error::UnexpectedToken(
                                self.lexer.get_current_pos(),
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
    fn read_variable_statement(&mut self) -> Result<Node, Error> {
        self.read_variable_declaration_list()
    }

    /// https://tc39.github.io/ecma262/#prod-VariableDeclarationList
    fn read_variable_declaration_list(&mut self) -> Result<Node, Error> {
        let pos = self.lexer.get_prev_pos();
        let mut list = vec![];

        loop {
            list.push(self.read_variable_declaration()?);
            if !self.variable_declaration_continuation()? {
                break;
            }
        }

        Ok(Node::new(NodeBase::StatementList(list), pos))
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
            self.lexer.get_current_pos(),
            "expect ';' or line terminator".to_string(),
        ))
    }

    /// https://tc39.github.io/ecma262/#prod-VariableDeclaration
    fn read_variable_declaration(&mut self) -> Result<Node, Error> {
        let pos = self.lexer.get_current_pos();
        let name = match self.lexer.next_skip_lineterminator()?.kind {
            Kind::Identifier(name) => name,
            _ => {
                return Err(Error::UnexpectedToken(
                    self.lexer.get_prev_pos(),
                    "Expect identifier.".to_string(),
                ));
            }
        };

        if self
            .lexer
            .next_if_skip_lineterminator(Kind::Symbol(Symbol::Assign))?
        {
            Ok(Node::new(
                NodeBase::VarDecl(name, Some(Box::new(self.read_initializer()?)), VarKind::Var),
                pos,
            ))
        } else {
            Ok(Node::new(NodeBase::VarDecl(name, None, VarKind::Var), pos))
        }
    }

    /// https://tc39.github.io/ecma262/#prod-Initializer
    fn read_initializer(&mut self) -> Result<Node, Error> {
        self.read_assignment_expression()
    }
}

impl Parser {
    fn read_if_statement(&mut self) -> Result<Node, Error> {
        let pos = self.lexer.get_prev_pos();
        let oparen = self.lexer.next_skip_lineterminator()?;
        if oparen.kind != Kind::Symbol(Symbol::OpeningParen) {
            return Err(Error::Expect(oparen.pos, "expect '('".to_string()));
        }
        let cond = self.read_expression()?;
        let cparen = self.lexer.next_skip_lineterminator()?;
        if cparen.kind != Kind::Symbol(Symbol::ClosingParen) {
            return Err(Error::Expect(cparen.pos, "expect ')'".to_string()));
        }

        let then_ = self.read_statement()?;

        let pos_else = self.lexer.get_current_pos();
        if let Ok(expect_else_tok) = self.lexer.next_skip_lineterminator() {
            if expect_else_tok.kind == Kind::Keyword(Keyword::Else) {
                let else_ = self.read_statement()?;
                return Ok(Node::new(
                    NodeBase::If(Box::new(cond), Box::new(then_), Box::new(else_)),
                    pos,
                ));
            } else {
                self.lexer.unget();
            }
        }

        Ok(Node::new(
            NodeBase::If(
                Box::new(cond),
                Box::new(then_),
                Box::new(Node::new(NodeBase::Nope, pos_else)),
            ),
            pos,
        ))
    }
}

impl Parser {
    fn read_while_statement(&mut self) -> Result<Node, Error> {
        let pos = self.lexer.get_prev_pos();

        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let cond = self.read_expression()?;

        expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");

        let body = self.read_statement()?;

        Ok(Node::new(
            NodeBase::While(Box::new(cond), Box::new(body)),
            pos,
        ))
    }

    fn read_for_statement(&mut self) -> Result<Node, Error> {
        let pos = self.lexer.get_prev_pos();

        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let init = match self.lexer.peek(0)?.kind {
            Kind::Keyword(Keyword::Var) => {
                assert_eq!(self.lexer.next()?.kind, Kind::Keyword(Keyword::Var));
                self.read_variable_declaration_list()?
            }
            Kind::Keyword(Keyword::Let) | Kind::Keyword(Keyword::Const) => {
                self.read_declaration()?
            }
            Kind::Symbol(Symbol::Semicolon) => Node::new(NodeBase::Nope, self.lexer.get_prev_pos()),
            _ => self.read_expression()?,
        };
        expect!(self, Kind::Symbol(Symbol::Semicolon), "expect ';'");

        let cond = if self.lexer.next_if(Kind::Symbol(Symbol::Semicolon)) {
            Node::new(NodeBase::Boolean(true), self.lexer.get_prev_pos())
        } else {
            let step = self.read_expression()?;
            expect!(self, Kind::Symbol(Symbol::Semicolon), "expect ';'");
            step
        };

        let step = if self.lexer.next_if(Kind::Symbol(Symbol::ClosingParen)) {
            Node::new(NodeBase::Nope, self.lexer.get_prev_pos())
        } else {
            let step = self.read_expression()?;
            expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");
            step
        };

        let body = self.read_statement()?;

        let for_node = Node::new(
            NodeBase::For(
                Box::new(init),
                Box::new(cond),
                Box::new(step),
                Box::new(body),
            ),
            pos,
        );

        Ok(Node::new(NodeBase::Block(vec![for_node]), pos))
    }
}

impl Parser {
    fn read_break_statement(&mut self) -> Result<Node, Error> {
        let pos = self.lexer.get_prev_pos();
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::LineTerminator
            | Kind::Symbol(Symbol::Semicolon)
            | Kind::Symbol(Symbol::ClosingBrace) => {
                self.lexer.unget();
                Ok(Node::new(NodeBase::Break(None), pos))
            }
            Kind::Identifier(name) => Ok(Node::new(NodeBase::Break(Some(name)), pos)),
            _ => Err(Error::UnexpectedToken(
                tok.pos,
                "expected ';', identifier or line terminator".to_string(),
            )),
        }
    }

    fn read_continue_statement(&mut self) -> Result<Node, Error> {
        let pos = self.lexer.get_prev_pos();
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::LineTerminator
            | Kind::Symbol(Symbol::Semicolon)
            | Kind::Symbol(Symbol::ClosingBrace) => {
                self.lexer.unget();
                Ok(Node::new(NodeBase::Continue(None), pos))
            }
            Kind::Identifier(name) => Ok(Node::new(NodeBase::Continue(Some(name)), pos)),
            _ => Err(Error::UnexpectedToken(
                tok.pos,
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
                    let pos = self.lexer.get_current_pos();
                    lhs = Node::new(NodeBase::BinaryOp(
                        Box::new(lhs),
                        Box::new(self. $lower ()?),
                        op.as_binop().unwrap(),
                    ), pos);
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
        let pos = self.lexer.get_current_pos();

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
                    let lhs_pos = lhs.pos;
                    lhs = Node::new(
                        NodeBase::Assign(
                            Box::new(lhs.clone()),
                            Box::new(Node::new(
                                NodeBase::BinaryOp(
                                    Box::new(lhs),
                                    Box::new(self.read_assignment_expression()?),
                                    BinOp::$op,
                                ),
                                pos,
                            )),
                        ),
                        lhs_pos,
                    );
                }};
            }
            match tok.kind {
                Kind::Symbol(Symbol::Assign) => {
                    let lhs_pos = lhs.pos;
                    lhs = Node::new(
                        NodeBase::Assign(
                            Box::new(lhs),
                            Box::new(self.read_assignment_expression()?),
                        ),
                        lhs_pos,
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
        let pos = self.lexer.get_current_pos();

        let lhs = self.read_logical_or_expression()?;

        if let Ok(tok) = self.lexer.next() {
            match tok.kind {
                Kind::Symbol(Symbol::Question) => {
                    let then_ = self.read_assignment_expression()?;
                    expect!(self, Kind::Symbol(Symbol::Colon), "expect ':'");
                    let else_ = self.read_assignment_expression()?;
                    return Ok(Node::new(
                        NodeBase::TernaryOp(Box::new(lhs), Box::new(then_), Box::new(else_)),
                        pos,
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
        let pos = self.lexer.get_current_pos();
        let lhs = self.read_update_expression()?;
        if let Ok(tok) = self.lexer.next() {
            if let Kind::Symbol(Symbol::Exp) = tok.kind {
                return Ok(Node::new(
                    NodeBase::BinaryOp(
                        Box::new(lhs),
                        Box::new(self.read_exponentiation_expression()?),
                        BinOp::Exp,
                    ),
                    pos,
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
        let pos = self.lexer.get_current_pos();
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::Keyword(Keyword::Delete) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.read_unary_expression()?), UnaryOp::Delete),
                pos,
            )),
            Kind::Keyword(Keyword::Void) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.read_unary_expression()?), UnaryOp::Void),
                pos,
            )),
            Kind::Keyword(Keyword::Typeof) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.read_unary_expression()?), UnaryOp::Typeof),
                pos,
            )),
            Kind::Symbol(Symbol::Add) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.read_unary_expression()?), UnaryOp::Plus),
                pos,
            )),
            Kind::Symbol(Symbol::Sub) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.read_unary_expression()?), UnaryOp::Minus),
                pos,
            )),
            Kind::Symbol(Symbol::BitwiseNot) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.read_unary_expression()?), UnaryOp::BitwiseNot),
                pos,
            )),
            Kind::Symbol(Symbol::Not) => Ok(Node::new(
                NodeBase::UnaryOp(Box::new(self.read_unary_expression()?), UnaryOp::Not),
                pos,
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
                let pos = self.lexer.get_prev_pos();
                return Ok(Node::new(
                    NodeBase::UnaryOp(
                        Box::new(self.read_left_hand_side_expression()?),
                        UnaryOp::PrInc,
                    ),
                    pos,
                ));
            }
            Kind::Symbol(Symbol::Dec) => {
                self.lexer.next_skip_lineterminator().unwrap();
                let pos = self.lexer.get_prev_pos();
                return Ok(Node::new(
                    NodeBase::UnaryOp(
                        Box::new(self.read_left_hand_side_expression()?),
                        UnaryOp::PrDec,
                    ),
                    pos,
                ));
            }
            _ => {}
        }

        let pos = self.lexer.get_current_pos();
        let e = self.read_left_hand_side_expression()?;
        if let Ok(tok) = self.lexer.peek(0) {
            match tok.kind {
                Kind::Symbol(Symbol::Inc) => {
                    self.lexer.next().unwrap();
                    return Ok(Node::new(
                        NodeBase::UnaryOp(Box::new(e), UnaryOp::PoInc),
                        pos,
                    ));
                }
                Kind::Symbol(Symbol::Dec) => {
                    self.lexer.next().unwrap();
                    return Ok(Node::new(
                        NodeBase::UnaryOp(Box::new(e), UnaryOp::PoDec),
                        pos,
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
        let pos = first_member_expr.pos;
        let mut lhs = first_member_expr;
        match self
            .lexer
            .next_if_skip_lineterminator(Kind::Symbol(Symbol::OpeningParen))
        {
            Ok(true) => {
                let args = self.read_arguments()?;
                lhs = Node::new(NodeBase::Call(Box::new(lhs), args), pos)
            }
            _ => {
                panic!("CallExpression MUST start with MemberExpression.");
            }
        };

        while let Ok(tok) = self.lexer.next_skip_lineterminator() {
            let pos_ = self.lexer.get_current_pos();
            match tok.kind {
                Kind::Symbol(Symbol::OpeningParen) => {
                    let args = self.read_arguments()?;
                    lhs = Node::new(NodeBase::Call(Box::new(lhs), args), pos)
                }
                Kind::Symbol(Symbol::Point) => match self.lexer.next_skip_lineterminator()?.kind {
                    Kind::Identifier(name) => {
                        lhs = Node::new(NodeBase::Member(Box::new(lhs), name), pos)
                    }
                    Kind::Keyword(kw) => {
                        lhs =
                            Node::new(NodeBase::Member(Box::new(lhs), kw.to_str().to_owned()), pos)
                    }
                    _ => {
                        return Err(Error::Expect(pos_, "expect identifier".to_string()));
                    }
                },
                Kind::Symbol(Symbol::OpeningBoxBracket) => {
                    let idx = self.read_expression()?;
                    if !self.lexer.next_if(Kind::Symbol(Symbol::ClosingBoxBracket)) {
                        return Err(Error::Expect(
                            self.lexer.get_current_pos(),
                            "expect ']'".to_string(),
                        ));
                    }
                    lhs = Node::new(NodeBase::Index(Box::new(lhs), Box::new(idx)), pos);
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
        let pos = self.lexer.get_current_pos();
        let mut lhs = if self.lexer.peek_skip_lineterminator()?.kind == Kind::Keyword(Keyword::New)
        {
            self.lexer.next_skip_lineterminator()?;
            let call_pos = self.lexer.get_current_pos();
            let lhs = self.read_member_expression()?;
            expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('.");
            let args = self.read_arguments()?;
            let call_node = Node::new(NodeBase::Call(Box::new(lhs), args), call_pos);
            let new_node = Node::new(NodeBase::New(Box::new(call_node)), pos);
            new_node
        } else {
            self.read_primary_expression()?
        };
        while let Ok(tok) = self.lexer.next_skip_lineterminator() {
            let pos_ = self.lexer.get_current_pos();
            match tok.kind {
                Kind::Symbol(Symbol::Point) => match self.lexer.next_skip_lineterminator()?.kind {
                    Kind::Identifier(name) => {
                        lhs = Node::new(NodeBase::Member(Box::new(lhs), name), pos)
                    }
                    Kind::Keyword(kw) => {
                        lhs =
                            Node::new(NodeBase::Member(Box::new(lhs), kw.to_str().to_owned()), pos)
                    }
                    _ => {
                        return Err(Error::Expect(pos_, "expect identifier".to_string()));
                    }
                },
                Kind::Symbol(Symbol::OpeningBoxBracket) => {
                    let idx = self.read_expression()?;
                    if !self.lexer.next_if(Kind::Symbol(Symbol::ClosingBoxBracket)) {
                        return Err(Error::Expect(
                            self.lexer.get_current_pos(),
                            "expect ']'".to_string(),
                        ));
                    }
                    lhs = Node::new(NodeBase::Index(Box::new(lhs), Box::new(idx)), pos);
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
                            self.lexer.get_prev_pos(),
                            "Unexpected token.".to_string(),
                        ));
                    }
                    if self
                        .lexer
                        .next_if_skip_lineterminator(Kind::Symbol(Symbol::ClosingParen))?
                    {
                        break;
                    }
                }
                Ok(_) => {
                    if args.len() != 0 {
                        return Err(Error::Expect(
                            self.lexer.get_prev_pos(),
                            "expect ',' or ')'.".to_string(),
                        ));
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
            Kind::Keyword(Keyword::This) => Ok(Node::new(NodeBase::This, tok.pos)),
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
                Ok(Node::new(NodeBase::Boolean(true), tok.pos))
            }
            Kind::Identifier(ref i) if i == "false" => {
                Ok(Node::new(NodeBase::Boolean(false), tok.pos))
            }
            // Kind::Identifier(ref i) if i == "undefined" => {
            //     Ok(Node::new(NodeBase::Undefined, tok.pos))
            // }
            Kind::Identifier(ref i) if i == "null" => Ok(Node::new(NodeBase::Null, tok.pos)),
            Kind::Identifier(ident) => Ok(Node::new(NodeBase::Identifier(ident), tok.pos)),
            Kind::String(s) => Ok(Node::new(NodeBase::String(s), tok.pos)),
            Kind::Number(num) => Ok(Node::new(NodeBase::Number(num), tok.pos)),
            _ => Err(Error::UnexpectedToken(
                tok.pos,
                format!("unexpected token."),
            )),
        }
    }

    /// https://www.ecma-international.org/ecma-262/6.0/#sec-arrow-function-definitions
    fn read_arrow_function(&mut self, is_parenthesized_param: bool) -> Result<Node, Error> {
        let params;
        let params_pos = self.lexer.get_current_pos();
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
        let body = if self
            .lexer
            .next_if_skip_lineterminator(Kind::Symbol(Symbol::OpeningBrace))?
        {
            self.read_block()?
        } else {
            let pos = self.lexer.get_current_pos();
            Node::new(
                NodeBase::Return(Some(Box::new(self.read_assignment_expression()?))),
                pos,
            )
        };
        Ok(Node::new(
            NodeBase::ArrowFunction(params, Box::new(body)),
            params_pos,
        ))
    }

    /// https://tc39.github.io/ecma262/#prod-FunctionDeclaration
    fn read_function_expression(&mut self) -> Result<Node, Error> {
        let pos = self.lexer.get_current_pos();
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
            pos,
        ))
    }

    /// https://tc39.github.io/ecma262/#prod-ArrayLiteral
    fn read_array_literal(&mut self) -> Result<Node, Error> {
        let pos = self.lexer.get_current_pos();
        let mut elements = vec![];

        loop {
            // TODO: Support all features.
            while self.lexer.next_if(Kind::Symbol(Symbol::Comma)) {
                elements.push(Node::new(NodeBase::Nope, pos));
            }

            if self.lexer.next_if(Kind::Symbol(Symbol::ClosingBoxBracket)) {
                break;
            }

            if self.lexer.is_empty() {
                return Err(Error::UnexpectedEOF("']' may be needed".to_string()));
            }

            if self
                .lexer
                .next_if_skip_lineterminator(Kind::Symbol(Symbol::Spread))?
            {
                let node = self.read_assignment_expression()?;
                let pos = node.pos;
                elements.push(Node::new(NodeBase::Spread(Box::new(node)), pos));
            } else {
                elements.push(self.read_assignment_expression()?);
            }
            self.lexer.next_if(Kind::Symbol(Symbol::Comma));
        }

        Ok(Node::new(NodeBase::Array(elements), pos))
    }

    /// https://tc39.github.io/ecma262/#prod-ObjectLiteral
    fn read_object_literal(&mut self) -> Result<Node, Error> {
        let pos = self.lexer.get_current_pos();
        let mut elements = vec![];

        loop {
            if self
                .lexer
                .next_if_skip_lineterminator(Kind::Symbol(Symbol::ClosingBrace))?
            {
                break;
            }

            elements.push(self.read_property_definition()?);

            if self
                .lexer
                .next_if_skip_lineterminator(Kind::Symbol(Symbol::ClosingBrace))?
            {
                break;
            }

            if !self
                .lexer
                .next_if_skip_lineterminator(Kind::Symbol(Symbol::Comma))?
            {
                return Err(Error::Expect(
                    self.lexer.get_current_pos(),
                    "expect ',' or '}'.".to_string(),
                ));
            }
        }

        Ok(Node::new(NodeBase::Object(elements), pos))
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

        if self
            .lexer
            .next_if_skip_lineterminator(Kind::Symbol(Symbol::Spread))?
        {
            let node = self.read_assignment_expression()?;
            return Ok(PropertyDefinition::SpreadObject(node));
        }

        let tok = self.lexer.next_skip_lineterminator()?;

        if self
            .lexer
            .next_if_skip_lineterminator(Kind::Symbol(Symbol::Colon))?
        {
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
            tok.pos,
            "Expect property definition.".to_string(),
        ))
    }
}

impl Parser {
    /// https://tc39.github.io/ecma262/#prod-ReturnStatement
    fn read_return_statement(&mut self) -> Result<Node, Error> {
        let pos = self.lexer.get_prev_pos();

        // no LineTerminator here
        if self.lexer.next_if(Kind::LineTerminator) {
            return Ok(Node::new(NodeBase::Return(None), pos));
        }

        if self.lexer.next_if(Kind::Symbol(Symbol::Semicolon)) {
            return Ok(Node::new(NodeBase::Return(None), pos));
        }

        if self.lexer.peek(0)?.kind == Kind::Symbol(Symbol::ClosingBrace) {
            return Ok(Node::new(NodeBase::Return(None), pos));
        }

        let expr = self.read_expression()?;
        self.lexer.next_if(Kind::Symbol(Symbol::Semicolon));

        Ok(Node::new(NodeBase::Return(Some(Box::new(expr))), pos))
    }
}

macro_rules! skip_symbol_or_error {
    ($lexer: expr, $symbol: path) => {
        if !$lexer.next_if_skip_lineterminator(Kind::Symbol($symbol))? {
            return Err(Error::UnexpectedToken(
                $lexer.get_current_pos(),
                format!("expected {}", get_string_for_symbol($symbol)),
            ));
        };
    };
}

impl Parser {
    /// http://www.ecma-international.org/ecma-262/9.0/index.html#sec-try-statement
    fn read_try_statement(&mut self) -> Result<Node, Error> {
        let pos_try = self.lexer.get_prev_pos();
        skip_symbol_or_error!(self.lexer, Symbol::OpeningBrace);
        let try_clause = self.read_block_statement()?;
        let is_catch = self
            .lexer
            .next_if_skip_lineterminator(Kind::Keyword(Keyword::Catch))
            .unwrap_or(false);
        let pos_catch = self.lexer.get_current_pos();
        let (catch, param) = if is_catch {
            skip_symbol_or_error!(self.lexer, Symbol::OpeningParen);
            // TODO: should accept BindingPattern
            let pos_param = self.lexer.get_current_pos();
            let catch_param = match self.lexer.next()?.kind {
                Kind::Identifier(s) => Node::new(NodeBase::Identifier(s), pos_param),
                _ => {
                    return Err(Error::UnexpectedToken(
                        pos_param,
                        "expected identifier.".to_string(),
                    ));
                }
            };
            skip_symbol_or_error!(self.lexer, Symbol::ClosingParen);
            skip_symbol_or_error!(self.lexer, Symbol::OpeningBrace);
            (self.read_block()?, catch_param)
        } else {
            (
                Node::new(NodeBase::Nope, pos_catch),
                Node::new(NodeBase::Nope, pos_catch),
            )
        };
        let is_finally = self
            .lexer
            .next_if_skip_lineterminator(Kind::Keyword(Keyword::Finally))
            .unwrap_or(false);
        let pos_finally = self.lexer.get_current_pos();
        let finally = if is_finally {
            skip_symbol_or_error!(self.lexer, Symbol::OpeningBrace);
            self.read_block_statement()?
        } else {
            Node::new(NodeBase::Nope, pos_finally)
        };

        Ok(Node::new(
            NodeBase::Try(
                Box::new(try_clause),
                Box::new(catch),
                Box::new(param),
                Box::new(finally),
            ),
            pos_try,
        ))
    }
}

impl Parser {
    /// https://tc39.github.io/ecma262/#prod-ThrowStatement
    fn read_throw_statement(&mut self) -> Result<Node, Error> {
        let pos_throw = self.lexer.get_prev_pos();
        let pos = self.lexer.get_current_pos();

        // no LineTerminator here
        if self.lexer.next_if(Kind::LineTerminator) {
            return Err(Error::General(
                pos,
                "Illegal new line after throw".to_string(),
            ));
        }

        if self.lexer.next_if(Kind::Symbol(Symbol::Semicolon)) {
            return Err(Error::UnexpectedToken(
                pos,
                "Unexpected token ;".to_string(),
            ));
        }

        if self.lexer.peek(0)?.kind == Kind::Symbol(Symbol::ClosingBrace) {
            return Err(Error::UnexpectedToken(
                pos,
                "Unexpected token }".to_string(),
            ));
        }

        let expr = self.read_expression()?;
        self.lexer.next_if(Kind::Symbol(Symbol::Semicolon));

        Ok(Node::new(NodeBase::Throw(Box::new(expr)), pos_throw))
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
        let pos = self.lexer.get_current_pos();
        let var_kind = if is_const {
            VarKind::Const
        } else {
            VarKind::Let
        };

        let mut list = vec![];

        loop {
            let pos = self.lexer.get_current_pos();
            let name = match self.lexer.next_skip_lineterminator()?.kind {
                Kind::Identifier(name) => name,
                _ => {
                    return Err(Error::UnexpectedToken(
                        self.lexer.get_prev_pos(),
                        "Expect identifier.".to_string(),
                    ));
                }
            };

            if self
                .lexer
                .next_if_skip_lineterminator(Kind::Symbol(Symbol::Assign))?
            {
                let init = Some(Box::new(self.read_initializer()?));
                let decl = NodeBase::VarDecl(name, init, var_kind);
                list.push(Node::new(decl, pos))
            } else {
                list.push(Node::new(NodeBase::VarDecl(name, None, var_kind), pos))
            }

            if !self.variable_declaration_continuation()? {
                break;
            }
        }

        Ok(Node::new(NodeBase::StatementList(list), pos))
    }

    /// https://tc39.github.io/ecma262/#prod-FunctionDeclaration
    fn read_function_declaration(&mut self) -> Result<Node, Error> {
        let pos = self.lexer.get_prev_pos();
        let name = if let Kind::Identifier(name) = self.lexer.next_skip_lineterminator()?.kind {
            name
        } else {
            return Err(Error::Expect(
                self.lexer.get_prev_pos(),
                "expect function name".to_string(),
            ));
        };

        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let params = self.read_formal_parameters()?;

        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");

        let body = self.read_block()?;

        Ok(Node::new(
            NodeBase::FunctionDecl(name, params, Box::new(body)),
            pos,
        ))
    }

    fn read_formal_parameters(&mut self) -> Result<FormalParameters, Error> {
        if self
            .lexer
            .next_if_skip_lineterminator(Kind::Symbol(Symbol::ClosingParen))?
        {
            return Ok(vec![]);
        }

        let mut params = vec![];

        loop {
            let mut rest_param = false;

            params.push(
                if self
                    .lexer
                    .next_if_skip_lineterminator(Kind::Symbol(Symbol::Spread))?
                {
                    rest_param = true;
                    self.read_function_rest_parameter()?
                } else {
                    self.read_formal_parameter()?
                },
            );

            if self.lexer.next_if(Kind::Symbol(Symbol::ClosingParen)) {
                break;
            }

            if rest_param {
                return Err(Error::UnexpectedToken(
                    self.lexer.get_current_pos(),
                    "rest parameter must be the last formal parameter".to_string(),
                ));
            }

            expect!(self, Kind::Symbol(Symbol::Comma), "expect ','");
        }

        Ok(params)
    }

    // TODO: Support all features: https://tc39.github.io/ecma262/#prod-FormalParameter
    fn read_formal_parameter(&mut self) -> Result<FormalParameter, Error> {
        let pos = self.lexer.get_current_pos();
        let name = if let Kind::Identifier(name) = self.lexer.next_skip_lineterminator()?.kind {
            name
        } else {
            return Err(Error::Expect(
                pos,
                "expect identifier (unsupported feature)".to_string(),
            ));
        };
        // TODO: Implement initializer.
        Ok(FormalParameter::new(name, None, false))
    }

    fn read_function_rest_parameter(&mut self) -> Result<FormalParameter, Error> {
        let pos = self.lexer.get_current_pos();
        Ok(FormalParameter::new(
            if let Kind::Identifier(name) = self.lexer.next()?.kind {
                name
            } else {
                return Err(Error::Expect(
                    pos,
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
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(NodeBase::Number(12345.0), 0)]),
            0
        )
    );
}

#[test]
fn string() {
    let mut parser = Parser::new("test", "\"aaa\"".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(NodeBase::String("aaa".to_string()), 0)]),
            0
        )
    );
}

#[test]
fn boolean() {
    let mut parser = Parser::new("test", "true; false".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![
                Node::new(NodeBase::Boolean(true), 0),
                Node::new(NodeBase::Boolean(false), 6)
            ]),
            0
        )
    );
}

#[test]
fn identifier() {
    let mut parser = Parser::new("test", "variable".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::Identifier("variable".to_string()),
                0,
            )]),
            0
        )
    );
}

#[test]
fn array1() {
    let mut parser = Parser::new("test", "[1, 2]".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::Array(vec![
                    Node::new(NodeBase::Number(1.0), 1),
                    Node::new(NodeBase::Number(2.0), 4),
                ]),
                1,
            )]),
            0
        )
    );
    for input in ["[1,2,"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn array2() {
    let mut parser = Parser::new("test", "[]".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(NodeBase::Array(vec![]), 1)]),
            0
        )
    );
}

#[test]
fn array3() {
    let mut parser = Parser::new("test", "[,,]".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::Array(vec![
                    Node::new(NodeBase::Nope, 1),
                    Node::new(NodeBase::Nope, 1),
                ]),
                1,
            )]),
            0
        )
    );
}

#[test]
fn object() {
    let mut parser = Parser::new("test", "a = {x: 123, 1.2: 456}".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::Assign(
                    Box::new(Node::new(NodeBase::Identifier("a".to_string()), 0)),
                    Box::new(Node::new(
                        NodeBase::Object(vec![
                            PropertyDefinition::Property(
                                "x".to_string(),
                                Node::new(NodeBase::Number(123.0), 8),
                            ),
                            PropertyDefinition::Property(
                                "1.2".to_string(),
                                Node::new(NodeBase::Number(456.0), 18),
                            ),
                        ]),
                        5,
                    )),
                ),
                0,
            )]),
            0
        )
    );
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
    use rapidus_ast::BinOp;

    let mut parser = Parser::new("test", "31 + 26 / 3 - 1 * 20 % 3".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::BinaryOp(
                    Box::new(Node::new(
                        NodeBase::BinaryOp(
                            Box::new(Node::new(NodeBase::Number(31.0), 0)),
                            Box::new(Node::new(
                                NodeBase::BinaryOp(
                                    Box::new(Node::new(NodeBase::Number(26.0), 5)),
                                    Box::new(Node::new(NodeBase::Number(3.0), 10)),
                                    BinOp::Div,
                                ),
                                10,
                            )),
                            BinOp::Add,
                        ),
                        5,
                    )),
                    Box::new(Node::new(
                        NodeBase::BinaryOp(
                            Box::new(Node::new(
                                NodeBase::BinaryOp(
                                    Box::new(Node::new(NodeBase::Number(1.0), 14)),
                                    Box::new(Node::new(NodeBase::Number(20.0), 18)),
                                    BinOp::Mul,
                                ),
                                18,
                            )),
                            Box::new(Node::new(NodeBase::Number(3.0), 23)),
                            BinOp::Rem,
                        ),
                        23,
                    )),
                    BinOp::Sub,
                ),
                14,
            )]),
            0
        )
    );
}

#[test]
fn simple_expr_eq() {
    for (input, op, last_pos) in [
        ("1 + 2 == 3", BinOp::Eq, 9),
        ("1 + 2 != 3", BinOp::Ne, 9),
        ("1 + 2 === 3", BinOp::SEq, 10),
        ("1 + 2 !== 3", BinOp::SNe, 10),
    ]
    .iter()
    {
        let mut parser = Parser::new("test", input.to_string());
        assert_eq!(
            Node::new(
                NodeBase::StatementList(vec![Node::new(
                    NodeBase::BinaryOp(
                        Box::new(Node::new(
                            NodeBase::BinaryOp(
                                Box::new(Node::new(NodeBase::Number(1.0), 0)),
                                Box::new(Node::new(NodeBase::Number(2.0), 4)),
                                BinOp::Add,
                            ),
                            4,
                        )),
                        Box::new(Node::new(NodeBase::Number(3.0), *last_pos)),
                        op.clone(),
                    ),
                    *last_pos,
                )]),
                0
            ),
            parser.parse_all().unwrap()
        );
    }
}

#[test]
fn simple_expr_rel() {
    //1 5 3 9 7 0
    for (input, op, last_pos) in [
        ("1 + 2 < 3", BinOp::Lt, 8),
        ("1 + 2 > 3", BinOp::Gt, 8),
        ("1 + 2 <= 3", BinOp::Le, 9),
        ("1 + 2 >= 3", BinOp::Ge, 9),
    ]
    .iter()
    {
        let mut parser = Parser::new("test", input.to_string());
        assert_eq!(
            Node::new(
                NodeBase::StatementList(vec![Node::new(
                    NodeBase::BinaryOp(
                        Box::new(Node::new(
                            NodeBase::BinaryOp(
                                Box::new(Node::new(NodeBase::Number(1.0), 0)),
                                Box::new(Node::new(NodeBase::Number(2.0), 4)),
                                BinOp::Add,
                            ),
                            4,
                        )),
                        Box::new(Node::new(NodeBase::Number(3.0), *last_pos)),
                        op.clone(),
                    ),
                    *last_pos,
                )]),
                0
            ),
            parser.parse_all().unwrap(),
        );
    }
}

#[test]
fn simple_expr_cond() {
    use rapidus_ast::BinOp;

    let mut parser = Parser::new("test", "n == 1 ? 2 : max".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::TernaryOp(
                    Box::new(Node::new(
                        NodeBase::BinaryOp(
                            Box::new(Node::new(NodeBase::Identifier("n".to_string()), 0)),
                            Box::new(Node::new(NodeBase::Number(1.0), 5)),
                            BinOp::Eq,
                        ),
                        5,
                    )),
                    Box::new(Node::new(NodeBase::Number(2.0), 9)),
                    Box::new(Node::new(NodeBase::Identifier("max".to_string()), 13)),
                ),
                0,
            )]),
            0
        )
    );
}

#[test]
fn simple_expr_logical_or() {
    use rapidus_ast::BinOp;

    for (input, op) in [("1 || 0", BinOp::LOr), ("1 && 0", BinOp::LAnd)].iter() {
        let mut parser = Parser::new("test", input.to_string());
        assert_eq!(
            parser.parse_all().unwrap(),
            Node::new(
                NodeBase::StatementList(vec![Node::new(
                    NodeBase::BinaryOp(
                        Box::new(Node::new(NodeBase::Number(1.0), 0)),
                        Box::new(Node::new(NodeBase::Number(0.0), 5)),
                        op.clone(),
                    ),
                    5,
                )]),
                0
            )
        );
    }
}

#[test]
fn simple_expr_bitwise_and() {
    use rapidus_ast::BinOp;

    for (input, op) in [
        ("1 & 3", BinOp::And),
        ("1 ^ 3", BinOp::Xor),
        ("1 | 3", BinOp::Or),
    ]
    .iter()
    {
        let mut parser = Parser::new("test", input.to_string());
        assert_eq!(
            Node::new(
                NodeBase::StatementList(vec![Node::new(
                    NodeBase::BinaryOp(
                        Box::new(Node::new(NodeBase::Number(1.0), 0)),
                        Box::new(Node::new(NodeBase::Number(3.0), 4)),
                        op.clone(),
                    ),
                    4,
                )]),
                0
            ),
            parser.parse_all().unwrap(),
        );
    }
}

#[test]
fn simple_expr_shift() {
    use rapidus_ast::BinOp;

    for (input, op) in [
        ("1 << 2", BinOp::Shl),
        ("1 >> 2", BinOp::Shr),
        ("1 >>> 2", BinOp::ZFShr),
    ]
    .iter()
    {
        let mut parser = Parser::new("test", input.to_string());
        assert_eq!(
            Node::new(
                NodeBase::StatementList(vec![Node::new(
                    NodeBase::BinaryOp(
                        Box::new(Node::new(NodeBase::Number(1.0), 0)),
                        Box::new(Node::new(
                            NodeBase::Number(2.0),
                            if op == &BinOp::ZFShr { 6 } else { 5 },
                        )),
                        op.clone(),
                    ),
                    if op == &BinOp::ZFShr { 6 } else { 5 },
                )]),
                0
            ),
            parser.parse_all().unwrap(),
        );
    }
}

#[test]
fn simple_expr_exp() {
    for input in ["20**50**70"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        assert_eq!(
            parser.parse_all().unwrap(),
            Node::new(
                NodeBase::StatementList(vec![Node::new(
                    NodeBase::BinaryOp(
                        Box::new(Node::new(NodeBase::Number(20.0), 0)),
                        Box::new(Node::new(
                            NodeBase::BinaryOp(
                                Box::new(Node::new(NodeBase::Number(50.0), 4)),
                                Box::new(Node::new(NodeBase::Number(70.0), 8)),
                                BinOp::Exp,
                            ),
                            4,
                        )),
                        BinOp::Exp,
                    ),
                    0,
                )]),
                0
            )
        );
    }
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
fn simple_expr_unary() {
    for (input, op, pos, pos2) in [
        ("delete a", UnaryOp::Delete, 7, 0),
        ("void a", UnaryOp::Void, 5, 0),
        ("typeof a", UnaryOp::Typeof, 7, 0),
        ("+a", UnaryOp::Plus, 1, 0),
        ("-a", UnaryOp::Minus, 1, 0),
        ("~a", UnaryOp::BitwiseNot, 1, 0),
        ("!a", UnaryOp::Not, 1, 0),
        ("++a", UnaryOp::PrInc, 2, 0),
        ("--a", UnaryOp::PrDec, 2, 0),
        ("a++", UnaryOp::PoInc, 0, 0),
        ("a--", UnaryOp::PoDec, 0, 0),
    ]
    .iter()
    {
        let mut parser = Parser::new("test", input.to_string());
        assert_eq!(
            parser.parse_all().unwrap(),
            Node::new(
                NodeBase::StatementList(vec![Node::new(
                    NodeBase::UnaryOp(
                        Box::new(Node::new(NodeBase::Identifier("a".to_string()), *pos)),
                        op.clone(),
                    ),
                    *pos2,
                )]),
                0
            ),
        );
    }
}

#[test]
#[rustfmt::skip]
fn simple_expr_assign() {
    let mut parser = Parser::new("test","v = 1".to_string());
    macro_rules! f { ($expr:expr) => {
        assert_eq!(
            Node::new(NodeBase::StatementList(vec![Node::new(NodeBase::Assign(
                Box::new(Node::new(NodeBase::Identifier("v".to_string()), 0)), Box::new($expr)
            ), 0)]), 0),
            parser.parse_all().unwrap()
        );
    } }
    f!(Node::new(NodeBase::Number(1.0), 4));
    parser = Parser::new("test","v += 1".to_string());
    f!(Node::new(NodeBase::BinaryOp(Box::new(Node::new(NodeBase::Identifier("v".to_string()), 0)), 
                                    Box::new(Node::new(NodeBase::Number(1.0), 5)), BinOp::Add), 0));
    parser = Parser::new("test","v -= 1".to_string());
    f!(Node::new(NodeBase::BinaryOp(Box::new(Node::new(NodeBase::Identifier("v".to_string()), 0)), 
                                    Box::new(Node::new(NodeBase::Number(1.0), 5)), BinOp::Sub), 0));
    parser = Parser::new("test","v *= 1".to_string());
    f!(Node::new(NodeBase::BinaryOp(Box::new(Node::new(NodeBase::Identifier("v".to_string()), 0)), 
                                    Box::new(Node::new(NodeBase::Number(1.0), 5)), BinOp::Mul), 0));
    parser = Parser::new("test","v /= 1".to_string());
    f!(Node::new(NodeBase::BinaryOp(Box::new(Node::new(NodeBase::Identifier("v".to_string()), 0)), 
                                    Box::new(Node::new(NodeBase::Number(1.0), 5)), BinOp::Div), 0));
    parser = Parser::new("test","v %= 1".to_string());
    f!(Node::new(NodeBase::BinaryOp(Box::new(Node::new(NodeBase::Identifier("v".to_string()), 0)), 
                                    Box::new(Node::new(NodeBase::Number(1.0), 5)), BinOp::Rem), 0));
}

#[test]
fn simple_expr_new() {
    let mut parser = Parser::new("test", "new f(1)".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::New(Box::new(Node::new(
                    NodeBase::Call(
                        Box::new(Node::new(NodeBase::Identifier("f".to_string()), 4)),
                        vec![Node::new(NodeBase::Number(1.0), 6)],
                    ),
                    4,
                ))),
                0,
            )]),
            0
        ),
    );
}

#[test]
fn simple_expr_parentheses() {
    let mut parser = Parser::new("test", "2 * (1 + 3)".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::BinaryOp(
                    Box::new(Node::new(NodeBase::Number(2.0), 0)),
                    Box::new(Node::new(
                        NodeBase::BinaryOp(
                            Box::new(Node::new(NodeBase::Number(1.0), 5)),
                            Box::new(Node::new(NodeBase::Number(3.0), 9)),
                            BinOp::Add,
                        ),
                        9,
                    )),
                    BinOp::Mul,
                ),
                4,
            )]),
            0
        )
    );
}

#[test]
fn call() {
    for (input, args) in [
        ("f()", vec![]),
        ("f(1, 2, 3)", vec![(1, 2), (2, 5), (3, 8)]),
        ("f(1, 2,)", vec![(1, 2), (2, 5)]),
    ]
    .iter()
    {
        let mut parser = Parser::new("test", input.to_string());
        assert_eq!(
            parser.parse_all().unwrap(),
            Node::new(
                NodeBase::StatementList(vec![Node::new(
                    NodeBase::Call(
                        Box::new(Node::new(NodeBase::Identifier("f".to_string()), 0)),
                        args.iter()
                            .map(|(n, pos)| Node::new(NodeBase::Number(*n as f64), *pos))
                            .collect(),
                    ),
                    0,
                )]),
                0
            )
        );
    }
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
fn member() {
    for (input, node) in [
        (
            "a.b.c",
            Node::new(
                NodeBase::Member(
                    Box::new(Node::new(
                        NodeBase::Member(
                            Box::new(Node::new(NodeBase::Identifier("a".to_string()), 0)),
                            "b".to_string(),
                        ),
                        0,
                    )),
                    "c".to_string(),
                ),
                0,
            ),
        ),
        (
            "console.log",
            Node::new(
                NodeBase::Member(
                    Box::new(Node::new(NodeBase::Identifier("console".to_string()), 0)),
                    "log".to_string(),
                ),
                0,
            ),
        ),
    ]
    .iter()
    {
        let mut parser = Parser::new("test", input.to_string());
        assert_eq!(
            parser.parse_all().unwrap(),
            Node::new(NodeBase::StatementList(vec![node.clone()]), 0)
        );
    }
}

#[test]
fn var_decl() {
    let mut parser = Parser::new("test", "var a, b = 21".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::StatementList(vec![
                    Node::new(NodeBase::VarDecl("a".to_string(), None, VarKind::Var), 4),
                    Node::new(
                        NodeBase::VarDecl(
                            "b".to_string(),
                            Some(Box::new(Node::new(NodeBase::Number(21.0), 11))),
                            VarKind::Var,
                        ),
                        7,
                    ),
                ]),
                0,
            )]),
            0
        )
    );
    for input in ["var 7"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn block() {
    let mut parser = Parser::new("test", "{ a=1 }".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::Block(vec![Node::new(
                    NodeBase::Assign(
                        Box::new(Node::new(NodeBase::Identifier("a".to_string()), 2)),
                        Box::new(Node::new(NodeBase::Number(1.0), 4)),
                    ),
                    2,
                )]),
                0,
            )]),
            0
        )
    );
    for input in ["{", "{ a", "{ a=", "{ a=1", "}", "{ 7z }", "{a=0 8k}"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn break_() {
    let mut parser = Parser::new("test", "while(1){break}".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::While(
                    Box::new(Node::new(NodeBase::Number(1.0), 6)),
                    Box::new(Node::new(
                        NodeBase::Block(vec![Node::new(NodeBase::Break(None), 9)]),
                        8,
                    )),
                ),
                0,
            )]),
            0
        )
    );
    for input in ["while(1){break 7}"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn continue_() {
    let mut parser = Parser::new("test", "while(1){continue}".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::While(
                    Box::new(Node::new(NodeBase::Number(1.0), 6)),
                    Box::new(Node::new(
                        NodeBase::Block(vec![Node::new(NodeBase::Continue(None), 9)]),
                        8,
                    )),
                ),
                0,
            )]),
            0
        )
    );
    for input in ["while(1){continue 825}"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn return_() {
    for (input, node) in [
        (
            "return 1",
            Node::new(
                NodeBase::Return(Some(Box::new(Node::new(NodeBase::Number(1.0), 7)))),
                0,
            ),
        ),
        ("return;", Node::new(NodeBase::Return(None), 0)),
    ]
    .iter()
    {
        let mut parser = Parser::new("test", input.to_string());
        assert_eq!(
            parser.parse_all().unwrap(),
            Node::new(NodeBase::StatementList(vec![node.clone()]), 0)
        );
    }
}

#[test]
fn if_() {
    use rapidus_ast::BinOp;

    let mut parser = Parser::new(
        "test",
        "if (x <= 2) 
            then_stmt 
        else 
            else_stmt"
            .to_string(),
    );
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::If(
                    Box::new(Node::new(
                        NodeBase::BinaryOp(
                            Box::new(Node::new(NodeBase::Identifier("x".to_string()), 4)),
                            Box::new(Node::new(NodeBase::Number(2.0), 9)),
                            BinOp::Le,
                        ),
                        9,
                    )),
                    Box::new(Node::new(NodeBase::Identifier("then_stmt".to_string()), 25)),
                    Box::new(Node::new(NodeBase::Identifier("else_stmt".to_string()), 62)),
                ),
                0,
            )]),
            0
        )
    );

    parser = Parser::new("test", "if (x <= 2) then_stmt ".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::If(
                    Box::new(Node::new(
                        NodeBase::BinaryOp(
                            Box::new(Node::new(NodeBase::Identifier("x".to_string()), 4)),
                            Box::new(Node::new(NodeBase::Number(2.0), 9)),
                            BinOp::Le,
                        ),
                        9,
                    )),
                    Box::new(Node::new(NodeBase::Identifier("then_stmt".to_string()), 12)),
                    Box::new(Node::new(NodeBase::Nope, 22)),
                ),
                0,
            )]),
            0
        )
    );

    for input in ["if(", "if()else", "if(true){} 8j"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn while_() {
    let mut parser = Parser::new("test", "while (true) { }".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::While(
                    Box::new(Node::new(NodeBase::Boolean(true), 7)),
                    Box::new(Node::new(NodeBase::Block(vec![]), 13)),
                ),
                0,
            )]),
            0
        )
    );
}

#[test]
fn for1() {
    let mut parser = Parser::new("test", "for (;;) { }".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::Block(vec![Node::new(
                    NodeBase::For(
                        Box::new(Node::new(NodeBase::Nope, 4)),
                        Box::new(Node::new(NodeBase::Boolean(true), 6)),
                        Box::new(Node::new(NodeBase::Nope, 7)),
                        Box::new(Node::new(NodeBase::Block(vec![]), 9)),
                    ),
                    0,
                )]),
                0,
            )]),
            0
        )
    );
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
fn function_decl() {
    for (input, node) in [
        (
            "function
            f
            (
            ) 
            { 
            }",
            Node::new(
                NodeBase::FunctionDecl(
                    "f".to_string(),
                    vec![],
                    Box::new(Node::new(NodeBase::StatementList(vec![]), 64)),
                ),
                0,
            ),
        ),
        (
            "function f() { return }",
            Node::new(
                NodeBase::FunctionDecl(
                    "f".to_string(),
                    vec![],
                    Box::new(Node::new(
                        NodeBase::StatementList(vec![Node::new(NodeBase::Return(None), 15)]),
                        13,
                    )),
                ),
                0,
            ),
        ),
        (
            "function f(x, y, ...z) { return x + y }",
            Node::new(
                NodeBase::FunctionDecl(
                    "f".to_string(),
                    vec![
                        FormalParameter::new("x".to_string(), None, false),
                        FormalParameter::new("y".to_string(), None, false),
                        FormalParameter::new("z".to_string(), None, true),
                    ],
                    Box::new(Node::new(
                        NodeBase::StatementList(vec![Node::new(
                            NodeBase::Return(Some(Box::new(Node::new(
                                NodeBase::BinaryOp(
                                    Box::new(Node::new(NodeBase::Identifier("x".to_string()), 32)),
                                    Box::new(Node::new(NodeBase::Identifier("y".to_string()), 36)),
                                    BinOp::Add,
                                ),
                                36,
                            )))),
                            25,
                        )]),
                        23,
                    )),
                ),
                0,
            ),
        ),
    ]
    .iter()
    {
        let mut parser = Parser::new("test", input.to_string());
        assert_eq!(
            parser.parse_all().unwrap(),
            Node::new(NodeBase::StatementList(vec![node.clone()]), 0)
        );
    }
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
fn arrow_function() {
    for (input, node) in [
        (
            "(a, b) => { return a + b }",
            Node::new(
                NodeBase::ArrowFunction(
                    vec![
                        FormalParameter {
                            name: "a".to_string(),
                            init: None,
                            is_rest_param: false,
                        },
                        FormalParameter {
                            name: "b".to_string(),
                            init: None,
                            is_rest_param: false,
                        },
                    ],
                    Box::new(Node::new(
                        NodeBase::StatementList(vec![Node::new(
                            NodeBase::Return(Some(Box::new(Node::new(
                                NodeBase::BinaryOp(
                                    Box::new(Node::new(NodeBase::Identifier("a".to_string()), 19)),
                                    Box::new(Node::new(NodeBase::Identifier("b".to_string()), 23)),
                                    BinOp::Add,
                                ),
                                23,
                            )))),
                            12,
                        )]),
                        10,
                    )),
                ),
                0,
            ),
        ),
        (
            "(a, b, ...c) => a",
            Node::new(
                NodeBase::ArrowFunction(
                    vec![
                        FormalParameter {
                            name: "a".to_string(),
                            init: None,
                            is_rest_param: false,
                        },
                        FormalParameter {
                            name: "b".to_string(),
                            init: None,
                            is_rest_param: false,
                        },
                        FormalParameter {
                            name: "c".to_string(),
                            init: None,
                            is_rest_param: true,
                        },
                    ],
                    Box::new(Node::new(
                        NodeBase::Return(Some(Box::new(Node::new(
                            NodeBase::Identifier("a".to_string()),
                            16,
                        )))),
                        16,
                    )),
                ),
                0,
            ),
        ),
    ]
    .iter()
    {
        let mut parser = Parser::new("test", input.to_string());
        assert_eq!(
            parser.parse_all().unwrap(),
            Node::new(NodeBase::StatementList(vec![node.clone()]), 0)
        );
    }
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
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::FunctionDecl(
                    "f".to_string(),
                    vec![],
                    Box::new(Node::new(
                        NodeBase::StatementList(vec![
                            Node::new(NodeBase::Return(None), 38),
                            Node::new(NodeBase::Block(vec![]), 59),
                        ]),
                        23,
                    )),
                ),
                0,
            )]),
            0
        )
    )
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
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![
                Node::new(
                    NodeBase::Assign(
                        Box::new(Node::new(NodeBase::Identifier("b".to_string()), 9)),
                        Box::new(Node::new(NodeBase::Identifier("a".to_string()), 13)),
                    ),
                    9,
                ),
                Node::new(
                    NodeBase::UnaryOp(
                        Box::new(Node::new(NodeBase::Identifier("b".to_string()), 25)),
                        UnaryOp::PrInc,
                    ),
                    23,
                ),
            ]),
            0
        )
    )
}
#[test]
fn throw() {
    let mut parser = Parser::new("test", "throw 10".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::Throw(Box::new(Node::new(NodeBase::Number(10.0), 6))),
                0
            )]),
            0
        )
    );
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
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::Try(
                    Box::new(Node::new(NodeBase::Block(vec![]), 4)),
                    Box::new(Node::new(NodeBase::StatementList(vec![]), 15)),
                    Box::new(Node::new(NodeBase::Identifier("e".to_string()), 13)),
                    Box::new(Node::new(NodeBase::Block(vec![]), 25))
                ),
                0
            )]),
            0
        )
    );
    for input in ["try {} catch", "try {} catch {}", "try {} catch(7)"].iter() {
        let mut parser = Parser::new("test", input.to_string());
        parser.parse_all().expect_err("should be error");
    }
}

#[test]
fn try_catch2() {
    let mut parser = Parser::new("test", "try {} catch(e){}".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::Try(
                    Box::new(Node::new(NodeBase::Block(vec![]), 4)),
                    Box::new(Node::new(NodeBase::StatementList(vec![]), 15)),
                    Box::new(Node::new(NodeBase::Identifier("e".to_string()), 13)),
                    Box::new(Node::new(NodeBase::Nope, 17))
                ),
                0
            )]),
            0
        )
    );
}

#[test]
fn try_catch3() {
    let mut parser = Parser::new("test", "try {} finally {}".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::Try(
                    Box::new(Node::new(NodeBase::Block(vec![]), 4)),
                    Box::new(Node::new(NodeBase::Nope, 7)),
                    Box::new(Node::new(NodeBase::Nope, 7)),
                    Box::new(Node::new(NodeBase::Block(vec![]), 15)),
                ),
                0
            )]),
            0
        )
    );
}
