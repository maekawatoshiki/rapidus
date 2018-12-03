use lexer;
use lexer::ErrorMsgKind;
use node::{BinOp, FormalParameter, FormalParameters, Node, NodeBase, PropertyDefinition, UnaryOp};
use token::{Keyword, Kind, Symbol, Token};

use ansi_term::Colour;

macro_rules! token_start_pos {
    ($var:ident, $lexer:expr) => {
        let $var = $lexer.pos;
    };
}

macro_rules! expect {
    ($self:ident, $kind:expr, $msg:expr) => {{
        let tok = $self.lexer.next_except_lineterminator()?;
        if tok.kind != $kind {
            return Err(Error::Expect(
                tok.pos,
                ErrorMsgKind::Normal,
                $msg.to_string(),
            ));
        }
    }};
}

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    NormalEOF,
    UnexpectedEOF(usize, ErrorMsgKind, String), // position, error msg kind, error msg
    UnexpectedToken(usize, ErrorMsgKind, String), // position
    UnsupportedFeature(usize),                  // position
    Expect(usize, ErrorMsgKind, String),        // position, error msg kind, error msg
}

fn proper_error_msg_kind(self_: &Parser, tok_pos: usize) -> ErrorMsgKind {
    let mut last_line = 0;
    for (pos, line) in &self_.lexer.pos_line_list {
        if tok_pos == *pos {
            if last_line != *line {
                return ErrorMsgKind::LastToken;
            } else {
                break;
            }
        }
        last_line = *line;
    }
    ErrorMsgKind::Normal
}

#[derive(Clone, Debug)]
pub struct Parser {
    pub lexer: lexer::Lexer,
}

impl Parser {
    pub fn new(code: String) -> Parser {
        Parser {
            lexer: lexer::Lexer::new(code),
        }
    }

    pub fn show_error_at(&self, pos: usize, kind: ErrorMsgKind, msg: &str) {
        let (source_at_err_point, pos) = self.lexer.get_code_around_err_point(pos, kind);
        eprintln!(
            "{}({}): {}\n{}",
            Colour::Red.bold().paint("error"),
            self.lexer
                .pos_line_list
                .iter()
                .find(|(p, _)| *p >= pos)
                .and_then(|x| Some(x.1 /*=line number*/))
                .unwrap_or(self.lexer.line),
            msg,
            source_at_err_point,
        );
    }

    pub fn enhanced_show_error_at(&self, tok_pos: usize, msg: &str) {
        self.show_error_at(
            tok_pos,
            // If tok_pos's line is different from the previous token's line,
            // show the previous token's line.
            // e.g.
            // ```
            // function f() {
            //  1 +
            // }
            // ```
            // The error (unexpected token) occurs at ``}`` in the last line (3rd line), but we had
            // better show the 2nd line containing ``1 +``.
            if || -> bool {
                let mut last_line = 0;
                for (pos, line) in &self.lexer.pos_line_list {
                    if tok_pos == *pos {
                        if last_line != *line {
                            return true;
                        } else {
                            break;
                        }
                    }
                    last_line = *line;
                }
                false
            }() {
                ErrorMsgKind::LastToken
            } else {
                ErrorMsgKind::Normal
            },
            msg,
        )
    }
}

impl Parser {
    pub fn parse_all(&mut self) -> Result<Node, Error> {
        self.read_script()
        //     Ok(ok) => Ok(ok),
        //     Err(Error::NormalEOF) => unreachable!(),
        //     // TODO: Show an appropriate error message depending on the kind of _e.
        //     Err(_e) => self.show_error_at(
        //         self.lexer.pos_line_list.last().unwrap().0,
        //         ErrorMsgKind::Normal,
        //         "reach unexpected EOF",
        //     ),
        // }
    }
}

impl Parser {
    fn read_script(&mut self) -> Result<Node, Error> {
        self.read_statement_list(false)
    }
}

impl Parser {
    fn read_statement_list(&mut self, break_when_closingbrase: bool) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);
        let mut items = vec![];

        loop {
            if self
                .lexer
                .skip_except_lineterminator(Kind::Symbol(Symbol::ClosingBrace))
            {
                if break_when_closingbrase {
                    break;
                }
            }

            if self.lexer.buf.is_empty() && self.lexer.eof() {
                if break_when_closingbrase {
                    return Err(Error::UnexpectedEOF(
                        self.lexer.pos_line_list.last().unwrap().0,
                        ErrorMsgKind::LastToken,
                        "reach unexpected EOF".to_string(),
                    ));
                } else {
                    break;
                }
            }

            match self.read_statement_list_item() {
                Ok(ok) => items.push(ok),
                Err(Error::NormalEOF) => {
                    return Err(Error::UnexpectedEOF(
                        self.lexer.pos_line_list.last().unwrap().0,
                        ErrorMsgKind::LastToken,
                        "reach unexpected EOF".to_string(),
                    ));
                }
                Err(e) => return Err(e),
            }

            while self
                .lexer
                .skip_except_lineterminator(Kind::Symbol(Symbol::Semicolon))
            {}
        }

        Ok(Node::new(NodeBase::StatementList(items), pos))
    }

    fn read_statement_list_item(&mut self) -> Result<Node, Error> {
        if self.is_declaration() {
            self.read_declaration()
        } else {
            self.read_statement()
        }
    }

    fn read_statement(&mut self) -> Result<Node, Error> {
        let tok = self.lexer.next_except_lineterminator()?;

        // Label
        if let Kind::Identifier(ref name) = tok.kind {
            let save_pos = self.lexer.pos;
            let maybe_colon = self.lexer.next_except_lineterminator();
            if let Ok(Token {
                kind: Kind::Symbol(Symbol::Colon),
                ..
            }) = maybe_colon
            {
                // TODO: https://tc39.github.io/ecma262/#prod-LabelledStatement
                let labeled_item = self.read_statement_list_item()?;
                return Ok(Node::new(
                    NodeBase::Label(name.clone(), Box::new(labeled_item)),
                    tok.pos,
                ));
            } else {
                self.lexer.pos = save_pos;
            }
        }

        let stmt = match tok.kind {
            Kind::Keyword(Keyword::If) => self.read_if_statement(),
            Kind::Keyword(Keyword::Var) => self.read_variable_statement(),
            Kind::Keyword(Keyword::While) => self.read_while_statement(),
            Kind::Keyword(Keyword::For) => self.read_for_statement(),
            Kind::Keyword(Keyword::Return) => self.read_return_statement(),
            Kind::Keyword(Keyword::Break) => self.read_break_statement(),
            Kind::Keyword(Keyword::Continue) => self.read_continue_statement(),
            Kind::Symbol(Symbol::OpeningBrace) => self.read_block_statement(),
            _ => {
                self.lexer.unget(&tok);
                self.read_expression_statement()
            }
        };

        self.lexer
            .skip_except_lineterminator(Kind::Symbol(Symbol::Semicolon));
        stmt
    }
}

impl Parser {
    /// https://tc39.github.io/ecma262/#prod-BlockStatement
    fn read_block_statement(&mut self) -> Result<Node, Error> {
        self.read_statement_list(true)
    }
}

impl Parser {
    /// https://tc39.github.io/ecma262/#prod-VariableStatement
    fn read_variable_statement(&mut self) -> Result<Node, Error> {
        self.read_variable_declaration_list()
    }

    /// https://tc39.github.io/ecma262/#prod-VariableDeclarationList
    fn read_variable_declaration_list(&mut self) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);
        let mut list = vec![];

        loop {
            list.push(self.read_variable_declaration()?);
            if !self.lexer.skip(Kind::Symbol(Symbol::Comma)) {
                break;
            }
        }

        Ok(Node::new(NodeBase::StatementList(list), pos))
    }

    /// https://tc39.github.io/ecma262/#prod-VariableDeclaration
    fn read_variable_declaration(&mut self) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);
        let name = match self.lexer.next()?.kind {
            Kind::Identifier(name) => name,
            _ => return Err(Error::UnsupportedFeature(self.lexer.pos)),
        };

        if self.lexer.skip(Kind::Symbol(Symbol::Assign)) {
            Ok(Node::new(
                NodeBase::VarDecl(name, Some(Box::new(self.read_initializer()?))),
                pos,
            ))
        } else {
            Ok(Node::new(NodeBase::VarDecl(name, None), pos))
        }
    }

    /// https://tc39.github.io/ecma262/#prod-Initializer
    fn read_initializer(&mut self) -> Result<Node, Error> {
        self.read_assignment_expression()
    }
}

impl Parser {
    fn read_if_statement(&mut self) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);
        let oparen = self.lexer.next_except_lineterminator()?;
        if oparen.kind != Kind::Symbol(Symbol::OpeningParen) {
            return Err(Error::Expect(
                oparen.pos,
                ErrorMsgKind::LastToken,
                "expect '('".to_string(),
            ));
        }
        let cond = self.read_expression()?;
        let cparen = self.lexer.next_except_lineterminator()?;
        if cparen.kind != Kind::Symbol(Symbol::ClosingParen) {
            return Err(Error::Expect(
                cparen.pos,
                ErrorMsgKind::LastToken,
                "expect ')'".to_string(),
            ));
        }

        let then_ = self.read_statement()?;

        if let Ok(expect_else_tok) = self.lexer.next_except_lineterminator() {
            if expect_else_tok.kind == Kind::Keyword(Keyword::Else) {
                let else_ = self.read_statement()?;
                return Ok(Node::new(
                    NodeBase::If(Box::new(cond), Box::new(then_), Box::new(else_)),
                    pos,
                ));
            } else {
                self.lexer.unget(&expect_else_tok);
            }
        }

        Ok(Node::new(
            NodeBase::If(
                Box::new(cond),
                Box::new(then_),
                Box::new(Node::new(NodeBase::Nope, 0)),
            ),
            pos,
        ))
    }
}

impl Parser {
    fn read_while_statement(&mut self) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);

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
        token_start_pos!(pos, self.lexer);

        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let init = if self.lexer.skip(Kind::Keyword(Keyword::Var)) {
            let init = self.read_variable_statement()?;
            expect!(self, Kind::Symbol(Symbol::Semicolon), "expect ';'");
            init
        } else if self.lexer.skip(Kind::Symbol(Symbol::Semicolon)) {
            Node::new(NodeBase::Nope, 0)
        } else {
            let expr = self.read_expression()?;
            expect!(self, Kind::Symbol(Symbol::Semicolon), "expect ';'");
            expr
        };

        let cond = if self.lexer.skip(Kind::Symbol(Symbol::Semicolon)) {
            Node::new(NodeBase::Boolean(true), 0)
        } else {
            let step = self.read_expression()?;
            expect!(self, Kind::Symbol(Symbol::Semicolon), "expect ';'");
            step
        };

        let step = if self.lexer.skip(Kind::Symbol(Symbol::ClosingParen)) {
            Node::new(NodeBase::Nope, 0)
        } else {
            let step = self.read_expression()?;
            expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");
            step
        };

        let body = self.read_statement()?;

        Ok(Node::new(
            NodeBase::For(
                Box::new(init),
                Box::new(cond),
                Box::new(step),
                Box::new(body),
            ),
            pos,
        ))
    }
}

impl Parser {
    fn read_break_statement(&mut self) -> Result<Node, Error> {
        let pos = self.lexer.pos - "break".len();
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::LineTerminator
            | Kind::Symbol(Symbol::Semicolon)
            | Kind::Symbol(Symbol::ClosingBrace) => {
                self.lexer.unget(&tok);
                Ok(Node::new(NodeBase::Break(None), pos))
            }
            Kind::Identifier(name) => Ok(Node::new(NodeBase::Break(Some(name)), pos)),
            _ => Err(Error::UnexpectedToken(
                tok.pos,
                ErrorMsgKind::Normal,
                "expected ';', identifier or line terminator".to_string(),
            )),
        }
    }

    fn read_continue_statement(&mut self) -> Result<Node, Error> {
        let pos = self.lexer.pos - "continue".len();
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::LineTerminator
            | Kind::Symbol(Symbol::Semicolon)
            | Kind::Symbol(Symbol::ClosingBrace) => {
                self.lexer.unget(&tok);
                Ok(Node::new(NodeBase::Continue(None), pos))
            }
            Kind::Identifier(name) => Ok(Node::new(NodeBase::Continue(Some(name)), pos)),
            _ => Err(Error::UnexpectedToken(
                tok.pos,
                ErrorMsgKind::Normal,
                "expected ';', identifier or line terminator".to_string(),
            )),
        }
    }
}

macro_rules! expression { ( $name:ident, $lower:ident, [ $( $op:path ),* ] ) => {
    fn $name (&mut self) -> Result<Node, Error> {
        let mut lhs = self. $lower ()?;
        while let Ok(tok) = self.lexer.next() {
            token_start_pos!(pos, self.lexer);
            match tok.kind {
                Kind::Symbol(ref op) if $( op == &$op )||* => {
                    lhs = Node::new(NodeBase::BinaryOp(
                        Box::new(lhs),
                        Box::new(self. $lower ()?),
                        op.as_binop().unwrap(),
                    ), pos);
                }
                _ => { self.lexer.unget(&tok); break }
            }
        }
        Ok(lhs)
    }
} }

impl Parser {
    fn read_expression_statement(&mut self) -> Result<Node, Error> {
        self.read_expression()
    }

    /// https://tc39.github.io/ecma262/#prod-Expression
    expression!(read_expression, read_assignment_expression, [Symbol::Comma]);

    /// https://tc39.github.io/ecma262/#prod-AssignmentExpression
    // TODO: Implement all features.
    fn read_assignment_expression(&mut self) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);
        let mut lhs = self.read_conditional_expression()?;
        if let Ok(tok) = self.lexer.next() {
            macro_rules! assignop {
                ($op:ident) => {{
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
                        pos,
                    );
                }};
            }
            match tok.kind {
                Kind::Symbol(Symbol::Assign) => {
                    lhs = Node::new(
                        NodeBase::Assign(
                            Box::new(lhs),
                            Box::new(self.read_assignment_expression()?),
                        ),
                        pos,
                    )
                }
                Kind::Symbol(Symbol::AssignAdd) => assignop!(Add),
                Kind::Symbol(Symbol::AssignSub) => assignop!(Sub),
                Kind::Symbol(Symbol::AssignMul) => assignop!(Mul),
                Kind::Symbol(Symbol::AssignDiv) => assignop!(Div),
                Kind::Symbol(Symbol::AssignMod) => assignop!(Rem),
                _ => self.lexer.unget(&tok),
            }
        }
        Ok(lhs)
    }

    /// https://tc39.github.io/ecma262/#prod-ConditionalExpression
    fn read_conditional_expression(&mut self) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);

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
                _ => self.lexer.unget(&tok),
            }
        }

        Ok(lhs)
    }

    /// https://tc39.github.io/ecma262/#prod-LogicalORExpression
    expression!(
        read_logical_or_expression,
        read_logical_and_expression,
        [Symbol::LOr]
    );

    /// https://tc39.github.io/ecma262/#prod-LogicalANDExpression
    expression!(
        read_logical_and_expression,
        read_bitwise_or_expression,
        [Symbol::LAnd]
    );

    /// https://tc39.github.io/ecma262/#prod-BitwiseORExpression
    expression!(
        read_bitwise_or_expression,
        read_bitwise_xor_expression,
        [Symbol::Or]
    );

    /// https://tc39.github.io/ecma262/#prod-BitwiseXORExpression
    expression!(
        read_bitwise_xor_expression,
        read_bitwise_and_expression,
        [Symbol::Xor]
    );

    /// https://tc39.github.io/ecma262/#prod-BitwiseANDExpression
    expression!(
        read_bitwise_and_expression,
        read_equality_expression,
        [Symbol::And]
    );

    /// https://tc39.github.io/ecma262/#prod-EqualityExpression
    expression!(
        read_equality_expression,
        read_relational_expression,
        [Symbol::Eq, Symbol::Ne, Symbol::SEq, Symbol::SNe]
    );

    /// https://tc39.github.io/ecma262/#prod-RelationalExpression
    expression!(
        read_relational_expression,
        read_shift_expression,
        [Symbol::Lt, Symbol::Gt, Symbol::Le, Symbol::Ge]
    );

    /// https://tc39.github.io/ecma262/#prod-ShiftExpression
    expression!(
        read_shift_expression,
        read_additive_expression,
        [Symbol::Shl, Symbol::Shr, Symbol::ZFShr]
    );

    /// https://tc39.github.io/ecma262/#prod-AdditiveExpression
    expression!(
        read_additive_expression,
        read_multiplicate_expression,
        [Symbol::Add, Symbol::Sub]
    );

    /// https://tc39.github.io/ecma262/#prod-MultiplicativeExpression
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
        token_start_pos!(pos, self.lexer);
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
                self.lexer.unget(&tok);
            }
        }
        Ok(lhs)
    }

    fn is_unary_expression(&mut self) -> bool {
        match self.lexer.peek() {
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
        token_start_pos!(pos, self.lexer);
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
                self.lexer.unget(&tok);
                self.read_update_expression()
            }
        }
    }

    /// https://tc39.github.io/ecma262/#prod-UpdateExpression
    // TODO: Implement all features.
    fn read_update_expression(&mut self) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::Symbol(Symbol::Inc) => {
                return Ok(Node::new(
                    NodeBase::UnaryOp(
                        Box::new(self.read_left_hand_side_expression()?),
                        UnaryOp::PrInc,
                    ),
                    pos,
                ));
            }
            Kind::Symbol(Symbol::Dec) => {
                return Ok(Node::new(
                    NodeBase::UnaryOp(
                        Box::new(self.read_left_hand_side_expression()?),
                        UnaryOp::PrDec,
                    ),
                    pos,
                ));
            }
            _ => self.lexer.unget(&tok),
        }

        token_start_pos!(pos, self.lexer);
        let e = self.read_left_hand_side_expression()?;
        if let Ok(tok) = self.lexer.next() {
            match tok.kind {
                Kind::Symbol(Symbol::Inc) => {
                    return Ok(Node::new(
                        NodeBase::UnaryOp(Box::new(e), UnaryOp::PoInc),
                        pos,
                    ));
                }
                Kind::Symbol(Symbol::Dec) => {
                    return Ok(Node::new(
                        NodeBase::UnaryOp(Box::new(e), UnaryOp::PoDec),
                        pos,
                    ));
                }
                _ => self.lexer.unget(&tok),
            }
        }

        Ok(e)
    }

    /// https://tc39.github.io/ecma262/#prod-LeftHandSideExpression
    // TODO: Implement all features.
    fn read_left_hand_side_expression(&mut self) -> Result<Node, Error> {
        let lhs = self.read_new_expression()?;

        Ok(lhs)
    }

    /// https://tc39.github.io/ecma262/#prod-NewExpression
    fn read_new_expression(&mut self) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);
        if self.lexer.skip(Kind::Keyword(Keyword::New)) {
            Ok(Node::new(
                NodeBase::New(Box::new(self.read_new_expression()?)),
                pos,
            ))
        } else {
            self.read_call_expression()
        }
    }

    /// https://tc39.github.io/ecma262/#prod-CallExpression
    // TODO: Implement all features.
    fn read_call_expression(&mut self) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);
        let mut lhs = self.read_primary_expression()?;
        let mut lineterminator = false;

        while let Ok(tok) = self.lexer.next() {
            let pos_ = self.lexer.pos;

            match tok.kind {
                Kind::Symbol(Symbol::OpeningParen) => {
                    let args = self.read_arguments()?;
                    lhs = Node::new(NodeBase::Call(Box::new(lhs), args), pos)
                }
                Kind::Symbol(Symbol::Point) => {
                    match self.lexer.next_except_lineterminator()?.kind {
                        Kind::Identifier(name) => {
                            lhs = Node::new(NodeBase::Member(Box::new(lhs), name), pos)
                        }
                        _ => {
                            return Err(Error::Expect(
                                pos_,
                                ErrorMsgKind::Normal,
                                "expect identifier".to_string(),
                            ));
                        }
                    }
                }
                Kind::Symbol(Symbol::OpeningBoxBracket) => {
                    let idx = self.read_expression()?;
                    if !self.lexer.skip(Kind::Symbol(Symbol::ClosingBoxBracket)) {
                        return Err(Error::Expect(
                            self.lexer.pos,
                            ErrorMsgKind::Normal,
                            "expect ']'".to_string(),
                        ));
                    }
                    lhs = Node::new(NodeBase::Index(Box::new(lhs), Box::new(idx)), pos);
                }
                Kind::LineTerminator => lineterminator = true,
                _ => {
                    if lineterminator {
                        self.lexer.unget(&Token::new_line_terminator(0));
                    }
                    self.lexer.unget(&tok);
                    break;
                }
            }
        }

        Ok(lhs)
    }

    fn read_arguments(&mut self) -> Result<Vec<Node>, Error> {
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::Symbol(Symbol::ClosingParen) => return Ok(vec![]),
            _ => {
                self.lexer.unget(&tok);
            }
        }

        let mut args = vec![];
        let mut pos = 0;
        loop {
            match self.lexer.next() {
                Ok(ref tok) if tok.kind == Kind::Symbol(Symbol::ClosingParen) => break,
                Ok(tok) => {
                    pos = tok.pos;
                    self.lexer.unget(&tok)
                }
                Err(_) => {
                    return Err(Error::UnexpectedEOF(
                        pos,
                        ErrorMsgKind::LastToken,
                        "reach unexpected EOF".to_string(),
                    ));
                }
            }

            args.push(self.read_assignment_expression()?);

            match self.lexer.next() {
                Ok(ref tok) if tok.kind == Kind::Symbol(Symbol::Comma) => pos = tok.pos,
                Ok(tok) => {
                    pos = tok.pos;
                    self.lexer.unget(&tok)
                }
                Err(_) => {
                    return Err(Error::UnexpectedEOF(
                        pos,
                        ErrorMsgKind::LastToken,
                        "reach unexpected EOF".to_string(),
                    ));
                }
            }
        }

        Ok(args)
    }

    /// https://tc39.github.io/ecma262/#prod-PrimaryExpression
    fn read_primary_expression(&mut self) -> Result<Node, Error> {
        let tok = self.lexer.next()?;

        match tok.kind {
            Kind::Keyword(Keyword::This) => Ok(Node::new(NodeBase::This, tok.pos)),
            Kind::Keyword(Keyword::Arguments) => Ok(Node::new(NodeBase::Arguments, tok.pos)),
            Kind::Keyword(Keyword::Function) => self.read_function_expression(),
            Kind::Symbol(Symbol::OpeningParen) => {
                let x = self.read_expression();
                expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");
                x
            }
            Kind::Symbol(Symbol::OpeningBoxBracket) => self.read_array_literal(),
            Kind::Symbol(Symbol::OpeningBrace) => self.read_object_literal(),
            Kind::Identifier(ref i) if i == "true" => {
                Ok(Node::new(NodeBase::Boolean(true), tok.pos))
            }
            Kind::Identifier(ref i) if i == "false" => {
                Ok(Node::new(NodeBase::Boolean(false), tok.pos))
            }
            Kind::Identifier(ref i) if i == "undefined" => {
                Ok(Node::new(NodeBase::Undefined, tok.pos))
            }
            Kind::Identifier(ref i) if i == "null" => Ok(Node::new(NodeBase::Null, tok.pos)),
            Kind::Identifier(ident) => Ok(Node::new(NodeBase::Identifier(ident), tok.pos)),
            Kind::String(s) => Ok(Node::new(NodeBase::String(s), tok.pos)),
            Kind::Number(num) => Ok(Node::new(NodeBase::Number(num), tok.pos)),
            Kind::LineTerminator => self.read_primary_expression(),
            _ => {
                return Err(Error::UnexpectedToken(
                    tok.pos,
                    proper_error_msg_kind(self, tok.pos),
                    "unexpected token".to_string(),
                ));
            }
        }
    }

    /// https://tc39.github.io/ecma262/#prod-FunctionDeclaration
    fn read_function_expression(&mut self) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);
        let name = if let Kind::Identifier(name) = self.lexer.peek()?.kind {
            self.lexer.next()?;
            Some(name)
        } else {
            None
        };

        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let params = self.read_formal_parameters()?;

        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");

        let body = self.read_statement_list(true)?;

        Ok(Node::new(
            NodeBase::FunctionExpr(name, params, Box::new(body)),
            pos,
        ))
    }

    /// https://tc39.github.io/ecma262/#prod-ArrayLiteral
    fn read_array_literal(&mut self) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);
        let mut elements = vec![];

        loop {
            // TODO: Support all features.
            while self.lexer.skip(Kind::Symbol(Symbol::Comma)) {
                elements.push(Node::new(NodeBase::Nope, pos));
            }

            if self.lexer.skip(Kind::Symbol(Symbol::ClosingBoxBracket)) {
                break;
            }

            if let Ok(elem) = self.read_assignment_expression() {
                elements.push(elem);
            }

            self.lexer.skip(Kind::Symbol(Symbol::Comma));
        }

        Ok(Node::new(NodeBase::Array(elements), pos))
    }

    /// https://tc39.github.io/ecma262/#prod-ObjectLiteral
    fn read_object_literal(&mut self) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);
        let mut elements = vec![];

        loop {
            if self.lexer.skip(Kind::Symbol(Symbol::ClosingBrace)) {
                break;
            }
            if let Ok(elem) = self.read_property_definition() {
                elements.push(elem);
            }
            self.lexer.skip(Kind::Symbol(Symbol::Comma));
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

        let tok = self.lexer.next()?;

        if self.lexer.skip(Kind::Symbol(Symbol::Colon)) {
            let val = self.read_assignment_expression()?;
            return Ok(PropertyDefinition::Property(to_string(tok.kind), val));
        }

        if let Kind::Identifier(name) = tok.kind {
            return Ok(PropertyDefinition::IdentifierReference(name));
        }

        // TODO: Support all features.
        Err(Error::UnsupportedFeature(tok.pos))
    }
}

impl Parser {
    /// https://tc39.github.io/ecma262/#prod-ReturnStatement
    fn read_return_statement(&mut self) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);

        // no LineTerminator here
        if self.lexer.skip(Kind::LineTerminator) {
            return Ok(Node::new(NodeBase::Return(None), pos));
        }

        if self.lexer.skip(Kind::Symbol(Symbol::Semicolon)) {
            return Ok(Node::new(NodeBase::Return(None), pos));
        }

        if self.lexer.peek()?.kind == Kind::Symbol(Symbol::ClosingBrace) {
            return Ok(Node::new(NodeBase::Return(None), pos));
        }

        let expr = self.read_expression()?;
        self.lexer.skip(Kind::Symbol(Symbol::Semicolon));

        Ok(Node::new(NodeBase::Return(Some(Box::new(expr))), pos))
    }
}

impl Parser {
    fn is_declaration(&mut self) -> bool {
        self.is_hoistable_declaration()
    }

    fn read_declaration(&mut self) -> Result<Node, Error> {
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::Keyword(Keyword::Function) => self.read_function_declaration(),
            _ => unreachable!(),
        }
    }

    /// https://tc39.github.io/ecma262/#prod-FunctionDeclaration
    fn read_function_declaration(&mut self) -> Result<Node, Error> {
        token_start_pos!(pos, self.lexer);
        let name = if let Kind::Identifier(name) = self.lexer.next()?.kind {
            name
        } else {
            return Err(Error::Expect(
                pos,
                ErrorMsgKind::Normal,
                "expect function name".to_string(),
            ));
        };

        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let params = self.read_formal_parameters()?;

        expect!(self, Kind::Symbol(Symbol::OpeningBrace), "expect '{'");

        let body = self.read_statement_list(true)?;

        Ok(Node::new(
            NodeBase::FunctionDecl(name, params, Box::new(body)),
            pos,
        ))
    }

    fn read_formal_parameters(&mut self) -> Result<FormalParameters, Error> {
        if self.lexer.skip(Kind::Symbol(Symbol::ClosingParen)) {
            return Ok(vec![]);
        }

        let mut params = vec![];

        loop {
            let mut rest_param = false;

            params.push(if self.lexer.skip(Kind::Symbol(Symbol::Rest)) {
                rest_param = true;
                self.read_function_rest_parameter()?
            } else {
                self.read_formal_parameter()?
            });

            if self.lexer.skip(Kind::Symbol(Symbol::ClosingParen)) {
                break;
            }

            if rest_param {
                return Err(Error::UnexpectedToken(
                    self.lexer.pos,
                    ErrorMsgKind::LastToken,
                    "rest parameter must be the last formal parameter".to_string(),
                ));
            }

            expect!(self, Kind::Symbol(Symbol::Comma), "expect ','");
        }

        Ok(params)
    }

    // TODO: Support all features: https://tc39.github.io/ecma262/#prod-FormalParameter
    fn read_formal_parameter(&mut self) -> Result<FormalParameter, Error> {
        token_start_pos!(pos, self.lexer);
        let name = if let Kind::Identifier(name) = self.lexer.next()?.kind {
            name
        } else {
            return Err(Error::Expect(
                pos,
                ErrorMsgKind::Normal,
                "expect identifier (unsupported feature)".to_string(),
            ));
        };
        // TODO: Implement initializer.
        Ok(FormalParameter::new(name, None, false))
    }

    fn read_function_rest_parameter(&mut self) -> Result<FormalParameter, Error> {
        token_start_pos!(pos, self.lexer);
        Ok(FormalParameter::new(
            if let Kind::Identifier(name) = self.lexer.next()?.kind {
                name
            } else {
                return Err(Error::Expect(
                    pos,
                    ErrorMsgKind::Normal,
                    "expect identifier (unsupported feature)".to_string(),
                ));
            },
            None,
            true,
        ))
    }
}

impl Parser {
    /// https://tc39.github.io/ecma262/#prod-HoistableDeclaration
    fn is_hoistable_declaration(&mut self) -> bool {
        self.is_function_declaration()
    }
}

impl Parser {
    /// https://tc39.github.io/ecma262/#prod-FunctionDeclaration
    fn is_function_declaration(&mut self) -> bool {
        match self.lexer.peek() {
            Ok(tok) => tok.is_the_keyword(Keyword::Function),
            Err(_) => false,
        }
    }
}

#[test]
fn number() {
    let mut parser = Parser::new("12345".to_string());
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
    let mut parser = Parser::new("\"aaa\"".to_string());
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
    let mut parser = Parser::new("true".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(NodeBase::Boolean(true), 0)]),
            0
        )
    );
}

#[test]
fn identifier() {
    let mut parser = Parser::new("variable".to_string());
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
    let mut parser = Parser::new("[1, 2]".to_string());
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
}

#[test]
fn array2() {
    let mut parser = Parser::new("[]".to_string());
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
    let mut parser = Parser::new("[,,]".to_string());
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
    let mut parser = Parser::new("a = {x: 123, 1.2: 456}".to_string());
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
                1,
            )]),
            0
        )
    );
}

#[test]
fn simple_expr_5arith() {
    use node::BinOp;

    let mut parser = Parser::new("31 + 26 / 3 - 1 * 20 % 3".to_string());
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
                                9,
                            )),
                            BinOp::Add,
                        ),
                        4,
                    )),
                    Box::new(Node::new(
                        NodeBase::BinaryOp(
                            Box::new(Node::new(
                                NodeBase::BinaryOp(
                                    Box::new(Node::new(NodeBase::Number(1.0), 14)),
                                    Box::new(Node::new(NodeBase::Number(20.0), 18)),
                                    BinOp::Mul,
                                ),
                                17,
                            )),
                            Box::new(Node::new(NodeBase::Number(3.0), 23)),
                            BinOp::Rem,
                        ),
                        22,
                    )),
                    BinOp::Sub,
                ),
                13,
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
        let mut parser = Parser::new(input.to_string());
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
                            3,
                        )),
                        Box::new(Node::new(NodeBase::Number(3.0), *last_pos)),
                        op.clone(),
                    ),
                    *last_pos - 1,
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
        let mut parser = Parser::new(input.to_string());
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
                            3,
                        )),
                        Box::new(Node::new(NodeBase::Number(3.0), *last_pos)),
                        op.clone(),
                    ),
                    *last_pos - 1,
                )]),
                0
            ),
            parser.parse_all().unwrap(),
        );
    }
}

#[test]
fn simple_expr_cond() {
    use node::BinOp;

    let mut parser = Parser::new("n == 1 ? 2 : max".to_string());
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
                        4,
                    )),
                    Box::new(Node::new(NodeBase::Number(2.0), 9)),
                    Box::new(Node::new(NodeBase::Identifier("max".to_string()), 13)),
                ),
                1,
            )]),
            0
        )
    );
}

#[test]
fn simple_expr_logical_or() {
    use node::BinOp;

    for (input, op) in [("1 || 0", BinOp::LOr), ("1 && 0", BinOp::LAnd)].iter() {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.parse_all().unwrap(),
            Node::new(
                NodeBase::StatementList(vec![Node::new(
                    NodeBase::BinaryOp(
                        Box::new(Node::new(NodeBase::Number(1.0), 0)),
                        Box::new(Node::new(NodeBase::Number(0.0), 5)),
                        op.clone(),
                    ),
                    4,
                )]),
                0
            )
        );
    }
}

#[test]
fn simple_expr_bitwise_and() {
    use node::BinOp;

    for (input, op) in [
        ("1 & 3", BinOp::And),
        ("1 ^ 3", BinOp::Xor),
        ("1 | 3", BinOp::Or),
    ]
    .iter()
    {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            Node::new(
                NodeBase::StatementList(vec![Node::new(
                    NodeBase::BinaryOp(
                        Box::new(Node::new(NodeBase::Number(1.0), 0)),
                        Box::new(Node::new(NodeBase::Number(3.0), 4)),
                        op.clone(),
                    ),
                    3,
                )]),
                0
            ),
            parser.parse_all().unwrap(),
        );
    }
}

#[test]
fn simple_expr_shift() {
    use node::BinOp;

    for (input, op) in [
        ("1 << 2", BinOp::Shl),
        ("1 >> 2", BinOp::Shr),
        ("1 >>> 2", BinOp::ZFShr),
    ]
    .iter()
    {
        let mut parser = Parser::new(input.to_string());
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
                    if op == &BinOp::ZFShr { 5 } else { 4 },
                )]),
                0
            ),
            parser.parse_all().unwrap(),
        );
    }
}

#[test]
fn simple_expr_exp() {
    for input in ["2**5**7"].iter() {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.parse_all().unwrap(),
            Node::new(
                NodeBase::StatementList(vec![Node::new(
                    NodeBase::BinaryOp(
                        Box::new(Node::new(NodeBase::Number(2.0), 0)),
                        Box::new(Node::new(
                            NodeBase::BinaryOp(
                                Box::new(Node::new(NodeBase::Number(5.0), 3)),
                                Box::new(Node::new(NodeBase::Number(7.0), 6)),
                                BinOp::Exp,
                            ),
                            4,
                        )),
                        BinOp::Exp,
                    ),
                    1,
                )]),
                0
            )
        );
    }
}

#[test]
fn simple_expr_unary() {
    for (input, op, pos, pos2) in [
        ("delete a", UnaryOp::Delete, 7, 6),
        ("void a", UnaryOp::Void, 5, 4),
        ("typeof a", UnaryOp::Typeof, 7, 6),
        ("+a", UnaryOp::Plus, 1, 1),
        ("-a", UnaryOp::Minus, 1, 1),
        ("~a", UnaryOp::BitwiseNot, 1, 1),
        ("!a", UnaryOp::Not, 1, 1),
        ("++a", UnaryOp::PrInc, 2, 2),
        ("--a", UnaryOp::PrDec, 2, 2),
        ("a++", UnaryOp::PoInc, 0, 1),
        ("a--", UnaryOp::PoDec, 0, 1),
    ]
    .iter()
    {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
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
            parser.parse_all().unwrap()
        );
    }
}

#[test]
#[rustfmt::skip]
fn simple_expr_assign() {
    let mut parser = Parser::new("v = 1".to_string());
    macro_rules! f { ($expr:expr) => {
        assert_eq!(
            Node::new(NodeBase::StatementList(vec![Node::new(NodeBase::Assign(
                Box::new(Node::new(NodeBase::Identifier("v".to_string()), 0)), Box::new($expr)
            ), 1)]), 0),
            parser.parse_all().unwrap()
        );
    } }
    f!(Node::new(NodeBase::Number(1.0), 4));
    parser = Parser::new("v += 1".to_string());
    f!(Node::new(NodeBase::BinaryOp(Box::new(Node::new(NodeBase::Identifier("v".to_string()), 0)), 
                                    Box::new(Node::new(NodeBase::Number(1.0), 5)), BinOp::Add), 1));
    parser = Parser::new("v -= 1".to_string());
    f!(Node::new(NodeBase::BinaryOp(Box::new(Node::new(NodeBase::Identifier("v".to_string()), 0)), 
                                    Box::new(Node::new(NodeBase::Number(1.0), 5)), BinOp::Sub), 1));
    parser = Parser::new("v *= 1".to_string());
    f!(Node::new(NodeBase::BinaryOp(Box::new(Node::new(NodeBase::Identifier("v".to_string()), 0)), 
                                    Box::new(Node::new(NodeBase::Number(1.0), 5)), BinOp::Mul), 1));
    parser = Parser::new("v /= 1".to_string());
    f!(Node::new(NodeBase::BinaryOp(Box::new(Node::new(NodeBase::Identifier("v".to_string()), 0)), 
                                    Box::new(Node::new(NodeBase::Number(1.0), 5)), BinOp::Div), 1));
    parser = Parser::new("v %= 1".to_string());
    f!(Node::new(NodeBase::BinaryOp(Box::new(Node::new(NodeBase::Identifier("v".to_string()), 0)), 
                                    Box::new(Node::new(NodeBase::Number(1.0), 5)), BinOp::Rem), 1));
}

#[test]
fn simple_expr_new() {
    let mut parser = Parser::new("new f(1)".to_string());
    assert_eq!(
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::New(Box::new(Node::new(
                    NodeBase::Call(
                        Box::new(Node::new(NodeBase::Identifier("f".to_string()), 4)),
                        vec![Node::new(NodeBase::Number(1.0), 6)],
                    ),
                    5,
                ))),
                3,
            )]),
            0
        ),
        parser.parse_all().unwrap(),
    );
}

#[test]
fn simple_expr_parentheses() {
    let mut parser = Parser::new("2 * (1 + 3)".to_string());
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
                        8,
                    )),
                    BinOp::Mul,
                ),
                3,
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
        let mut parser = Parser::new(input.to_string());
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
                    1,
                )]),
                0
            )
        );
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
                        1,
                    )),
                    "c".to_string(),
                ),
                1,
            ),
        ),
        (
            "console.log",
            Node::new(
                NodeBase::Member(
                    Box::new(Node::new(NodeBase::Identifier("console".to_string()), 0)),
                    "log".to_string(),
                ),
                7,
            ),
        ),
    ]
    .iter()
    {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.parse_all().unwrap(),
            Node::new(NodeBase::StatementList(vec![node.clone()]), 0)
        );
    }
}

#[test]
fn var_decl() {
    let mut parser = Parser::new("var a, b = 21".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::StatementList(vec![
                    Node::new(NodeBase::VarDecl("a".to_string(), None), 3),
                    Node::new(
                        NodeBase::VarDecl(
                            "b".to_string(),
                            Some(Box::new(Node::new(NodeBase::Number(21.0), 11))),
                        ),
                        6,
                    ),
                ]),
                3,
            )]),
            0
        )
    );
}

#[test]
fn block() {
    let mut parser = Parser::new("{ a=1 }".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::StatementList(vec![Node::new(
                    NodeBase::Assign(
                        Box::new(Node::new(NodeBase::Identifier("a".to_string()), 2)),
                        Box::new(Node::new(NodeBase::Number(1.0), 4)),
                    ),
                    3,
                )]),
                1,
            )]),
            0
        )
    );
}

#[test]
fn break_() {
    let mut parser = Parser::new("while(1){break}".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::While(
                    Box::new(Node::new(NodeBase::Number(1.0), 6)),
                    Box::new(Node::new(
                        NodeBase::StatementList(vec![Node::new(NodeBase::Break(None), 9)]),
                        9,
                    )),
                ),
                5,
            )]),
            0
        )
    );
}

#[test]
fn continue_() {
    let mut parser = Parser::new("while(1){continue}".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::While(
                    Box::new(Node::new(NodeBase::Number(1.0), 6)),
                    Box::new(Node::new(
                        NodeBase::StatementList(vec![Node::new(NodeBase::Continue(None), 9)]),
                        9,
                    )),
                ),
                5,
            )]),
            0
        )
    );
}

#[test]
fn return_() {
    for (input, node) in [
        (
            "return 1",
            Node::new(
                NodeBase::Return(Some(Box::new(Node::new(NodeBase::Number(1.0), 7)))),
                6,
            ),
        ),
        ("return;", Node::new(NodeBase::Return(None), 6)),
    ]
    .iter()
    {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.parse_all().unwrap(),
            Node::new(NodeBase::StatementList(vec![node.clone()]), 0)
        );
    }
}

#[test]
fn if_() {
    use node::BinOp;

    let mut parser = Parser::new(
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
                        8,
                    )),
                    Box::new(Node::new(NodeBase::Identifier("then_stmt".to_string()), 25)),
                    Box::new(Node::new(NodeBase::Identifier("else_stmt".to_string()), 62)),
                ),
                2,
            )]),
            0
        )
    );

    parser = Parser::new("if (x <= 2) then_stmt ".to_string());
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
                        8,
                    )),
                    Box::new(Node::new(NodeBase::Identifier("then_stmt".to_string()), 12)),
                    Box::new(Node::new(NodeBase::Nope, 0)),
                ),
                2,
            )]),
            0
        )
    );
}

#[test]
fn while_() {
    let mut parser = Parser::new("while (true) { }".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::While(
                    Box::new(Node::new(NodeBase::Boolean(true), 7)),
                    Box::new(Node::new(NodeBase::StatementList(vec![]), 14)),
                ),
                5,
            )]),
            0
        )
    );
}

#[test]
fn for_() {
    let mut parser = Parser::new("for (;;) { }".to_string());
    assert_eq!(
        parser.parse_all().unwrap(),
        Node::new(
            NodeBase::StatementList(vec![Node::new(
                NodeBase::For(
                    Box::new(Node::new(NodeBase::Nope, 0)),
                    Box::new(Node::new(NodeBase::Boolean(true), 0)),
                    Box::new(Node::new(NodeBase::Nope, 0)),
                    Box::new(Node::new(NodeBase::StatementList(vec![]), 10)),
                ),
                3,
            )]),
            0
        )
    );
}

#[test]
fn function_decl() {
    for (input, node) in [
        (
            "function f() { }",
            Node::new(
                NodeBase::FunctionDecl(
                    "f".to_string(),
                    vec![],
                    Box::new(Node::new(NodeBase::StatementList(vec![]), 14)),
                ),
                8,
            ),
        ),
        (
            "function f() { return }",
            Node::new(
                NodeBase::FunctionDecl(
                    "f".to_string(),
                    vec![],
                    Box::new(Node::new(
                        NodeBase::StatementList(vec![Node::new(NodeBase::Return(None), 21)]),
                        14,
                    )),
                ),
                8,
            ),
        ),
        (
            "function f(x, y) { return x + y }",
            Node::new(
                NodeBase::FunctionDecl(
                    "f".to_string(),
                    vec![
                        FormalParameter::new("x".to_string(), None, false),
                        FormalParameter::new("y".to_string(), None, false),
                    ],
                    Box::new(Node::new(
                        NodeBase::StatementList(vec![Node::new(
                            NodeBase::Return(Some(Box::new(Node::new(
                                NodeBase::BinaryOp(
                                    Box::new(Node::new(NodeBase::Identifier("x".to_string()), 26)),
                                    Box::new(Node::new(NodeBase::Identifier("y".to_string()), 30)),
                                    BinOp::Add,
                                ),
                                29,
                            )))),
                            25,
                        )]),
                        18,
                    )),
                ),
                8,
            ),
        ),
    ]
    .iter()
    {
        let mut parser = Parser::new(input.to_string());
        assert_eq!(
            parser.parse_all().unwrap(),
            Node::new(NodeBase::StatementList(vec![node.clone()]), 0)
        );
    }
}

#[test]
fn asi1() {
    let mut parser = Parser::new(
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
                            Node::new(NodeBase::Return(None), 44),
                            Node::new(NodeBase::StatementList(vec![]), 60),
                        ]),
                        24,
                    )),
                ),
                8,
            )]),
            0
        )
    )
}

#[test]
fn asi2() {
    let mut parser = Parser::new(
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
                    10,
                ),
                Node::new(
                    NodeBase::UnaryOp(
                        Box::new(Node::new(NodeBase::Identifier("b".to_string()), 25)),
                        UnaryOp::PrInc,
                    ),
                    25,
                ),
            ]),
            0
        )
    )
}
