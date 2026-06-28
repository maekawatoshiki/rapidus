use super::{expect, Parser};
use rapidus_ast::{loc::SourceLoc, Node, NodeBase, VarKind};
use rapidus_lexer::token::{Keyword, Kind, Symbol, Token};
use rapidus_lexer::Error;

impl Parser {
    pub(super) fn read_for_statement(&mut self, loc: SourceLoc) -> Result<Node, Error> {
        fn is_uninitialized_pattern_decl(node: &Node) -> bool {
            matches!(node.base, NodeBase::VarDeclPattern(_, None, _))
        }

        let is_for_await = if self.allow_await {
            match self.lexer.peek_skip_lineterminator()? {
                Token {
                    kind: Kind::Identifier(ref name),
                    ..
                } if name == "await" => {
                    self.lexer.next_skip_lineterminator()?;
                    true
                }
                _ => false,
            }
        } else {
            false
        };

        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let init = match self.lexer.peek_skip_lineterminator()? {
            Token {
                kind: Kind::Keyword(Keyword::Var),
                loc,
                ..
            } => {
                assert_eq!(
                    self.lexer.next_skip_lineterminator()?.kind,
                    Kind::Keyword(Keyword::Var)
                );
                let first_decl = self.with_allow_in(false, |parser| {
                    parser.read_variable_declaration_with_kind_allow_pattern_without_init(
                        VarKind::Var,
                        true,
                    )
                })?;
                if self.lexer.skip(Kind::Keyword(Keyword::In))? {
                    if is_for_await {
                        return Err(Error::Expect(loc, "expect 'of'".to_string()));
                    }
                    let right = self.read_expression()?;
                    expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");
                    let body = self.read_statement()?;
                    return Ok(Node::new(
                        NodeBase::ForIn(Box::new(first_decl), Box::new(right), Box::new(body)),
                        loc,
                    ));
                }
                if self.skip_contextual_of()? {
                    let right = self.read_expression()?;
                    expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");
                    let body = self.read_statement()?;
                    return Ok(Node::new(
                        NodeBase::ForOf(Box::new(first_decl), Box::new(right), Box::new(body)),
                        loc,
                    ));
                }
                if is_for_await {
                    return Err(Error::Expect(loc, "expect 'of'".to_string()));
                }
                if is_uninitialized_pattern_decl(&first_decl) {
                    return Err(Error::Expect(
                        loc,
                        "destructuring declaration requires initializer".to_string(),
                    ));
                }

                let mut list = vec![first_decl];
                while self.variable_declaration_continuation()? {
                    list.push(
                        self.with_allow_in(false, |parser| parser.read_variable_declaration())?,
                    );
                }
                Node::new(NodeBase::StatementList(list), loc)
            }
            Token {
                kind: Kind::Keyword(Keyword::Let) | Kind::Keyword(Keyword::Const),
                loc,
                ..
            } => {
                let var_kind = match self.lexer.next_skip_lineterminator()?.kind {
                    Kind::Keyword(Keyword::Let) => VarKind::Let,
                    Kind::Keyword(Keyword::Const) => VarKind::Const,
                    _ => unreachable!(),
                };
                let first_decl = self.with_allow_in(false, |parser| {
                    parser.read_variable_declaration_with_kind_allow_pattern_without_init(
                        var_kind, true,
                    )
                })?;
                if self.lexer.skip(Kind::Keyword(Keyword::In))? {
                    if is_for_await {
                        return Err(Error::Expect(loc, "expect 'of'".to_string()));
                    }
                    let right = self.read_expression()?;
                    expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");
                    let body = self.read_statement()?;
                    return Ok(Node::new(
                        NodeBase::ForIn(Box::new(first_decl), Box::new(right), Box::new(body)),
                        loc,
                    ));
                }
                if self.skip_contextual_of()? {
                    let right = self.read_expression()?;
                    expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");
                    let body = self.read_statement()?;
                    return Ok(Node::new(
                        NodeBase::ForOf(Box::new(first_decl), Box::new(right), Box::new(body)),
                        loc,
                    ));
                }
                if is_for_await {
                    return Err(Error::Expect(loc, "expect 'of'".to_string()));
                }
                if is_uninitialized_pattern_decl(&first_decl) {
                    return Err(Error::Expect(
                        loc,
                        "destructuring declaration requires initializer".to_string(),
                    ));
                }

                let mut list = vec![first_decl];
                while self.variable_declaration_continuation()? {
                    list.push(self.with_allow_in(false, |parser| {
                        parser.read_variable_declaration_with_kind(var_kind)
                    })?);
                }
                Node::new(NodeBase::StatementList(list), loc)
            }
            Token {
                kind: Kind::Symbol(Symbol::Semicolon),
                loc,
                ..
            } => Node::new(NodeBase::Nope, loc),
            _ => {
                let left = self.with_allow_in(false, |parser| parser.read_expression())?;
                if self.lexer.skip(Kind::Keyword(Keyword::In))? {
                    if is_for_await {
                        return Err(Error::Expect(loc, "expect 'of'".to_string()));
                    }
                    let left = self.expression_to_assignment_pattern(left)?;
                    let right = self.read_expression()?;
                    expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");
                    let body = self.read_statement()?;
                    return Ok(Node::new(
                        NodeBase::ForIn(Box::new(left), Box::new(right), Box::new(body)),
                        loc,
                    ));
                }
                if self.skip_contextual_of()? {
                    let left = self.expression_to_assignment_pattern(left)?;
                    let right = self.read_expression()?;
                    expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");
                    let body = self.read_statement()?;
                    return Ok(Node::new(
                        NodeBase::ForOf(Box::new(left), Box::new(right), Box::new(body)),
                        loc,
                    ));
                }
                if is_for_await {
                    return Err(Error::Expect(loc, "expect 'of'".to_string()));
                }
                left
            }
        };
        if is_for_await {
            return Err(Error::Expect(loc, "expect 'of'".to_string()));
        }
        expect!(self, Kind::Symbol(Symbol::Semicolon), "expect ';'");

        let loc_ = self.lexer.get_current_loc();
        let cond = if self
            .lexer
            .skip(Kind::Symbol(Symbol::Semicolon))
            .unwrap_or(false)
        {
            Node::new(NodeBase::Boolean(true), loc_)
        } else {
            let step = self.read_expression()?;
            expect!(self, Kind::Symbol(Symbol::Semicolon), "expect ';'");
            step
        };

        let loc_ = self.lexer.get_current_loc();
        let step = if self
            .lexer
            .skip(Kind::Symbol(Symbol::ClosingParen))
            .unwrap_or(false)
        {
            Node::new(NodeBase::Nope, loc_)
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
            loc,
        );

        Ok(Node::new(NodeBase::Block(vec![for_node]), loc))
    }

    fn skip_contextual_of(&mut self) -> Result<bool, Error> {
        match self.lexer.peek_skip_lineterminator()? {
            Token {
                kind: Kind::Identifier(ref name),
                ..
            } if name == "of" => {
                self.lexer.next_skip_lineterminator()?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }
}
