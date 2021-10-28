use super::{expect, Parser};
use rapidus_ast::{loc::SourceLoc, Node, NodeBase};
use rapidus_lexer::token::{Keyword, Kind, Symbol, Token};
use rapidus_lexer::Error;

impl Parser {
    pub(super) fn read_for_statement(&mut self, loc: SourceLoc) -> Result<Node, Error> {
        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let init = match self.lexer.peek_skip_lineterminator()? {
            Token {
                kind: Kind::Keyword(Keyword::Var),
                loc,
            } => {
                assert_eq!(self.lexer.next()?.kind, Kind::Keyword(Keyword::Var));
                self.read_variable_declaration_list(loc)?
            }
            Token {
                kind: Kind::Keyword(Keyword::Let) | Kind::Keyword(Keyword::Const),
                ..
            } => self.read_declaration()?,
            Token {
                kind: Kind::Symbol(Symbol::Semicolon),
                loc,
            } => Node::new(NodeBase::Nope, loc),
            _ => self.read_expression()?,
        };
        expect!(self, Kind::Symbol(Symbol::Semicolon), "expect ';'");

        let loc_ = self.lexer.get_current_loc();
        let cond = if self.lexer.next_if(Kind::Symbol(Symbol::Semicolon)) {
            Node::new(NodeBase::Boolean(true), loc_)
        } else {
            let step = self.read_expression()?;
            expect!(self, Kind::Symbol(Symbol::Semicolon), "expect ';'");
            step
        };

        let loc_ = self.lexer.get_current_loc();
        let step = if self.lexer.next_if(Kind::Symbol(Symbol::ClosingParen)) {
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
}
