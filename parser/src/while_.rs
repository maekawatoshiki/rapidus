use super::{expect, Parser};
use rapidus_ast::{loc::SourceLoc, Node, NodeBase};
use rapidus_lexer::token::{Keyword, Kind, Symbol};
use rapidus_lexer::Error;

impl Parser {
    pub(super) fn read_while_statement(&mut self, loc: SourceLoc) -> Result<Node, Error> {
        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let cond = self.read_expression()?;

        expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");

        let body = self.read_statement()?;

        Ok(Node::new(
            NodeBase::While(Box::new(cond), Box::new(body)),
            loc,
        ))
    }

    pub(super) fn read_do_while_statement(&mut self, loc: SourceLoc) -> Result<Node, Error> {
        let body = self.read_statement()?;

        expect!(self, Kind::Keyword(Keyword::While), "expect 'while'");
        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let cond = self.read_expression()?;

        expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");
        let _ = self.lexer.skip(Symbol::Semicolon);

        Ok(Node::new(
            NodeBase::DoWhile(Box::new(body), Box::new(cond)),
            loc,
        ))
    }
}
