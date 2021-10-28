use super::{expect, Parser};
use rapidus_ast::{loc::SourceLoc, Node, NodeBase};
use rapidus_lexer::token::{Kind, Symbol};
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
}
