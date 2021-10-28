use super::{expect, Parser};
use rapidus_ast::{Node, NodeBase};
use rapidus_lexer::token::{Kind, Symbol};
pub use rapidus_lexer::Error;

impl Parser {
    pub(super) fn read_while_statement(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();

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
