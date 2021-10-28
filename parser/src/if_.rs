use super::{expect, Parser};
use rapidus_ast::{loc::SourceLoc, Node, NodeBase};
use rapidus_lexer::token::{Keyword, Kind, Symbol};
pub use rapidus_lexer::Error;

impl Parser {
    pub(super) fn read_if_statement(&mut self, loc: SourceLoc) -> Result<Node, Error> {
        expect!(self, Kind::Symbol(Symbol::OpeningParen), "expect '('");

        let cond = self.read_expression()?;

        expect!(self, Kind::Symbol(Symbol::ClosingParen), "expect ')'");

        let thn = self.read_statement()?;
        let els = self.read_else_if_any()?;

        Ok(Node::new(
            NodeBase::If(Box::new(cond), Box::new(thn), Box::new(els)),
            loc,
        ))
    }

    fn read_else_if_any(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();

        if self.lexer.skip(Keyword::Else).unwrap_or(false) {
            return self.read_statement();
        }

        Ok(Node::new(NodeBase::Nope, loc))
    }
}
