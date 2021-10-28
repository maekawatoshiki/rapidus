use super::Parser;
use rapidus_ast::{loc::SourceLoc, Node, NodeBase};
use rapidus_lexer::token::{Keyword, Kind, Symbol};
pub use rapidus_lexer::Error;

impl Parser {
    pub(super) fn read_if_statement(&mut self, loc: SourceLoc) -> Result<Node, Error> {
        let oparen = self.lexer.next_skip_lineterminator()?;
        if oparen.kind != Kind::Symbol(Symbol::OpeningParen) {
            return Err(Error::Expect(oparen.loc, "expect '('".to_string()));
        }
        let cond = self.read_expression()?;
        let cparen = self.lexer.next_skip_lineterminator()?;
        if cparen.kind != Kind::Symbol(Symbol::ClosingParen) {
            return Err(Error::Expect(cparen.loc, "expect ')'".to_string()));
        }

        let thn = self.read_statement()?;
        let els = self.read_else_if_any()?;

        Ok(Node::new(
            NodeBase::If(Box::new(cond), Box::new(thn), Box::new(els)),
            loc,
        ))
    }

    fn read_else_if_any(&mut self) -> Result<Node, Error> {
        let loc = self.lexer.get_current_loc();

        if let Ok(maybe_else) = self.lexer.next_skip_lineterminator() {
            if maybe_else.kind == Kind::Keyword(Keyword::Else) {
                return self.read_statement();
            }
            self.lexer.unget();
        }

        Ok(Node::new(NodeBase::Nope, loc))
    }
}
