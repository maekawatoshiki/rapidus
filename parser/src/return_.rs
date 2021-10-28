use super::{expect, Parser};
use rapidus_ast::{loc::SourceLoc, Node, NodeBase};
use rapidus_lexer::token::{Keyword, Kind, Symbol};
use rapidus_lexer::Error;

impl Parser {
    /// <https://tc39.github.io/ecma262/#prod-ReturnStatement>
    pub(super) fn read_return_statement(&mut self, loc: SourceLoc) -> Result<Node, Error> {
        // no LineTerminator here
        if self.lexer.next_if(Kind::LineTerminator) {
            return Ok(Node::new(NodeBase::Return(None), loc));
        }

        if self.lexer.next_if(Kind::Symbol(Symbol::Semicolon)) {
            return Ok(Node::new(NodeBase::Return(None), loc));
        }

        if self.lexer.peek(0)?.kind == Kind::Symbol(Symbol::ClosingBrace) {
            return Ok(Node::new(NodeBase::Return(None), loc));
        }

        let expr = self.read_expression()?;
        self.lexer.next_if(Kind::Symbol(Symbol::Semicolon));

        Ok(Node::new(NodeBase::Return(Some(Box::new(expr))), loc))
    }
}
