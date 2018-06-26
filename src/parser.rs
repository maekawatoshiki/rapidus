use lexer;
use token::{Keyword, Kind, Symbol};

use std::boxed::Box;

#[derive(Clone, Debug)]
pub struct Parser {
    pub lexer: lexer::Lexer,
}

#[derive(Clone, Debug)]
pub enum Node {
    StatementList(Vec<Node>),
    BinOp(Box<Node>, Box<Node>, BinOp),
    Identifier(String),
    Boolean(bool),
    Number(f64),
}

#[derive(Clone, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    LAnd,
    LOr,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Shl,
    Shr,
    Comma,
    Assign,
}

impl Parser {
    pub fn new(code: String) -> Parser {
        Parser {
            lexer: lexer::Lexer::new(code),
        }
    }
}

impl Parser {
    pub fn next(&mut self) -> Result<Node, ()> {
        self.read_script()
    }
}

impl Parser {
    fn read_script(&mut self) -> Result<Node, ()> {
        self.read_statement_list()
    }
}

impl Parser {
    fn read_statement_list(&mut self) -> Result<Node, ()> {
        let mut items = vec![];

        loop {
            if self.lexer.eof() {
                if items.is_empty() {
                    return Err(());
                }
                break;
            }

            if let Ok(ok) = self.lexer.peek() {
                if ok.is_the_symbol(Symbol::ClosingBrace) {
                    break;
                }
            }

            if let Ok(item) = self.read_statement_list_item() {
                items.push(item)
            }
        }

        Ok(Node::StatementList(items))
    }

    fn read_statement_list_item(&mut self) -> Result<Node, ()> {
        if self.is_declaration() {
            self.read_declaration()
        } else {
            self.read_statement()
        }
    }

    fn read_statement(&mut self) -> Result<Node, ()> {
        self.read_expression_statement()
    }
}

impl Parser {
    fn read_expression_statement(&mut self) -> Result<Node, ()> {
        self.read_expression()
    }

    fn read_expression(&mut self) -> Result<Node, ()> {
        let lhs = self.read_assignment_expression();
        lhs
    }

    /// https://tc39.github.io/ecma262/#prod-AssignmentExpression
    fn read_assignment_expression(&mut self) -> Result<Node, ()> {
        let lhs = self.read_additive_expression();
        lhs
    }

    /// https://tc39.github.io/ecma262/#prod-AssignmentExpression
    fn read_additive_expression(&mut self) -> Result<Node, ()> {
        let mut lhs = self.read_multiplicative_expression()?;
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::Symbol(Symbol::Add) => {
                lhs = Node::BinOp(
                    Box::new(lhs),
                    Box::new(self.read_additive_expression()?),
                    BinOp::Add,
                )
            }
            Kind::Symbol(Symbol::Sub) => {
                lhs = Node::BinOp(
                    Box::new(lhs),
                    Box::new(self.read_additive_expression()?),
                    BinOp::Sub,
                )
            }
            _ => self.lexer.unget(&tok),
        };
        Ok(lhs)
    }

    /// https://tc39.github.io/ecma262/#prod-AssignmentExpression
    fn read_multiplicative_expression(&mut self) -> Result<Node, ()> {
        let mut lhs = self.read_primary_expression()?;
        let tok = self.lexer.next()?;
        match tok.kind {
            Kind::Symbol(Symbol::Asterisk) => {
                lhs = Node::BinOp(
                    Box::new(lhs),
                    Box::new(self.read_multiplicative_expression()?),
                    BinOp::Mul,
                )
            }
            Kind::Symbol(Symbol::Div) => {
                lhs = Node::BinOp(
                    Box::new(lhs),
                    Box::new(self.read_multiplicative_expression()?),
                    BinOp::Div,
                )
            }
            _ => self.lexer.unget(&tok),
        };
        Ok(lhs)
    }

    /// https://tc39.github.io/ecma262/#prod-PrimaryExpression
    fn read_primary_expression(&mut self) -> Result<Node, ()> {
        match self.lexer.next()?.kind {
            Kind::Keyword(Keyword::This) => unimplemented!(),
            Kind::Identifier(ref i) if i == "true" => Ok(Node::Boolean(true)),
            Kind::Identifier(ref i) if i == "false" => Ok(Node::Boolean(false)),
            Kind::Identifier(ident) => Ok(Node::Identifier(ident)),
            Kind::Number(num) => Ok(Node::Number(num)),
            Kind::LineTerminator => self.read_primary_expression(),
            e => unimplemented!("{:?}", e),
        }
    }
}

impl Parser {
    fn is_declaration(&mut self) -> bool {
        self.is_hoistable_declaration()
    }

    fn read_declaration(&mut self) -> Result<Node, ()> {
        Err(())
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
