use lexer;

pub struct Parser {
    pub lexer: lexer::Lexer,
}

pub enum Node {
    StatementList(Vec<Node>),
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
        Err(())
    }
}

impl Parser {
    fn read_script(&mut self) -> Result<(), ()> {
        Err(())
    }
}

impl Parser {
    fn read_statement_list(&mut self) -> Result<Node, ()> {
        let mut items = vec![];

        while let Ok(item) = self.read_statement_list_item() {
            items.push(item)
        }

        Ok(Node::StatementList(items))
    }

    fn read_statement_list_item(&mut self) -> Result<Node, ()> {
        if let Ok(true) = self.is_declaration() {
            self.read_declaration()
        } else {
            self.read_statement()
        }
    }

    fn read_statement(&mut self) -> Result<Node, ()> {
        Err(())
    }
}

impl Parser {
    fn is_declaration(&self) -> Result<bool, ()> {
        Ok(false)
    }

    fn read_declaration(&mut self) -> Result<Node, ()> {
        Err(())
    }
}
