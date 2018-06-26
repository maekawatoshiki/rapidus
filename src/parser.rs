use lexer;

pub struct Parser {
    pub lexer: lexer::Lexer,
}

pub struct Node {
}

impl Parser {
    pub fn new(code:String) -> Parser {
        Parser {
            lexer: lexer::Lexer::new(code)
        }
    }
}

impl Parser {
    pub fn next(&mut self) -> Result<Node, ()> {
        Err(())
    }
}

impl Parser {
    fn read_script(&mut self) -> Result<Node, ()> { 
        Err(())
    }
}

impl Parser {
    fn read_statement_list(&mut self) -> Result<Node, ()> {
        Err(())
    }
}
