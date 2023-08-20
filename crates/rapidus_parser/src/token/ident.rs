use ecow::EcoString;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ident {
    Reserved(ReservedWord),
    Ident(EcoString),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ReservedWord {
    Await,
    Break,
    Case,
    Catch,
    Class,
    Const,
    Continue,
    Debugger,
    Default,
    Delete,
    Do,
    Else,
    Enum,
    Export,
    Extends,
    False,
    Finally,
    For,
    Function,
    If,
    Import,
    In,
    Instanceof,
    New,
    Null,
    Return,
    Super,
    Switch,
    This,
    Throw,
    True,
    Try,
    Typeof,
    Var,
    Void,
    While,
    With,
    Yield,
}

impl From<ReservedWord> for Ident {
    fn from(word: ReservedWord) -> Self {
        Ident::Reserved(word)
    }
}

impl TryFrom<EcoString> for ReservedWord {
    type Error = ();

    fn try_from(word: EcoString) -> Result<Self, Self::Error> {
        match word.as_str() {
            "await" => Ok(ReservedWord::Await),
            "break" => Ok(ReservedWord::Break),
            "case" => Ok(ReservedWord::Case),
            "catch" => Ok(ReservedWord::Catch),
            "class" => Ok(ReservedWord::Class),
            "const" => Ok(ReservedWord::Const),
            "continue" => Ok(ReservedWord::Continue),
            "debugger" => Ok(ReservedWord::Debugger),
            "default" => Ok(ReservedWord::Default),
            "delete" => Ok(ReservedWord::Delete),
            "do" => Ok(ReservedWord::Do),
            "else" => Ok(ReservedWord::Else),
            "enum" => Ok(ReservedWord::Enum),
            "export" => Ok(ReservedWord::Export),
            "extends" => Ok(ReservedWord::Extends),
            "false" => Ok(ReservedWord::False),
            "finally" => Ok(ReservedWord::Finally),
            "for" => Ok(ReservedWord::For),
            "function" => Ok(ReservedWord::Function),
            "if" => Ok(ReservedWord::If),
            "import" => Ok(ReservedWord::Import),
            "in" => Ok(ReservedWord::In),
            "instanceof" => Ok(ReservedWord::Instanceof),
            "new" => Ok(ReservedWord::New),
            "null" => Ok(ReservedWord::Null),
            "return" => Ok(ReservedWord::Return),
            "super" => Ok(ReservedWord::Super),
            "switch" => Ok(ReservedWord::Switch),
            "this" => Ok(ReservedWord::This),
            "throw" => Ok(ReservedWord::Throw),
            "true" => Ok(ReservedWord::True),
            "try" => Ok(ReservedWord::Try),
            "typeof" => Ok(ReservedWord::Typeof),
            "var" => Ok(ReservedWord::Var),
            "void" => Ok(ReservedWord::Void),
            "while" => Ok(ReservedWord::While),
            "with" => Ok(ReservedWord::With),
            "yield" => Ok(ReservedWord::Yield),
            _ => Err(()),
        }
    }
}
