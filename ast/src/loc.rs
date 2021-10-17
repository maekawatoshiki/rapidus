use std::fmt;

#[derive(Copy, Clone, PartialEq)]
pub struct SourceLoc {
    /// Line (1-based index)
    pub line: usize,

    /// Column
    pub column: usize,

    /// Position from the beginning of the source code.
    pub pos: usize,
}

impl Default for SourceLoc {
    fn default() -> Self {
        Self {
            line: 1,
            column: 0,
            pos: 0,
        }
    }
}

impl SourceLoc {
    pub fn new(line: usize, column: usize, pos: usize) -> Self {
        Self { line, column, pos }
    }
}

impl fmt::Debug for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Loc(line:{},column:{},pos:{})",
            self.line, self.column, self.pos
        )
    }
}
