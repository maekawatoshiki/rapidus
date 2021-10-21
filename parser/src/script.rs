use std::path::PathBuf;

#[derive(Clone, Debug)]
/// Information about a source file (module).
pub struct ScriptInfo {
    /// Absolute file path of the script.
    file_path: PathBuf,

    /// Source text of the script.
    code: String,
}

impl ScriptInfo {
    pub fn new<P: Into<PathBuf>, S: Into<String>>(file_path: P, code: S) -> Self {
        Self {
            file_path: file_path.into(),
            code: code.into(),
        }
    }

    pub fn file_path(&self) -> &PathBuf {
        &self.file_path
    }

    pub fn code(&self) -> &String {
        &self.code
    }
}
