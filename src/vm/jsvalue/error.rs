#[derive(Clone, Debug)]
pub struct ErrorObjectInfo {
    pub stack_trace: String,
}

impl ErrorObjectInfo {
    pub fn new() -> Self {
        ErrorObjectInfo {
            stack_trace: "".to_string(),
        }
    }
}
