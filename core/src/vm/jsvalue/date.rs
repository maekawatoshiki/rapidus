use chrono::{DateTime, Utc};

#[derive(Clone, Debug)]
pub struct DateObjectInfo {
    utc: DateTime<Utc>,
}

impl Default for DateObjectInfo {
    fn default() -> Self {
        Self { utc: Utc::now() }
    }
}

impl DateObjectInfo {
    pub fn to_string(&self) -> String {
        self.utc.to_rfc3339()
    }
}
