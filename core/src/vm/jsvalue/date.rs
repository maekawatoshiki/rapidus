use chrono::{DateTime, TimeZone, Utc};

#[derive(Clone, Debug)]
pub struct DateObjectInfo {
    millis: Option<i64>,
}

impl Default for DateObjectInfo {
    fn default() -> Self {
        Self {
            millis: Some(Utc::now().timestamp_millis()),
        }
    }
}

impl DateObjectInfo {
    pub fn from_millis(millis: f64) -> Self {
        if millis.is_finite() && millis.abs() <= 8.64e15 {
            Self {
                millis: Some(millis.trunc() as i64),
            }
        } else {
            Self { millis: None }
        }
    }

    pub fn millis(&self) -> Option<i64> {
        self.millis
    }

    pub fn set_millis(&mut self, millis: Option<i64>) {
        self.millis = millis;
    }

    pub fn utc(&self) -> Option<DateTime<Utc>> {
        self.millis
            .and_then(|millis| Utc.timestamp_millis_opt(millis).single())
    }

    pub fn to_string(&self) -> String {
        self.utc()
            .map(|utc| utc.to_rfc3339())
            .unwrap_or_else(|| "Invalid Date".to_string())
    }
}
