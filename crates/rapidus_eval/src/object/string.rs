#[derive(Debug, Clone)]
pub struct JsString(pub(crate) Vec<u16>);

impl JsString {
    pub fn new(s: &str) -> Self {
        Self(s.encode_utf16().collect())
    }

    pub fn new_leaked(s: &str) -> &'static mut Self {
        Box::leak(Box::new(Self::new(s)))
    }

    pub fn inner(&self) -> &[u16] {
        &self.0
    }
}

impl ToString for JsString {
    fn to_string(&self) -> String {
        String::from_utf16_lossy(&self.0)
    }
}
