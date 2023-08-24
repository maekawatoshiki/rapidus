use std::fmt;

use crate::object::string::JsString;

// Value representation by nan-boxing:
//   float  : FFFFFFFFFFFF|FFFF| FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF
//   pointer: 111111111111|0001| PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP
//   string : 111111111111|0010| PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP
//   bool   : 111111111111|0011| XXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXY (Y: 0 for false, 1 for true)
//                     tag ^^^^
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct JsValue(u64);

pub const TAG_F64: u64 = 0xFFF0;
pub const TAG_PTR: u64 = 0xFFF1;
pub const TAG_STR: u64 = 0xFFF2;
pub const TAG_BOOL: u64 = 0xFFF3;
pub const TAG_NULL: u64 = 0xFFF4;
pub const TAG_UNDEFINED: u64 = 0xFFF5;

impl JsValue {
    pub fn ptr(p: *const u8) -> Self {
        assert!(
            p as u64 & 0xFFFF000000000000 == 0,
            "The upper 64-48=16 bits must be 0."
        );
        Self(p as u64 | (TAG_PTR << 48))
    }

    pub fn str(s: *const u8) -> Self {
        assert!(
            s as u64 & 0xFFFF000000000000 == 0,
            "The upper 64-48=16 bits must be 0."
        );
        Self(s as u64 | (TAG_STR << 48))
    }

    pub fn bool(b: bool) -> Self {
        Self((b as u64) | (TAG_BOOL << 48))
    }

    pub fn f64(f: f64) -> Self {
        let n = f.to_bits();
        Self(if f.is_nan() {
            n & (0xFFF0_FFFF_FFFF_FFFF)
        } else {
            n
        })
    }

    pub fn null() -> Self {
        Self(TAG_NULL << 48)
    }

    pub fn undefined() -> Self {
        Self(TAG_UNDEFINED << 48)
    }

    pub fn is_ptr(&self) -> bool {
        self.0 >> 48 == TAG_PTR
    }

    pub fn is_str(&self) -> bool {
        self.0 >> 48 == TAG_STR
    }

    pub fn is_bool(&self) -> bool {
        self.0 >> 48 == TAG_BOOL
    }

    pub fn is_f64(&self) -> bool {
        !f64::from_bits(self.0).is_nan() || (self.0 >> 48 == TAG_F64)
    }

    pub fn is_null(&self) -> bool {
        self.0 >> 48 == TAG_NULL
    }

    pub fn is_undefined(&self) -> bool {
        self.0 >> 48 == TAG_UNDEFINED
    }

    pub fn as_ptr(&self) -> Option<*const u8> {
        self.is_ptr()
            .then_some((self.0 & 0x0000FFFFFFFFFFFF) as *const u8)
    }

    pub fn as_str(&self) -> Option<*const u8> {
        self.is_str()
            .then_some((self.0 & 0x0000FFFFFFFFFFFF) as *const u8)
    }

    pub fn as_bool(&self) -> Option<bool> {
        self.is_bool().then_some((self.0 & 0x0000FFFFFFFFFFFF) != 0)
    }

    pub fn as_f64(&self) -> Option<f64> {
        self.is_f64().then_some(f64::from_bits(self.0))
    }

    pub fn tag(&self) -> u64 {
        self.0 >> 48
    }
}

impl fmt::Debug for JsValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.tag() {
            TAG_PTR => write!(f, "ptr({:p})", self.as_ptr().unwrap()),
            TAG_STR => write!(f, "str({:?})", {
                let p = self.as_str().unwrap() as *const JsString;
                unsafe { &*p }.to_string()
            }),
            TAG_BOOL => write!(f, "bool({})", self.as_bool().unwrap()),
            TAG_NULL => write!(f, "null"),
            TAG_UNDEFINED => write!(f, "undefined"),
            _ if self.is_f64() => write!(f, "f64({})", self.as_f64().unwrap()),
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jsvalue_ptr() {
        let v = Vec::new();
        let p = v.as_ptr();
        let x = JsValue::ptr(p);
        assert!(x.is_ptr());
    }

    #[test]
    fn jsvalue_str() {
        let v = Vec::new();
        let p = v.as_ptr();
        let x = JsValue::str(p);
        assert!(x.is_str());
    }

    #[test]
    fn jsvalue_bool() {
        assert!(JsValue::bool(true).is_bool());
        assert!(JsValue::bool(false).is_bool());
    }

    #[test]
    fn jsvalue_f64() {
        assert!(JsValue::f64(0.0).is_f64());
        assert!(JsValue::f64(f64::MAX).is_f64());
        assert!(JsValue::f64(f64::MIN).is_f64());
        assert!(JsValue::f64(f64::NAN).is_f64());
    }

    #[test]
    fn jsvalue_null() {
        assert!(JsValue::null().is_null());
        assert!(JsValue::null().tag() == TAG_NULL);
    }

    #[test]
    fn jsvalue_undefined() {
        assert!(JsValue::undefined().is_undefined());
        assert!(JsValue::undefined().tag() == TAG_UNDEFINED);
    }

    #[test]
    fn jsvalue_arith() {
        let x = JsValue::f64(1.0);
        let y = JsValue::f64(2.0);
        assert_eq!(x.as_f64().unwrap() + y.as_f64().unwrap(), 3.0);
    }
}
