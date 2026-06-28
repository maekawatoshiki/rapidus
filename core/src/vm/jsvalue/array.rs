// use super::super::context::LexicalEnvironmentRef;
use super::value::*;
// use builtin::BuiltinFuncTy;
// use bytecode_gen::ByteCode;

#[derive(Clone, Debug)]
pub struct ArrayObjectInfo {
    pub elems: Vec<Property>,
    pub length: usize,
    pub length_writable: bool,
}

impl ArrayObjectInfo {
    pub fn get_element(&self, idx: usize) -> Property {
        if idx >= self.elems.len() {
            return Property::new_data_simple(Value::undefined());
        }

        if let Property::Data(DataProperty {
            val,
            writable,
            enumerable,
            configurable,
        }) = self.elems[idx]
        {
            return Property::Data(DataProperty {
                val: val.to_undefined_if_empty(),
                writable,
                enumerable,
                configurable,
            });
        }

        self.elems[idx]
    }

    pub fn set_element(&mut self, idx: usize, val_: Value) -> (Option<Value>, bool) {
        // Extend
        if idx >= self.elems.len() {
            self.length = self.length.max(idx + 1);
            while self.elems.len() <= idx {
                self.elems.push(Property::new_data_simple(Value::empty()))
            }
        }

        match self.elems[idx] {
            Property::Data(DataProperty {
                ref mut val,
                writable,
                ..
            }) => {
                if writable {
                    *val = val_;
                    return (None, true);
                }
                (None, false)
            }
            Property::Accessor(AccessorProperty { set, .. }) => {
                if set.is_undefined() {
                    (None, false)
                } else {
                    (Some(set), true)
                }
            }
        }
    }

    pub fn set_length(&mut self, len: usize) {
        // Shorten
        if self.elems.len() > len {
            unsafe { self.elems.set_len(len) };
        }
        self.length = len;
    }

    #[inline]
    pub fn get_length(&self) -> usize {
        self.length
    }
}

impl ArrayObjectInfo {
    /// https://tc39.github.io/ecma262/#sec-array.prototype.join
    pub fn join(&self, separator: Option<String>) -> String {
        let separator = separator.unwrap_or(",".to_string());
        let separator_str = separator.as_str();
        let mut res = "".to_string();
        for i in 0..self.length {
            let elem = self.get_element(i);
            if let Some(data) = elem.get_data() {
                if !data.val.is_undefined() {
                    res += &data.val.to_string();
                }
                if self.length - 1 != i {
                    res += separator_str;
                }
            }
        }
        res
    }
}
