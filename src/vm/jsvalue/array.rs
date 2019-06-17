// use super::super::context::LexicalEnvironmentRef;
use super::value::*;
// use builtin::BuiltinFuncTy;
// use bytecode_gen::ByteCode;

#[derive(Clone, Debug)]
pub struct ArrayObjectInfo {
    pub elems: Vec<Property>,
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

    pub fn set_element(&mut self, idx: usize, val_: Value) -> Option<Value> {
        // Extend
        if idx >= self.elems.len() {
            self.set_length(idx + 1);
        }

        match self.elems[idx] {
            Property::Data(DataProperty { ref mut val, .. }) => {
                *val = val_;
                None
            }
            Property::Accessor(AccessorProperty { set, .. }) => {
                if set.is_undefined() {
                    None
                } else {
                    Some(set)
                }
            }
        }
    }

    pub fn set_length(&mut self, len: usize) {
        // Extend
        if self.elems.len() < len {
            while self.elems.len() < len {
                self.elems.push(Property::new_data_simple(Value::empty()))
            }
            return;
        }

        // Shorten
        if self.elems.len() > len {
            unsafe { self.elems.set_len(len) };
            return;
        }
    }

    #[inline]
    pub fn get_length(&self) -> usize {
        self.elems.len()
    }
}

impl ArrayObjectInfo {
    /// https://tc39.github.io/ecma262/#sec-array.prototype.join
    pub fn join(&self, separator: Option<String>) -> String {
        let separator = separator.unwrap_or(",".to_string());
        let separator_str = separator.as_str();
        let mut res = "".to_string();
        for (i, elem) in self.elems.iter().enumerate() {
            if let Some(data) = elem.get_data() {
                res += &(data.val.to_string()
                    + if self.elems.len() - 1 != i {
                        separator_str
                    } else {
                        ""
                    });
            }
        }
        res
    }
}
