use crate::vm::{
    jsvalue::value::*,
    vm::{VMValueResult, VM},
};
use rustc_hash::FxHashMap;

pub fn json_parse(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let text = to_json_text(vm, *args.get(0).unwrap_or(&Value::undefined()))?;
    let mut parser = JsonParser::new(&text);
    let value = parser.parse_value(vm).map_err(|msg| {
        vm.current_context
            .error_syntax(format!("JSON.parse: {}", msg))
    })?;
    parser.skip_ws();
    if !parser.eof() {
        return Err(vm
            .current_context
            .error_syntax("JSON.parse: unexpected input"));
    }
    Ok(value)
}

fn to_json_text(vm: &mut VM, value: Value) -> Result<String, crate::vm::error::RuntimeError> {
    if value.is_symbol() {
        return Err(vm
            .current_context
            .error_type("Cannot convert Symbol to string"));
    }
    if !value.is_object() {
        return Ok(value.to_string());
    }
    for method_name in ["toString", "valueOf"] {
        let key = vm.factory.string(method_name);
        let method = vm.get_property_by_value(value, key)?;
        if method.is_function_object() {
            let primitive = vm.call_function(method, &[], value)?;
            if !primitive.is_object() || primitive.is_symbol() {
                if primitive.is_symbol() {
                    return Err(vm
                        .current_context
                        .error_type("Cannot convert Symbol to string"));
                }
                return Ok(primitive.to_string());
            }
        }
    }
    Err(vm
        .current_context
        .error_type("Cannot convert object to string"))
}

pub fn json_stringify(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let value = *args.get(0).unwrap_or(&Value::undefined());
    let mut stack = Vec::new();
    match stringify_value(vm, value, &mut stack)? {
        Some(json) => Ok(vm.factory.string(json)),
        None => Ok(Value::undefined()),
    }
}

struct JsonParser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> JsonParser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn peek(&self) -> Option<u8> {
        self.input.as_bytes().get(self.pos).copied()
    }

    fn bump(&mut self) -> Option<u8> {
        let byte = self.peek()?;
        self.pos += 1;
        Some(byte)
    }

    fn skip_ws(&mut self) {
        while matches!(self.peek(), Some(b' ' | b'\n' | b'\r' | b'\t')) {
            self.pos += 1;
        }
    }

    fn parse_value(&mut self, vm: &mut VM) -> Result<Value, String> {
        self.skip_ws();
        match self.peek() {
            Some(b'n') => self.expect_word("null").map(|_| Value::null()),
            Some(b't') => self.expect_word("true").map(|_| Value::bool(true)),
            Some(b'f') => self.expect_word("false").map(|_| Value::bool(false)),
            Some(b'"') => self.parse_string().map(|string| vm.factory.string(string)),
            Some(b'[') => self.parse_array(vm),
            Some(b'{') => self.parse_object(vm),
            Some(b'-' | b'0'..=b'9') => self.parse_number().map(Value::Number),
            _ => Err("unexpected token".to_string()),
        }
    }

    fn expect_word(&mut self, word: &str) -> Result<(), String> {
        if self.input[self.pos..].starts_with(word) {
            self.pos += word.len();
            Ok(())
        } else {
            Err(format!("expected {}", word))
        }
    }

    fn parse_array(&mut self, vm: &mut VM) -> Result<Value, String> {
        self.bump();
        let mut elems = Vec::new();
        self.skip_ws();
        if self.peek() == Some(b']') {
            self.bump();
            return Ok(vm.factory.array(elems));
        }
        loop {
            let value = self.parse_value(vm)?;
            elems.push(Property::new_data_simple(value));
            self.skip_ws();
            match self.bump() {
                Some(b',') => {}
                Some(b']') => return Ok(vm.factory.array(elems)),
                _ => return Err("expected ',' or ']'".to_string()),
            }
        }
    }

    fn parse_object(&mut self, vm: &mut VM) -> Result<Value, String> {
        self.bump();
        let object = vm
            .factory
            .object_with_property_order(FxHashMap::default(), Vec::new());
        self.skip_ws();
        if self.peek() == Some(b'}') {
            self.bump();
            return Ok(object);
        }
        loop {
            self.skip_ws();
            if self.peek() != Some(b'"') {
                return Err("expected property name".to_string());
            }
            let key = self.parse_string()?;
            self.skip_ws();
            if self.bump() != Some(b':') {
                return Err("expected ':'".to_string());
            }
            let value = self.parse_value(vm)?;
            object.get_object_info().insert_property(
                key,
                Property::Data(DataProperty {
                    val: value,
                    writable: true,
                    enumerable: true,
                    configurable: true,
                }),
            );
            self.skip_ws();
            match self.bump() {
                Some(b',') => {}
                Some(b'}') => return Ok(object),
                _ => return Err("expected ',' or '}'".to_string()),
            }
        }
    }

    fn parse_string(&mut self) -> Result<String, String> {
        if self.bump() != Some(b'"') {
            return Err("expected string".to_string());
        }
        let mut out = String::new();
        while let Some(byte) = self.bump() {
            match byte {
                b'"' => return Ok(out),
                b'\\' => out.push(self.parse_escape()?),
                0x00..=0x1f => return Err("control character in string".to_string()),
                _ => {
                    let start = self.pos - 1;
                    let ch = self.input[start..]
                        .chars()
                        .next()
                        .ok_or_else(|| "invalid string".to_string())?;
                    self.pos = start + ch.len_utf8();
                    out.push(ch);
                }
            }
        }
        Err("unterminated string".to_string())
    }

    fn parse_escape(&mut self) -> Result<char, String> {
        match self.bump() {
            Some(b'"') => Ok('"'),
            Some(b'\\') => Ok('\\'),
            Some(b'/') => Ok('/'),
            Some(b'b') => Ok('\u{0008}'),
            Some(b'f') => Ok('\u{000c}'),
            Some(b'n') => Ok('\n'),
            Some(b'r') => Ok('\r'),
            Some(b't') => Ok('\t'),
            Some(b'u') => {
                let first = self.parse_hex4()?;
                if (0xd800..=0xdbff).contains(&first) && self.input[self.pos..].starts_with("\\u") {
                    self.pos += 2;
                    let second = self.parse_hex4()?;
                    if (0xdc00..=0xdfff).contains(&second) {
                        let scalar =
                            0x10000 + (((first - 0xd800) as u32) << 10) + (second - 0xdc00) as u32;
                        return char::from_u32(scalar).ok_or_else(|| "invalid unicode".to_string());
                    }
                }
                char::from_u32(first as u32).ok_or_else(|| "invalid unicode".to_string())
            }
            _ => Err("invalid escape".to_string()),
        }
    }

    fn parse_hex4(&mut self) -> Result<u16, String> {
        if self.pos + 4 > self.input.len() {
            return Err("short unicode escape".to_string());
        }
        let slice = &self.input[self.pos..self.pos + 4];
        if !slice.bytes().all(|byte| byte.is_ascii_hexdigit()) {
            return Err("invalid unicode escape".to_string());
        }
        self.pos += 4;
        u16::from_str_radix(slice, 16).map_err(|_| "invalid unicode escape".to_string())
    }

    fn parse_number(&mut self) -> Result<f64, String> {
        let start = self.pos;
        if self.peek() == Some(b'-') {
            self.pos += 1;
        }
        match self.peek() {
            Some(b'0') => self.pos += 1,
            Some(b'1'..=b'9') => {
                self.pos += 1;
                while matches!(self.peek(), Some(b'0'..=b'9')) {
                    self.pos += 1;
                }
            }
            _ => return Err("invalid number".to_string()),
        }
        if self.peek() == Some(b'.') {
            self.pos += 1;
            if !matches!(self.peek(), Some(b'0'..=b'9')) {
                return Err("invalid number".to_string());
            }
            while matches!(self.peek(), Some(b'0'..=b'9')) {
                self.pos += 1;
            }
        }
        if matches!(self.peek(), Some(b'e' | b'E')) {
            self.pos += 1;
            if matches!(self.peek(), Some(b'+' | b'-')) {
                self.pos += 1;
            }
            if !matches!(self.peek(), Some(b'0'..=b'9')) {
                return Err("invalid number".to_string());
            }
            while matches!(self.peek(), Some(b'0'..=b'9')) {
                self.pos += 1;
            }
        }
        self.input[start..self.pos]
            .parse::<f64>()
            .map_err(|_| "invalid number".to_string())
    }
}

fn stringify_value(
    vm: &mut VM,
    value: Value,
    stack: &mut Vec<Value>,
) -> Result<Option<String>, crate::vm::error::RuntimeError> {
    if value.is_undefined() || value.is_function_object() || value.is_symbol() {
        return Ok(None);
    }
    if value.is_null() {
        return Ok(Some("null".to_string()));
    }
    if let Value::Bool(_) = value {
        return Ok(Some(value.to_string()));
    }
    if let Value::Number(number) = value {
        return Ok(Some(if number.is_finite() {
            value.to_string()
        } else {
            "null".to_string()
        }));
    }
    if value.is_string() {
        return Ok(Some(quote_json_string(&value.to_string())));
    }
    if !value.is_object() {
        return Ok(None);
    }
    if stack.iter().any(|item| item.strict_eq_bool(value)) {
        return Err(vm
            .current_context
            .error_type("Converting circular structure to JSON"));
    }
    stack.push(value);
    let result = if value.is_array_object() {
        let array = value.as_array_mut();
        let mut parts = Vec::new();
        for index in 0..array.get_length() {
            let element = array
                .get_element(index)
                .as_data()
                .val
                .to_undefined_if_empty();
            parts.push(stringify_value(vm, element, stack)?.unwrap_or_else(|| "null".to_string()));
        }
        Some(format!("[{}]", parts.join(",")))
    } else {
        let keys = value
            .get_object_info()
            .enumerable_own_string_property_keys();
        let mut parts = Vec::new();
        for key in keys {
            let key_value = vm.factory.string(key.clone());
            let property_value = vm.get_property_by_value(value, key_value)?;
            if let Some(json) = stringify_value(vm, property_value, stack)? {
                parts.push(format!("{}:{}", quote_json_string(&key), json));
            }
        }
        Some(format!("{{{}}}", parts.join(",")))
    };
    stack.pop();
    Ok(result)
}

fn quote_json_string(input: &str) -> String {
    let mut out = String::from("\"");
    for ch in input.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\u{0008}' => out.push_str("\\b"),
            '\u{000c}' => out.push_str("\\f"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ch if ch <= '\u{001f}' => out.push_str(&format!("\\u{:04x}", ch as u32)),
            ch => out.push(ch),
        }
    }
    out.push('"');
    out
}
