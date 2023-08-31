use std::fmt;

use crate::value::JsValue;

use super::insn::Opcode;

#[derive(Clone)]
pub struct Code {
    bytecode: Vec<u8>,
    literals: Vec<JsValue>,
}

impl Code {
    pub fn new() -> Self {
        Self {
            bytecode: Vec::new(),
            literals: Vec::new(),
        }
    }

    pub fn push_opcode(&mut self, opcode: Opcode) {
        self.bytecode.push(opcode.0);
    }

    pub fn push_f64(&mut self, value: f64) {
        self.bytecode.extend_from_slice(&value.to_le_bytes());
    }

    pub fn push_bytes<const N: usize>(&mut self, bytes: [u8; N]) {
        self.bytecode.extend_from_slice(&bytes);
    }

    pub fn len(&self) -> usize {
        self.bytecode.len()
    }

    pub fn get<const N: usize>(&self, idx: usize) -> Option<[u8; N]> {
        self.bytecode[idx..idx + N].try_into().ok()
    }

    pub fn push_lit(&mut self, lit: JsValue) -> u32 {
        let idx = self.literals.len();
        self.literals.push(lit);
        idx as u32
    }

    pub fn get_lit(&self, idx: usize) -> Option<JsValue> {
        self.literals.get(idx).copied()
    }

    pub fn iter(&self) -> impl Iterator<Item = &u8> {
        self.bytecode.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut u8> {
        self.bytecode.iter_mut()
    }

    pub fn into_inner(self) -> Vec<u8> {
        self.bytecode
    }
}

impl fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut pos = 0;
        while pos < self.bytecode.len() {
            let opcode = Opcode(self.bytecode[pos]);
            let name = opcode.name();
            let total_bytes = opcode.total_bytes();
            writeln!(f, "{:05} {:?}", pos, name)?;
            pos += total_bytes;
        }
        Ok(())
    }
}
