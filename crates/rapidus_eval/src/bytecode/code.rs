use std::fmt;

use super::insn::Opcode;

#[derive(Clone)]
pub struct Code(Vec<u8>);

impl Code {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push_opcode(&mut self, opcode: Opcode) {
        self.0.push(opcode.0);
    }

    pub fn push_f64(&mut self, value: f64) {
        self.0.extend_from_slice(&value.to_le_bytes());
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get<const N: usize>(&self, idx: usize) -> Option<[u8; N]> {
        self.0[idx..idx + N].try_into().ok()
    }

    pub fn iter(&self) -> impl Iterator<Item = &u8> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut u8> {
        self.0.iter_mut()
    }

    pub fn into_inner(self) -> Vec<u8> {
        self.0
    }
}

impl fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut pos = 0;
        while pos < self.0.len() {
            let opcode = Opcode(self.0[pos]);
            let name = opcode.name();
            let total_bytes = opcode.total_bytes();
            writeln!(f, "{:05} {:?}", pos, name)?;
            pos += total_bytes;
        }
        Ok(())
    }
}
