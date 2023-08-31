#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Opcode(pub u8);

pub const NOP: u8 = 0x00;
pub const ADD: u8 = 0x01;
pub const SUB: u8 = 0x02;
pub const MUL: u8 = 0x03;
pub const DIV: u8 = 0x04;
pub const MOD: u8 = 0x05;
pub const DROP: u8 = 0x06;
pub const CONST_F64: u8 = 0x07;

impl Opcode {
    pub fn name(self) -> &'static str {
        match self.0 {
            NOP => "nop",
            ADD => "add",
            SUB => "sub",
            MUL => "mul",
            DIV => "div",
            MOD => "mod",
            DROP => "drop",
            CONST_F64 => "const_f64",
            _ => unreachable!(),
        }
    }

    pub fn total_bytes(self) -> usize {
        match self.0 {
            NOP | ADD | SUB | MUL | DIV | MOD | DROP => 1,
            CONST_F64 => 9,
            _ => unreachable!(),
        }
    }
}
