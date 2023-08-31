#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Opcode(pub u8);

pub const NOP: Opcode = Opcode(0x00);
pub const ADD: Opcode = Opcode(0x01);
pub const SUB: Opcode = Opcode(0x02);
pub const MUL: Opcode = Opcode(0x03);
pub const DIV: Opcode = Opcode(0x04);
pub const MOD: Opcode = Opcode(0x05);
pub const DROP: Opcode = Opcode(0x06);
pub const CONST_F64: Opcode = Opcode(0x07);
pub const NULL: Opcode = Opcode(0x08);

impl Opcode {
    pub fn name(self) -> &'static str {
        match self {
            NOP => "nop",
            ADD => "add",
            SUB => "sub",
            MUL => "mul",
            DIV => "div",
            MOD => "mod",
            DROP => "drop",
            CONST_F64 => "const_f64",
            NULL => "null",
            _ => unreachable!(),
        }
    }

    pub const fn total_bytes(self) -> usize {
        match self {
            NOP | ADD | SUB | MUL | DIV | MOD | DROP | NULL => 1,
            CONST_F64 => 9,
            _ => unreachable!(),
        }
    }
}
