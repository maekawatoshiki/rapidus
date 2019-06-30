use crate::vm::constant;
use crate::vm::jsvalue::value::Value;

pub type ByteCode = Vec<u8>;

#[derive(Debug)]
pub struct ByteCodeGenerator<'a> {
    pub constant_table: &'a mut constant::ConstantTable,
}

impl<'a> ByteCodeGenerator<'a> {
    pub fn new(constant_table: &'a mut constant::ConstantTable) -> Self {
        ByteCodeGenerator { constant_table }
    }

    pub fn append_end(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::END);
    }

    pub fn append_construct(&self, argc: usize, iseq: &mut ByteCode) {
        iseq.push(VMInst::CONSTRUCT);
        self.append_int32(argc as i32, iseq);
    }

    pub fn append_create_object(&self, len: usize, iseq: &mut ByteCode) {
        iseq.push(VMInst::CREATE_OBJECT);
        self.append_int32(len as i32, iseq);
    }

    pub fn append_create_array(&self, len: usize, iseq: &mut ByteCode) {
        iseq.push(VMInst::CREATE_ARRAY);
        self.append_int32(len as i32, iseq);
    }

    pub fn append_push_int8(&self, n: i8, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_INT8);
        self.append_int8(n, iseq);
    }

    pub fn append_push_int32(&self, n: i32, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_INT32);
        self.append_int32(n, iseq);
    }

    pub fn append_push_number(&mut self, n: f64, iseq: &mut ByteCode) {
        // If 'n' is an integer:
        if n - n.floor() == 0.0 {
            // If 'n' is within 1 byte:
            if (::std::i8::MIN as f64) < n && n < (::std::i8::MAX as f64) {
                self.append_push_int8(n as i8, iseq);
                return;
            } else if (::std::i32::MIN as f64) <= n && n <= (::std::i32::MAX as f64) {
                self.append_push_int32(n as i32, iseq);
                return;
            }
        }

        self.append_push_const(Value::Number(n), iseq)
    }

    pub fn append_push_bool(&self, b: bool, iseq: &mut ByteCode) {
        iseq.push(if b {
            VMInst::PUSH_TRUE
        } else {
            VMInst::PUSH_FALSE
        })
    }

    pub fn append_push_const(&mut self, val: Value, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_CONST);
        let id = self.constant_table.add_value(val);
        self.append_int32(id as i32, iseq);
    }

    pub fn append_push_null(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_NULL);
    }

    pub fn append_push_this(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_THIS);
    }

    pub fn append_push_arguments(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_ARGUMENTS);
    }

    pub fn append_push_undefined(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_UNDEFINED);
    }

    pub fn append_lnot(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::LNOT);
    }
    pub fn append_posi(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::POSI);
    }
    pub fn append_neg(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::NEG);
    }
    pub fn append_add(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::ADD);
    }
    pub fn append_sub(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::SUB);
    }
    pub fn append_mul(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::MUL);
    }
    pub fn append_div(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::DIV);
    }
    pub fn append_rem(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::REM);
    }
    pub fn append_exp(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::EXP);
    }
    pub fn append_lt(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::LT);
    }
    pub fn append_gt(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::GT);
    }
    pub fn append_le(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::LE);
    }
    pub fn append_ge(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::GE);
    }
    pub fn append_eq(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::EQ);
    }
    pub fn append_ne(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::NE);
    }
    pub fn append_seq(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::SEQ);
    }
    pub fn append_sne(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::SNE);
    }
    pub fn append_not(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::NOT);
    }
    pub fn append_and(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::AND);
    }
    pub fn append_or(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::OR);
    }
    pub fn append_xor(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::XOR);
    }
    pub fn append_shl(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::SHL);
    }
    pub fn append_shr(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::SHR);
    }
    pub fn append_zfshr(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::ZFSHR);
    }
    pub fn append_land(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::LAND);
    }
    pub fn append_lor(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::LOR);
    }
    pub fn append_double(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::DOUBLE);
    }
    pub fn append_pop(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::POP);
    }
    pub fn append_get_member(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::GET_MEMBER);
    }

    pub fn append_set_member(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::SET_MEMBER);
    }

    pub fn append_call(&self, argc: u32, iseq: &mut ByteCode) {
        iseq.push(VMInst::CALL);
        self.append_int32(argc as i32, iseq);
    }

    pub fn append_call_method(&self, argc: u32, iseq: &mut ByteCode) {
        iseq.push(VMInst::CALL_METHOD);
        self.append_int32(argc as i32, iseq);
    }

    pub fn append_jmp(&self, dst: i32, iseq: &mut ByteCode) {
        iseq.push(VMInst::JMP);
        self.append_int32(dst, iseq);
    }

    pub fn append_jmp_if_false(&self, dst: i32, iseq: &mut ByteCode) {
        iseq.push(VMInst::JMP_IF_FALSE);
        self.append_int32(dst, iseq);
    }

    pub fn append_return_sub(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::RETURN_SUB);
    }

    pub fn append_jmp_sub(&self, dst: i32, iseq: &mut ByteCode) {
        iseq.push(VMInst::JMP_SUB);
        self.append_int32(dst, iseq);
    }

    pub fn append_return(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::RETURN);
    }

    pub fn append_return_try(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::RETURN_TRY);
        self.append_int32(0, iseq);
    }

    pub fn append_get_value(&mut self, name: &String, iseq: &mut ByteCode) {
        let id = self.constant_table.add_string(name.clone()) as i32;
        iseq.push(VMInst::GET_VALUE);
        self.append_int32(id, iseq);
    }

    pub fn append_set_value(&mut self, name: &String, iseq: &mut ByteCode) {
        let id = self.constant_table.add_string(name.clone()) as i32;
        iseq.push(VMInst::SET_VALUE);
        self.append_int32(id, iseq);
    }

    pub fn append_cond_op(&mut self, iseq: &mut ByteCode) {
        iseq.push(VMInst::COND_OP);
    }

    pub fn append_loop_start(&mut self, iseq: &mut ByteCode) {
        iseq.push(VMInst::LOOP_START);
        self.append_int32(0, iseq);
    }

    pub fn append_throw(&mut self, iseq: &mut ByteCode) {
        iseq.push(VMInst::THROW);
    }

    pub fn append_push_env(&mut self, id: u32, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_ENV);
        self.append_uint32(id, iseq);
    }

    pub fn append_pop_env(&mut self, iseq: &mut ByteCode) {
        iseq.push(VMInst::POP_ENV);
    }

    pub fn append_set_outer_env(&mut self, iseq: &mut ByteCode) {
        iseq.push(VMInst::SET_OUTER_ENV);
    }

    pub fn append_typeof(&mut self, iseq: &mut ByteCode) {
        iseq.push(VMInst::TYPEOF);
    }

    // Utils

    pub fn append_int8(&self, n: i8, iseq: &mut ByteCode) {
        iseq.push(n as u8);
    }

    pub fn append_int32(&self, n: i32, iseq: &mut ByteCode) {
        iseq.push(((n >> 0) & 0xff as i32) as u8);
        iseq.push(((n >> 8) & 0xff as i32) as u8);
        iseq.push(((n >> 16) & 0xff as i32) as u8);
        iseq.push(((n >> 24) & 0xff as i32) as u8);
    }

    pub fn append_uint32(&self, n: u32, iseq: &mut ByteCode) {
        iseq.push(((n >> 0) & 0xff as u32) as u8);
        iseq.push(((n >> 8) & 0xff as u32) as u8);
        iseq.push(((n >> 16) & 0xff as u32) as u8);
        iseq.push(((n >> 24) & 0xff as u32) as u8);
    }

    pub fn replace_int32(&self, n: i32, iseq: &mut [u8]) {
        iseq[3] = (n >> 24) as u8;
        iseq[2] = (n >> 16) as u8;
        iseq[1] = (n >> 8) as u8;
        iseq[0] = (n >> 0) as u8;
    }
}

pub fn slice_to_int32(iseq: &[u8]) -> i32 {
    ((iseq[3] as i32) << 24) + ((iseq[2] as i32) << 16) + ((iseq[1] as i32) << 8) + (iseq[0] as i32)
}

pub fn read_int32(iseq: &ByteCode, pc: usize) -> i32 {
    (((iseq[pc as usize + 3] as u32) << 24)
        + ((iseq[pc as usize + 2] as u32) << 16)
        + ((iseq[pc as usize + 1] as u32) << 8)
        + (iseq[pc as usize + 0] as u32)) as i32
}

pub fn show_inst_seq(code: &ByteCode, const_table: &constant::ConstantTable) {
    let mut i = 0;
    while i < code.len() {
        println!("{}", show_inst(code, i, const_table));
        i += if let Some(size) = VMInst::get_inst_size(code[i]) {
            size
        } else {
            unreachable!("inst_size not defined.");
        };
    }
}

pub fn show_inst(code: &ByteCode, i: usize, const_table: &constant::ConstantTable) -> String {
    format!(
        "{:05} {:<25}",
        i,
        match code[i] {
            VMInst::CONSTRUCT => {
                let int32 = read_int32(code, i + 1);
                format!("Construct {}", int32)
            }
            VMInst::CREATE_OBJECT => {
                let int32 = read_int32(code, i + 1);
                format!("CreateObject {}", int32)
            }
            VMInst::CREATE_ARRAY => {
                let int32 = read_int32(code, i + 1);
                format!("CreateArray {}", int32)
            }
            VMInst::PUSH_INT8 => {
                let int8 = code[i + 1] as i32;
                format!("PushInt8 {}", int8)
            }
            VMInst::PUSH_INT32 => {
                let int32 = read_int32(code, i + 1);
                format!("PushInt32 {}", int32)
            }
            VMInst::PUSH_CONST => {
                let int32 = read_int32(code, i + 1);
                let value = const_table.get(int32 as usize).as_value();
                // TODO: Implement 'format' for 'value'
                format!("PushConst {}", value)
            }
            VMInst::JMP_IF_FALSE => {
                let int32 = read_int32(code, i + 1);
                format!("JmpIfFalse {:05}", i as i32 + int32 + 5)
            }
            VMInst::JMP => {
                let int32 = read_int32(code, i + 1);
                format!("Jmp {:05}", i as i32 + int32 + 5)
            }
            /*VMInst::JMP_UNWIND => {
                let dest = read_int32(code, i + 1);
                let pop = read_int32(code, i + 5);
                format!("JmpUnwind {:05} {}", i as i32 + dest + 5, pop)
            }*/
            VMInst::CALL => {
                let int32 = read_int32(code, i + 1);
                format!("Call {}", int32)
            }
            VMInst::CALL_METHOD => {
                let int32 = read_int32(code, i + 1);
                format!("CallMethod {}", int32)
            }
            VMInst::RETURN => format!("Return"),
            VMInst::DOUBLE => format!("Double"),
            VMInst::POP => format!("Pop"),
            VMInst::LAND => format!("LogAnd"),
            VMInst::LOR => format!("LogOr"),
            //VMInst::UPDATE_PARENT_SCOPE => format!("UpdateParentScope"),
            VMInst::GET_VALUE => {
                let int32 = read_int32(code, i + 1);
                let name = const_table.get(int32 as usize).as_string();
                format!("GetValue '{}'", name)
            }
            VMInst::SET_VALUE => {
                let int32 = read_int32(code, i + 1);
                let name = const_table.get(int32 as usize).as_string();
                format!("SetValue '{}'", name)
            }
            VMInst::DECL_VAR => {
                let int32 = read_int32(code, i + 1);
                let name = const_table.get(int32 as usize).as_string();
                format!("DeclVar '{}'", name)
            }
            VMInst::DECL_CONST => {
                let int32 = read_int32(code, i + 1);
                let name = const_table.get(int32 as usize).as_string();
                format!("DeclConst '{}'", name)
            }
            VMInst::DECL_LET => {
                let int32 = read_int32(code, i + 1);
                let name = const_table.get(int32 as usize).as_string();
                format!("DeclLet '{}'", name)
            }
            VMInst::PUSH_ENV => {
                let int32 = read_int32(code, i + 1);
                let names = const_table.get(int32 as usize).as_lex_env_info();
                format!("PushEnv '{:?}'", names)
            }
            VMInst::JMP_SUB => {
                let int32 = read_int32(code, i + 1);
                format!("JmpSub {:05}", i as i32 + int32 + 5)
            }
            _ => inst_to_inst_name(code[i]).to_string(),
        }
    )
}

pub fn inst_to_inst_name(inst: u8) -> &'static str {
    match inst {
        VMInst::END => "End",
        VMInst::CONSTRUCT => "Construct",
        VMInst::CREATE_OBJECT => "CreateObject",
        VMInst::CREATE_ARRAY => "CreateArray",
        VMInst::PUSH_INT8 => "PushInt8",
        VMInst::PUSH_INT32 => "PushInt32",
        VMInst::PUSH_FALSE => "PushFalse",
        VMInst::PUSH_TRUE => "PushTrue",
        VMInst::PUSH_CONST => "PushConst",
        VMInst::PUSH_NULL => "PushNull",
        VMInst::PUSH_THIS => "PushThis",
        VMInst::PUSH_ARGUMENTS => "PushArguments",
        VMInst::PUSH_UNDEFINED => "PushUndefined",
        VMInst::LNOT => "LogNot",
        VMInst::POSI => "Posi",
        VMInst::NEG => "Neg",
        VMInst::ADD => "Add",
        VMInst::SUB => "Sub",
        VMInst::MUL => "Mul",
        VMInst::DIV => "Div",
        VMInst::REM => "Rem",
        VMInst::LT => "Lt",
        VMInst::GT => "Gt",
        VMInst::LE => "Le",
        VMInst::GE => "Ge",
        VMInst::EQ => "Eq",
        VMInst::NE => "Ne",
        VMInst::SEQ => "SEq",
        VMInst::SNE => "SNeg",
        VMInst::AND => "BitwiseAnd",
        VMInst::OR => "BitwiseOr",
        VMInst::XOR => "BitwiseXor",
        VMInst::SHL => "Shift-L",
        VMInst::SHR => "Shift-R",
        VMInst::ZFSHR => "ZeroFill-Shift-R",
        VMInst::GET_MEMBER => "GetMember",
        VMInst::SET_MEMBER => "SetMember",
        VMInst::JMP_IF_FALSE => "JmpIfFalse",
        VMInst::JMP => "Jmp",
        VMInst::CALL => "Call",
        VMInst::CALL_METHOD => "CallMethod",
        VMInst::RETURN => "Return",
        VMInst::DOUBLE => "Double",
        VMInst::POP => "Pop",
        VMInst::LAND => "LogAnd",
        VMInst::LOR => "LogOr",
        VMInst::GET_VALUE => "GetValue",
        VMInst::SET_VALUE => "SetValue",
        VMInst::DECL_VAR => "DeclVar",
        VMInst::DECL_CONST => "DeclConst",
        VMInst::DECL_LET => "DeclLet",
        VMInst::PUSH_ENV => "PushEnv",
        VMInst::POP_ENV => "PopEnv",
        VMInst::COND_OP => "CondOp",
        VMInst::LOOP_START => "LoopStart",
        VMInst::THROW => "Throw",
        VMInst::RETURN_TRY => "ReturnTry",
        VMInst::NOT => "BitwiseNot",
        VMInst::SET_OUTER_ENV => "SetOuterEnv",
        VMInst::JMP_SUB => "JmpSub",
        VMInst::RETURN_SUB => "ReturnSub",
        VMInst::TYPEOF => "Typeof",
        VMInst::EXP => "Exp",
        _ => "???",
    }
}

#[allow(non_snake_case)]
pub mod VMInst {
    pub const END: u8 = 0x00;
    pub const CREATE_OBJECT: u8 = 0x03;
    pub const CREATE_ARRAY: u8 = 0x04;
    pub const PUSH_INT8: u8 = 0x05;
    pub const PUSH_INT32: u8 = 0x06;
    pub const PUSH_FALSE: u8 = 0x07;
    pub const PUSH_TRUE: u8 = 0x08;
    pub const PUSH_CONST: u8 = 0x09;
    pub const PUSH_NULL: u8 = 0x46;
    pub const PUSH_THIS: u8 = 0x0a;
    pub const PUSH_ARGUMENTS: u8 = 0x0b;
    pub const PUSH_UNDEFINED: u8 = 0x0c;
    pub const DOUBLE: u8 = 0x29;
    pub const POP: u8 = 0x2a;
    pub const LNOT: u8 = 0x0d;
    pub const POSI: u8 = 0x0e;
    pub const NEG: u8 = 0x0f;
    pub const ADD: u8 = 0x10;
    pub const SUB: u8 = 0x11;
    pub const MUL: u8 = 0x12;
    pub const DIV: u8 = 0x13;
    pub const REM: u8 = 0x14;
    pub const EXP: u8 = 0x47;
    pub const LAND: u8 = 0x2b;
    pub const LOR: u8 = 0x2c;
    pub const NOT: u8 = 0x3d;
    pub const LT: u8 = 0x15;
    pub const GT: u8 = 0x16;
    pub const LE: u8 = 0x17;
    pub const GE: u8 = 0x18;
    pub const EQ: u8 = 0x19;
    pub const NE: u8 = 0x1a;
    pub const SEQ: u8 = 0x1b;
    pub const SNE: u8 = 0x1c;
    pub const AND: u8 = 0x1d;
    pub const OR: u8 = 0x1e;
    pub const XOR: u8 = 0x1f;
    pub const SHL: u8 = 0x20;
    pub const SHR: u8 = 0x21;
    pub const ZFSHR: u8 = 0x22;
    pub const JMP_IF_FALSE: u8 = 0x25;
    pub const JMP: u8 = 0x26;
    pub const CALL: u8 = 0x27;
    pub const CALL_METHOD: u8 = 0x41;
    pub const CONSTRUCT: u8 = 0x02;
    pub const RETURN: u8 = 0x28;
    pub const GET_MEMBER: u8 = 0x23;
    pub const SET_MEMBER: u8 = 0x24;
    pub const GET_VALUE: u8 = 0x2e;
    pub const SET_VALUE: u8 = 0x2f;
    pub const DECL_VAR: u8 = 0x30;
    pub const DECL_CONST: u8 = 0x3b;
    pub const DECL_LET: u8 = 0x3c;
    pub const PUSH_ENV: u8 = 0x3f;
    pub const POP_ENV: u8 = 0x40;
    pub const COND_OP: u8 = 0x31;
    pub const LOOP_START: u8 = 0x32;
    pub const THROW: u8 = 0x33;
    pub const RETURN_TRY: u8 = 0x38;
    pub const SET_OUTER_ENV: u8 = 0x42;
    pub const JMP_SUB: u8 = 0x43;
    pub const RETURN_SUB: u8 = 0x44;
    pub const TYPEOF: u8 = 0x45;

    pub fn get_inst_size(inst: u8) -> Option<usize> {
        match inst {
            THROW | RETURN_SUB | SET_OUTER_ENV | POP_ENV | TYPEOF | PUSH_NULL => Some(1),
            CONSTRUCT | CREATE_OBJECT | PUSH_CONST | PUSH_INT32 | CREATE_ARRAY | JMP_IF_FALSE
            | RETURN_TRY | DECL_VAR | LOOP_START | JMP | SET_VALUE | GET_VALUE | CALL | JMP_SUB
            | CALL_METHOD | PUSH_ENV | DECL_LET | DECL_CONST => Some(5),
            PUSH_INT8 => Some(2),
            PUSH_FALSE | END | PUSH_TRUE | PUSH_THIS | ADD | SUB | MUL | DIV | REM | LT | EXP
            | PUSH_ARGUMENTS | NEG | POSI | GT | LE | GE | EQ | NE | GET_MEMBER | RETURN | SNE
            | ZFSHR | POP | DOUBLE | AND | COND_OP | OR | SEQ | SET_MEMBER | LNOT
            | PUSH_UNDEFINED | LAND | SHR | SHL | XOR | LOR | NOT => Some(1),
            _ => None,
        }
    }
}
