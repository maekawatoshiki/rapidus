use vm::{value::Value, vm::ConstantTable};

pub type ByteCode = Vec<u8>;

#[allow(non_snake_case)]
pub mod VMInst {
    pub const END: u8 = 0x00;
    pub const CREATE_CONTEXT: u8 = 0x01;
    pub const CONSTRUCT: u8 = 0x02;
    pub const CREATE_OBJECT: u8 = 0x03;
    pub const CREATE_ARRAY: u8 = 0x04;
    pub const PUSH_INT8: u8 = 0x05;
    pub const PUSH_INT32: u8 = 0x06;
    pub const PUSH_FALSE: u8 = 0x07;
    pub const PUSH_TRUE: u8 = 0x08;
    pub const PUSH_CONST: u8 = 0x09;
    pub const PUSH_THIS: u8 = 0x0a;
    pub const PUSH_ARGUMENTS: u8 = 0x0b;
    pub const PUSH_UNDEFINED: u8 = 0x0c;
    pub const LNOT: u8 = 0x0d;
    pub const POSI: u8 = 0x0e;
    pub const NEG: u8 = 0x0f;
    pub const ADD: u8 = 0x10;
    pub const SUB: u8 = 0x11;
    pub const MUL: u8 = 0x12;
    pub const DIV: u8 = 0x13;
    pub const REM: u8 = 0x14;
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
    pub const GET_MEMBER: u8 = 0x23;
    pub const SET_MEMBER: u8 = 0x24;
    pub const JMP_IF_FALSE: u8 = 0x25;
    pub const JMP: u8 = 0x26;
    pub const CALL: u8 = 0x27;
    pub const RETURN: u8 = 0x28;
    pub const DOUBLE: u8 = 0x29;
    pub const POP: u8 = 0x2a;
    pub const LAND: u8 = 0x2b;
    pub const LOR: u8 = 0x2c;
    pub const UPDATE_PARENT_SCOPE: u8 = 0x2d;
    pub const GET_VALUE: u8 = 0x2e;
    pub const SET_VALUE: u8 = 0x2f;
    pub const DECL_VAR: u8 = 0x30;
    pub const COND_OP: u8 = 0x31;
    pub const LOOP_START: u8 = 0x32;
    pub const THROW: u8 = 0x33;
    pub const ENTER_TRY: u8 = 0x34;
    pub const LEAVE_TRY: u8 = 0x35;
    pub const CATCH: u8 = 0x36;
    pub const FINALLY: u8 = 0x37;
    pub const RETURN_TRY: u8 = 0x38;
    pub const PUSH_SCOPE: u8 = 0x39;
    pub const POP_SCOPE: u8 = 0x3a;
    pub const DECL_CONST: u8 = 0x3b;
    pub const DECL_LET: u8 = 0x3c;
    pub const NOT: u8 = 0x3d;
    pub const JMP_UNWIND: u8 = 0x3e;

    pub fn get_inst_size(inst: u8) -> Option<usize> {
        match inst {
            CREATE_CONTEXT | THROW | LEAVE_TRY | CATCH | FINALLY | POP_SCOPE | PUSH_SCOPE => {
                Some(1)
            }
            CONSTRUCT | CREATE_OBJECT | PUSH_CONST | PUSH_INT32 | CREATE_ARRAY | JMP_IF_FALSE
            | RETURN_TRY | DECL_VAR | LOOP_START | JMP | SET_VALUE | GET_VALUE | CALL
            | DECL_LET | DECL_CONST => Some(5),
            PUSH_INT8 => Some(2),
            PUSH_FALSE | END | PUSH_TRUE | PUSH_THIS | ADD | SUB | MUL | DIV | REM | LT
            | PUSH_ARGUMENTS | NEG | POSI | GT | LE | GE | EQ | NE | GET_MEMBER | RETURN | SNE
            | ZFSHR | POP | DOUBLE | AND | COND_OP | OR | SEQ | SET_MEMBER | LNOT
            | UPDATE_PARENT_SCOPE | PUSH_UNDEFINED | LAND | SHR | SHL | XOR | LOR | NOT => Some(1),
            ENTER_TRY => Some(9),
            JMP_UNWIND => Some(13),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ByteCodeGen {
    pub const_table: ConstantTable,
}

impl ByteCodeGen {
    pub fn new() -> ByteCodeGen {
        ByteCodeGen {
            const_table: ConstantTable::new(),
        }
    }
}

impl ByteCodeGen {
    pub fn gen_end(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::END);
    }

    pub fn gen_create_context(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::CREATE_CONTEXT);
    }

    pub fn gen_constract(&self, argc: usize, iseq: &mut ByteCode) {
        iseq.push(VMInst::CONSTRUCT);
        self.gen_int32(argc as i32, iseq);
    }

    pub fn gen_create_object(&self, len: usize, iseq: &mut ByteCode) {
        iseq.push(VMInst::CREATE_OBJECT);
        self.gen_int32(len as i32, iseq);
    }

    pub fn gen_create_array(&self, len: usize, iseq: &mut ByteCode) {
        iseq.push(VMInst::CREATE_ARRAY);
        self.gen_int32(len as i32, iseq);
    }

    pub fn gen_push_int8(&self, n: i8, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_INT8);
        self.gen_int8(n, iseq);
    }

    pub fn gen_push_int32(&self, n: i32, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_INT32);
        self.gen_int32(n, iseq);
    }

    pub fn gen_push_number(&mut self, n: f64, iseq: &mut ByteCode) {
        // If 'n' is an integer:
        if n - n.floor() == 0.0 {
            // If 'n' is within 1 byte:
            if (::std::i8::MIN as f64) < n && n < (::std::i8::MAX as f64) {
                self.gen_push_int8(n as i8, iseq);
                return;
            } else if (::std::i32::MIN as f64) <= n && n <= (::std::i32::MAX as f64) {
                self.gen_push_int32(n as i32, iseq);
                return;
            }
        }

        self.gen_push_const(Value::Number(n), iseq)
    }

    pub fn gen_push_bool(&self, b: bool, iseq: &mut ByteCode) {
        iseq.push(if b {
            VMInst::PUSH_TRUE
        } else {
            VMInst::PUSH_FALSE
        })
    }

    pub fn gen_push_const(&mut self, val: Value, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_CONST);
        let id = self.const_table.value.len();
        self.const_table.value.push(val);
        self.gen_int32(id as i32, iseq);
    }

    pub fn gen_push_this(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_THIS);
    }

    pub fn gen_push_arguments(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_ARGUMENTS);
    }

    pub fn gen_push_undefined(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_UNDEFINED);
    }

    pub fn gen_lnot(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::LNOT);
    }
    pub fn gen_posi(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::POSI);
    }
    pub fn gen_neg(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::NEG);
    }
    pub fn gen_add(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::ADD);
    }
    pub fn gen_sub(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::SUB);
    }
    pub fn gen_mul(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::MUL);
    }
    pub fn gen_div(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::DIV);
    }
    pub fn gen_rem(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::REM);
    }
    pub fn gen_lt(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::LT);
    }
    pub fn gen_gt(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::GT);
    }
    pub fn gen_le(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::LE);
    }
    pub fn gen_ge(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::GE);
    }
    pub fn gen_eq(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::EQ);
    }
    pub fn gen_ne(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::NE);
    }
    pub fn gen_seq(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::SEQ);
    }
    pub fn gen_sne(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::SNE);
    }
    pub fn gen_not(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::NOT);
    }
    pub fn gen_and(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::AND);
    }
    pub fn gen_or(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::OR);
    }
    pub fn gen_xor(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::XOR);
    }
    pub fn gen_shl(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::SHL);
    }
    pub fn gen_shr(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::SHR);
    }
    pub fn gen_zfshr(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::ZFSHR);
    }
    pub fn gen_land(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::LAND);
    }
    pub fn gen_lor(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::LOR);
    }
    pub fn gen_double(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::DOUBLE);
    }
    pub fn gen_pop(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::POP);
    }
    pub fn gen_get_member(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::GET_MEMBER);
    }

    pub fn gen_set_member(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::SET_MEMBER);
    }

    pub fn gen_call(&self, argc: u32, iseq: &mut ByteCode) {
        iseq.push(VMInst::CALL);
        self.gen_int32(argc as i32, iseq);
    }

    pub fn gen_jmp(&self, dst: i32, iseq: &mut ByteCode) {
        iseq.push(VMInst::JMP);
        self.gen_int32(dst, iseq);
    }

    pub fn gen_jmp_if_false(&self, dst: i32, iseq: &mut ByteCode) {
        iseq.push(VMInst::JMP_IF_FALSE);
        self.gen_int32(dst, iseq);
    }

    pub fn gen_jmp_unwind(&self, dst: i32, scope_level: u32, try_level: u32, iseq: &mut ByteCode) {
        iseq.push(VMInst::JMP_UNWIND);
        self.gen_int32(dst, iseq);
        self.gen_uint32(scope_level, iseq);
        self.gen_uint32(try_level, iseq);
    }

    pub fn gen_return(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::RETURN);
    }

    pub fn gen_return_try(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::RETURN_TRY);
        self.gen_int32(0, iseq);
    }

    pub fn gen_push_scope(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::PUSH_SCOPE);
    }

    pub fn gen_pop_scope(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::POP_SCOPE);
    }

    pub fn gen_update_parent_scope(&self, iseq: &mut ByteCode) {
        iseq.push(VMInst::UPDATE_PARENT_SCOPE);
    }

    pub fn gen_get_value(&mut self, name: &String, iseq: &mut ByteCode) {
        let id = self.add_const_string(name);
        iseq.push(VMInst::GET_VALUE);
        self.gen_int32(id as i32, iseq);
    }

    pub fn gen_set_value(&mut self, name: &String, iseq: &mut ByteCode) {
        let id = self.add_const_string(name);
        iseq.push(VMInst::SET_VALUE);
        self.gen_int32(id as i32, iseq);
    }

    pub fn gen_decl_var(&mut self, name: &String, iseq: &mut ByteCode) {
        let id = self.add_const_string(name);
        iseq.push(VMInst::DECL_VAR);
        self.gen_int32(id as i32, iseq);
    }

    pub fn gen_decl_const(&mut self, name: &String, iseq: &mut ByteCode) {
        let id = self.add_const_string(name);
        iseq.push(VMInst::DECL_CONST);
        self.gen_int32(id as i32, iseq);
    }

    pub fn gen_decl_let(&mut self, name: &String, iseq: &mut ByteCode) {
        let id = self.add_const_string(name);
        iseq.push(VMInst::DECL_LET);
        self.gen_int32(id as i32, iseq);
    }

    pub fn gen_cond_op(&mut self, iseq: &mut ByteCode) {
        iseq.push(VMInst::COND_OP);
    }

    pub fn gen_loop_start(&mut self, iseq: &mut ByteCode) {
        iseq.push(VMInst::LOOP_START);
        self.gen_int32(0, iseq);
    }

    pub fn gen_throw(&mut self, iseq: &mut ByteCode) {
        iseq.push(VMInst::THROW);
    }

    pub fn gen_enter_try(&mut self, iseq: &mut ByteCode) {
        iseq.push(VMInst::ENTER_TRY);
        self.gen_int32(0, iseq); // distance from ENTER_TRY to CATCH
        self.gen_int32(0, iseq); // distance from ENTER_TRY to FINALLY
    }

    pub fn gen_leave_try(&mut self, iseq: &mut ByteCode) {
        iseq.push(VMInst::LEAVE_TRY);
    }

    pub fn gen_catch(&mut self, iseq: &mut ByteCode) {
        iseq.push(VMInst::CATCH);
    }

    pub fn gen_finally(&mut self, iseq: &mut ByteCode) {
        iseq.push(VMInst::FINALLY);
    }

    // Utils

    pub fn gen_int8(&self, n: i8, iseq: &mut ByteCode) {
        iseq.push(n as u8);
    }

    pub fn gen_int32(&self, n: i32, iseq: &mut ByteCode) {
        iseq.push(((n >> 0) & 0xff as i32) as u8);
        iseq.push(((n >> 8) & 0xff as i32) as u8);
        iseq.push(((n >> 16) & 0xff as i32) as u8);
        iseq.push(((n >> 24) & 0xff as i32) as u8);
    }

    pub fn gen_uint32(&self, n: u32, iseq: &mut ByteCode) {
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

    fn add_const_string(&mut self, name: &String) -> usize {
        for (i, string) in self.const_table.string.iter().enumerate() {
            if name == string {
                return i;
            }
        }

        let id = self.const_table.string.len();
        self.const_table.string.push(name.clone());
        id
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

pub fn show(code: &ByteCode, const_table: &ConstantTable) {
    let mut i = 0;
    while i < code.len() {
        show_inst(code, i, const_table);
        println!();
        i = i + if let Some(size) = VMInst::get_inst_size(code[i]) {
            size
        } else {
            unreachable!("inst_size not defined.");
        };
    }
}

pub fn show_inst(code: &ByteCode, i: usize, const_table: &ConstantTable) {
    print!("{:04x} ", i);
    match code[i] {
        VMInst::END => {
            print!("End");
        }
        VMInst::CREATE_CONTEXT => {
            print!("CreateContext");
        }
        VMInst::CONSTRUCT => {
            let int32 = read_int32(code, i + 1);
            print!("Construct {} params", int32);
        }
        VMInst::CREATE_OBJECT => {
            let int32 = read_int32(code, i + 1);
            print!("CreateObject {} params", int32);
        }
        VMInst::CREATE_ARRAY => {
            let int32 = read_int32(code, i + 1);
            print!("CreateArray {} params", int32);
        }
        VMInst::PUSH_INT8 => {
            let int8 = code[i + 1] as i32;
            print!("PushInt8 {}", int8);
        }
        VMInst::PUSH_INT32 => {
            let int32 = read_int32(code, i + 1);
            print!("PushInt32 {}", int32);
        }
        VMInst::PUSH_FALSE => {
            print!("PushFalse");
        }
        VMInst::PUSH_TRUE => {
            print!("PushTrue");
        }
        VMInst::PUSH_CONST => {
            let int32 = read_int32(code, i + 1);
            let value = &const_table.value[int32 as usize];
            print!("PushConst {}", value.format(1, false));
        }
        VMInst::PUSH_THIS => {
            print!("PushThis");
        }
        VMInst::PUSH_ARGUMENTS => {
            print!("PushArguments");
        }
        VMInst::PUSH_UNDEFINED => {
            print!("PushUndefined");
        }
        VMInst::LNOT => {
            print!("LogNot");
        }
        VMInst::POSI => {
            print!("Posi");
        }
        VMInst::NEG => {
            print!("Neg");
        }
        VMInst::ADD => {
            print!("Add");
        }
        VMInst::SUB => {
            print!("Sub");
        }
        VMInst::MUL => {
            print!("Mul");
        }
        VMInst::DIV => {
            print!("Div");
        }
        VMInst::REM => {
            print!("Rem");
        }
        VMInst::LT => {
            print!("Lt");
        }
        VMInst::GT => {
            print!("Gt");
        }
        VMInst::LE => {
            print!("Le");
        }
        VMInst::GE => {
            print!("Ge");
        }
        VMInst::EQ => {
            print!("Eq");
        }
        VMInst::NE => {
            print!("Ne");
        }
        VMInst::SEQ => {
            print!("SEq");
        }
        VMInst::SNE => {
            print!("SNeg");
        }
        VMInst::AND => {
            print!("BitwiseAnd");
        }
        VMInst::OR => {
            print!("BitwiseOr");
        }
        VMInst::XOR => {
            print!("BitwiseXor");
        }
        VMInst::SHL => {
            print!("Shift-L");
        }
        VMInst::SHR => {
            print!("Shift-R");
        }
        VMInst::ZFSHR => {
            print!("ZeroFill-Shift-R");
        }
        VMInst::GET_MEMBER => {
            print!("GetMember");
        }
        VMInst::SET_MEMBER => {
            print!("SetMember");
        }
        VMInst::JMP_IF_FALSE => {
            let int32 = read_int32(code, i + 1);
            print!("JmpIfFalse {:04x}", i as i32 + int32 + 5);
        }
        VMInst::JMP => {
            let int32 = read_int32(code, i + 1);
            print!("Jmp {:04x}", i as i32 + int32 + 5);
        }
        VMInst::JMP_UNWIND => {
            let dest = read_int32(code, i + 1);
            let pop = read_int32(code, i + 5);
            print!("JmpUnwind {:04x} {}", i as i32 + dest + 5, pop);
        }
        VMInst::CALL => {
            let int32 = read_int32(code, i + 1);
            print!("Call {} params", int32);
        }
        VMInst::RETURN => {
            print!("Return");
        }
        VMInst::DOUBLE => {
            print!("Double");
        }
        VMInst::POP => {
            print!("Pop");
        }
        VMInst::LAND => {
            print!("LogAnd");
        }
        VMInst::LOR => {
            print!("LogOr");
        }
        VMInst::UPDATE_PARENT_SCOPE => {
            print!("UpdateParentScope");
        }
        VMInst::GET_VALUE => {
            let int32 = read_int32(code, i + 1);
            let name = &const_table.string[int32 as usize];
            print!("GetValue '{}'", name);
        }
        VMInst::SET_VALUE => {
            let int32 = read_int32(code, i + 1);
            let name = &const_table.string[int32 as usize];
            print!("SetValue '{}'", name);
        }
        VMInst::DECL_VAR => {
            let int32 = read_int32(code, i + 1);
            let name = &const_table.string[int32 as usize];
            print!("DeclVar '{}'", name);
        }
        VMInst::DECL_CONST => {
            let int32 = read_int32(code, i + 1);
            let name = &const_table.string[int32 as usize];
            print!("DeclConst '{}'", name);
        }
        VMInst::DECL_LET => {
            let int32 = read_int32(code, i + 1);
            let name = &const_table.string[int32 as usize];
            print!("DeclLet '{}'", name);
        }
        VMInst::COND_OP => {
            print!("CondOp");
        }
        VMInst::LOOP_START => {
            print!("LoopStart");
        }
        VMInst::THROW => {
            print!("Throw");
        }
        VMInst::ENTER_TRY => {
            print!("EnterTry");
        }
        VMInst::LEAVE_TRY => {
            print!("LeaveTry");
        }
        VMInst::CATCH => {
            print!("Catch");
        }
        VMInst::FINALLY => {
            print!("Finally");
        }
        VMInst::RETURN_TRY => {
            print!("ReturnTry");
        }
        VMInst::PUSH_SCOPE => {
            print!("PushScope");
        }
        VMInst::POP_SCOPE => {
            print!("PopScope");
        }
        VMInst::NOT => {
            print!("BitwiseNot");
        }
        _ => unreachable!("sorry. need to implement more opcodes"),
    }
}
