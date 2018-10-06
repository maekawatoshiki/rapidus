use id::Id;
use vm::{ConstantTable, Value};

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
    pub const NEG: u8 = 0x0c;
    pub const ADD: u8 = 0x0d;
    pub const SUB: u8 = 0x0e;
    pub const MUL: u8 = 0x0f;
    pub const DIV: u8 = 0x10;
    pub const REM: u8 = 0x11;
    pub const LT: u8 = 0x12;
    pub const GT: u8 = 0x13;
    pub const LE: u8 = 0x14;
    pub const GE: u8 = 0x15;
    pub const EQ: u8 = 0x16;
    pub const NE: u8 = 0x17;
    pub const SEQ: u8 = 0x18;
    pub const SNE: u8 = 0x19;
    pub const AND: u8 = 0x1a;
    pub const OR: u8 = 0x1b;
    pub const GET_MEMBER: u8 = 0x1c;
    pub const SET_MEMBER: u8 = 0x1d;
    pub const GET_GLOBAL: u8 = 0x1e;
    pub const SET_GLOBAL: u8 = 0x1f;
    pub const GET_LOCAL: u8 = 0x20;
    pub const SET_LOCAL: u8 = 0x21;
    pub const GET_ARG_LOCAL: u8 = 0x22;
    pub const SET_ARG_LOCAL: u8 = 0x23;
    pub const JMP_IF_FALSE: u8 = 0x24;
    pub const JMP: u8 = 0x25;
    pub const CALL: u8 = 0x26;
    pub const RETURN: u8 = 0x27;
    pub const ASG_FREST_PARAM: u8 = 0x28;
    pub const DOUBLE: u8 = 0x29;
    pub const POP: u8 = 0x2a;
    pub const LAND: u8 = 0x2b;
    pub const LOR: u8 = 0x2c;
    //
    pub const SET_CUR_CALLOBJ: u8 = 0x2d;
    pub const GET_NAME: u8 = 0x2e;
    pub const SET_NAME: u8 = 0x2f;
    pub const DECL_VAR: u8 = 0x30;

    pub fn get_inst_size(inst: u8) -> Option<usize> {
        match inst {
            ASG_FREST_PARAM => Some(9),
            CREATE_CONTEXT => Some(5),
            CONSTRUCT | CREATE_OBJECT | PUSH_CONST | PUSH_INT32 | SET_GLOBAL | GET_LOCAL
            | SET_ARG_LOCAL | GET_ARG_LOCAL | CREATE_ARRAY | SET_LOCAL | JMP_IF_FALSE | JMP
            | DECL_VAR | SET_NAME | GET_NAME | GET_GLOBAL | CALL => Some(5),
            PUSH_INT8 => Some(2),
            PUSH_FALSE | END | PUSH_TRUE | PUSH_THIS | ADD | SUB | MUL | DIV | REM | LT
            | PUSH_ARGUMENTS | NEG | GT | LE | GE | EQ | NE | GET_MEMBER | RETURN | SNE | LAND
            | POP | DOUBLE | AND | OR | SEQ | SET_MEMBER | LOR | SET_CUR_CALLOBJ => Some(1),
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
    pub fn gen_end(&self, insts: &mut ByteCode) {
        insts.push(VMInst::END);
    }

    pub fn gen_create_context(&self, num_local_var: usize, insts: &mut ByteCode) {
        insts.push(VMInst::CREATE_CONTEXT);
        self.gen_int32(num_local_var as i32, insts);
    }

    pub fn gen_constract(&self, argc: usize, insts: &mut ByteCode) {
        insts.push(VMInst::CONSTRUCT);
        self.gen_int32(argc as i32, insts);
    }

    pub fn gen_create_object(&self, len: usize, insts: &mut ByteCode) {
        insts.push(VMInst::CREATE_OBJECT);
        self.gen_int32(len as i32, insts);
    }

    pub fn gen_create_array(&self, len: usize, insts: &mut ByteCode) {
        insts.push(VMInst::CREATE_ARRAY);
        self.gen_int32(len as i32, insts);
    }

    pub fn gen_push_int8(&self, n: i8, insts: &mut ByteCode) {
        insts.push(VMInst::PUSH_INT8);
        self.gen_int8(n, insts);
    }

    pub fn gen_push_int32(&self, n: i32, insts: &mut ByteCode) {
        insts.push(VMInst::PUSH_INT32);
        self.gen_int32(n, insts);
    }

    pub fn gen_push_bool(&self, b: bool, insts: &mut ByteCode) {
        insts.push(if b {
            VMInst::PUSH_TRUE
        } else {
            VMInst::PUSH_FALSE
        })
    }

    pub fn gen_push_const(&mut self, val: Value, insts: &mut ByteCode) {
        insts.push(VMInst::PUSH_CONST);
        let id = self.const_table.value.len();
        self.const_table.value.push(val);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_push_this(&self, insts: &mut ByteCode) {
        insts.push(VMInst::PUSH_THIS);
    }

    pub fn gen_push_arguments(&self, insts: &mut ByteCode) {
        insts.push(VMInst::PUSH_ARGUMENTS);
    }

    pub fn gen_neg(&self, insts: &mut ByteCode) {
        insts.push(VMInst::NEG);
    }

    pub fn gen_add(&self, insts: &mut ByteCode) {
        insts.push(VMInst::ADD);
    }
    pub fn gen_sub(&self, insts: &mut ByteCode) {
        insts.push(VMInst::SUB);
    }
    pub fn gen_mul(&self, insts: &mut ByteCode) {
        insts.push(VMInst::MUL);
    }
    pub fn gen_div(&self, insts: &mut ByteCode) {
        insts.push(VMInst::DIV);
    }
    pub fn gen_rem(&self, insts: &mut ByteCode) {
        insts.push(VMInst::REM);
    }
    pub fn gen_lt(&self, insts: &mut ByteCode) {
        insts.push(VMInst::LT);
    }
    pub fn gen_gt(&self, insts: &mut ByteCode) {
        insts.push(VMInst::GT);
    }
    pub fn gen_le(&self, insts: &mut ByteCode) {
        insts.push(VMInst::LE);
    }
    pub fn gen_ge(&self, insts: &mut ByteCode) {
        insts.push(VMInst::GE);
    }
    pub fn gen_eq(&self, insts: &mut ByteCode) {
        insts.push(VMInst::EQ);
    }
    pub fn gen_ne(&self, insts: &mut ByteCode) {
        insts.push(VMInst::NE);
    }
    pub fn gen_seq(&self, insts: &mut ByteCode) {
        insts.push(VMInst::SEQ);
    }
    pub fn gen_sne(&self, insts: &mut ByteCode) {
        insts.push(VMInst::SNE);
    }
    pub fn gen_and(&self, insts: &mut ByteCode) {
        insts.push(VMInst::AND);
    }
    pub fn gen_or(&self, insts: &mut ByteCode) {
        insts.push(VMInst::OR);
    }

    pub fn gen_land(&self, insts: &mut ByteCode) {
        insts.push(VMInst::LAND);
    }
    pub fn gen_lor(&self, insts: &mut ByteCode) {
        insts.push(VMInst::LOR);
    }

    pub fn gen_double(&self, insts: &mut ByteCode) {
        insts.push(VMInst::DOUBLE);
    }
    pub fn gen_pop(&self, insts: &mut ByteCode) {
        insts.push(VMInst::POP);
    }

    pub fn gen_get_member(&self, insts: &mut ByteCode) {
        insts.push(VMInst::GET_MEMBER);
    }

    pub fn gen_set_member(&self, insts: &mut ByteCode) {
        insts.push(VMInst::SET_MEMBER);
    }

    pub fn gen_get_global(&mut self, name: String, insts: &mut ByteCode) {
        insts.push(VMInst::GET_GLOBAL);
        let id = self.const_table.string.len();
        self.const_table.string.push(name);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_set_global(&mut self, name: String, insts: &mut ByteCode) {
        insts.push(VMInst::SET_GLOBAL);
        let id = self.const_table.string.len();
        self.const_table.string.push(name);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_get_local(&self, id: u32, insts: &mut ByteCode) {
        insts.push(VMInst::GET_LOCAL);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_set_local(&self, id: u32, insts: &mut ByteCode) {
        insts.push(VMInst::SET_LOCAL);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_get_arg_local(&self, id: u32, insts: &mut ByteCode) {
        insts.push(VMInst::GET_ARG_LOCAL);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_set_arg_local(&self, id: u32, insts: &mut ByteCode) {
        insts.push(VMInst::SET_ARG_LOCAL);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_call(&self, argc: u32, insts: &mut ByteCode) {
        insts.push(VMInst::CALL);
        self.gen_int32(argc as i32, insts);
    }

    pub fn gen_jmp(&self, dst: i32, insts: &mut ByteCode) {
        insts.push(VMInst::JMP);
        self.gen_int32(dst, insts);
    }

    pub fn gen_jmp_if_false(&self, dst: i32, insts: &mut ByteCode) {
        insts.push(VMInst::JMP_IF_FALSE);
        self.gen_int32(dst, insts);
    }

    pub fn gen_return(&self, insts: &mut ByteCode) {
        insts.push(VMInst::RETURN);
    }

    pub fn gen_assign_func_rest_param(
        &self,
        num_func_params: usize,
        dst_var_id: Id,
        insts: &mut ByteCode,
    ) {
        insts.push(VMInst::ASG_FREST_PARAM);
        self.gen_int32(num_func_params as i32, insts);
        self.gen_int32(dst_var_id as i32, insts);
    }

    pub fn gen_set_cur_callobj(&self, insts: &mut ByteCode) {
        insts.push(VMInst::SET_CUR_CALLOBJ);
    }

    pub fn gen_get_name(&mut self, name: &String, insts: &mut ByteCode) {
        let id = (|| {
            for (i, string) in self.const_table.string.iter().enumerate() {
                if name == string {
                    return i;
                }
            }

            let id = self.const_table.string.len();
            self.const_table.string.push(name.clone());
            id
        })();
        insts.push(VMInst::GET_NAME);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_set_name(&mut self, name: &String, insts: &mut ByteCode) {
        let id = (|| {
            for (i, string) in self.const_table.string.iter().enumerate() {
                if name == string {
                    return i;
                }
            }

            let id = self.const_table.string.len();
            self.const_table.string.push(name.clone());
            id
        })();
        insts.push(VMInst::SET_NAME);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_decl_var(&mut self, name: &String, insts: &mut ByteCode) {
        let id = (|| {
            for (i, string) in self.const_table.string.iter().enumerate() {
                if name == string {
                    return i;
                }
            }

            let id = self.const_table.string.len();
            self.const_table.string.push(name.clone());
            id
        })();
        insts.push(VMInst::DECL_VAR);
        self.gen_int32(id as i32, insts);
    }

    // Utils

    pub fn gen_int8(&self, n: i8, insts: &mut ByteCode) {
        insts.push(n as u8);
    }

    pub fn gen_int32(&self, n: i32, insts: &mut ByteCode) {
        insts.push(((n >> 0) & 0xff as i32) as u8);
        insts.push(((n >> 8) & 0xff as i32) as u8);
        insts.push(((n >> 16) & 0xff as i32) as u8);
        insts.push(((n >> 24) & 0xff as i32) as u8);
    }

    pub fn replace_int32(&self, n: i32, insts: &mut [u8]) {
        insts[3] = (n >> 24) as u8;
        insts[2] = (n >> 16) as u8;
        insts[1] = (n >> 8) as u8;
        insts[0] = (n >> 0) as u8;
    }
}

pub fn slice_to_int32(insts: &[u8]) -> i32 {
    ((insts[3] as i32) << 24)
        + ((insts[2] as i32) << 16)
        + ((insts[1] as i32) << 8)
        + (insts[0] as i32)
}

pub fn show(code: &ByteCode) {
    let mut i = 0;
    while i < code.len() {
        print!("{:04x} ", i);
        match code[i] {
            VMInst::END => {
                println!("End");
                i += 1
            }
            VMInst::CREATE_CONTEXT => {
                println!("CreateContext");
                i += 5
            }
            VMInst::CONSTRUCT => {
                println!("Construct");
                i += 5
            }
            VMInst::CREATE_OBJECT => {
                println!("CreateObject");
                i += 5
            }
            VMInst::PUSH_INT8 => {
                println!("PushInt8");
                i += 2
            }
            VMInst::PUSH_INT32 => {
                println!("PushInt32");
                i += 5
            }
            VMInst::PUSH_FALSE => {
                println!("PushFalse");
                i += 1
            }
            VMInst::PUSH_TRUE => {
                println!("PushTrue");
                i += 1
            }
            VMInst::PUSH_CONST => {
                println!("PushConst");
                i += 5
            }
            VMInst::PUSH_THIS => {
                println!("PushThis");
                i += 1
            }
            VMInst::PUSH_ARGUMENTS => {
                println!("PushArguments");
                i += 1
            }
            VMInst::NEG => {
                println!("Neg");
                i += 1
            }
            VMInst::ADD => {
                println!("Add");
                i += 1
            }
            VMInst::SUB => {
                println!("Sub");
                i += 1
            }
            VMInst::MUL => {
                println!("Mul");
                i += 1
            }
            VMInst::DIV => {
                println!("Div");
                i += 1
            }
            VMInst::REM => {
                println!("Rem");
                i += 1
            }
            VMInst::LT => {
                println!("Lt");
                i += 1
            }
            VMInst::GT => {
                println!("Gt");
                i += 1
            }
            VMInst::LE => {
                println!("Le");
                i += 1
            }
            VMInst::GE => {
                println!("Ge");
                i += 1
            }
            VMInst::EQ => {
                println!("Eq");
                i += 1
            }
            VMInst::NE => {
                println!("Ne");
                i += 1
            }
            VMInst::GET_MEMBER => {
                println!("GetMember");
                i += 1
            }
            VMInst::SET_MEMBER => {
                println!("SetMember");
                i += 1
            }
            VMInst::GET_GLOBAL => {
                println!("GetGlobal");
                i += 5
            }
            VMInst::SET_GLOBAL => {
                println!("SetGlobal");
                i += 5
            }
            VMInst::GET_LOCAL => {
                println!("GetLocal",);
                i += 5
            }
            VMInst::SET_LOCAL => {
                println!("SetLocal",);
                i += 5
            }
            VMInst::GET_ARG_LOCAL => {
                println!("GetArgLocal",);
                i += 5
            }
            VMInst::SET_ARG_LOCAL => {
                println!("SetArgLocal",);
                i += 5
            }
            VMInst::JMP_IF_FALSE => {
                println!("JmpIfFalse");
                i += 5
            }
            VMInst::JMP => {
                println!("Jmp");
                i += 5
            }
            VMInst::CALL => {
                println!("Call");
                i += 5
            }
            VMInst::RETURN => {
                println!("Return");
                i += 1
            }
            VMInst::ASG_FREST_PARAM => {
                println!("AssignFunctionRestParam");
                i += 9
            }
            VMInst::SET_CUR_CALLOBJ => {
                println!("SetCurCallObj");
                i += 1;
            }
            VMInst::GET_NAME => {
                println!("GetName");
                i += 5;
            }
            VMInst::SET_NAME => {
                println!("SetName");
                i += 5;
            }
            VMInst::DECL_VAR => {
                println!("DeclVar");
                i += 5;
            }
            _ => unreachable!(),
        }
    }
}
