use vm::{
    ConstantTable, HeapAddr, Inst, PUSH_INT32, PUSH_INT8, Value, ADD, CALL, CONSTRACT,
    CREATE_CONTEXT, CREATE_OBJECT, DIV, END, EQ, GE, GET_GLOBAL, GET_LOCAL, GET_MEMBER, GT, JMP,
    JMP_IF_FALSE, LE, LT, MUL, NE, PUSH_CONST, PUSH_FALSE, PUSH_THIS, PUSH_TRUE, REM, RETURN,
    SET_GLOBAL, SET_LOCAL, SET_MEMBER, SUB,
};

use std::boxed::Box;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::CStr;
use std::rc::Rc;

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
    pub fn gen_end(&self, insts: &mut Vec<u8>) {
        insts.push(END);
    }

    pub fn gen_create_context(&self, n: usize, argc: usize, insts: &mut Vec<u8>) {
        insts.push(CREATE_CONTEXT);
        self.gen_int32(n as i32, insts);
        self.gen_int32(argc as i32, insts);
    }

    pub fn gen_constract(&self, argc: usize, insts: &mut Vec<u8>) {
        insts.push(CONSTRACT);
        self.gen_int32(argc as i32, insts);
    }

    pub fn gen_create_object(&self, len: usize, insts: &mut Vec<u8>) {
        insts.push(CREATE_OBJECT);
        self.gen_int32(len as i32, insts);
    }

    pub fn gen_push_int8(&self, n: i8, insts: &mut Vec<u8>) {
        insts.push(PUSH_INT8);
        self.gen_int8(n, insts);
    }

    pub fn gen_push_int32(&self, n: i32, insts: &mut Vec<u8>) {
        insts.push(PUSH_INT32);
        self.gen_int32(n, insts);
    }

    pub fn gen_push_bool(&self, b: bool, insts: &mut Vec<u8>) {
        insts.push(if b { PUSH_TRUE } else { PUSH_FALSE })
    }

    pub fn gen_push_const(&mut self, val: Value, insts: &mut Vec<u8>) {
        insts.push(PUSH_CONST);
        let id = self.const_table.value.len();
        self.const_table.value.push(val);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_push_this(&self, insts: &mut Vec<u8>) {
        insts.push(PUSH_THIS);
    }

    pub fn gen_add(&self, insts: &mut Vec<u8>) {
        insts.push(ADD);
    }
    pub fn gen_sub(&self, insts: &mut Vec<u8>) {
        insts.push(SUB);
    }
    pub fn gen_mul(&self, insts: &mut Vec<u8>) {
        insts.push(MUL);
    }
    pub fn gen_div(&self, insts: &mut Vec<u8>) {
        insts.push(DIV);
    }
    pub fn gen_rem(&self, insts: &mut Vec<u8>) {
        insts.push(REM);
    }
    pub fn gen_lt(&self, insts: &mut Vec<u8>) {
        insts.push(LT);
    }
    pub fn gen_gt(&self, insts: &mut Vec<u8>) {
        insts.push(GT);
    }
    pub fn gen_le(&self, insts: &mut Vec<u8>) {
        insts.push(LE);
    }
    pub fn gen_ge(&self, insts: &mut Vec<u8>) {
        insts.push(GE);
    }
    pub fn gen_eq(&self, insts: &mut Vec<u8>) {
        insts.push(EQ);
    }
    pub fn gen_ne(&self, insts: &mut Vec<u8>) {
        insts.push(NE);
    }

    pub fn gen_get_member(&self, insts: &mut Vec<u8>) {
        insts.push(GET_MEMBER);
    }

    pub fn gen_set_member(&self, insts: &mut Vec<u8>) {
        insts.push(SET_MEMBER);
    }

    pub fn gen_get_global(&mut self, name: String, insts: &mut Vec<u8>) {
        insts.push(GET_GLOBAL);
        let id = self.const_table.string.len();
        self.const_table.string.push(name);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_set_global(&mut self, name: String, insts: &mut Vec<u8>) {
        insts.push(SET_GLOBAL);
        let id = self.const_table.string.len();
        self.const_table.string.push(name);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_get_local(&self, id: u32, insts: &mut Vec<u8>) {
        insts.push(GET_LOCAL);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_set_local(&self, id: u32, insts: &mut Vec<u8>) {
        insts.push(SET_LOCAL);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_call(&self, argc: u32, insts: &mut Vec<u8>) {
        insts.push(CALL);
        self.gen_int32(argc as i32, insts);
    }

    pub fn gen_jmp(&self, dst: i32, insts: &mut Vec<u8>) {
        insts.push(JMP);
        self.gen_int32(dst, insts);
    }

    pub fn gen_jmp_if_false(&self, dst: i32, insts: &mut Vec<u8>) {
        insts.push(JMP_IF_FALSE);
        self.gen_int32(dst, insts);
    }

    pub fn gen_return(&self, insts: &mut Vec<u8>) {
        insts.push(RETURN);
    }

    // Utils

    pub fn gen_int8(&self, n: i8, insts: &mut Vec<u8>) {
        insts.push(n as u8);
    }

    pub fn gen_int32(&self, n: i32, insts: &mut Vec<u8>) {
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
