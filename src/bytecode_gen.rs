use id::Id;
use vm::{
    ConstantTable, PUSH_INT32, PUSH_INT8, Value, ADD, ASG_FREST_PARAM, CALL, CONSTRUCT,
    CREATE_ARRAY, CREATE_CONTEXT, CREATE_OBJECT, DIV, END, EQ, GE, GET_ARG_LOCAL, GET_GLOBAL,
    GET_LOCAL, GET_MEMBER, GT, JMP, JMP_IF_FALSE, LE, LT, MUL, NE, NEG, PUSH_ARGUMENTS, PUSH_CONST,
    PUSH_FALSE, PUSH_THIS, PUSH_TRUE, REM, RETURN, SET_ARG_LOCAL, SET_GLOBAL, SET_LOCAL,
    SET_MEMBER, SUB,
};

pub type ByteCode = Vec<u8>;

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
        insts.push(END);
    }

    pub fn gen_create_context(&self, num_local_var: usize, insts: &mut ByteCode) {
        insts.push(CREATE_CONTEXT);
        self.gen_int32(num_local_var as i32, insts);
    }

    pub fn gen_constract(&self, argc: usize, insts: &mut ByteCode) {
        insts.push(CONSTRUCT);
        self.gen_int32(argc as i32, insts);
    }

    pub fn gen_create_object(&self, len: usize, insts: &mut ByteCode) {
        insts.push(CREATE_OBJECT);
        self.gen_int32(len as i32, insts);
    }

    pub fn gen_create_array(&self, len: usize, insts: &mut ByteCode) {
        insts.push(CREATE_ARRAY);
        self.gen_int32(len as i32, insts);
    }

    pub fn gen_push_int8(&self, n: i8, insts: &mut ByteCode) {
        insts.push(PUSH_INT8);
        self.gen_int8(n, insts);
    }

    pub fn gen_push_int32(&self, n: i32, insts: &mut ByteCode) {
        insts.push(PUSH_INT32);
        self.gen_int32(n, insts);
    }

    pub fn gen_push_bool(&self, b: bool, insts: &mut ByteCode) {
        insts.push(if b { PUSH_TRUE } else { PUSH_FALSE })
    }

    pub fn gen_push_const(&mut self, val: Value, insts: &mut ByteCode) {
        insts.push(PUSH_CONST);
        let id = self.const_table.value.len();
        self.const_table.value.push(val);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_push_this(&self, insts: &mut ByteCode) {
        insts.push(PUSH_THIS);
    }

    pub fn gen_push_arguments(&self, insts: &mut ByteCode) {
        insts.push(PUSH_ARGUMENTS);
    }

    pub fn gen_neg(&self, insts: &mut ByteCode) {
        insts.push(NEG);
    }

    pub fn gen_add(&self, insts: &mut ByteCode) {
        insts.push(ADD);
    }
    pub fn gen_sub(&self, insts: &mut ByteCode) {
        insts.push(SUB);
    }
    pub fn gen_mul(&self, insts: &mut ByteCode) {
        insts.push(MUL);
    }
    pub fn gen_div(&self, insts: &mut ByteCode) {
        insts.push(DIV);
    }
    pub fn gen_rem(&self, insts: &mut ByteCode) {
        insts.push(REM);
    }
    pub fn gen_lt(&self, insts: &mut ByteCode) {
        insts.push(LT);
    }
    pub fn gen_gt(&self, insts: &mut ByteCode) {
        insts.push(GT);
    }
    pub fn gen_le(&self, insts: &mut ByteCode) {
        insts.push(LE);
    }
    pub fn gen_ge(&self, insts: &mut ByteCode) {
        insts.push(GE);
    }
    pub fn gen_eq(&self, insts: &mut ByteCode) {
        insts.push(EQ);
    }
    pub fn gen_ne(&self, insts: &mut ByteCode) {
        insts.push(NE);
    }

    pub fn gen_get_member(&self, insts: &mut ByteCode) {
        insts.push(GET_MEMBER);
    }

    pub fn gen_set_member(&self, insts: &mut ByteCode) {
        insts.push(SET_MEMBER);
    }

    pub fn gen_get_global(&mut self, name: String, insts: &mut ByteCode) {
        insts.push(GET_GLOBAL);
        let id = self.const_table.string.len();
        self.const_table.string.push(name);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_set_global(&mut self, name: String, insts: &mut ByteCode) {
        insts.push(SET_GLOBAL);
        let id = self.const_table.string.len();
        self.const_table.string.push(name);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_get_local(&self, id: u32, insts: &mut ByteCode) {
        insts.push(GET_LOCAL);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_set_local(&self, id: u32, insts: &mut ByteCode) {
        insts.push(SET_LOCAL);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_get_arg_local(&self, id: u32, insts: &mut ByteCode) {
        insts.push(GET_ARG_LOCAL);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_set_arg_local(&self, id: u32, insts: &mut ByteCode) {
        insts.push(SET_ARG_LOCAL);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_call(&self, argc: u32, insts: &mut ByteCode) {
        insts.push(CALL);
        self.gen_int32(argc as i32, insts);
    }

    pub fn gen_jmp(&self, dst: i32, insts: &mut ByteCode) {
        insts.push(JMP);
        self.gen_int32(dst, insts);
    }

    pub fn gen_jmp_if_false(&self, dst: i32, insts: &mut ByteCode) {
        insts.push(JMP_IF_FALSE);
        self.gen_int32(dst, insts);
    }

    pub fn gen_return(&self, insts: &mut ByteCode) {
        insts.push(RETURN);
    }

    pub fn gen_assign_func_rest_param(
        &self,
        num_func_params: usize,
        dst_var_id: Id,
        insts: &mut ByteCode,
    ) {
        insts.push(ASG_FREST_PARAM);
        self.gen_int32(num_func_params as i32, insts);
        self.gen_int32(dst_var_id as i32, insts);
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
            END => {
                println!("End");
                i += 1
            }
            CREATE_CONTEXT => {
                println!("CreateContext");
                i += 5
            }
            CONSTRUCT => {
                println!("Construct");
                i += 5
            }
            CREATE_OBJECT => {
                println!("CreateObject");
                i += 5
            }
            PUSH_INT8 => {
                println!("PushInt8");
                i += 2
            }
            PUSH_INT32 => {
                println!("PushInt32");
                i += 5
            }
            PUSH_FALSE => {
                println!("PushFalse");
                i += 1
            }
            PUSH_TRUE => {
                println!("PushFalse");
                i += 1
            }
            PUSH_CONST => {
                println!("PushConst");
                i += 5
            }
            PUSH_THIS => {
                println!("PushThis");
                i += 1
            }
            PUSH_ARGUMENTS => {
                println!("PushArguments");
                i += 1
            }
            NEG => {
                println!("Neg");
                i += 1
            }
            ADD => {
                println!("Add");
                i += 1
            }
            SUB => {
                println!("Sub");
                i += 1
            }
            MUL => {
                println!("Mul");
                i += 1
            }
            DIV => {
                println!("Div");
                i += 1
            }
            REM => {
                println!("Rem");
                i += 1
            }
            LT => {
                println!("Lt");
                i += 1
            }
            GT => {
                println!("Gt");
                i += 1
            }
            LE => {
                println!("Le");
                i += 1
            }
            GE => {
                println!("Ge");
                i += 1
            }
            EQ => {
                println!("Eq");
                i += 1
            }
            NE => {
                println!("Ne");
                i += 1
            }
            GET_MEMBER => {
                println!("GetMember");
                i += 1
            }
            SET_MEMBER => {
                println!("SetMember");
                i += 1
            }
            GET_GLOBAL => {
                println!("GetGlobal");
                i += 5
            }
            SET_GLOBAL => {
                println!("SetGlobal");
                i += 5
            }
            GET_LOCAL => {
                println!("GetLocal",);
                i += 5
            }
            SET_LOCAL => {
                println!("SetLocal",);
                i += 5
            }
            GET_ARG_LOCAL => {
                println!("GetArgLocal",);
                i += 5
            }
            SET_ARG_LOCAL => {
                println!("SetArgLocal",);
                i += 5
            }
            JMP_IF_FALSE => {
                println!("JmpIfFalse");
                i += 5
            }
            JMP => {
                println!("Jmp");
                i += 5
            }
            CALL => {
                println!("Call");
                i += 5
            }
            RETURN => {
                println!("Return");
                i += 1
            }
            ASG_FREST_PARAM => {
                println!("AssignFunctionRestParam");
                i += 9
            }
            _ => unreachable!(),
        }
    }
}
