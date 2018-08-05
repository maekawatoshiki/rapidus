use vm::{
    ConstantTable, Inst, PUSH_INT32, PUSH_INT8, Value, ADD, CALL, CONSTRACT, CREATE_CONTEXT,
    CREATE_OBJECT, DIV, END, EQ, GE, GET_GLOBAL, GET_LOCAL, GET_MEMBER, GT, JMP, JMP_IF_FALSE, LE,
    LT, MUL, NE, PUSH_CONST, PUSH_THIS, REM, RETURN, SET_GLOBAL, SET_LOCAL, SET_MEMBER, SUB,
};

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
        let id = self.const_table.value.len();
        self.const_table.string.push(name);
        self.gen_int32(id as i32, insts);
    }

    pub fn gen_set_global(&mut self, name: String, insts: &mut Vec<u8>) {
        insts.push(SET_GLOBAL);
        let id = self.const_table.value.len();
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

    pub fn gen_jmp(&self, dst: u32, insts: &mut Vec<u8>) {
        insts.push(JMP);
        self.gen_int32(dst as i32, insts);
    }

    pub fn gen_jmp_if_false(&self, dst: u32, insts: &mut Vec<u8>) {
        insts.push(JMP_IF_FALSE);
        self.gen_int32(dst as i32, insts);
    }

    pub fn gen_return(&self, insts: &mut Vec<u8>) {
        insts.push(RETURN);
    }

    // Utils

    pub fn gen_int8(&self, n: i8, insts: &mut Vec<u8>) {
        insts.push(n as u8);
    }

    pub fn gen_int32(&self, n: i32, insts: &mut Vec<u8>) {
        insts.push(((n << 24) >> 24) as u8);
        insts.push(((n << 16) >> 24) as u8);
        insts.push(((n << 8) >> 24) as u8);
        insts.push((n >> 24) as u8);
    }

    pub fn replace_int32(&self, n: i32, insts: &mut [u8]) {
        insts[0] = ((n << 24) >> 24) as u8;
        insts[1] = ((n << 16) >> 24) as u8;
        insts[2] = ((n << 8) >> 24) as u8;
        insts[3] = (n >> 24) as u8;
    }

    // Convert VM::Inst into Bytecode

    pub fn convert(&mut self, insts: &Vec<Inst>) -> Vec<u8> {
        let mut output = vec![];
        for inst in insts {
            match inst {
                Inst::PushThis => self.gen_push_this(&mut output),
                Inst::Push(Value::Number(n)) if *n - (*n).floor() == 0.0 => {
                    println!("here");
                    self.gen_push_int32(*n as i32, &mut output)
                }
                Inst::Push(val) => self.gen_push_const(val.clone(), &mut output),
                Inst::Constract(argc) => self.gen_constract(*argc, &mut output),
                Inst::CreateObject(len) => self.gen_create_object(*len, &mut output),
                Inst::Add => self.gen_add(&mut output),
                Inst::Sub => self.gen_sub(&mut output),
                Inst::Mul => self.gen_mul(&mut output),
                Inst::Div => self.gen_div(&mut output),
                Inst::Rem => self.gen_rem(&mut output),
                Inst::Lt => self.gen_lt(&mut output),
                Inst::Le => self.gen_le(&mut output),
                Inst::Gt => self.gen_gt(&mut output),
                Inst::Ge => self.gen_ge(&mut output),
                Inst::Eq => self.gen_eq(&mut output),
                Inst::Ne => self.gen_ne(&mut output),
                Inst::GetMember => self.gen_get_member(&mut output),
                Inst::SetMember => self.gen_set_member(&mut output),
                Inst::GetGlobal(name) => self.gen_get_global(name.clone(), &mut output),
                Inst::GetLocal(id) => self.gen_get_local(*id as u32, &mut output),
                Inst::SetGlobal(name) => self.gen_set_global(name.clone(), &mut output),
                Inst::SetLocal(id) => self.gen_set_local(*id as u32, &mut output),
                Inst::Call(argc) => self.gen_call(*argc as u32, &mut output),
                Inst::Jmp(dst) => self.gen_jmp(*dst as u32, &mut output),
                Inst::JmpIfFalse(dst) => self.gen_jmp(*dst as u32, &mut output),
                Inst::AllocLocalVar(n, argc) => {
                    self.gen_create_context(*n as usize, *argc as usize, &mut output)
                }
                Inst::Return => self.gen_return(&mut output),
                Inst::End => self.gen_end(&mut output),
                _ => unimplemented!(),
            }
        }
        output
    }
}
