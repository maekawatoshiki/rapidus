use vm::{
    ConstantTable, PUSH_INT32, PUSH_INT8, Value, ADD, CALL, CONSTRACT, CREATE_CONTEXT,
    CREATE_OBJECT, DIV, END, EQ, GE, GET_GLOBAL, GET_LOCAL, GET_MEMBER, GT, JMP, JMP_IF_FALSE, LE,
    LT, MUL, NE, PUSH_CONST, PUSH_FALSE, PUSH_THIS, PUSH_TRUE, REM, RETURN, SET_GLOBAL, SET_LOCAL,
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

    pub fn gen_create_context(&self, n: usize, argc: usize, insts: &mut ByteCode) {
        insts.push(CREATE_CONTEXT);
        self.gen_int32(n as i32, insts);
        self.gen_int32(argc as i32, insts);
    }

    pub fn gen_constract(&self, argc: usize, insts: &mut ByteCode) {
        insts.push(CONSTRACT);
        self.gen_int32(argc as i32, insts);
    }

    pub fn gen_create_object(&self, len: usize, insts: &mut ByteCode) {
        insts.push(CREATE_OBJECT);
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

pub fn show(code: &ByteCode) {
    let mut i = 0;
    while i < code.len() {
        match code[i] {
            END => {
                println!("{:02x}                        : End", END);
                i += 1
            }
            CREATE_CONTEXT => {
                println!(
                    "{:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}: CreateContext",
                    CREATE_CONTEXT,
                    code[i + 1],
                    code[i + 2],
                    code[i + 3],
                    code[i + 4],
                    code[i + 5],
                    code[i + 6],
                    code[i + 7],
                    code[i + 8],
                );
                i += 9
            }
            CONSTRACT => {
                println!(
                    "{:02x} {:02x} {:02x} {:02x} {:02x}           : Constract",
                    CONSTRACT,
                    code[i + 1],
                    code[i + 2],
                    code[i + 3],
                    code[i + 4],
                );
                i += 5
            }
            CREATE_OBJECT => {
                println!(
                    "{:02x} {:02x} {:02x} {:02x} {:02x}           : CreateObject",
                    CREATE_OBJECT,
                    code[i + 1],
                    code[i + 2],
                    code[i + 3],
                    code[i + 4],
                );
                i += 5
            }
            PUSH_INT8 => {
                println!(
                    "{:02x} {:02x}                     : PushInt8",
                    PUSH_INT8,
                    code[i + 1],
                );
                i += 2
            }
            PUSH_INT32 => {
                println!(
                    "{:02x} {:02x} {:02x} {:02x} {:02x}            : PushInt32",
                    PUSH_INT32,
                    code[i + 1],
                    code[i + 2],
                    code[i + 3],
                    code[i + 4],
                );
                i += 5
            }
            PUSH_FALSE => {
                println!("{:02x}                        : PushFalse", PUSH_FALSE,);
                i += 1
            }
            PUSH_TRUE => {
                println!("{:02x}                        : PushFalse", PUSH_TRUE,);
                i += 1
            }
            PUSH_CONST => {
                println!(
                    "{:02x} {:02x} {:02x} {:02x} {:02x}            : PushConst",
                    PUSH_CONST,
                    code[i + 1],
                    code[i + 2],
                    code[i + 3],
                    code[i + 4],
                );
                i += 5
            }
            PUSH_THIS => {
                println!("{:02x}                        : PushThis", PUSH_THIS,);
                i += 1
            }
            ADD => {
                println!("{:02x}                        : Add", ADD);
                i += 1
            }
            SUB => {
                println!("{:02x}                        : Sub", SUB);
                i += 1
            }
            MUL => {
                println!("{:02x}                        : Mul", MUL);
                i += 1
            }
            DIV => {
                println!("{:02x}                        : Div", DIV);
                i += 1
            }
            REM => {
                println!("{:02x}                        : Rem", REM);
                i += 1
            }
            LT => {
                println!("{:02x}                        : Lt", LT);
                i += 1
            }
            GT => {
                println!("{:02x}                        : Gt", GT);
                i += 1
            }
            LE => {
                println!("{:02x}                        : Le", LE);
                i += 1
            }
            GE => {
                println!("{:02x}                        : Ge", GE);
                i += 1
            }
            EQ => {
                println!("{:02x}                        : Eq", EQ);
                i += 1
            }
            NE => {
                println!("{:02x}                        : Ne", NE);
                i += 1
            }
            GET_MEMBER => {
                println!("{:02x}                        : GetMember", GET_MEMBER);
                i += 1
            }
            SET_MEMBER => {
                println!("{:02x}                        : SetMember", SET_MEMBER);
                i += 1
            }
            GET_GLOBAL => {
                println!(
                    "{:02x} {:02x} {:02x} {:02x} {:02x}            : GetGlobal",
                    GET_GLOBAL,
                    code[i + 1],
                    code[i + 2],
                    code[i + 3],
                    code[i + 4],
                );
                i += 5
            }
            SET_GLOBAL => {
                println!(
                    "{:02x} {:02x} {:02x} {:02x} {:02x}            : SetGlobal",
                    SET_GLOBAL,
                    code[i + 1],
                    code[i + 2],
                    code[i + 3],
                    code[i + 4],
                );
                i += 5
            }
            GET_LOCAL => {
                println!(
                    "{:02x} {:02x} {:02x} {:02x} {:02x}            : GetLocal",
                    GET_LOCAL,
                    code[i + 1],
                    code[i + 2],
                    code[i + 3],
                    code[i + 4],
                );
                i += 5
            }
            SET_LOCAL => {
                println!(
                    "{:02x} {:02x} {:02x} {:02x} {:02x}            : SetLocal",
                    SET_LOCAL,
                    code[i + 1],
                    code[i + 2],
                    code[i + 3],
                    code[i + 4],
                );
                i += 5
            }
            JMP_IF_FALSE => {
                println!(
                    "{:02x} {:02x} {:02x} {:02x} {:02x}            : JmpIfFalse",
                    JMP_IF_FALSE,
                    code[i + 1],
                    code[i + 2],
                    code[i + 3],
                    code[i + 4],
                );
                i += 5
            }
            JMP => {
                println!(
                    "{:02x} {:02x} {:02x} {:02x} {:02x}            : Jmp",
                    JMP,
                    code[i + 1],
                    code[i + 2],
                    code[i + 3],
                    code[i + 4],
                );
                i += 5
            }
            CALL => {
                println!(
                    "{:02x} {:02x} {:02x} {:02x} {:02x}            : Call",
                    CALL,
                    code[i + 1],
                    code[i + 2],
                    code[i + 3],
                    code[i + 4],
                );
                i += 5
            }
            RETURN => {
                println!("{:02x}                        : Return", RETURN);
                i += 1
            }
            _ => unreachable!(),
        }
    }
}
