use std::boxed::Box;
use std::collections::HashMap;

use libc;
use std::ffi::CStr;

pub type HeapAddr = *mut Value;
pub type RawStringPtr = *mut libc::c_char;

pub unsafe fn alloc_rawstring(s: &str) -> RawStringPtr {
    let p = libc::calloc(1, s.len() + 2) as RawStringPtr;
    libc::strncpy(p, s.as_ptr() as *const i8, s.len());
    p
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Undefined,
    Bool(bool),
    Number(f64),
    String(RawStringPtr),
    Function(usize),
    Cls(Box<Value>, Vec<usize>),   // Function, Vec<free variable addr>
    ClsSp(Box<Value>, Vec<Value>), // Function, Vec<value of free variable>
    EmbeddedFunction(usize),       // unknown if usize == 0; specific function if usize > 0
    Object(HashMap<String, HeapAddr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    Push(Value),
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    GetMember,
    SetMember,
    GetGlobal(String),
    GetLocal(usize),
    SetGlobal(String),
    SetLocal(usize),
    Call(usize),
    Jmp(isize),
    JmpIfFalse(isize),
    JmpIfTrue(isize),
    AllocLocalVar(usize, usize),
    Return,
    End,
}

pub struct VM {
    pub global_objects: HashMap<String, Value>,
    pub stack: Vec<Value>,
    pub bp_buf: Vec<usize>,
    pub bp: usize,
    pub sp_history: Vec<usize>,
    pub return_addr: Vec<isize>,
}

impl VM {
    pub fn new() -> VM {
        let mut obj = HashMap::new();

        unsafe {
            let console_log_addr = VM::alloc_for_value();
            *console_log_addr = Value::EmbeddedFunction(1);

            obj.insert("console".to_string(), {
                let mut map = HashMap::new();
                map.insert("log".to_string(), console_log_addr);
                Value::Object(map)
            });
        }

        VM {
            global_objects: obj,
            stack: Vec::with_capacity(128),
            bp_buf: vec![],
            bp: 0,
            sp_history: vec![],
            return_addr: vec![],
        }
    }

    pub unsafe fn alloc_for_value() -> HeapAddr {
        libc::malloc(::std::mem::size_of::<Value>() * 2) as *mut Value
    }
}

impl VM {
    pub fn run(&mut self, insts: Vec<Inst>) {
        let insts_len = insts.len() as isize;

        let mut pc = 0isize;
        while pc < insts_len {
            match insts[pc as usize].clone() {
                Inst::End => break,
                Inst::AllocLocalVar(ref n, ref argc) => {
                    self.bp_buf.push(self.bp);
                    self.sp_history.push(self.stack.len() - argc);
                    self.bp = self.stack.len() - argc;
                    for _ in 0..*n {
                        self.stack.push(Value::Undefined);
                    }
                    pc += 1
                }
                Inst::Return => {
                    let val = self.stack.pop().unwrap();
                    let former_sp = self.sp_history.pop().unwrap();
                    self.stack.truncate(former_sp);
                    self.stack.push(val);
                    pc = self.return_addr.pop().unwrap();
                    self.bp = self.bp_buf.pop().unwrap();
                }
                Inst::Push(ref val) => {
                    self.stack.push(val.clone());
                    pc += 1
                }
                ref op
                    if op == &Inst::Add
                        || op == &Inst::Sub
                        || op == &Inst::Mul
                        || op == &Inst::Div
                        || op == &Inst::Rem
                        || op == &Inst::Lt
                        || op == &Inst::Gt
                        || op == &Inst::Le
                        || op == &Inst::Ge
                        || op == &Inst::Eq
                        || op == &Inst::Ne =>
                {
                    self.run_binary_op(op);
                    pc += 1
                }
                Inst::GetLocal(ref n) => {
                    let val = self.stack[self.bp + *n].clone();
                    if let Value::Cls(callee, addrs) = val {
                        let mut fv_val = vec![];
                        for addr in addrs {
                            fv_val.push(self.stack[self.bp + addr].clone());
                        }
                        self.stack.push(Value::ClsSp(callee, fv_val));
                    } else {
                        self.stack.push(val);
                    }
                    pc += 1
                }
                Inst::GetGlobal(ref name) => {
                    let val = self.global_objects.get(name.as_str()).unwrap().clone();
                    if let Value::Cls(callee, addrs) = val {
                        let mut fv_val = vec![];
                        for addr in addrs {
                            fv_val.push(self.stack[self.bp + addr].clone());
                        }
                        self.stack.push(Value::ClsSp(callee, fv_val));
                    } else {
                        self.stack.push(val);
                    }
                    pc += 1
                }
                Inst::SetLocal(ref n) => {
                    let val = self.stack.pop().unwrap();
                    self.stack[self.bp + *n] = val;
                    pc += 1
                }
                Inst::SetGlobal(name) => {
                    self.global_objects.insert(name, self.stack.pop().unwrap());
                    pc += 1
                }
                Inst::GetMember => {
                    let member = self.stack.pop().unwrap();
                    if let Value::String(name) = member {
                        unsafe {
                            let member_name = CStr::from_ptr(name).to_str().unwrap();
                            let parent = self.stack.pop().unwrap();
                            if let Value::Object(map) = parent {
                                match map.get(member_name) {
                                    Some(addr) => self.stack.push((**addr).clone()),
                                    None => panic!(),
                                }
                            }
                        }
                    } else {
                        panic!()
                    }
                    pc += 1
                }
                Inst::Call(argc) => {
                    self.run_function(argc, &mut pc);
                }
                Inst::Jmp(dst) => pc += dst,
                Inst::JmpIfFalse(dst) => {
                    let cond = self.stack.pop().unwrap();
                    if let Value::Bool(false) = cond {
                        pc += dst
                    } else {
                        pc += 1
                    }
                }
                _ => {}
            }
        }
    }

    fn run_binary_op(&mut self, op: &Inst) {
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();
        match (lhs, rhs) {
            (Value::Number(n1), Value::Number(n2)) => self.stack.push(match op {
                &Inst::Add => Value::Number(n1 + n2),
                &Inst::Sub => Value::Number(n1 - n2),
                &Inst::Mul => Value::Number(n1 * n2),
                &Inst::Div => Value::Number(n1 / n2),
                &Inst::Rem => Value::Number((n1 as i64 % n2 as i64) as f64),
                &Inst::Lt => Value::Bool(n1 < n2),
                &Inst::Gt => Value::Bool(n1 > n2),
                &Inst::Le => Value::Bool(n1 <= n2),
                &Inst::Ge => Value::Bool(n1 >= n2),
                &Inst::Eq => Value::Bool(n1 == n2),
                &Inst::Ne => Value::Bool(n1 != n2),
                _ => panic!(),
            }),
            _ => {}
        }
    }

    fn run_function(&mut self, argc: usize, pc: &mut isize) {
        let mut fv_vals = vec![];
        let mut args = vec![];
        for _ in 0..argc {
            args.push(self.stack.pop().unwrap());
        }
        args.reverse();

        let mut callee = self.stack.pop().unwrap();

        loop {
            match callee {
                Value::EmbeddedFunction(1) => {
                    console_log(args);
                    *pc += 1;
                    break;
                }
                Value::Function(dst) => {
                    self.return_addr.push(*pc + 1);
                    for fv_val in fv_vals {
                        self.stack.push(fv_val);
                    }
                    for arg in args {
                        self.stack.push(arg);
                    }
                    *pc = dst as isize;
                    break;
                }
                Value::ClsSp(callee_, fv_vals_) => {
                    fv_vals = fv_vals_;
                    callee = *callee_;
                }
                c => {
                    println!("err: {:?}, pc = {}", c, pc);
                    break;
                }
            }
        }

        // EmbeddedFunction(1)
        fn console_log(args: Vec<Value>) {
            unsafe {
                let args_len = args.len();
                for i in 0..args_len {
                    match args[i] {
                        Value::String(ref s) => {
                            libc::printf(b"%s\0".as_ptr() as RawStringPtr, *s as RawStringPtr);
                        }
                        Value::Number(ref n) => {
                            libc::printf(b"%g\0".as_ptr() as RawStringPtr, *n);
                        }
                        _ => {}
                    }
                    if args_len - 1 != i {
                        libc::printf(b" \0".as_ptr() as RawStringPtr);
                    }
                }
                libc::puts(b"\0".as_ptr() as RawStringPtr);
            }
        }
    }
}

pub fn test() {
    // let insts = vec![
    //     Inst::GetGlobal("console".to_string()),
    //     Inst::Push(Value::Data("log".to_string())),
    //     Inst::GetMember,
    //     Inst::Push(Value::String("hello".to_string())),
    //     Inst::Call(1),
    //     Inst::Push(Value::Number(123.0)),
    //     Inst::SetGlobal("n".to_string()),
    //     Inst::GetGlobal("console".to_string()),
    //     Inst::Push(Value::Data("log".to_string())),
    //     Inst::GetMember,
    //     Inst::GetGlobal("n".to_string()),
    //     Inst::Call(1),
    // ];
    let insts = vec![
        Inst::Push(Value::Number(0.0)),
        Inst::SetGlobal("n".to_string()),
        Inst::GetGlobal("console".to_string()),
        // Inst::Push(Value::Data("log".to_string())),
        Inst::GetMember,
        Inst::GetGlobal("n".to_string()),
        Inst::Call(1),
        Inst::GetGlobal("n".to_string()),
        Inst::Push(Value::Number(1.0)),
        Inst::Add,
        Inst::SetGlobal("n".to_string()),
        Inst::Jmp(2),
    ];
    let mut vm = VM::new();
    vm.run(insts);
}
