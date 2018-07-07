use std::collections::HashMap;
use std::collections::VecDeque;

use std::alloc::{Alloc, Global, Layout};

pub type HeapAddr = *mut Value;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Number(f64),
    String(String),
    Data(String),
    Function(Vec<i32>),      // Vec<i32> will replaced with appropriate type.
    EmbeddedFunction(usize), // unknown if usize == 0; specific function if usize > 0
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
    Jmp(usize),
    JmpIfFalse(usize),
    JmpIfTrue(usize),
    AllocLocalVar(usize),
}

pub struct VM {
    pub global_objects: HashMap<String, Value>,
    pub stack: VecDeque<Value>,
    pub sp_buf: Vec<usize>,
    pub sp: usize,
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
            stack: VecDeque::new(),
            sp_buf: vec![],
            sp: 0,
        }
    }

    pub unsafe fn alloc_for_value() -> HeapAddr {
        let layout = Layout::from_size_align(
            ::std::mem::size_of::<Value>() * 2, // Without '* 2', segv occurs. Why?
            ::std::mem::align_of::<Value>() * 2,
        ).unwrap();
        let ptr_nonnull = Global.alloc(layout.clone()).unwrap();
        ptr_nonnull.as_ptr() as *mut Value
    }
}

impl VM {
    pub fn run(&mut self, insts: Vec<Inst>) {
        let insts_len = insts.len();

        let mut pc = 0usize;
        while pc < insts_len {
            match insts[pc].clone() {
                Inst::AllocLocalVar(ref n) => {
                    for _ in 0..*n {
                        self.stack.push_back(Value::Number(0.0));
                    }
                    self.sp = *n;
                    pc += 1
                }
                Inst::Push(ref val) => {
                    self.stack.push_back(val.clone());
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
                    let val = self.stack[*n].clone();
                    self.stack.push_back(val);
                    pc += 1
                }
                Inst::GetGlobal(ref name) => {
                    self.stack
                        .push_back(self.global_objects.get(name.as_str()).unwrap().clone());
                    pc += 1
                }
                Inst::SetLocal(ref n) => {
                    let val = self.stack.pop_back().unwrap();
                    self.stack[*n] = val;
                    pc += 1
                }
                Inst::SetGlobal(name) => {
                    self.global_objects
                        .insert(name, self.stack.pop_back().unwrap());
                    pc += 1
                }
                Inst::GetMember => {
                    let member_name = {
                        let member = self.stack.pop_back().unwrap();
                        if let Value::Data(name) = member {
                            name
                        } else {
                            panic!()
                        }
                    };
                    let parent = self.stack.pop_back().unwrap();
                    if let Value::Object(map) = parent {
                        match map.get(member_name.as_str()) {
                            Some(addr) => unsafe { self.stack.push_back((**addr).clone()) },
                            None => {}
                        }
                    }
                    pc += 1
                }
                Inst::Call(argc) => {
                    self.run_function(argc);
                    pc += 1
                }
                Inst::Jmp(dst) => pc = dst,
                Inst::JmpIfFalse(dst) => {
                    let cond = self.stack.pop_back().unwrap();
                    if let Value::Bool(false) = cond {
                        pc = dst
                    } else {
                        pc += 1
                    }
                }
                _ => {}
            }
        }
    }

    fn run_binary_op(&mut self, op: &Inst) {
        let rhs = self.stack.pop_back().unwrap();
        let lhs = self.stack.pop_back().unwrap();
        match (lhs, rhs) {
            (Value::Number(n1), Value::Number(n2)) => self.stack.push_back(match op {
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

    fn run_function(&mut self, argc: usize) {
        let mut args = vec![];
        for _ in 0..argc {
            args.push(self.stack.pop_back().unwrap());
        }

        let callee = self.stack.pop_back().unwrap();
        match callee {
            Value::EmbeddedFunction(1) => console_log(&args[0]),
            c => println!("{:?}", c),
        }

        // EmbeddedFunction(1)
        fn console_log(arg: &Value) {
            match arg {
                &Value::String(ref s) => println!("{}", s),
                &Value::Number(ref n) => println!("{}", *n),
                _ => {}
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
        Inst::Push(Value::Data("log".to_string())),
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
