use std::collections::HashMap;
use std::collections::VecDeque;

use std::alloc::{Alloc, Global, Layout};

pub type HeapAddr = *mut Value;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
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
    Member,
    GetGlobal(String),
    GetLocal(String),
    SetGlobal(String, Value),
    Call(usize),
}

pub struct VM {
    pub global_objects: HashMap<String, Value>,
    pub stack: VecDeque<Value>,
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
        }
    }

    pub unsafe fn alloc_for_value() -> HeapAddr {
        let layout = Layout::from_size_align(
            ::std::mem::size_of::<Value>() + 1, // Without '+ 1', segv occurs. Why?
            ::std::mem::align_of::<Value>(),
        ).unwrap();
        let ptr_nonnull = Global.alloc(layout.clone()).unwrap();
        ptr_nonnull.as_ptr() as *mut Value
    }
}

impl VM {
    pub fn run(&mut self, insts: Vec<Inst>) {
        for inst in insts {
            match inst {
                Inst::Push(val) => self.stack.push_back(val),
                Inst::GetGlobal(name) => self.stack
                    .push_back(self.global_objects.get(name.as_str()).unwrap().clone()),
                Inst::SetGlobal(name, obj) => {
                    self.global_objects.insert(name, obj);
                }
                Inst::Member => {
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
                }
                Inst::Call(argc) => self.run_function(argc),
                _ => {}
            }
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
    let insts = vec![
        Inst::GetGlobal("console".to_string()),
        Inst::Push(Value::Data("log".to_string())),
        Inst::Member,
        Inst::Push(Value::String("hello".to_string())),
        Inst::Call(1),
        Inst::SetGlobal("n".to_string(), Value::Number(123.0)),
        Inst::GetGlobal("console".to_string()),
        Inst::Push(Value::Data("log".to_string())),
        Inst::Member,
        Inst::GetGlobal("n".to_string()),
        Inst::Call(1),
    ];
    let mut vm = VM::new();
    vm.run(insts);
}
