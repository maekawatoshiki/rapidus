use std::boxed::Box;
use std::collections::HashMap;

use libc;
use std::cell::RefCell;
use std::ffi::CStr;
use std::rc::Rc;

pub type HeapAddr = *mut Value;
pub type ObjectAddr = *mut HashMap<String, HeapAddr>;
pub type RawStringPtr = *mut libc::c_char;

thread_local!(pub static ALLOCATED_MEM_LIST: RefCell<Vec<HeapAddr>> = {
    RefCell::new(vec![])
});

pub unsafe fn alloc_rawstring(s: &str) -> RawStringPtr {
    let p = libc::calloc(1, s.len() + 2) as RawStringPtr;
    libc::strncpy(p, s.as_ptr() as *const i8, s.len());
    p
}

pub unsafe fn alloc_for_value() -> HeapAddr {
    let p = libc::calloc(1, 64) as *mut Value;
    ALLOCATED_MEM_LIST.with(|list| list.borrow_mut().push(p));
    p
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Undefined,
    Bool(bool),
    Number(f64),
    String(RawStringPtr),
    Function(usize),
    NeedThis(Box<Value>),
    WithThis(Box<Value>, Box<Value>),               // Function, This
    EmbeddedFunction(usize), // unknown if usize == 0; specific function if usize > 0
    Object(Rc<RefCell<HashMap<String, HeapAddr>>>), // Object(HashMap<String, HeapAddr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    PushThis,
    Push(Value),
    Pop,
    PushNeedThis(Box<Value>),
    Constract(usize),
    CreateObject(usize),
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
    pub global_objects: Rc<RefCell<HashMap<String, HeapAddr>>>,
    pub stack: Vec<Value>,
    pub bp_buf: Vec<usize>,
    pub bp: usize,
    pub sp_history: Vec<usize>,
    pub return_addr: Vec<isize>,
    pub insts: Vec<Inst>,
}

impl VM {
    pub fn new() -> VM {
        let mut obj = HashMap::new();

        unsafe {
            let console_log_addr = alloc_for_value();
            *console_log_addr = Value::EmbeddedFunction(1);

            obj.insert("console".to_string(), {
                let mut map = HashMap::new();
                map.insert("log".to_string(), console_log_addr);
                let obj = alloc_for_value();
                *obj = Value::Object(Rc::new(RefCell::new(map)));
                obj
            });
        }

        let global_objects = Rc::new(RefCell::new(obj));

        VM {
            global_objects: global_objects.clone(),
            stack: {
                let mut stack = Vec::with_capacity(128);
                stack.push(Value::Object(global_objects.clone()));
                stack
            },
            bp_buf: Vec::with_capacity(128),
            bp: 0,
            sp_history: Vec::with_capacity(128),
            return_addr: Vec::with_capacity(128),
            insts: vec![],
        }
    }
}

impl VM {
    pub fn run(&mut self, insts: Vec<Inst>) {
        let mut pc = 0isize;
        self.do_run(&mut pc, &insts);
    }

    pub fn do_run(&mut self, pc: &mut isize, insts: &Vec<Inst>) {
        loop {
            match &insts[*pc as usize] {
                &Inst::End => break,
                &Inst::AllocLocalVar(ref n, ref argc) => {
                    self.bp_buf.push(self.bp);
                    self.sp_history.push(self.stack.len() - argc);
                    self.bp = self.stack.len() - argc;
                    for _ in 0..*n {
                        self.stack.push(Value::Undefined);
                    }
                    *pc += 1;
                }
                &Inst::Return => {
                    let val = self.stack.pop().unwrap();
                    let former_sp = self.sp_history.pop().unwrap();
                    self.stack.truncate(former_sp);
                    self.stack.push(val);
                    *pc = self.return_addr.pop().unwrap();
                    self.bp = self.bp_buf.pop().unwrap();
                    return;
                }
                &Inst::Pop => {
                    self.stack.pop();
                    *pc += 1;
                }
                &Inst::Push(ref val) => {
                    self.stack.push(val.clone());
                    *pc += 1;
                }
                &Inst::PushThis => {
                    let val = self.stack[self.bp].clone();
                    self.stack.push(val);
                    *pc += 1;
                }
                &Inst::PushNeedThis(ref callee) => {
                    self.stack.push(Value::NeedThis(callee.clone()));
                    *pc += 1;
                }
                ref op
                    if *op == &Inst::Add
                        || *op == &Inst::Sub
                        || *op == &Inst::Mul
                        || *op == &Inst::Div
                        || *op == &Inst::Rem
                        || *op == &Inst::Lt
                        || *op == &Inst::Gt
                        || *op == &Inst::Le
                        || *op == &Inst::Ge
                        || *op == &Inst::Eq
                        || *op == &Inst::Ne =>
                {
                    self.run_binary_op(op);
                    *pc += 1;
                }
                &Inst::GetLocal(ref n) => {
                    let val = self.stack[self.bp + *n].clone();
                    self.stack.push(val);
                    *pc += 1;
                }
                &Inst::GetGlobal(ref name) => {
                    unsafe {
                        let val =
                            (**(*self.global_objects).borrow().get(name.as_str()).unwrap()).clone();
                        self.stack.push(val);
                    }
                    *pc += 1
                }
                &Inst::SetLocal(ref n) => {
                    let val = self.stack.pop().unwrap();
                    self.stack[self.bp + *n] = val;
                    *pc += 1;
                }
                &Inst::SetGlobal(ref name) => unsafe {
                    **(*self.global_objects)
                        .borrow_mut()
                        .entry(name.to_string())
                        .or_insert_with(|| alloc_for_value()) = self.stack.pop().unwrap();
                    *pc += 1
                },
                &Inst::GetMember => {
                    let member_name = {
                        let member = self.stack.pop().unwrap();
                        if let Value::String(name) = member {
                            unsafe { CStr::from_ptr(name).to_str().unwrap() }
                        } else {
                            panic!("runtime err")
                        }
                    };
                    let parent = self.stack.pop().unwrap();
                    unsafe {
                        if let Value::Object(map) = parent {
                            match map.borrow().get(member_name) {
                                Some(addr) => {
                                    let val = (**addr).clone();
                                    if let Value::NeedThis(callee) = val {
                                        self.stack.push(Value::WithThis(
                                            callee,
                                            Box::new(Value::Object(map.clone())),
                                        ))
                                    } else {
                                        self.stack.push(val)
                                    }
                                }
                                None => self.stack.push(Value::Undefined),
                            }
                        } else {
                            panic!("runtime err")
                        }
                    }
                    *pc += 1
                }
                &Inst::SetMember => {
                    let member = self.stack.pop().unwrap();
                    if let Value::String(name) = member {
                        unsafe {
                            let member_name = CStr::from_ptr(name).to_str().unwrap();
                            let parent = self.stack.pop().unwrap();
                            let val = self.stack.pop().unwrap();
                            if let Value::Object(map) = parent {
                                **map
                                    .borrow_mut()
                                    .entry(member_name.to_string())
                                    .or_insert_with(|| alloc_for_value()) = val;
                            }
                        }
                    } else {
                        panic!()
                    }
                    *pc += 1
                }
                &Inst::Call(argc) => {
                    self.run_function(argc, pc, insts);
                }
                &Inst::Constract(argc) => {
                    self.run_constract(argc, pc, insts);
                }
                &Inst::CreateObject(len) => {
                    self.run_create_object(len, pc);
                }
                &Inst::Jmp(dst) => *pc += dst,
                &Inst::JmpIfFalse(dst) => {
                    let cond = self.stack.pop().unwrap();
                    if let Value::Bool(false) = cond {
                        *pc += dst
                    } else {
                        *pc += 1
                    }
                }
                _ => {}
            }
        }

        ALLOCATED_MEM_LIST.with(|list| {
            for p in (*list.borrow()).iter() {
                unsafe { libc::free(*p as *mut libc::c_void) }
            }
        });
    }

    #[inline]
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
            (Value::String(s1), Value::Number(n2)) => unsafe {
                self.stack.push(match op {
                    &Inst::Add => {
                        let concat = format!("{}{}", CStr::from_ptr(s1).to_str().unwrap(), n2);
                        Value::String(alloc_rawstring(concat.as_str()))
                    }
                    _ => panic!(),
                })
            },
            (Value::Number(n1), Value::String(s2)) => unsafe {
                self.stack.push(match op {
                    &Inst::Add => {
                        let concat = format!("{}{}", n1, CStr::from_ptr(s2).to_str().unwrap());
                        Value::String(alloc_rawstring(concat.as_str()))
                    }
                    _ => panic!(),
                })
            },
            (Value::String(s1), Value::String(s2)) => unsafe {
                self.stack.push(match op {
                    &Inst::Add => {
                        let concat = format!(
                            "{}{}",
                            CStr::from_ptr(s1).to_str().unwrap(),
                            CStr::from_ptr(s2).to_str().unwrap()
                        );
                        Value::String(alloc_rawstring(concat.as_str()))
                    }
                    _ => panic!(),
                })
            },
            _ => {}
        }
    }

    #[inline]
    fn run_function(&mut self, argc: usize, pc: &mut isize, insts: &Vec<Inst>) {
        let mut this = None;

        let mut callee = self.stack.pop().unwrap();

        loop {
            match callee {
                Value::EmbeddedFunction(1) => {
                    let mut args = vec![];
                    for _ in 0..argc {
                        args.push(self.stack.pop().unwrap());
                    }
                    args.reverse();
                    console_log(args);
                    *pc += 1;
                    break;
                }
                Value::Function(dst) => {
                    self.return_addr.push(*pc + 1);
                    if let Some(this) = this {
                        let pos = self.stack.len() - argc;
                        self.stack.insert(pos, this);
                    }
                    *pc = dst as isize;
                    self.do_run(pc, insts);
                    break;
                }
                Value::NeedThis(callee_) => {
                    this = Some(Value::Object(self.global_objects.clone()));
                    callee = *callee_;
                }
                Value::WithThis(callee_, this_) => {
                    this = Some(*this_);
                    callee = *callee_;
                }
                c => {
                    println!("Call: err: {:?}, pc = {}", c, pc);
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
                            libc::printf(b"%.15g\0".as_ptr() as RawStringPtr, *n);
                        }
                        Value::Undefined => {
                            libc::printf(b"undefined\0".as_ptr() as RawStringPtr);
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

    #[inline]
    fn run_constract(&mut self, argc: usize, pc: &mut isize, insts: &Vec<Inst>) {
        let mut callee = self.stack.pop().unwrap();

        loop {
            match callee {
                Value::Function(dst) => {
                    self.return_addr.push(*pc + 1);

                    // insert new 'this'
                    let pos = self.stack.len() - argc;
                    let new_new = Rc::new(RefCell::new(HashMap::new()));
                    self.stack.insert(pos, Value::Object(new_new.clone()));

                    *pc = dst as isize;
                    self.do_run(pc, insts);
                    self.stack.pop(); // return value by func
                    self.stack.push(Value::Object(new_new));
                    break;
                }
                Value::NeedThis(callee_) => {
                    callee = *callee_;
                }
                Value::WithThis(callee_, _this) => {
                    callee = *callee_;
                }
                c => {
                    println!("Call: err: {:?}, pc = {}", c, pc);
                    break;
                }
            }
        }
    }

    #[inline]
    fn run_create_object(&mut self, len: usize, pc: &mut isize) {
        let mut map = HashMap::new();
        for _ in 0..len {
            let name = if let Value::String(name) = self.stack.pop().unwrap() {
                name
            } else {
                panic!()
            };
            let val = self.stack.pop().unwrap();
            map.insert(
                unsafe { CStr::from_ptr(name).to_str().unwrap().to_string() },
                unsafe {
                    let p = alloc_for_value();
                    *p = val.clone();
                    p
                },
            );
        }
        self.stack.push(Value::Object(Rc::new(RefCell::new(map))));
        *pc += 1;
    }
}
