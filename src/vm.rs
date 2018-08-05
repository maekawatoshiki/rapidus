use std::boxed::Box;
use std::collections::HashMap;

use libc;
use std::cell::RefCell;
use std::ffi::CStr;
use std::rc::Rc;

use node::BinOp;

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
    Function(usize, Rc<RefCell<HashMap<String, HeapAddr>>>),
    NeedThis(Box<Value>),
    WithThis(Box<Value>, Box<Value>),               // Function, This
    EmbeddedFunction(usize), // unknown if usize == 0; specific function if usize > 0
    Object(Rc<RefCell<HashMap<String, HeapAddr>>>), // Object(HashMap<String, HeapAddr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    PushThis,
    Push(Value),
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
                &Inst::Push(ref val) => {
                    self.stack.push(val.clone());
                    *pc += 1;
                }
                &Inst::PushThis => {
                    let val = self.stack[self.bp].clone();
                    self.stack.push(val);
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
                    let member = self.stack.pop().unwrap().to_string();
                    let parent = self.stack.pop().unwrap();
                    match parent {
                        Value::Object(map)
                        | Value::Function(_, map)
                        | Value::NeedThis(box Value::Function(_, map)) => {
                            match map.borrow().get(member.as_str()) {
                                Some(addr) => {
                                    let val = unsafe { (**addr).clone() };
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
                        }
                        _ => unreachable!(),
                    }
                    *pc += 1
                }
                &Inst::SetMember => {
                    let member = self.stack.pop().unwrap().to_string();
                    let parent = self.stack.pop().unwrap();
                    let val = self.stack.pop().unwrap();
                    match parent {
                        Value::Object(map)
                        | Value::Function(_, map)
                        | Value::NeedThis(box Value::Function(_, map)) => unsafe {
                            **map
                                .borrow_mut()
                                .entry(member)
                                .or_insert_with(|| alloc_for_value()) = val;
                        },
                        e => unreachable!("{:?}", e),
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
                Value::Function(dst, _) => {
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
                Value::Function(dst, _) => {
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

impl Value {
    fn to_string(self) -> String {
        match self {
            Value::String(name) => unsafe { CStr::from_ptr(name).to_str().unwrap() }.to_string(),
            Value::Number(n) => format!("{}", n),
            _ => unimplemented!(),
        }
    }
}

pub struct VM2 {
    pub global_objects: Rc<RefCell<HashMap<String, HeapAddr>>>,
    pub stack: Vec<Value>,
    pub bp_buf: Vec<usize>,
    pub bp: usize,
    pub sp_history: Vec<usize>,
    pub return_addr: Vec<isize>,
    pub constant_table: Vec<Value>,
    pub const_str_table: Vec<String>,
    pub insts: Vec<u8>,
    pub pc: isize,
    pub op_table: [fn(&mut VM2) -> bool; 30],
}

impl VM2 {
    pub fn new() -> VM2 {
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

        VM2 {
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
            constant_table: vec![],
            const_str_table: vec![],
            insts: vec![],
            pc: 0isize,
            op_table: [
                end,
                create_context,
                constract,
                create_object,
                push_int8,
                push_int32,
                push_const,
                push_const_need_this,
                push_this,
                add,
                sub,
                mul,
                div,
                rem,
                lt,
                gt,
                le,
                ge,
                eq,
                ne,
                get_member,
                set_member,
                get_global,
                set_global,
                get_local,
                set_local,
                jmp_if_false,
                jmp,
                call,
                return_,
            ],
        }
    }
}

const END: u8 = 0x00;
const CREATE_CONTEXT: u8 = 0x01;
const CONSTRACT: u8 = 0x02;
const CREATE_OBJECT: u8 = 0x03;
const PUSH_INT8: u8 = 0x04;
const PUSH_INT32: u8 = 0x05;
const PUSH_CONST: u8 = 0x06;
const PUSH_CONST_NEED_THIS: u8 = 0x07;
const PUSH_THIS: u8 = 0x08;
const ADD: u8 = 0x09;
const SUB: u8 = 0x0a;
const MUL: u8 = 0x0b;
const DIV: u8 = 0x0c;
const REM: u8 = 0x0d;
const LT: u8 = 0x0e;
const GT: u8 = 0x0f;
const LE: u8 = 0x10;
const GE: u8 = 0x11;
const EQ: u8 = 0x12;
const NE: u8 = 0x13;
const GET_MEMBER: u8 = 0x14;
const SET_MEMBER: u8 = 0x15;
const GET_GLOBAL: u8 = 0x16;
const SET_GLOBAL: u8 = 0x17;
const GET_LOCAL: u8 = 0x18;
const SET_LOCAL: u8 = 0x19;
const JMP_IF_FALSE: u8 = 0x1a;
const JMP: u8 = 0x1b;
const CALL: u8 = 0x1c;
const RETURN: u8 = 0x1d;

impl VM2 {
    pub fn run(&mut self, insts: Vec<u8>) {
        self.insts = insts;
        self.do_run();
        println!("stack trace: {:?}", self.stack);
    }

    pub fn do_run(&mut self) {
        while self.op_table[self.insts[self.pc as usize] as usize](self) {}
        // loop {
        //     println!("inst: {}", self.insts[self.pc as usize]);
        //     if !self.op_table[self.insts[self.pc as usize] as usize](self) {
        //         break;
        //     }
        //     // println!("stack trace: {:?} - {}", self.stack, *pc);
        // }
    }
}

macro_rules! get_int8 {
    ($self:ident, $var:ident, $ty:ty) => {
        let $var = $self.insts[$self.pc as usize] as $ty;
        $self.pc += 1;
    };
}

macro_rules! get_int32 {
    ($self:ident, $var:ident, $ty:ty) => {
        let $var = (($self.insts[$self.pc as usize + 3] as $ty) << 24)
            + (($self.insts[$self.pc as usize + 2] as $ty) << 16)
            + (($self.insts[$self.pc as usize + 1] as $ty) << 8)
            + ($self.insts[$self.pc as usize + 0] as $ty);
        $self.pc += 4;
    };
}

#[inline]
fn end(_self: &mut VM2) -> bool {
    false
}

#[inline]
fn create_context(self_: &mut VM2) -> bool {
    self_.pc += 1; // create_context
    get_int32!(self_, n, usize);
    get_int32!(self_, argc, usize);
    self_.bp_buf.push(self_.bp);
    self_.sp_history.push(self_.stack.len() - argc);
    self_.bp = self_.stack.len() - argc;
    for _ in 0..n {
        self_.stack.push(Value::Undefined);
    }
    true
}

#[inline]
fn constract(self_: &mut VM2) -> bool {
    self_.pc += 1; // constract
    get_int32!(self_, argc, usize);

    let mut callee = self_.stack.pop().unwrap();

    loop {
        match callee {
            Value::Function(dst, _) => {
                self_.return_addr.push(self_.pc);

                // insert new 'this'
                let pos = self_.stack.len() - argc;
                let new_this = Rc::new(RefCell::new(HashMap::new()));
                self_.stack.insert(pos, Value::Object(new_this.clone()));

                self_.pc = dst as isize;
                self_.do_run();
                self_.stack.pop(); // return value by func
                self_.stack.push(Value::Object(new_this));
                break;
            }
            Value::NeedThis(callee_) => {
                callee = *callee_;
            }
            Value::WithThis(callee_, _this) => {
                callee = *callee_;
            }
            c => {
                println!("Call: err: {:?}, pc = {}", c, self_.pc);
                break;
            }
        }
    }
    true
}

#[inline]
fn create_object(self_: &mut VM2) -> bool {
    self_.pc += 1; // create_context
    get_int32!(self_, len, usize);

    let mut map = HashMap::new();
    for _ in 0..len {
        let name = if let Value::String(name) = self_.stack.pop().unwrap() {
            name
        } else {
            panic!()
        };
        let val = self_.stack.pop().unwrap();
        map.insert(
            unsafe { CStr::from_ptr(name).to_str().unwrap().to_string() },
            unsafe {
                let p = alloc_for_value();
                *p = val.clone();
                p
            },
        );
    }
    self_.stack.push(Value::Object(Rc::new(RefCell::new(map))));
    true
}

#[inline]
fn push_int8(self_: &mut VM2) -> bool {
    self_.pc += 1; // push_int
    get_int8!(self_, n, i32);
    self_.stack.push(Value::Number(n as f64));
    true
}

#[inline]
fn push_int32(self_: &mut VM2) -> bool {
    self_.pc += 1; // push_int
    get_int32!(self_, n, i32);
    self_.stack.push(Value::Number(n as f64));
    true
}

#[inline]
fn push_const(self_: &mut VM2) -> bool {
    self_.pc += 1; // push_const
    get_int32!(self_, n, usize);
    self_.stack.push(self_.constant_table[n].clone());
    true
}

#[inline]
fn push_const_need_this(self_: &mut VM2) -> bool {
    self_.pc += 1; // push_this
    let val = self_.stack[self_.bp].clone();
    self_.stack.push(val);
    true
}

#[inline]
fn push_this(self_: &mut VM2) -> bool {
    self_.pc += 1; // push_this
    let val = self_.stack[self_.bp].clone();
    self_.stack.push(val);
    true
}

macro_rules! bin_op {
    ($name:ident, $binop:ident) => {
        #[inline]
        fn $name(self_: &mut VM2) -> bool {
            self_.pc += 1; // $name
            binary(self_, &BinOp::$binop);
            true
        }
    };
}

bin_op!(add, Add);
bin_op!(sub, Sub);
bin_op!(mul, Mul);
bin_op!(div, Div);
bin_op!(rem, Rem);
bin_op!(lt, Lt);
bin_op!(gt, Gt);
bin_op!(le, Le);
bin_op!(ge, Ge);
bin_op!(eq, Eq);
bin_op!(ne, Ne);

#[inline]
fn binary(self_: &mut VM2, op: &BinOp) {
    let rhs = self_.stack.pop().unwrap();
    let lhs = self_.stack.pop().unwrap();
    match (lhs, rhs) {
        (Value::Number(n1), Value::Number(n2)) => self_.stack.push(match op {
            &BinOp::Add => Value::Number(n1 + n2),
            &BinOp::Sub => Value::Number(n1 - n2),
            &BinOp::Mul => Value::Number(n1 * n2),
            &BinOp::Div => Value::Number(n1 / n2),
            &BinOp::Rem => Value::Number((n1 as i64 % n2 as i64) as f64),
            &BinOp::Lt => Value::Bool(n1 < n2),
            &BinOp::Gt => Value::Bool(n1 > n2),
            &BinOp::Le => Value::Bool(n1 <= n2),
            &BinOp::Ge => Value::Bool(n1 >= n2),
            &BinOp::Eq => Value::Bool(n1 == n2),
            &BinOp::Ne => Value::Bool(n1 != n2),
            _ => panic!(),
        }),
        (Value::String(s1), Value::Number(n2)) => unsafe {
            self_.stack.push(match op {
                &BinOp::Add => {
                    let concat = format!("{}{}", CStr::from_ptr(s1).to_str().unwrap(), n2);
                    Value::String(alloc_rawstring(concat.as_str()))
                }
                _ => panic!(),
            })
        },
        (Value::Number(n1), Value::String(s2)) => unsafe {
            self_.stack.push(match op {
                &BinOp::Add => {
                    let concat = format!("{}{}", n1, CStr::from_ptr(s2).to_str().unwrap());
                    Value::String(alloc_rawstring(concat.as_str()))
                }
                _ => panic!(),
            })
        },
        (Value::String(s1), Value::String(s2)) => unsafe {
            self_.stack.push(match op {
                &BinOp::Add => {
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
fn get_member(self_: &mut VM2) -> bool {
    self_.pc += 1; // get_global
    let member = self_.stack.pop().unwrap().to_string();
    let parent = self_.stack.pop().unwrap();
    match parent {
        Value::Object(map)
        | Value::Function(_, map)
        | Value::NeedThis(box Value::Function(_, map)) => match map.borrow().get(member.as_str()) {
            Some(addr) => {
                let val = unsafe { (**addr).clone() };
                if let Value::NeedThis(callee) = val {
                    self_.stack.push(Value::WithThis(
                        callee,
                        Box::new(Value::Object(map.clone())),
                    ))
                } else {
                    self_.stack.push(val)
                }
            }
            None => self_.stack.push(Value::Undefined),
        },
        _ => unreachable!(),
    }
    true
}

#[inline]
fn set_member(self_: &mut VM2) -> bool {
    self_.pc += 1; // get_global
    let member = self_.stack.pop().unwrap().to_string();
    let parent = self_.stack.pop().unwrap();
    let val = self_.stack.pop().unwrap();
    match parent {
        Value::Object(map)
        | Value::Function(_, map)
        | Value::NeedThis(box Value::Function(_, map)) => unsafe {
            **map
                .borrow_mut()
                .entry(member)
                .or_insert_with(|| alloc_for_value()) = val;
        },
        e => unreachable!("{:?}", e),
    }
    true
}

#[inline]
fn get_global(self_: &mut VM2) -> bool {
    self_.pc += 1; // get_global
    get_int32!(self_, n, usize);
    unsafe {
        let val = (**(*self_.global_objects)
            .borrow()
            .get(self_.const_str_table[n].as_str())
            .unwrap())
            .clone();
        self_.stack.push(val);
    }
    true
}

#[inline]
fn set_global(self_: &mut VM2) -> bool {
    self_.pc += 1; // set_global
    get_int32!(self_, n, usize);
    unsafe {
        **(*self_.global_objects)
            .borrow_mut()
            .entry(self_.const_str_table[n].clone())
            .or_insert_with(|| alloc_for_value()) = self_.stack.pop().unwrap();
    }
    true
}

#[inline]
fn get_local(self_: &mut VM2) -> bool {
    self_.pc += 1; // get_local
    get_int32!(self_, n, usize);
    let val = self_.stack[self_.bp + n].clone();
    self_.stack.push(val);
    true
}

#[inline]
fn set_local(self_: &mut VM2) -> bool {
    self_.pc += 1; // set_local
    get_int32!(self_, n, usize);
    let val = self_.stack.pop().unwrap();
    self_.stack[self_.bp + n] = val;
    true
}

#[inline]
fn jmp(self_: &mut VM2) -> bool {
    self_.pc += 1; // jmp
    get_int32!(self_, dst, isize);
    self_.pc += dst;
    true
}

#[inline]
fn jmp_if_false(self_: &mut VM2) -> bool {
    self_.pc += 1; // jmp_if_false
    get_int32!(self_, dst, isize);
    let cond = self_.stack.pop().unwrap();
    if let Value::Bool(false) = cond {
        self_.pc += dst
    }
    true
}

#[inline]
fn call(self_: &mut VM2) -> bool {
    self_.pc += 1; // Call
    get_int32!(self_, argc, usize);

    let mut this = None;

    let mut callee = self_.stack.pop().unwrap();

    loop {
        match callee {
            Value::EmbeddedFunction(1) => {
                let mut args = vec![];
                for _ in 0..argc {
                    args.push(self_.stack.pop().unwrap());
                }
                args.reverse();
                console_log(args);
                break;
            }
            Value::Function(dst, _) => {
                self_.return_addr.push(self_.pc);
                if let Some(this) = this {
                    let pos = self_.stack.len() - argc;
                    self_.stack.insert(pos, this);
                }
                self_.pc = dst as isize;
                self_.do_run();
                break;
            }
            Value::NeedThis(callee_) => {
                this = Some(Value::Object(self_.global_objects.clone()));
                callee = *callee_;
            }
            Value::WithThis(callee_, this_) => {
                this = Some(*this_);
                callee = *callee_;
            }
            c => {
                println!("Call: err: {:?}, pc = {}", c, self_.pc);
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
    true
}

#[inline]
fn return_(self_: &mut VM2) -> bool {
    let val = self_.stack.pop().unwrap();
    let former_sp = self_.sp_history.pop().unwrap();
    self_.stack.truncate(former_sp);
    self_.stack.push(val);
    self_.pc = self_.return_addr.pop().unwrap();
    self_.bp = self_.bp_buf.pop().unwrap();
    false
}

#[rustfmt::skip]
pub fn vm2_test() {
    let mut vm2 = VM2::new();
    vm2.constant_table.push(Value::Function(38, Rc::new(RefCell::new(HashMap::new()))));
    unsafe {
        vm2.constant_table.push(Value::String(alloc_rawstring("log")));
    }
    vm2.const_str_table.push("console".to_string());

    // Loop for 100,000,000
    // AllocLocalVar(1, 1)
    // Push(Number(0.0))
    // SetLocal(1)
    // GetLocal(1)
    // Push(Number(100000000.0))
    // Lt
    // JmpIfFalse(6)
    // GetLocal(1)
    // Push(Number(1.0))
    // Add
    // SetLocal(1)
    // Jmp(-8)
    // End
    // vm2.run(vec![
    //         CREATE_CONTEXT, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, // CreateContext 1, 1
    //         PUSH_INT32, 0x00, 0x00, 0x00, 0x00, // PushInt 0
    //         SET_LOCAL, 0x01, 0x00, 0x00, 0x00, // SetLocal 1
    //         GET_LOCAL, 0x01, 0x00, 0x00, 0x00, // GetLocal 1
    //         PUSH_INT32, 0x00, 0xe1, 0xf5, 0x05, // PushInt 100,000,000
    //         LT, // Lt
    //         JMP_IF_FALSE, 0x15, 0x00, 0x00, 0x00, // JmpIfFalse 21
    //         GET_LOCAL, 0x01, 0x00, 0x00, 0x00, // GetLocal 1
    //         PUSH_INT32, 0x01, 0x00, 0x00, 0x00, // PushInt 1
    //         ADD, // Add
    //         SET_LOCAL, 0x01, 0x00, 0x00, 0x00, // SetLocal 1
    //         JMP, 0xdb, 0xff, 0xff, 0xff, // Jmp -37
    //         END, // End
    // ]);

    // Fibo 10
    // AllocLocalVar(0, 1)
    // Push(Number(10.0))
    // Push(Function(5, RefCell { value: {} }))
    // Call(1)
    // End
    // AllocLocalVar(0, 1)
    // GetLocal(0)
    // Push(Number(2.0))
    // Lt
    // JmpIfFalse(3)
    // Push(Number(1.0))
    // Return
    // GetLocal(0)
    // Push(Number(1.0))
    // Sub
    // Push(Function(5, RefCell { value: {} }))
    // Call(1) 
    // GetLocal(0)
    // Push(Number(2.0))
    // Sub
    // Push(Function(5, RefCell { value: {} }))
    // Call(1)
    // Add
    // Return
    vm2.run(vec![
        CREATE_CONTEXT, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, // CreateContext 1, 1
        PUSH_INT8, 0x0a, // PushInt 10
        PUSH_CONST, 0x00, 0x00, 0x00, 0x00, // PushConst 0
        CALL, 0x01, 0x00, 0x00, 0x00, // Call 1
        GET_GLOBAL, 0x00, 0x00, 0x00, 0x00, // GetGlobal 0 (console)
        PUSH_CONST, 0x01, 0x00, 0x00, 0x00, // PushConst 1 (log)
        GET_MEMBER, // GetMember
        CALL, 0x01, 0x00, 0x00, 0x00, // Call 1
        END, // End
        CREATE_CONTEXT, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, // CreateContext 0, 1
        GET_LOCAL, 0x00, 0x00, 0x00, 0x00, // GetLocal 0
        PUSH_INT8, 0x02, // PushInt 2
        LT, // Lt
        JMP_IF_FALSE, 0x03, 0x00, 0x00, 0x00, // JmpIfFalse 3
        PUSH_INT8, 0x01, // PushInt 1
        RETURN, // Return
        GET_LOCAL, 0x00, 0x00, 0x00, 0x00, // GetLocal 0
        PUSH_INT8, 0x01, // PushInt 1
        SUB, // Sub
        PUSH_CONST, 0x00, 0x00, 0x00, 0x00, // PushConst 0
        CALL, 0x01, 0x00, 0x00, 0x00, // Call 1
        GET_LOCAL, 0x00, 0x00, 0x00, 0x00, // GetLocal 0
        PUSH_INT8, 0x02, // PushInt 2
        SUB, // Sub
        PUSH_CONST, 0x00, 0x00, 0x00, 0x00, // PushConst 0
        CALL, 0x01, 0x00, 0x00, 0x00, // Call 1
        ADD, // Add
        RETURN, // Return
    ]);
}
