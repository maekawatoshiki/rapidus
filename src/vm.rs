use std::collections::HashMap;
use std::collections::VecDeque;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Function(Vec<i32>), // i32 will replaced with appropriate type.
    EmbeddedFunction,
    Object(HashMap<String, Object>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Object {
    pub name: Option<String>,
    pub val: Value,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    Push(Object),
    GetGlobal(String),
    GetLocal(String),
    SetGlobal(String, Object),
    Call(Object, usize),
}

pub struct VM {
    pub global_objects: HashMap<String, Object>,
    pub stack: VecDeque<Object>,
}

impl Object {
    pub fn new(name: Option<String>, val: Value) -> Object {
        Object {
            name: name,
            val: val,
        }
    }

    pub fn new_no_name(val: Value) -> Object {
        Object {
            name: None,
            val: val,
        }
    }
}

impl VM {
    pub fn new() -> VM {
        let mut obj = HashMap::new();
        obj.insert(
            "console".to_string(),
            Object::new(
                Some("console".to_string()),
                Value::Object({
                    let mut obj = HashMap::new();
                    obj.insert(
                        "log".to_string(),
                        Object::new(Some("log".to_string()), Value::EmbeddedFunction),
                    );
                    obj
                }),
            ),
        );

        VM {
            global_objects: obj,
            stack: VecDeque::new(),
        }
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
                Inst::Call(
                    Object {
                        val: Value::EmbeddedFunction,
                        name,
                    },
                    argc,
                ) => self.run_embedded_function(name.unwrap(), argc),
                _ => {}
            }
        }
    }

    fn run_embedded_function(&mut self, name: String, argc: usize) {
        let mut args = vec![];
        for _ in 0..argc {
            args.push(self.stack.pop_back().unwrap());
        }

        let parent = self.stack.pop_back().unwrap();
        if parent.name == Some("console".to_string()) {
            if name == "log" {
                match args[0].val {
                    Value::String(ref s) => println!("{}", s),
                    Value::Number(ref n) => println!("{}", *n),
                    _ => {}
                }
            }
        }
    }
}

pub fn test() {
    let insts = vec![
        Inst::GetGlobal("console".to_string()),
        Inst::Push(Object::new_no_name(Value::String("hello".to_string()))),
        Inst::Call(
            Object::new(Some("log".to_string()), Value::EmbeddedFunction),
            1,
        ),
        Inst::SetGlobal(
            "n".to_string(),
            Object::new(Some("n".to_string()), Value::Number(123.0)),
        ),
        Inst::GetGlobal("console".to_string()),
        Inst::GetGlobal("n".to_string()),
        Inst::Call(
            Object::new(Some("log".to_string()), Value::EmbeddedFunction),
            1,
        ),
    ];
    let mut vm = VM::new();
    vm.run(insts);
}
