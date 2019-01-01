use builtins::object;
use vm::{
    callobj::CallObject,
    error::RuntimeError,
    value::*,
    vm::{call_function, VM},
};

thread_local! {
    pub static FUNCTION_PROTOTYPE: Value = {
        let map = Value::propmap_from_npp(&make_npp!(
            length:     Value::Number(0f64),
            name:       Value::string("".to_string()),
            apply:      Value::default_builtin_function(prototype_apply),
            call:       Value::default_builtin_function(prototype_call),
            __proto__:  object::OBJECT_PROTOTYPE.with(|x| x.clone())
        ));
        let co = CallObject::new(Value::Undefined);
        let kind = ObjectKind::Function(
            Box::new((
                FuncInfo::new(0,vec![],vec![]),
                co.clone()
            ))
        );
        Value::Object(map, kind)
    };
}

pub fn init() -> Value {
    let prototype = FUNCTION_PROTOTYPE.with(|x| x.clone());
    // Function constructor
    let mut npp = &mut make_npp!(
        length: Value::Number(1f64)
    );
    let obj = Value::builtin_function(new, None, &mut npp, Some(prototype.clone()));
    prototype.set_constructor(obj.clone());

    obj
}

fn new(_vm: &mut VM, _args: &Vec<Value>, _: &CallObject) -> Result<(), RuntimeError> {
    unimplemented!("sorry");
}

fn prototype_apply(
    vm: &mut VM,
    args: &Vec<Value>,
    callobj: &CallObject,
) -> Result<(), RuntimeError> {
    let callee = &*callobj.this;
    let arg_this = args[0].clone();
    let arg = match args[1] {
        Value::Object(_, ObjectKind::Array(aryval)) => {
            let aryval = unsafe { &*aryval };
            let mut elems = vec![];
            for i in 0..aryval.length {
                elems.push(aryval.elems[i].val.clone());
            }
            elems
        }
        Value::Object(_, ObjectKind::Arguments(callobj)) => {
            let mut elems = vec![];
            let callobj = unsafe { &*callobj.clone() };
            let length = callobj.get_arguments_length();
            for i in 0..length {
                elems.push(callobj.get_arguments_nth_value(i).unwrap());
            }
            elems
        }
        _ => vec![],
    };

    match callee {
        Value::Object(_, ObjectKind::BuiltinFunction(box (ref info, ref callobj))) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            (info.func)(vm, &arg, &callobj)?;
        }
        Value::Object(_, ObjectKind::Function(box (func_info, ref callobj))) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            call_function(vm, func_info.clone(), &mut callobj, &arg).unwrap();
        }
        _ => vm.state.stack.push(Value::Undefined),
    };
    Ok(())
}

fn prototype_call(
    vm: &mut VM,
    args: &Vec<Value>,
    callobj: &CallObject,
) -> Result<(), RuntimeError> {
    let callee = &*callobj.this;
    let arg_this = args[0].clone();
    match callee {
        Value::Object(_, ObjectKind::BuiltinFunction(box (ref info, ref callobj))) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            (info.func)(vm, &args[1..].to_vec(), &callobj)?;
        }
        Value::Object(_, ObjectKind::Function(box (func_info, ref callobj))) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            call_function(vm, func_info.clone(), &mut callobj, &args[1..].to_vec()).unwrap();
        }
        _ => vm.state.stack.push(Value::Undefined),
    };
    Ok(())
}
