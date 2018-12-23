use builtins::object;
use vm::{
    callobj::CallObject,
    error::RuntimeError,
    value::Value,
    vm::{call_function, VM},
};

thread_local! {
    pub static FUNCTION_PROTOTYPE: Value = {
        let nvp = make_nvp!(
            apply:      Value::default_builtin_function(prototype_apply),
            call:       Value::default_builtin_function(prototype_call),
            __proto__:  object::OBJECT_PROTOTYPE.with(|x| x.clone())
        );

        Value::Function(
             Box::new((0, vec![], Value::propmap_from_nvp(&nvp), CallObject::new(Value::undefined()))))
    };
}

pub fn init() -> Value {
    let prototype = FUNCTION_PROTOTYPE.with(|x| x.clone());
    //Value::insert_propmap(prototype, svp: &Vec<(&'static str, Value)>)
    let obj = Value::builtin_constructor_from_nvp(new, &mut vec![], Some(prototype.clone()));
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
        Value::Array(aryval) => {
            let aryval = unsafe { &*aryval };
            let mut elems = vec![];
            for i in 0..aryval.length {
                elems.push(aryval.elems[i].val.clone());
            }
            elems
        }
        Value::Arguments => {
            let mut elems = vec![];
            let callobj = unsafe { &**vm.state.scope.last().unwrap() };
            let length = callobj.get_arguments_length();
            for i in 0..length {
                elems.push(callobj.get_arguments_nth_value(i).unwrap());
            }
            elems
        }
        _ => vec![],
    };

    match callee {
        Value::BuiltinFunction(box (ref info, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            (info.func)(vm, &arg, &callobj)?;
        }
        Value::Function(box (id, ref iseq, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            call_function(vm, *id, iseq, &arg, callobj).unwrap();
        }
        _ => vm.state.stack.push(Value::undefined()),
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
        Value::BuiltinFunction(box (ref info, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            (info.func)(vm, &args[1..].to_vec(), &callobj)?;
        }
        Value::Function(box (id, ref iseq, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            call_function(vm, *id, iseq, &args[1..].to_vec(), callobj).unwrap();
        }
        _ => vm.state.stack.push(Value::undefined()),
    };
    Ok(())
}
