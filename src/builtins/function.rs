use builtins::object;
use gc;
use vm::{
    callobj::CallObject,
    error::RuntimeError,
    frame, jsvalue,
    jsvalue::value::Value2,
    value::*,
    vm,
    vm::{call_function, VM},
};

pub fn function(
    memory_allocator: &mut gc::MemoryAllocator,
    object_prototypes: &jsvalue::prototype::ObjectPrototypes,
) -> Value2 {
    let func = Value2::builtin_function(
        memory_allocator,
        object_prototypes,
        "Function".to_string(),
        function_constructor,
    );
    func.set_property_by_string_key("prototype".to_string(), object_prototypes.function);
    func.get_property_by_str_key("prototype")
        .set_constructor(func);
    func
}

pub fn function_constructor(
    vm: &mut vm::VM2,
    _args: &Vec<Value2>,
    _cur_frame: &frame::Frame,
) -> vm::VMResult {
    vm.stack.push(Value2::undefined().into());
    Ok(())
}

pub fn function_prototype_call(
    vm: &mut vm::VM2,
    args: &Vec<Value2>,
    cur_frame: &frame::Frame,
) -> vm::VMResult {
    let this_arg = *args.get(0).unwrap_or(&Value2::undefined());
    let func = cur_frame.this;
    vm.call_function(
        func,
        args.get(1..).unwrap_or(&[]).to_vec(),
        this_arg,
        cur_frame,
    )
}

/////////////////////////////////////////////////////////////////

thread_local! {
    pub static FUNCTION_PROTOTYPE: Value = {
        let map = Value::propmap_from_npp(&make_npp!(
            length:     Value::Number(0f64),
            name:       Value::string("".to_string()),
            apply:      Value::default_builtin_function(prototype_apply),
            call:       Value::default_builtin_function(prototype_call),
            __proto__:  object::OBJECT_PROTOTYPE.with(|x| x.clone())
        ));
        let co = CallObject::new_with_this(Value::Undefined);
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
    let mut prototype = FUNCTION_PROTOTYPE.with(|x| x.clone());
    // Function constructor
    let mut npp = &mut make_npp!(
        length: Value::Number(1f64)
    );
    let obj = Value::builtin_function(new, None, &mut npp, Some(prototype.clone()));
    prototype.set_constructor(obj.clone());

    obj
}

fn new(_vm: &mut VM, _args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
    unimplemented!("sorry");
}

fn prototype_apply(
    vm: &mut VM,
    args: &Vec<Value>,
    callobj: CallObjectRef,
) -> Result<(), RuntimeError> {
    let callee = &*callobj.this;
    let arg_this = args[0].clone();
    let arg = match args[1].clone() {
        Value::Object(_, ObjectKind::Array(aryval)) => {
            let aryval = &aryval;
            let mut elems = vec![];
            for i in 0..aryval.length {
                elems.push(aryval.elems[i].val.clone());
            }
            elems
        }
        Value::Object(_, ObjectKind::Arguments(state)) => {
            let mut elems = vec![];
            let length = state.get_arguments_length();
            for i in 0..length {
                elems.push(state.get_arguments_nth_value(i).unwrap());
            }
            elems
        }
        _ => vec![],
    };

    match callee {
        Value::Object(_, ObjectKind::BuiltinFunction(box (ref info, callobj))) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            (info.func)(vm, &arg, callobj)?;
        }
        Value::Object(_, ObjectKind::Function(box (func_info, callobj))) => {
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
    callobj: CallObjectRef,
) -> Result<(), RuntimeError> {
    let callee = &*callobj.this;
    let arg_this = args[0].clone();
    match callee {
        Value::Object(_, ObjectKind::BuiltinFunction(box (ref info, callobj))) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            (info.func)(vm, &args[1..].to_vec(), callobj)?;
        }
        Value::Object(_, ObjectKind::Function(box (func_info, callobj))) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            call_function(vm, func_info.clone(), &mut callobj, &args[1..].to_vec()).unwrap();
        }
        _ => vm.state.stack.push(Value::Undefined),
    };
    Ok(())
}
