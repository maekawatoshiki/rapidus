use builtin::{BuiltinFuncInfo, BuiltinFuncTy};
use gc;
use vm::{
    callobj::CallObject,
    value::{Value, ValueBase},
    vm::{call_function, VM},
};

use rustc_hash::FxHashMap;

thread_local!(
    pub static FUNCTION_PROTOTYPE: Value = {
        pub fn builtin_function(func: BuiltinFuncTy) -> Value {
            let obj = FxHashMap::default();
            Value::new(ValueBase::BuiltinFunction(Box::new((
                BuiltinFuncInfo::new(func, None),
                gc::new(obj),
                CallObject::new(Value::undefined()),
            ))))
        }

        let mut prototype = FxHashMap::default();

        prototype.insert(
            "apply".to_string(),
            builtin_function(function_prototype_apply),
        );

        prototype.insert(
            "call".to_string(),
            builtin_function(function_prototype_call),
        );

        Value::new(
             ValueBase::Function(
             Box::new((0, vec![], gc::new(prototype), CallObject::new(Value::undefined()))))
        )
    };

    pub static FUNCTION_OBJ: Value = {
        let prototype = FUNCTION_PROTOTYPE.with(|x| x.clone());
        let obj = gc::new({
            let mut obj = FxHashMap::default();
            obj.insert("prototype".to_string(), prototype.clone());
            obj
        });

        let function = Value::new(ValueBase::BuiltinFunction(Box::new((
            BuiltinFuncInfo::new(function_new, None),
            obj, CallObject::new(Value::undefined()),
        ))));

        unsafe {
            if let Value { val: ValueBase::Function(box (_, _, obj, _)), .. } = prototype {
                 (*obj).insert("constructor".to_string(), function.clone());
            }
        }

        unsafe {
            (*obj).insert("__proto__".to_string(),prototype.clone());
        }

        function
    }
);

pub fn function_new(_vm: &mut VM, _args: &Vec<Value>, _: &CallObject) {
    unimplemented!("sorry");
}

pub fn function_prototype_apply(vm: &mut VM, args: &Vec<Value>, callobj: &CallObject) {
    let callee = &*callobj.this;
    let arg_this = args[0].clone();
    let arg = match args[1].val {
        ValueBase::Array(aryval) => {
            let aryval = unsafe { &*aryval };
            let mut elems = vec![];
            for i in 0..aryval.length {
                elems.push(aryval.elems[i].clone());
            }
            elems
        }
        ValueBase::Arguments => {
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

    match callee.val {
        ValueBase::BuiltinFunction(box (ref info, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            (info.func)(vm, &arg, &callobj);
        }
        ValueBase::Function(box (id, ref iseq, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            call_function(vm, id, iseq, &arg, callobj).unwrap();
        }
        _ => vm.state.stack.push(Value::undefined()),
    }
}

pub fn function_prototype_call(vm: &mut VM, args: &Vec<Value>, callobj: &CallObject) {
    let callee = &*callobj.this;
    let arg_this = args[0].clone();
    match callee.val {
        ValueBase::BuiltinFunction(box (ref info, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            (info.func)(vm, &args[1..].to_vec(), &callobj);
        }
        ValueBase::Function(box (id, ref iseq, _, ref callobj)) => {
            let mut callobj = callobj.clone();
            *callobj.this = arg_this;
            call_function(vm, id, iseq, &args[1..].to_vec(), callobj).unwrap();
        }
        _ => vm.state.stack.push(Value::undefined()),
    }
}
