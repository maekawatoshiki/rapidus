use builtin;
use builtin::{BuiltinFuncInfo, BuiltinFuncTy};
use gc;
use vm::{
    callobj::CallObject,
    value::{Value, ValueBase},
    vm::VM,
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
            builtin_function(builtin::function_prototype_apply),
        );

        prototype.insert(
            "call".to_string(),
            builtin_function(builtin::function_prototype_call),
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

pub fn function_new(vm: &mut VM, args: &Vec<Value>, _: &CallObject) {
    unimplemented!("sorry");
}
