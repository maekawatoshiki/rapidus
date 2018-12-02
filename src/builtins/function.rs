use builtin;
use gc;
use vm::{
    callobj::CallObject, value::{Value, ValueBase}, vm::VM,
};
use builtin::{BuiltinFuncInfo, BuiltinFuncTy};

use rustc_hash::FxHashMap;

thread_local!(
    pub static FUNCTION_PROTOTYPE: *mut FxHashMap<String, Value> = {
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

        gc::new(prototype)
    }; 

    pub static FUNCTION_OBJ: Value = {
       let prototype = FUNCTION_PROTOTYPE.with(|x|*x);
       let empty_func = Value::new(
            ValueBase::Function(
            Box::new((0, vec![], prototype, CallObject::new(Value::undefined()))))
       );
       let obj = gc::new({
           let mut obj = FxHashMap::default();
           obj.insert("prototype".to_string(), empty_func.clone());
           obj
       });

       let function = Value::new(ValueBase::BuiltinFunction(Box::new((
           BuiltinFuncInfo::new(function_new, None),
           obj, CallObject::new(Value::undefined()),
       ))));

       unsafe { (*prototype).insert("constructor".to_string(), function.clone()); }
       unsafe {
           (*obj).insert(
               "__proto__".to_string(),
               empty_func.clone(),
           );
       }
   
       function
   }
);

pub fn function_new(vm: &mut VM, args: &Vec<Value>, _: &CallObject) {
    unimplemented!("sorry");
}
