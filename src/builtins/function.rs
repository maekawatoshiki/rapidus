use builtin;
use gc;
use vm::{callobj::CallObject, value::Value};

use rustc_hash::FxHashMap;

thread_local!(
    pub static FUNCTION_PROTOTYPE: *mut FxHashMap<String, Value> = {
        let mut prototype = FxHashMap::default();

        prototype.insert(
            "apply".to_string(),
            Value::builtin_function(
                builtin::function_prototype_apply,
                builtin::Builtins::FunctionPrototypeApply,
                CallObject::new(Value::undefined()),
            ),
        );

        prototype.insert(
            "call".to_string(),
            Value::builtin_function(
                builtin::function_prototype_call,
                builtin::Builtins::FunctionPrototypeCall,
                CallObject::new(Value::undefined()),
            ),
        );

        gc::new(prototype)
    }; // pub static FUNCTION_OBJ: Value = {
    //     let prototype = FUNCTION_PROTOTYPE.with(|x|*x);
    //     let function = Value::builtin_function_with_obj_and_prototype(
    //         function_new,
    //         builtin::Builtins::FunctionNew,
    //         CallObject::new(Value::undefined()),
    //         {
    //             let obj = FxHashMap::default();
    //             // TODO: Add:
    //             //          - Array.from()
    //             //          - Array.isArray()
    //             //          - Array.observe()
    //             //          - Array.of()
    //             //          etc...
    //             obj
    //         },
    //         Value::new(ValueBase::Function(Box::new((0,vec![],prototype,CallObject::new(Value::undefined()))))),
    //     );
        //
        // unsafe {(*prototype).obj.insert("constructor".to_string(), array.clone()); }
        // array
        //
        // let mut val = Value::new(ValueBase::Function(Box::new((
        //     id,
        //     iseq,
        //     gc::new({
        //         let mut hm = FxHashMap::default();
        //         hm.insert(
        //             "prototype".to_string(),
        //             Value::new(ValueBase::Object(gc::new(FxHashMap::default()))),
        //         );
        //         hm.insert(
        //             "__proto__".to_string(),
        //             Value::new(ValueBase::Function(Box::new((
        //                 0,
        //                 vec![],
        //                 function::FUNCTION_PROTOTYPE.with(|x| *x),
        //                 CallObject::new(Value::undefined()),
        //             )))),
        //         );
        //         hm
        //     }),
        //     callobj,
        // ))));

        // let v2 = val.clone();
        // if let ValueBase::Function(box (_, _, ref mut obj, _)) = &mut val.val {
        //     // TODO: Add constructor of this function itself (==Function). (not prototype.constructor)
        //     unsafe {
        //         if let ValueBase::Object(ref mut obj) = (**obj).get_mut("prototype").unwrap().val {
        //             (**obj).insert("constructor".to_string(), v2);
        //         }
        //     }
        // }
    // }
);
