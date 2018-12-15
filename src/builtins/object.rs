use gc;
use vm::{
    callobj::CallObject,
    error::RuntimeError,
    value::{Value, ValueBase},
    vm::VM,
};

use rustc_hash::FxHashMap;

thread_local!(
    pub static OBJECT_PROTOTYPE: Value = {
        make_object!(
            __proto__: Value::null()
        )
    };

    pub static OBJECT_OBJ: Value = {
        let prototype = OBJECT_PROTOTYPE.with(|x| x.clone());
        let object = Value::builtin_function_with_obj_and_prototype(
            object_new,
            None,
            CallObject::new(Value::undefined()),
            make_hashmap!(),
            prototype.clone()
        );

        prototype.set_constructor(object.clone());

        object
    }
);

/// https://www.ecma-international.org/ecma-262/6.0/#sec-object-objects
pub fn object_new(vm: &mut VM, args: &Vec<Value>, _: &CallObject) -> Result<(), RuntimeError> {
    if args.len() == 0 {
        vm.set_return_value(make_object!());
        return Ok(());
    }

    match &args[0].val {
        ValueBase::Null | ValueBase::Undefined => {
            vm.set_return_value(make_object!());
            return Ok(());
        }
        ValueBase::Empty => unreachable!(),
        _ => {
            // TODO: Follow the specification
            vm.set_return_value(args[0].clone());
            return Ok(());
        }
    }
}
//
// pub fn function_prototype_apply(
//     vm: &mut VM,
//     args: &Vec<Value>,
//     callobj: &CallObject,
// ) -> Result<(), RuntimeError> {
//     let callee = &*callobj.this;
//     let arg_this = args[0].clone();
//     let arg = match args[1].val {
//         ValueBase::Array(aryval) => {
//             let aryval = unsafe { &*aryval };
//             let mut elems = vec![];
//             for i in 0..aryval.length {
//                 elems.push(aryval.elems[i].clone());
//             }
//             elems
//         }
//         ValueBase::Arguments => {
//             let mut elems = vec![];
//             let callobj = unsafe { &**vm.state.scope.last().unwrap() };
//             let length = callobj.get_arguments_length();
//             for i in 0..length {
//                 elems.push(callobj.get_arguments_nth_value(i).unwrap());
//             }
//             elems
//         }
//         _ => vec![],
//     };
//
//     match callee.val {
//         ValueBase::BuiltinFunction(box (ref info, _, ref callobj)) => {
//             let mut callobj = callobj.clone();
//             *callobj.this = arg_this;
//             (info.func)(vm, &arg, &callobj)?;
//         }
//         ValueBase::Function(box (id, ref iseq, _, ref callobj)) => {
//             let mut callobj = callobj.clone();
//             *callobj.this = arg_this;
//             call_function(vm, id, iseq, &arg, callobj).unwrap();
//         }
//         _ => vm.state.stack.push(Value::undefined()),
//     };
//     Ok(())
// }
//
// pub fn function_prototype_call(
//     vm: &mut VM,
//     args: &Vec<Value>,
//     callobj: &CallObject,
// ) -> Result<(), RuntimeError> {
//     let callee = &*callobj.this;
//     let arg_this = args[0].clone();
//     match callee.val {
//         ValueBase::BuiltinFunction(box (ref info, _, ref callobj)) => {
//             let mut callobj = callobj.clone();
//             *callobj.this = arg_this;
//             (info.func)(vm, &args[1..].to_vec(), &callobj)?;
//         }
//         ValueBase::Function(box (id, ref iseq, _, ref callobj)) => {
//             let mut callobj = callobj.clone();
//             *callobj.this = arg_this;
//             call_function(vm, id, iseq, &args[1..].to_vec(), callobj).unwrap();
//         }
//         _ => vm.state.stack.push(Value::undefined()),
//     };
//     Ok(())
// }
