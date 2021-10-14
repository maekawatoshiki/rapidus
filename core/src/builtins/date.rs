use crate::vm::{
    jsvalue::value::Value,
    vm::{Factory, VMValueResult, VM},
};
use chrono::Timelike;

pub fn date(factory: &mut Factory) -> Value {
    factory.generate_builtin_constructor("Date", date_constructor, factory.object_prototypes.date)
}

pub fn date_constructor(vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    Ok(vm.factory.date())
}

pub fn date_get_hours(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if !this.is_date_object() {
        return Err(vm.current_context.error_unknown());
    }

    let date = this.as_date().unwrap();
    Ok(Value::Number(date.utc().hour() as f64))
}

// use chrono::Utc;
// use vm::value::*;
// use vm::{error::RuntimeError, vm::VM};
//
// thread_local!(
//     pub static DATE_PROTOTYPE: Value = {
//         make_object!(
//             // TODO: Add methods
//         )
//     };
//
//     pub static DATE_OBJ: Value = {
//         let mut prototype = DATE_PROTOTYPE.with(|x| x.clone());
//         let date = Value::builtin_function(
//             date,
//             None,
//             &mut make_npp!(
//                 // TODO: Add methods
//                 now:    Value::default_builtin_function(date_now)
//             ),
//             Some(prototype.clone())
//         );
//
//         prototype.set_constructor(date.clone());
//
//         date
//     }
// );
//
// pub fn date(vm: &mut VM, _args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
//     let now = Utc::now();
//
//     vm.state.stack.push(Value::string(now.to_rfc3339()));
//
//     Ok(())
// }
//
// pub fn date_new(vm: &mut VM, _args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
//     let now = Utc::now();
//
//     vm.state.stack.push(Value::date(now));
//
//     Ok(())
// }
//
// pub fn date_now(vm: &mut VM, _args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
//     let now = Utc::now();
//     let now_millis = now.timestamp_millis();
//     vm.state.stack.push(Value::Number(now_millis as f64));
//     Ok(())
// }
