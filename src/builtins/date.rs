use gc;
use vm::{callobj::CallObject, error::RuntimeError, value::Value, vm::VM};

use chrono::Utc;
use rustc_hash::FxHashMap;

thread_local!(
    pub static DATE_PROTOTYPE: Value = {
        let prototype = FxHashMap::default();

        // TODO: Add methods

        Value::object(gc::new(prototype))
    };

    pub static DATE_OBJ: Value = {
        let prototype = DATE_PROTOTYPE.with(|x| x.clone());
        let date = Value::builtin_function(
            date,
            None,
            &mut vec![
                // TODO: Add methods
                (
                    "now",
                    Value::default_builtin_function(date_now),
                )
            ],
            Some(prototype.clone())
        );

        prototype.set_constructor(date.clone());

        date
    }
);

pub fn date(vm: &mut VM, _args: &Vec<Value>, _: &CallObject) -> Result<(), RuntimeError> {
    let now = Utc::now();

    vm.state.stack.push(Value::string(now.to_rfc3339()));

    Ok(())
}

pub fn date_new(vm: &mut VM, _args: &Vec<Value>, _: &CallObject) -> Result<(), RuntimeError> {
    let now = Utc::now();

    vm.state.stack.push(Value::date(now));

    Ok(())
}

pub fn date_now(vm: &mut VM, _args: &Vec<Value>, _: &CallObject) -> Result<(), RuntimeError> {
    let now = Utc::now();
    let now_millis = now.timestamp_millis();
    vm.state.stack.push(Value::number(now_millis as f64));
    Ok(())
}
