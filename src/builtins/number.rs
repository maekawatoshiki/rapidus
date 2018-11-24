use builtin;
use gc;
use vm::{
    callobj::CallObject, value::{Value, ValueBase}, vm::VM,
};

use rustc_hash::FxHashMap;
use std::ffi::CString;

thread_local!(
    pub static NUMBER_PROTOTYPE: *mut FxHashMap<String, Value> = {
        let mut prototype = FxHashMap::default();

        prototype.insert(
            "toString".to_string(),
            Value::builtin_function(
                number_prototype_tostring,
                builtin::Builtins::NumberPrototypeTostring,
                CallObject::new(Value::new(ValueBase::Undefined)),
            ),
        );

        gc::new(prototype)
    };
);

pub fn number_prototype_tostring(vm: &mut VM, args: &Vec<Value>, callobj: &CallObject) {
    let number = if let ValueBase::Number(num) = callobj.this.val {
        num
    } else {
        vm.state.stack.push(Value::undefined());
        return;
    };

    let base = match args.get(0) {
        Some(Value { val, .. }) => {
            let num = val.to_number();
            if num - num.floor() == 0.0 && 2.0 <= num && num <= 36.0 {
                num as usize
            } else {
                10
            }
        }
        _ => 10,
    };

    vm.state.stack.push(Value::string(
        CString::new(f64_to_string(number, base)).unwrap(),
    ));
}

// TODO: Maybe, this function had better be somewhere else. (like ./src/util.rs)
fn f64_to_string(f: f64, radix: usize) -> String {
    if f.is_nan() {
        return "NaN".to_string();
    }
    if f.is_infinite() {
        return "Infinity".to_string();
    }

    let chars = "0123456789abcdefghijklmnopqrstuvwxyz";
    let negative = f < 0.0;
    let mut integer = f.abs() as usize;
    let mut fraction = f.abs() - integer as f64;
    let mut s = "".to_string();

    while integer > 0 {
        s.push(chars[(integer % radix)..].chars().next().unwrap());
        integer /= radix;
    }

    s = s.chars().rev().collect();
    if fraction != 0.0 {
        s.push('.');
    }

    let mut count = 0;
    while fraction > 0.0 && count < /* digits = */14 {
        fraction *= radix as f64;
        s.push(chars[fraction as usize..].chars().next().unwrap());
        fraction -= fraction.floor();
        count += 1;
    }

    if negative {
        s.insert(0, '-')
    }

    s
}
