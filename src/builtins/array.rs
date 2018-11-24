use builtin;
use gc;
use vm::{
    callobj::CallObject, value::{ArrayValue, Value, ValueBase}, vm::{call_function, VM},
};

use rustc_hash::FxHashMap;

thread_local!(
    pub static ARRAY_PROTOTYPE: *mut ArrayValue = {
        let mut prototype = FxHashMap::default();

        prototype.insert(
            "push".to_string(),
            Value::builtin_function(
                array_prototype_push,
                builtin::Builtins::ArrayPrototypePush,
                CallObject::new(Value::new(ValueBase::Undefined)),
            ),
        );

        prototype.insert(
            "pop".to_string(),
            Value::builtin_function(
                array_prototype_pop,
                builtin::Builtins::ArrayPrototypePop,
                CallObject::new(Value::new(ValueBase::Undefined)),
            ),
        );

        prototype.insert(
            "map".to_string(),
            Value::builtin_function(
                array_prototype_map,
                builtin::Builtins::ArrayPrototypeMap,
                CallObject::new(Value::new(ValueBase::Undefined)),
            ),
        );

        // https://www.ecma-international.org/ecma-262/7.0/#sec-properties-of-the-array-prototype-object
        // TODO: precise implementation
        gc::new(ArrayValue {
            elems: vec![],
            length: 0,
            obj: prototype
        })
    };

    pub static ARRAY_OBJ: Value = {
        let prototype = ArrayValue::prototype();
        let array = Value::builtin_function_with_obj_and_prototype(
            array_new,
            builtin::Builtins::ArrayNew,
            CallObject::new(Value::undefined()),
            {
                let obj = FxHashMap::default();
                // TODO: Add:
                //          - Array.from()
                //          - Array.isArray()
                //          - Array.observe()
                //          - Array.of()
                //          etc...
                obj
            },
            Value::array(ArrayValue::prototype()),
        );

        unsafe {(*prototype).obj.insert("constructor".to_string(), array.clone()); }
        array
    }
);

pub fn array_new(vm: &mut VM, args: &Vec<Value>, _: &CallObject) {
    let args_len = args.len();

    if args_len == 0 {
        vm.state
            .stack
            .push(Value::array(gc::new(ArrayValue::new(vec![]))));
        gc::mark_and_sweep(&vm.state);
        return;
    }

    let mut elems = vec![];

    match args[0].val {
        ValueBase::Number(length) if args_len == 1 => {
            for _ in 0..length as usize {
                elems.push(Value::empty());
            }
        }
        _ => {
            for arg in args {
                elems.push(arg.clone());
            }
        }
    }

    vm.state
        .stack
        .push(Value::array(gc::new(ArrayValue::new(elems))));

    gc::mark_and_sweep(&vm.state);
}

pub fn array_prototype_push(vm: &mut VM, args: &Vec<Value>, callobj: &CallObject) {
    let array = if let ValueBase::Array(ref array) = callobj.this.val {
        unsafe { &mut **array }
    } else {
        vm.state.stack.push(Value::undefined());
        return;
    };

    for val in args {
        array.elems.push(val.clone());
    }

    array.length += args.len();

    vm.state.stack.push(Value::number(array.length as f64))
}

pub fn array_prototype_pop(vm: &mut VM, _args: &Vec<Value>, callobj: &CallObject) {
    let array = if let ValueBase::Array(ref array) = callobj.this.val {
        unsafe { &mut **array }
    } else {
        vm.state.stack.push(Value::undefined());
        return;
    };

    if let Some(val) = array.elems.pop() {
        array.length -= 1;
        vm.state.stack.push(val);
        return;
    }

    vm.state.stack.push(Value::undefined())
}

pub fn array_prototype_map(vm: &mut VM, args: &Vec<Value>, callobj: &CallObject) {
    let array = if let ValueBase::Array(ref array) = callobj.this.val {
        unsafe { &mut **array }
    } else {
        vm.state.stack.push(Value::undefined());
        return;
    };

    let mut new_array = ArrayValue::new(vec![]);
    let callback = &args[0];

    let mut args_for_callback = vec![
        Value::undefined(),
        Value::number(0.0),
        /* array itself = */ (*callobj.this).clone(),
    ];

    for i in 0..array.length {
        args_for_callback[0] = array.elems[i].clone();
        args_for_callback[1].set_number_if_possible(i as f64);

        match callback.val {
            ValueBase::BuiltinFunction(box (ref info, _, ref callobj)) => {
                // let mut callobj = callobj.clone();
                // *callobj.this = arg_this;
                (info.func)(vm, &args_for_callback, callobj);
            }
            ValueBase::Function(box (id, ref iseq, _, ref callobj)) => {
                let mut callobj = callobj.clone();
                // *callobj.this = arg_this;
                unsafe {
                    (*callobj.vals).clear();
                }
                call_function(vm, id, iseq, &args_for_callback, callobj).unwrap();
            }
            _ => vm.state.stack.push(Value::undefined()),
        }

        let val = vm.state.stack.pop().unwrap();
        new_array.push(val);
    }

    vm.state.stack.push(Value::array(gc::new(new_array)))
}
