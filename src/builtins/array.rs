use builtins::object::*;
use gc;
use vm::{
    error::RuntimeError,
    frame::Frame,
    jsvalue::{prototype::ObjectPrototypes, value::Value2},
    value::*,
    vm::{VMResult, VM, VM2},
};

pub fn array(
    memory_allocator: &mut gc::MemoryAllocator,
    object_prototypes: &ObjectPrototypes,
) -> Value2 {
    let ary = Value2::builtin_function(
        memory_allocator,
        object_prototypes,
        "Array".to_string(),
        array_constructor,
    );
    ary.set_property_by_string_key("prototype".to_string(), object_prototypes.array);
    ary.get_property_by_str_key("prototype")
        .set_constructor(ary);
    ary
}

pub fn array_constructor(vm: &mut VM2, _args: &[Value2], _cur_frame: &Frame) -> VMResult {
    vm.stack.push(Value2::undefined().into());
    Ok(())
}

//////////////////////////////////////////////////

thread_local!(
    pub static ARRAY_PROTOTYPE: Value = {
        let map = Value::propmap_from_npp(&make_npp!(
            // https://www.ecma-international.org/ecma-262/7.0/#sec-properties-of-the-array-prototype-object
            // TODO: precise implementation
            push: Value::default_builtin_function(prototype_push),
            pop: Value::default_builtin_function(prototype_pop),
            map: Value::default_builtin_function(prototype_map),
            __proto__: OBJECT_PROTOTYPE.with(|x| x.clone())
        ));

        Value::Object(
            map,
            ObjectKind::Array(gc::new(ArrayValue {
                elems: vec![],
                length: 0,
            })),
        )
    };
);
pub fn init() -> Value {
    let mut prototype = ARRAY_PROTOTYPE.with(|x| x.clone());
    let array = Value::builtin_function(
        prototype_new,
        None,
        &mut vec![
            // TODO: Add:
            //          - Array.from()
            //          - Array.isArray()
            //          - Array.observe()
            //          - Array.of()
            //          etc...
        ],
        Some(prototype.clone()),
    );
    prototype.set_constructor(array.clone());

    array
}

fn prototype_new(vm: &mut VM, args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
    let args_len = args.len();

    if args_len == 0 {
        vm.set_return_value(Value::array_from_elems(vec![]));
        gc::mark_and_sweep(vm);
        return Ok(());
    }

    let mut elems = vec![];

    match args[0] {
        Value::Number(length) if args_len == 1 => {
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

    vm.set_return_value(Value::array_from_elems(elems));
    gc::mark_and_sweep(vm);

    Ok(())
}

fn prototype_push(
    vm: &mut VM,
    args: &Vec<Value>,
    callobj: CallObjectRef,
) -> Result<(), RuntimeError> {
    let mut array = if let Value::Object(_, ObjectKind::Array(array)) = *callobj.this.clone() {
        array
    } else {
        vm.set_return_value(Value::Undefined);
        return Err(RuntimeError::Type(
            "Array.prototype.push called on non-array object.".to_string(),
        ));
    };

    for val in args {
        array.elems.push(val.to_property());
    }

    array.length += args.len();

    vm.set_return_value(Value::Number(array.length as f64));

    Ok(())
}

fn prototype_pop(
    vm: &mut VM,
    _args: &Vec<Value>,
    callobj: CallObjectRef,
) -> Result<(), RuntimeError> {
    let mut array = if let Value::Object(_, ObjectKind::Array(ref array)) = *callobj.this {
        array.clone()
    } else {
        vm.set_return_value(Value::Undefined);
        return Err(RuntimeError::Unknown);
    };

    if let Some(prop) = array.clone().elems.pop() {
        array.length -= 1;
        vm.set_return_value(prop.val);
        return Ok(());
    }

    vm.set_return_value(Value::Undefined);

    Ok(())
}

pub fn prototype_map(
    vm: &mut VM,
    args: &Vec<Value>,
    callobj: CallObjectRef,
) -> Result<(), RuntimeError> {
    let array = if let Value::Object(_, ObjectKind::Array(ref array)) = *callobj.this {
        array.clone()
    } else {
        vm.state.stack.push(Value::Undefined);
        return Err(RuntimeError::Unknown);
    };

    let mut new_array = vec![];
    let callback = &args[0];

    let mut args_for_callback = vec![
        Value::Undefined,
        Value::Number(0.0),
        /* array itself = */ (*callobj.this).clone(),
    ];

    for i in 0..array.length {
        args_for_callback[0] = array.elems[i].val.clone();
        args_for_callback[1].set_number_if_possible(i as f64);

        vm.call_function_simply(callback, &args_for_callback)?;

        let val = vm.state.stack.pop().unwrap();
        new_array.push(val);
    }

    vm.set_return_value(Value::array_from_elems(new_array));

    Ok(())
}
