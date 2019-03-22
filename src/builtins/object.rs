use gc;
use rustc_hash::FxHashMap;
use vm::{error::RuntimeError, value::*, vm::VM};
use vm::{frame, jsvalue, vm};

pub fn object(
    memory_allocator: &mut gc::MemoryAllocator,
    object_prototypes: &jsvalue::prototype::ObjectPrototypes,
) -> jsvalue::value::Value2 {
    let obj = jsvalue::value::Value2::builtin_function(
        memory_allocator,
        object_prototypes,
        "Object".to_string(),
        object_constructor,
    );
    obj.set_property_by_string_key("prototype".to_string(), object_prototypes.object);
    obj.get_property_by_str_key("prototype")
        .set_constructor(obj);
    obj
}

pub fn object_constructor(
    vm: &mut vm::VM2,
    args: &Vec<jsvalue::value::Value2>,
    _cur_frame: &frame::Frame,
) -> vm::VMResult {
    if args.len() == 0 {
        let empty_obj = jsvalue::value::Value2::object(
            vm.memory_allocator(),
            vm.code_generator.object_prototypes,
            FxHashMap::default(),
        );
        vm.stack.push(empty_obj.into());
        return Ok(());
    }

    match &args[0] {
        jsvalue::value::Value2::Other(jsvalue::value::NULL)
        | jsvalue::value::Value2::Other(jsvalue::value::UNDEFINED) => {
            let empty_obj = jsvalue::value::Value2::object(
                vm.memory_allocator(),
                vm.code_generator.object_prototypes,
                FxHashMap::default(),
            );
            vm.stack.push(empty_obj.into());
        }
        jsvalue::value::Value2::Other(jsvalue::value::EMPTY) => unreachable!(),
        _ => {
            // TODO: Follow the specification
            vm.stack.push(args[0].into());
        }
    }

    Ok(())
}

thread_local!(
    pub static OBJECT_PROTOTYPE: Value =
        // can not use Value::object_from_npp() here.
        { Value::Object(
            Value::propmap_from_npp(&make_npp!(
                __proto__: Value::Null,
                toString: Value::default_builtin_function(to_string)
            )),
            ObjectKind::Ordinary
        ) };
);

pub fn init() -> Value {
    let mut prototype = OBJECT_PROTOTYPE.with(|x| x.clone());
    // Object constructor
    let obj = Value::builtin_function(
        new,
        None,
        &mut make_npp!(create: Value::default_builtin_function(create)),
        Some(prototype.clone()),
    );
    prototype.set_constructor(obj.clone());

    obj
}

/// https://www.ecma-international.org/ecma-262/6.0/#sec-object-objects
fn new(vm: &mut VM, args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
    if args.len() == 0 {
        vm.set_return_value(Value::object_from_npp(&vec![]));
        return Ok(());
    }

    match &args[0] {
        Value::Null | Value::Undefined => {
            vm.set_return_value(Value::object_from_npp(&vec![]));
            return Ok(());
        }
        Value::Empty => unreachable!(),
        _ => {
            // TODO: Follow the specification
            vm.set_return_value(args[0].clone());
            return Ok(());
        }
    }
}

fn create(vm: &mut VM, args: &Vec<Value>, _: CallObjectRef) -> Result<(), RuntimeError> {
    let maybe_obj = match args.len() {
        0 => {
            return Err(RuntimeError::General(
                "Object.create needs one argument at least".to_string(),
            ));
        }
        1 => &args[0],
        // TODO: Implement the case when args.len() == 2
        _ => return Err(RuntimeError::Unimplemented),
    };

    let obj = match maybe_obj {
        Value::Object(map, ObjectKind::Ordinary) => {
            let new_obj = Value::object_from_npp(&vec![]);
            let proto = new_obj.get_property(Value::string("__proto__".to_string()), None);
            for (name, prop) in (*map).iter() {
                proto.clone().set_property(
                    Value::string(name.to_string()),
                    prop.clone().val,
                    None,
                )?;
            }
            new_obj
        }
        Value::Null => Value::object_from_npp(&vec![]),
        _ => {
            return Err(RuntimeError::Type(
                "type error: Object.create: 1st argument must be Object or null".to_string(),
            ));
        }
    };

    vm.set_return_value(obj);

    Ok(())
}

fn to_string(vm: &mut VM, _: &Vec<Value>, callobj: CallObjectRef) -> Result<(), RuntimeError> {
    let this = *callobj.this.clone();
    let obj = Value::string(this.to_string());
    vm.set_return_value(obj);

    Ok(())
}
