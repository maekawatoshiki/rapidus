use crate::gc::MemoryAllocator;
use crate::vm::{
    error::RuntimeError,
    frame,
    jsvalue::value::*,
    vm::{VMResult, VM},
};

pub fn symbol(
    memory_allocator: &mut MemoryAllocator,
    object_prototypes: &ObjectPrototypes,
) -> Value {
    let obj = Value::builtin_function(
        memory_allocator,
        object_prototypes,
        "Symbol".to_string(),
        symbol_constructor,
    );

    // Symbol.for
    obj.set_property_by_string_key("for".to_string(), {
        Value::builtin_function(
            memory_allocator,
            object_prototypes,
            "for".to_string(),
            symbol_for,
        )
    });
    // Symbol.keyFor
    obj.set_property_by_string_key("keyFor".to_string(), {
        Value::builtin_function(
            memory_allocator,
            object_prototypes,
            "keyFor".to_string(),
            symbol_key_for,
        )
    });

    obj.set_property_by_string_key("prototype".to_string(), object_prototypes.symbol);
    obj.get_property_by_str_key("prototype")
        .set_constructor(obj);
    obj
}

pub fn symbol_constructor(vm: &mut VM, args: &[Value], _cur_frame: &frame::Frame) -> VMResult {
    let symbol = Value::symbol(
        &mut vm.factory.memory_allocator,
        &vm.factory.object_prototypes,
        args.get(0).map(|arg| arg.to_string()),
    );
    vm.stack.push(symbol.into());
    Ok(())
}

pub fn symbol_for(vm: &mut VM, args: &[Value], _cur_frame: &frame::Frame) -> VMResult {
    let sym = vm.global_symbol_registry.for_(
        &mut vm.factory.memory_allocator,
        &vm.factory.object_prototypes,
        args.get(0).unwrap_or(&Value::undefined()).to_string(),
    );
    vm.stack.push(sym.into());
    Ok(())
}

pub fn symbol_key_for(vm: &mut VM, args: &[Value], _cur_frame: &frame::Frame) -> VMResult {
    let sym = args.get(0).map(|x| *x).unwrap_or(Value::undefined());

    if !sym.is_symbol() {
        return Err(RuntimeError::Type(format!(
            "{} is not symbol",
            sym.debug_string(true)
        )));
    }

    let key = vm
        .global_symbol_registry
        .key_for(&mut vm.factory.memory_allocator, sym);
    vm.stack.push(key.into());
    Ok(())
}
