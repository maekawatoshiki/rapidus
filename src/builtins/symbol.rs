use gc::MemoryAllocator;
use vm::{
    frame,
    jsvalue::value::*,
    vm::{VMResult, VM2},
};

pub fn symbol(
    memory_allocator: &mut MemoryAllocator,
    object_prototypes: &ObjectPrototypes,
) -> Value2 {
    let obj = Value2::builtin_function(
        memory_allocator,
        object_prototypes,
        "Symbol".to_string(),
        symbol_constructor,
    );

    // Symbol.for
    obj.set_property_by_string_key("for".to_string(), {
        Value2::builtin_function(
            memory_allocator,
            object_prototypes,
            "for".to_string(),
            symbol_for,
        )
    });

    obj.set_property_by_string_key("prototype".to_string(), object_prototypes.symbol);
    obj.get_property_by_str_key("prototype")
        .set_constructor(obj);
    obj
}

pub fn symbol_constructor(vm: &mut VM2, args: &[Value2], _cur_frame: &frame::Frame) -> VMResult {
    let symbol = Value2::symbol(
        &mut vm.memory_allocator,
        &vm.object_prototypes,
        args.get(0).map(|arg| arg.to_string()),
    );
    vm.stack.push(symbol.into());
    Ok(())
}

pub fn symbol_for(vm: &mut VM2, args: &[Value2], _cur_frame: &frame::Frame) -> VMResult {
    let sym = vm.global_symbol_registry.for_(
        &mut vm.memory_allocator,
        &vm.object_prototypes,
        args.get(0).unwrap_or(&Value2::undefined()).to_string(),
    );
    vm.stack.push(sym.into());
    Ok(())
}
