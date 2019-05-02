use vm::{
    error::RuntimeError,
    frame,
    jsvalue::value::*,
    vm::{VMResult, VM2},
};

pub fn symbol(vm: &mut VM2) -> Value {
    let obj = vm
        .factory
        .builtin_function("Symbol".to_string(), symbol_constructor);

    // Symbol.for
    obj.set_property_by_string_key("for".to_string(), {
        vm.factory.builtin_function("for".to_string(), symbol_for)
    });
    // Symbol.keyFor
    obj.set_property_by_string_key("keyFor".to_string(), {
        vm.factory
            .builtin_function("keyFor".to_string(), symbol_key_for)
    });

    obj.set_property_by_string_key("prototype".to_string(), vm.factory.object_prototypes.symbol);
    obj.get_property_by_str_key("prototype")
        .set_constructor(obj);
    obj
}

pub fn symbol_constructor(vm: &mut VM2, args: &[Value], _cur_frame: &frame::Frame) -> VMResult {
    let symbol = vm.factory.symbol(args.get(0).map(|arg| arg.to_string()));
    vm.stack.push(symbol.into());
    Ok(())
}

pub fn symbol_for(vm: &mut VM2, args: &[Value], _cur_frame: &frame::Frame) -> VMResult {
    let sym = vm.symbol_registory_for_(args.get(0).unwrap_or(&Value::undefined()).to_string());
    vm.stack.push(sym.into());
    Ok(())
}

pub fn symbol_key_for(vm: &mut VM2, args: &[Value], _cur_frame: &frame::Frame) -> VMResult {
    let sym = args.get(0).map(|x| *x).unwrap_or(Value::undefined());

    if !sym.is_symbol() {
        return Err(RuntimeError::Type(format!(
            "{} is not symbol",
            sym.debug_string(true)
        )));
    }

    let key = vm.global_symbol_registry.key_for(&mut vm.factory, sym);
    vm.stack.push(key.into());
    Ok(())
}
