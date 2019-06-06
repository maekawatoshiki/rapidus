use crate::vm::{
    error::RuntimeError,
    frame,
    jsvalue::value::*,
    vm::{Factory, VMResult, VM},
};

pub fn symbol(factory: &mut Factory) -> Value {
    let obj = factory.builtin_function("Symbol", symbol_constructor);

    // Symbol.for
    obj.set_property_by_string_key("for", { factory.builtin_function("for", symbol_for) });
    // Symbol.keyFor
    obj.set_property_by_string_key("keyFor", {
        factory.builtin_function("keyFor", symbol_key_for)
    });

    obj.set_property_by_string_key("prototype", factory.object_prototypes.symbol);
    obj.get_property_by_str_key("prototype")
        .set_constructor(obj);
    obj
}

pub fn symbol_constructor(vm: &mut VM, args: &[Value], _cur_frame: &frame::Frame) -> VMResult {
    let symbol = vm.factory.symbol(args.get(0).map(|arg| arg.to_string()));
    vm.stack.push(symbol.into());
    Ok(())
}

pub fn symbol_for(vm: &mut VM, args: &[Value], _cur_frame: &frame::Frame) -> VMResult {
    let sym = vm.global_symbol_registry.for_(
        &mut vm.factory,
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

    let key = vm.global_symbol_registry.key_for(&mut vm.factory, sym);
    vm.stack.push(key.into());
    Ok(())
}
