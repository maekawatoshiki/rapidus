use super::helpers::set_function_length;
use crate::vm::{
    error::RuntimeError,
    jsvalue::object::{DataProperty, Property},
    jsvalue::symbol::{
        SYMBOL_ASYNC_ITERATOR_ID, SYMBOL_HAS_INSTANCE_ID, SYMBOL_IS_CONCAT_SPREADABLE_ID,
        SYMBOL_ITERATOR_ID, SYMBOL_MATCH_ALL_ID, SYMBOL_MATCH_ID, SYMBOL_REPLACE_ID,
        SYMBOL_SEARCH_ID, SYMBOL_SPECIES_ID, SYMBOL_SPLIT_ID, SYMBOL_TO_PRIMITIVE_ID,
        SYMBOL_TO_STRING_TAG_ID, SYMBOL_UNSCOPABLES_ID,
    },
    jsvalue::value::*,
    vm::{Factory, VMValueResult, VM},
};

pub fn symbol(factory: &mut Factory) -> Value {
    let obj = factory.generate_builtin_constructor(
        "Symbol",
        symbol_constructor,
        factory.object_prototypes.symbol,
    );

    let symbol_for = factory.builtin_function("for", symbol_for);
    let symbol_key_for = factory.builtin_function("keyFor", symbol_key_for);
    set_function_length(symbol_for, 1.0);
    set_function_length(symbol_key_for, 1.0);
    set_constructor_function(obj, "for", symbol_for);
    set_constructor_function(obj, "keyFor", symbol_key_for);

    for (name, id) in [
        ("asyncIterator", SYMBOL_ASYNC_ITERATOR_ID),
        ("hasInstance", SYMBOL_HAS_INSTANCE_ID),
        ("isConcatSpreadable", SYMBOL_IS_CONCAT_SPREADABLE_ID),
        ("iterator", SYMBOL_ITERATOR_ID),
        ("match", SYMBOL_MATCH_ID),
        ("matchAll", SYMBOL_MATCH_ALL_ID),
        ("replace", SYMBOL_REPLACE_ID),
        ("search", SYMBOL_SEARCH_ID),
        ("species", SYMBOL_SPECIES_ID),
        ("split", SYMBOL_SPLIT_ID),
        ("toPrimitive", SYMBOL_TO_PRIMITIVE_ID),
        ("toStringTag", SYMBOL_TO_STRING_TAG_ID),
        ("unscopables", SYMBOL_UNSCOPABLES_ID),
    ] {
        set_constructor_constant(obj, name, factory.well_known_symbol(id));
    }
    obj
}

fn set_constructor_function(constructor: Value, name: &str, value: Value) {
    constructor.get_object_info().insert_property(
        name.to_string(),
        Property::new_data(DataProperty::new(value).set_writable().set_configurable()),
    );
}

fn set_constructor_constant(constructor: Value, name: &str, value: Value) {
    constructor.get_object_info().insert_property(
        name.to_string(),
        Property::new_data(DataProperty::new(value)),
    );
}

pub fn symbol_constructor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let description = match args.get(0) {
        Some(value) if !value.is_undefined() => Some(vm.to_string(*value)?),
        _ => None,
    };
    let symbol = vm.factory.symbol(description);
    Ok(symbol)
}

pub fn symbol_prototype_description(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let symbol = this_symbol_value(vm, this)?;
    match symbol.get_symbol_info().description.clone() {
        Some(description) => Ok(vm.factory.string(description)),
        None => Ok(Value::undefined()),
    }
}

pub fn symbol_prototype_to_string(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let symbol = this_symbol_value(vm, this)?;
    Ok(vm
        .factory
        .string(symbol_descriptive_string(symbol).unwrap()))
}

pub fn symbol_prototype_value_of(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    this_symbol_value(vm, this)
}

pub fn symbol_descriptive_string(symbol: Value) -> Option<String> {
    if !symbol.is_symbol() {
        return None;
    }
    let description = symbol.get_symbol_info().description.clone();
    Some(match description {
        Some(description) => format!("Symbol({})", description),
        None => "Symbol()".to_string(),
    })
}

fn this_symbol_value(vm: &mut VM, this: Value) -> Result<Value, RuntimeError> {
    if this.is_symbol() {
        return Ok(this);
    }
    if this.is_object() {
        let data = this.get_property("__symbol_data");
        if data.is_symbol() {
            return Ok(data);
        }
    }
    Err(vm.current_context.error_type("Symbol.prototype"))
}

pub fn symbol_for(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let key = vm.to_string(args.get(0).copied().unwrap_or(Value::undefined()))?;
    let symbol = vm.global_symbol_registry.for_(&mut vm.factory, key);
    Ok(symbol)
}

pub fn symbol_key_for(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let sym = args.get(0).map(|x| *x).unwrap_or(Value::undefined());

    if !sym.is_symbol() {
        return Err(vm
            .current_context
            .error_type(format!("{} is not symbol", sym.debug_string(true))));
    }

    let key = vm.global_symbol_registry.key_for(&mut vm.factory, sym);
    Ok(key)
}
