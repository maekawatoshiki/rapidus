use crate::vm::{
    jsvalue::{
        object::{DataProperty, Property},
        value::*,
    },
    vm::{Factory, VMValueResult, VM},
};
use rustc_hash::FxHashMap;

pub fn boolean(factory: &mut Factory) -> Value {
    let prototype = factory.object(FxHashMap::default());
    prototype.get_object_info().property.insert(
        "__boolean_data".to_string(),
        Property::new_data(DataProperty::new(Value::bool(false))),
    );
    let to_string = factory.builtin_function("toString", boolean_prototype_to_string);
    to_string.get_object_info().property.insert(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
    );
    let value_of = factory.builtin_function("valueOf", boolean_prototype_value_of);
    value_of.get_object_info().property.insert(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
    );
    prototype.get_object_info().property.insert(
        "toString".to_string(),
        Property::new_data(
            DataProperty::new(to_string)
                .set_writable()
                .set_configurable(),
        ),
    );
    prototype.get_object_info().property.insert(
        "valueOf".to_string(),
        Property::new_data(
            DataProperty::new(value_of)
                .set_writable()
                .set_configurable(),
        ),
    );
    factory.object_prototypes.boolean = prototype;
    factory.generate_builtin_constructor("Boolean", boolean_constructor, prototype)
}

pub fn boolean_constructor(_vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    let value = Value::bool(args.get(0).unwrap_or(&Value::undefined()).to_boolean());
    if this.is_object() {
        this.get_object_info().property.insert(
            "__boolean_data".to_string(),
            Property::new_data(DataProperty::new(value)),
        );
    }
    Ok(value)
}

pub fn boolean_prototype_to_string(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    let value = this_boolean_value(vm, this)?;
    Ok(vm.factory.string(if value {
        "true".to_string()
    } else {
        "false".to_string()
    }))
}

pub fn boolean_prototype_value_of(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    Ok(Value::bool(this_boolean_value(vm, this)?))
}

fn this_boolean_value(vm: &mut VM, this: Value) -> Result<bool, crate::vm::error::RuntimeError> {
    if matches!(this, Value::Bool(_)) {
        return Ok(this.into_bool());
    }
    if this.is_object() {
        let value = this.get_property("__boolean_data");
        if matches!(value, Value::Bool(_)) {
            return Ok(value.into_bool());
        }
    }
    Err(vm.current_context.error_type("Boolean.prototype"))
}
