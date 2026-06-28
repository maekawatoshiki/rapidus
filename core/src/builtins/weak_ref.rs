use crate::vm::{
    jsvalue::{
        object::{DataProperty, Object, ObjectKind, Property, WeakRefObjectInfo},
        symbol::SYMBOL_TO_STRING_TAG_ID,
        value::Value,
    },
    vm::{Factory, VMValueResult, VM},
};
use rustc_hash::FxHashMap;

pub fn weak_ref(factory: &mut Factory) -> Value {
    let prototype = weak_ref_prototype(factory);
    let constructor =
        factory.generate_builtin_constructor("WeakRef", weak_ref_constructor, prototype);
    constructor.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(1.0)).set_configurable()),
    );
    constructor
}

fn weak_ref_prototype(factory: &mut Factory) -> Value {
    let deref = factory.builtin_function("deref", weak_ref_prototype_deref);
    deref.get_object_info().insert_property(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(0.0)).set_configurable()),
    );
    let tag = factory.string("WeakRef");
    let prototype = Value::Object(factory.alloc(Object {
        kind: ObjectKind::Ordinary,
        prototype: factory.object_prototypes.object,
        property: make_property_map!(deref => true, false, true: deref),
        property_order: make_property_order!(deref => true, false, true: deref),
        private_elements: rustc_hash::FxHashMap::default(),
        sym_property: FxHashMap::default(),
        sym_property_order: Vec::new(),
        extensible: true,
    }));
    super::helpers::define_well_known_symbol_property(
        factory,
        prototype,
        SYMBOL_TO_STRING_TAG_ID,
        Property::new_data(DataProperty::new(tag).set_configurable()),
    );
    prototype
}

pub fn weak_ref_constructor(vm: &mut VM, args: &[Value], this: Value) -> VMValueResult {
    if !vm.builtin_constructor_call {
        return Err(vm.current_context.error_type("WeakRef constructor"));
    }
    let target = args.get(0).copied().unwrap_or(Value::undefined());
    if !can_be_held_weakly(target) {
        return Err(vm.current_context.error_type("WeakRef target"));
    }
    this.get_object_info().kind = ObjectKind::WeakRef(WeakRefObjectInfo { target });
    Ok(this)
}

pub fn weak_ref_prototype_deref(vm: &mut VM, _args: &[Value], this: Value) -> VMValueResult {
    if let Value::Object(info) = this {
        if let ObjectKind::WeakRef(ref weak_ref) = crate::vm::jsvalue::object::ObjectRef(info).kind
        {
            return Ok(weak_ref.target);
        }
    }
    Err(vm.current_context.error_type("WeakRef.prototype.deref"))
}

fn can_be_held_weakly(value: Value) -> bool {
    if !value.is_object() {
        return false;
    }
    let obj = value.get_object_info();
    match obj.kind {
        ObjectKind::Symbol(ref info) => !info.registered,
        _ => true,
    }
}
