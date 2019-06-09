use crate::vm::{
    exec_context::ExecContext,
    jsvalue::value::{Property, Value},
    vm::VMValueResult,
    vm::VM,
};

pub fn string_prototype_split(
    vm: &mut VM,
    args: &[Value],
    this: Value,
    _cur_frame: &mut ExecContext,
) -> VMValueResult {
    let string = this.into_str();
    let separator_ = args.get(0).map(|x| *x).unwrap_or(Value::undefined());
    if separator_.is_undefined() {
        let ary = vm.factory.array(vec![Property::new_data_simple(this)]);
        return Ok(ary);
    }
    let separator = separator_.to_string();
    let elems = string
        .split(separator.as_str())
        .collect::<Vec<&str>>()
        .iter()
        .map(|s| Property::new_data_simple(vm.factory.string(s.to_string())))
        .collect::<Vec<Property>>();
    let ary = vm.factory.array(elems);
    Ok(ary)
}

pub fn string_prototype_index_of(
    _vm: &mut VM,
    args: &[Value],
    this: Value,
    _cur_frame: &mut ExecContext,
) -> VMValueResult {
    let string = this.into_str();
    let search_string = args.get(0).unwrap_or(&Value::undefined()).to_string();
    let position = args.get(1).unwrap_or(&Value::Number(0.0)).into_number() as usize;
    let found_pos = string[position..]
        .find(search_string.as_str())
        .map_or(-1.0, |p| p as f64);
    let val = Value::Number(found_pos);
    Ok(val)
}
