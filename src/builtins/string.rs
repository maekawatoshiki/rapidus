use vm::{error::RuntimeError, frame::Frame, jsvalue::value::*, vm::VM2};

pub fn string_prototype_split(
    vm: &mut VM2,
    args: &[Value2],
    cur_frame: &Frame,
) -> Result<(), RuntimeError> {
    let string = cur_frame.this.into_str();
    let separator_ = args.get(0).map(|x| *x).unwrap_or(Value2::undefined());
    if separator_.is_undefined() {
        let ary = Value2::array(
            &mut vm.memory_allocator,
            &vm.object_prototypes,
            vec![Property2::new_data_simple(cur_frame.this)],
        );
        vm.stack.push(ary.into());
        return Ok(());
    }
    let separator = separator_.to_string();
    let elems = string
        .split(separator.as_str())
        .collect::<Vec<&str>>()
        .iter()
        .map(|s| {
            Property2::new_data_simple(Value2::string(&mut vm.memory_allocator, s.to_string()))
        })
        .collect::<Vec<Property2>>();
    let ary = Value2::array(&mut vm.memory_allocator, &vm.object_prototypes, elems);
    vm.stack.push(ary.into());
    Ok(())
}

pub fn string_prototype_index_of(
    vm: &mut VM2,
    args: &[Value2],
    cur_frame: &Frame,
) -> Result<(), RuntimeError> {
    let string = cur_frame.this.into_str();
    let search_string = args.get(0).unwrap_or(&Value2::undefined()).to_string();
    let position = args.get(1).unwrap_or(&Value2::Number(0.0)).into_number() as usize;
    let found_pos = string[position..]
        .find(search_string.as_str())
        .map_or(-1.0, |p| p as f64);
    vm.stack.push(Value2::Number(found_pos).into());
    Ok(())
}
