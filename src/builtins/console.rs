use crate::vm::{
    exec_context::ExecContext,
    jsvalue::value::{
        cstrp_to_str, AccessorProperty, DataProperty, ObjectKind, ObjectRef, Property, Value,
        EMPTY, NULL, UNDEFINED, UNINITIALIZED,
    },
    vm::VMValueResult,
    vm::VM,
};

pub fn console_log(
    _vm: &mut VM,
    args: &[Value],
    _this: Value,
    _cur_frame: &mut ExecContext,
) -> VMValueResult {
    let args_len = args.len();

    for i in 0..args_len {
        debug_print(&args[i], false);
        if args_len - 1 != i {
            print!(" ");
        }
    }
    println!();

    Ok(Value::undefined())
}

pub fn debug_print(val: &Value, nest: bool) {
    fn show_obj(sorted_key_val: Vec<(&String, &Property)>) {
        for (i, tupple) in sorted_key_val.iter().enumerate() {
            print!("'{}': ", tupple.0.as_str());

            match tupple.1 {
                Property::Data(DataProperty { val, .. }) => {
                    debug_print(&val, true);
                }
                Property::Accessor(AccessorProperty { get, set, .. }) => {
                    let s_get = if get.is_undefined() { "" } else { "Getter" };
                    let s_set = if set.is_undefined() { "" } else { "Setter" };
                    print!(
                        "[{}{}{}]",
                        s_get,
                        if !get.is_undefined() && !set.is_undefined() {
                            "/"
                        } else {
                            ""
                        },
                        s_set
                    );
                }
            }

            print!(
                "{}",
                if i != sorted_key_val.len() - 1 {
                    ", "
                } else {
                    " "
                }
            );
        }
    }

    match val {
        Value::Other(UNINITIALIZED) => print!("uninitialized"),
        Value::Other(EMPTY) => print!("empty"),
        Value::Other(NULL) => print!("null"),
        Value::Other(UNDEFINED) => print!("undefined"),
        Value::Other(_) => unreachable!(),
        Value::Bool(1) => print!("true"),
        Value::Bool(0) => print!("false"),
        Value::Bool(_) => unreachable!(),
        Value::Number(n) if n.is_nan() => print!("NaN"),
        Value::Number(n) if n.is_infinite() => print!("Infinity"),
        Value::Number(n) => print!("{}", *n),
        Value::String(ref s) => {
            let s = cstrp_to_str(*s);
            if nest {
                print!("'{}'", s)
            } else {
                print!("{}", s)
            }
        }
        Value::Object(obj_info) => {
            let obj_info = ObjectRef(*obj_info);

            match obj_info.kind {
                ObjectKind::Ordinary => {
                    print!("{{ ");

                    let mut sorted_key_val = (&obj_info.property)
                        .iter()
                        .collect::<Vec<(&String, &Property)>>();
                    sorted_key_val.sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));

                    show_obj(sorted_key_val);

                    print!("}}");
                }
                ObjectKind::Symbol(ref info) => print!(
                    "Symbol({})",
                    info.description.as_ref().unwrap_or(&"".to_string())
                ),
                ObjectKind::Error(ref _info) => print!(
                    "Error({})",
                    obj_info.get_property_by_str_key("message").to_string()
                ),
                ObjectKind::Function(ref func_info) => {
                    if let Some(ref name) = func_info.name {
                        print!("[Function: {}]", name);
                    } else {
                        print!("[Function]");
                    }
                }
                ObjectKind::Array(ref ary_info) => {
                    print!("[ ");

                    let mut sorted_key_val = (&obj_info.property)
                        .iter()
                        .collect::<Vec<(&String, &Property)>>();
                    sorted_key_val.sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));

                    let length = ary_info.elems.len();
                    let is_last_idx = |idx: usize| -> bool { idx == length - 1 };
                    let mut i = 0;
                    while i < length {
                        let mut empty_elems = 0;
                        while i < length && Value::empty() == ary_info.elems[i].as_data().val {
                            empty_elems += 1;
                            i += 1;
                        }

                        if empty_elems > 0 {
                            print!(
                                "<{} empty item{}>{}",
                                empty_elems,
                                if empty_elems >= 2 { "s" } else { "" },
                                if is_last_idx(i - 1) && sorted_key_val.len() == 0 {
                                    " "
                                } else {
                                    ", "
                                }
                            );

                            if is_last_idx(i - 1) {
                                break;
                            }
                        }

                        debug_print(&ary_info.elems[i].as_data().val, true);

                        if is_last_idx(i) && sorted_key_val.len() == 0 {
                            print!(" ")
                        } else {
                            print!(", ")
                        }

                        i += 1;
                    }

                    show_obj(sorted_key_val);

                    print!("]");
                }
            }
        } // Value::Object(_, ObjectKind::Date(box time_val)) => {
          //     // TODO: Date needs toString() ?
          //     libc::printf(
          //         "%s\0".as_ptr() as RawStringPtr,
          //         CString::new(time_val.to_rfc3339()).unwrap().as_ptr(),
          //     );
          // }
    }
}
