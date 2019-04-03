use vm::{error::RuntimeError, frame::Frame, jsvalue::value::*, vm::VM2};

pub fn console_log(vm: &mut VM2, args: &[Value2], _cur_frame: &Frame) -> Result<(), RuntimeError> {
    let args_len = args.len();

    for i in 0..args_len {
        debug_print(&args[i], false);
        if args_len - 1 != i {
            print!(" ");
        }
    }
    println!();

    vm.stack.push(Value2::undefined().into());

    Ok(())
}

pub fn debug_print(val: &Value2, nest: bool) {
    fn show_obj(sorted_key_val: Vec<(&String, &Property2)>) {
        for (i, tupple) in sorted_key_val.iter().enumerate() {
            print!("'{}': ", tupple.0.as_str());

            match tupple.1 {
                Property2::Data(DataProperty { val, .. }) => {
                    debug_print(&val, true);
                }
                Property2::Accessor(AccessorProperty { get, set, .. }) => {
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
        Value2::Other(UNINITIALIZED) => print!("uninitialized"),
        Value2::Other(EMPTY) => print!("empty"),
        Value2::Other(NULL) => print!("null"),
        Value2::Other(UNDEFINED) => print!("undefined"),
        Value2::Other(_) => unreachable!(),
        Value2::Bool(1) => print!("true"),
        Value2::Bool(0) => print!("false"),
        Value2::Bool(_) => unreachable!(),
        Value2::Number(n) if n.is_nan() => print!("NaN"),
        Value2::Number(n) if n.is_infinite() => print!("Infinity"),
        Value2::Number(n) => print!("{}", *n),
        Value2::String(ref s) => {
            let s = unsafe { &**s }.to_str().unwrap();
            if nest {
                print!("'{}'", s)
            } else {
                print!("{}", s)
            }
        }
        Value2::Object(obj_info) => {
            let obj_info = unsafe { &**obj_info };

            match obj_info.kind {
                ObjectKind2::Ordinary => {
                    print!("{{ ");

                    let mut sorted_key_val = (&obj_info.property)
                        .iter()
                        .collect::<Vec<(&String, &Property2)>>();
                    sorted_key_val.sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));

                    show_obj(sorted_key_val);

                    print!("}}");
                }
                ObjectKind2::Symbol(ref info) => print!(
                    "Symbol({})",
                    info.description.as_ref().unwrap_or(&"".to_string())
                ),
                ObjectKind2::Function(ref func_info) => {
                    if let Some(ref name) = func_info.name {
                        print!("[Function: {}]", name);
                    } else {
                        print!("[Function]");
                    }
                }
                ObjectKind2::Array(ref ary_info) => {
                    print!("[ ");

                    let mut sorted_key_val = (&obj_info.property)
                        .iter()
                        .collect::<Vec<(&String, &Property2)>>();
                    sorted_key_val.sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));

                    let length = ary_info.elems.len();
                    let is_last_idx = |idx: usize| -> bool { idx == length - 1 };
                    let mut i = 0;
                    while i < length {
                        let mut empty_elems = 0;
                        while i < length && Value2::empty() == ary_info.elems[i].as_data().val {
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
