use crate::vm::{
    error::RuntimeError,
    frame::Frame,
    jsvalue::value::*,
    vm::{VMResult, VM},
};

pub type BuiltinFuncTy = fn(&mut VM, &[Value], Value, &mut Frame) -> VMResult;

pub fn parse_float(vm: &mut VM, args: &[Value], _this: Value, _cur_frame: &mut Frame) -> VMResult {
    let string = args.get(0).unwrap_or(&Value::undefined()).to_string();
    vm.stack
        .push(Value::Number(string.parse::<f64>().unwrap_or(::std::f64::NAN)).into());
    Ok(())
}

pub fn deep_seq(vm: &mut VM, args: &[Value], _this: Value, _cur_frame: &mut Frame) -> VMResult {
    if args.len() != 2 {
        return Err(RuntimeError::General(
            "__assert_deep_seq():Two arguments are needed.".to_string(),
        ));
    };
    let lval = args.get(0).unwrap();
    let rval = args.get(1).unwrap();
    vm.stack.push(Value::bool(deep_seq_bool(lval, rval)).into());
    Ok(())
}

/// Check deep strict equality.
/// Currently, only Object and Array are supported.
/// Accesor property is not suppoeed. (alway return false)
fn deep_seq_bool(lval: &Value, rval: &Value) -> bool {
    match (*lval, *rval) {
        (Value::Object(l_info), Value::Object(r_info)) => {
            let lobj_info = ObjectRef(l_info);
            let robj_info = ObjectRef(r_info);
            // sort and compare properties
            let mut l_sorted_propmap = (&lobj_info.property)
                .iter()
                .collect::<Vec<(&String, &Property)>>();
            l_sorted_propmap.sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));
            let mut r_sorted_propmap = (&robj_info.property)
                .iter()
                .collect::<Vec<(&String, &Property)>>();
            r_sorted_propmap.sort_by(|(key1, _), (key2, _)| key1.as_str().cmp(key2.as_str()));
            if l_sorted_propmap.len() != r_sorted_propmap.len() {
                return false;
            }
            for i in 0..l_sorted_propmap.len() {
                // compare keys
                if l_sorted_propmap[i].0 != r_sorted_propmap[i].0 {
                    return false;
                }
                // compare values
                match (l_sorted_propmap[i].1, r_sorted_propmap[i].1) {
                    (Property::Data(lprop), Property::Data(rprop)) => {
                        if !deep_seq_bool(&lprop.val, &rprop.val) {
                            return false;
                        }
                    }
                    (_, _) => return false,
                }
            }
            match (&lobj_info.kind, &robj_info.kind) {
                (ObjectKind::Ordinary, ObjectKind::Ordinary) => true,
                (ObjectKind::Array(l_info), ObjectKind::Array(r_info)) => {
                    let l_elems = &*l_info.elems;
                    let r_elems = &*r_info.elems;
                    if l_elems.len() != r_elems.len() {
                        return false;
                    };
                    for i in 0..l_elems.len() {
                        let (lval, rval) = match (l_elems[i], r_elems[i]) {
                            (Property::Data(lprop), Property::Data(rprop)) => {
                                (lprop.val, rprop.val)
                            }
                            (_, _) => return false,
                        };
                        if !deep_seq_bool(&lval, &rval) {
                            return false;
                        }
                    }
                    true
                }
                (_, _) => false,
            }
        }
        (_, _) => lval.strict_eq_bool(*rval),
    }
}

pub fn require(vm: &mut VM, args: &[Value], _this: Value, cur_frame: &mut Frame) -> VMResult {
    use std::path::Path;
    let file_name = {
        let val = args.get(0).ok_or(RuntimeError::General(
            "require():One argument is needed.".to_string(),
        ))?;
        match val {
            Value::String(_) => val.to_string(),
            _ => {
                return Err(RuntimeError::Type(
                    "require():An argument should be string.".to_string(),
                ));
            }
        }
    };
    let path = Path::new(&file_name);
    let absolute_path = r#try!(path
        .canonicalize()
        .map_err(|ioerr| RuntimeError::General(format!("require(): \"{}\" {}", file_name, ioerr))));
    let absolute_path = absolute_path.to_str().unwrap_or("<failed to convert>");
    let script = r#try!(std::fs::read_to_string(absolute_path)
        .map_err(|ioerr| RuntimeError::General(format!("require(): \"{}\" {}", file_name, ioerr))));

    use crate::parser::Parser;
    let mut parser = Parser::new(script);
    let node = r#try!(parser.parse_all().map_err(|parse_err| {
        parser.handle_error(&parse_err);
        RuntimeError::General(format!("Error in parsing module \"{}\"", file_name))
    }));

    let mut iseq = vec![];
    let id = crate::id::get_unique_id();
    use crate::vm::codegen::Error;
    let global_info = r#try!(vm
        .compile(&node, &mut iseq, true, id)
        .map_err(|codegen_err| {
            let Error { msg, token_pos, .. } = codegen_err;
            parser.show_error_at(token_pos, msg);
            RuntimeError::General(format!("Error in parsing module \"{}\"", file_name))
        }));

    let user_func_info = UserFunctionInfo {
        id,
        params: vec![],
        var_names: global_info.var_names,
        lex_names: global_info.lex_names,
        func_decls: global_info.func_decls,
        constructible: false,
        this_mode: ThisMode::Global,
        code: iseq,
        exception_table: global_info.exception_table,
        outer: Some(vm.global_environment),
    };

    let mut frame = vm
        .prepare_frame_for_function_invokation(
            &user_func_info,
            args: &[Value],
            Value::undefined(),
            cur_frame,
        )?
        .module_call(true);

    let empty_object = make_normal_object!(vm.factory);
    let id_object = vm.factory.string(absolute_path);
    let module = make_normal_object!(
        vm.factory,
        id       => false, false, false: id_object,
        exports  => true,  false, false: empty_object
    );
    frame.lex_env_mut().set_own_value("module", module)?;

    *cur_frame = frame;
    if vm.is_trace {
        println!("--> call module")
    };
    //vm.stack.push(vm.factory.string("module").into());

    Ok(())
}
