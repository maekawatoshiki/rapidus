use crate::vm::{
    jsvalue::value::*,
    vm::{CallMode, VMValueResult, VM},
};

pub type BuiltinFuncTy = fn(&mut VM, &[Value], Value) -> VMValueResult;

pub fn parse_float(_vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let string = args.get(0).unwrap_or(&Value::undefined()).to_string();
    let val = Value::Number(string.parse::<f64>().unwrap_or(::std::f64::NAN));
    Ok(val)
}

pub fn deep_seq(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    if args.len() != 2 {
        return Err(vm
            .current_context
            .error_general("__assert_deep_seq(): Two arguments are needed."));
    };
    let lval = args.get(0).unwrap();
    let rval = args.get(1).unwrap();
    let val = Value::bool(deep_seq_bool(lval, rval));
    Ok(val)
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

pub fn require(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let file_name = {
        let val = args.get(0).ok_or(
            vm.current_context
                .error_general("require():One argument is needed."),
        )?;
        match val {
            Value::String(_) => val.to_string(),
            _ => {
                return Err(vm
                    .current_context
                    .error_type("require():An argument should be string."));
            }
        }
    };

    use crate::parser::Parser;
    let mut parser = Parser::load_module(file_name.clone())
        .map_err(|e| return vm.current_context.error_general(format!("{:?}", e)))?;
    let absolute_path = parser.file_name.clone();

    let node = r#try!(parser.parse_all().map_err(|parse_err| {
        parser.handle_error(&parse_err);
        vm.current_context
            .error_general(format!("Error in parsing module \"{}\"", file_name))
    }));

    let mut iseq = vec![];
    let id = crate::id::get_unique_id();
    use crate::vm::codegen::Error;
    let global_info = r#try!(vm
        .compile(&node, &mut iseq, true, id)
        .map_err(|codegen_err| {
            let Error { msg, token_pos, .. } = codegen_err;
            parser.show_error_at(token_pos, msg);
            vm.current_context
                .error_general(format!("Error in parsing module \"{}\"", file_name))
        }));
    let script_info = parser.into_script_info();
    vm.script_info.push((id, script_info));
    let user_func_info = UserFunctionInfo {
        id,
        module_func_id: id,
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

    vm.prepare_context_for_function_invokation(
        &user_func_info,
        args: &[Value],
        Value::undefined(),
        CallMode::ModuleCall,
        false,
    )?;

    let empty_object = make_normal_object!(vm.factory);
    let id_object = vm.factory.string(absolute_path);
    let module = make_normal_object!(
        vm.factory,
        id       => false, false, false: id_object,
        exports  => true,  false, false: empty_object
    );
    vm.current_context
        .lex_env_mut()
        .set_own_value("module", module)?;

    if vm.is_trace {
        println!("--> call module");
        println!(
            "  module_id:{} func_id:{}",
            vm.current_context.module_func_id, vm.current_context.func_id
        );
    };

    Ok(Value::empty())
}
