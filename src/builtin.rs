use crate::vm::{error::RuntimeError, frame::Frame, jsvalue::value::*, vm::VM};

pub type BuiltinFuncTy2 = fn(&mut VM, &[Value], &Frame) -> Result<(), RuntimeError>;

pub fn parse_float(vm: &mut VM, args: &[Value], _cur_frame: &Frame) -> Result<(), RuntimeError> {
    let string = args.get(0).unwrap_or(&Value::undefined()).to_string();
    vm.stack
        .push(Value::Number(string.parse::<f64>().unwrap_or(::std::f64::NAN)).into());
    Ok(())
}

pub fn deep_seq(vm: &mut VM, args: &[Value], _cur_frame: &Frame) -> Result<(), RuntimeError> {
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

pub fn require(vm: &mut VM, args: &[Value], _cur_frame: &Frame) -> Result<(), RuntimeError> {
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
    let script = r#try!(std::fs::read_to_string(file_name.clone())
        .map_err(|ioerr| RuntimeError::General(format!("require(): \"{}\" {}", file_name, ioerr))));

    use crate::parser::Parser;
    let mut parser = Parser::new(script.clone());
    let node = r#try!(parser.parse_all().map_err(|parse_err| {
        parser.handle_error(&parse_err);
        RuntimeError::General(format!("Error in parsing module \"{}\"", file_name))
    }));

    let mut iseq = vec![];
    use crate::vm::codegen::Error;
    let _global_info = r#try!(vm.compile(&node, &mut iseq, true).map_err(|codegen_err| {
        let Error { msg, token_pos, .. } = codegen_err;
        parser.show_error_at(token_pos, msg.as_str());
        RuntimeError::General(format!("Error in parsing module \"{}\"", file_name))
    }));
    vm.stack
        .push(Value::string(&mut vm.factory.memory_allocator, script).into());
    Ok(())
}
