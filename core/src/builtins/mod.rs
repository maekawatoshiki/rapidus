pub mod array;
pub mod console;
pub mod date;
pub mod error;
pub mod function;
pub mod math;
pub mod number;
pub mod object;
pub mod string;
pub mod symbol;

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

/// https://tc39.es/ecma262/#sec-parseint-string-radix
pub fn parse_int(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let input_string = args.get(0).unwrap_or(&Value::undefined()).to_string();
    let mut s = input_string.trim_start();
    let mut strip_prefix = true;
    let mut radix = args
        .get(1)
        .unwrap_or(&Value::undefined())
        .to_int32(&mut vm.factory.memory_allocator) as u32;
    if radix != 0 {
        if radix < 2 || radix > 36 {
            return Ok(Value::Number(f64::NAN));
        }
        if radix != 16 {
            strip_prefix = false;
        }
    } else {
        radix = 10
    };
    if strip_prefix {
        if s.starts_with("0x") || s.starts_with("0X") {
            s = &s[2..];
            radix = 16;
        }
    }
    let z = s.split(|c: char| !c.is_digit(radix)).next().unwrap_or("");
    if z.is_empty() {
        return Ok(Value::Number(f64::NAN));
    }
    let val = Value::Number(i64::from_str_radix(z, radix).expect("unreachable") as f64);
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
                .error_general("require(): One argument is needed."),
        )?;
        match val {
            Value::String(_) => val.to_string(),
            _ => {
                return Err(vm
                    .current_context
                    .error_type("require(): An argument should be string."));
            }
        }
    };

    use rapidus_parser::Parser;
    let mut parser = Parser::load_module(file_name.clone())
        .map_err(|e| return vm.current_context.error_general(format!("{:?}", e)))?;
    let absolute_path = parser.file_name.clone();

    let node = parser.parse_all().map_err(|parse_err| {
        parser.handle_error(&parse_err);
        vm.current_context
            .error_general(format!("Error in parsing module \"{}\"", file_name))
    })?;

    use crate::vm::codegen::Error;
    let module_info = vm.compile(&node, true).map_err(|codegen_err| {
        let Error { msg, loc, .. } = codegen_err;
        parser.show_error_at(loc, msg);
        vm.current_context
            .error_general(format!("Error in parsing module \"{}\"", file_name))
    })?;
    let id = module_info.module_func_id;
    let script_info = parser.into_script_info();
    vm.script_info.insert(id, script_info);

    vm.prepare_context_for_function_invokation(
        Value::undefined(), // TODO: wrong?
        module_info,
        Some(vm.global_environment),
        args: &[Value],
        Value::undefined(),
        CallMode::Module,
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
            "  module_id:{:?} func_id:{:?}",
            vm.current_context.func_ref.module_func_id, vm.current_context.func_ref.func_id
        );
    };

    Ok(Value::empty())
}
