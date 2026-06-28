use super::helpers::define_well_known_symbol_property;
use crate::vm::{
    jsvalue::{
        object::{DataProperty, Property},
        symbol::SYMBOL_TO_STRING_TAG_ID,
        value::Value,
    },
    vm::{Factory, VMValueResult, VM},
};
use rand::random;
use rustc_hash::FxHashMap;

pub fn math(factory: &mut Factory) -> Value {
    let mut props = FxHashMap::default();
    insert_const(&mut props, "E", std::f64::consts::E);
    insert_const(&mut props, "LN10", std::f64::consts::LN_10);
    insert_const(&mut props, "LN2", std::f64::consts::LN_2);
    insert_const(&mut props, "LOG10E", std::f64::consts::LOG10_E);
    insert_const(&mut props, "LOG2E", std::f64::consts::LOG2_E);
    insert_const(&mut props, "PI", std::f64::consts::PI);
    insert_const(&mut props, "SQRT1_2", std::f64::consts::FRAC_1_SQRT_2);
    insert_const(&mut props, "SQRT2", std::f64::consts::SQRT_2);

    insert_func(factory, &mut props, "abs", 1.0, math_abs);
    insert_func(factory, &mut props, "acos", 1.0, math_acos);
    insert_func(factory, &mut props, "acosh", 1.0, math_acosh);
    insert_func(factory, &mut props, "asin", 1.0, math_asin);
    insert_func(factory, &mut props, "asinh", 1.0, math_asinh);
    insert_func(factory, &mut props, "atan", 1.0, math_atan);
    insert_func(factory, &mut props, "atan2", 2.0, math_atan2);
    insert_func(factory, &mut props, "atanh", 1.0, math_atanh);
    insert_func(factory, &mut props, "cbrt", 1.0, math_cbrt);
    insert_func(factory, &mut props, "ceil", 1.0, math_ceil);
    insert_func(factory, &mut props, "clz32", 1.0, math_clz32);
    insert_func(factory, &mut props, "cos", 1.0, math_cos);
    insert_func(factory, &mut props, "cosh", 1.0, math_cosh);
    insert_func(factory, &mut props, "exp", 1.0, math_exp);
    insert_func(factory, &mut props, "expm1", 1.0, math_expm1);
    insert_func(factory, &mut props, "floor", 1.0, math_floor);
    insert_func(factory, &mut props, "fround", 1.0, math_fround);
    insert_func(factory, &mut props, "hypot", 2.0, math_hypot);
    insert_func(factory, &mut props, "imul", 2.0, math_imul);
    insert_func(factory, &mut props, "log", 1.0, math_log);
    insert_func(factory, &mut props, "log10", 1.0, math_log10);
    insert_func(factory, &mut props, "log1p", 1.0, math_log1p);
    insert_func(factory, &mut props, "log2", 1.0, math_log2);
    insert_func(factory, &mut props, "max", 2.0, math_max);
    insert_func(factory, &mut props, "min", 2.0, math_min);
    insert_func(factory, &mut props, "pow", 2.0, math_pow);
    insert_func(factory, &mut props, "random", 0.0, math_random);
    insert_func(factory, &mut props, "round", 1.0, math_round);
    insert_func(factory, &mut props, "sign", 1.0, math_sign);
    insert_func(factory, &mut props, "sin", 1.0, math_sin);
    insert_func(factory, &mut props, "sinh", 1.0, math_sinh);
    insert_func(factory, &mut props, "sqrt", 1.0, math_sqrt);
    insert_func(factory, &mut props, "tan", 1.0, math_tan);
    insert_func(factory, &mut props, "tanh", 1.0, math_tanh);
    insert_func(factory, &mut props, "trunc", 1.0, math_trunc);

    let obj = factory.object(props);
    let tag = factory.string("Math".to_string());
    define_well_known_symbol_property(
        factory,
        obj,
        SYMBOL_TO_STRING_TAG_ID,
        Property::new_data(DataProperty::new(tag).set_configurable()),
    );
    obj
}

fn insert_const(props: &mut FxHashMap<String, Property>, name: &str, value: f64) {
    props.insert(
        name.to_string(),
        Property::new_data(DataProperty::new(Value::Number(value))),
    );
}

fn insert_func(
    factory: &mut Factory,
    props: &mut FxHashMap<String, Property>,
    name: &str,
    len: f64,
    func: crate::builtins::BuiltinFuncTy,
) {
    let value = factory.builtin_function(name, func);
    value.get_object_info().property.insert(
        "length".to_string(),
        Property::new_data(DataProperty::new(Value::Number(len)).set_configurable()),
    );
    props.insert(
        name.to_string(),
        Property::new_data(DataProperty::new(value).set_writable().set_configurable()),
    );
}

fn arg_number(vm: &mut VM, args: &[Value], index: usize) -> f64 {
    args.get(index)
        .unwrap_or(&Value::undefined())
        .to_number(&mut vm.factory.memory_allocator)
}

fn unary(vm: &mut VM, args: &[Value], op: impl FnOnce(f64) -> f64) -> VMValueResult {
    Ok(Value::Number(op(arg_number(vm, args, 0))))
}

pub fn math_abs(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::abs)
}

pub fn math_acos(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::acos)
}

pub fn math_acosh(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::acosh)
}

pub fn math_asin(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::asin)
}

pub fn math_asinh(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::asinh)
}

pub fn math_atan(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::atan)
}

pub fn math_atan2(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    Ok(Value::Number(
        arg_number(vm, args, 0).atan2(arg_number(vm, args, 1)),
    ))
}

pub fn math_atanh(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::atanh)
}

pub fn math_cbrt(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::cbrt)
}

pub fn math_ceil(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::ceil)
}

pub fn math_clz32(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let value = args
        .get(0)
        .unwrap_or(&Value::undefined())
        .to_uint32(&mut vm.factory.memory_allocator);
    Ok(Value::Number(value.leading_zeros() as f64))
}

pub fn math_cos(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::cos)
}

pub fn math_cosh(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::cosh)
}

pub fn math_exp(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::exp)
}

pub fn math_expm1(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::exp_m1)
}

pub fn math_floor(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::floor)
}

pub fn math_fround(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    Ok(Value::Number(arg_number(vm, args, 0) as f32 as f64))
}

pub fn math_hypot(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let mut sum = 0.0;
    for index in 0..args.len() {
        let value = arg_number(vm, args, index);
        if value.is_infinite() {
            return Ok(Value::Number(f64::INFINITY));
        }
        sum += value * value;
    }
    Ok(Value::Number(sum.sqrt()))
}

pub fn math_imul(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let a = args
        .get(0)
        .unwrap_or(&Value::undefined())
        .to_int32(&mut vm.factory.memory_allocator);
    let b = args
        .get(1)
        .unwrap_or(&Value::undefined())
        .to_int32(&mut vm.factory.memory_allocator);
    Ok(Value::Number(a.wrapping_mul(b) as f64))
}

pub fn math_log(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::ln)
}

pub fn math_log10(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::log10)
}

pub fn math_log1p(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::ln_1p)
}

pub fn math_log2(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::log2)
}

pub fn math_max(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let mut result = f64::NEG_INFINITY;
    for index in 0..args.len() {
        let value = arg_number(vm, args, index);
        if value.is_nan() {
            return Ok(Value::Number(f64::NAN));
        }
        if value > result || (value == 0.0 && result == 0.0 && !value.is_sign_negative()) {
            result = value;
        }
    }
    Ok(Value::Number(result))
}

pub fn math_min(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let mut result = f64::INFINITY;
    for index in 0..args.len() {
        let value = arg_number(vm, args, index);
        if value.is_nan() {
            return Ok(Value::Number(f64::NAN));
        }
        if value < result || (value == 0.0 && result == 0.0 && value.is_sign_negative()) {
            result = value;
        }
    }
    Ok(Value::Number(result))
}

pub fn math_pow(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    Ok(Value::Number(
        arg_number(vm, args, 0).powf(arg_number(vm, args, 1)),
    ))
}

pub fn math_random(_vm: &mut VM, _args: &[Value], _this: Value) -> VMValueResult {
    Ok(Value::Number(random::<f64>()))
}

pub fn math_round(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let value = arg_number(vm, args, 0);
    if value.is_nan() || value.is_infinite() || value == 0.0 {
        return Ok(Value::Number(value));
    }
    Ok(Value::Number((value + 0.5).floor()))
}

pub fn math_sign(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    let value = arg_number(vm, args, 0);
    Ok(Value::Number(if value.is_nan() || value == 0.0 {
        value
    } else if value < 0.0 {
        -1.0
    } else {
        1.0
    }))
}

pub fn math_sin(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::sin)
}

pub fn math_sinh(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::sinh)
}

pub fn math_sqrt(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::sqrt)
}

pub fn math_tan(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::tan)
}

pub fn math_tanh(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::tanh)
}

pub fn math_trunc(vm: &mut VM, args: &[Value], _this: Value) -> VMValueResult {
    unary(vm, args, f64::trunc)
}
