use rand::random;
use vm::{
    callobj::CallObject, value::{Value, ValueBase}, vm::VM,
};

macro_rules! simple_math {
    ($name:ident, $f:ident) => {
        #[allow(unused_variables)]
        pub fn $name(vm: &mut VM, args: &Vec<Value>, callobj: &CallObject) {
            if let ValueBase::Number(n) = args[0].val {
                return vm.state.stack.push(Value::number(n.$f()));
            }
            vm.state.stack.push(Value::undefined())
        }
    };
}

simple_math!(math_floor, floor);
simple_math!(math_abs, abs);
simple_math!(math_acos, acos);
simple_math!(math_acosh, acosh);
simple_math!(math_asin, asin);
simple_math!(math_asinh, asinh);
simple_math!(math_atan, atan);
simple_math!(math_atanh, atanh);

pub fn math_atan2(vm: &mut VM, args: &Vec<Value>, _: &CallObject) {
    if let ValueBase::Number(n1) = args[0].val {
        if let ValueBase::Number(n2) = args[1].val {
            return vm.state.stack.push(Value::number(n1.atan2(n2)));
        }
        vm.state.stack.push(Value::undefined())
    }
}
simple_math!(math_cbrt, cbrt);
simple_math!(math_ceil, ceil);

pub fn math_clz32(vm: &mut VM, args: &Vec<Value>, _: &CallObject) {
    if let ValueBase::Number(n) = args[0].val {
        return vm.state.stack.push(Value::number(if n == 0.0 {
            32.0
        } else {
            // TODO: >> ? >>> ?
            31.0 - ((n as i32 >> 0) as f64 * ::std::f64::consts::LOG2_E)
                .log(::std::f64::consts::E)
                .floor()
        }));
    }
    vm.state.stack.push(Value::undefined())
}
simple_math!(math_cos, cos);
simple_math!(math_cosh, cosh);
simple_math!(math_exp, exp);
simple_math!(math_expm1, exp_m1);
simple_math!(math_fround, round);

pub fn math_hypot(vm: &mut VM, args: &Vec<Value>, _: &CallObject) {
    let mut sum2 = 0.0;
    for n in args {
        if let ValueBase::Number(n) = n.val {
            sum2 += n * n;
        }
    }
    vm.state.stack.push(Value::number(sum2.sqrt()));
}

pub fn math_log(vm: &mut VM, args: &Vec<Value>, _: &CallObject) {
    if let ValueBase::Number(n1) = args[0].val {
        return vm
            .state
            .stack
            .push(Value::number(n1.log(::std::f64::consts::E)));
    }
    vm.state.stack.push(Value::undefined())
}

pub fn math_log1p(vm: &mut VM, args: &Vec<Value>, _: &CallObject) {
    if let ValueBase::Number(n1) = args[0].val {
        return vm
            .state
            .stack
            .push(Value::number(n1.log(1.0 + ::std::f64::consts::E)));
    }
    vm.state.stack.push(Value::undefined())
}

simple_math!(math_log10, log10);
simple_math!(math_log2, log2);

pub fn math_max(vm: &mut VM, args: &Vec<Value>, _: &CallObject) {
    let mut max = if let ValueBase::Number(n) = args[0].val {
        n
    } else {
        0.0
    };
    for n in args[1..].iter() {
        if let ValueBase::Number(n) = n.val {
            if n > max {
                max = n;
            }
        }
    }
    vm.state.stack.push(Value::number(max));
}

pub fn math_min(vm: &mut VM, args: &Vec<Value>, _: &CallObject) {
    let mut min = if let ValueBase::Number(n) = args[0].val {
        n
    } else {
        0.0
    };
    for n in args[1..].iter() {
        if let ValueBase::Number(n) = n.val {
            if n < min {
                min = n;
            }
        }
    }
    vm.state.stack.push(Value::number(min));
}

simple_math!(math_round, round);

pub fn math_sign(vm: &mut VM, args: &Vec<Value>, _: &CallObject) {
    if let ValueBase::Number(n) = args[0].val {
        return vm.state.stack.push(Value::number(if n == 0.0 {
            n
        } else if n > 0.0 {
            1.0
        } else {
            -1.0
        }));
    }
    vm.state.stack.push(Value::undefined())
}

simple_math!(math_sin, sin);
simple_math!(math_sinh, sinh);
simple_math!(math_sqrt, sqrt);
simple_math!(math_tan, tan);
simple_math!(math_tanh, tanh);
simple_math!(math_trunc, trunc);

pub fn math_random(vm: &mut VM, _: &Vec<Value>, _: &CallObject) {
    vm.state.stack.push(Value::number(random::<f64>()))
}

pub fn math_pow(vm: &mut VM, args: &Vec<Value>, _: &CallObject) {
    if let ValueBase::Number(f1) = args[0].val {
        if let ValueBase::Number(f2) = args[1].val {
            return vm.state.stack.push(Value::number(f1.powf(f2)));
        }
    }
    vm.state.stack.push(Value::undefined())
}

// *** Functions for JIT ***

#[no_mangle]
pub extern "C" fn jit_math_floor(n: f64) -> f64 {
    n.floor()
}

// TODO: Find a better way for rand gen. (rand::random is slow)
pub static mut MATH_RAND_SEED: u64 = 0xf6d582196d588cac;
#[no_mangle]
pub extern "C" fn jit_math_random() -> f64 {
    unsafe {
        MATH_RAND_SEED = MATH_RAND_SEED ^ (MATH_RAND_SEED << 13);
        MATH_RAND_SEED = MATH_RAND_SEED ^ (MATH_RAND_SEED >> 17);
        MATH_RAND_SEED = MATH_RAND_SEED ^ (MATH_RAND_SEED << 5);
        (MATH_RAND_SEED as f64) / ::std::u64::MAX as f64
    }
}

#[no_mangle]
pub extern "C" fn jit_math_pow(x: f64, y: f64) -> f64 {
    x.powf(y)
}
