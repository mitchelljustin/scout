use std::io::{stdin, stdout, BufRead, Write};

use anyhow::bail;

use crate::interpreter::{Environment, Value, Value::Nil};

pub macro replace_expr($_t:tt $sub:expr) {
    $sub
}

pub macro count( $($xs:tt)* ) {
    0usize $(+ replace_expr!($xs 1usize))*
}

macro native_functions(
    [$const_name:ident]
        $(
            fn $fn_name:ident($env:ident, $args:ident) $body:tt
        )+
    ) {
    $(
        fn $fn_name($env: &mut $crate::interpreter::Environment, $args: Vec<Value>) -> anyhow::Result<Value> $body
    )+
    pub const $const_name: [(&str, $crate::interpreter::NativeFunction); $crate::interpreter::stdlib::count!($($fn_name)+)] = [
        $(
            (
                stringify!($fn_name),
                $fn_name,
            ),
        )+
    ];
}

macro expect_arity([$($arg:ident),+] = $args:expr) {
    const ARITY: usize = count!($($arg)+);
    let Ok([$($arg),+]) = <[_; ARITY]>::try_from($args) else {
        bail!("expected {} arguments", ARITY);
    };
}

native_functions![
    [NATIVE_FUNCTIONS]
    fn print(_env, args) {
        let num_args = args.len();
        for (i, arg) in args.into_iter().enumerate() {
            print!("{arg}");
            if i < num_args - 1 {
                print!(" ");
            }
        }
        stdout().lock().flush()?;
        Ok(Nil)
    }
    fn println(_env, args) {
        print(_env, args)?;
        println!();
        Ok(Nil)
    }
    fn readline(_env, args) {
        if !args.is_empty() {
            bail!("expected no arguments");
        }
        let mut out = String::new();
        stdin().lock().read_line(&mut out)?;
        Ok(Value::String(out))
    }
    fn str(_env, args) {
        Ok(
            Value::String(
                args
                    .into_iter()
                    .fold(String::new(), |string, value| string + &value.to_string())
            )
        )
    }
    fn num(_env, args) {
        expect_arity!([value] = args);
        let Value::String(string) = value else {
            bail!("expected string");
        };
        let Ok(value) = string.trim().parse() else {
            return Ok(Nil);
        };
        Ok(Value::Number(value))
    }
    fn len(_env, args) {
        expect_arity!([arg] = args);
        Ok(Value::Number(match arg {
            Value::String(string) => string.len() as _,
            Value::Array(array) => array.len() as _,
            _ => bail!("expected string or array")
        }))
    }
];

pub mod ops {
    use crate::interpreter::stdlib::{expect_arity, native_functions};
    use crate::interpreter::Value;

    use anyhow::bail;

    native_functions!(
        [NATIVE_FUNCTIONS]
        fn add(_env, args) {
            expect_arity!([lhs, rhs] = args);
            match (lhs, rhs) {
                (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
                (Value::String(lhs), Value::String(rhs)) => Ok(Value::String(lhs + &rhs)),
                (Value::Array(mut lhs), Value::Array(rhs)) => {
                    lhs.extend(rhs);
                    Ok(Value::Array(lhs))
                }
                _ => bail!("expected strings, numbers or arrays")
            }
        }
        fn sub(_env, args) {
            expect_arity!([lhs, rhs] = args);
            let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) else {
                bail!("expected numbers");
            };
            Ok(Value::Number(lhs - rhs))
        }
        fn mul(_env, args) {
            expect_arity!([lhs, rhs] = args);
            let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) else {
                bail!("expected numbers");
            };
            Ok(Value::Number(lhs * rhs))
        }
        fn div(_env, args) {
            expect_arity!([lhs, rhs] = args);
            let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) else {
                bail!("expected numbers");
            };
            if rhs == 0.0 {
                bail!("cannot divide by zero");
            }
            Ok(Value::Number(lhs / rhs))
        }
        fn lt(_env, args) {
            expect_arity!([lhs, rhs] = args);
            let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) else {
                bail!("expected numbers");
            };
            Ok(Value::Bool(lhs < rhs))
        }
        fn gt(_env, args) {
            expect_arity!([lhs, rhs] = args);
            let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) else {
                bail!("expected numbers");
            };
            Ok(Value::Bool(lhs > rhs))
        }
        fn lte(_env, args) {
            expect_arity!([lhs, rhs] = args);
            let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) else {
                bail!("expected numbers");
            };
            Ok(Value::Bool(lhs <= rhs))
        }
        fn gte(_env, args) {
            expect_arity!([lhs, rhs] = args);
            let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) else {
                bail!("expected numbers");
            };
            Ok(Value::Bool(lhs >= rhs))
        }
        fn eq(_env, args) {
            expect_arity!([lhs, rhs] = args);
            Ok(Value::Bool(lhs == rhs))
        }
        fn ne(_env, args) {
            expect_arity!([lhs, rhs] = args);
            Ok(Value::Bool(lhs != rhs))
        }
    );
}

pub const MODULE_STD: &str = "std";
pub const MODULE_OPS: &str = "ops";

pub fn init_modules(env: &mut Environment) {
    env.define_module(MODULE_STD.to_string()).unwrap();
    env.define_native_functions(NATIVE_FUNCTIONS);

    env.define_module(MODULE_OPS.to_string()).unwrap();
    env.define_native_functions(ops::NATIVE_FUNCTIONS);
    env.end_module();

    env.end_module();
}
