use crate::parse::{BinaryOp, Path};
use std::io::{stdin, stdout, BufRead, Write};

use crate::interpreter::runtime::Runtime;
use crate::interpreter::RuntimeError::TypeMismatch;
use crate::interpreter::{Primitive, Primitive::Nil};

pub macro replace_expr($_t:tt $sub:expr) {
    $sub
}

pub macro count( $($xs:tt)* ) {
    0usize $(+ replace_expr!($xs 1usize))*
}

#[macro_export]
macro_rules! native_function_def {
    ($fn_name:ident () $body:tt) => {
        fn $fn_name(args: Vec<Primitive>) -> $crate::interpreter::error::Result {
            if !args.is_empty() {
                return Err($crate::interpreter::RuntimeError::ArityMismatch { expected: 0, actual: args.len() });
            }
            $body
        }
    };

    ($fn_name:ident (...$args:ident) $body:tt) => {
        fn $fn_name($args: Vec<Primitive>) -> $crate::interpreter::error::Result $body
    };

    ($fn_name:ident ($($arg:ident),+) $body:tt) => {
        fn $fn_name(args: Vec<Primitive>) -> $crate::interpreter::error::Result {
            const ARITY: usize = count!($($arg)+);
            let actual = args.len();
            let Ok([$($arg),+]) = <[Primitive; ARITY]>::try_from(args) else {
                return Err($crate::interpreter::RuntimeError::ArityMismatch { expected: ARITY, actual });
            };
            $body
        }
    };
}

macro native_functions(
    [$const_name:ident]
    $(
        fn $fn_name:ident $args:tt $body:tt
    )+
) {
    use crate::native_function_def;
    $(
        native_function_def!{ $fn_name $args $body }
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

native_functions![
    [NATIVE_FUNCTIONS]
    fn print(...args) {
        let num_args = args.len();
        for (i, arg) in args.into_iter().enumerate() {
            print!("{arg}");
            if i < num_args - 1 {
                print!(" ");
            }
        }
        stdout().lock().flush().unwrap();
        Ok(Nil)
    }
    fn println(...args) {
        print(args)?;
        println!();
        Ok(Nil)
    }
    fn readline() {
        let mut out = String::new();
        stdin().lock().read_line(&mut out).unwrap();
        Ok(Primitive::String(out))
    }
    fn str(...args) {
        Ok(
            Primitive::String(
                args
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join("")
            )
        )
    }
    fn num(value) {
        let Primitive::String(string) = value else {
            return Err(TypeMismatch { expected: "string" });
        };
        let Ok(value) = string.trim().parse() else {
            return Ok(Nil);
        };
        Ok(Primitive::Number(value))
    }
    fn len(arg) {
        Ok(Primitive::Number(match arg {
            Primitive::String(string) => string.len() as _,
            Primitive::Array(array) => array.len() as _,
            _ => return Err(TypeMismatch { expected: "string or array" })
        }))
    }
];

pub mod ops {
    use crate::interpreter::error::RuntimeError::{DivideByZero, TypeMismatch};
    use crate::interpreter::stdlib::native_functions;
    use crate::interpreter::Primitive;

    native_functions!(
        [NATIVE_FUNCTIONS]
        fn add(lhs, rhs) {
            match (lhs, rhs) {
                (Primitive::Number(lhs), Primitive::Number(rhs)) => Ok(Primitive::Number(lhs + rhs)),
                (Primitive::String(lhs), Primitive::String(rhs)) => Ok(Primitive::String(lhs + &rhs)),
                (Primitive::Array(mut lhs), Primitive::Array(rhs)) => {
                    lhs.extend(rhs);
                    Ok(Primitive::Array(lhs))
                }
                _ => return Err(TypeMismatch { expected: "strings, numbers or arrays" })
            }
        }
        fn sub(lhs, rhs) {
            let (Primitive::Number(lhs), Primitive::Number(rhs)) = (lhs, rhs) else {
                return Err(TypeMismatch { expected: "numbers" });
            };
            Ok(Primitive::Number(lhs - rhs))
        }
        fn mul(lhs, rhs) {
            let (Primitive::Number(lhs), Primitive::Number(rhs)) = (lhs, rhs) else {
                return Err(TypeMismatch { expected: "numbers" });
            };
            Ok(Primitive::Number(lhs * rhs))
        }
        fn div(lhs, rhs) {
            let (Primitive::Number(lhs), Primitive::Number(rhs)) = (lhs, rhs) else {
                return Err(TypeMismatch { expected: "numbers" });
            };
            if rhs == 0.0 {
                return Err(DivideByZero)
            }
            Ok(Primitive::Number(lhs / rhs))
        }
        fn lt(lhs, rhs) {
            let (Primitive::Number(lhs), Primitive::Number(rhs)) = (lhs, rhs) else {
                return Err(TypeMismatch { expected: "numbers" });
            };
            Ok(Primitive::Bool(lhs < rhs))
        }
        fn gt(lhs, rhs) {
            let (Primitive::Number(lhs), Primitive::Number(rhs)) = (lhs, rhs) else {
                return Err(TypeMismatch { expected: "numbers" });
            };
            Ok(Primitive::Bool(lhs > rhs))
        }
        fn lte(lhs, rhs) {
            let (Primitive::Number(lhs), Primitive::Number(rhs)) = (lhs, rhs) else {
                return Err(TypeMismatch { expected: "numbers" });
            };
            Ok(Primitive::Bool(lhs <= rhs))
        }
        fn gte(lhs, rhs) {
            let (Primitive::Number(lhs), Primitive::Number(rhs)) = (lhs, rhs) else {
                return Err(TypeMismatch { expected: "numbers" });
            };
            Ok(Primitive::Bool(lhs >= rhs))
        }
        fn eq(lhs, rhs) {
            Ok(Primitive::Bool(lhs == rhs))
        }
        fn ne(lhs, rhs) {
            Ok(Primitive::Bool(lhs != rhs))
        }
    );
}

pub const MODULE_STD: &str = "std";
pub const MODULE_OPS: &str = "ops";

pub fn init(env: &mut Runtime) {
    env.define_module(MODULE_STD.to_string()).unwrap();
    env.define_native_functions(NATIVE_FUNCTIONS);

    env.define_module(MODULE_OPS.to_string()).unwrap();
    env.define_native_functions(ops::NATIVE_FUNCTIONS);
    env.end_module();

    env.end_module();
}

pub fn binary_op_to_fn_path(op: BinaryOp) -> Path {
    let op_fn_name = match op {
        BinaryOp::Add => "add",
        BinaryOp::Subtract => "sub",
        BinaryOp::Multiply => "mul",
        BinaryOp::Divide => "div",
        BinaryOp::Less => "lt",
        BinaryOp::Greater => "gt",
        BinaryOp::LessEqual => "lte",
        BinaryOp::GreaterEqual => "gte",
        BinaryOp::Equal => "eq",
        BinaryOp::NotEqual => "ne",
    };
    [MODULE_STD, MODULE_OPS, op_fn_name].into_iter().collect()
}
