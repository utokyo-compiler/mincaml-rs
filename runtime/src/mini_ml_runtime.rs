use std::{
    io::{ErrorKind, Write},
    str::FromStr,
};

use wasmtime::{Caller, Engine, HostContext, IntoFunc, Linker, Result, WasmRet, WasmTy};

/// Provides minrt implementations to the given linker.
///
/// This implementation should be synced with the `miniMLRuntime.{mli, ml}` files. If you think
/// there is a mismatch, please open an issue or consult the instructor.
pub fn provide_minrt_impls<T>(linker: &mut Linker<T>) -> Result<()> {
    const MODULE: &str = "mincaml:runtime";

    linker.func_wrap(MODULE, "fequal", BoolAsI32(|x: f32, y: f32| x == y))?;
    linker.func_wrap(MODULE, "fless", BoolAsI32(|x: f32, y: f32| x < y))?;

    linker.func_wrap(MODULE, "fispos", BoolAsI32(|x: f32| x.is_sign_positive()))?;
    linker.func_wrap(MODULE, "fisneg", BoolAsI32(|x: f32| x.is_sign_negative()))?;
    linker.func_wrap(MODULE, "fiszero", BoolAsI32(|x: f32| x == 0.0))?;

    linker.func_wrap(MODULE, "xor", BoolAsI32(|x: bool, y: bool| x ^ y))?;
    linker.func_wrap(MODULE, "not", BoolAsI32(|x: bool| !x))?;

    linker.func_wrap(MODULE, "fabs", |x: f32| x.abs())?;
    linker.func_wrap(MODULE, "fneg", |x: f32| -x)?;
    linker.func_wrap(MODULE, "fsqr", |x: f32| x * x)?;
    linker.func_wrap(MODULE, "fhalf", |x: f32| x / 2.0)?;
    linker.func_wrap(MODULE, "floor", |x: f32| x.floor())?;

    linker.func_wrap(MODULE, "atan", |x: f32| x.atan())?;

    linker.func_wrap(MODULE, "read_float", read_ws_delimited_once::<f32>)?;
    linker.func_wrap(MODULE, "read_int", read_ws_delimited_once::<i32>)?;

    linker.func_wrap(MODULE, "print_char", |value: i32| {
        let byte = value as u8;
        std::io::stdout().write_all(&[byte]).unwrap();
    })?;

    Ok(())
}

fn read_ws_delimited_once<T>() -> T
where
    T: FromStr,
    <T as std::str::FromStr>::Err: std::fmt::Debug,
{
    fn is_space_byte(b: u8) -> bool {
        matches!(b, b' ' | b'\n' | b'\r' | b'\t')
    }

    fn skip_till_ws(reader: &mut impl std::io::BufRead) -> std::io::Result<()> {
        loop {
            let (done, used) = {
                let available = match reader.fill_buf() {
                    Ok(n) => n,
                    Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
                    Err(e) => return Err(e),
                };
                match available.iter().position(|&b| !is_space_byte(b)) {
                    Some(i) => (true, i),
                    None => (false, available.len()),
                }
            };
            reader.consume(used);
            if done || used == 0 {
                return Ok(());
            }
        }
    }

    fn read_until_ws(r: &mut impl std::io::BufRead, buf: &mut Vec<u8>) -> std::io::Result<usize> {
        let mut read = 0;
        loop {
            let (done, used) = {
                let available = match r.fill_buf() {
                    Ok(n) => n,
                    Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
                    Err(e) => return Err(e),
                };
                match available.iter().position(|&b| is_space_byte(b)) {
                    Some(i) => {
                        buf.extend_from_slice(&available[..i]);
                        (true, i + 1)
                    }
                    None => {
                        buf.extend_from_slice(available);
                        (false, available.len())
                    }
                }
            };
            r.consume(used);
            read += used;
            if done || used == 0 {
                return Ok(read);
            }
        }
    }

    let mut buf = Vec::with_capacity(8);
    let mut reader = std::io::stdin().lock();

    // skip till whitespace
    skip_till_ws(&mut reader).unwrap();

    // read until whitespace
    read_until_ws(&mut reader, &mut buf).unwrap();

    let str = std::str::from_utf8(&buf).expect("invalid utf-8");
    str.parse().expect("invalid input")
}

struct BoolAsI32<F>(F);

trait WrappedWasmTy {
    type T: WasmTy;
    fn from_wasm_ty(from: Self::T) -> Self;
}
trait WrappedWasmRet {
    type T: WasmRet;
    fn into_wasm_ret(self) -> Self::T;
}

impl WrappedWasmTy for bool {
    type T = i32;
    fn from_wasm_ty(from: Self::T) -> Self {
        from != 0
    }
}
impl WrappedWasmRet for bool {
    type T = i32;
    fn into_wasm_ret(self) -> Self::T {
        self as i32
    }
}

macro_rules! transparent_wasm_ty_impl {
    ($for_ty:ty) => {
        impl WrappedWasmTy for $for_ty {
            type T = $for_ty;
            fn from_wasm_ty(from: Self::T) -> Self {
                from
            }
        }
    };
    ($($for_ty:ty)*) => {
        $(transparent_wasm_ty_impl!($for_ty);)*
    }
}

transparent_wasm_ty_impl!(i32 f32);

macro_rules! for_each_function_signature {
    ($makro:ident) => {
        $makro!();
        $makro!(A1);
        $makro!(A1 A2);
    }
}

macro_rules! impl_into_func {
    ($($args:ident)*) => {
        impl<T, F, $($args,)* R> IntoFunc<T, ($($args,)*), R> for BoolAsI32<F>
        where
            F: Fn($($args),*) -> R + Send + Sync + 'static,
            $($args: WrappedWasmTy,)*
            R: WrappedWasmRet,
        {
            fn into_func(self, engine: &Engine) -> HostContext {
                #[allow(non_snake_case)]
                let f = move |_: Caller<'_, T>, $($args:<$args as WrappedWasmTy>::T),*| {
                    self.0($($args::from_wasm_ty($args)),*).into_wasm_ret()
                };

                f.into_func(engine)
            }
        }

        impl<T, F, $($args,)* R> IntoFunc<T, (Caller<'_, T>, $($args,)*), R> for BoolAsI32<F>
        where
            F: Fn(Caller<'_, T>, $($args),*) -> R + Send + Sync + 'static,
            $($args: WrappedWasmTy,)*
            R: WrappedWasmRet,
        {
            fn into_func(self, engine: &Engine) -> HostContext {
                #[allow(non_snake_case)]
                let f = move |caller: Caller<'_, T>, $($args:<$args as WrappedWasmTy>::T),*| {
                    self.0(caller, $($args::from_wasm_ty($args)),*).into_wasm_ret()
                };

                f.into_func(engine)
            }
        }
    }
}

for_each_function_signature!(impl_into_func);
