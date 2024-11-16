use wasmtime::{Linker, Result};

/// Provides intrinsic functions to the given linker.
pub fn provide_intrinsic_impls<T>(linker: &mut Linker<T>) -> Result<()> {
    const MODULE: &str = "mincaml:runtime";
    linker.func_wrap(MODULE, "print_int", |value: i32| print!("{value}"))?;
    linker.func_wrap(MODULE, "print_newline", || println!())?;

    linker.func_wrap(MODULE, "int_of_float", |value: f32| value as i32)?;
    linker.func_wrap(MODULE, "float_of_int", |value: i32| value as f32)?;

    linker.func_wrap(MODULE, "sin", f32::sin)?;
    linker.func_wrap(MODULE, "cos", f32::cos)?;
    linker.func_wrap(MODULE, "sqrt", f32::sqrt)?;
    linker.func_wrap(MODULE, "abs_float", f32::abs)?;
    Ok(())
}
