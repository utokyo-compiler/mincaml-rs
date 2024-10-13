use wasmtime::{Linker, Result};

pub fn provide_intrinsic_impls<T>(linker: &mut Linker<T>) -> Result<()> {
    linker.func_wrap("mincaml:runtime", "print_int", |value: i32| {
        println!("{value}");
    })?;
    Ok(())
}
