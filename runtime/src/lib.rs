// This code is taken from:
// https://docs.wasmtime.dev/examples-rust-wasi.html#invoke-the-wasm-module

mod intrinsic;
#[cfg(feature = "minrt-runtime")]
mod mini_ml_runtime;

use wasi_common::sync::WasiCtxBuilder;
use wasmtime::{Engine, Linker, Module, Result, Store};

pub fn run_bytes<T>(bytes: &[u8], inherit_args: impl Iterator<Item = T>) -> Result<()>
where
    T: AsRef<str>,
{
    // Define the WASI functions globally on the `Config`.
    let engine = Engine::default();
    let mut linker = Linker::new(&engine);
    wasi_common::sync::add_to_linker(&mut linker, |s| s)?;
    intrinsic::provide_intrinsic_impls(&mut linker)?;
    #[cfg(feature = "minrt-runtime")]
    mini_ml_runtime::provide_minrt_impls(&mut linker)?;

    // Create a WASI context and put it in a Store; all instances in the store
    // share this context. `WasiCtxBuilder` provides a number of ways to
    // configure what the target program will have access to.
    let mut wasi_ctx_builder = WasiCtxBuilder::new();
    wasi_ctx_builder.inherit_stdio();
    for arg in inherit_args {
        wasi_ctx_builder.arg(arg.as_ref())?;
    }
    let wasi = wasi_ctx_builder.build();
    let mut store = Store::new(&engine, wasi);

    // Instantiate our module with the imports we've created, and run it.
    let module = Module::new(&engine, bytes)?;
    linker.module(&mut store, "", &module)?;
    linker
        .get_default(&mut store, "")?
        .typed::<(), ()>(&store)?
        .call(&mut store, ())?;

    Ok(())
}
