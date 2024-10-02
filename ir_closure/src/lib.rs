//! IR after the closure conversion.
//!
//! You are encouraged to implement several optimization passes
//! on this IR, or merge this IR into upper-level one by adding
//! `ClosureMake`, `Function`, `Program` to it.

mod context;
mod lowering;
mod syntax;

pub use context::*;
pub use lowering::*;
pub use syntax::*;
