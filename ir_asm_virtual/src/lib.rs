//! Architecture-agnostic intermediate representation
//! based on Control-Flow Graph and Basic Blocks.
//!
//! Various optimizations can be performed on this IR.
//! You can lower it into a more specific IR, assembly, machine code,
//! or even a well-known IR like LLVM IR or MLIR.

#![feature(new_range_api)]
pub(crate) mod builder;
mod context;
mod graph;
mod lowering;
mod pretty;
mod syntax;
mod visit;

pub use context::*;
pub use graph::*;
pub use lowering::*;
pub use syntax::*;
pub use visit::*;
