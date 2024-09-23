#![feature(hash_set_entry)]
pub mod arena;
pub mod index;
pub mod interning;
pub mod set_like_vec;

pub use rustc_hash::FxHashMap;
pub use rustc_hash::FxHashSet;

pub use set_like_vec::SetLikeVec;
