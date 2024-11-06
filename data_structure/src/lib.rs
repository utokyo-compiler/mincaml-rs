#![feature(hash_set_entry)]
#![feature(new_range_api)]
pub mod arena;
pub mod graph;
pub mod index;
pub mod interning;
pub mod set_like_vec;

pub use bitvec::array::BitArray;
pub use bitvec::vec::BitVec;
pub use rustc_hash::{FxHashMap, FxHashSet, FxHasher};

pub use set_like_vec::SetLikeVec;

use std::hash::BuildHasherDefault;

pub type FxIndexMap<K, V> = indexmap::IndexMap<K, V, BuildHasherDefault<FxHasher>>;
pub type FxIndexSet<V> = indexmap::IndexSet<V, BuildHasherDefault<FxHasher>>;
