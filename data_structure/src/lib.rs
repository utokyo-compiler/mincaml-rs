#![feature(hash_set_entry)]
#![feature(new_range_api)]
pub mod arena;
pub mod graph;
pub mod index;
pub mod interning;

pub use bitvec::array::BitArray;
pub use bitvec::vec::BitVec;
pub type FxIndexSet<T> = indexmap::IndexSet<T, rustc_hash::FxBuildHasher>;
pub type FxIndexMap<K, V> = indexmap::IndexMap<K, V, rustc_hash::FxBuildHasher>;
pub use rustc_hash::FxHashMap;
pub use rustc_hash::FxHashSet;
