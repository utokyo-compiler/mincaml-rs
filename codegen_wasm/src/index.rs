use data_structure::index::vec::Idx;

macro_rules! define_wasm_index {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub struct $name(u32);
        impl $name {
            /// Unwrap the index. Use this only when encoding the index.
            pub fn unwrap_idx(self) -> u32 {
                self.0
            }
        }
        impl Idx for $name {
            fn new(idx: usize) -> Self {
                Self(idx as u32)
            }
            fn index(self) -> usize {
                self.0 as usize
            }
        }
    };
}

define_wasm_index!(TypeIdx);
define_wasm_index!(FuncIdx);
define_wasm_index!(LocalIdx);
