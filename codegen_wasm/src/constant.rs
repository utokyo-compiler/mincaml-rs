use wasm_encoder::MemArg;

/// The global index of the heap pointer.
pub const HEAP_PTR: u32 = 0;

/// Default memory argument.
pub const MEM_ARG: MemArg = MemArg {
    offset: 0,
    align: 2,
    memory_index: 0,
};

/// closure calling convention
///
/// The index of the table. Currently, we only have one table.
pub const TABLE_IDX: u32 = 0;
