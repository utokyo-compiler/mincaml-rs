//! Graph implementation for the IR.
mod back_edges;

pub use back_edges::find as find_back_edges;

use data_structure::graph;

use crate::{BasicBlock, FunctionDef};

impl graph::DirectedGraph for FunctionDef<'_> {
    type Node = BasicBlock;

    fn num_nodes(&self) -> usize {
        self.basic_blocks.len()
    }
}

impl graph::StartNode for FunctionDef<'_> {
    fn start_node(&self) -> Self::Node {
        BasicBlock::ENTRY_BLOCK
    }
}

impl graph::Successors for FunctionDef<'_> {
    fn successors(&self, node: Self::Node) -> impl Iterator<Item = Self::Node> {
        self.basic_blocks[node].terminator().successors()
    }
}
