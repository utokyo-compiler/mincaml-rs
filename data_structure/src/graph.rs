pub mod traversal;

use crate::index;

pub trait DirectedGraph {
    type Node: index::vec::Idx;

    fn num_nodes(&self) -> usize;
}

pub trait StartNode: DirectedGraph {
    fn start_node(&self) -> Self::Node;
}

pub trait Successors: DirectedGraph {
    fn successors(&self, node: Self::Node) -> impl Iterator<Item = Self::Node>;
}

pub trait Predecessors: DirectedGraph {
    fn predecessors(&self, node: Self::Node) -> impl Iterator<Item = Self::Node>;
}

pub trait ControlFlowGraph: DirectedGraph + StartNode + Successors + Predecessors {}
impl<T> ControlFlowGraph for T where T: DirectedGraph + StartNode + Successors + Predecessors {}
