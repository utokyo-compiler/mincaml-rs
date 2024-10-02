use crate::{index::vec::Idx, BitVec, SetLikeVec};

use super::{DirectedGraph, StartNode, Successors};

pub struct DepthFirstSearch<'graph, G>
where
    G: DirectedGraph + Successors,
{
    graph: &'graph G,
    stack: SetLikeVec<G::Node>,
    visited: BitVec<usize>,
}

impl<'graph, G> DepthFirstSearch<'graph, G>
where
    G: DirectedGraph + Successors,
{
    pub fn new(graph: &'graph G) -> Self {
        Self {
            graph,
            stack: SetLikeVec::new(),
            visited: BitVec::repeat(false, graph.num_nodes()),
        }
    }

    pub fn run_from(&mut self, start: G::Node, visitor: &mut impl DepthFirstSearchVisitor<G>) {
        self.visit(start);
        visitor.visit_node(self, start, None);

        while let Some(node) = self.stack.pop() {
            for succ in self.graph.successors(node) {
                if !self.visited[succ.index()] {
                    self.visit(succ);
                    visitor.visit_node(self, succ, Some(node));
                }
            }
        }
    }
    fn visit(&mut self, node: G::Node) {
        self.stack.push(node);
        self.visited.set(node.index(), true);
    }

    pub fn stack(&self) -> &SetLikeVec<G::Node> {
        &self.stack
    }
}

impl<G> DepthFirstSearch<'_, G>
where
    G: DirectedGraph + Successors + StartNode,
{
    pub fn run(&mut self, visitor: &mut impl DepthFirstSearchVisitor<G>) {
        self.run_from(self.graph.start_node(), visitor);
    }
}

pub trait DepthFirstSearchVisitor<G: DirectedGraph + Successors> {
    fn visit_node(&mut self, search: &DepthFirstSearch<G>, node: G::Node, pred: Option<G::Node>);
}
