use data_structure::graph::traversal;

use crate::{BasicBlock, FunctionDef};

/// Assuming the function is a reducible control flow graph, find all back edges.
///
/// Reducible control flow graph is a control flow graph whose edges can be
/// partitioned into a set of forward edges and a set of back edges.
pub fn find(function: &FunctionDef<'_>) -> Vec<(BasicBlock, BasicBlock)> {
    let mut finder = Finder {
        back_edges: Vec::new(),
    };
    let mut search = traversal::DepthFirstSearch::new(function);
    search.run(&mut finder);
    finder.back_edges
}

struct Finder {
    back_edges: Vec<(BasicBlock, BasicBlock)>,
}

impl traversal::DepthFirstSearchVisitor<FunctionDef<'_>> for Finder {
    fn visit_node(
        &mut self,
        search: &traversal::DepthFirstSearch<FunctionDef<'_>>,
        node: BasicBlock,
        pred: Option<BasicBlock>,
    ) {
        if let Some(pred) = pred {
            if search.stack().contains(&node) {
                self.back_edges.push((pred, node));
            }
        }
    }
}
