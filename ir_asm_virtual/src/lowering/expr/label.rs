//! This module contains the `Label` type and is about label resolution.
//!
//! You probably not need to read this module.

use data_structure::{index::vec::IndexVec, FxHashMap};

use crate::{
    builder::DeferredBasicBlock, AbsCallingConv, ArgIndex, BasicBlock, Branch, Local,
    TerminatorKind,
};

use super::State;

impl<'ctx> State<'_, 'ctx> {
    fn resolved_block(&self, label: Label) -> BasicBlock {
        self.label_resolution.get_unchecked(label)
    }

    fn resolved_branch(&self, branch: LabelBranch) -> Branch {
        Branch {
            target: self.resolved_block(branch.target),
            args: branch.args,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
/// A label for a basic block. It is used to refer to a basic block without
/// knowing its index.
pub struct Label(usize);

impl Label {
    /// Create a new label.
    pub fn new() -> Self {
        static COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }
}

#[derive(PartialEq, Eq, Hash)]
/// See [`TerminatorKind::Branch`], [`Branch`]
pub struct LabelBranch {
    pub target: Label,
    pub args: IndexVec<ArgIndex, Local>,
}

impl LabelBranch {
    pub fn new(target: Label) -> Self {
        Self {
            target,
            args: IndexVec::new(),
        }
    }
}

/// The constructor of a terminator used in the lowering process. This is a
/// defunctionalized version of `FnOnce<fn(State) -> TerminatorKind>`.
///
/// Instead of using this, you can make `TerminatorKind` (and `Branch`) generic over
/// `Target = BasicBlock` so that it can be instantiated also with `Label`.
pub enum TerminatorCtor<'ctx> {
    Branch(LabelBranch),
    Call {
        calling_conv: AbsCallingConv<'ctx>,
        args: IndexVec<ArgIndex, Local>,
        branch: LabelBranch,
    },
    ConditionalBranch {
        condition: Local,
        targets: [LabelBranch; 2],
    },
}

impl<'ctx> TerminatorCtor<'ctx> {
    fn construct(self, state: &mut State<'_, '_>) -> TerminatorKind<'ctx> {
        match self {
            TerminatorCtor::Branch(LabelBranch { target, args }) => {
                TerminatorKind::Branch(Branch {
                    target: state.resolved_block(target),
                    args,
                })
            }
            TerminatorCtor::Call {
                calling_conv,
                args,
                branch:
                    LabelBranch {
                        target: branch_target,
                        args: branch_args,
                    },
            } => TerminatorKind::Call {
                calling_conv,
                args,
                branch: Branch {
                    target: state.resolved_block(branch_target),
                    args: branch_args,
                },
            },
            TerminatorCtor::ConditionalBranch { condition, targets } => {
                TerminatorKind::ConditionalBranch {
                    condition,
                    targets: targets.map(|target| state.resolved_branch(target)),
                }
            }
        }
    }
}

pub struct ResolveHandler<'builder, 'ctx> {
    deferred_basic_block: DeferredBasicBlock,
    ctor: TerminatorCtor<'ctx>,
    _marker: std::marker::PhantomData<&'builder ()>,
}

impl<'builder, 'ctx> ResolveHandler<'builder, 'ctx> {
    pub fn new(deferred_basic_block: DeferredBasicBlock, ctor: TerminatorCtor<'ctx>) -> Self {
        Self {
            deferred_basic_block,
            ctor,
            _marker: std::marker::PhantomData,
        }
    }

    /// This call does not create a new basic block.
    pub fn run(self, state: &mut State<'builder, 'ctx>) {
        let Self {
            deferred_basic_block,
            ctor,
            ..
        } = self;
        let terminator = ctor.construct(state);
        state
            .builder
            .terminate_block_deferred(deferred_basic_block, terminator);
    }
}

#[derive(Default)]
struct HandlerMap<'builder, 'ctx>(FxHashMap<Label, Vec<ResolveHandler<'builder, 'ctx>>>);

impl<'builder, 'ctx> HandlerMap<'builder, 'ctx> {
    pub fn register(&mut self, label: Label, handler: ResolveHandler<'builder, 'ctx>) {
        self.0.entry(label).or_default().push(handler);
    }

    fn remove(&mut self, label: Label) -> Option<Vec<ResolveHandler<'builder, 'ctx>>> {
        self.0.remove(&label)
    }
}

#[derive(Default)]
pub struct LabelResolution<'builder, 'ctx> {
    resolved: FxHashMap<Label, BasicBlock>,
    handlers: HandlerMap<'builder, 'ctx>,
}

impl<'builder, 'ctx> LabelResolution<'builder, 'ctx> {
    #[must_use]
    pub fn insert(
        &mut self,
        label: Label,
        basic_block: BasicBlock,
    ) -> Option<Vec<ResolveHandler<'builder, 'ctx>>> {
        self.resolved.insert(label, basic_block);
        self.handlers.remove(label)
    }

    pub fn get_unchecked(&self, label: Label) -> BasicBlock {
        *self.resolved.get(&label).expect("label not resolved")
    }

    #[inline]
    pub fn register(&mut self, label: Label, handler: ResolveHandler<'builder, 'ctx>) {
        self.handlers.register(label, handler)
    }
}
