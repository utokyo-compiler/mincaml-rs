//! Syntax of this IR.

use core::range::Range;

use data_structure::{
    arena::Box,
    index::{
        vec::{Idx, IndexVec},
        Indexable,
    },
};

pub use ir_closure::{
    ArgIndex, BinOp, DisambiguatedIdent, FnIndex, FnName, FunctionInstance, Ident, LitKind,
    Pattern, TupleIndex, Ty, TyKind, Typed, UnOp,
};

pub type Expr<'ctx> = Box<'ctx, TypedExprKind<'ctx>>;
pub type TypedExprKind<'ctx> = Typed<'ctx, ExprKind<'ctx>>;

pub struct Program<'ctx> {
    pub functions: IndexVec<FnIndex, FunctionDef<'ctx>>,
}

impl Indexable<FnIndex> for FunctionDef<'_> {}

/// A function definition.
///
/// A function may be defined as a closure
/// so it may capture variables from the environment.
pub struct FunctionDef<'ctx> {
    pub name: FnName<'ctx>,

    /// Local variables of the function.
    ///
    /// This includes arguments and also variables captured by the closure.
    pub local_decls: IndexVec<Local, LocalDecl<'ctx>>,

    /// Arguments passed to the function.
    ///
    /// (meta): Actual value of the arguments are allocated in `local_decls`.
    pub args: Range<Local>,

    /// Arguments captured by the closure.
    ///
    /// They are not passed as arguments to the function but
    /// are required to evaluate the function body.
    pub args_via_closure: Range<Local>,

    /// The body of the function as a control-flow graph.
    pub basic_blocks: IndexVec<BasicBlock, BasicBlockData<'ctx>>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
pub struct Local(usize);
impl Indexable<ArgIndex> for Local {}
impl Indexable<TupleIndex> for Local {}

impl Idx for Local {
    fn new(idx: usize) -> Self {
        Self(idx)
    }

    fn index(self) -> usize {
        self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LocalDecl<'ctx> {
    pub ident: Ident<'ctx>,
}

impl<'ctx> LocalDecl<'ctx> {
    pub fn ty(&self) -> Ty<'ctx> {
        self.ident.ty
    }
}

impl Indexable<Local> for LocalDecl<'_> {}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct BasicBlock(usize);

impl Idx for BasicBlock {
    fn new(idx: usize) -> Self {
        Self(idx)
    }

    fn index(self) -> usize {
        self.0
    }
}
impl BasicBlock {
    pub const ENTRY_BLOCK: Self = Self(0);
}

pub struct BasicBlockData<'ctx> {
    /// Arguments passed to the block.
    ///
    /// This is required to support the functionality of phi nodes,
    /// which is enough expressive to represent `if` expressions
    /// in SSA form.
    ///
    /// DO NOT forget to count `args` as a definition.
    pub args: IndexVec<ArgIndex, Local>,

    pub stmts: IndexVec<StmtIndex, StmtKind<'ctx>>,

    /// Terminator instruction for this block.
    ///
    /// This field should only be `None` during construction.
    /// Allowing `None` is beneficial for the construction of
    /// the control-flow graph because it can eliminate
    /// dependency on the order of the construction,
    /// which decides the `BasicBlock` of `BasicBlockData`.
    pub(crate) terminator: Option<TerminatorKind<'ctx>>,
}
// `BasicBlock` is just an index to `BasicBlockData`.
impl Indexable<BasicBlock> for BasicBlockData<'_> {}

impl<'ctx> BasicBlockData<'ctx> {
    pub fn terminator(&self) -> &TerminatorKind<'ctx> {
        self.terminator.as_ref().expect("terminator must be set")
    }

    pub fn terminator_mut(&mut self) -> &mut TerminatorKind<'ctx> {
        self.terminator.as_mut().expect("terminator must be set")
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct StmtIndex(usize);

impl Idx for StmtIndex {
    fn new(idx: usize) -> Self {
        Self(idx)
    }

    fn index(self) -> usize {
        self.0
    }
}

impl Indexable<StmtIndex> for StmtKind<'_> {}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum StmtKind<'ctx> {
    /// No operation. Useful for removing statements.
    Nop,

    /// Assign a value to a place.
    Assign {
        /// The place to assign the value. if `None`, the value is discarded.
        place: Option<Place>,

        value: Expr<'ctx>,
    },
}

impl<'ctx> StmtKind<'ctx> {
    /// Remove the statement without changing the `StmtIndex`.
    pub fn make_nop(&mut self) {
        *self = Self::Nop;
    }

    /// Check if the statement has any effect. Assumes SSA form.
    pub fn has_effect(&self) -> bool {
        matches!(
            self,
            Self::Assign {
                place: Some(Place::Projection { .. }),
                ..
            }
        )
    }

    pub fn local(&self) -> Option<Local> {
        let Self::Assign {
            place: Some(place), ..
        } = self
        else {
            return None;
        };
        Some(match place {
            Place::Local(local) => *local,
            Place::Projection { base, .. } => *base,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
/// Location of a value. Also known as "L-value".
pub enum Place {
    Local(Local),
    Projection {
        base: Local,
        projection_kind: ProjectionKind,
    },
}

#[derive(Debug, PartialEq, Eq, Hash)]
/// Projection from a value.
pub enum ProjectionKind {
    /// Projection from a tuple, like `x.0`.
    TupleIndex(TupleIndex),

    /// Projection from an array, like `x[i]`.
    ///
    /// It is useful to allow this variant to have also `Constant` as an index.
    ArrayElem(Local),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TerminatorKind<'ctx> {
    /// Return from the function. The return value is passed as arguments.
    Return(IndexVec<ArgIndex, Local>),

    /// MLIR-like branch instruction.
    ///
    /// Instead of phi nodes, we pass arguments to the destination block.
    ///
    /// DO NOT forget to count `args` as a use of the operands of the source block.
    ///
    /// See also: https://mlir.llvm.org/docs/Dialects/ControlFlowDialect/#cfbr-cfbranchop
    Branch(Branch),

    /// Conditional branch.
    ///
    /// You might want to have instructions with more branch targets
    /// to support `switch`-like `if` expressions.
    ConditionalBranch {
        condition: Local,

        /// Branch to the first target if the condition is true.
        ///
        /// Note that we have not decided how `true` and
        /// `false` are represented at this point.
        targets: [Branch; 2],
    },

    /// Call a function.
    Call {
        /// The calling convention of the function.
        calling_conv: AbsCallingConv<'ctx>,

        args: IndexVec<ArgIndex, Local>,

        /// The block to branch to after the call and
        /// the destination place for the return value.
        branch: Branch,
    },
}

#[derive(Debug, PartialEq, Eq, Hash)]
/// See [`TerminatorKind::Branch`].
pub struct Branch {
    pub target: BasicBlock,
    pub args: IndexVec<ArgIndex, Local>,
}

impl Branch {
    pub fn no_args(target: BasicBlock) -> Self {
        Self {
            target,
            args: IndexVec::new(),
        }
    }
    pub fn one_arg(target: BasicBlock, arg: Local) -> Self {
        Self {
            target,
            args: IndexVec::from_raw_vec(vec![arg]),
        }
    }
}

impl TerminatorKind<'_> {
    pub fn successors(&self) -> impl DoubleEndedIterator<Item = BasicBlock> + '_ {
        let map_target = |branch: &Branch| branch.target;
        match self {
            Self::Return(..) => [].iter().map(map_target).chain(None),
            Self::Branch(branch) | Self::Call { branch, .. } => {
                [].iter().map(map_target).chain(Some(branch.target))
            }
            Self::ConditionalBranch { targets, .. } => targets.iter().map(map_target).chain(None),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
/// Abstract calling convention. A concrete calling convention must separate
/// these two cases.
pub enum AbsCallingConv<'ctx> {
    /// Direct call.
    Direct {
        /// The function to call.
        function: FunctionInstance<'ctx>,
    },

    /// Call a closure.
    Closure {
        /// The closure to call.
        local: Local,
    },
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExprKind<'ctx> {
    Const(LitKind),
    Unary(UnOp, Local),
    Binary(BinOp, Local, Local),
    ClosureMake(Closure<'ctx>),
    Tuple(IndexVec<TupleIndex, Local>),
    ArrayMake(Local, Local),
    Read(Place),
}

impl ExprKind<'_> {
    pub fn kind(&self) -> &Self {
        self
    }

    pub fn is_unit(&self) -> bool {
        match self {
            ExprKind::Const(LitKind::Unit) => true,
            ExprKind::Tuple(elems) => elems.is_empty(),
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Closure<'ctx> {
    pub function: FunctionInstance<'ctx>,
    pub captured_args: IndexVec<ArgIndex, Local>,
}
