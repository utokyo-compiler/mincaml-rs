//! Architecture-agnostic intermediate representation
//! based on Control-Flow Graph and Basic Blocks.
//!
//! Various optimizations can be performed on this IR.
//! You can lower it into a more specific IR, assembly, machine code,
//! or even a well-known IR like LLVM IR or MLIR.

use core::range::Range;

use data_structure::{
    arena::Box,
    index::{
        vec::{Idx, IndexVec},
        Indexable,
    },
};

pub use ir_closure::{
    ArgIndex, BinOp, DisambiguatedIdent, FnName, Ident, LitKind, Pattern, TupleIndex, Ty, Typed,
    UnOp,
};

pub type Expr<'ctx> = Box<'ctx, TypedExprKind<'ctx>>;
pub type TypedExprKind<'ctx> = Typed<'ctx, ExprKind<'ctx>>;

pub struct Program<'ctx> {
    pub functions: Vec<Function<'ctx>>,
    pub main: Function<'ctx>,
}

pub const MAIN_FN_NAME: FnName<'static> =
    // safety: "main" does not get referenced in the program.
    FnName::new_unchecked(DisambiguatedIdent::new_compiler_unchecked("main", 0));

/// A function definition.
///
/// A function may be defined as a closure
/// so it may capture variables from the environment.
pub struct Function<'ctx> {
    pub name: FnName<'ctx>,

    /// Local variables of the function.
    ///
    /// This includes arguments and also variables captured as a thunk.
    pub local_decls: IndexVec<Local, LocalDecl<'ctx>>,

    /// Arguments passed to the function.
    ///
    /// (meta): Actual value of the arguments are allocated to `local_decls`.
    pub args: Range<Local>,

    /// Arguments captured by the closure.
    ///
    /// They are not passed as arguments to the function but
    /// are required to evaluate the function body.
    pub args_via_closure: Range<Local>,

    pub basic_blocks: IndexVec<BasicBlock, BasicBlockData<'ctx>>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Local(usize);
impl Local {
    pub const RETURN_LOCAL: Self = Self(0);
}
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
    /// This is required to support the functionality of phi nodes.
    pub args: IndexVec<ArgIndex, Local>,
    pub stmts: IndexVec<StmtIndex, StmtKind<'ctx>>,
    pub terminator: TerminatorKind<'ctx>,
}
impl Indexable<BasicBlock> for BasicBlockData<'_> {}

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
    Assign { place: Place, value: Expr<'ctx> },
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Place {
    Discard,
    Local(Local),
    Projection {
        base: Local,
        projection_kind: ProjectionKind,
    },
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ProjectionKind {
    TupleIndex(TupleIndex),
    ArrayElem(Local),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TerminatorKind<'ctx> {
    /// Return from the function.
    Return,

    /// MLIR-like branch instruction.
    ///
    /// Instead of phi nodes, we pass arguments to the destination block.
    ///
    /// DO NOT forget to count `args` as a use of the operands of the source block.
    ///
    /// See also: https://mlir.llvm.org/docs/Dialects/ControlFlowDialect/#cfcond_br-cfcondbranchop
    Branch {
        target: BasicBlock,
        args: IndexVec<ArgIndex, Local>,
    },

    /// Conditional branch.
    ///
    /// You might want to have instructions with more branch targets
    /// to support switch-like if expressions.
    ConditionalBranch {
        condition: Local,
        /// Branch to the first target if the condition is true.
        ///
        /// Note that we have not decided how `true` and
        /// `false` are represented at this point.
        targets: [BasicBlock; 2],
    },

    /// Call a function.
    Call {
        calling_conv: AbsCallingConv,
        fn_name: FnName<'ctx>,
        args: IndexVec<ArgIndex, Local>,
        destination: Place,
        /// The block to branch to after the call.
        target: BasicBlock,
    },
}

impl<'ctx> TerminatorKind<'ctx> {
    pub fn as_mut_branch_target(&mut self) -> Option<&mut BasicBlock> {
        match self {
            Self::Branch { target, .. } => Some(target),
            _ => None,
        }
    }
    pub fn as_mut_conditional_branch_targets(&mut self) -> Option<&mut [BasicBlock; 2]> {
        match self {
            Self::ConditionalBranch { targets, .. } => Some(targets),
            _ => None,
        }
    }
}

/// Abstract calling convention.
pub type AbsCallingConv = ir_closure::ApplyKind;

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

impl<'ctx> ExprKind<'ctx> {
    pub fn kind(&self) -> &Self {
        self
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Closure<'ctx> {
    pub fn_name: FnName<'ctx>,
    pub captured_args: IndexVec<ArgIndex, Local>,
}
