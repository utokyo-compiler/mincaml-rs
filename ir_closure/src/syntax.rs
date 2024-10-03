//! Syntax of this IR.
use data_structure::{
    arena::Box,
    index::{
        vec::{Idx, IndexVec},
        Indexable,
    },
};

pub use ir_knorm::{
    ArgIndex, BinOp, RelationBinOpKind, DisambiguatedIdent, FloatBinOpKind, Ident, IntBinOpKind,
    LitKind, Pattern, TupleIndex, Ty, TyKind, Typed, TypedIdent, UnOp,
};

pub type Expr<'ctx> = Box<'ctx, TypedExprKind<'ctx>>;
pub type TypedExprKind<'ctx> = Typed<'ctx, ExprKind<'ctx>>;

pub struct Program<'ctx> {
    pub functions: IndexVec<FnIndex, FunctionDef<'ctx>>,
}

/// Function definition.
///
/// A function may be defined as a closure
/// so it may capture variables from the environment.
pub struct FunctionDef<'ctx> {
    pub name: FnName<'ctx>,

    /// Arguments passed to the function.
    pub args: IndexVec<ArgIndex, Ident<'ctx>>,

    /// Arguments captured by the closure.
    ///
    /// They are not passed as arguments to the function but
    /// are required to evaluate the function body.
    pub args_via_closure: IndexVec<ArgIndex, Ident<'ctx>>,

    pub body: Expr<'ctx>,
}

impl<'ctx> FunctionDef<'ctx> {
    pub fn ret_ty(&self) -> Ty<'ctx> {
        self.body.ty
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Name of a function (or a closure).
pub struct FnName<'ctx>(Option<Ident<'ctx>>);

impl Indexable<FnIndex> for FnName<'_> {}

impl std::fmt::Display for FnName<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            Some(ident) => write!(f, "{}", ident.value),
            None => write!(f, "main"),
        }
    }
}

impl<'ctx> FnName<'ctx> {
    pub fn new(ident: Ident<'ctx>) -> Self {
        Self(Some(ident))
    }

    pub fn as_non_main(&self) -> Ident<'ctx> {
        self.0.unwrap()
    }

    pub const MAIN_FN_NAME: Self = Self(None);
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct FnIndex(usize);
impl Idx for FnIndex {
    fn new(idx: usize) -> Self {
        Self(idx)
    }

    fn index(self) -> usize {
        self.0
    }
}

impl Indexable<FnIndex> for FunctionDef<'_> {}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExprKind<'ctx> {
    Const(LitKind),
    Unary(UnOp, Ident<'ctx>),
    Binary(BinOp, Ident<'ctx>, Ident<'ctx>),
    If(Ident<'ctx>, Expr<'ctx>, Expr<'ctx>),
    Let(LetBinding<'ctx>, Expr<'ctx>),
    Var(Ident<'ctx>),

    /// Create a closure which captures the given arguments.
    ///
    /// The original `make_closure` has been splitted into this and `Let`.
    ClosureMake(Closure<'ctx>),

    App(ApplyKind<'ctx>, IndexVec<ArgIndex, Ident<'ctx>>),
    Tuple(IndexVec<TupleIndex, Ident<'ctx>>),
    ArrayMake(Ident<'ctx>, Ident<'ctx>),
    Get(Ident<'ctx>, Ident<'ctx>),
    Set(Ident<'ctx>, Ident<'ctx>, Ident<'ctx>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Closure<'ctx> {
    pub function: FunctionInstance<'ctx>,
    pub captured_args: IndexVec<ArgIndex, Ident<'ctx>>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ApplyKind<'ctx> {
    Direct {
        /// The function to apply.
        function: FunctionInstance<'ctx>,
    },
    Closure {
        /// The closure to apply.
        ident: Ident<'ctx>,
    },
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum FunctionInstance<'ctx> {
    /// A function defined in the program.
    Defined(FnIndex),

    /// A function imported from another module, such as `pervasives`.
    Imported(FnName<'ctx>),
}

impl<'ctx> ExprKind<'ctx> {
    pub fn kind(&self) -> &Self {
        self
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct LetBinding<'ctx> {
    pub pattern: Pattern<'ctx>,
    pub value: Expr<'ctx>,
}
