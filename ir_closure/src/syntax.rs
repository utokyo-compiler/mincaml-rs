//! Syntax tree after the closure conversion.
//!
//! You are encouraged to implement several optimization passes
//! on this IR, or merge this IR into upper-level one by adding
//! `ClosureMake`, `Function`, `Program` to it.
use data_structure::{arena::Box, index::vec::IndexVec};

pub use ir_knorm::{
    ArgIndex, BinOp, DisambiguatedIdent, Ident, LitKind, Pattern, TupleIndex, Ty, Typed,
    TypedIdent, UnOp,
};

use crate::Context;

pub type Expr<'ctx> = Box<'ctx, TypedExprKind<'ctx>>;
pub type TypedExprKind<'ctx> = Typed<'ctx, ExprKind<'ctx>>;

pub struct Program<'ctx> {
    pub functions: Vec<Function<'ctx>>,
    pub main: Function<'ctx>,
}

/// Function definition.
///
/// A function may be defined as a closure
/// so it may capture variables from the environment.
pub struct Function<'ctx> {
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

impl<'ctx> Function<'ctx> {
    pub fn ret_ty(&self) -> Ty<'ctx> {
        self.body.ty
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Name of a function (or a closure).
///
/// This type is different from `Ident`, but `ExprKind` can have
/// their value as an `Ident`.
pub struct FnName<'ctx>(Ident<'ctx>);

impl<'ctx> FnName<'ctx> {
    /// Create a new function name without checking
    /// whether this is a valid function name.
    pub fn new_unchecked(ctx: &'ctx Context<'ctx>, ident: TypedIdent<'ctx>) -> Self {
        Self(ctx.new_ident(ident))
    }

    pub fn new(ident: Ident<'ctx>) -> Self {
        Self(ident)
    }

    pub fn main_fn_name(ctx: &'ctx Context<'ctx>, ty: Ty<'ctx>) -> Self {
        // safety: "main" does not get referenced in the program.
        Self::new_unchecked(
            ctx,
            Typed::new(DisambiguatedIdent::new_compiler_unchecked("main", 0), ty),
        )
    }
}

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

    App(ApplyKind, FnName<'ctx>, IndexVec<ArgIndex, Ident<'ctx>>),
    Tuple(IndexVec<TupleIndex, Ident<'ctx>>),
    ArrayMake(Ident<'ctx>, Ident<'ctx>),
    Get(Ident<'ctx>, Ident<'ctx>),
    Set(Ident<'ctx>, Ident<'ctx>, Ident<'ctx>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Closure<'ctx> {
    pub fn_name: FnName<'ctx>,
    pub captured_args: IndexVec<ArgIndex, Ident<'ctx>>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ApplyKind {
    Direct,
    Closure,
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
