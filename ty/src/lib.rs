pub mod context;

use std::fmt::Debug;

use context::TypingContext;
use data_structure::{
    index::{
        vec::{Idx, IndexVec},
        Indexable,
    },
    interning::Interned,
};

/// Use this instead of `TyKind` whenever possible.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty<'ctx>(pub Interned<'ctx, TyKind<'ctx>>);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct ArgIndex(usize);
impl Idx for ArgIndex {
    fn new(idx: usize) -> Self {
        Self(idx)
    }

    fn index(self) -> usize {
        self.0
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TupleIndex(usize);
impl Idx for TupleIndex {
    fn new(idx: usize) -> Self {
        Self(idx)
    }

    fn index(self) -> usize {
        self.0
    }
}

impl Indexable<ArgIndex> for Ty<'_> {}
impl Indexable<TupleIndex> for Ty<'_> {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind<'ctx> {
    Unit,
    Bool,
    Int,
    Float,
    Fun(IndexVec<ArgIndex, Ty<'ctx>>, Ty<'ctx>),
    Tuple(IndexVec<TupleIndex, Ty<'ctx>>),
    Array(Ty<'ctx>),

    /// A type variable.
    ///
    /// Used in type checking, not allowed after that.
    TyVar(TyVarId),
}

impl<'ctx> TyKind<'ctx> {
    pub fn as_ty_var(&self) -> Option<TyVarId> {
        if let Self::TyVar(v) = self {
            Some(*v)
        } else {
            None
        }
    }
}

/// Used in type checking.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVarId(usize);

impl TyVarId {
    pub fn new_unchecked(id: usize) -> Self {
        Self(id)
    }
}

impl<'ctx> Ty<'ctx> {
    pub fn new<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>, kind: TyKind<'ctx>) -> Self {
        ctx.mk_ty_from_kind(kind)
    }

    pub fn kind(self) -> &'ctx TyKind<'ctx> {
        self.0 .0
    }

    pub fn is_unit(self) -> bool {
        matches!(self.kind(), TyKind::Unit)
    }

    #[inline(always)]
    pub fn mk_unit<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>) -> Self {
        Self::new(ctx, TyKind::Unit)
    }

    #[inline(always)]
    pub fn mk_bool<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>) -> Self {
        Self::new(ctx, TyKind::Bool)
    }

    #[inline(always)]
    pub fn mk_int<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>) -> Self {
        Self::new(ctx, TyKind::Int)
    }

    #[inline(always)]
    pub fn mk_float<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>) -> Self {
        Self::new(ctx, TyKind::Float)
    }

    #[inline(always)]
    pub fn mk_fun<Ident, Expr>(
        ctx: &TypingContext<'ctx, Ident, Expr>,
        args: Vec<Ty<'ctx>>,
        ret: Ty<'ctx>,
    ) -> Self {
        Self::new(ctx, TyKind::Fun(IndexVec::from_raw_vec(args), ret))
    }

    #[inline(always)]
    pub fn mk_tuple<Ident, Expr>(
        ctx: &TypingContext<'ctx, Ident, Expr>,
        tys: Vec<Ty<'ctx>>,
    ) -> Self {
        Self::new(ctx, TyKind::Tuple(IndexVec::from_raw_vec(tys)))
    }

    #[inline(always)]
    pub fn mk_array<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>, ty: Ty<'ctx>) -> Self {
        Self::new(ctx, TyKind::Array(ty))
    }

    #[inline(always)]
    pub fn mk_ty_var<Ident, Expr>(ctx: &TypingContext<'ctx, Ident, Expr>) -> Self {
        Self::new(ctx, TyKind::TyVar(ctx.fresh_ty_var()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Typed<'ctx, T> {
    pub value: T,
    pub ty: Ty<'ctx>,
}

impl<'ctx, T> Typed<'ctx, T> {
    pub fn new(value: T, ty: Ty<'ctx>) -> Self {
        Self { value, ty }
    }
}

impl<'ctx, T> std::ops::Deref for Typed<'ctx, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<'ctx, T> std::ops::DerefMut for Typed<'ctx, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
