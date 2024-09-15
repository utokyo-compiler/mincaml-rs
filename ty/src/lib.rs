pub mod context;

use std::fmt::Debug;

use context::TypingContext;
use data_structure::interning::Interned;

/// Use this instead of `TyKind` whenever possible.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty<'ctx>(pub Interned<'ctx, TyKind<'ctx>>);

type Tys<'ctx> = Vec<Ty<'ctx>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind<'ctx> {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Tys<'ctx>, Ty<'ctx>),
    Tuple(Tys<'ctx>),
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
    pub fn new<Expr>(ctx: &TypingContext<'ctx, Expr>, kind: TyKind<'ctx>) -> Self {
        ctx.mk_ty_from_kind(kind)
    }

    pub fn kind(self) -> &'ctx TyKind<'ctx> {
        self.0 .0
    }

    #[inline(always)]
    pub fn mk_unit<Expr>(ctx: &TypingContext<'ctx, Expr>) -> Self {
        Self::new(ctx, TyKind::Unit)
    }

    #[inline(always)]
    pub fn mk_bool<Expr>(ctx: &TypingContext<'ctx, Expr>) -> Self {
        Self::new(ctx, TyKind::Bool)
    }

    #[inline(always)]
    pub fn mk_int<Expr>(ctx: &TypingContext<'ctx, Expr>) -> Self {
        Self::new(ctx, TyKind::Int)
    }

    #[inline(always)]
    pub fn mk_float<Expr>(ctx: &TypingContext<'ctx, Expr>) -> Self {
        Self::new(ctx, TyKind::Float)
    }

    #[inline(always)]
    pub fn mk_fun<Expr>(ctx: &TypingContext<'ctx, Expr>, args: Tys<'ctx>, ret: Ty<'ctx>) -> Self {
        Self::new(ctx, TyKind::Fun(args, ret))
    }

    #[inline(always)]
    pub fn mk_tuple<Expr>(ctx: &TypingContext<'ctx, Expr>, tys: Tys<'ctx>) -> Self {
        Self::new(ctx, TyKind::Tuple(tys))
    }

    #[inline(always)]
    pub fn mk_array<Expr>(ctx: &TypingContext<'ctx, Expr>, ty: Ty<'ctx>) -> Self {
        Self::new(ctx, TyKind::Array(ty))
    }

    #[inline(always)]
    pub fn mk_ty_var<Expr>(ctx: &TypingContext<'ctx, Expr>) -> Self {
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
