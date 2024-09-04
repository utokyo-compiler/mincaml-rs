pub mod context;

use std::fmt::Debug;

use context::TypingContext;
use data_structure::interning::Interned;

/// Use this instead of `TyKind` whenever possible.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty<'ctx>(pub Interned<'ctx, TyKind<'ctx>>);

type Tys<'ctx> = Vec<Ty<'ctx>>;
type TyVar = TyVarId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind<'ctx> {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Tys<'ctx>, Ty<'ctx>),
    Tuple(Tys<'ctx>),
    Array(Ty<'ctx>),
    TyVar(TyVar),
}

impl<'ctx> TyKind<'ctx> {
    pub fn as_ty_var(&self) -> Option<TyVar> {
        if let Self::TyVar(v) = self {
            Some(*v)
        } else {
            None
        }
    }
}

/// used in type checking.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVarId(usize);

impl TyVarId {
    pub fn new_unchecked(id: usize) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone)]
pub struct Typed<'ctx, T> {
    pub node: T,
    pub ty: Ty<'ctx>,
}

impl<'ctx> Ty<'ctx> {
    pub fn new(ctx: &TypingContext<'ctx>, kind: TyKind<'ctx>) -> Self {
        ctx.mk_ty_from_kind(kind)
    }

    pub fn kind(self) -> &'ctx TyKind<'ctx> {
        self.0 .0
    }
}
