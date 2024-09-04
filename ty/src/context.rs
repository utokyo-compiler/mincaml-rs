use std::sync::atomic::AtomicUsize;

use data_structure::{
    arena::TypedArena,
    interning::{HashSetInterner, Interned},
};

use crate::{Ty, TyKind, TyVarId};

pub struct TypingContext<'ctx> {
    ty_arena: &'ctx TypedArena<TyKind<'ctx>>,
    ty_interner: HashSetInterner<&'ctx TyKind<'ctx>>,
    fresh_ty_var_id: AtomicUsize,
}

impl<'ctx> TypingContext<'ctx> {
    pub fn new(ty_arena: &'ctx TypedArena<TyKind<'ctx>>) -> Self {
        Self {
            ty_arena,
            ty_interner: Default::default(),
            fresh_ty_var_id: Default::default(),
        }
    }

    pub fn mk_ty_from_kind(&self, kind: TyKind<'ctx>) -> Ty<'ctx> {
        Ty(Interned::new_unchecked(
            self.ty_interner
                .intern(kind, |kind| self.ty_arena.alloc(kind)),
        ))
    }

    pub fn fresh_ty_var(&self) -> TyVarId {
        let id = self
            .fresh_ty_var_id
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        TyVarId::new_unchecked(id)
    }
}

/// Common, pre-interned types.
pub struct CommonTypes<'ctx> {
    pub unit: Ty<'ctx>,
    pub bool: Ty<'ctx>,
    pub int: Ty<'ctx>,
    pub float: Ty<'ctx>,
}

impl<'ctx> CommonTypes<'ctx> {
    pub fn new(ctx: &TypingContext<'ctx>) -> Self {
        Self {
            unit: ctx.mk_ty_from_kind(TyKind::Unit),
            bool: ctx.mk_ty_from_kind(TyKind::Bool),
            int: ctx.mk_ty_from_kind(TyKind::Int),
            float: ctx.mk_ty_from_kind(TyKind::Float),
        }
    }
}
