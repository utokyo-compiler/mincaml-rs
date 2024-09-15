use std::sync::atomic::AtomicUsize;

use data_structure::{
    arena::{Box, TypedArena},
    interning::{HashSetInterner, Interned},
};

use crate::{Ty, TyKind, TyVarId};

/// The context for type checking.
///
/// `Expr` is just a placeholder for the typed expression type.
pub struct TypingContext<'ctx, Expr> {
    ty_arena: &'ctx TypedArena<TyKind<'ctx>>,
    typed_expr_arena: &'ctx TypedArena<Expr>,
    ty_interner: HashSetInterner<&'ctx TyKind<'ctx>>,
    fresh_ty_var_id: AtomicUsize,
}

impl<'ctx, Expr> TypingContext<'ctx, Expr> {
    pub fn new(
        ty_arena: &'ctx TypedArena<TyKind<'ctx>>,
        typed_expr_arena: &'ctx TypedArena<Expr>,
    ) -> Self {
        Self {
            ty_arena,
            typed_expr_arena,
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

    pub fn new_expr(&self, expr: Expr) -> Box<'ctx, Expr> {
        self.typed_expr_arena.alloc_boxed(expr)
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
    pub fn new<Expr>(ctx: &TypingContext<'ctx, Expr>) -> Self {
        Self {
            unit: ctx.mk_ty_from_kind(TyKind::Unit),
            bool: ctx.mk_ty_from_kind(TyKind::Bool),
            int: ctx.mk_ty_from_kind(TyKind::Int),
            float: ctx.mk_ty_from_kind(TyKind::Float),
        }
    }
}
