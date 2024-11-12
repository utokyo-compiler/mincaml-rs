//! TyVar substitution
//!
//! The difference from the original is that the original implementation stores
//! type-variable information in `Type.t`, this implementation manages it via `Env`.

use data_structure::FxHashMap;
use ty::{Ty, TyVarId};

use crate::Context;

#[derive(Clone)]
pub struct Env<'ctx> {
    inner: FxHashMap<TyVarId, Ty<'ctx>>,
}

impl<'ctx> Env<'ctx> {
    pub fn new() -> Self {
        Self {
            inner: Default::default(),
        }
    }
    pub fn get(&self, var: TyVarId) -> Option<Ty<'ctx>> {
        self.inner.get(&var).copied()
    }

    pub fn merge(&mut self, var: TyVarId, ty: Ty<'ctx>) {
        self.inner.insert(var, ty);
    }

    pub fn deref_ty_var(self, ctx: &'ctx Context<'ctx>, e: &mut ir_typed_ast::Expr<'ctx>) {
        use ir_typed_ast::MutVisitor;
        let mut visitor = DerefTyVisitor { env: self, ctx };
        visitor.visit_expr(e);
    }
}

struct DerefTyVisitor<'ctx> {
    env: Env<'ctx>,
    ctx: &'ctx Context<'ctx>,
}

impl<'ctx> ir_typed_ast::MutVisitor<'ctx> for DerefTyVisitor<'ctx> {
    fn visit_ty(&mut self, ty: &mut Ty<'ctx>) {
        *ty = self.deref_ty(*ty);
    }
}

impl<'ctx> DerefTyVisitor<'ctx> {
    fn deref_ty(&self, mut ty: Ty<'ctx>) -> Ty<'ctx> {
        loop {
            match ty.kind() {
                ty::TyKind::TyVar(var) => {
                    if let Some(new_ty) = self.env.get(*var) {
                        ty = new_ty;
                    } else {
                        return ty;
                    }
                }
                ty::TyKind::Fun(index_vec, ty) => {
                    return Ty::mk_fun(
                        self.ctx,
                        index_vec.iter().map(|ty| self.deref_ty(*ty)).collect(),
                        self.deref_ty(*ty),
                    );
                }
                ty::TyKind::Tuple(index_vec) => {
                    return Ty::mk_tuple(
                        self.ctx,
                        index_vec.iter().map(|ty| self.deref_ty(*ty)).collect(),
                    );
                }
                ty::TyKind::Array(ty) => {
                    return Ty::mk_array(self.ctx, self.deref_ty(*ty));
                }
                ty::TyKind::Unit | ty::TyKind::Bool | ty::TyKind::Int | ty::TyKind::Float => {
                    return ty;
                }
            }
        }
    }
}
