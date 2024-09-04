use std::collections::HashMap;

use ty::{context::TypingContext, Ty, TyKind, TyVarId};

#[derive(Debug)]
pub enum Error<'ctx> {
    UnifyFailed(Ty<'ctx>, Ty<'ctx>),
    OccurckFailed(TyVarId, Ty<'ctx>),
}

pub fn typeck<'input, 'ctx>(
    _parsed: syntax::Expr<'ctx>,
    _ctx: &'ctx TypingContext<'ctx>,
) -> Result<ir_typed_ast::Expr<'input, 'ctx>, Error<'ctx>> {
    todo!()
}

fn type_of<'input, 'ctx>(
    subst: &mut TyVarSubst<'ctx>,
    expr: syntax::Expr<'ctx>,
) -> Result<ir_typed_ast::Expr<'input, 'ctx>, Error<'ctx>> {
    todo!()
}

fn unify<'ctx>(
    subst: &mut TyVarSubst<'ctx>,
    lhs: Ty<'ctx>,
    rhs: Ty<'ctx>,
) -> Result<(), Error<'ctx>> {
    // N.B. This comparison covers the case
    // where both `lhs` and `rhs` are the same type variable
    // so as not to cause an error in `occurck`.
    if lhs == rhs {
        return Ok(());
    }
    match (lhs.kind(), rhs.kind()) {
        (TyKind::TyVar(v), _) => {
            if let Some(ty) = subst.get(*v) {
                return unify(subst, ty, rhs);
            }
            occurck(subst, *v, rhs)?;
            subst.env.insert(*v, rhs);
            Ok(())
        }
        (_, TyKind::TyVar(v)) => {
            if let Some(ty) = subst.get(*v) {
                return unify(subst, lhs, ty);
            }
            occurck(subst, *v, lhs)?;
            subst.env.insert(*v, lhs);
            Ok(())
        }
        (TyKind::Fun(lhs_args, lhs_ret), TyKind::Fun(rhs_args, rhs_ret)) => {
            if lhs_args.len() != rhs_args.len() {
                return Err(Error::UnifyFailed(lhs, rhs));
            }
            for (lhs_arg, rhs_arg) in lhs_args.iter().zip(rhs_args.iter()) {
                unify(subst, *lhs_arg, *rhs_arg)?;
            }
            unify(subst, *lhs_ret, *rhs_ret)
        }
        (TyKind::Tuple(lhs_tys), TyKind::Tuple(rhs_tys)) => {
            if lhs_tys.len() != rhs_tys.len() {
                return Err(Error::UnifyFailed(lhs, rhs));
            }
            for (lhs, rhs) in lhs_tys.iter().zip(rhs_tys) {
                unify(subst, *lhs, *rhs)?;
            }
            Ok(())
        }
        (TyKind::Array(lhs), TyKind::Array(rhs)) => unify(subst, *lhs, *rhs),
        _ => Err(Error::UnifyFailed(lhs, rhs)),
    }
}

/// Check if the type variable `var` occurs in the type `ty`.
fn occurck<'ctx>(subst: &TyVarSubst<'ctx>, var: TyVarId, ty: Ty<'ctx>) -> Result<(), Error<'ctx>> {
    match ty.kind() {
        TyKind::Fun(args, ret) => {
            for arg in args {
                occurck(subst, var, *arg)?;
            }
            occurck(subst, var, *ret)
        }
        TyKind::Tuple(tys) => {
            for ty in tys {
                occurck(subst, var, *ty)?;
            }
            Ok(())
        }
        TyKind::Array(ty) => occurck(subst, var, *ty),
        TyKind::TyVar(v) => {
            if var == *v {
                Err(Error::OccurckFailed(var, ty))
            } else if let Some(ty) = subst.get(*v) {
                occurck(subst, var, ty)
            } else {
                Ok(())
            }
        }
        _ => Ok(()),
    }
}

struct TyVarSubst<'ctx> {
    ctx: &'ctx TypingContext<'ctx>,
    env: HashMap<TyVarId, Ty<'ctx>>,
}

impl<'ctx> TyVarSubst<'ctx> {
    fn new(ctx: &'ctx TypingContext<'ctx>) -> Self {
        Self {
            ctx,
            env: Default::default(),
        }
    }

    fn get(&self, var: TyVarId) -> Option<Ty<'ctx>> {
        self.env.get(&var).copied()
    }
}
