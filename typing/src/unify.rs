use std::borrow::Cow;

use ty::{Ty, TyKind, TyVarId};

use crate::{error::UnifyError, ty_var_subst};

/// Unify two types, `lhs` and `rhs`, using the given substitution environment `subst`.
///
/// This function will attempt to unify the two types and update the substitution environment accordingly.
/// If the unification fails, it will return an error.
pub fn unify<'ctx>(
    subst: &mut Cow<ty_var_subst::Env<'ctx>>,
    lhs: Ty<'ctx>,
    rhs: Ty<'ctx>,
) -> Result<(), UnifyError<'ctx>> {
    // N.B., This comparison covers the case
    // where both `lhs` and `rhs` are the same type variable
    // so as not to cause an error in `occurck`.
    if lhs == rhs {
        return Ok(());
    }
    match (lhs.kind(), rhs.kind()) {
        (TyKind::TyVar(v), _) if let Some(ty) = subst.get(*v) => unify(subst, ty, rhs),
        (_, TyKind::TyVar(v)) if let Some(ty) = subst.get(*v) => unify(subst, lhs, ty),
        (TyKind::TyVar(v), _) => {
            occurck(subst, *v, rhs)?;
            subst.to_mut().merge(*v, rhs);
            Ok(())
        }
        (_, TyKind::TyVar(v)) => {
            occurck(subst, *v, lhs)?;
            subst.to_mut().merge(*v, lhs);
            Ok(())
        }
        (TyKind::Fun(lhs_args, lhs_ret), TyKind::Fun(rhs_args, rhs_ret)) => {
            if lhs_args.len() != rhs_args.len() {
                return Err(UnifyError::FunArgLenMismatch {
                    left: lhs_args.len(),
                    right: rhs_args.len(),
                });
            }
            for (lhs_arg, rhs_arg) in lhs_args.iter().zip(rhs_args) {
                unify(subst, *lhs_arg, *rhs_arg).unwrap();
            }
            unify(subst, *lhs_ret, *rhs_ret)
        }
        (TyKind::Tuple(lhs_tys), TyKind::Tuple(rhs_tys)) => {
            if lhs_tys.len() != rhs_tys.len() {
                return Err(UnifyError::TupleLenMismatch);
            }
            let mut errors = Vec::new();
            for (lhs, rhs) in lhs_tys.iter().zip(rhs_tys) {
                let result = unify(subst, *lhs, *rhs);
                if let Err(err) = result {
                    errors.push(err);
                }
            }
            if !errors.is_empty() {
                return Err(UnifyError::TupleElemsMismatch { mismatches: errors });
            }
            Ok(())
        }
        (TyKind::Array(lhs_ty), TyKind::Array(rhs_ty)) => unify(subst, *lhs_ty, *rhs_ty),
        _ => Err(UnifyError::DifferentDiscriminants),
    }
}

/// Check if the type variable `var` occurs in the type `ty`.
fn occurck<'ctx>(
    subst: &ty_var_subst::Env<'ctx>,
    var: TyVarId,
    arg: Ty<'ctx>,
) -> Result<(), UnifyError<'ctx>> {
    match arg.kind() {
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
                Err(UnifyError::OccurckFailed { var, ty: arg })
            } else if let Some(ty) = subst.get(*v) {
                occurck(subst, var, ty)
            } else {
                Ok(())
            }
        }
        _ => Ok(()),
    }
}
