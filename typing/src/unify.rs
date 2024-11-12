use std::borrow::Cow;

use errors::DiagContext;
use sourcemap::Span;
use ty::{Ty, TyKind, TyVarId};

use crate::{
    error::{self, UnifyError, UnifyFailKind},
    ty_var_subst,
};

pub trait UnifyArg<'ctx>: Copy {
    fn span(self) -> Option<Span> {
        None
    }
    fn ty(self) -> Ty<'ctx>;
}

impl<'ctx> UnifyArg<'ctx> for Ty<'ctx> {
    fn ty(self) -> Ty<'ctx> {
        self
    }
}

#[derive(Debug, Clone, Copy)]
/// Optionally spanned [`Ty`].
pub struct SpannedTy<'ctx> {
    ty: Ty<'ctx>,
    span: Option<Span>,
}

impl<'ctx> SpannedTy<'ctx> {
    pub fn new(arg: impl UnifyArg<'ctx>) -> Self {
        Self {
            ty: arg.ty(),
            span: arg.span(),
        }
    }

    /// Create a new [`SpannedTy`] with the given [`Ty`] and span.
    fn override_ty(self, ty: Ty<'ctx>) -> Self {
        Self { ty, ..self }
    }
}

impl<'ctx> UnifyArg<'ctx> for SpannedTy<'ctx> {
    fn span(self) -> Option<Span> {
        self.span
    }
    fn ty(self) -> Ty<'ctx> {
        self.ty
    }
}

impl<'a, 'ctx> UnifyArg<'ctx> for &'a ir_typed_ast::Expr<'ctx> {
    fn span(self) -> Option<Span> {
        Some(*self.span.as_user_defined().unwrap())
    }
    fn ty(self) -> Ty<'ctx> {
        self.ty
    }
}

/// Unify two types, `lhs` and `rhs`, using the given substitution environment `subst`.
///
/// This function will attempt to unify the two types and update the substitution environment accordingly.
/// If the unification fails, it will return an error.
pub fn unify<'ctx>(
    dcx: &'ctx DiagContext<'ctx>,
    subst: &mut Cow<ty_var_subst::Env<'ctx>>,
    lhs: impl UnifyArg<'ctx>,
    rhs: impl UnifyArg<'ctx>,
) -> Result<(), UnifyError<'ctx>> {
    let lhs = SpannedTy::new(lhs);
    let rhs = SpannedTy::new(rhs);
    // N.B., This comparison covers the case
    // where both `lhs` and `rhs` are the same type variable
    // so as not to cause an error in `occurck`.
    if lhs.ty == rhs.ty {
        return Ok(());
    }
    match (lhs.ty.kind(), rhs.ty.kind()) {
        (TyKind::TyVar(v), _) if let Some(ty) = subst.get(*v) => {
            unify(dcx, subst, lhs.override_ty(ty), rhs)
        }
        (_, TyKind::TyVar(v)) if let Some(ty) = subst.get(*v) => {
            unify(dcx, subst, lhs, rhs.override_ty(ty))
        }
        (TyKind::TyVar(v), _) => {
            occurck(dcx, subst, *v, rhs)?;
            subst.to_mut().merge(*v, rhs.ty);
            Ok(())
        }
        (_, TyKind::TyVar(v)) => {
            occurck(dcx, subst, *v, lhs)?;
            subst.to_mut().merge(*v, lhs.ty);
            Ok(())
        }
        (TyKind::Fun(lhs_args, lhs_ret), TyKind::Fun(rhs_args, rhs_ret)) => {
            if lhs_args.len() != rhs_args.len() {
                return Err(UnifyError::failed(
                    lhs,
                    rhs,
                    UnifyFailKind::FunArgLenMismatch {
                        left: lhs_args.len(),
                        right: rhs_args.len(),
                    },
                ));
            }
            for (lhs_arg, rhs_arg) in lhs_args.iter().zip(rhs_args) {
                unify(dcx, subst, *lhs_arg, *rhs_arg).unwrap();
            }
            unify(dcx, subst, *lhs_ret, *rhs_ret)
        }
        (TyKind::Tuple(lhs_tys), TyKind::Tuple(rhs_tys)) => {
            if lhs_tys.len() != rhs_tys.len() {
                return Err(UnifyError::failed(
                    lhs,
                    rhs,
                    UnifyFailKind::TupleLenMismatch,
                ));
            }
            let mut is_error = false;
            for (lhs, rhs) in lhs_tys.iter().zip(rhs_tys) {
                let result = unify(dcx, subst, *lhs, *rhs);
                if result.is_err() {
                    is_error = true;
                }
            }
            if is_error {
                return Err(UnifyError::failed(
                    lhs,
                    rhs,
                    UnifyFailKind::TupleElemsMismatch {
                        mismatched: lhs_tys
                            .iter()
                            .zip(rhs_tys)
                            .filter_map(
                                |(lhs, rhs)| {
                                    if lhs != rhs {
                                        Some((*lhs, *rhs))
                                    } else {
                                        None
                                    }
                                },
                            )
                            .collect(),
                    },
                ));
            }
            Ok(())
        }
        (TyKind::Array(lhs_ty), TyKind::Array(rhs_ty)) => unify(
            dcx,
            subst,
            lhs.override_ty(*lhs_ty),
            rhs.override_ty(*rhs_ty),
        ),
        _ => Err(UnifyError::failed(
            lhs,
            rhs,
            UnifyFailKind::DifferentDiscriminants,
        )),
    }
}

/// Check if the type variable `var` occurs in the type `ty`.
fn occurck<'ctx>(
    dcx: &'ctx DiagContext<'ctx>,
    subst: &ty_var_subst::Env<'ctx>,
    var: TyVarId,
    arg: impl UnifyArg<'ctx>,
) -> Result<(), UnifyError<'ctx>> {
    let ty = arg.ty();
    match ty.kind() {
        TyKind::Fun(args, ret) => {
            for arg in args {
                occurck(dcx, subst, var, *arg)?;
            }
            occurck(dcx, subst, var, *ret)
        }
        TyKind::Tuple(tys) => {
            for ty in tys {
                occurck(dcx, subst, var, *ty)?;
            }
            Ok(())
        }
        TyKind::Array(ty) => occurck(dcx, subst, var, *ty),
        TyKind::TyVar(v) => {
            if var == *v {
                Err(UnifyError::report(
                    dcx,
                    error::OccurckFailed {
                        span: arg.span(),
                        var,
                        ty,
                        error: (),
                    },
                ))
            } else if let Some(ty) = subst.get(*v) {
                occurck(dcx, subst, var, ty)
            } else {
                Ok(())
            }
        }
        _ => Ok(()),
    }
}
