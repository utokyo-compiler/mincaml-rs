use std::num::NonZeroU32;

use rustc_hash::FxHashMap;
use ty::{context::TypingContext, Ty, TyKind, TyVarId};

#[derive(Debug)]
pub enum Error<'ctx> {
    /// Cannot unify these two types.
    UnifyFailed(Ty<'ctx>, Ty<'ctx>),
    /// The type variable occurs in the type.
    OccurckFailed(TyVarId, Ty<'ctx>),
    /// Unbound identifier.
    UnboundIdent(syntax::Ident<'ctx>),
}

/// The main entry point for type checking.
///
/// Originally named `Typing.f`.
pub fn typeck<'ctx>(
    _ctx: &'ctx TypingContext<'ctx>,
    _parsed: syntax::Expr<'ctx>,
) -> Result<ir_typed_ast::Expr<'ctx>, Error<'ctx>> {
    todo!()
}

/// Recursively infer the type of the given expression.
///
/// Originally named `Typing.g` (or `typing_g`).
fn type_of<'ctx>(
    ctx: &'ctx TypingContext<'ctx>,
    env: &mut FxHashMap<syntax::Ident<'ctx>, Ty<'ctx>>,
    subst: &mut TyVarSubst<'ctx>,
    expr: syntax::Expr<'ctx>,
) -> Result<Ty<'ctx>, Error<'ctx>> {
    match expr.kind() {
        syntax::ExprKind::Const(lit) => {
            let ty_kind = match lit {
                syntax::LitKind::Int(..) => TyKind::Int,
                syntax::LitKind::Float(..) => TyKind::Float,
                syntax::LitKind::Bool(..) => TyKind::Bool,
                syntax::LitKind::Unit => TyKind::Unit,
            };
            Ok(ctx.mk_ty_from_kind(ty_kind))
        }
        syntax::ExprKind::Unary(un_op, e) => {
            let ty = type_of(ctx, env, subst, *e)?;
            match un_op {
                syntax::UnOp::Neg => {
                    unify(subst, ty, Ty::mk_int(ctx))?;
                    Ok(Ty::mk_int(ctx))
                }
                syntax::UnOp::FNeg => {
                    unify(subst, ty, Ty::mk_float(ctx))?;
                    Ok(Ty::mk_float(ctx))
                }
                syntax::UnOp::Not => {
                    unify(subst, ty, Ty::mk_bool(ctx))?;
                    Ok(Ty::mk_bool(ctx))
                }
            }
        }
        syntax::ExprKind::Binary(bin_op, e1, e2) => {
            let ty1 = type_of(ctx, env, subst, *e1)?;
            let ty2 = type_of(ctx, env, subst, *e2)?;
            match bin_op {
                syntax::BinOp::BBinOp(..) => {
                    unify(subst, ty1, ty2)?;
                    Ok(Ty::mk_bool(ctx))
                }
                syntax::BinOp::IBinOp(..) => {
                    unify(subst, ty1, Ty::mk_int(ctx))?;
                    unify(subst, ty2, Ty::mk_int(ctx))?;
                    Ok(Ty::mk_int(ctx))
                }
                syntax::BinOp::FBinOp(..) => {
                    unify(subst, ty1, Ty::mk_float(ctx))?;
                    unify(subst, ty2, Ty::mk_float(ctx))?;
                    Ok(Ty::mk_float(ctx))
                }
            }
        }
        syntax::ExprKind::If(e1, e2, e3) => {
            let ty1 = type_of(ctx, env, subst, *e1)?;
            let ty2 = type_of(ctx, env, subst, *e2)?;
            let ty3 = type_of(ctx, env, subst, *e3)?;
            unify(subst, ty1, Ty::mk_bool(ctx))?;
            unify(subst, ty2, ty3)?;
            Ok(ty2)
        }
        syntax::ExprKind::Let(let_kind) => match let_kind {
            syntax::LetKind::LetVar(x, e1, e2) => {
                let ty1 = type_of(ctx, &mut env.clone(), subst, *e1)?;
                env.insert(*x, ty1);
                type_of(ctx, env, subst, *e2)
            }
            syntax::LetKind::LetRec(syntax::FunDef { name, args, body }, e2) => {
                for arg in args {
                    let arg_ty = Ty::mk_ty_var(ctx);
                    env.insert(*arg, arg_ty);
                }
                let body_ty = type_of(ctx, env, subst, *body)?;
                let name_ty = Ty::mk_ty_var(ctx);
                todo!()
            }
            syntax::LetKind::LetTuple(_, _, _) => todo!(),
        },
        syntax::ExprKind::Then(e1, e2) => {
            let ty1 = type_of(ctx, env, subst, *e1)?;
            let ty2 = type_of(ctx, env, subst, *e2)?;
            unify(subst, ty1, Ty::mk_unit(ctx))?;
            Ok(ty2)
        }
        syntax::ExprKind::Var(_) => todo!(),
        syntax::ExprKind::App(_, _) => todo!(),
        syntax::ExprKind::Tuple(_) => todo!(),
        syntax::ExprKind::ArrayMake(_, _) => todo!(),
        syntax::ExprKind::Get(_, _) => todo!(),
        syntax::ExprKind::Set(_, _, _) => todo!(),
    }
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
    env: FxHashMap<TyVarId, Ty<'ctx>>,
}

impl<'ctx> TyVarSubst<'ctx> {
    fn get(&self, var: TyVarId) -> Option<Ty<'ctx>> {
        self.env.get(&var).copied()
    }
}
