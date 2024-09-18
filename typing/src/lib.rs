use sourcemap::Spanned;
use ty::{context::CommonTypes, Ty, TyKind, TyVarId, Typed};

mod name_res;
mod ty_var_subst;

pub type TypingContext<'ctx> = ty::context::TypingContext<
    'ctx,
    Typed<'ctx, ir_typed_ast::DisambiguatedIdent<'ctx>>,
    Typed<'ctx, Spanned<ir_typed_ast::ExprKind<'ctx>>>,
>;

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
    ctx: &'ctx TypingContext<'ctx>,
    common_types: &CommonTypes<'ctx>,
    parsed: syntax::Expr<'ctx>,
) -> Result<ir_typed_ast::Expr<'ctx>, Error<'ctx>> {
    let mut name_res = name_res::Env::new();
    let mut subst = ty_var_subst::Env::new();
    let mut typed = decide_ty(ctx, common_types, &mut name_res, &mut subst, parsed)?;
    unify(&mut subst, typed.ty, Ty::mk_unit(ctx))?;
    subst.deref_ty_var(&mut typed);
    Ok(typed)
}

/// Recursively infer the type of the given expression.
///
/// Originally named `Typing.g` (or `typing_g`).
fn decide_ty<'ctx>(
    ctx: &'ctx TypingContext<'ctx>,
    common_types: &CommonTypes<'ctx>,
    name_res: &mut name_res::Env<'ctx>,
    subst: &mut ty_var_subst::Env<'ctx>,
    expr: syntax::Expr<'ctx>,
) -> Result<ir_typed_ast::Expr<'ctx>, Error<'ctx>> {
    let span = expr.span;
    let (ty, node) = match expr.kind() {
        syntax::ExprKind::Const(lit) => {
            let ty = match lit {
                syntax::LitKind::Int(..) => common_types.int,
                syntax::LitKind::Float(..) => common_types.float,
                syntax::LitKind::Bool(..) => common_types.bool,
                syntax::LitKind::Unit => common_types.unit,
            };
            (ty, ir_typed_ast::ExprKind::Const(*lit))
        }
        syntax::ExprKind::Unary(un_op, e) => {
            let e = decide_ty(ctx, common_types, name_res, subst, e)?;
            let ty = match un_op {
                syntax::UnOp::Neg => {
                    unify(subst, e.ty, common_types.int)?;
                    common_types.int
                }
                syntax::UnOp::FNeg => {
                    unify(subst, e.ty, common_types.float)?;
                    common_types.float
                }
                syntax::UnOp::Not => {
                    unify(subst, e.ty, common_types.bool)?;
                    common_types.bool
                }
            };
            (ty, ir_typed_ast::ExprKind::Unary(*un_op, e))
        }
        syntax::ExprKind::Binary(bin_op, e1, e2) => {
            let e1 = decide_ty(ctx, common_types, name_res, subst, e1)?;
            let e2 = decide_ty(ctx, common_types, name_res, subst, e2)?;
            let ty = match bin_op {
                syntax::BinOp::BBinOp(..) => {
                    unify(subst, e1.ty, e2.ty)?;
                    common_types.bool
                }
                syntax::BinOp::IBinOp(..) => {
                    unify(subst, e1.ty, common_types.int)?;
                    unify(subst, e2.ty, common_types.int)?;
                    common_types.int
                }
                syntax::BinOp::FBinOp(..) => {
                    unify(subst, e1.ty, common_types.float)?;
                    unify(subst, e2.ty, common_types.float)?;
                    common_types.float
                }
            };
            (ty, ir_typed_ast::ExprKind::Binary(*bin_op, e1, e2))
        }
        syntax::ExprKind::If(e1, e2, e3) => {
            let e1 = decide_ty(ctx, common_types, name_res, subst, e1)?;
            let e2 = decide_ty(ctx, common_types, name_res, subst, e2)?;
            let e3 = decide_ty(ctx, common_types, name_res, subst, e3)?;
            unify(subst, e1.ty, Ty::mk_bool(ctx))?;
            unify(subst, e2.ty, e3.ty)?;
            (e2.ty, ir_typed_ast::ExprKind::If(e1, e2, e3))
        }
        syntax::ExprKind::Let(let_binder, follows) => {
            let mut typed_args: Vec<ir_typed_ast::Ident<'_>> =
                Vec::with_capacity(let_binder.len_args());
            let typed_bound_value = if let_binder.has_args() {
                let inner_scope = name_res.begin_scope();
                for arg in let_binder.args() {
                    let ty = Ty::mk_ty_var(ctx);
                    let ident = name_res.define_in(inner_scope, arg, ty);
                    typed_args.push(ctx.new_ident(ident));
                }
                let typed_bound_value =
                    decide_ty(ctx, common_types, name_res, subst, let_binder.value())?;
                name_res.end_scope(inner_scope);
                typed_bound_value
            } else {
                decide_ty(ctx, common_types, name_res, subst, let_binder.value())?
            };
            let scope = name_res.begin_scope();
            let typed_place = match let_binder.place() {
                syntax::Pattern::Var(var) => {
                    let ident = name_res.define_in(scope, *var, Ty::mk_ty_var(ctx));
                    ir_typed_ast::Pattern::Var(ctx.new_ident(ident))
                }
                syntax::Pattern::Tuple(vars) => {
                    let idents = vars
                        .iter()
                        .map(|var| {
                            let ident = name_res.define_in(scope, *var, Ty::mk_ty_var(ctx));
                            ctx.new_ident(ident)
                        })
                        .collect();
                    ir_typed_ast::Pattern::Tuple(idents)
                }
            };
            let typed_follows = decide_ty(ctx, common_types, name_res, subst, follows)?;
            name_res.end_scope(scope);

            (
                typed_follows.ty,
                ir_typed_ast::ExprKind::Let(
                    ir_typed_ast::LetBinding {
                        place: typed_place,
                        args: typed_args,
                        value: typed_bound_value,
                    },
                    typed_follows,
                ),
            )
        }
        syntax::ExprKind::Then(e1, e2) => {
            let e1 = decide_ty(ctx, common_types, name_res, subst, e1)?;
            let e2 = decide_ty(ctx, common_types, name_res, subst, e2)?;
            unify(subst, e1.ty, Ty::mk_unit(ctx))?;
            (e2.ty, ir_typed_ast::ExprKind::Then(e1, e2))
        }
        syntax::ExprKind::Var(var) => {
            let Some(typed_var) = name_res.get(*var) else {
                return Err(Error::UnboundIdent(*var));
            };
            let typed_var = ctx.new_ident(typed_var);
            (typed_var.ty, ir_typed_ast::ExprKind::Var(typed_var))
        }
        syntax::ExprKind::App(fun, args) => {
            let typed_fun = decide_ty(ctx, common_types, name_res, subst, fun)?;
            let typed_args = decide_ty_many(ctx, common_types, name_res, subst, args)?;
            let ret_ty = Ty::mk_ty_var(ctx);
            let fun_ty = Ty::mk_fun(ctx, typed_args.iter().map(|e| e.ty).collect(), ret_ty);
            unify(subst, typed_fun.ty, fun_ty)?;
            (ret_ty, ir_typed_ast::ExprKind::App(typed_fun, typed_args))
        }
        syntax::ExprKind::Tuple(exprs) => {
            let typed_exprs = decide_ty_many(ctx, common_types, name_res, subst, exprs)?;
            let ty = Ty::mk_tuple(ctx, typed_exprs.iter().map(|e| e.ty).collect());
            (ty, ir_typed_ast::ExprKind::Tuple(typed_exprs))
        }
        syntax::ExprKind::ArrayMake(_, _) => todo!(),
        syntax::ExprKind::Get(_, _) => todo!(),
        syntax::ExprKind::Set(_, _, _) => todo!(),
    };
    let e = ctx.new_expr(Typed::new(Spanned { node, span }, ty));
    Ok(e)
}

fn decide_ty_many<'ctx>(
    ctx: &'ctx TypingContext<'ctx>,
    common_types: &CommonTypes<'ctx>,
    name_res: &mut name_res::Env<'ctx>,
    subst: &mut ty_var_subst::Env<'ctx>,
    exprs: &Vec<syntax::Expr<'ctx>>,
) -> Result<Vec<ir_typed_ast::Expr<'ctx>>, Error<'ctx>> {
    let mut typed = Vec::with_capacity(exprs.len());
    for expr in exprs {
        let e = decide_ty(ctx, common_types, name_res, subst, expr)?;
        typed.push(e);
    }
    Ok(typed)
}

fn unify<'ctx>(
    subst: &mut ty_var_subst::Env<'ctx>,
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
            subst.merge(*v, rhs);
            Ok(())
        }
        (_, TyKind::TyVar(v)) => {
            if let Some(ty) = subst.get(*v) {
                return unify(subst, lhs, ty);
            }
            occurck(subst, *v, lhs)?;
            subst.merge(*v, lhs);
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
fn occurck<'ctx>(
    subst: &ty_var_subst::Env<'ctx>,
    var: TyVarId,
    ty: Ty<'ctx>,
) -> Result<(), Error<'ctx>> {
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
