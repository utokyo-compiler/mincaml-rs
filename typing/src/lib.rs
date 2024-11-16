#![feature(iterator_try_collect)]
#![feature(if_let_guard)]

use data_structure::index::vec::IndexVec;
use sourcemap::Spanned;
use ty::{context::CommonTypes, Ty, TyKind, TyVarId, Typed};

mod name_res;
mod ty_var_subst;

pub type Context<'ctx> = ty::context::TypingContext<
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

    /// Invalid syntax.
    InvalidSyntax(sourcemap::Span),

    /// Invalid type ascription.
    InvalidTypeAscription(syntax::Ident<'ctx>),
}

/// The main entry point for type checking.
///
/// Originally named `Typing.f`.
///
/// # Parameters
///
/// - `ctx`: The context for type checking.
/// - `common_types`: Common, pre-interned types.
/// - `parsed`: The parsed expression.
/// - `parsed_interface`: The parsed interface.
/// - `typed_interface`: The typed interface to be filled.
pub fn typeck<'ctx>(
    ctx: &'ctx Context<'ctx>,
    common_types: &CommonTypes<'ctx>,
    parsed: syntax::Expr<'ctx>,
    parsed_interface: syntax::mli::Mli<'ctx>,
    typed_interface: &'ctx ir_typed_ast::mli::Mli<'ctx>,
) -> Result<ir_typed_ast::Expr<'ctx>, Error<'ctx>> {
    let mut name_res = name_res::Env::new();

    for syntax::mli::Declaration {
        item_ident,
        ascribed_ty: syntax::mli::AscribedTy { elements },
    } in parsed_interface.declarations
    {
        if let Some(&ident) = item_ident.as_var() {
            fn ty_try_from_ascribed<'ctx>(
                ident: syntax::Ident<'ctx>,
                common_types: &CommonTypes<'ctx>,
            ) -> Result<Ty<'ctx>, syntax::Ident<'ctx>> {
                Ok(match ident.0 {
                    "int" => common_types.int,
                    "float" => common_types.float,
                    "bool" => common_types.bool,
                    "unit" => common_types.unit,
                    _ => return Err(ident),
                })
            }
            let mut types: Vec<_> = elements
                .into_iter()
                .map(|element| ty_try_from_ascribed(element, common_types))
                .try_collect()
                .map_err(Error::InvalidTypeAscription)
                .unwrap();
            let ret = types.pop().expect("elements is must not empty");
            let ty = Ty::mk_fun(ctx, types, ret);
            let declaration = ir_typed_ast::mli::Declaration {
                item_ident: ident,
                ty,
            };
            typed_interface.add_declaration(declaration);
            name_res.register_intrinsic(ident, ty);
        }
    }

    let mut subst = ty_var_subst::Env::new();
    let mut typed = decide_ty(ctx, common_types, &mut name_res, &mut subst, parsed).unwrap();
    unify(&mut subst, typed.ty, common_types.unit).unwrap();
    subst.deref_ty_var(ctx, &mut typed);
    Ok(typed)
}

/// Recursively infer the type of the given expression.
///
/// Originally named `Typing.g` (or `typing_g`).
fn decide_ty<'ctx>(
    ctx: &'ctx Context<'ctx>,
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
            let e = decide_ty(ctx, common_types, name_res, subst, e).unwrap();
            let (un_op, ty) = match un_op {
                syntax::UnOp::Neg => {
                    if let Err(_err) = unify(subst, e.ty, common_types.int) {
                        unify(subst, e.ty, common_types.float).unwrap();
                        (ir_typed_ast::UnOp::Fneg, common_types.float)
                    } else {
                        (ir_typed_ast::UnOp::Ineg, common_types.int)
                    }
                }
                syntax::UnOp::FNeg => {
                    unify(subst, e.ty, common_types.float).unwrap();
                    (ir_typed_ast::UnOp::Fneg, common_types.float)
                }
                syntax::UnOp::Not => {
                    unify(subst, e.ty, common_types.bool).unwrap();
                    (ir_typed_ast::UnOp::Not, common_types.bool)
                }
            };
            (ty, ir_typed_ast::ExprKind::Unary(un_op, e))
        }
        syntax::ExprKind::Binary(bin_op, e1, e2) => {
            let e1 = decide_ty(ctx, common_types, name_res, subst, e1).unwrap();
            let e2 = decide_ty(ctx, common_types, name_res, subst, e2).unwrap();
            let ty = match bin_op {
                syntax::BinOp::Relation(..) => {
                    unify(subst, e1.ty, e2.ty).unwrap();
                    common_types.bool
                }
                syntax::BinOp::Int(..) => {
                    unify(subst, e1.ty, common_types.int).unwrap();
                    unify(subst, e2.ty, common_types.int).unwrap();
                    common_types.int
                }
                syntax::BinOp::Float(..) => {
                    unify(subst, e1.ty, common_types.float).unwrap();
                    unify(subst, e2.ty, common_types.float).unwrap();
                    common_types.float
                }
            };
            (ty, ir_typed_ast::ExprKind::Binary(*bin_op, e1, e2))
        }
        syntax::ExprKind::If(e1, e2, e3) => {
            let e1 = decide_ty(ctx, common_types, name_res, subst, e1).unwrap();
            let e2 = decide_ty(ctx, common_types, name_res, subst, e2).unwrap();
            let e3 = decide_ty(ctx, common_types, name_res, subst, e3).unwrap();
            unify(subst, e1.ty, common_types.bool).unwrap();
            unify(subst, e2.ty, e3.ty).unwrap();
            (e2.ty, ir_typed_ast::ExprKind::If(e1, e2, e3))
        }
        syntax::ExprKind::Let(let_binder, follows) => {
            let mut typed_args: Vec<ir_typed_ast::Ident<'_>> =
                Vec::with_capacity(let_binder.len_args());
            let scope = name_res.begin_scope();
            let pattern = let_binder.pattern();
            let mut rhs_ty = None;
            let mut typed_pattern = None;

            let typed_bound_value = if let_binder.has_args() {
                let syntax::Pattern::Var(var) = pattern else {
                    unreachable!()
                };
                let rhs_ty_ = Ty::mk_ty_var(ctx);
                let arg_tys = let_binder
                    .args()
                    .map(|_| Ty::mk_ty_var(ctx))
                    .collect::<Vec<_>>();
                let lhs_ty = Ty::mk_fun(ctx, arg_tys.clone(), rhs_ty_);
                let ident = name_res.define_in(scope, *var, lhs_ty);
                rhs_ty = Some(rhs_ty_);
                typed_pattern = Some(ir_typed_ast::Pattern::Var(ir_typed_ast::Ident::new(
                    ctx.alloc_ident(ident),
                )));

                let inner_scope = name_res.begin_scope();
                for (arg, ty) in let_binder.args().zip(arg_tys) {
                    let ident = name_res.define_in(inner_scope, arg, ty);
                    typed_args.push(ir_typed_ast::Ident::new(ctx.alloc_ident(ident)));
                }
                let typed_bound_value =
                    decide_ty(ctx, common_types, name_res, subst, let_binder.value()).unwrap();
                name_res.end_scope(inner_scope);
                typed_bound_value
            } else {
                decide_ty(ctx, common_types, name_res, subst, let_binder.value()).unwrap()
            };

            if !let_binder.has_args() {
                if let syntax::Pattern::Var(var) = pattern {
                    {
                        let lhs_ty = Ty::mk_ty_var(ctx);
                        let ident = name_res.define_in(scope, *var, lhs_ty);
                        rhs_ty = Some(lhs_ty);
                        typed_pattern = Some(ir_typed_ast::Pattern::Var(ir_typed_ast::Ident::new(
                            ctx.alloc_ident(ident),
                        )));
                    }
                }
            }

            if let syntax::Pattern::Tuple(vars) = pattern {
                let mut lhs_tys = Vec::with_capacity(vars.len());
                let idents = vars
                    .iter()
                    .map(|var| {
                        let lhs_ty = Ty::mk_ty_var(ctx);
                        lhs_tys.push(lhs_ty);
                        let ident = name_res.define_in(scope, *var, lhs_ty);
                        ir_typed_ast::Ident::new(ctx.alloc_ident(ident))
                    })
                    .collect();
                rhs_ty = Some(Ty::mk_tuple(ctx, lhs_tys));
                typed_pattern = Some(ir_typed_ast::Pattern::Tuple(idents));
            }

            unify(subst, rhs_ty.unwrap(), typed_bound_value.ty).unwrap();
            let typed_follows = decide_ty(ctx, common_types, name_res, subst, follows).unwrap();
            name_res.end_scope(scope);

            (
                typed_follows.ty,
                ir_typed_ast::ExprKind::Let(
                    ir_typed_ast::LetBinding {
                        pattern: typed_pattern.unwrap(),
                        args: IndexVec::from_raw_vec(typed_args),
                        value: typed_bound_value,
                    },
                    typed_follows,
                ),
            )
        }
        syntax::ExprKind::Then(e1, e2) => {
            let e1 = decide_ty(ctx, common_types, name_res, subst, e1).unwrap();
            let e2 = decide_ty(ctx, common_types, name_res, subst, e2).unwrap();
            unify(subst, e1.ty, common_types.unit).unwrap();
            (e2.ty, ir_typed_ast::ExprKind::Then(e1, e2))
        }
        syntax::ExprKind::Var(var) => {
            let Some(typed_var) = name_res.get(*var) else {
                return Err(Error::UnboundIdent(*var));
            };
            let typed_var = ir_typed_ast::Ident::new(ctx.alloc_ident(typed_var));
            (typed_var.ty, ir_typed_ast::ExprKind::Var(typed_var))
        }
        syntax::ExprKind::App(fun, args) => {
            let typed_fun = decide_ty(ctx, common_types, name_res, subst, fun).unwrap();
            let typed_args = decide_ty_many(ctx, common_types, name_res, subst, args).unwrap();
            let ret_ty = Ty::mk_ty_var(ctx);
            let fun_ty = Ty::mk_fun(ctx, typed_args.iter().map(|e| e.ty).collect(), ret_ty);
            unify(subst, typed_fun.ty, fun_ty).unwrap();
            (
                ret_ty,
                ir_typed_ast::ExprKind::App(typed_fun, IndexVec::from_raw_vec(typed_args)),
            )
        }
        syntax::ExprKind::Tuple(exprs) => {
            let typed_exprs = decide_ty_many(ctx, common_types, name_res, subst, exprs).unwrap();
            let ty = Ty::mk_tuple(ctx, typed_exprs.iter().map(|e| e.ty).collect());
            (
                ty,
                ir_typed_ast::ExprKind::Tuple(IndexVec::from_raw_vec(typed_exprs)),
            )
        }
        syntax::ExprKind::ArrayMake(e1, e2) => {
            let e1 = decide_ty(ctx, common_types, name_res, subst, e1).unwrap();
            let e2 = decide_ty(ctx, common_types, name_res, subst, e2).unwrap();
            unify(subst, e1.ty, common_types.int).unwrap();
            let ty = Ty::mk_array(ctx, e2.ty);
            (ty, ir_typed_ast::ExprKind::ArrayMake(e1, e2))
        }
        syntax::ExprKind::Get(e1, e2) => {
            let e1 = decide_ty(ctx, common_types, name_res, subst, e1).unwrap();
            let e2 = decide_ty(ctx, common_types, name_res, subst, e2).unwrap();
            let ty = Ty::mk_ty_var(ctx);
            unify(subst, e1.ty, Ty::mk_array(ctx, ty)).unwrap();
            unify(subst, e2.ty, common_types.int).unwrap();
            (ty, ir_typed_ast::ExprKind::Get(e1, e2))
        }
        syntax::ExprKind::Set(e1, e3) => {
            // We restrict the form of `e1` to be `Get(e1, e2)` here.
            let syntax::ExprKind::Get(e1, e2) = &e1.node else {
                return Err(Error::InvalidSyntax(*e1.span.as_user_defined().unwrap()));
            };
            let e1 = decide_ty(ctx, common_types, name_res, subst, e1).unwrap();
            let e2 = decide_ty(ctx, common_types, name_res, subst, e2).unwrap();
            let e3 = decide_ty(ctx, common_types, name_res, subst, e3).unwrap();
            unify(subst, e1.ty, Ty::mk_array(ctx, e3.ty)).unwrap();
            unify(subst, e2.ty, common_types.int).unwrap();
            (common_types.unit, ir_typed_ast::ExprKind::Set(e1, e2, e3))
        }
    };
    let e = ctx.alloc_expr(Typed::new(Spanned { node, span }, ty));
    Ok(ir_typed_ast::Expr::new(e))
}

fn decide_ty_many<'ctx>(
    ctx: &'ctx Context<'ctx>,
    common_types: &CommonTypes<'ctx>,
    name_res: &mut name_res::Env<'ctx>,
    subst: &mut ty_var_subst::Env<'ctx>,
    exprs: &Vec<syntax::Expr<'ctx>>,
) -> Result<Vec<ir_typed_ast::Expr<'ctx>>, Error<'ctx>> {
    let mut typed = Vec::with_capacity(exprs.len());
    for expr in exprs {
        let e = decide_ty(ctx, common_types, name_res, subst, expr).unwrap();
        typed.push(e);
    }
    Ok(typed)
}

fn unify<'ctx>(
    subst: &mut ty_var_subst::Env<'ctx>,
    lhs: Ty<'ctx>,
    rhs: Ty<'ctx>,
) -> Result<(), Error<'ctx>> {
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
            occurck(subst, *v, rhs).unwrap();
            subst.merge(*v, rhs);
            Ok(())
        }
        (_, TyKind::TyVar(v)) => {
            occurck(subst, *v, lhs).unwrap();
            subst.merge(*v, lhs);
            Ok(())
        }
        (TyKind::Fun(lhs_args, lhs_ret), TyKind::Fun(rhs_args, rhs_ret)) => {
            if lhs_args.len() != rhs_args.len() {
                return Err(Error::UnifyFailed(lhs, rhs));
            }
            for (lhs_arg, rhs_arg) in lhs_args.iter().zip(rhs_args) {
                unify(subst, *lhs_arg, *rhs_arg).unwrap();
            }
            unify(subst, *lhs_ret, *rhs_ret)
        }
        (TyKind::Tuple(lhs_tys), TyKind::Tuple(rhs_tys)) => {
            if lhs_tys.len() != rhs_tys.len() {
                return Err(Error::UnifyFailed(lhs, rhs));
            }
            for (lhs, rhs) in lhs_tys.iter().zip(rhs_tys) {
                unify(subst, *lhs, *rhs).unwrap();
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
                occurck(subst, var, *arg).unwrap();
            }
            occurck(subst, var, *ret)
        }
        TyKind::Tuple(tys) => {
            for ty in tys {
                occurck(subst, var, *ty).unwrap();
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
