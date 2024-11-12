#![feature(iterator_try_collect)]
#![feature(if_let_guard)]

use std::borrow::Cow;

use data_structure::{index::vec::IndexVec, FxHashSet};
use error::UnifyError;
use errors::{Diag, DiagContext};
use macros::fluent_messages;
use sourcemap::Spanned;
use ty::{context::CommonTypes, Ty, Typed};

mod error;
mod name_res;
mod ty_var_subst;
mod unify;

fluent_messages! { "../messages.ftl" }

use unify::{unify, UnifyArg};

pub type Context<'ctx> = ty::context::TypingContext<
    'ctx,
    Typed<'ctx, ir_typed_ast::DisambiguatedIdent<'ctx>>,
    Typed<'ctx, Spanned<ir_typed_ast::ExprKind<'ctx>>>,
>;

#[derive(Debug)]
pub enum Phase {
    Interface,
    Main,
}

impl std::fmt::Display for Phase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Phase::Interface => write!(f, "interface"),
            Phase::Main => write!(f, "main expression"),
        }
    }
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
    dcx: &'ctx DiagContext<'ctx>,
    common_types: &CommonTypes<'ctx>,
    parsed: syntax::Expr<'ctx>,
    parsed_interface: syntax::mli::Mli<'ctx>,
    typed_interface: &'ctx ir_typed_ast::mli::Mli<'ctx>,
) -> (Result<ir_typed_ast::Expr<'ctx>, Diag<'ctx>>, Taint<'ctx>) {
    let mut name_res = name_res::Env::new();
    let mut taint = Taint::default();

    let mut phase = Phase::Interface;

    for syntax::mli::Declaration {
        item_ident,
        ascribed_ty: syntax::mli::AscribedTy { elements },
    } in parsed_interface.declarations
    {
        if let Some(&ident) = item_ident.as_var() {
            /// Try to convert an ascribed type to a [`Ty`].
            ///
            /// This is an ad-hoc implementation that only supports a few types.
            /// See [`syntax::mli::AscribedTy`] for their restrictions.
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
            let mut types = Vec::with_capacity(elements.len());
            let mut has_error = elements.len() >= 2;
            for element in elements {
                let ty = ty_try_from_ascribed(element, common_types);
                match ty {
                    Ok(ty) => types.push(ty),
                    Err(_) => {
                        has_error = true;
                    }
                }
            }
            if has_error {
                dcx.emit_err(error::InvalidTypeAscription::default());
                taint.fail();
                return (Err(dcx.create_err(error::TypingError { phase })), taint);
            }
            let ret = types.pop().unwrap();
            let ty = Ty::mk_fun(ctx, types, ret);
            let declaration = ir_typed_ast::mli::Declaration {
                item_ident: ident,
                ty,
            };
            typed_interface.add_declaration(declaration);
            name_res.register_intrinsic(ident, ty);
        }
    }

    phase = Phase::Main;

    // Top level call of `decide_ty` to infer the type of the main expression.
    let mut subst = Cow::Owned(ty_var_subst::Env::new());
    let Some(mut typed) = decide_ty(
        ctx,
        dcx,
        common_types,
        &mut name_res,
        &mut subst,
        &mut taint,
        parsed,
    ) else {
        taint.fail();
        return (Err(dcx.create_err(error::TypingError { phase })), taint);
    };
    // Unify the type of the main expression with `unit`.
    unify(dcx, &mut subst, typed.ty, common_types.unit).unwrap();
    subst.into_owned().deref_ty_var(ctx, &mut typed);
    (Ok(typed), taint)
}

#[derive(Default)]
/// Global state about errors for the type checker.
pub struct Taint<'ctx> {
    /// A set of identifiers that are not bound in the current scope.
    ///
    /// These identifiers are only valid immediately after the type checking,
    /// because they are likely to be removed or modified in the next phases.
    pub undefined_vars: FxHashSet<ir_typed_ast::Ident<'ctx>>,

    /// A boolean flag indicating whether the type checking has failed.
    pub failed: bool,
}

impl<'ctx> Taint<'ctx> {
    fn undefined(&mut self, ident: ir_typed_ast::Ident<'ctx>) {
        self.undefined_vars.insert(ident);
    }

    fn fail(&mut self) {
        self.failed = true;
    }
}

fn expect<'ctx>(
    dcx: &'ctx DiagContext<'ctx>,
    subst: &mut Cow<ty_var_subst::Env<'ctx>>,
    expected_ty: Ty<'ctx>,
    expr: &ir_typed_ast::Expr<'ctx>,
) {
    if let Err(err) = unify(dcx, subst, expr.ty, expected_ty) {
        let diag = dcx.create_err(error::GeneralExpectFailure {
            span: *expr.span.as_user_defined().unwrap(),
            expected_ty,
            found_ty: expr.ty,
            expr_kind: expr.kind().into(),
        });
        if let UnifyError::UnifyFailed { lhs, rhs, kind } = err {
            todo!()
        }
        diag.emit();
    }
}

/// Recursively infer the type of the given expression.
///
/// Originally named `Typing.g` (or `typing_g`).
fn decide_ty<'ctx>(
    ctx: &'ctx Context<'ctx>,
    dcx: &'ctx DiagContext<'ctx>,
    common_types: &CommonTypes<'ctx>,
    name_res: &mut name_res::Env<'ctx>,
    subst: &mut Cow<ty_var_subst::Env<'ctx>>,
    taint: &mut Taint<'ctx>,
    expr: syntax::Expr<'ctx>,
) -> Option<ir_typed_ast::Expr<'ctx>> {
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
            let e = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e)?;
            let (un_op, ty) = match un_op {
                syntax::UnOp::Neg => {
                    // Try to unify with `int` first in a new temporary environment,
                    // then `float` if it fails.
                    let mut new_subst = Cow::Borrowed(&**subst);
                    if let Err(_err) = unify(dcx, &mut new_subst, e.ty, common_types.int) {
                        unify(dcx, subst, e.ty, common_types.float).unwrap();
                        (ir_typed_ast::UnOp::Fneg, common_types.float)
                    } else {
                        // If the first unification succeeds and modified the environment,
                        // we should update the original substitution environment with the new one.
                        if let Cow::Owned(new_subst) = new_subst {
                            // N.B., These two expressions looks identical,
                            // but they refer to different instances of `Cow`.
                            *subst = Cow::Owned(new_subst);
                        }
                        (ir_typed_ast::UnOp::Ineg, common_types.int)
                    }
                }
                syntax::UnOp::FNeg => {
                    unify(dcx, subst, e.ty, common_types.float).unwrap();
                    (ir_typed_ast::UnOp::Fneg, common_types.float)
                }
                syntax::UnOp::Not => {
                    unify(dcx, subst, e.ty, common_types.bool).unwrap();
                    (ir_typed_ast::UnOp::Not, common_types.bool)
                }
            };
            (ty, ir_typed_ast::ExprKind::Unary(un_op, e))
        }
        syntax::ExprKind::Binary(bin_op, e1, e2) => {
            let e1 = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e1).unwrap();
            let e2 = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e2).unwrap();
            let ty = match bin_op {
                syntax::BinOp::Relation(..) => {
                    unify(dcx, subst, e1.ty, e2.ty).unwrap();
                    common_types.bool
                }
                syntax::BinOp::Int(..) => {
                    unify(dcx, subst, e1.ty, common_types.int).unwrap();
                    unify(dcx, subst, e2.ty, common_types.int).unwrap();
                    common_types.int
                }
                syntax::BinOp::Float(..) => {
                    unify(dcx, subst, e1.ty, common_types.float).unwrap();
                    unify(dcx, subst, e2.ty, common_types.float).unwrap();
                    common_types.float
                }
            };
            (ty, ir_typed_ast::ExprKind::Binary(*bin_op, e1, e2))
        }
        syntax::ExprKind::If(e1, e2, e3) => {
            let e1_typed = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e1);
            let e2_typed = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e2);
            let e3_typed = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e3);
            if let Some(e1) = &e1_typed {
                expect(dcx, subst, common_types.bool, e1);
            }
            if let (Some(e2), Some(e3)) = (&e2_typed, &e3_typed) {
                unify(dcx, subst, e2.ty, e3.ty).unwrap();
            }
            let (Some(e1), Some(e2), Some(e3)) = (e1_typed, e2_typed, e3_typed) else {
                taint.fail();
                return None;
            };
            (e2.ty, ir_typed_ast::ExprKind::If(e1, e2, e3))
        }
        syntax::ExprKind::Let(let_binder, follows) => {
            let mut typed_args: Vec<ir_typed_ast::Ident<'_>> =
                Vec::with_capacity(let_binder.len_args());
            with_scope! {
            with scope = name_res.with_scope();
            let pattern = let_binder.pattern();

            #[derive(Debug)]
            struct AnalyzedBinding<'ctx> {
                rhs_ty: Ty<'ctx>,
                typed_pattern: ir_typed_ast::Pattern<'ctx>,
            }
            struct SetOnce<T> {
                value: Option<T>,
            }
            impl<T> SetOnce<T> {
                fn new() -> Self {
                    Self { value: None }
                }
                fn set(&mut self, value: T) where T: std::fmt::Debug {
                    if self.value.is_some() {
                        panic!("Value already set: {:?}", self.value);
                    }
                    self.value = Some(value);
                }
                fn unwrap(self) -> T {
                    self.value.expect("Value not set")
                }
            }
            let mut analyzed: SetOnce<AnalyzedBinding> = SetOnce::new();

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
                analyzed.set(AnalyzedBinding {
                    rhs_ty: rhs_ty_,
                    typed_pattern: ir_typed_ast::Pattern::Var(ir_typed_ast::Ident::new(
                        ctx.alloc_ident(ident),
                    )),
                });

                with_scope! {
                with inner_scope = name_res.with_scope();
                for (arg, ty) in let_binder.args().zip(arg_tys) {
                    let ident = name_res.define_in(inner_scope, arg, ty);
                    typed_args.push(ir_typed_ast::Ident::new(ctx.alloc_ident(ident)));
                }
                let typed_bound_value =
                    decide_ty(ctx, dcx, common_types, name_res, subst, taint,let_binder.value());
                }

                typed_bound_value
            } else {
                decide_ty(ctx, dcx, common_types, name_res, subst, taint, let_binder.value())
            };

            if !let_binder.has_args() {
                if let syntax::Pattern::Var(var) = pattern {
                    {
                        let lhs_ty = Ty::mk_ty_var(ctx);
                        let ident = name_res.define_in(scope, *var, lhs_ty);

                        analyzed.set(AnalyzedBinding {
                            rhs_ty: lhs_ty,
                            typed_pattern: ir_typed_ast::Pattern::Var(ir_typed_ast::Ident::new(
                                ctx.alloc_ident(ident),
                            )),
                        });
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

                analyzed.set(AnalyzedBinding {
                    rhs_ty: Ty::mk_tuple(ctx, lhs_tys),
                    typed_pattern: ir_typed_ast::Pattern::Tuple(idents),
                });
            }

            let AnalyzedBinding { rhs_ty, typed_pattern } = analyzed.unwrap();

            let Some(typed_bound_value) = typed_bound_value else {
                // Register the variables as unbound.
                for ident in typed_pattern.iter_idents() {
                    // `to_owned`: `ident` is borrowed from `typed_pattern`, which is going to be moved.
                    taint.undefined(ident.to_owned());
                }

                // Try continue with the next expression.
                return decide_ty(ctx, dcx, common_types, name_res, subst,taint, follows);
            };

            unify(dcx, subst, rhs_ty, typed_bound_value.ty).unwrap();
            let typed_follows =
                decide_ty(ctx, dcx, common_types, name_res, subst, taint,follows).unwrap();
            }

            (
                typed_follows.ty,
                ir_typed_ast::ExprKind::Let(
                    ir_typed_ast::LetBinding {
                        pattern: typed_pattern,
                        args: IndexVec::from_raw_vec(typed_args),
                        value: typed_bound_value,
                    },
                    typed_follows,
                ),
            )
        }
        syntax::ExprKind::Then(e1, e2) => {
            let e1 = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e1).unwrap();
            let e2 = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e2).unwrap();
            unify(dcx, subst, e1.ty, common_types.unit).unwrap();
            (e2.ty, ir_typed_ast::ExprKind::Then(e1, e2))
        }
        syntax::ExprKind::Var(var) => {
            let Some(typed_var) = name_res.get(*var) else {
                dcx.emit_err(error::UnboundIdent {
                    span: *expr.span.as_user_defined().unwrap(),
                    var: *var,
                });
                return None;
            };
            let typed_var = ir_typed_ast::Ident::new(ctx.alloc_ident(typed_var));
            (typed_var.ty, ir_typed_ast::ExprKind::Var(typed_var))
        }
        syntax::ExprKind::App(fun, args) => {
            let typed_fun = decide_ty(ctx, dcx, common_types, name_res, subst, taint, fun).unwrap();
            let typed_args =
                decide_ty_many(ctx, dcx, common_types, name_res, subst, taint, args).unwrap();
            let ret_ty = Ty::mk_ty_var(ctx);
            let fun_ty = Ty::mk_fun(ctx, typed_args.iter().map(|e| e.ty).collect(), ret_ty);
            unify(dcx, subst, typed_fun.ty, fun_ty).unwrap();
            (
                ret_ty,
                ir_typed_ast::ExprKind::App(typed_fun, IndexVec::from_raw_vec(typed_args)),
            )
        }
        syntax::ExprKind::Tuple(exprs) => {
            let typed_exprs =
                decide_ty_many(ctx, dcx, common_types, name_res, subst, taint, exprs).unwrap();
            let ty = Ty::mk_tuple(ctx, typed_exprs.iter().map(|e| e.ty).collect());
            (
                ty,
                ir_typed_ast::ExprKind::Tuple(IndexVec::from_raw_vec(typed_exprs)),
            )
        }
        syntax::ExprKind::ArrayMake(e1, e2) => {
            let e1 = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e1).unwrap();
            let e2 = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e2).unwrap();
            unify(dcx, subst, e1.ty, common_types.int).unwrap();
            let ty = Ty::mk_array(ctx, e2.ty);
            (ty, ir_typed_ast::ExprKind::ArrayMake(e1, e2))
        }
        syntax::ExprKind::Get(e1, e2) => {
            let e1 = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e1).unwrap();
            let e2 = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e2).unwrap();
            let ty = Ty::mk_ty_var(ctx);
            unify(dcx, subst, e1.ty, Ty::mk_array(ctx, ty)).unwrap();
            unify(dcx, subst, e2.ty, common_types.int).unwrap();
            (ty, ir_typed_ast::ExprKind::Get(e1, e2))
        }
        syntax::ExprKind::Set(e1, e3) => {
            // We restrict the form of `e1` to be `Get(e1, e2)` here.
            let syntax::ExprKind::Get(e1, e2) = &e1.node else {
                dcx.emit_err(error::InvalidSetSyntax {
                    lhs: *e1.span.as_user_defined().unwrap(),
                    note: (),
                });
                return None;
            };
            let e1 = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e1).unwrap();
            let e2 = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e2).unwrap();
            let e3 = decide_ty(ctx, dcx, common_types, name_res, subst, taint, e3).unwrap();
            unify(dcx, subst, e1.ty, Ty::mk_array(ctx, e3.ty)).unwrap();
            unify(dcx, subst, e2.ty, common_types.int).unwrap();
            (common_types.unit, ir_typed_ast::ExprKind::Set(e1, e2, e3))
        }
    };
    let e = ctx.alloc_expr(Typed::new(Spanned { node, span }, ty));
    Some(ir_typed_ast::Expr::new(e))
}

fn decide_ty_many<'ctx>(
    ctx: &'ctx Context<'ctx>,
    dcx: &'ctx DiagContext<'ctx>,
    common_types: &CommonTypes<'ctx>,
    name_res: &mut name_res::Env<'ctx>,
    subst: &mut Cow<ty_var_subst::Env<'ctx>>,
    taint: &mut Taint<'ctx>,
    exprs: &Vec<syntax::Expr<'ctx>>,
) -> Result<Vec<ir_typed_ast::Expr<'ctx>>, ()> {
    let mut typed = Vec::with_capacity(exprs.len());
    for expr in exprs {
        let e = decide_ty(ctx, dcx, common_types, name_res, subst, taint, expr).unwrap();
        typed.push(e);
    }
    Ok(typed)
}
