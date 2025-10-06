use std::borrow::Cow;

use data_structure::{index::vec::IndexVec, FxHashSet};
use errors::{AlwaysShow, Diag, DiagContext};
use sourcemap::{Span, Spanned};
use ty::{context::CommonTypes, HasTy, Ty, Typed};

use crate::{
    decide_ty_edsl, error, name_res,
    ty_var_subst::{self, IntroducedTypeVar},
    unify::unify,
    Context,
};

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

/// Error type for this crate.
pub struct Error<'ctx> {
    /// The diagnosis for the error.
    pub diag: Diag<'ctx>,

    /// The taint information for the error.
    pub taint: Taint<'ctx>,
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
) -> Result<ir_typed_ast::Expr<'ctx>, Error<'ctx>> {
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
            let mut has_error = false;
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
                taint.fail();
                dcx.emit_err(error::InvalidTypeAscription::default());
                return Err(Error {
                    diag: dcx.create_err(error::TypingError {
                        phase,
                        note: AlwaysShow,
                    }),
                    taint,
                });
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
    let mut type_checker = TypeChecker {
        ctx,
        dcx,
        common_types,
        name_res: &mut name_res,
        subst: &mut subst,
        taint: &mut taint,
    };
    let DecideTyReturn::Ok(mut typed) = type_checker.decide_ty(parsed) else {
        return Err(Error {
            diag: dcx.create_err(error::TypingError {
                phase,
                note: AlwaysShow,
            }),
            taint,
        });
    };
    // Unify the type of the main expression with `unit`.
    unify(&mut subst, typed.ty, common_types.unit).unwrap();
    if let Err(error) = subst.deref_ty_var(ctx, &mut typed) {
        dcx.emit_err(error);
        return Err(Error {
            diag: dcx.create_err(error::TypingError {
                phase,
                note: AlwaysShow,
            }),
            taint,
        });
    };
    Ok(typed)
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
    failed: bool,
}

impl<'ctx> Taint<'ctx> {
    fn undefined(&mut self, ident: ir_typed_ast::Ident<'ctx>) {
        self.undefined_vars.insert(ident);
    }

    fn fail(&mut self) {
        self.failed = true;
    }

    #[allow(dead_code)]
    fn has_failed(&self) -> bool {
        self.failed
    }
}

/// The type checker.
///
/// This struct carries references that are used over function calls.
///
/// This item is `pub(crate)` to allow access from the [`decide_ty_edsl!`] macro documentation.
pub(crate) struct TypeChecker<'ctx, 'common_types, 'name_res, 'subst, 'env, 'taint> {
    ctx: &'ctx Context<'ctx>,
    dcx: &'ctx DiagContext<'ctx>,
    common_types: &'common_types CommonTypes<'ctx>,
    name_res: &'name_res mut name_res::Env<'ctx>,
    subst: &'subst mut Cow<'env, ty_var_subst::Env<'ctx>>,
    taint: &'taint mut Taint<'ctx>,
}

impl<'ctx> TypeChecker<'ctx, '_, '_, '_, '_, '_> {
    /// Recursively infer the type of the given expression.
    ///
    /// Originally named `Typing.g` (or `typing_g`).
    ///
    /// # Returns
    ///
    /// - `Ok(typed)`: The typed expression.
    /// - `RecoverTy(ty)`: Failed to infer the type, but the type is recoverable.
    /// - `Fail`: Failed to infer the type and the type is not recoverable.
    ///
    /// See [`DecideTyReturn`] for more details.
    fn decide_ty(&mut self, expr: syntax::Expr<'ctx>) -> DecideTyReturn<'ctx> {
        let span = expr.span;
        let (ty, node) = match expr.kind() {
            syntax::ExprKind::Const(lit) => {
                let ty = match lit {
                    syntax::LitKind::Int(..) => self.common_types.int,
                    syntax::LitKind::Float(..) => self.common_types.float,
                    syntax::LitKind::Bool(..) => self.common_types.bool,
                    syntax::LitKind::Unit => self.common_types.unit,
                };
                (ty, ir_typed_ast::ExprKind::Const(*lit))
            }
            syntax::ExprKind::Unary(syntax::UnOp::Neg, e) => {
                let e_alias = e;
                // `- : int -> int | float -> float`
                decide_ty_edsl! { self;
                    prelude: decide e;
                }
                let Some(t) = e.ty() else {
                    // If the type is not recoverable, we can fail, but
                    // we should recover the type to continue type checking.
                    return DecideTyReturn::Recover(
                        self.new_ty_var(IntroducedTypeVar::undecidable_polymorphism(e_alias)),
                    );
                };
                // Try to unify with `int` first in a new temporary environment,
                // then try `float` if it fails.
                let mut new_subst = Cow::Borrowed(&**self.subst);
                let (un_op, ty) = if let Err(_err) = unify(&mut new_subst, t, self.common_types.int)
                {
                    // If the first unification fails, we should try to unify with `float`,
                    // providing the original substitution environment.
                    self.unify_with_current_subst(t, self.common_types.float)
                        .unwrap();
                    (ir_typed_ast::UnOp::Fneg, self.common_types.float)
                } else {
                    // If the first unification succeeds and modified the environment,
                    // we should update the original substitution environment with the new one.
                    if let Cow::Owned(new_subst) = new_subst {
                        // N.B., These two expressions looks identical,
                        // but they refer to different instances of `Cow`.
                        *self.subst = Cow::Owned(new_subst);
                    }
                    (ir_typed_ast::UnOp::Ineg, self.common_types.int)
                };
                decide_ty_edsl! { self;
                    postlude: for(e)
                    (
                        || (#ty),
                        ir_typed_ast::ExprKind::Unary(un_op, e)
                    )
                }
            }
            syntax::ExprKind::Unary(syntax::UnOp::FNeg, e) => {
                decide_ty_edsl! { self;
                    decide e;
                    unify {
                        e: float;
                    }
                    (
                        || float,
                        ir_typed_ast::ExprKind::Unary(ir_typed_ast::UnOp::Fneg, e)
                    )
                }
            }
            syntax::ExprKind::Unary(syntax::UnOp::Not, e) => {
                decide_ty_edsl! { self;
                    decide e;
                    unify {
                        e: bool;
                    }
                    (
                        || bool,
                        ir_typed_ast::ExprKind::Unary(ir_typed_ast::UnOp::Not, e)
                    )
                }
            }
            syntax::ExprKind::Binary(bin_op, e1, e2) => {
                decide_ty_edsl! { self;
                    prelude: decide e1, e2
                }
                let ty = match bin_op {
                    syntax::BinOp::Relation(..) => {
                        // `(relationship) : 'a -> 'a -> bool`
                        decide_ty_edsl! { self;
                            interlude: unify {
                                (e1, e2): ('a, 'a);
                            }
                        }
                        self.common_types.bool
                    }
                    syntax::BinOp::Int(..) => {
                        // `(integer operation) : int -> int -> int`
                        decide_ty_edsl! { self;
                            interlude: unify {
                                e1: int;
                                e2: int;
                            }
                        }
                        self.common_types.int
                    }
                    syntax::BinOp::Float(..) => {
                        // `(float operation) : float -> float -> float`
                        decide_ty_edsl! { self;
                            interlude: unify {
                                e1: float;
                                e2: float;
                            }
                        }
                        self.common_types.float
                    }
                };
                decide_ty_edsl! { self;
                    postlude: for(e1, e2)
                    (
                        || (#ty),
                        ir_typed_ast::ExprKind::Binary(*bin_op, e1, e2)
                    )
                }
            }
            syntax::ExprKind::If(e1, e2, e3) => {
                // `if : bool -> 'a -> 'a -> 'a (with conditional evaluation)`
                decide_ty_edsl! { self;
                    decide e1, e2, e3;
                    unify {
                        e1: bool;
                        (e2, e3): ('a, 'a);
                    }
                    (
                        |e2| (#e2) else {
                            |e3| {
                                return recover (#e3);
                            }
                        },
                        ir_typed_ast::ExprKind::If(e1, e2, e3)
                    )
                }
            }
            syntax::ExprKind::Let(let_binder, follows) => {
                let mut typed_args: Vec<ir_typed_ast::Ident<'_>> =
                    Vec::with_capacity(let_binder.len_args());
                let scope = self.name_res.begin_scope();
                let pattern = let_binder.pattern();
                #[derive(Debug)]
                struct AnalyzedBinding<'ctx> {
                    rhs_ty: Ty<'ctx>,
                    typed_pattern: ir_typed_ast::Pattern<'ctx>,
                }
                /// This struct is a contract between the producer and the consumer
                /// of the [`AnalyzedBinding`] struct. It should be removed
                /// when the analysis part is extracted into a separate function.
                struct SetOnce<T> {
                    value: Option<T>,
                }
                impl<T> SetOnce<T> {
                    fn unset() -> Self {
                        Self { value: None }
                    }
                    fn set(&mut self, value: T)
                    where
                        T: std::fmt::Debug,
                    {
                        if self.value.is_some() {
                            panic!("Value already set: {:?}", self.value);
                        }
                        self.value = Some(value);
                    }
                    fn unwrap(self) -> T {
                        self.value.expect("Value not set")
                    }
                }
                let mut analyzed: SetOnce<AnalyzedBinding> = SetOnce::unset();

                let typed_bound_value = if let_binder.has_args() {
                    let syntax::Pattern::Var(var) = pattern else {
                        unreachable!()
                    };
                    let rhs_ty = self.new_ty_var(IntroducedTypeVar::let_var(*var));
                    let arg_tys = let_binder
                        .args()
                        .map(|spanned_ident| {
                            self.new_ty_var(IntroducedTypeVar::let_arg(spanned_ident))
                        })
                        .collect::<Vec<_>>();
                    let lhs_ty = Ty::mk_fun(self.ctx, arg_tys.clone(), rhs_ty);
                    let ident = self.name_res.define_in(scope, *var, lhs_ty);
                    analyzed.set(AnalyzedBinding {
                        rhs_ty,
                        typed_pattern: ir_typed_ast::Pattern::Var(ir_typed_ast::Ident::new(
                            self.ctx.alloc_ident(ident),
                        )),
                    });

                    let inner_scope = self.name_res.begin_scope();
                    for (arg, ty) in let_binder.args().zip(arg_tys) {
                        let ident = self.name_res.define_in(inner_scope, arg, ty);
                        typed_args.push(ir_typed_ast::Ident::new(self.ctx.alloc_ident(ident)));
                    }
                    let typed_bound_value = self.decide_ty(let_binder.value());
                    self.name_res.end_scope(inner_scope);

                    typed_bound_value
                } else {
                    self.decide_ty(let_binder.value())
                };

                if !let_binder.has_args() {
                    if let syntax::Pattern::Var(var) = pattern {
                        {
                            let lhs_ty = self.new_ty_var(IntroducedTypeVar::let_var(*var));
                            let ident = self.name_res.define_in(scope, *var, lhs_ty);

                            analyzed.set(AnalyzedBinding {
                                rhs_ty: lhs_ty,
                                typed_pattern: ir_typed_ast::Pattern::Var(
                                    ir_typed_ast::Ident::new(self.ctx.alloc_ident(ident)),
                                ),
                            });
                        }
                    }
                }

                if let syntax::Pattern::Tuple(vars) = pattern {
                    let mut lhs_tys = Vec::with_capacity(vars.len());
                    let idents = vars
                        .iter()
                        .map(|var| {
                            let lhs_ty = self.new_ty_var(IntroducedTypeVar::let_tuple(*var));
                            lhs_tys.push(lhs_ty);
                            let ident = self.name_res.define_in(scope, *var, lhs_ty);
                            ir_typed_ast::Ident::new(self.ctx.alloc_ident(ident))
                        })
                        .collect();

                    analyzed.set(AnalyzedBinding {
                        rhs_ty: Ty::mk_tuple(self.ctx, lhs_tys),
                        typed_pattern: ir_typed_ast::Pattern::Tuple(idents),
                    });
                }

                let AnalyzedBinding {
                    rhs_ty,
                    typed_pattern,
                } = analyzed.unwrap();

                if let Some(t) = typed_bound_value.ty() {
                    // Unify the type of the bound value with the type of the pattern.
                    self.unify_with_current_subst(rhs_ty, t).unwrap();
                }

                let typed_bound_value = match typed_bound_value {
                    DecideTyReturn::Ok(typed_bound_value) => typed_bound_value,
                    _ => {
                        // Register the variables as unbound.
                        for ident in typed_pattern.iter_idents() {
                            // `to_owned`: `ident` is borrowed from `typed_pattern`,
                            // which is going to be moved.
                            self.taint.undefined(ident.to_owned());
                        }

                        // Try continue with the next expression.
                        return self.decide_ty(follows);
                    }
                };

                let typed_follower = self.decide_ty(follows);
                // the current scope should last till here.
                self.name_res.end_scope(scope);

                match typed_follower {
                    DecideTyReturn::Ok(typed_follower) => (
                        typed_follower.ty,
                        ir_typed_ast::ExprKind::Let(
                            ir_typed_ast::LetBinding {
                                pattern: typed_pattern,
                                args: IndexVec::from_raw_vec(typed_args),
                                value: typed_bound_value,
                            },
                            typed_follower,
                        ),
                    ),
                    other => return other,
                }
            }
            syntax::ExprKind::Then(e1, e2) => {
                // `(;) : unit -> 'a -> 'a`
                decide_ty_edsl! { self;
                    decide e1, e2;
                    unify {
                        e1: unit;
                    }
                    (|e2| (#e2), ir_typed_ast::ExprKind::Then(e1, e2))
                }
            }
            syntax::ExprKind::Var(var) => {
                let Some(typed_var) = self.name_res.get(*var) else {
                    self.throw_first_cause(error::UnboundIdent {
                        span: expr.span.as_user_defined().unwrap(),
                        var: *var,
                    });
                    return DecideTyReturn::Fail(());
                };
                let typed_var = ir_typed_ast::Ident::new(self.ctx.alloc_ident(typed_var));
                (typed_var.ty, ir_typed_ast::ExprKind::Var(typed_var))
            }
            syntax::ExprKind::App(fun, args) => {
                let fun_alias = fun;
                let fun = self.decide_ty(fun);
                let args = self.decide_ty_many(args);
                let ret_ty = self.new_ty_var(IntroducedTypeVar::app_impl(fun_alias));
                let typed_args =
                    self.complete_ty_list(&args, ty_var_subst::RecoverTyListKind::Function);
                decide_ty_edsl! { self;
                    interlude: unify {
                        fun: (#typed_args -> #ret_ty) else {
                            self.subst.to_mut().mark_as_incomplete(ret_ty);
                        };
                    }
                }
                decide_ty_edsl! { self;
                    postlude: for(fun, args)
                    (
                        || (#ret_ty),
                        ir_typed_ast::ExprKind::App(fun, IndexVec::from_raw_vec(args)),
                    )
                }
            }
            syntax::ExprKind::Tuple(exprs) => {
                let typed_exprs = self.decide_ty_many(exprs);
                let tys =
                    self.complete_ty_list(&typed_exprs, ty_var_subst::RecoverTyListKind::Tuple);
                let ty = Ty::mk_tuple(self.ctx, tys);
                decide_ty_edsl! { self;
                    postlude: for(typed_exprs)
                    (
                        || (#ty),
                        ir_typed_ast::ExprKind::Tuple(IndexVec::from_raw_vec(typed_exprs)),
                    )
                }
            }
            syntax::ExprKind::ArrayMake(e1, e2) => {
                // `Array.Make: int -> 'a -> 'a array`
                decide_ty_edsl! { self;
                    decide e1, e2;
                    unify {
                        e1: int;
                    }
                    (|e2| (#e2 array), ir_typed_ast::ExprKind::ArrayMake(e1, e2))
                }
            }
            syntax::ExprKind::Get(e1, e2) => {
                // `Get: 'a array -> int -> 'a`
                let ty = self.new_ty_var(IntroducedTypeVar::get_impl(e1));
                decide_ty_edsl! { self;
                    decide e1, e2;
                    unify {
                        e1: (#ty array) else {
                            self.subst.to_mut().mark_as_incomplete(ty);
                        };
                        e2: int;
                    }
                    (|| (#ty), ir_typed_ast::ExprKind::Get(e1, e2))
                }
            }
            syntax::ExprKind::Set(e1, e3) => {
                // We restrict the form of `e1` to be `Get(e1, e2)` here.
                let syntax::ExprKind::Get(e1, e2) = &e1.node else {
                    self.throw_first_cause(error::InvalidSetSyntax {
                        lhs: e1.span.as_user_defined().unwrap(),
                        note: AlwaysShow,
                    });
                    return DecideTyReturn::Fail(());
                };

                // `Set: 'a array -> int -> 'a -> unit`
                decide_ty_edsl! { self;
                    decide e1, e2, e3;
                    unify {
                        (e1, e3): ('a array, 'a);
                        e2: int;
                    }
                    (|| unit, ir_typed_ast::ExprKind::Set(e1, e2, e3))
                }
            }
        };
        // Do not add important implementations here, as this is a
        // successful continuation of each branch.

        let e = self.ctx.alloc_expr(Typed::new(Spanned { node, span }, ty));
        DecideTyReturn::Ok(ir_typed_ast::Expr::new(e))
    }

    /// Recursively infer the types of the given expressions.
    ///
    /// See [`Self::decide_ty`] for more details,
    /// and [`DecideTyManyReturn`] for the return type.
    fn decide_ty_many(&mut self, exprs: &[syntax::Expr<'ctx>]) -> DecideTyManyReturn<'ctx> {
        let recover_path =
            |this: &mut Self, typed: Vec<ir_typed_ast::Expr<'ctx>>, span, failure, it| {
                // Recovery path: store the type of the expression already seen
                // and the type of the expression in the recovery vector.

                let mut recovery = Vec::new();
                for t in typed {
                    recovery.push(Ok(t.ty));
                }
                recovery.push(if let DecideTyReturn::Recover(ty) = failure {
                    Ok(ty)
                } else {
                    Err(span)
                });
                for &expr in it {
                    // Already failed, so we just push recovery information.
                    recovery.push(this.decide_ty(expr).ty().ok_or(expr.span.as_user_defined()));
                }
                DecideTyManyReturn::Recover(recovery)
            };
        let mut typed = Vec::with_capacity(exprs.len());
        let mut it = exprs.iter();
        while let Some(expr) = it.next() {
            match self.decide_ty(expr) {
                DecideTyReturn::Ok(typed_expr) => {
                    typed.push(typed_expr);
                }
                failure => {
                    return recover_path(self, typed, expr.span.as_user_defined(), failure, it);
                }
            }
        }

        DecideTyManyReturn::Ok(typed)
    }

    #[inline(always)]
    #[track_caller]
    /// Calls [`unify`] with the current substitution environment.
    fn unify_with_current_subst(
        &mut self,
        lhs: Ty<'ctx>,
        rhs: Ty<'ctx>,
    ) -> Result<(), error::UnifyError<'ctx>> {
        unify(self.subst, lhs, rhs)
    }

    #[track_caller]
    /// Emits an error diagnostic and marks the type checker as failed.
    ///
    /// This function is used to throw the first cause of the error.
    fn throw_first_cause(&mut self, diag: impl errors::Diagnostic) {
        self.taint.fail();
        self.dcx.emit_err(diag);
    }

    #[inline(always)]
    fn new_ty_var(&mut self, introduced: ty_var_subst::IntroducedTypeVar<'ctx>) -> Ty<'ctx> {
        // `to_mut()` here is likely no-op, but it is needed to obtain a mutable reference.
        self.subst.to_mut().new_ty_var(self.ctx, introduced)
    }

    #[inline(always)]
    fn complete_ty_list(
        &mut self,
        result: &DecideTyManyReturn<'ctx>,
        kind: ty_var_subst::RecoverTyListKind,
    ) -> Vec<Ty<'ctx>> {
        result.complete_ty_list(|span| {
            self.new_ty_var(IntroducedTypeVar::recover_ty_list(span, kind))
        })
    }
}

#[derive(Debug)]
/// An underlying type for [`DecideTyReturn`] and [`DecideTyManyReturn`].
///
/// This type is introduced to call them structurally from [`decide_ty_edsl!`] macro.
enum ResultWithRecover<Ok, Recover, Fail> {
    /// The input is successfully typed.
    Ok(Ok),

    /// The input is not successfully typed, but the type is recoverable.
    Recover(Recover),

    /// The input is not successfully typed and the type is not recoverable.
    Fail(Fail),
}

/// A return type for [`TypeChecker::decide_ty`].
///
/// This type is used to return the result of type checking an expression.
/// It has three variants:
/// - [`Ok`]: The expression is successfully typed.
/// - [`RecoverTy`]: The expression is not successfully typed, but the type is recoverable.
/// - [`Fail`]: The expression is not successfully typed and the type is not recoverable.
///
/// This type is nominally different from [`Result`] or [`Option`], to prevent
/// the caller from using `?` operator or [`Result::unwrap`] method on it, since
/// the implementation of [`TypeChecker::decide_ty`] is crucial to the type system and should
/// not be overly simplified.
///
/// N.B., do not add or remove variants from this enum as `if let` statements
/// are used in the code. Instead, consider reimplementing the entire function.
///
/// [`Ok`]: ResultWithRecover::Ok
/// [`RecoverTy`]: ResultWithRecover::Recover
/// [`Fail`]: ResultWithRecover::Fail
type DecideTyReturn<'ctx> = ResultWithRecover<ir_typed_ast::Expr<'ctx>, Ty<'ctx>, ()>;

impl<'ctx> DecideTyReturn<'ctx> {
    /// Extracts the type of the expression if it is obtainable.
    fn ty(&self) -> Option<Ty<'ctx>> {
        match self {
            DecideTyReturn::Ok(e) => Some(e.ty),
            DecideTyReturn::Recover(ty) => Some(*ty),
            DecideTyReturn::Fail(()) => None,
        }
    }
}

/// A return type for [`TypeChecker::decide_ty_many`].
///
/// This type has two purposes:
/// - To return a vector of typed expressions, and
/// - To return a recovery information for the expressions in case of failure.
///
/// See [`DecideTyReturn`] for more details.
type DecideTyManyReturn<'ctx> = ResultWithRecover<
    Vec<ir_typed_ast::Expr<'ctx>>,
    Vec<Result<Ty<'ctx>, Option<Span>>>,
    std::convert::Infallible,
>;

impl<'ctx> DecideTyManyReturn<'ctx> {
    /// Returns a vector of types for the expressions.
    ///
    /// If an element of the types vector is unrecoverable, it will be replaced
    /// with a new type variable. `ctx` is needed to create new type variables.
    fn complete_ty_list(
        &self,
        mut new_ty_var: impl FnMut(Option<Span>) -> Ty<'ctx>,
    ) -> Vec<Ty<'ctx>> {
        match self {
            DecideTyManyReturn::Ok(exprs) => exprs.iter().map(|e| e.ty).collect(),
            DecideTyManyReturn::Recover(types) => types
                .iter()
                .map(|o| o.unwrap_or_else(&mut new_ty_var))
                .collect(),
            // This case should never happen, but current version of rustc
            // requires it to be handled as below.
            DecideTyManyReturn::Fail(infallible) => match *infallible {},
        }
    }
}
