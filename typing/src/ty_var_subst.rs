//! TyVar substitution
//!
//! The difference from the original is that the original implementation stores
//! type-variable information in `Type.t`, this implementation manages it via `Env`.

use data_structure::{FxHashMap, FxHashSet};
use errors::into_diag_arg_using_display;
use macros::{Diagnostic, Subdiagnostic};
use sourcemap::{Span, Spanned};
use ty::{Ty, TyVarId};

use crate::{error::CoarseExprKind, Context};

#[derive(Clone)]
/// Mapping from type variables to types.
pub struct Env<'ctx> {
    inner: FxHashMap<TyVarId, Ty<'ctx>>,
    info: FxHashMap<TyVarId, IntroducedTypeVar<'ctx>>,
}

impl<'ctx> Env<'ctx> {
    pub fn new() -> Self {
        Self {
            inner: Default::default(),
            info: Default::default(),
        }
    }
    pub fn get(&self, var: TyVarId) -> Option<Ty<'ctx>> {
        self.inner.get(&var).copied()
    }

    pub fn merge(&mut self, var: TyVarId, ty: Ty<'ctx>) {
        self.inner.insert(var, ty);
    }

    pub fn deref_ty_var(
        &self,
        ctx: &'ctx Context<'ctx>,
        e: &mut ir_typed_ast::Expr<'ctx>,
    ) -> Result<(), DerefTyError<'ctx>> {
        use ir_typed_ast::MutVisitor;
        let mut visitor = DerefTyVisitor {
            env: self,
            ctx,
            error: DerefTyError::default(),
        };
        visitor.visit_expr(e);
        if visitor.error.is_error() {
            Err(visitor.error)
        } else {
            Ok(())
        }
    }
}

/// Part of [`Env::deref_ty_var`].
struct DerefTyVisitor<'a, 'ctx> {
    env: &'a Env<'ctx>,
    ctx: &'ctx Context<'ctx>,
    error: DerefTyError<'ctx>,
}

impl<'ctx> ir_typed_ast::MutVisitor<'ctx> for DerefTyVisitor<'_, 'ctx> {
    fn visit_ty(&mut self, ty: &mut Ty<'ctx>) {
        *ty = self.deref_ty(*ty);
    }
}

impl<'ctx> DerefTyVisitor<'_, 'ctx> {
    fn deref_ty(&mut self, mut ty: Ty<'ctx>) -> Ty<'ctx> {
        loop {
            match ty.kind() {
                ty::TyKind::TyVar(var) => {
                    if let Some(new_ty) = self.env.get(*var) {
                        ty = new_ty;
                    } else {
                        // If the type variable is not found, it means that
                        // the type variable is not yet unified.
                        if self.error.seen.insert(*var) {
                            self.error.count += 1;
                            if let Some(IntroducedTypeVar {
                                reason: Some(reason),
                                ..
                            }) = self.env.info.get(var)
                            {
                                self.error.introduced_reason.push(*reason);
                            }
                        }

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

#[derive(Diagnostic, Default)]
#[diag(typing_typevar_remain)]
pub struct DerefTyError<'ctx> {
    count: usize,
    #[skip_arg]
    seen: FxHashSet<TyVarId>,
    #[subdiagnostic]
    introduced_reason: Vec<NewTypeVarReason<'ctx>>,
}

impl<'ctx> DerefTyError<'ctx> {
    fn is_error(&self) -> bool {
        self.count > 0
    }
}

impl<'ctx> Env<'ctx> {
    pub fn mark_as_incomplete(&mut self, ty: Ty<'ctx>) {
        if let Some(var) = ty.as_ty_var() {
            self.info.entry(var).or_default().incomplete_unification = true;
        }
    }

    pub fn new_ty_var(
        &mut self,
        ctx: &'ctx Context<'ctx>,
        introduced: IntroducedTypeVar<'ctx>,
    ) -> Ty<'ctx> {
        let ty = Ty::mk_ty_var(ctx);
        let var = ty.as_ty_var().expect("new_ty_var should return a TyVar");
        self.info.insert(var, introduced);
        ty
    }
}

#[derive(Debug, Default, Clone, Copy)]
/// The type variable information.
///
/// Implementation for this struct should be synchronized with [`decide_ty`].
///
/// [`decide_ty`]: crate::typeck::TypeChecker::decide_ty
pub struct IntroducedTypeVar<'ctx> {
    reason: Option<NewTypeVarReason<'ctx>>,

    /// Whether the type variable is introduced for incomplete unification.
    ///
    /// This field is only used for debugging purposes currently.
    incomplete_unification: bool,
}

impl<'ctx> IntroducedTypeVar<'ctx> {
    #[allow(dead_code)]
    pub fn new_unknown() -> Self {
        Self {
            reason: None,
            incomplete_unification: false,
        }
    }
    /// The type variable is introduced because of undecidable polymorphism.
    pub fn undecidable_polymorphism(unify_failed: syntax::Expr<'ctx>) -> Self {
        Self {
            reason: Some(NewTypeVarReason::RecoverUndecidablePolymorphism {
                span_unify_failed: unify_failed.span.as_user_defined(),
                kind: unify_failed.kind().into(),
            }),
            incomplete_unification: true,
        }
    }
    /// The type variable is introduced for let body of this identifier.
    pub fn let_var(ident_to_be_defined: Spanned<syntax::Ident<'ctx>>) -> Self {
        Self {
            reason: Some(NewTypeVarReason::LetVar {
                def_site: ident_to_be_defined.span.as_user_defined(),
            }),
            incomplete_unification: false,
        }
    }
    /// The type variable is introduced for let argument of this identifier.
    pub fn let_arg(ident_to_be_defined: Spanned<syntax::Ident<'ctx>>) -> Self {
        Self {
            reason: Some(NewTypeVarReason::LetArg {
                def_site: ident_to_be_defined.span.as_user_defined(),
            }),
            incomplete_unification: false,
        }
    }
    /// The type variable is introduced for tuple-pattern element of this identifier.
    pub fn let_tuple(ident_to_be_defined: Spanned<syntax::Ident<'ctx>>) -> Self {
        Self {
            reason: Some(NewTypeVarReason::LetTuplePattern {
                def_site: ident_to_be_defined.span.as_user_defined(),
            }),
            incomplete_unification: false,
        }
    }
    /// The type variable is introduced for implementation of `Get` syntax.
    pub fn get_impl(array_type_expr: syntax::Expr<'ctx>) -> Self {
        Self {
            reason: Some(NewTypeVarReason::GetImpl {
                array_type_expr: array_type_expr.span.as_user_defined(),
                kind: array_type_expr.kind().into(),
            }),
            incomplete_unification: false,
        }
    }
    /// The type variable is introduced while recovering type list.
    pub fn recover_ty_list(span: Option<Span>, kind: RecoverTyListKind) -> Self {
        Self {
            reason: Some(NewTypeVarReason::RecoverTyList { span, kind }),
            incomplete_unification: true,
        }
    }
    /// The type variable is introduced for implementation of `App` syntax.
    pub fn app_impl(fun: syntax::Expr<'ctx>) -> Self {
        Self {
            reason: Some(NewTypeVarReason::AppImpl {
                app_fun_span: fun.span.as_user_defined(),
            }),
            incomplete_unification: false,
        }
    }
}

#[derive(Debug, Clone, Copy, Subdiagnostic)]
/// The reason why the type variable is introduced.
///
/// This is used for debugging or diagnostic purposes.
enum NewTypeVarReason<'ctx> {
    #[note(typing_typevar_recover_polymorphism)]
    RecoverUndecidablePolymorphism {
        #[primary_span]
        span_unify_failed: Option<Span>,
        kind: CoarseExprKind,
    },
    #[note(typing_typevar_let_var)]
    LetVar {
        #[primary_span]
        def_site: Option<Span>,
    },
    #[note(typing_typevar_let_arg)]
    LetArg {
        #[primary_span]
        def_site: Option<Span>,
    },
    #[note(typing_typevar_let_tuple_pat)]
    LetTuplePattern {
        /// The identifier is used for tuple-pattern element.
        #[primary_span]
        def_site: Option<Span>,
    },
    #[note(typing_typevar_get_impl)]
    GetImpl {
        /// The expression typed `'a array`.
        #[primary_span]
        array_type_expr: Option<Span>,
        kind: CoarseExprKind,
    },
    #[note(typing_typevar_recover_list)]
    RecoverTyList {
        #[primary_span]
        span: Option<Span>,
        kind: RecoverTyListKind,
    },
    #[note(typing_typevar_app_impl)]
    AppImpl {
        #[primary_span]
        app_fun_span: Option<Span>,
    },
    /// `'ctx` is used so frequently but this enum does not have it.
    _Phantom(#[skip_arg] std::marker::PhantomData<&'ctx ()>),
}

into_diag_arg_using_display!(RecoverTyListKind);
#[derive(Debug, Clone, Copy)]
pub enum RecoverTyListKind {
    Function,
    Tuple,
}
impl std::fmt::Display for RecoverTyListKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RecoverTyListKind::Function => write!(f, "this function"),
            RecoverTyListKind::Tuple => write!(f, "this tuple"),
        }
    }
}
