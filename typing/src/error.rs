use errors::{into_diag_arg_using_display, DiagContext, Diagnostic};
use macros::Diagnostic;
use sourcemap::Span;
use ty::{Ty, TyVarId};

into_diag_arg_using_display!(Phase);

use crate::{unify::SpannedTy, Phase};

into_diag_arg_using_display!(CoarseExprKind);
#[derive(Debug)]
pub enum CoarseExprKind {
    Constant,
    Variable,
    Other,
}
impl<'a> From<&'a syntax::ExprKind<'_>> for CoarseExprKind {
    fn from(value: &'a syntax::ExprKind) -> Self {
        match value {
            syntax::ExprKind::Const(..) => CoarseExprKind::Constant,
            syntax::ExprKind::Var(..) => CoarseExprKind::Variable,
            _ => CoarseExprKind::Other,
        }
    }
}
impl<'a> From<&'a ir_typed_ast::ExprKind<'_>> for CoarseExprKind {
    fn from(value: &'a ir_typed_ast::ExprKind) -> Self {
        match value {
            ir_typed_ast::ExprKind::Const(..) => CoarseExprKind::Constant,
            ir_typed_ast::ExprKind::Var(..) => CoarseExprKind::Variable,
            _ => CoarseExprKind::Other,
        }
    }
}

impl std::fmt::Display for CoarseExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CoarseExprKind::Constant => write!(f, "this constant"),
            CoarseExprKind::Variable => write!(f, "the value"),
            CoarseExprKind::Other => write!(f, "this expression"),
        }
    }
}

#[derive(Debug, Diagnostic)]
#[diag(typing_fail)]
pub struct TypingError {
    pub phase: Phase,
}

#[derive(Debug)]
pub enum UnifyError<'ctx> {
    UnifyFailed {
        lhs: SpannedTy<'ctx>,
        rhs: SpannedTy<'ctx>,
        kind: UnifyFailKind<'ctx>,
    },
    AlreadyReported,
}

#[derive(Debug)]
pub enum UnifyFailKind<'ctx> {
    FunArgLenMismatch {
        left: usize,
        right: usize,
    },
    TupleLenMismatch,
    TupleElemsMismatch {
        mismatched: Vec<(Ty<'ctx>, Ty<'ctx>)>,
    },
    DifferentDiscriminants,
}

impl<'ctx> UnifyError<'ctx> {
    pub fn failed(lhs: SpannedTy<'ctx>, rhs: SpannedTy<'ctx>, kind: UnifyFailKind<'ctx>) -> Self {
        Self::UnifyFailed { lhs, rhs, kind }
    }

    pub fn report(dcx: &'ctx DiagContext<'ctx>, diag: impl Diagnostic) -> Self {
        dcx.emit_err(diag);
        Self::AlreadyReported
    }
}

#[derive(Diagnostic)]
#[diag(typing_occurck_failed)]
pub struct OccurckFailed<'ctx> {
    #[note]
    pub span: Option<Span>,
    pub var: TyVarId,
    pub ty: Ty<'ctx>,
    #[note(error)]
    pub error: (),
}

#[derive(Diagnostic)]
#[diag(typing_invalid_set_syntax)]
pub struct UnboundIdent<'ctx> {
    #[primary_span]
    #[label]
    pub span: Span,
    pub var: syntax::Ident<'ctx>,
}

#[derive(Diagnostic, Default)]
#[diag(typing_invalid_type_ascription)]
pub struct InvalidTypeAscription {
    #[note]
    note: (),
    #[note(bug_report)]
    bug_report: (),
}

#[derive(Diagnostic)]
#[diag(typing_invalid_set_syntax)]
pub struct InvalidSetSyntax {
    #[primary_span]
    #[label]
    pub lhs: Span,
    #[note(bug_report)]
    pub note: (),
}

#[derive(Diagnostic)]
#[diag(typing_general_expect)]
pub struct GeneralExpectFailure<'ctx> {
    #[primary_span]
    #[label]
    pub span: Span,
    pub expected_ty: Ty<'ctx>,
    pub found_ty: Ty<'ctx>,
    pub expr_kind: CoarseExprKind,
}

#[derive(Diagnostic)]
#[diag(typing_unify_then_else_different)]
pub struct ThenElseDifferent {
    #[label]
    pub whole_span: Span,
    #[label(reason)]
    pub then_span: Span,
    #[primary_span]
    pub else_span: Span,
}
