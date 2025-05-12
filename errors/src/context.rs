use crate::{
    Diag, DiagInner, DiagMessage, Diagnostic, EarlyDiag, EarlyDiagnosticEmitter, Level,
    SelectedEmitter,
};

/// A switch between early and late diagnostics.
///
/// This is used to determine the type of source map to use in the
/// [`DiagContext`] and [`EarlyDiagContext`].
///
/// [`DiagContext`]: crate::DiagContext
/// [`EarlyDiagContext`]: crate::EarlyDiagContext
pub trait EarlySwitch {
    type SourceMap;
}

pub struct Early<'dummy> {
    _phantom: std::marker::PhantomData<&'dummy ()>,
}
pub struct Late<'dcx> {
    _phantom: std::marker::PhantomData<&'dcx ()>,
}

impl EarlySwitch for Early<'_> {
    type SourceMap = ();
}

impl<'dcx> EarlySwitch for Late<'dcx> {
    type SourceMap = &'dcx sourcemap::MultipleInput;
}

pub struct DiagContext<Switch: EarlySwitch> {
    emitter: SelectedEmitter<Switch>,
}

impl crate::EarlyDiagContext {
    pub fn new(emitter: EarlyDiagnosticEmitter) -> Self {
        Self { emitter }
    }

    pub(crate) fn early_emit_diagnostic(&self, diag: DiagInner) {
        self.emitter.emit_diagnostic(diag);
    }

    pub fn struct_err<'a>(&'a self, msg: impl Into<DiagMessage<'a>>) -> EarlyDiag<'a> {
        EarlyDiag::new(self, Level::Error, msg)
    }
}

impl<'dcx> crate::DiagContext<'dcx> {
    pub fn from_early(
        early: DiagContext<Early<'_>>,
        source_map: &'dcx sourcemap::MultipleInput,
    ) -> Self {
        Self {
            emitter: SelectedEmitter::from_early(early.emitter, source_map),
        }
    }

    pub(crate) fn emit_diagnostic(&self, diag: DiagInner) {
        self.emitter.emit_diagnostic(diag);
    }

    pub fn struct_err(&'dcx self, msg: impl Into<DiagMessage<'dcx>>) -> Diag<'dcx> {
        Diag::new(self, Level::Error, msg)
    }

    #[track_caller]
    pub fn create_err(&'dcx self, err: impl Diagnostic) -> Diag<'dcx> {
        err.into_diag(self, Level::Error)
    }

    #[track_caller]
    pub fn emit_err(&'dcx self, err: impl Diagnostic) {
        self.create_err(err).emit();
    }
}
