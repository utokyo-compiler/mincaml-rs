use crate::{
    Diag, DiagInner, DiagMessage, EarlyDiag, EarlyDiagnosticEmitter, Level, SelectedEmitter,
};

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

impl DiagContext<Early<'static>> {
    pub fn new(emitter: EarlyDiagnosticEmitter) -> Self {
        Self { emitter }
    }

    pub(crate) fn early_emit_diagnostic(&self, diag: DiagInner) {
        self.emitter.emit_diagnostic(diag);
    }

    pub fn struct_err<'a>(&'a self, msg: impl Into<DiagMessage<'a>>) -> EarlyDiag<'_> {
        EarlyDiag::new(self, Level::Error, msg)
    }
}

impl<'dcx> DiagContext<Late<'dcx>> {
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

    pub fn struct_err(&'dcx self, msg: impl Into<DiagMessage<'dcx>>) -> Diag<'_> {
        Diag::new(self, Level::Error, msg)
    }
}
