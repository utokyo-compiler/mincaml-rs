use crate::{DiagInner, DiagnosticEmitter};

pub struct DiagContext<'dcx> {
    emitter: DiagnosticEmitter<'dcx>,
}

impl<'gcx> DiagContext<'gcx> {
    pub fn new(emitter: DiagnosticEmitter<'gcx>) -> Self {
        Self { emitter }
    }

    pub(crate) fn emit_diagnostic(&self, diag: DiagInner) {
        self.emitter.emit_diagnostic(diag);
    }
}
