use crate::{DiagInner, DiagnosticEmitter};

pub struct DiagContext {
    emitter: DiagnosticEmitter,
}

impl DiagContext {
    pub fn new(emitter: DiagnosticEmitter) -> Self {
        Self { emitter }
    }

    pub(crate) fn emit_diagnostic(&self, diag: DiagInner) {
        self.emitter.emit_diagnostic(diag);
    }
}
