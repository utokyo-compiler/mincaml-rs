#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
/// Wrapper around `wasm_encoder::ValType`.
pub enum WasmTy {
    I32,
    F32,
}

impl WasmTy {
    pub fn into_valtype(self) -> wasm_encoder::ValType {
        match self {
            Self::I32 => wasm_encoder::ValType::I32,
            Self::F32 => wasm_encoder::ValType::F32,
        }
    }

    /// Returns the size of the given type in bytes.
    pub fn size_of(&self) -> u32 {
        4
    }

    /// Converts the given type into a corresponding type.
    ///
    /// Returns `None` if the given type is `Unit`.
    pub fn from_ty(ty: ir_closure::Ty) -> Option<Self> {
        Some(match ty.kind() {
            ir_closure::TyKind::Unit => return None,
            ir_closure::TyKind::Bool => Self::I32,
            ir_closure::TyKind::Int => Self::I32,
            ir_closure::TyKind::Float => Self::F32,
            ir_closure::TyKind::Fun(..) => Self::I32,
            ir_closure::TyKind::Tuple(..) => Self::I32,
            ir_closure::TyKind::Array(..) => Self::I32,
            ir_closure::TyKind::TyVar(..) => unreachable!(),
        })
    }
}
