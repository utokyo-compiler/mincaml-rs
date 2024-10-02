#[derive(Clone, Copy, Hash, PartialEq, Eq)]
/// Wrapper around `wasm_encoder::ValType`.
pub enum WasmPrimitiveTy {
    I32,
    F32,
    RefFn,
}

impl WasmPrimitiveTy {
    pub fn into_valtype(self) -> wasm_encoder::ValType {
        match self {
            Self::I32 => wasm_encoder::ValType::I32,
            Self::F32 => wasm_encoder::ValType::F32,
            Self::RefFn => wasm_encoder::ValType::Ref(wasm_encoder::RefType::FUNCREF),
        }
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Self::I32 | Self::F32)
    }

    /// Returns the size of the given type in bytes.
    pub fn size_of(&self) -> u32 {
        assert!(
            self.is_numeric(),
            "do not call `size_of` on non-numeric types"
        );
        4
    }
}

#[derive(Clone)]
pub enum WasmTy {
    Primitive(WasmPrimitiveTy),
    Many(Vec<WasmPrimitiveTy>),
}

impl WasmTy {
    pub fn into_iter_primitives(self) -> impl Iterator<Item = WasmPrimitiveTy> {
        match self {
            Self::Primitive(ty) => Vec::new().into_iter().chain(Some(ty)),
            Self::Many(tys) => tys.into_iter().chain(None),
        }
    }

    pub fn from_ty(ty: ir_closure::Ty) -> Self {
        match ty.kind() {
            ir_closure::TyKind::Unit => Self::Primitive(WasmPrimitiveTy::I32),
            ir_closure::TyKind::Bool => Self::Primitive(WasmPrimitiveTy::I32),
            ir_closure::TyKind::Int => Self::Primitive(WasmPrimitiveTy::I32),
            ir_closure::TyKind::Float => Self::Primitive(WasmPrimitiveTy::F32),
            ir_closure::TyKind::Fun(args, _ret) => Self::Many({
                let mut tys = Vec::new();
                tys.push(WasmPrimitiveTy::RefFn);
                tys.extend(
                    args.iter()
                        .flat_map(|arg| Self::from_ty(*arg).into_iter_primitives()),
                );
                tys
            }),
            ir_closure::TyKind::Tuple(..) => Self::Primitive(WasmPrimitiveTy::I32),
            ir_closure::TyKind::Array(..) => Self::Primitive(WasmPrimitiveTy::I32),
            ir_closure::TyKind::TyVar(..) => unreachable!(),
        }
    }

    pub fn as_primitive(&self) -> Option<&WasmPrimitiveTy> {
        if let Self::Primitive(v) = self {
            Some(v)
        } else {
            None
        }
    }
}
