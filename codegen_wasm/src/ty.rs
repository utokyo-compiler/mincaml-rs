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
/// Represents a sequence of primitive types.
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

    /// Converts the given type into a corresponding sequence of primitive types.
    ///
    /// Returns `None` if the given type is `Unit`.
    pub fn from_ty(ty: ir_closure::Ty) -> Option<Self> {
        Some(match ty.kind() {
            ir_closure::TyKind::Unit => return None,
            ir_closure::TyKind::Bool => Self::Primitive(WasmPrimitiveTy::I32),
            ir_closure::TyKind::Int => Self::Primitive(WasmPrimitiveTy::I32),
            ir_closure::TyKind::Float => Self::Primitive(WasmPrimitiveTy::F32),
            ir_closure::TyKind::Fun(args, _ret) => Self::Many({
                let mut tys = Vec::new();
                tys.push(WasmPrimitiveTy::RefFn);
                tys.extend(
                    args.iter()
                        .flat_map(|arg| Self::from_ty(*arg))
                        .flat_map(Self::into_iter_primitives),
                );
                tys
            }),
            ir_closure::TyKind::Tuple(..) => Self::Primitive(WasmPrimitiveTy::I32),
            ir_closure::TyKind::Array(..) => Self::Primitive(WasmPrimitiveTy::I32),
            ir_closure::TyKind::TyVar(..) => unreachable!(),
        })
    }

    /// Converts the given type into a corresponding sequence of primitive types.
    pub fn ty_to_primitive_iter(ty: ir_closure::Ty) -> impl Iterator<Item = WasmPrimitiveTy> {
        Self::from_ty(ty)
            .into_iter()
            .flat_map(Self::into_iter_primitives)
    }

    pub fn as_primitive(&self) -> Option<WasmPrimitiveTy> {
        if let Self::Primitive(v) = self {
            Some(*v)
        } else {
            None
        }
    }
}
