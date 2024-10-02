use anyhow::Result;
use data_structure::{
    index::{vec::IndexVec, Indexable},
    FxHashMap,
};

use crate::{
    expr,
    index::{LocalIdx, TypeIdx},
    program::{self, FnTypeSignature},
    ty::{WasmPrimitiveTy, WasmTy},
};

pub fn codegen<'ctx>(
    program_state: &mut program::State<'_, 'ctx>,
    function: ir_closure::FunctionDef<'ctx>,
) -> Result<()> {
    let mut state = State {
        locals: FxHashMap::default(),
        local_decls: IndexVec::new(),
        instrs: Vec::new(),
    };

    let mut params = Vec::new();
    // closure calling convention
    for args in function.args_via_closure.into_iter().chain(function.args) {
        for wasm_ty in WasmTy::from_ty(args.ty).into_iter_primitives() {
            let val_type = wasm_ty;
            params.push(val_type);
            state.new_local_from(val_type, Some(args));
        }
    }

    expr::codegen(program_state, &mut state, &function.body)?;

    let results = WasmTy::from_ty(function.body.ty)
        .into_iter_primitives()
        .collect();

    let function_def = FunctionDef {
        local_decls: state.local_decls,
        sig: program_state
            .signature_interner
            .intern(FnTypeSignature { params, results }),
        instrs: state.instrs,
    };
    program_state.push_function_def(function_def);

    Ok(())
}

pub struct FunctionDef<'ctx> {
    pub local_decls: IndexVec<LocalIdx, LocalDecl<'ctx>>,
    pub sig: TypeIdx,
    pub instrs: Vec<wasm_encoder::Instruction<'static>>,
}

pub struct State<'ctx> {
    /// Mapping from identifiers to local indices created already.
    ///
    /// This is a cache for `get_local`.
    locals: FxHashMap<ir_closure::Ident<'ctx>, LocalGroup>,
    /// Local declarations.
    local_decls: IndexVec<LocalIdx, LocalDecl<'ctx>>,

    /// Instructions built so far.
    instrs: Vec<wasm_encoder::Instruction<'static>>,
}

#[derive(Clone, Copy)]
pub struct LocalDecl<'ctx> {
    pub wasm_ty: WasmPrimitiveTy,

    #[allow(dead_code)]
    /// The corresponding identifier of the local. For debugging purposes.
    ident: Option<ir_closure::Ident<'ctx>>,
}
impl Indexable<LocalIdx> for LocalDecl<'_> {}

impl<'ctx> State<'ctx> {
    /// Get the local index of the given identifier. If the identifier is not found, create a new local.
    pub fn get_local(&mut self, ident: ir_closure::Ident<'ctx>) -> LocalGroup {
        if let Some(local_group) = self.locals.get(&ident) {
            return local_group.clone();
        }

        let local_group = match WasmTy::from_ty(ident.ty) {
            WasmTy::Primitive(wasm_primitive_ty) => {
                LocalGroup::Single(self.new_local_from(wasm_primitive_ty, Some(ident)))
            }
            WasmTy::Many(vec) => LocalGroup::Many(
                vec.into_iter()
                    .map(|wasm_primitive_ty| self.new_local_from(wasm_primitive_ty, Some(ident)))
                    .collect(),
            ),
        };
        self.locals.insert(ident, local_group.clone());
        local_group
    }

    /// Create a new local.
    pub fn new_local(&mut self, wasm_ty: WasmPrimitiveTy) -> LocalIdx {
        self.new_local_from(wasm_ty, None)
    }

    fn new_local_from(
        &mut self,
        wasm_ty: WasmPrimitiveTy,
        ident: Option<ir_closure::Ident<'ctx>>,
    ) -> LocalIdx {
        self.local_decls.push(LocalDecl { wasm_ty, ident })
    }

    /// Push a raw instruction.
    pub fn push_raw(&mut self, value: wasm_encoder::Instruction<'static>) {
        self.instrs.push(value)
    }

    pub fn get_local_decl(&self, index: LocalIdx) -> &LocalDecl {
        &self.local_decls[index]
    }
}

#[derive(Clone)]
/// A group of local variables.
pub enum LocalGroup {
    Single(LocalIdx),

    /// There are multiple local variables corresponding to the given identifier.
    /// This is the case when the identifier points to a closure.
    Many(Vec<LocalIdx>),
}

impl LocalGroup {
    pub fn as_single(&self) -> Option<LocalIdx> {
        if let Self::Single(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_many(&self) -> Option<&[LocalIdx]> {
        if let Self::Many(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn expect_single(&self) -> Result<LocalIdx> {
        self.as_single()
            .ok_or_else(|| anyhow::anyhow!("closure is not allowed here"))
    }

    pub fn iter(&self) -> impl Iterator<Item = LocalIdx> + '_ {
        match self {
            Self::Single(v) => [].iter().copied().chain(Some(*v)),
            Self::Many(v) => v.iter().copied().chain(None),
        }
    }
}
