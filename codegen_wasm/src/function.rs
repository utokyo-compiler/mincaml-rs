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
    mut function: ir_closure::FunctionDef<'ctx>,
) -> Result<()> {
    let mut state = State {
        local_def: LocalDef {
            locals: FxHashMap::default(),
            local_decls: IndexVec::new(),
        },
        instrs: Vec::new(),
    };

    let args = std::mem::take(&mut function.args);
    let args_via_closure = std::mem::take(&mut function.args_via_closure);

    let mut params = Vec::new();
    let results = WasmTy::ty_to_primitive_iter(function.body().ty).collect();

    // closure calling convention
    for arg in args_via_closure.into_iter().chain(args) {
        for wasm_ty in WasmTy::ty_to_primitive_iter(arg.ty) {
            let val_type = wasm_ty;
            params.push(val_type);
        }
        state.local_def.get(arg);
    }

    expr::codegen(program_state, &mut state, function.body())?;
    state.instrs.push(wasm_encoder::Instruction::End);

    let function_def = FunctionDef {
        local_decls: state.local_def.local_decls,
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
    /// Local declarations.
    pub local_def: LocalDef<'ctx>,

    /// Instructions built so far.
    pub instrs: Vec<wasm_encoder::Instruction<'static>>,
}

pub struct LocalDef<'ctx> {
    /// Mapping from identifiers to local indices created already.
    ///
    /// This is a cache for `get_local`.
    locals: FxHashMap<ir_closure::Ident<'ctx>, LocalGroup>,
    /// Local declarations.
    local_decls: IndexVec<LocalIdx, LocalDecl<'ctx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct LocalDecl<'ctx> {
    pub wasm_ty: WasmPrimitiveTy,

    #[allow(dead_code)]
    /// The corresponding identifier of the local. For debugging purposes.
    ident: Option<ir_closure::Ident<'ctx>>,
}
impl Indexable<LocalIdx> for LocalDecl<'_> {}

impl<'ctx> LocalDef<'ctx> {
    /// Get the local index of the given identifier. If the identifier is not found, create a new local.
    pub fn get(&mut self, ident: ir_closure::Ident<'ctx>) -> Option<&'_ LocalGroup> {
        match self.locals.entry(ident) {
            std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                Some(occupied_entry.into_mut())
            }
            std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                let local_group = match WasmTy::from_ty(ident.ty)? {
                    WasmTy::Primitive(wasm_primitive_ty) => LocalGroup::Single({
                        self.local_decls.push(LocalDecl {
                            wasm_ty: wasm_primitive_ty,
                            ident: Some(ident),
                        })
                    }),
                    WasmTy::Many(vec) => LocalGroup::Many(
                        vec.into_iter()
                            .map(|wasm_primitive_ty| {
                                self.local_decls.push(LocalDecl {
                                    wasm_ty: wasm_primitive_ty,
                                    ident: Some(ident),
                                })
                            })
                            .collect(),
                    ),
                };
                Some(vacant_entry.insert(local_group))
            }
        }
    }

    /// Create a new local.
    pub fn new_local(&mut self, wasm_ty: WasmPrimitiveTy) -> LocalIdx {
        self.local_decls.push(LocalDecl {
            wasm_ty,
            ident: None,
        })
    }

    pub fn get_decl(&self, index: LocalIdx) -> &LocalDecl {
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
