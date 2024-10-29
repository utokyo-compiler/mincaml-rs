use anyhow::Result;
use data_structure::{
    index::{vec::IndexVec, Indexable},
    FxHashMap,
};
use wasm_encoder::{Instruction, MemArg};

use crate::{
    constant::{HEAP_PTR, MEM_ARG},
    expr,
    index::{LocalIdx, TypeIdx},
    program::{self, FnTypeSignature},
    ty::WasmTy,
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
    let results = WasmTy::from_ty(function.body().ty);

    for arg in args {
        // about parameters (arguments)

        let Some(wasm_ty) = WasmTy::from_ty(arg.ty) else {
            continue;
        };
        let val_type = wasm_ty;
        params.push(val_type);
        state.local_def.get(arg);
    }

    // closure calling convention
    if function.is_closure {
        // about parameters (arguments)

        // The last argument is the thunk pointer.
        let pointer_ty = WasmTy::I32;
        params.push(pointer_ty);
        let local = state.local_def.new_local(pointer_ty);

        // about locals

        state.instrs_load_from_tuple(
            args_via_closure.into_iter(),
            local,
            MemArg {
                offset: WasmTy::I32.size_of() as u64,
                ..MEM_ARG
            },
        );
    } else {
        #[cfg(debug_assertions)]
        assert!(args_via_closure.is_empty());
    }

    // about body

    // Generate instructions for the body.
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
    locals: FxHashMap<ir_closure::Ident<'ctx>, LocalIdx>,
    /// Local declarations.
    local_decls: IndexVec<LocalIdx, LocalDecl<'ctx>>,
}

#[derive(Debug, Clone, Copy)]
pub struct LocalDecl<'ctx> {
    pub wasm_ty: WasmTy,

    #[allow(dead_code)]
    /// The corresponding identifier of the local. For debugging purposes.
    ident: Option<ir_closure::Ident<'ctx>>,
}
impl Indexable<LocalIdx> for LocalDecl<'_> {}

impl<'ctx> LocalDef<'ctx> {
    /// Get the local index of the given identifier.
    /// If the identifier is not found, create a new local.
    ///
    /// Returns `None` if the given type is `Unit`. We don't create a local for
    /// a `Unit` typed value.
    pub fn get(&mut self, ident: ir_closure::Ident<'ctx>) -> Option<LocalIdx> {
        match self.locals.entry(ident) {
            std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                Some(*occupied_entry.get())
            }
            std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                let wasm_primitive_ty = WasmTy::from_ty(ident.ty)?;

                let local = self.local_decls.push(LocalDecl {
                    wasm_ty: wasm_primitive_ty,
                    ident: Some(ident),
                });
                vacant_entry.insert(local);
                Some(local)
            }
        }
    }

    /// Get the local index and the type of the given identifier.
    pub fn get_typed(&mut self, ident: ir_closure::Ident<'ctx>) -> Option<(LocalIdx, WasmTy)> {
        let local = self.get(ident)?;
        let wasm_ty = self.local_decls[local].wasm_ty;
        Some((local, wasm_ty))
    }

    /// Create a new local.
    pub fn new_local(&mut self, wasm_ty: WasmTy) -> LocalIdx {
        self.local_decls.push(LocalDecl {
            wasm_ty,
            ident: None,
        })
    }
}

// helper functions
impl<'ctx> State<'ctx> {
    /// Load the values from the tuple and set them in the local variables.
    pub fn instrs_load_from_tuple(
        &mut self,
        vars_filled: impl Iterator<Item = ir_closure::Ident<'ctx>>,
        local: LocalIdx,
        mut mem_arg: MemArg,
    ) {
        for (var, wasm_ty) in vars_filled.filter_map(|var| self.local_def.get_typed(var)) {
            self.instrs.push(Instruction::LocalGet(local.unwrap_idx()));
            match wasm_ty {
                WasmTy::I32 => self.instrs.push(Instruction::I32Load(mem_arg)),
                WasmTy::F32 => self.instrs.push(Instruction::F32Load(mem_arg)),
            };
            self.instrs.push(Instruction::LocalSet(var.unwrap_idx()));
            mem_arg.offset += wasm_ty.size_of() as u64;
        }
    }

    /// Allocate a new tuple on the heap and store the values in the tuple.
    pub fn instrs_allocate_tuple(&mut self, vars: impl Iterator<Item = (LocalIdx, WasmTy)>) {
        // return the address of the tuple
        self.instrs.push(Instruction::GlobalGet(HEAP_PTR));

        let mut mem_arg = MEM_ARG;
        for (local, wasm_ty) in vars {
            self.instrs.push(Instruction::GlobalGet(HEAP_PTR));
            self.instrs.push(Instruction::LocalGet(local.unwrap_idx()));

            match wasm_ty {
                WasmTy::I32 => self.instrs.push(Instruction::I32Store(mem_arg)),
                WasmTy::F32 => self.instrs.push(Instruction::F32Store(mem_arg)),
            };
            mem_arg.offset += wasm_ty.size_of() as u64;
        }
        self.instrs_grow_heap(mem_arg.offset as i32);
    }

    /// Grow the heap pointer by the given size.
    pub fn instrs_grow_heap(&mut self, size: i32) {
        if size == 0 {
            return;
        }
        self.instrs.push(Instruction::GlobalGet(HEAP_PTR));
        self.instrs.push(Instruction::I32Const(size));
        self.instrs.push(Instruction::I32Add);
        self.instrs.push(Instruction::GlobalSet(HEAP_PTR));
    }

    /// Compute the address of the element.
    pub fn instrs_calc_addr(&mut self, base: LocalIdx, index: LocalIdx, base_ty: WasmTy) {
        self.instrs.push(Instruction::LocalGet(base.unwrap_idx()));
        self.instrs.push(Instruction::LocalGet(index.unwrap_idx()));
        self.instrs
            .push(Instruction::I32Const(base_ty.size_of() as i32));
        self.instrs.push(Instruction::I32Mul);
        self.instrs.push(Instruction::I32Add);
    }
}
