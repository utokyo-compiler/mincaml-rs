use anyhow::Result;
use data_structure::{
    arena::TypedArena,
    index::{vec::Idx, Indexable},
    FxHashMap,
};
use wasm_encoder::{EntityType, ImportSection, Module};

use crate::{
    function,
    index::{FuncIdx, TypeIdx},
    ty::WasmPrimitiveTy,
};

pub fn codegen(asm_virtual_prog: ir_closure::Program<'_>) -> Result<Vec<u8>> {
    let signarute_arena = TypedArena::new();

    let program = {
        let signature_interner = SignatureInterner::new(&signarute_arena);
        let import_fns = Vec::new();

        let mut state = State {
            import_fns_len: import_fns.len(),
            signature_interner,
            functions: Vec::new(),
        };

        for function in asm_virtual_prog.functions {
            function::codegen(&mut state, function)?;
        }

        Program {
            import_fns,
            functions: state.functions,
        }
    };

    let mut module_builder = Module::new();

    // write type section
    let mut type_section = wasm_encoder::TypeSection::new();
    for signature in signarute_arena.into_vec() {
        type_section.function(
            signature
                .params
                .into_iter()
                .map(WasmPrimitiveTy::into_valtype),
            signature
                .results
                .into_iter()
                .map(WasmPrimitiveTy::into_valtype),
        );
    }

    // write import section
    for ImportFn { module, field, sig } in &program.import_fns {
        let mut import_section = ImportSection::new();
        import_section.import(module, field, EntityType::Function(sig.unwrap_idx()));
        module_builder.section(&import_section);
    }

    // write function section
    let mut function_section = wasm_encoder::FunctionSection::new();
    for function in &program.functions {
        function_section.function(function.sig.unwrap_idx());
    }
    module_builder.section(&function_section);

    // write table section
    let mut table_section = wasm_encoder::TableSection::new();
    table_section.table(wasm_encoder::TableType {
        element_type: wasm_encoder::RefType::FUNCREF,
        table64: false,
        minimum: 1,
        maximum: None,
        shared: false,
    });
    module_builder.section(&table_section);

    // write memory section
    let mut memory_section = wasm_encoder::MemorySection::new();
    memory_section.memory(wasm_encoder::MemoryType {
        minimum: 1,
        maximum: None,
        memory64: false,
        shared: false,
        page_size_log2: None,
    });
    module_builder.section(&memory_section);

    // write code section
    let mut code_section = wasm_encoder::CodeSection::new();
    for function in &program.functions {
        let mut function_builder = wasm_encoder::Function::new_with_locals_types(
            function
                .local_decls
                .iter()
                .map(|local_decl| local_decl.wasm_ty.into_valtype()),
        );
        for instr in &function.instrs {
            function_builder.instruction(instr);
        }
        code_section.function(&function_builder);
    }
    module_builder.section(&code_section);

    Ok(module_builder.finish())
}

pub struct Program<'ctx> {
    pub import_fns: Vec<ImportFn>,
    pub functions: Vec<function::FunctionDef<'ctx>>,
}

pub struct State<'arena, 'ctx> {
    pub import_fns_len: usize,
    pub functions: Vec<function::FunctionDef<'ctx>>,
    pub signature_interner: SignatureInterner<'arena>,
}

impl<'ctx> State<'_, 'ctx> {
    pub fn push_function_def(&mut self, value: function::FunctionDef<'ctx>) {
        self.functions.push(value)
    }

    /// Create a new function index.
    pub fn new_fn_index(&self, index: ir_closure::FnIndex) -> FuncIdx {
        FuncIdx::new(self.import_fns_len + index.index())
    }
}

pub struct ImportFn {
    pub module: &'static str,
    pub field: &'static str,
    pub sig: TypeIdx,
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct FnTypeSignature {
    pub params: Vec<WasmPrimitiveTy>,
    pub results: Vec<WasmPrimitiveTy>,
}

impl FnTypeSignature {
    #[allow(dead_code)]
    #[inline]
    pub fn new(
        params: impl IntoIterator<Item = WasmPrimitiveTy>,
        results: impl IntoIterator<Item = WasmPrimitiveTy>,
    ) -> Self {
        Self {
            params: params.into_iter().collect(),
            results: results.into_iter().collect(),
        }
    }

    pub fn from_results(results: Vec<WasmPrimitiveTy>) -> Self {
        Self {
            params: Vec::new(),
            results,
        }
    }
}

pub struct SignatureInterner<'arena> {
    map: FxHashMap<&'arena FnTypeSignature, TypeIdx>,
    signatures_arena: &'arena TypedArena<FnTypeSignature>,
    next_index: usize,
}
impl Indexable<TypeIdx> for FnTypeSignature {}

impl<'arena> SignatureInterner<'arena> {
    fn new(signatures_arena: &'arena TypedArena<FnTypeSignature>) -> Self {
        Self {
            map: FxHashMap::default(),
            signatures_arena,
            next_index: 0,
        }
    }

    pub fn intern(&mut self, signature: FnTypeSignature) -> TypeIdx {
        if let Some(&idx) = self.map.get(&signature) {
            return idx;
        }
        let idx = TypeIdx::new(self.next_index);
        self.next_index += 1;
        self.map.insert(self.signatures_arena.alloc(signature), idx);
        idx
    }
}
