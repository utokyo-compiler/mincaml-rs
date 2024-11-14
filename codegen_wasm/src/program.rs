use std::borrow::Cow;

use anyhow::Result;
use data_structure::{
    arena::TypedArena,
    index::{vec::Idx, Indexable},
    interning::Interned,
    FxHashMap,
};
use ir_closure::ImportedFnName;
use wasm_encoder::{ConstExpr, EntityType, ImportSection, Module};

use crate::{
    function,
    index::{FuncIdx, TypeIdx},
    ty::WasmTy,
};

pub fn codegen<'ctx>(
    closure_prog: ir_closure::Program<'ctx>,
    typed_interface: &middleware::Mli<'ctx>,
) -> Result<Vec<u8>> {
    let signarute_arena = TypedArena::new();
    let mut main_fn_idx = None;

    let program = {
        let mut signature_interner = SignatureInterner::new(&signarute_arena);

        use ir_closure::Visitor;
        struct VisitImportedFn<'ctx> {
            fn_names: Vec<Interned<'ctx, str>>,
        }
        impl<'ctx> Visitor<'ctx> for VisitImportedFn<'ctx> {
            fn visit_imported_function(&mut self, fn_name: &Interned<'ctx, str>) {
                self.fn_names.push(*fn_name);
            }
        }
        let mut visitor = VisitImportedFn {
            fn_names: Vec::new(),
        };
        visitor.visit_program(&closure_prog);

        let import_fns = visitor
            .fn_names
            .into_iter()
            .map(|fn_name| ImportFn {
                namespace: NameSpace::Intrinsic { fn_name },
                sig: signature_interner.intern(FnTypeSignature::from_fun_ty(
                    typed_interface.find_declaration(fn_name.0).unwrap().ty,
                )),
            })
            .collect();

        let mut state = State {
            import_fns,
            signature_interner,
            functions: Vec::new(),
        };

        for (fn_index, function) in closure_prog.functions.into_iter_enumerated() {
            if function.name == ir_closure::FnName::MAIN_FN_NAME {
                main_fn_idx = Some(state.get_func_idx_from_fn_index(fn_index));
            }
            function::codegen(&mut state, function)?;
        }

        Program {
            import_fns: state.import_fns,
            functions: state.functions,
        }
    };

    let mut module_builder = Module::new();

    // write type section
    let mut type_section = wasm_encoder::TypeSection::new();
    for signature in signarute_arena.into_vec() {
        type_section.function(
            signature.params.into_iter().map(WasmTy::into_valtype),
            signature.results.map(WasmTy::into_valtype),
        );
    }
    module_builder.section(&type_section);

    // write import section
    let mut import_section = ImportSection::new();
    for ImportFn { namespace, sig } in &program.import_fns {
        let (module, field) = namespace.into_wasm();
        import_section.import(module, &field, EntityType::Function(sig.unwrap_idx()));
    }
    module_builder.section(&import_section);

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
        minimum: (program.import_fns.len() + program.functions.len()) as u64,
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

    // write global section
    let mut global_section = wasm_encoder::GlobalSection::new();
    global_section.global(
        wasm_encoder::GlobalType {
            val_type: wasm_encoder::ValType::I32,
            mutable: true,
            shared: false,
        },
        &wasm_encoder::ConstExpr::i32_const(0),
    );
    module_builder.section(&global_section);

    // write export section
    let mut export_section = wasm_encoder::ExportSection::new();
    // export main function as default
    if let Some(main_fn_idx) = main_fn_idx {
        export_section.export("", wasm_encoder::ExportKind::Func, main_fn_idx.unwrap_idx());
    }
    module_builder.section(&export_section);

    // write element section
    let mut element_section = wasm_encoder::ElementSection::new();
    let offset = program.import_fns.len();
    let range = offset as u32..offset as u32 + program.functions.len() as u32;
    element_section.active(
        None,
        &ConstExpr::i32_const(offset as i32),
        wasm_encoder::Elements::Functions(range.into_iter().collect::<Vec<_>>().as_slice()),
    );
    module_builder.section(&element_section);

    // write code section
    let mut code_section = wasm_encoder::CodeSection::new();
    for function in &program.functions {
        let mut function_builder = wasm_encoder::Function::new_with_locals_types(
            function
                .local_decls
                .iter()
                .skip(function.arg_count)
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
    pub import_fns: Vec<ImportFn<'ctx>>,
    pub functions: Vec<function::FunctionDef<'ctx>>,
}

pub struct State<'arena, 'ctx> {
    pub import_fns: Vec<ImportFn<'ctx>>,
    pub functions: Vec<function::FunctionDef<'ctx>>,
    pub signature_interner: SignatureInterner<'arena>,
}

impl<'ctx> State<'_, 'ctx> {
    pub fn push_function_def(&mut self, value: function::FunctionDef<'ctx>) {
        self.functions.push(value)
    }

    /// Create a new function index.
    pub fn get_func_idx(&self, function: ir_closure::FunctionInstance<'ctx>) -> FuncIdx {
        match function {
            ir_closure::FunctionInstance::Defined(fn_index) => {
                self.get_func_idx_from_fn_index(fn_index)
            }
            ir_closure::FunctionInstance::Imported(fn_name) => self
                .find_imported_fn(fn_name)
                .map(FuncIdx::new)
                .unwrap_or_else(|| panic!("imported function not found: {fn_name:?}")),
        }
    }

    fn find_imported_fn(&self, fn_name: ImportedFnName<'ctx>) -> Option<usize> {
        self.import_fns
            .iter()
            .enumerate()
            .find_map(|(index, import_fn)| import_fn.namespace.matches(fn_name).then_some(index))
    }

    /// Create a new function index.
    pub fn get_func_idx_from_fn_index(&self, index: ir_closure::FnIndex) -> FuncIdx {
        FuncIdx::new(self.import_fns.len() + index.index())
    }
}

pub struct ImportFn<'ctx> {
    pub namespace: NameSpace<'ctx>,
    pub sig: TypeIdx,
}

#[derive(Clone, Copy)]
pub enum NameSpace<'ctx> {
    #[allow(dead_code)]
    Wasm {
        module: &'static str,
        field: &'static str,
    },
    Intrinsic {
        fn_name: ImportedFnName<'ctx>,
    },
}

impl<'ctx> NameSpace<'ctx> {
    pub fn into_wasm(self) -> (&'static str, Cow<'ctx, str>) {
        match self {
            Self::Wasm { module, field } => (module, field.into()),
            Self::Intrinsic { fn_name } => ("mincaml:runtime", fn_name.0.into()),
        }
    }
    pub fn matches(&self, fn_name: ImportedFnName<'ctx>) -> bool {
        match self {
            Self::Wasm { .. } => false,
            Self::Intrinsic {
                fn_name: self_fn_name,
            } => self_fn_name == &fn_name,
        }
    }
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct FnTypeSignature {
    pub params: Vec<WasmTy>,
    pub results: Option<WasmTy>,
}

impl FnTypeSignature {
    pub fn from_results(results: Option<WasmTy>) -> Self {
        Self {
            params: Vec::new(),
            results,
        }
    }

    /// Create a new function type signature from the given function type.
    ///
    /// # Panics
    ///
    /// Panics if the given type is not a function type.
    pub fn from_fun_ty(ty: ir_closure::Ty<'_>) -> Self {
        let (params, result) = ty.kind().as_fun_ty().unwrap();
        Self {
            params: params.iter().copied().filter_map(WasmTy::from_ty).collect(),
            results: WasmTy::from_ty(result),
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

    /// Intern the given function type signature.
    ///
    /// If the given signature is already interned, the existing index is returned.
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
