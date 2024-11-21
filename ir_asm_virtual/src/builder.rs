use core::range::Range;

use data_structure::{
    index::vec::{Idx, IndexVec},
    FxIndexMap,
};

use crate::{
    ArgIndex, BasicBlock, BasicBlockData, Context, DisambiguatedIdent, FnName, FunctionDef, Ident,
    Local, LocalDecl, StmtIndex, StmtKind, TerminatorKind, Ty, Typed,
};

#[derive(Default)]
pub struct BasicBlockBuilder<'ctx> {
    args: IndexVec<ArgIndex, Local>,
    stmts: IndexVec<StmtIndex, StmtKind<'ctx>>,
}

impl<'ctx> BasicBlockBuilder<'ctx> {
    pub fn set_args(&mut self, args: IndexVec<ArgIndex, Local>) {
        #[cfg(debug_assertions)]
        assert!(self.args.is_empty());

        self.args = args;
    }
    pub fn push_stmt(&mut self, value: StmtKind<'ctx>) -> StmtIndex {
        self.stmts.push(value)
    }

    /// Terminates the current block and wraps up into a `BasicBlockData`.
    pub fn terminate_block(self, terminator: Option<TerminatorKind<'ctx>>) -> BasicBlockData<'ctx> {
        BasicBlockData {
            args: self.args,
            stmts: self.stmts,
            terminator,
        }
    }
}

pub struct FunctionBuilder<'ctx> {
    name: FnName<'ctx>,
    local_decls: IndexVec<Local, LocalDecl<'ctx>>,
    args: Range<Local>,
    args_via_closure: Range<Local>,
    basic_blocks: IndexVec<BasicBlock, BasicBlockData<'ctx>>,

    ident_map: FxIndexMap<Ident<'ctx>, Local>,
    basic_block_builder: BasicBlockBuilder<'ctx>,

    #[cfg(debug_assertions)]
    unterminated_blocks: data_structure::FxHashSet<BasicBlock>,
}

impl<'ctx> FunctionBuilder<'ctx> {
    pub fn new(
        ctx: &'ctx Context<'ctx>,
        name: FnName<'ctx>,
        args: impl Iterator<Item = Ident<'ctx>>,
        args_via_closure: impl Iterator<Item = Ident<'ctx>>,
        return_ty: Ty<'ctx>,
    ) -> Self {
        let mut local_decls = IndexVec::default();
        let mut ident_map = FxIndexMap::default();

        static COMPILER_GENERATED_COUNTER: std::sync::atomic::AtomicUsize =
            std::sync::atomic::AtomicUsize::new(0);
        local_decls.push(LocalDecl {
            ident: ctx.new_ident_unchecked(Typed::new(
                DisambiguatedIdent::new_compiler_unchecked(
                    "return_place",
                    COMPILER_GENERATED_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
                ),
                return_ty,
            )),
        });

        let range_start = local_decls.len();
        for arg in args {
            let local = local_decls.push(LocalDecl { ident: arg });
            ident_map.insert(arg, local);
        }
        let range_end = local_decls.len();
        let args = Range::from(Local::new(range_start)..Local::new(range_end));

        let range_start = local_decls.len();
        for arg in args_via_closure {
            let local = local_decls.push(LocalDecl { ident: arg });
            ident_map.insert(arg, local);
        }
        let range_end = local_decls.len();
        let args_via_closure = Range::from(Local::new(range_start)..Local::new(range_end));

        Self {
            name,
            local_decls,
            args,
            args_via_closure,
            basic_blocks: IndexVec::default(),
            ident_map,
            basic_block_builder: BasicBlockBuilder::default(),

            #[cfg(debug_assertions)]
            unterminated_blocks: data_structure::FxHashSet::default(),
        }
    }
    pub fn finish_function(self) -> FunctionDef<'ctx> {
        FunctionDef {
            name: self.name,
            local_decls: self.local_decls,
            args: self.args,
            args_via_closure: self.args_via_closure,
            basic_blocks: self.basic_blocks,
        }
    }

    /// Get a local variable by its identifier.
    ///
    /// If the identifier is not seen before, it will be added
    /// to the local declarations.
    pub fn get_local(&mut self, ident: Ident<'ctx>) -> Local {
        if let Some(local) = self.ident_map.get(&ident) {
            return *local;
        }
        let local = self.local_decls.push(LocalDecl { ident });
        self.ident_map.insert(ident, local);
        local
    }

    /// Set the arguments of the current basic block.
    pub fn set_args_to_current(&mut self, args: IndexVec<ArgIndex, Local>) {
        self.basic_block_builder.set_args(args)
    }

    /// Push a statement to the current basic block.
    pub fn push_stmt_to_current(&mut self, value: StmtKind<'ctx>) -> StmtIndex {
        self.basic_block_builder.push_stmt(value)
    }

    /// Obtain the next basic block. The result is always an invalid block at
    /// the time of this call, but is useful if we are going to start a new block.
    pub fn next_basic_block(&self) -> BasicBlock {
        BasicBlock::new(self.basic_blocks.len())
    }
}

/// Represents a basic block that is not terminated but created.
///
/// Do not pub their fields to prevent double-terminating the block.
pub struct DeferredBasicBlock(BasicBlock);

impl<'ctx> FunctionBuilder<'ctx> {
    /// Finish current basic block and start a new one.
    ///
    /// Returns the finished block.
    pub fn terminate_block(&mut self, terminator: TerminatorKind<'ctx>) -> BasicBlock {
        let basic_block_data =
            std::mem::take(&mut self.basic_block_builder).terminate_block(Some(terminator));
        self.push_basic_block(basic_block_data)
    }

    /// Deferred version of `terminate_block`.
    pub fn terminate_block_deferred(
        &mut self,
        basic_block: DeferredBasicBlock,
        terminator: TerminatorKind<'ctx>,
    ) {
        #[cfg(debug_assertions)]
        assert!(self.unterminated_blocks.remove(&basic_block.0));
        self.basic_blocks[basic_block.0].terminator = Some(terminator);
    }

    /// Defer creating a terminator for the current block but finish it.
    pub fn defer_terminate_block(&mut self) -> DeferredBasicBlock {
        let basic_block_data = std::mem::take(&mut self.basic_block_builder).terminate_block(None);
        let basic_block = self.push_basic_block(basic_block_data);

        #[cfg(debug_assertions)]
        assert!(self.unterminated_blocks.insert(basic_block));

        DeferredBasicBlock(basic_block)
    }

    fn push_basic_block(&mut self, data: BasicBlockData<'ctx>) -> BasicBlock {
        self.basic_blocks.push(data)
    }
}
