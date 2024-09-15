//! Name resolution for identifiers.
//!
//! The original implementation conducts this (alpha-conversion)
//! after K-normalization.

use ir_typed_ast::DisambiguatedIdent;
use rustc_hash::{FxHashMap, FxHashSet};
use sourcemap::{Span, Spanned};
use ty::{Ty, Typed};

pub struct Env<'ctx> {
    candidates_map: FxHashMap<syntax::Ident<'ctx>, Candidates<'ctx>>,
    fresh_scope_id: usize,
    ended_scopes: FxHashSet<ScopeId>,
}

struct Candidates<'ctx> {
    inner: Vec<ScopedIdent<'ctx>>,
}

impl<'ctx> Candidates<'ctx> {
    fn new() -> Self {
        Self {
            inner: Default::default(),
        }
    }

    pub fn push(
        &mut self,
        ident: syntax::Ident<'ctx>,
        ty: Ty<'ctx>,
        span: Span,
        scope_id: ScopeId,
    ) -> Typed<'ctx, DisambiguatedIdent<'ctx>> {
        let ident = Typed::new(ir_typed_ast::DisambiguatedIdent::new_user(ident, span), ty);
        self.inner.push(ScopedIdent { ident, scope_id });
        ident
    }

    pub fn find(
        &mut self,
        ended_scopes: &FxHashSet<ScopeId>,
    ) -> Option<Typed<'ctx, DisambiguatedIdent<'ctx>>> {
        while let Some(tail) = self.inner.last() {
            if ended_scopes.contains(&tail.scope_id) {
                self.inner.pop();
            } else {
                return Some(tail.ident);
            }
        }
        None
    }
}

impl<'ctx> Default for Candidates<'ctx> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(usize);

struct ScopedIdent<'ctx> {
    ident: Typed<'ctx, DisambiguatedIdent<'ctx>>,
    scope_id: ScopeId,
}

impl<'ctx> Env<'ctx> {
    pub fn new() -> Self {
        Self {
            candidates_map: Default::default(),
            fresh_scope_id: 0,
            ended_scopes: Default::default(),
        }
    }

    pub fn begin_scope(&mut self) -> ScopeId {
        let scope_id = ScopeId(self.fresh_scope_id);
        self.fresh_scope_id += 1;
        scope_id
    }

    pub fn end_scope(&mut self, scope_id: ScopeId) {
        self.ended_scopes.insert(scope_id);
    }

    pub fn define_in(
        &mut self,
        scope_id: ScopeId,
        ident: Spanned<syntax::Ident<'ctx>>,
        ty: Ty<'ctx>,
    ) -> Typed<'ctx, DisambiguatedIdent<'ctx>> {
        self.candidates_map.entry(ident.node).or_default().push(
            ident.node,
            ty,
            *ident.span.as_user_defined().unwrap(),
            scope_id,
        )
    }

    pub fn get(
        &mut self,
        ident: syntax::Ident<'ctx>,
    ) -> Option<Typed<'ctx, DisambiguatedIdent<'ctx>>> {
        self.candidates_map
            .get_mut(&ident)
            .and_then(|candidates| candidates.find(&self.ended_scopes))
    }
}
