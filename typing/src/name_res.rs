//! Name resolution for identifiers.
//!
//! The original implementation conducts this (alpha-conversion) after K-normalization.

use rustc_hash::{FxHashMap, FxHashSet};
use ty::{Ty, Typed};

pub struct Env<'ctx> {
    inner: FxHashMap<syntax::Ident<'ctx>, Candidates<'ctx>>,
    fresh_scope_id: usize,
    ended_scopes: FxHashSet<ScopeId>,
}

struct Candidates<'ctx> {
    inner: Vec<ScopedIdent<'ctx>>,
    fresh_disambiguator: u32,
}

impl<'ctx> Candidates<'ctx> {
    fn new() -> Self {
        Self {
            inner: Default::default(),
            fresh_disambiguator: 0,
        }
    }

    pub fn push(
        &mut self,
        ident: syntax::Ident<'ctx>,
        ty: Ty<'ctx>,
        origin: ir_typed_ast::IdentOrigin,
        scope_id: ScopeId,
    ) -> ir_typed_ast::Ident<'ctx> {
        self.fresh_disambiguator += 1;
        let ident = Typed::new(
            ir_typed_ast::DisambiguatedIdent::new_unchecked(
                ident,
                origin,
                self.fresh_disambiguator,
            ),
            ty,
        );
        self.inner.push(ScopedIdent { ident, scope_id });
        ident
    }

    pub fn find(&mut self, ended_scopes: &FxHashSet<ScopeId>) -> Option<ir_typed_ast::Ident<'ctx>> {
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
    ident: ir_typed_ast::Ident<'ctx>,
    scope_id: ScopeId,
}

impl<'ctx> Env<'ctx> {
    pub fn new() -> Self {
        Self {
            inner: Default::default(),
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

    pub fn insert_in(
        &mut self,
        scope_id: ScopeId,
        ident: syntax::Ident<'ctx>,
        ty: Ty<'ctx>,
    ) -> ir_typed_ast::Ident<'ctx> {
        self.inner.entry(ident).or_default().push(
            ident,
            ty,
            ir_typed_ast::IdentOrigin::UserDefined,
            scope_id,
        )
    }

    pub fn get(&mut self, ident: syntax::Ident<'ctx>) -> Option<ir_typed_ast::Ident<'ctx>> {
        self.inner
            .get_mut(&ident)
            .and_then(|candidates| candidates.find(&self.ended_scopes))
    }
}
