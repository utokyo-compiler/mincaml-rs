use std::sync::{Arc, Mutex};

use crate::Ty;

#[derive(Debug, Default)]
/// The interface of intrinsic library.
pub struct Mli<'ctx> {
    pub(crate) declarations: Arc<Mutex<Vec<Declaration<'ctx>>>>,
}

impl<'ctx> Mli<'ctx> {
    pub fn add_declaration(&self, declaration: Declaration<'ctx>) {
        self.declarations.lock().unwrap().push(declaration);
    }

    pub fn find_declaration(&self, ident: &'ctx str) -> Option<Declaration<'ctx>> {
        self.declarations
            .lock()
            .unwrap()
            .iter()
            .find(|decl_ident| decl_ident.item_ident.0 == ident)
            .copied()
    }

    pub fn list_declaration(&self) -> Vec<Declaration<'ctx>> {
        self.declarations.lock().unwrap().iter().copied().collect()
    }
}

#[derive(Debug, Clone, Copy)]
/// A module-level item.
pub struct Declaration<'ctx> {
    pub item_ident: syntax::Ident<'ctx>,
    pub ty: Ty<'ctx>,
}
