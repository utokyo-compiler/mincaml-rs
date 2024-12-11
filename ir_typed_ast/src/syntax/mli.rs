use std::sync::{Arc, Mutex};

use data_structure::FxIndexMap;

use crate::Ty;

#[derive(Debug, Default)]
/// The interface of intrinsic library.
pub struct Mli<'ctx> {
    pub(crate) declarations: Arc<Mutex<FxIndexMap<&'ctx str, Declaration<'ctx>>>>,
}

impl<'ctx> Mli<'ctx> {
    pub fn add_declaration(&self, declaration: Declaration<'ctx>) {
        self.declarations
            .lock()
            .unwrap()
            .insert(declaration.item_ident.0, declaration);
    }

    pub fn find_declaration(&self, ident: &'ctx str) -> Option<Declaration<'ctx>> {
        self.declarations.lock().unwrap().get(ident).copied()
    }

    pub fn list_declaration(&self) -> Vec<Declaration<'ctx>> {
        self.declarations
            .lock()
            .unwrap()
            .values()
            .copied()
            .collect()
    }
}

#[derive(Debug, Clone, Copy)]
/// A module-level item.
pub struct Declaration<'ctx> {
    pub item_ident: syntax::Ident<'ctx>,
    pub ty: Ty<'ctx>,
}
