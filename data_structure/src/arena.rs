pub struct TypedArena<T> {
    container: typed_arena::Arena<T>,
}

impl<T> TypedArena<T> {
    pub fn new() -> Self {
        Self {
            container: typed_arena::Arena::new(),
        }
    }

    pub fn alloc(&self, value: T) -> &mut T {
        self.container.alloc(value)
    }
}

impl TypedArena<u8> {
    pub fn alloc_str(&self, value: &str) -> &mut str {
        self.container.alloc_str(value)
    }
}

impl<T> Default for TypedArena<T> {
    fn default() -> Self {
        Self::new()
    }
}
