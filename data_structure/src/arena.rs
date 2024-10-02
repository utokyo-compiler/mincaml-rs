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

    pub fn alloc_boxed(&self, value: T) -> Box<'_, T> {
        Box::new_unchecked(self.container.alloc(value), self)
    }

    pub fn into_vec(self) -> Vec<T> {
        self.container.into_vec()
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

/// A pointer type that uniquely owns an *arena* allocation of type `T`.
///
/// Does not `drop` the owned inner value when it gets `drop`ped.
/// This type exists only to provide `Clone` implementation
/// for arena allocated objects.
pub struct Box<'arena, T> {
    inner: &'arena mut T,
    arena: &'arena TypedArena<T>,
}

impl<'arena, T> std::ops::Deref for Box<'arena, T> {
    type Target = &'arena mut T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'arena, T> std::ops::DerefMut for Box<'arena, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<'arena, T> Box<'arena, T> {
    fn new_unchecked(inner: &'arena mut T, arena: &'arena TypedArena<T>) -> Self {
        Self { inner, arena }
    }
}

impl<'arena, T: Clone> Clone for Box<'arena, T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.arena.alloc(self.inner.clone()),
            arena: self.arena,
        }
    }
}

impl<'arena, T: std::fmt::Debug> std::fmt::Debug for Box<'arena, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl<'arena, T: PartialEq> PartialEq for Box<'arena, T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<'arena, T: Eq> Eq for Box<'arena, T> {}

impl<'arena, T: std::hash::Hash> std::hash::Hash for Box<'arena, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}
