use std::{
    borrow::Borrow,
    fmt::Debug,
    hash::{Hash, Hasher},
    ops::Deref,
    sync::Mutex,
};

use rustc_hash::FxHashSet;

mod sealed {
    #[derive(Clone, Copy)]
    pub struct SealedZst;
}

/// an interned value that is unique in the context.
pub struct Interned<'ctx, T: ?Sized>(pub &'ctx T, sealed::SealedZst);

impl<T: Debug + ?Sized> Debug for Interned<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: ?Sized> Clone for Interned<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for Interned<'_, T> {}

impl<T: ?Sized> PartialEq for Interned<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<T: ?Sized> Eq for Interned<'_, T> {}

impl<T: PartialOrd + ?Sized> PartialOrd for Interned<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if std::ptr::eq(self.0, other.0) {
            Some(std::cmp::Ordering::Equal)
        } else {
            self.0.partial_cmp(other.0)
        }
    }
}

impl<T: Ord + ?Sized> Ord for Interned<'_, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if std::ptr::eq(self.0, other.0) {
            std::cmp::Ordering::Equal
        } else {
            self.0.cmp(other.0)
        }
    }
}

impl<T: ?Sized> Hash for Interned<'_, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0, state)
    }
}

impl<'ctx, T: ?Sized> Interned<'ctx, T> {
    /// create a new interned value.
    /// The uniqueness of the value is not checked here.
    pub fn new_unchecked(value: &'ctx T) -> Self {
        Self(value, sealed::SealedZst)
    }
}

impl<T> Deref for Interned<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

pub struct HashSetInterner<K> {
    set: Mutex<FxHashSet<K>>,
}

impl<K: Hash + Eq + Copy> HashSetInterner<K> {
    pub fn new() -> Self {
        Self {
            set: Default::default(),
        }
    }

    #[inline]
    pub fn intern_ref<Q>(&self, value: &Q, new: impl FnOnce() -> K) -> K
    where
        K: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        match self.set.lock().unwrap().get(value) {
            Some(x) => *x,
            None => {
                let value = new();
                self.set.lock().unwrap().insert(value);
                value
            }
        }
    }

    #[inline]
    pub fn intern<Q>(&self, value: Q, new: impl FnOnce(Q) -> K) -> K
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        match self.set.lock().unwrap().get(&value) {
            Some(x) => *x,
            None => {
                let value = new(value);
                self.set.lock().unwrap().insert(value);
                value
            }
        }
    }
}

impl<K: Hash + Eq + Copy> Default for HashSetInterner<K> {
    fn default() -> Self {
        Self::new()
    }
}
