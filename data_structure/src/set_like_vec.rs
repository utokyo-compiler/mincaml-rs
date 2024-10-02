use crate::FxHashMap;

/// A vector that only stores unique elements.
///
/// Remembers the order of insertion.
pub struct SetLikeVec<T: Copy> {
    vec: Vec<T>,
    map: FxHashMap<T, usize>,
}

impl<T> SetLikeVec<T>
where
    T: Copy,
{
    pub fn new() -> Self {
        Self {
            vec: Vec::default(),
            map: FxHashMap::default(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

impl<T> Default for SetLikeVec<T>
where
    T: Copy,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T> SetLikeVec<T>
where
    T: Copy + Eq + std::hash::Hash,
{
    pub fn insert(&mut self, value: T) {
        if self.map.insert(value, self.vec.len()).is_none() {
            self.vec.push(value);
        }
    }

    pub fn push(&mut self, value: T) {
        self.insert(value);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.vec.pop().inspect(|value| {
            self.map.remove(value);
        })
    }

    pub fn get<Q>(&self, value: &Q) -> Option<usize>
    where
        T: std::borrow::Borrow<Q>,
        Q: ?Sized + std::hash::Hash + Eq,
    {
        self.map.get(value).copied()
    }

    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        T: std::borrow::Borrow<Q>,
        Q: ?Sized + std::hash::Hash + Eq,
    {
        self.map.contains_key(value)
    }

    pub fn remove<Q>(&mut self, value: &Q) -> Option<usize>
    where
        T: std::borrow::Borrow<Q>,
        Q: ?Sized + std::hash::Hash + Eq,
    {
        self.map.remove(value)
        // We don't need to remove `value` from the vector
        // because of the impl of `IntoIter`.
    }
}

impl<T: Copy + Eq + std::hash::Hash> IntoIterator for SetLikeVec<T> {
    type Item = T;

    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            iter: self.vec.into_iter(),
            set: self.map,
        }
    }
}

pub struct Iter<'a, T> {
    iter: std::slice::Iter<'a, T>,
    set: &'a FxHashMap<T, usize>,
}

impl<'a, T: Copy + Eq + std::hash::Hash> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        // The vector members are unique, so we don't need to remove `value` from the set,
        // but need to check if the value is in the set since the set may have been modified.
        self.iter.find(|&value| self.set.contains_key(value))
    }
}

pub struct IntoIter<T> {
    iter: std::vec::IntoIter<T>,
    set: FxHashMap<T, usize>,
}

impl<T: Copy + Eq + std::hash::Hash> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        // The vector members are unique, so we don't need to remove `value` from the set,
        // but need to check if the value is in the set since the set may have been modified.
        self.iter.find(|&value| self.set.contains_key(&value))
    }
}
