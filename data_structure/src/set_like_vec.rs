use crate::FxHashSet;

/// A vector that only stores unique elements.
///
/// Remembers the order of insertion.
pub struct SetLikeVec<T: Copy> {
    vec: Vec<T>,
    set: FxHashSet<T>,
}

impl<T> SetLikeVec<T>
where
    T: Copy,
{
    pub fn new() -> Self {
        Self {
            vec: Vec::default(),
            set: FxHashSet::default(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
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
        if self.set.insert(value) {
            self.vec.push(value);
        }
    }

    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        T: std::borrow::Borrow<Q>,
        Q: ?Sized + std::hash::Hash + Eq,
    {
        self.set.contains(value)
    }

    pub fn remove<Q>(&mut self, value: &Q) -> bool
    where
        T: std::borrow::Borrow<Q>,
        Q: ?Sized + std::hash::Hash + Eq,
    {
        self.set.remove(value)
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
            set: self.set,
        }
    }
}

pub struct IntoIter<T> {
    iter: std::vec::IntoIter<T>,
    set: FxHashSet<T>,
}

impl<T: Copy + Eq + std::hash::Hash> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        // The vector members are unique, so we don't need to remove `value` from the set.
        self.iter.find(|&value| self.set.contains(&value))
    }
}
