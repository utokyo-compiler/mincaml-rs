use core::range::Range;
use std::ops::{Index, IndexMut};

use super::Indexable;

#[derive(Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct IndexVec<I: Idx, T: Indexable<I>> {
    data: Vec<T>,
    // captures the type `I` and `T` in a phantom type
    // and make this type contravariant for `I`
    _marker: std::marker::PhantomData<fn(&I) -> T>,
}

impl<I: Idx, T: Indexable<I> + std::fmt::Debug> std::fmt::Debug for IndexVec<I, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.data.fmt(f)
    }
}

impl<I: Idx, T: Indexable<I>> std::ops::Deref for IndexVec<I, T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<I: Idx, T: Indexable<I>> std::ops::DerefMut for IndexVec<I, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<I: Idx, T: Indexable<I>> FromIterator<T> for IndexVec<I, T> {
    fn from_iter<Iter: IntoIterator<Item = T>>(iter: Iter) -> Self {
        Self::from_raw_vec(Vec::from_iter(iter))
    }
}

impl<I: Idx, T: Indexable<I>> IntoIterator for IndexVec<I, T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}

impl<'a, I: Idx, T: Indexable<I>> IntoIterator for &'a IndexVec<I, T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.data.iter()
    }
}

impl<'a, I: Idx, T: Indexable<I>> IntoIterator for &'a mut IndexVec<I, T> {
    type Item = &'a mut T;
    type IntoIter = std::slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.data.iter_mut()
    }
}

impl<I: Idx, T: Indexable<I>> IndexVec<I, T> {
    pub fn new() -> Self {
        Self {
            data: Default::default(),
            _marker: std::marker::PhantomData,
        }
    }
}

impl<I: Idx, T: Indexable<I>> IndexVec<I, T> {
    fn next_index(&self) -> I {
        I::new(self.data.len())
    }

    pub fn push(&mut self, value: T) -> I {
        let idx = self.next_index();
        self.data.push(value);
        idx
    }

    pub fn from_raw_vec(data: Vec<T>) -> Self {
        Self {
            data,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn into_iter_enumerated(self) -> impl DoubleEndedIterator<Item = (I, T)> {
        self.data
            .into_iter()
            .enumerate()
            .map(|(idx, value)| (I::new(idx), value))
    }

    pub fn iter_enumerated(&self) -> impl DoubleEndedIterator<Item = (I, &'_ T)> {
        self.data
            .iter()
            .enumerate()
            .map(|(idx, value)| (I::new(idx), value))
    }
}

impl<I: Idx, T: Indexable<I>> Default for IndexVec<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Idx, T: Indexable<I>> Index<I> for IndexVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.data[index.index()]
    }
}

impl<I: Idx, T: Indexable<I>> Index<Range<I>> for IndexVec<I, T> {
    type Output = [T];

    fn index(&self, index: Range<I>) -> &Self::Output {
        &self.data[index.start.index()..index.end.index()]
    }
}

impl<I: Idx, T: Indexable<I>> IndexMut<I> for IndexVec<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.data[index.index()]
    }
}

impl<I: Idx, T: Indexable<I>> IndexMut<Range<I>> for IndexVec<I, T> {
    fn index_mut(&mut self, index: Range<I>) -> &mut Self::Output {
        &mut self.data[index.start.index()..index.end.index()]
    }
}

pub trait Idx: Copy + Eq + std::fmt::Debug + std::hash::Hash {
    fn new(idx: usize) -> Self;
    fn index(self) -> usize;
}

impl Idx for usize {
    fn new(idx: usize) -> Self {
        idx
    }

    fn index(self) -> usize {
        self
    }
}

impl<I: Idx> Indexable<I> for bool {}
impl<I: Idx, T: Indexable<I>> Indexable<I> for Option<T> {}

// map like functions

impl<I: Idx> IndexVec<I, bool> {
    pub fn contains(&self, index: I) -> bool {
        self[index]
    }
}
impl<I: Idx, T: Indexable<I>> IndexVec<I, Option<T>> {
    pub fn contains(&self, index: I) -> bool {
        self[index].is_some()
    }
}

#[macro_export]
macro_rules! index_vec {
    ($($tt:tt)*) => {
        IndexVec::from_raw_vec(vec!($($tt)*))
    };
}
