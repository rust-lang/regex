use std::ops::Deref;
use std::slice;

#[derive(Debug)]
pub struct SparseSet<T> {
    dense: Vec<T>,
    sparse: Vec<usize>,
    size: usize,
}

pub trait SparseIndexed: Clone + Default {
    fn index(&self) -> usize;
}

impl SparseIndexed for usize {
    fn index(&self) -> usize {
        *self
    }
}

impl<T: SparseIndexed> SparseSet<T> {
    pub fn new(size: usize) -> SparseSet<T> {
        SparseSet {
            dense: vec![T::default(); size],
            sparse: vec![0; size],
            size: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.size
    }

    pub fn capacity(&self) -> usize {
        self.dense.len()
    }

    pub fn add(&mut self, v: T) -> usize {
        let i = self.size;
        let sparse_index = v.index();
        self.dense[i] = v;
        self.sparse[sparse_index] = i;
        self.size += 1;
        i
    }

    pub fn contains_sparse_index(&self, sparse_index: usize) -> bool {
        let i = self.sparse[sparse_index];
        i < self.size && self.dense[i].index() == sparse_index
    }

    pub fn clear(&mut self) {
        self.size = 0;
    }
}

impl<T> Deref for SparseSet<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.dense[0..self.size]
    }
}

impl<'a, T> IntoIterator for &'a SparseSet<T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter { self.iter() }
}
