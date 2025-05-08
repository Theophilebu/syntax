use std::{marker::PhantomData, ops::Index};
use num::{Unsigned, PrimInt};
use std::fmt::Debug;
use std::mem::size_of;

use crate::Idx;

pub trait Indexing: From<usize> + Into<usize> + Copy {}

impl Indexing for Idx {}
// -------------------------------------


/// stores values of type T
pub struct FlatTable<T, Id: Indexing>
{
    // Id represents the index type for both dimensions and for the flat index
    pub table: Vec<T>,
    // points to the start of each subarray representing a row
    pub rows: Vec<Id>,
}

impl <T, Id: Indexing> FlatTable<T, Id>
{
    pub fn new(table: Vec<T>, rows: Vec<Id>) -> Self {
        Self {table, rows}
    }

    pub fn from_vec_vec(mut table2d: Vec<Vec<T>>) -> Self {
        let total_size: usize = table2d.iter().map(|v| v.len()).sum();
        let mut table: Vec<T> = Vec::with_capacity(total_size);
        let mut rows: Vec<Id> = Vec::with_capacity(table2d.len());

        for values in table2d.iter_mut() {
            rows.push(table.len().into());
            table.append(values);
        }

        Self { table, rows }
    }

    pub fn size(&self) -> Id {
        Id::from(self.table.len())
    }

    pub fn get_by_id(&self, id: Id) -> &T {
        &self.table[id.into()]
    }
}

impl <T, Id: Indexing> Index<Id> for FlatTable<T, Id>
{
    type Output = [T];

    fn index(&self, index: Id) -> &[T] {
        let id: usize = index.into();
        let start: usize = self.rows[id].into();
        let end: usize = (*self
            .rows
            .get(id+1)
            .unwrap_or(&FlatTable::<T, Id>::size(self)))
            .into();
        // for some reason self.size() doesn't work
        &self.table[start..end]
    }
}

