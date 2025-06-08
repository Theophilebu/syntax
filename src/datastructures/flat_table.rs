use std::{marker::PhantomData, ops::Index};
use num::{Unsigned, PrimInt};
use std::fmt::Debug;
use std::mem::size_of;

use crate::datastructures::indexing::Indexing;

// -------------------------------------

/*
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
 */



/// stores values of type T
/// assumes that the FlatId type is large enough to index the whole table
/// this is always true if sizeof(FlatId)>=sizeof(ColId)+sizeof(RowId)
/// panics when out of bounds access
pub struct FlatTable<T, ColId: Indexing, RowId: Indexing, FlatId: Indexing>
{
    // table is indexed by FlatId
    pub table: Vec<T>,

    // indexed by RowId
    // points to the start of each subarray representing a row
    pub rows: Vec<FlatId>,

    pub phantom: PhantomData<(ColId, RowId)>,
}

impl <T, RowId: Indexing, ColId: Indexing, FlatId: Indexing> FlatTable<T, RowId, ColId, FlatId>
{
    pub fn new(table: Vec<T>, rows: Vec<FlatId>) -> Self {
        Self {table, rows, phantom: PhantomData}
    }

    pub fn from_vec_vec(mut table2d: Vec<Vec<T>>) -> Self {
        let total_size: usize = table2d.iter().map(|v| v.len()).sum();
        let mut table: Vec<T> = Vec::with_capacity(total_size);
        let mut rows: Vec<FlatId> = Vec::with_capacity(table2d.len());

        for values in table2d.iter_mut() {
            rows.push(FlatId::from_usize(table.len()));
            table.append(values);
        }

        Self { table, rows, phantom: PhantomData }
    }

    pub fn nbr_rows(&self) -> RowId {
        RowId::from_usize(self.rows.len())
    }

    pub fn size(&self) -> FlatId {
        FlatId::from_usize(self.table.len())
    }

    pub fn get_by_flat_id(&self, flat_id: FlatId) -> &T {
        &self.table[flat_id.into_usize()]
    }

    pub fn get_by_row_col_ids(&self, row: RowId, col: ColId) -> &T {
        &self.get_row(row)[col.into_usize()]
    }

    pub fn get_row(&self, row: RowId) -> &[T] {
        let row_usize_id: usize = row.into_usize();
        let start: usize = self.rows[row_usize_id].into_usize();
        let end: usize = (*
            self
            .rows
            .get(row_usize_id+1)
            .unwrap_or(&self.size())
        )
        .into_usize();
            
        // for some reason self.size() doesn't work
        &self.table[start..end]
    }
}

impl <T, RowId: Indexing, ColId: Indexing, FlatId: Indexing> Index<RowId> for FlatTable<T, RowId, ColId, FlatId>
{
    type Output = [T];

    fn index(&self, row: RowId) -> &[T] {
        return self.get_row(row);
    }
}

