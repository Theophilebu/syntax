use std::{marker::PhantomData, ops::Index};

use crate::datastructures::indexing::Indexing;

// -------------------------------------


/// stores values of type T
/// assumes that the FlatId type is large enough to index the whole table
/// this is always true if sizeof(FlatId)>=sizeof(ColId)+sizeof(RowId)
/// panics when out of bounds access
#[derive(Debug)]
pub struct FlatTable<T, RowId: Indexing, ColId: Indexing, FlatId: Indexing>
{
    // table is indexed by FlatId
    pub values: Vec<T>,

    // indexed by RowId
    // points to the start of each subarray representing a row
    pub rows: Vec<FlatId>,

    pub phantom: PhantomData<(RowId, ColId)>,
}

impl <T, RowId: Indexing, ColId: Indexing, FlatId: Indexing> FlatTable<T, RowId, ColId, FlatId>
{
    pub fn new(values: Vec<T>, rows: Vec<FlatId>) -> Self {
        Self {values, rows, phantom: PhantomData}
    }

    pub fn from_vec_vec(mut table2d: Vec<Vec<T>>) -> Self {
        let total_size: usize = table2d.iter().map(|v| v.len()).sum();
        let mut table: Vec<T> = Vec::with_capacity(total_size);
        let mut rows: Vec<FlatId> = Vec::with_capacity(table2d.len());

        for values in table2d.iter_mut() {
            rows.push(FlatId::from_usize(table.len()));
            table.append(values);
        }

        Self { values: table, rows, phantom: PhantomData }
    }

    pub fn nbr_rows(&self) -> RowId {
        RowId::from_usize(self.rows.len())
    }

    pub fn size(&self) -> FlatId {
        FlatId::from_usize(self.values.len())
    }

    pub fn get_by_flat_id(&self, flat_id: FlatId) -> &T {
        &self.values[flat_id.into_usize()]
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
        &self.values[start..end]
    }
}

impl <T, RowId: Indexing, ColId: Indexing, FlatId: Indexing> Index<RowId> for FlatTable<T, RowId, ColId, FlatId>
{
    type Output = [T];

    fn index(&self, row: RowId) -> &[T] {
        return self.get_row(row);
    }
}

