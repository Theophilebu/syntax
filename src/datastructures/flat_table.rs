use std::{marker::PhantomData, ops::Index};
use num::{Unsigned, PrimInt};
use std::fmt::Debug;
use std::mem::size_of;

pub trait Indexing: From<usize> + Into<usize> + Copy {}

// -------------------------------------


/// stores values of type T
pub struct FlatTable<T, HeightIdx, WidthIdx, FlatIdx>
where
    HeightIdx: Indexing,
    WidthIdx: Indexing,
    FlatIdx: Indexing,
{
    pub table: Vec<T>,
    // points to the start of each subarray representing a row
    pub rows: Vec<FlatIdx>,
    phantom: PhantomData<(HeightIdx, WidthIdx)>,
}

impl <T, HeightIdx, WidthIdx, FlatIdx> FlatTable<T, HeightIdx, WidthIdx, FlatIdx>
where
    HeightIdx: Indexing,
    WidthIdx: Indexing,
    FlatIdx: Indexing,
{
    pub fn new(table: Vec<T>, rows: Vec<FlatIdx>) -> Self {
        Self {table, rows, phantom: PhantomData}
    }

    pub fn from_vec_vec(mut table2d: Vec<Vec<T>>) -> Self {
        let total_size: usize = table2d.iter().map(|v| v.len()).sum();
        let mut table: Vec<T> = Vec::with_capacity(total_size);
        let mut rows: Vec<FlatIdx> = Vec::with_capacity(table2d.len());

        for values in table2d.iter_mut() {
            rows.push(table.len().into());
            table.append(values);
        }

        Self { table, rows, phantom: PhantomData }
    }

    pub fn size(&self) -> FlatIdx {
        FlatIdx::from(self.table.len())
    }

    pub fn get_by_id(&self, id: FlatIdx) -> &T {
        &self.table[id.into()]
    }
}

impl <T, HeightIdx, WidthIdx, FlatIdx> Index<HeightIdx> for FlatTable<T, HeightIdx, WidthIdx, FlatIdx>
where
    HeightIdx: Indexing,
    WidthIdx: Indexing,
    FlatIdx: Indexing,
{
    type Output = [T];

    fn index(&self, index: HeightIdx) -> &[T] {
        let id: usize = index.into();
        let start: usize = self.rows[id].into();
        let end: usize = (*self
            .rows
            .get(id+1)
            .unwrap_or(&FlatTable::<T, HeightIdx, WidthIdx, FlatIdx>::size(self)))
            .into();
        // for some reason self.size() doesn't work
        &self.table[start..end]
    }
}

// -----------------------------------------

pub struct RectFlatTable<T, Idxx>
where
    Idxx: Unsigned + PrimInt,
    Idxx: TryFrom<usize>,
    usize: From<Idxx>,
    <Idxx as TryFrom<usize>>::Error: Debug,
{
    pub table: Vec<T>,
    pub height: Idxx,
    pub width: usize,
}

impl <T, Idxx> RectFlatTable<T, Idxx>
where
    Idxx: Unsigned + PrimInt,
    Idxx: TryFrom<usize>,
    usize: From<Idxx>,
    <Idxx as TryFrom<usize>>::Error: Debug,
{
    pub fn new(table: Vec<T>, height: Idxx, width: usize) -> Self {
        Self {table, height, width}
    }

    pub fn from_vec_vec(mut table2d: Vec<Vec<T>>) -> Self {
        let height: Idxx = table2d.len().try_into().unwrap();
        let width: usize = { if height==Idxx::zero() {0} else {table2d[0].len()}};
        let mut table: Vec<T> = Vec::with_capacity(usize::from(height)*width);

        for values in table2d.iter_mut() {
            table.append(values);
        }

        Self { table, height, width }
    }
}

impl <T ,Idxx> Index<Idxx> for RectFlatTable<T, Idxx>
where
    Idxx: Unsigned + PrimInt,
    Idxx: TryFrom<usize>,
    usize: From<Idxx>,
    <Idxx as TryFrom<usize>>::Error: Debug,
{
    type Output = [T];

    fn index(&self, index: Idxx) -> &[T] {
        &self.table[usize::from(index)*self.width..usize::from(index)*(self.width+1)]
    }
}


