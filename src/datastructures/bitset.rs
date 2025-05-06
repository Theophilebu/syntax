use num::{Unsigned, PrimInt};
use std::fmt::Binary;

#[derive(Debug, Clone)]
pub struct BitSet<UINT: Unsigned + PrimInt + Binary> 
{
    data: Vec<UINT>,
    size: usize,
}

// - - - - - - - - - - - - - - - - 


impl <UINT> BitSet<UINT>
where 
    UINT: Unsigned + PrimInt + Binary,
{

    pub fn size(&self) -> usize {
        self.size
    }

    fn nbr_uints(&self) -> usize {
        self.data.len()
    }

    fn nbr_used_uints(&self) -> usize {
        (self.size + 7) / (8 * size_of::<UINT>())
    }


    pub fn new_filled(value: bool, size: usize) -> BitSet<UINT> {
        let uint_value = match value {
            true => {UINT::max_value()}
            false => {UINT::min_value()}
        };
        BitSet {
            data: vec![uint_value; (size + 7) / (8 * size_of::<UINT>())],
            size: size,
        }
    }

    pub fn new(data: Vec<UINT>, size: usize) -> BitSet<UINT> {
        assert_eq!(data.len(), (size + 7) / (8 * size_of::<UINT>()),
            "the size of the table is not compatible with the given size");
        Self {
            data,
            size,
        }
    }

    pub fn len(&self) -> usize {
        // |!| not constant time
        let mut length: u32 = 0;
        for i in 0..self.nbr_used_uints() {
            length+=(self.data[i]).count_ones();
        }
        length as usize
    }


    pub fn contains(&self, n: usize) -> bool{
        if n>=self.size(){
            panic!("index out of bounds");
        }
        let uint_index: usize = n/(8*size_of::<UINT>()) as usize;   // index in data
        let position_in_uint: usize = n - (8*size_of::<UINT>())*uint_index;
        let bit_mask: UINT = UINT::one() << (8*size_of::<UINT>() - 1 - position_in_uint);
        return (bit_mask & self.data[uint_index])>UINT::zero();
    }

    pub fn insert(&mut self, n: usize) {
        if n>=self.size(){
            panic!("index out of bounds");
        }
        let uint_index: usize = n/(8*size_of::<UINT>()) as usize;   // index in data
        let position_in_uint: usize = n - (8*size_of::<UINT>())*uint_index;
        let bit_mask: UINT = UINT::one() << (8*size_of::<UINT>() - 1 - position_in_uint);
        self.data[uint_index] = bit_mask | self.data[uint_index];
    }

    pub fn remove(&mut self, n: usize) {
        if n>=self.size(){
            panic!("index out of bounds");
        }
        let uint_index: usize = n/(8*size_of::<UINT>()) as usize;   // index in data
        let position_in_uint: usize = n - (8*size_of::<UINT>())*uint_index;
        let bit_mask: UINT = UINT::one() << (8*size_of::<UINT>() - 1 - position_in_uint);
        self.data[uint_index] = bit_mask & !self.data[uint_index];
    }

    pub fn set_value(&mut self, n: usize, value: bool){

        if n>=self.size(){
            panic!("index out of bounds");
        }
        if value{
            self.insert(n);
        } else{
            self.remove(n);
        }
    }

    
    pub fn union(&self, other: &Self) -> Self {
    
        if self.size() != other.size() {
            panic!("operations on bitsets with different sizes are not allowed");
        }

        let mut new_bitset: BitSet<UINT> = BitSet::new_filled(false, self.size);

        for i in 0..self.nbr_used_uints() {
            new_bitset.data[i] = self.data[i] | other.data[i];
        }

        new_bitset
    }

    pub fn intersection(&self, other: &Self) -> Self {
        if self.size() != other.size() {
            panic!("operations on bitsets with different sizes are not allowed");
        }

        let mut new_bitset: BitSet<UINT> = BitSet::new_filled(false, self.size);

        for i in 0..self.nbr_used_uints() {
            new_bitset.data[i] = self.data[i] & other.data[i];
        }

        new_bitset
    }

    pub fn difference(&self, other: &Self) -> Self {
        if self.size() != other.size() {
            panic!("operations on bitsets with different sizes are not allowed");
        }

        let mut new_bitset: BitSet<UINT> = BitSet::new_filled(false, self.size);

        for i in 0..self.nbr_used_uints() {
            new_bitset.data[i] = self.data[i] & !other.data[i];
        }

        new_bitset
    }

    pub fn symmetric_difference(&self, other: &Self) -> Self {
        if self.size() != other.size() {
            panic!("operations on bitsets with different sizes are not allowed");
        }

        let mut new_bitset: BitSet<UINT> = BitSet::new_filled(false, self.size);

        for i in 0..self.nbr_used_uints() {
            new_bitset.data[i] = self.data[i] ^ other.data[i];
        }

        new_bitset
    }

    pub fn complement(&self) -> Self {
        let mut new_bitset: BitSet<UINT> = BitSet::new_filled(false, self.size);

        for i in 0..self.nbr_used_uints() {
            new_bitset.data[i] = !self.data[i];
        }

        new_bitset
    }


    pub fn concatenate(&self, other: &Self) -> BitSet<UINT> {
        let mut new_bitset: BitSet<UINT> = BitSet::<UINT>::new_filled(false, self.size+other.size);

        for i in 0..self.nbr_used_uints() {
            new_bitset.data[i] = self.data[i];
        }

        // number of bits NOT USED in the last uint of the data of self
        let right: usize = 8*size_of::<UINT>()*self.nbr_used_uints() - self.size();

        // number of bits USED in the last uint of the data of self
        let left: usize = 8*size_of::<UINT>() - right;


        // -1 to avoid index out of bounds
        for index_in_uint in 0..other.nbr_used_uints()-1 {
            new_bitset.data[self.nbr_used_uints()+index_in_uint-1] = 
                new_bitset.data[self.nbr_used_uints()-1] | other.data[index_in_uint] >> left;

            new_bitset.data[self.nbr_used_uints()+index_in_uint] = other.data[index_in_uint] << right;            
        }

        new_bitset.data[self.nbr_used_uints()+other.nbr_used_uints()-2] = 
            new_bitset.data[self.nbr_used_uints()+other.nbr_used_uints()-2] 
                | other.data[other.nbr_used_uints()-1] >> left;

        new_bitset
    }


    pub fn update_union(&mut self, other: &Self) {
        if self.size() != other.size() {
            panic!("operations on bitsets with different sizes are not allowed");
        }

        for i in 0..self.nbr_used_uints() {
            self.data[i] = self.data[i] | other.data[i];
        }
    }

    pub fn update_intersection(&mut self, other: &Self) {
        if self.size() != other.size() {
            panic!("operations on bitsets with different sizes are not allowed");
        }

        for i in 0..self.nbr_used_uints() {
            self.data[i] = self.data[i] & other.data[i];
        }
    }

    pub fn update_difference(&mut self, other: &Self) {
        if self.size() != other.size() {
            panic!("operations on bitsets with different sizes are not allowed");
        }

        for i in 0..self.nbr_used_uints() {
            self.data[i] = self.data[i] & !other.data[i];
        }
    }

    pub fn update_symmetric_difference(&mut self, other: &Self) {
        if self.size() != other.size() {
            panic!("operations on bitsets with different sizes are not allowed");
        }

        for i in 0..self.nbr_used_uints() {
            self.data[i] = self.data[i] ^ other.data[i];
        }
    }

    pub fn update_complement(&mut self) {
        for i in 0..self.nbr_used_uints() {
            self.data[i] = !self.data[i];
        }
    }


    pub fn clear(&mut self){
        for i in 0..self.nbr_uints(){
            self.data[i] = UINT::min_value();
        }
    }

    pub fn fill(&mut self){
        for i in 0..self.nbr_uints(){
            self.data[i] = UINT::max_value();
        }
    }


    pub fn is_disjoint(&self, other: &Self) -> bool{
        if self.size() != other.size() {
            panic!("operations on bitsets with different sizes are not allowed");
        }

        for i in 0..self.nbr_used_uints(){
            if self.data[i] & other.data[i] > UINT::zero() {
                return false;
            }
        }
        return true;
    }

    pub fn is_empty(&self) -> bool{

        for i in 0..self.nbr_used_uints() {
            if self.data[i] > UINT::zero() {
                return false;
            }
        }
        true
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        for i in 0..self.nbr_uints(){
            if self.data[i] & !other.data[i] > UINT::zero() {
                return false;
            }
        }
        true
    }

    pub fn is_superset(&self, other: &Self) -> bool {
        for i in 0..self.nbr_uints(){
            if !self.data[i] & other.data[i] > UINT::zero() {
                return false;
            }
        }
        true
    }


    pub fn print(&self){
        let mut s: String = String::from("");
        for i in 0..self.nbr_used_uints() {
            let uint: UINT = self.data[i];
            let uint_str: String = format!("{uint:b}");
            // total starts with 0 bits
            let mut total: String = std::iter::repeat("0")
                .take(8-uint_str.len())
                .collect();

            total.push_str(&uint_str);
            s.push_str(&total);
        }
        println!("{s}");
    }


    pub fn iter<'se>(&'se self) -> BitSetIter<'se, UINT> {
        self.into_iter()
    }

}



// -------------------------- Iterator -----------------------


pub struct BitSetIter<'bitset, UINT>
where 
    UINT: Unsigned + PrimInt + Binary,
{
    bitset: &'bitset BitSet<UINT>,

    uint_index: usize,
    index_in_uint: usize,
    
    working_uint: UINT,       // not really the actual uint, it gets modified
    nbr_ones_in_uint: u32,    // same, it gets decremented
}


impl <'bitset, UINT> IntoIterator for &'bitset BitSet<UINT>
where 
    UINT: Unsigned + PrimInt + Binary,
{
    type Item = usize;
    type IntoIter = BitSetIter<'bitset, UINT>;

    fn into_iter(self) -> Self::IntoIter {
        BitSetIter {
            bitset: self,

            uint_index: 0,
            index_in_uint: 0,

            working_uint: self.data[0],
            nbr_ones_in_uint: self.data[0].count_ones(),
        }
    }
}

impl <'bitset, UINT> Iterator for BitSetIter<'bitset, UINT>
where 
    UINT: Unsigned + PrimInt + Binary,
{
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        

        // finds a non-empty uint, or returns None if the end is reached without encountering any zeros
        while self.nbr_ones_in_uint == 0 {
            
            self.uint_index += 1;
            if self.uint_index == self.bitset.nbr_used_uints() {
                return None;
            }
            
            self.working_uint = self.bitset.data[self.uint_index];
            self.nbr_ones_in_uint = self.working_uint.count_ones();
        }
        

        let one_left: UINT = UINT::one() << (8*size_of::<UINT>() - 1);  // 0b1000..0
        loop {
            
            if (self.working_uint & one_left) == one_left {
                self.nbr_ones_in_uint -= 1;

                let res: Option<usize> = Some(8*size_of::<UINT>()*self.uint_index + self.index_in_uint);
                if res.unwrap() >= self.bitset.size() {
                    return None;
                }

                if self.nbr_ones_in_uint == 0 {
                    self.index_in_uint = 0; // returns to the start of the next uint with the next call of "next"
                }
                else {
                    self.working_uint = self.working_uint << 1; // skips to the next bit
                    self.index_in_uint += 1;
                }
                return res;
            }            
            else {
                self.working_uint = self.working_uint << 1;
                self.index_in_uint += 1;
            }
        }

    }

}

// -------------------------- Iterator mutable ref -----------------------


pub struct MutBitSetIter<'bitset, UINT>
where 
    UINT: Unsigned + PrimInt + Binary,
{
    pub bitset: &'bitset mut BitSet<UINT>,

    uint_index: usize,
    index_in_uint: usize,
    
    working_uint: UINT,       // not really the actual uint, it gets modified
    nbr_ones_in_uint: u32,    // same, it gets decremented
}


impl <'bitset, UINT> MutBitSetIter<'bitset, UINT>
where
    UINT: Unsigned + PrimInt + Binary,
{
    pub fn reset(&mut self) {
        self.uint_index = 0;
        self.index_in_uint = 0;

        self.working_uint = self.bitset.data[0];
        self.nbr_ones_in_uint = self.working_uint.count_ones();
    }

}


impl <'bitset, UINT> IntoIterator for &'bitset mut BitSet<UINT>
where
    UINT: Unsigned + PrimInt + Binary,
{
    type Item = usize;
    type IntoIter = MutBitSetIter<'bitset, UINT>;

    fn into_iter(self) -> Self::IntoIter {
        let working_uint = self.data[0];
        let nbr_ones_in_uint = working_uint.count_ones();
        MutBitSetIter{
            bitset: self,

            uint_index: 0,
            index_in_uint: 0,

            working_uint,
            nbr_ones_in_uint,
        }
    }
}

impl <'bitset, UINT> Iterator for MutBitSetIter<'bitset, UINT>
where
    UINT: Unsigned + PrimInt + Binary,
{
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        

        // finds a non-empty uint, or returns None if the end is reached without encountering any zeros
        while self.nbr_ones_in_uint == 0 {
            // println!("cherche uint");
            
            self.uint_index += 1;
            if self.uint_index == self.bitset.nbr_used_uints() {
                return None;
            }
            
            self.working_uint = self.bitset.data[self.uint_index];
            self.nbr_ones_in_uint = self.working_uint.count_ones();
        }
        

        let one_left: UINT = UINT::one() << (8*size_of::<UINT>() - 1);
        loop {
            
            if (self.working_uint & one_left) == one_left {
                self.nbr_ones_in_uint -= 1;

                let res: Option<usize> = Some(8*size_of::<UINT>()*self.uint_index + self.index_in_uint);
                if res.unwrap() >= self.bitset.size() {
                    return None;
                }

                if self.nbr_ones_in_uint == 0 {
                    self.index_in_uint = 0; // returns to the start of the next uint at the next call of "next"
                }
                else {
                    self.working_uint = self.working_uint << 1; // skips to the next bit
                    self.index_in_uint += 1;
                }
                return res;
            }            
            else {
                self.working_uint = self.working_uint << 1;
                self.index_in_uint += 1;
            }
        }

    }

}


#[cfg(test)]
mod tests{
    use crate::datastructures::bitset::{BitSet, BitSetIter, MutBitSetIter};

    #[test]
    fn test1(){
        let table1: Vec<u8> = vec![0; 2];
        let table2: Vec<u8> = vec![0; 2];

        let mut bitset1: BitSet<u8>  = BitSet::new(table1, 10);
        let mut bitset2: BitSet<u8> = BitSet::new(table2, 10);

        for i in 0..bitset1.nbr_used_uints() {
            let rand_value: u8 = ((197 + i*157)%255) as u8 & 0b01101001;
            bitset1.data[i] = rand_value;
        }
        bitset1.data[1] = bitset1.data[1] & 0b11000000;

        

        for i in 0..bitset2.nbr_used_uints (){
            let rand_value: u8 = ((100 + i*37)%255) as u8 & 0b11001011;
            bitset2.data[i] = rand_value;
        }
        bitset2.data[1] = bitset2.data[1] & 0b11000000;


        println!("size: {}", bitset1.size());
        println!("bitset1.nbr_used_uints(): {}", bitset1.nbr_used_uints());
        bitset1.print();
        bitset2.print();

        bitset1.insert(3);
        bitset1.print();

        (bitset1.union(&bitset2)).print();
        (bitset1.intersection(&bitset2)).print();
        (bitset1.difference(&bitset2)).print();
        (bitset1.symmetric_difference(&bitset2)).print();

        bitset1.print();
        bitset2.print();

        for (i, value) in (&bitset1).into_iter().enumerate(){
            println!("i: {}, value: {}", i, value);
        }

        (bitset1.concatenate(&bitset2)).print();
        bitset1.print();

        let mut it: MutBitSetIter<u8> = (&mut bitset1).into_iter();
        for i in 0..5 {
            it.bitset.insert(i);
            while let Some(symb) = it.next() {
                println!("value youpi: {}", symb);
            }
            it.reset();
        }
        
    }
}




