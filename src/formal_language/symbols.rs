use crate::datastructures::indexing::{Indexing, Handle};


// avoid useless generic parameter
use crate::BitSetUINT;
type BitSet = crate::datastructures::bitset::BitSet<BitSetUINT>;


pub type SymbolId = u16;
pub type NonTermId = SymbolId;
pub type TermId = SymbolId;

pub struct Alphabet {
    chars: Vec<char>,   // sorted, no duplicates
}

impl Alphabet {

    pub fn new(mut chars: Vec<char>) -> Self {
        chars.sort();
        chars.dedup();
        Alphabet { chars: chars }
    }

    pub fn id(&self, c: char) -> Option<usize> {
        self.chars.binary_search(&c).ok()
    }

    pub fn size(&self) -> usize {
        self.chars.len()
    }

    pub fn chars(&self) -> &[char] {
        &self.chars
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct NonTerm {
    pub id: NonTermId,
}
impl Handle for NonTerm {
    type Id = NonTermId;
    type Context<'c> = ();

    fn id(&self, _: ()) -> NonTermId {
        self.id
    }
    fn from_id(id: NonTermId, _: ()) -> Self {
        return NonTerm {id};
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Term {
    pub id: TermId,
}
impl Handle for Term {
    type Id = TermId;
    type Context<'c> = ();

    fn id(&self, _: ()) -> TermId {
        self.id
    }
    fn from_id(id: TermId, _: ()) -> Self {
        return Term {id};
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Symbol {
    pub id: SymbolId,
}
impl Handle for Symbol {
    type Id = SymbolId;
    type Context<'c> = ();

    fn id(&self, _: ()) -> SymbolId {
        self.id
    }
    fn from_id(id: SymbolId, _: ()) -> Self {
        return Symbol {id};
    }
}

impl Symbol {
    
    /// here context is the number of non-terminals
    pub fn into_enum(&self, context: NonTermId) -> SymbolEnum {
        if self.id<context {
            return SymbolEnum::NonTerm(NonTerm { id: self.id });
        }
        else {
            return SymbolEnum::Term(Term{ id: self.id - context});
        }
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum SymbolEnum {
    NonTerm(NonTerm),
    Term(Term),
}

impl SymbolEnum {
    
    /// here context is the number of non-terminals
    pub fn into_symbol(&self, context: NonTermId) -> Symbol {
        match self {
            SymbolEnum::NonTerm(non_term) => Symbol { id: non_term.id },
            SymbolEnum::Term(term) => Symbol { id: term.id + context },
        }
    }
}




