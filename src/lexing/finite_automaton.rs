pub mod dfa;
pub mod nfa;

use crate::datastructures::indexing::Handle;

// avoid useless generic parameter
type BitSet = crate::datastructures::bitset::BitSet<crate::BitSetUINT>;

// used to represent epsilon-transitions in nfa
const EPS: char = char::from_u32(0xE000).unwrap();  // private use area


/// finite automaton state
/// id: 0 represents None/invisible the trash bin state(not even counted as a state)
/// which means that id and id() are not the same numbers
/// because a field id=0 isn't a real state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FAState {
    pub id: u16,
}
impl Handle for FAState {
    
    type Context<'c> = ();
    type Id = u16;
    
    fn id(&self, _context: ()) -> Self::Id {
        self.id - 1
    }

    fn from_id(id: Self::Id, _context: ()) -> Self {
        FAState { id: id + 1 }
    }
}

impl FAState {

    const MAX: u16 = u16::MAX;

    fn none() -> Self {
        Self { id: 0 }
    }

    fn is_none(&self) -> bool {
        return self.id == 0;
    }
}


pub enum ReturnValue<RETURN: Clone>
{
    NotAccepted,
    Accepted,
    Value(RETURN),
}

#[derive(Debug, Clone)]
pub struct StateTransition {
    pub origin_state_id: FAState,
    pub char_read: char,
    pub target_state_id: FAState,
}

#[derive(Debug, Clone)]
struct StateTransitionSet
{
    origin_state_id: FAState,
    char_read: char,    // EPS is allowed
    target_state_ids: FAState,
}