pub mod dfa;
pub mod nfa;

use strum_macros::Display;

use crate::datastructures::bitset::BitSet;

use crate::UINT;
type SINT = i16; // used for state ids, increase from i16 to i32 for more states(32,768 to 2,147,483,648)


const EPS: char = char::from_u32(0xE000).unwrap();  // private use area


pub enum ReturnValue<RETURN: Clone>
{
    NotAccepted,
    Accepted,
    Value(RETURN),
}


pub struct FiniteAutomatonState<RETURN: Clone, DATA> {
    // data might often be empty type
    pub return_value: ReturnValue<RETURN>,
    pub data: DATA,
}

#[derive(Debug, Clone)]
pub struct StateTransition {
    pub origin_state_id: usize,
    pub char_read: char,
    pub target_state_id: usize,
}

#[derive(Debug, Clone)]
struct StateTransitionSet
{
    origin_state_id: usize,
    char_read: char,    // EPS is allowed
    target_state_ids: BitSet<UINT>,
}