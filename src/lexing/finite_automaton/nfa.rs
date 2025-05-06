use thiserror::Error;

use super::{ReturnValue, FiniteAutomatonState, StateTransition, StateTransitionSet, SINT, UINT, EPS};
use super::super::machine::{Machine, MachineError, RunInfo};
use crate::{datastructures::bitset::BitSet, formal_language::Alphabet};






#[derive(Error, Debug)]
pub enum NfaError {
    #[error("State Transition {transition:?} has the character {} which is not in the alphabet", transition.char_read)]
    InvalidTransitionChar{transition: StateTransitionSet},

    #[error("State Transition {transition:?} has the origin state id {} which is not a valid state id", transition.origin_state_id)]
    InvalidTransitionOrigin{transition: StateTransitionSet},

    #[error("State Transition {transition:?} has a target of size different than the number of states")]
    InvalidTransitionTarget{transition: StateTransitionSet},

    #[error("The number of states({nbr_states}) is too large (max={})", SINT::MAX)]
    TooManyStates{nbr_states: usize},

    #[error("The number of states {table_height} in the table doesn't match the length({vec_len}) of the vector states")]
    WrongNbrStates{table_height: usize, vec_len: usize},

    #[error("The number of chars {table_width} in the table doesn't match the size({alphabet_size}) of the alphabet")]
    WrongNbrChars{table_width: usize, alphabet_size: usize},

    #[error("The number of states and valid chars can't be 0")]
    EmptyTable,

    #[error("The table passed should be rectangular")]
    NonRectTable,
    
    #[error("The char {c} is not in the alphabet")]
    InvalidChar{c: char},

    #[error("The state id {state_id} is not valid")]
    InvalidStateId{state_id: usize},

    #[error("The size of the state id set {state_id_set:?} should be equal to the number of states")]
    InvalidStateIdSet{state_id_set: BitSet<UINT>},
}

impl From<NfaError> for MachineError<NfaError> {
    fn from(value: NfaError) -> Self {
        MachineError::Other { other_err: value }
    }
}


pub struct Nfa<'alp, RETURN: Clone, DATA>
{
    // start_state is 0
    // can handle up to 32768 states (change SINT for more(why though))
    // EPS is state id: alphabet.size()
    transition_table: Vec<Vec<BitSet<UINT>>>,
    states: Vec<FiniteAutomatonState<RETURN, DATA>>,
    alphabet: &'alp Alphabet,

    // transition_table[origin_state_id][symbol_read_id] = bitset of target_state_id
}

impl <'alp, RETURN: Clone, DATA> Nfa<'alp, RETURN, DATA>
{
    pub fn nbr_chars(&self) -> usize {
        self.alphabet.size()
    }

    pub fn nbr_states(&self) -> usize {
        self.states.len()
    }

    pub fn char_id(&self, c: char) -> Option<usize> {
        if c==EPS {
            Some(self.nbr_chars())
        }
        else {
            self.alphabet.id(c)
        }
    }

    pub fn is_char_valid(&self, c: char) -> bool {
        // returns false for EPS
        !self.alphabet.id(c).is_none()
    }

    pub fn is_state_id_valid(&self, state_id: usize) -> bool {
        state_id<self.nbr_states()
    }

    pub fn from_table(table: Vec<Vec<BitSet<UINT>>>, states: Vec<FiniteAutomatonState<RETURN, DATA>>,
        alphabet: &'alp Alphabet) -> Result<Self, NfaError> {

        if table.len()==0 || table[0].len()==0 {
            return Err(NfaError::EmptyTable);
        }

        if table.len() != states.len() {
            return Err(NfaError::WrongNbrStates { table_height: table.len(), vec_len: states.len() });
        }

        if table[0].len() != alphabet.size() {
            return Err(NfaError::WrongNbrChars { table_width: table[0].len(), alphabet_size: alphabet.size() });
        }

        let nbr_states: usize = table.len();
        if nbr_states>(SINT::MAX as usize) {
            return Err(NfaError::TooManyStates { nbr_states });
        }

        let nbr_chars: usize = table[0].len();
        if table.iter().any(|v:&Vec<BitSet<UINT>>| v.len()!=nbr_chars) {
            return Err(NfaError::NonRectTable);
        }

        Ok(Nfa {
            transition_table: table,
            states,
            alphabet,
        })
    }

    pub fn from_transition_sets( transitions: Vec<StateTransitionSet>, states: Vec<FiniteAutomatonState<RETURN, DATA>>,
        alphabet: &'alp Alphabet) -> Result<Self, NfaError> {

        let nbr_chars: usize = alphabet.size();
        let nbr_states: usize = states.len();

        if nbr_states>(SINT::MAX as usize) {
            return Err(NfaError::TooManyStates { nbr_states });
        }

        // empty initial table
        let mut table: Vec<Vec<BitSet<UINT>>> = 
        vec![vec![BitSet::new_filled(false, nbr_chars+1);nbr_chars]; nbr_states];

        // checks each transition and adds an element to the table
        for transition in transitions {
            // checks char
            let opt_cher_id: Option<usize> = alphabet.id(transition.char_read);
            if let None = opt_cher_id {
                return Err(NfaError::InvalidTransitionChar {transition});
            }
            let char_id: usize = opt_cher_id.unwrap();

            let char_id: usize = {
                if transition.char_read == EPS {nbr_chars}
                else if let Some(char_id) = alphabet.id(transition.char_read) {char_id}
                else {return Err(NfaError::InvalidTransitionChar { transition });}
            };

            // checks origin
            if transition.origin_state_id>=nbr_states {
                return Err(NfaError::InvalidTransitionChar {transition});
            }

            // checks target: size of bitset
            if transition.target_state_ids.size() != nbr_states {
                return Err(NfaError::InvalidTransitionChar {transition});
            }

            table[transition.origin_state_id][char_id].update_union(&transition.target_state_ids);
        }
        
        Ok(Nfa{
            transition_table: table,
            states,
            alphabet,
        })
    }

    pub fn next_state_ids(&self, current_state_ids: &BitSet<UINT>, char_read: char) -> Result<BitSet<UINT>, NfaError> {
        // char_read can be EPS

        if current_state_ids.size() != self.nbr_states() {
            return Err(NfaError::InvalidStateIdSet { state_id_set: current_state_ids.clone() });
        }

        let Some(char_read_id) = self.char_id(char_read) else {
            return Err(NfaError::InvalidChar { c: char_read });
        };

        let mut next_state_ids: BitSet<UINT> = BitSet::new_filled(false, self.nbr_states());

        for possible_state_id in current_state_ids {
            next_state_ids.update_union(&self.transition_table[possible_state_id][char_read_id]);
        }

        Ok(next_state_ids)
    }

    pub fn get_state(&self, state_id: usize) -> Result<&FiniteAutomatonState<RETURN, DATA>, NfaError> {
        if !self.is_state_id_valid(state_id) {
            return Err(NfaError::InvalidStateId { state_id });
        }
        Ok(&self.states[state_id])
    }

}



