//use thiserror::Error;

use super::super::machine::{Machine, RunInfo, MachineError};
use crate::datastructures::indexing::{Handle, Indexing};
use crate::formal_language::Alphabet;

use super::{ReturnValue, StateTransition, FAState};

use crate::Result;
use crate::Error;

// region: error handling


#[derive(Debug)]
pub enum DfaError {
    // #[error("Transitions {transition1:?} and {transition2:?} are incompatible, it would introduce nondeterminism")]
    NotDeterministic{transition1: StateTransition, transition2: StateTransition},

    // #[error("State Transition {transition:?} has the character {} which is not in the alphabet", transition.char_read)]
    InvalidTransitionChar{transition: StateTransition},

    // #[error("State Transition {transition:?} has the origin state id {} which is not a valid state id", transition.origin_state_id)]
    InvalidTransitionOrigin{transition: StateTransition},

    // #[error("State Transition {transition:?} has the target state id {} which is not a valid state id", transition.target_state_id)]
    InvalidTransitionTarget{transition: StateTransition},

    // #[error("The number of states({nbr_states}) is too large (max={})", OptionUint::max_value())]
    TooManyStates{nbr_states: usize},

    // #[error("The number of states {table_height} in the table doesn't match the length({vec_len}) of the vector states")]
    WrongNbrStates{table_height: usize, vec_len: usize},

    // #[error("The number of states {table_width} in the table doesn't match the size({alphabet_size}) of the alphabet")]
    WrongNbrChars{table_width: usize, alphabet_size: usize},

    // #[error("The number of states and valid chars can't be 0")]
    EmptyTable,

    // #[error("The table passed should be rectangular")]
    NonRectTable,
    
    // #[error("The char {c} is not in the alphabet")]
    InvalidChar{c: char},

    // #[error("The state id {state_id} is not valid")]
    InvalidState{state: FAState},
}

impl std::fmt::Display for DfaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for DfaError {}
/*
impl From<DfaError> for MachineError<DfaError> {
    fn from(value: DfaError) -> Self {
        MachineError::Other { other_err: value }
    }
}

*/

// endregion

pub struct Dfa<'alp, RETURN: Clone>
{
    // start_state is 1
    // can handle up to 65535 states
    transition_table: Vec<Vec<FAState>>,
    return_values: Vec<ReturnValue<RETURN>>,
    alphabet: &'alp Alphabet,

    // transition_table[origin_state_id][symbol_read_id] = target_state
}

impl <'alp, RETURN: Clone> Dfa<'alp, RETURN>
{
    pub fn nbr_chars(&self) -> usize {
        self.alphabet.size()
    }

    pub fn nbr_states(&self) -> <FAState as Handle>::Id {
        <FAState as Handle>::Id::from_usize(self.return_values.len())
    }

    pub fn char_id(&self, c: char) -> Option<usize> {
        self.alphabet.id(c)
    }

    pub fn is_char_valid(&self, c: char) -> bool {
        !self.char_id(c).is_none()
    }

    pub fn is_state_valid(&self, state: FAState) -> bool {
        // 0<state.id(()) && 
        state.id(())<self.nbr_states()
    }

    pub fn from_table(table: Vec<Vec<FAState>>, return_values: Vec<ReturnValue<RETURN>>,
    alphabet: &'alp Alphabet) -> Result<Self> {

        if table.len()==0 || table[0].len()==0 {
            return Err(Box::new(DfaError::EmptyTable));
            
        }

        if table.len() != return_values.len() {
            return Err(Box::new(DfaError::WrongNbrStates { table_height: table.len(), vec_len: return_values.len() }));
        }

        if table[0].len() != alphabet.size() {
            return Err(Box::new(DfaError::WrongNbrChars { table_width: table[0].len(), alphabet_size: alphabet.size() }));
        }

        let nbr_states: usize = table.len();
        if nbr_states>(FAState::MAX.into_usize()) {
            return Err(Box::new(DfaError::TooManyStates { nbr_states }));
        }

        let nbr_chars: usize = table[0].len();
        if table.iter().any(|v:&Vec<FAState>| v.len()!=nbr_chars) {
            return Err(Box::new(DfaError::NonRectTable));
        }

        Ok(Dfa {
            transition_table: table,
            return_values,
            alphabet,
        })
    }

    pub fn from_transitions( transitions: Vec<StateTransition>, states: Vec<FiniteAutomatonState<RETURN, DATA>>,
        alphabet: &'alp Alphabet) -> Result<Self, DfaError> {

        let nbr_chars: usize = alphabet.size();
        let nbr_states: usize = states.len();

        if nbr_states>(SINT::MAX as usize) {
            return Err(DfaError::TooManyStates { nbr_states });
        }

        // empty initial table
        let mut table: Vec<Vec<OptionUint<SINT>>> = 
        vec![vec![OptionUint::from(None);nbr_chars.into()]; nbr_states.into()];

        // checks each transition and adds an element to the table
        for transition in transitions {
            // checks char
            let opt_cher_id: Option<usize> = alphabet.id(transition.char_read);
            if let None = opt_cher_id {
                return Err(DfaError::InvalidTransitionChar {transition});
            }
            let char_id: usize = opt_cher_id.unwrap();

            // checks origin
            if transition.origin_state_id>=nbr_states {
                return Err(DfaError::InvalidTransitionOrigin {transition});
            }

            // checks target
            if transition.target_state_id>=nbr_states {
                return Err(DfaError::InvalidTransitionTarget {transition});
            }



            let opt_current_target: Option<usize> = 
            table[transition.origin_state_id][char_id].get_value();

            // checks that for each pair (state, symbol), there is at most one transition possible.
            if let Some(current_target) = opt_current_target {
                return Err(DfaError::NotDeterministic {
                    transition1: StateTransition {
                        origin_state_id: transition.origin_state_id,
                        char_read: transition.char_read,
                        target_state_id: current_target,
                    },
                    transition2: transition,
                });
            }

            let new_value: Option<usize> = Some(transition.target_state_id);
            table[transition.origin_state_id][char_id] = OptionUint::from(new_value);
        }
        
        Ok(Dfa{
            transition_table: table,
            return_values: states,
            alphabet,
        })
    }

    pub fn next_state_id(&self, current_state_id: usize, char_read: char) -> Result<Option<usize>, DfaError> {
        if !self.is_char_valid(char_read) {
            return Err(DfaError::InvalidChar { c: char_read });
        }

        if !self.is_state_id_valid(current_state_id) {
            return Err(DfaError::InvalidStateId { state_id: current_state_id });
        }

        Ok(self.transition_table[current_state_id][self.char_id(char_read).unwrap()].get_value())
    }

    pub fn get_state(&self, state_id: usize) -> Result<&FiniteAutomatonState<RETURN, DATA>, DfaError> {
        if !self.is_state_id_valid(state_id) {
            return Err(DfaError::InvalidStateId { state_id });
        }
        Ok(&self.return_values[state_id])
    }
}





pub struct DfaRunner<'dfa, 'alp, RETURN: Clone, DATA>
where
    'alp: 'dfa
{
    dfa: &'dfa Dfa<'alp, RETURN, DATA>,
    current_state_id: usize,
    run_info: RunInfo,
}

impl <'dfa, 'alp, RETURN: Clone, DATA> DfaRunner<'dfa, 'alp, RETURN, DATA>
where
    'alp: 'dfa
{
    pub fn new(dfa: &'dfa Dfa<'alp, RETURN, DATA>) -> Self {
        DfaRunner {
            dfa: dfa,
            current_state_id: 0,
            run_info: RunInfo::Ready,
        }
    }

    pub fn get_dfa(&self) -> &'dfa Dfa<'alp, RETURN, DATA> {
        self.dfa
    }
}

impl <'dfa, 'alp, RETURN: Clone, DATA> Machine<char, usize, DfaError> 
for DfaRunner<'dfa, 'alp, RETURN, DATA>
where
    'alp: 'dfa
{
    fn clear(&mut self){
        self.current_state_id = 0;
        self.run_info = RunInfo::Ready;
    }

    fn get_run_info(&self) -> &RunInfo {
        &self.run_info
    }

    fn update(&mut self, c: &char) -> Result<(), MachineError<DfaError>> {
        
        if self.is_finished() {
            return Err(MachineError::Finished);
        }

        let next_state_id: Option<usize> = self.dfa.next_state_id(self.current_state_id, *c)?;
        
        
        match next_state_id {
            None => {
                self.run_info = RunInfo::Finished;
            }
            Some(actual_next_state_id) => {
                self.run_info = RunInfo::Running;
                self.current_state_id = actual_next_state_id;
            } 
        }
        Ok(())
    }

    fn get_state(&self) -> &usize {
        &self.current_state_id
    }

}

