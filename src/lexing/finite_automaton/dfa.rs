use super::super::machine::{Automaton, RunInfo, AutomatonError};
use crate::datastructures::flat_table::FlatTable;
use crate::datastructures::indexing::{Handle, Indexing};
use crate::formal_language::symbols::Alphabet;
use super::{ReturnValue, StateTransition, FAState, FAStateId};
use crate::lexing::char_class::{is_char_in_first_PUA, CompactExtendedChar, ExtendedChar, CharSetList};

use crate::{Error, Result};

// region: error handling


#[derive(Debug)]
pub enum DfaError {
    // #[error("Transitions {transition1:?} and {transition2:?} are incompatible, it would introduce nondeterminism")]
    // NotDeterministic{transition1: StateTransition, transition2: StateTransition},

    // #[error("State Transition {transition:?} has the character {} which is not in the alphabet", transition.char_read)]
    InvalidTransitionChar{transition: StateTransition},

    // #[error("State Transition {transition:?} has the origin state id {} which is not a valid state id", transition.origin_state_id)]
    InvalidTransitionOrigin{transition: StateTransition},

    // #[error("State Transition {transition:?} has the target state id {} which is not a valid state id", transition.target_state_id)]
    InvalidTransitionTarget{transition: StateTransition},

    // #[error("The number of states {table_height} in the table doesn't match the length({vec_len}) of the vector states")]
    WrongNbrStates{table_height: FAStateId, expected_nbr_states: FAStateId},

    TooManyStates { input_nbr_states: usize },

    // #[error("The number of states and valid chars can't be 0")]
    EmptyTable,

    UnexpectedEpsilon {extended_char: ExtendedChar},

    // #[error("The state id {state_id} is not valid")]
    InvalidState{state: FAState},

    InvalidChar { c: char },
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
        MachineError::Other(value)
    }
}
*/


// endregion

/// start state has field id: 1
/// RETURN type is what a state returns if the automaton ends on this state
/// can handle up to 65535 states
#[derive(Debug)]
 pub struct Dfa<RETURN: Clone>
{
    // ColId: usize: could be anything, because we do not care about accessing an element by a char/char set
    transition_table: FlatTable<(CompactExtendedChar, FAState), FAStateId, usize, FAStateId>,

    return_values: Vec<ReturnValue<RETURN>>,

    char_set_list: CharSetList,


    // self.overlapping_transitions() is true if for some origin state, 
    // there are multiple transitions that accept the same char(not extended).
    // In this case, because a Dfa should be deterministic, we just pick the first transition that matches
}

impl <RETURN: Clone> Dfa<RETURN>
{

    pub fn nbr_states(&self) -> FAStateId {
        FAStateId::from_usize(self.return_values.len())
    }

    pub fn is_state_valid(&self, state: FAState) -> bool {
        !state.is_none() && state.id(())<self.nbr_states()
    }

    pub fn from_table(table: FlatTable<(CompactExtendedChar, FAState), FAStateId, usize, FAStateId>,
     return_values: Vec<ReturnValue<RETURN>>, char_set_list: CharSetList) -> Result<Self> {

        if table.size()==0 {
            return Err(Box::new(DfaError::EmptyTable));
            
        }

        if table.nbr_rows().into_usize() != return_values.len() {
            return Err(Box::new(DfaError::WrongNbrStates { table_height: table.nbr_rows(), expected_nbr_states: FAStateId::from_usize(return_values.len()) }));
        }

        // check for epsilon transitions (forbidden)
        for (compact_extended_char, _state) in &table.values {
            if compact_extended_char.is_none() {
                return Err(Box::new(DfaError::UnexpectedEpsilon {
                    extended_char: ExtendedChar::from(*compact_extended_char)
                }));
            }
        }

        Ok(Dfa {
            transition_table: table,
            return_values,
            char_set_list,
        })
    }

    /// requires transitions to be sorted by origin state
    pub fn from_transitions(transitions: Vec<StateTransition>, return_values: Vec<ReturnValue<RETURN>>,
    char_set_list: CharSetList) -> Result<Self> {

        if FAStateId::try_from(return_values.len()).is_err() {
            return Err(Box::new(DfaError::TooManyStates { input_nbr_states: return_values.len() }));
        }

        let nbr_states: FAStateId = FAStateId::from_usize(return_values.len());
        let nbr_states_usize: usize = return_values.len();

        // empty initial table
        let mut values: Vec<(CompactExtendedChar, FAState)> = Vec::with_capacity(transitions.len());
        let mut rows: Vec<FAStateId> = Vec::with_capacity(nbr_states_usize);

        let mut current_row_id: FAStateId = 0;
        let mut current_nbr_values: FAStateId = 0;
        let mut old_nbr_values: FAStateId = 0; // number of transitions with state id smaller than the current row id

        // checks each transition and adds an element to the table
        for transition in transitions {

            // check for epsilon transitions (forbidden)
            if transition.char_read.is_none() {
                return Err(Box::new(DfaError::UnexpectedEpsilon {
                    extended_char: ExtendedChar::from(transition.char_read)
                }));
            }
            

            // checks origin
            if transition.origin_state.is_none()
                || transition.origin_state.id(())>=nbr_states {
                return Err(Box::new(DfaError::InvalidTransitionOrigin {transition}));
            }

            // checks target
            if transition.target_state.is_none()
                || transition.target_state.id(())>=nbr_states {
                return Err(Box::new(DfaError::InvalidTransitionTarget {transition}));
            }

            let new_value: (CompactExtendedChar, FAState) = (transition.char_read, transition.target_state);
            values.push(new_value);
            current_nbr_values+=1;

            let row_difference: FAStateId = transition.origin_state.id(()) - current_row_id;

            // iter::repeat_n because of emtpy rows 
            rows.extend(std::iter::repeat_n(
                old_nbr_values, 
                row_difference.into_usize(),
            ));

            if row_difference > 0 {
                old_nbr_values = current_nbr_values - 1; // we don't include the one we just added
            }
            

            current_row_id = transition.origin_state.id(());
        }

        rows.push(old_nbr_values);

        rows.extend(std::iter::repeat_n(
            nbr_states, 
            nbr_states_usize - rows.len(),
        ));



        let table: FlatTable<(CompactExtendedChar, FAState), FAStateId, usize, FAStateId> 
        = FlatTable::new(values, rows);
        
        Ok(Dfa{
            transition_table: table,
            return_values,
            char_set_list,
        })
    }

    /// returns an error only if the char read is not valid(in the first unicode private use area)
    /// If there is no next state to go, returns Ok(FAState::none())
    pub fn next_state(&self, current_state: FAState, char_read: char) -> Result<FAState> {

        if is_char_in_first_PUA(char_read) {
            return Err(Box::new(DfaError::InvalidChar { c: char_read }));
        }

        let row: &[(CompactExtendedChar, FAState)] = self.transition_table.get_row(current_state.id(()));

        for &(compact_extended_char, state) in row.iter() {
            if compact_extended_char.match_char(char_read, &self.char_set_list) {
                return Ok(state);
            }

        }

        Ok(FAState::none())
    }

    pub fn get_state_value(&self, state: FAState) -> Result<ReturnValue<RETURN>> {
        if !self.is_state_valid(state) {
            return Err(Box::new(DfaError::InvalidState { state }));
        }
        Ok(self.return_values[state.id(()).into_usize()].clone())
    }

    pub fn has_overlap(&self, alphabet: &Alphabet) -> bool {
        for origin_state_id in 1..self.nbr_states()+1 {
            let _origin_state: FAState = FAState::from_id(origin_state_id, ());
            for &c in alphabet.chars() {
                let mut char_matched: bool = false;
                for &(compact_extended_char, _target_state) in self.transition_table.get_row(origin_state_id) {
                    if compact_extended_char.match_char(c, &self.char_set_list) {
                        if char_matched {
                            return true;
                        }
                        char_matched = true;
                    }

                }
            }
        }
        true
    }

}


#[derive(Debug)]
pub struct DfaRunner<'dfa, RETURN: Clone>
{
    dfa: &'dfa Dfa<RETURN>,
    current_state: FAState,
    run_info: RunInfo,
}

impl <'dfa, RETURN: Clone> DfaRunner<'dfa, RETURN>
{
    pub fn new(dfa: &'dfa Dfa<RETURN>) -> Self {
        DfaRunner {
            dfa: dfa,
            current_state: FAState {id: 1},
            run_info: RunInfo::Ready,
        }
    }

    pub fn get_dfa(&self) -> &'dfa Dfa<RETURN> {
        self.dfa
    }
}

impl <'dfa, RETURN: Clone> Automaton<char, ReturnValue<RETURN>, Error> 
for DfaRunner<'dfa, RETURN>
{
    fn clear(&mut self) {
        self.current_state = FAState {id: 1};
        self.run_info = RunInfo::Ready;
    }

    fn get_run_info(&self) -> RunInfo {
        return self.run_info;
    }

    // if an error occurs it has no effect on self
    fn update(&mut self, c: char) -> std::result::Result<(), AutomatonError<Error>>{
        
        if self.is_finished() {
            return Err(AutomatonError::Finished);
        }

        let next_state: FAState = self.dfa.next_state(self.current_state, c)?;
        
        
        if next_state.is_none() {
            self.run_info = RunInfo::Finished;
        }
        else {
            self.run_info = RunInfo::Running;
            self.current_state = next_state;
        }

        Ok(())
    }

    fn get_state(&self) -> ReturnValue<RETURN> {
        self.dfa.get_state_value(self.current_state).unwrap()
    }

    fn finish(&mut self) {
        self.run_info = RunInfo::Finished;
    }
}