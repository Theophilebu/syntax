use super::machine::*;
use super::finite_automaton::ReturnValue;
use super::finite_automaton::dfa::*;
// use crate::formal_language::symbols::{SymbolEnum, SymbolId, Term, TermId, NonTerm, NonTermId};
use crate::lexing::tokens::{
    TokenType, TokenTypeEnum, TokenTypeId, VariableTokenType, FixedTokenType, TokenTypeList,
    Token, TokenId, Position1D, Position2D, TokenList};

use crate::Result;
use crate::Error;


use crate::datastructures::indexing::{Handle, Indexing};

// region: error handling
/*

use thiserror::Error;


#[derive(Error, Debug)]
pub enum DfaLexerError {
    #[error("The char {c} is not in the alphabet")]
    InvalidChar{c: char},

    #[error("{}", err)]
    DfaError{err: DfaError}
}


impl From<MachineError<DfaError>> for MachineError<DfaLexerError> {
    fn from(value: MachineError<DfaError>) -> Self {

        match value {
            MachineError::Finished => MachineError::Finished,
            MachineError::Other { other_err } => {
                MachineError::Other { other_err: DfaLexerError::DfaError { err: other_err } }
            }
        }
        
    }
}
*/
// endregion


/*

token_type_list: TokenTypeList,

    nbr_fixed_tokens: TokenId,
    nbr_variable_tokens: TokenId,

    // indexed by tokens
    token_types: Vec<TokenType>,

    // lexemes for the variable tokens
    // each variable lexeme is a substring of lexemes
    variable_lexemes: String,

    // indexed by Token, but should only be used with variable tokens
    // maps a token to the start of its lexeme
    // if token is fixed, lexeme_offsets[token.id()] == lexeme_offsets[token.id()+1]
    lexeme_offsets: Vec<u32>,

    // holds the position of each newline '\n' character
    new_lines: Vec<Position1D>,

    // indexed by tokens
    token_positions: Vec<Position1D>,

*/
#[derive(Debug)]
pub struct DfaLexerRunner<'dfa>
{
    // parameters
    dfa_runner: DfaRunner<'dfa, TokenType>,
    token_type_list: TokenTypeList,
    error_token_type: VariableTokenType,
    ignore_token_type: VariableTokenType,
    

    // what TokenList needs

    nbr_fixed_tokens: TokenId,
    nbr_variable_tokens: TokenId,

    // indexed by tokens
    token_types: Vec<TokenType>,

    // lexemes for the variable tokens
    // each variable lexeme is a substring of lexemes
    variable_lexemes: String,

    // indexed by Token, but should only be used with variable tokens
    // maps a token to the start of its lexeme
    // if token is fixed, lexeme_offsets[token.id()] == lexeme_offsets[token.id()+1]
    lexeme_offsets: Vec<u32>,

    // holds the position of each newline '\n' character
    new_lines: Vec<Position1D>,

    // indexed by tokens
    token_positions: Vec<Position1D>,

    
    // information updated often (like every token)

    nbr_chars_read: u32,
    current_lexeme_position: u32,
    current_lexeme: String,
    last_char_read: char,
    run_info: RunInfo,
}



impl <'dfa> DfaLexerRunner<'dfa>
{
    pub fn new(dfa_runner: DfaRunner<'dfa, TokenType>, token_type_list: TokenTypeList,
     error_token_type: VariableTokenType, ignore_token_type: VariableTokenType) -> Self { 
        Self {
            dfa_runner,
            token_type_list,
            error_token_type,
            ignore_token_type,


            nbr_fixed_tokens: 0,
            nbr_variable_tokens: 0,
            token_types: Vec::new(),
            
            variable_lexemes: String::new(),
            lexeme_offsets: Vec::new(),

            new_lines: Vec::new(),
            token_positions: Vec::new(),
            
            nbr_chars_read: 0,
            current_lexeme_position: 0,
            current_lexeme: String::new(),
            last_char_read: '¤',      // it will be initialised when the first char is read
            run_info: RunInfo::Ready,
        }
    }

    pub fn clear(&mut self) {
        self.dfa_runner.clear();

        self.nbr_fixed_tokens = 0;
        self.nbr_variable_tokens = 0;
        self.token_types.clear();
        
        self.variable_lexemes.clear();
        self.lexeme_offsets.clear();

        self.new_lines.clear();
        self.token_positions.clear();
        
        self.nbr_chars_read = 0;
        self.current_lexeme_position = 0;
        self.current_lexeme.clear();
        self.last_char_read = '¤';      // it will be initialised when the first char is read
        self.run_info = RunInfo::Ready;
    }

    pub fn get_run_info(& self) -> &RunInfo {
        &self.run_info
    }

    /// returns an error only if:
    /// - the char read is not valid(in the first unicode private use area)
    /// - the lexer is finished
    pub fn read_char(&mut self, c: char) -> Result<()> {

        if *self.get_run_info() == RunInfo::Finished {
            return Err("the lexer can't read a char because it is finished".into());
        }

        self.run_info = RunInfo::Running;

        // here, the dfa can be finished or not

        // restarts the dfa_runner if needed, set it ready to start a new token 
        self.handle_dfa_end();

        println!("char read: {c}, dfa run info: {:#?}", self.dfa_runner.get_run_info());

        self.dfa_runner.update(c)?;

        self.last_char_read = c;

        // if the new char doesn't fit the end of the current lexeme, we don't add it to the current lexeme
        // also, nbr_chars_read and new_lines are updated only when dfa_runner isn't running to avoid duplicates
        if !self.dfa_runner.is_finished() {
            self.current_lexeme.push(c);
            self.nbr_chars_read += 1;
            if c == '\n' {
                self.new_lines.push(Position1D { pos: self.nbr_chars_read });
            }
            
        }
        Ok(())
    }

    pub fn read_string(&mut self, input_string: &str) {
        // maybe preallocations of vectors with the estimated final size could be good? not much imo

        for c in input_string.chars() {
            self.read_char(c);
        }
    }

    // restarts the dfa_runner if needed, set it ready to start a new token 
    pub fn handle_dfa_end(&mut self) {

        // if the dfa is not finished, no problem
        if !self.dfa_runner.is_finished() {
            return;
        }

        println!("handling dfa end");

        /*
        if it is finished, it means that the last time it updated, there was no state to go to.
        there are two cases:
        1) The last state of the dfa is a valid token type -> nice
        2) It is not -> we put the error_token_type instead
        
        Slight inconvenient distinction:
         2.1) The dfa is not on the initial state -> 
               we can take all the previous chars(the last read is excluded) into a lexeme and make it an error token
               we then read again the very last char read as 
        
         2.2) The dfa is still on the initial state -> the first char can't be at the beginning of a token
              We can't read this char again because it would create an infinite loop

        In case 1 and 2.1, we clear the dfa to run it again from the initial state with the coming chars
        
        examples of case 2 in rust: 
        let ¤ = +1;
            ^unknown start of token: \u{a4}
        
        let ab¤ = +1;
            ^^^unknown start of token: \u{a4}, Syntax Error: expected SEMICOLON, unused variable: `ab`
        
        here rust analyser still knows that "ab" is a variable: when it saw '¤', it probably registered all
        the previous chars ("ab" here) as an identifier
        It then probably read it again and found the "unknown start of token: \u{a4}" error, like 2.2
        
        let ab ¤ = +1;
            ^  ^unknown start of token: \u{a4}
            |Syntax Error: expected SEMICOLON, unused variable: `ab`
        
        here both a and ¤ are underlined by rust analyzer
        */

        let mut read_again: bool = true;
        let dfa_state = self.dfa_runner.get_state();
        match dfa_state {
            ReturnValue::Value(token_type) => 'value: {

                if token_type == self.token_type_list.into_token_type(TokenTypeEnum::Variable(self.ignore_token_type)) {
                    break 'value;
                }

                // save the new token found
                self.token_types.push(token_type);
                self.token_positions.push(Position1D { pos: self.current_lexeme_position});

                match self.token_type_list.into_enum(token_type) {
                    TokenTypeEnum::Fixed(_fixed_token_type) => {
                        self.nbr_fixed_tokens += 1;
                    },
                    TokenTypeEnum::Variable(_variable_token_type) => {
                        self.nbr_variable_tokens += 1;

                        // add variable lexeme
                        self.lexeme_offsets.push(u32::from_usize(self.variable_lexemes.len()));
                        // the current lexeme doesn't contain the last char read
                        self.variable_lexemes.push_str(&self.current_lexeme);
                    },
                }
            },

            // case 2
            // whether it is Accepted or NotAccepted isn't relevant here,
            // both are equivalent because they don't give the token type
            _ => {
                self.token_types.push(self.token_type_list.into_token_type(TokenTypeEnum::Variable(self.error_token_type)));
                self.token_positions.push(Position1D { pos: self.current_lexeme_position});

                self.nbr_variable_tokens += 1;

                // add variable lexeme
                self.lexeme_offsets.push(u32::from_usize(self.variable_lexemes.len()));


                // case 2.1: add the current error token
                if self.current_lexeme.len()>0 {
                    // the current lexeme doesn't contain the last char read
                    self.variable_lexemes.push_str(&self.current_lexeme);
                }

                // case 2.2: add the last read char as error token, and it won't be read again
                else {
                    // the current lexeme doesn't contain the last char read
                    self.variable_lexemes.push(self.last_char_read);

                    self.nbr_chars_read += 1;

                    read_again = false;
                }
            }
        }

        self.dfa_runner.clear();
        self.current_lexeme_position = self.nbr_chars_read;
        self.current_lexeme.clear();

        if read_again {
            self.read_char(self.last_char_read);
        }

    }

    pub fn finish(&mut self) {

        // if the last char read lead to a dfa end
        self.handle_dfa_end();

        // act like the dfa can't go anywhere -> handle the remaining chars just like if a new token started after
        self.dfa_runner.finish();
        self.handle_dfa_end();
        self.run_info = RunInfo::Finished;

        // example 12+3
        // "3" ended the "+" token, it needs to be handled -> reads again '3'
        // next, we manually end the dfa runner and handle this to add "3" as a new token
    }

}



