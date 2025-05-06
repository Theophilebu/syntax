use thiserror::Error;


use super::machine::*;
use super::finite_automaton::ReturnValue;
use super::finite_automaton::dfa::*;
use crate::formal_language::*;


#[derive(Error, Debug)]
pub enum DfaLexerError {
    #[error("The char {c} is not in the alphabet")]
    InvalidChar{c: char},

    #[error("{}", err)]
    DfaError{err: DfaError}
}

impl From<DfaLexerError> for MachineError<DfaLexerError> {
    fn from(value: DfaLexerError) -> Self {
        MachineError::Other { other_err: value }
    }
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

pub struct DfaLexer<'dfa, 'alp>
where 'alp: 'dfa
{
    dfa: &'dfa Dfa<'alp, Symbol, ()>,
    error_symbol: Symbol,   // terminal symbol
}

impl <'dfa, 'alp> DfaLexer<'dfa, 'alp>
where 'alp: 'dfa
{
    pub fn get_dfa(&self) -> &'dfa Dfa<'alp, Symbol, ()> {
        self.dfa
    }

    pub fn tokenise_from_iter<IT: Iterator<Item = char>>(&self, input_stream: IT)
    -> Result<Vec<Token>, MachineError<DfaLexerError>> {

        let mut dfa_lexer_runner: DfaLexerRunner<'_, 'dfa, 'alp> = DfaLexerRunner::new(self);

        for c in input_stream {
            if !self.dfa.is_char_valid(c) {
                return Err(MachineError::Other { other_err: DfaLexerError::InvalidChar { c } });
            }
            dfa_lexer_runner.update(&c)?;
        }

        dfa_lexer_runner.finish();

        Ok(dfa_lexer_runner.tokens)
    }

    
}



pub struct DfaLexerRunner<'dfa_lexer, 'dfa, 'alp>
where
    'dfa: 'dfa_lexer,
    'alp: 'dfa,
{
    dfa_lexer: &'dfa_lexer DfaLexer<'dfa, 'alp>,

    dfa_runner: DfaRunner<'dfa, 'alp, Symbol, ()>,
    
    tokens: Vec<Token>,

    current_lexeme: String,
    lexeme_line: usize,
    lexeme_column: usize,

    run_info: RunInfo,
}



impl <'dfa_lexer, 'dfa, 'alp> Machine<char, Vec<Token>, DfaLexerError> 
for DfaLexerRunner<'dfa_lexer, 'dfa, 'alp>
where
    'dfa: 'dfa_lexer,
    'alp: 'dfa,
{
    fn clear(&mut self) {
        self.tokens = vec![];
    }

    fn get_run_info(& self) -> &RunInfo {
        &self.run_info
    }

    fn update(&mut self, c: &char) -> Result<(), MachineError<DfaLexerError>> {

        self.handle_dfa_end();
        // restarts the dfa_runner if needed, set it ready to start a new token 

        self.dfa_runner.update(c)?;
        self.current_lexeme.push(*c);

        Ok(())
    }

    fn get_state(&self) -> &Vec<Token> {
        &self.tokens
    }

}

impl <'dfa_lexer, 'dfa, 'alp> UnendingMachine<char, Vec<Token>, DfaLexerError> 
for DfaLexerRunner<'dfa_lexer, 'dfa, 'alp>
where
    'dfa: 'dfa_lexer,
    'alp: 'dfa,
{
    fn finish(&mut self) {
        // adds a token with the current lexeme
        // (this is usefull because it might have return a different token type if more chars were added to the lexeme) 
        self.handle_dfa_end();

        if self.current_lexeme.len() != 0 {

            let final_state_id: usize = *self.dfa_runner.get_state();
            let final_state_return_value: &ReturnValue<Symbol> = 
                &self.dfa_runner
                    .get_dfa()
                    .get_state(final_state_id)
                    .unwrap()
                    .return_value;
            // safe unwrap because final_state_id comes from dfa_runner.get_state() 

            let token_type_found: Symbol = match final_state_return_value {
                ReturnValue::Accepted => self.dfa_lexer.error_symbol.clone(),
                ReturnValue::NotAccepted => self.dfa_lexer.error_symbol.clone(),
                ReturnValue::Value(output_value) => output_value.clone(),
            };

            self.tokens.push(Token{
                token_type: token_type_found,
                lexeme: self.current_lexeme.clone(),
                line: self.lexeme_line,
                column: self.lexeme_column,
            });
        }

        self.run_info = RunInfo::Finished;

        
    }

}


impl <'dfa_lexer, 'dfa, 'alp> DfaLexerRunner<'dfa_lexer, 'dfa, 'alp>
where
    'dfa: 'dfa_lexer,
    'alp: 'dfa,
{
    fn handle_dfa_end(&mut self) {
        // called to handle the case in which the dfa should update but we don't know if it is finished yet
        // if it is not finished, nothing will happen
        // otherwise, it will add a token and reset the dfa so that it is ready to use

        if self.dfa_runner.is_finished() {
            let last_symbol: char = self.current_lexeme.pop().unwrap();

            let final_state_id: usize = *self.dfa_runner.get_state();
            let final_state_return_value: &ReturnValue<Symbol> = 
                &self.dfa_runner
                    .get_dfa()
                    .get_state(final_state_id)
                    .unwrap()// can't fail because it comes from dfa_runner.get_state()
                    .return_value;

            let token_type_found: Symbol = match final_state_return_value {
                ReturnValue::Accepted => self.dfa_lexer.error_symbol.clone(),
                ReturnValue::NotAccepted => self.dfa_lexer.error_symbol.clone(),
                ReturnValue::Value(output_value) => output_value.clone(),
            };

            self.tokens.push(Token{
                token_type: token_type_found,
                lexeme: self.current_lexeme.clone(),
                line: self.lexeme_line,
                column: self.lexeme_column,
            });

            self.dfa_runner.clear();
            self.dfa_runner.update(&last_symbol).unwrap();
            // safe unwrap because last_symbol was checked to be valid in the previous call of "update"

            let (next_line, next_column) = self.tokens.last().unwrap().next_position();

            if self.dfa_runner.is_finished() {
                // the char that can't be at the end of the last lexeme
                // also can't be at the start of a new lexeme
                // we add an error token with this char only as the lexeme

                self.tokens.push(Token{
                    token_type: self.dfa_lexer.error_symbol.clone(),
                    lexeme: last_symbol.to_string(),
                    line: next_line,
                    column: next_column, 
                });

                self.dfa_runner.clear();
                (self.lexeme_line, self.lexeme_column) = self.tokens.last().unwrap().next_position();

                self.current_lexeme = String::new();
            }
            else {
                (self.lexeme_line, self.lexeme_column) = (next_line, next_column);

                self.current_lexeme = last_symbol.to_string();
            }

        }
    }

    pub fn new(dfa_lexer: &'dfa_lexer DfaLexer<'dfa, 'alp>) -> Self {
        
        let dfa_runner: DfaRunner<'dfa, 'alp, Symbol, ()> = DfaRunner::new(dfa_lexer.get_dfa());

        DfaLexerRunner {
            dfa_lexer,
        
            dfa_runner,
            
            tokens: Vec::new(),
        
            current_lexeme: String::new(),
            lexeme_line: 0,
            lexeme_column: 0,
        
            run_info: RunInfo::Ready,
        }

    }

}



