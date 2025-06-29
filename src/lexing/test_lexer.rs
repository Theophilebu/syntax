


#[cfg(test)]
mod tests{

    use crate::lexing::tokens::*;
    use crate::lexing::char_class::*;
    use crate::lexing::dfa_lexer::DfaLexerRunner;
    use crate::lexing::finite_automaton::{*, dfa::*};
    #[test]
    fn test1() {

        // token types

        let token_type_list: TokenTypeList = TokenTypeList {
            nbr_fixed_token_types: 4,
            nbr_variable_token_types: 4,
            token_type_representations: vec![
                String::from("+"),
                String::from("-"),
                String::from("*"),
                String::from("/"),
                String::from("INTEGER"),
                String::from("FLOAT"),
                String::from("ERROR"),
                String::from("IGNORE"),
                ],
            fixed_token_lexemes: vec![
                String::from("+"),
                String::from("-"),
                String::from("*"),
                String::from("/"),
            ],
        };

        let PLUS_TT: TokenType = TokenType { id: 0};
        let MINUS_TT: TokenType = TokenType { id: 1};
        let STAR_TT: TokenType = TokenType { id: 2};
        let SLASH_TT: TokenType = TokenType { id: 3};
        let INTEGER_TT: TokenType = TokenType { id: 4};
        let FLOAT_TT: TokenType = TokenType { id: 5};
        let ERROR_TT: TokenType = TokenType { id: 6};
        let IGNORE_TT: TokenType = TokenType { id: 7};


        // char sets

        let char_set_list: CharSetList = CharSetList {
            membership_functions: vec![Box::new(|c: char| c.is_numeric())]
        };

        let CSC = |c: char| CompactExtendedChar::from(ExtendedChar::Char(c));

        let DIGIT_CS: CharSet = CharSet { id: 0};
        let DIGIT_C: CompactExtendedChar = CompactExtendedChar::from(ExtendedChar::CharSet(DIGIT_CS));


        // dfa states and transitions

        let START_S: FAState = FAState { id: 1 };
        let INTEGER_S: FAState = FAState { id: 2 };
        let FLOAT_S: FAState = FAState { id: 3 };
        let PLUS_S: FAState = FAState { id: 4 };
        let MINUS_S: FAState = FAState { id: 5 };
        let STAR_S: FAState = FAState { id: 6 };
        let SLASH_S: FAState = FAState { id: 7 };
        let POINT_S: FAState = FAState { id: 8 };
        let INVISIBLE_S: FAState = FAState { id: 9 };

        let return_values: Vec<ReturnValue<TokenType>> = vec![
            ReturnValue::NotAccepted,
            ReturnValue::Value(INTEGER_TT),
            ReturnValue::Value(FLOAT_TT),
            ReturnValue::Value(PLUS_TT),
            ReturnValue::Value(MINUS_TT),
            ReturnValue::Value(STAR_TT),
            ReturnValue::Value(SLASH_TT),
            ReturnValue::NotAccepted,
            ReturnValue::Value(IGNORE_TT),
        ];


        // sorted by origin state
        let transitions: Vec<StateTransition> = vec![
            StateTransition {origin_state: START_S, char_read: DIGIT_C, target_state: INTEGER_S},

            StateTransition {origin_state: START_S, char_read: CSC('+'), target_state: PLUS_S},
            StateTransition {origin_state: START_S, char_read: CSC('-'), target_state: MINUS_S},
            StateTransition {origin_state: START_S, char_read: CSC('*'), target_state: STAR_S},
            StateTransition {origin_state: START_S, char_read: CSC('/'), target_state: SLASH_S},

            StateTransition {origin_state: START_S, char_read: CSC(' '), target_state: INVISIBLE_S},
            StateTransition {origin_state: START_S, char_read: CSC('\t'), target_state: INVISIBLE_S},
            StateTransition {origin_state: START_S, char_read: CSC('\n'), target_state: INVISIBLE_S},


            StateTransition {origin_state: INTEGER_S, char_read: DIGIT_C, target_state: INTEGER_S},
            StateTransition {origin_state: INTEGER_S, char_read: CSC('.'), target_state: FLOAT_S},


            StateTransition {origin_state: FLOAT_S, char_read: DIGIT_C, target_state: FLOAT_S},


            StateTransition {origin_state: INVISIBLE_S, char_read: CSC(' '), target_state: INVISIBLE_S},
            StateTransition {origin_state: INVISIBLE_S, char_read: CSC('\t'), target_state: INVISIBLE_S},
            StateTransition {origin_state: INVISIBLE_S, char_read: CSC('\n'), target_state: INVISIBLE_S},
        ];

        let dfa: Dfa<TokenType> = Dfa::from_transitions(transitions, return_values, char_set_list).unwrap();

        let dfa_runner = DfaRunner::new(&dfa);

        let error_token_type: VariableTokenType = token_type_list.to_variable(ERROR_TT);
        let ignore_token_type: VariableTokenType = token_type_list.to_variable(IGNORE_TT);
        let mut dfa_lexer_runner = DfaLexerRunner::new(dfa_runner, token_type_list, error_token_type, ignore_token_type);

        // dfa_lexer_runner.read_string("12+3*456");
        dfa_lexer_runner.read_string("12+3");

        dfa_lexer_runner.finish();

        println!("{:#?}", dfa_lexer_runner);

    }
}