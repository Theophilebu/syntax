use crate::datastructures::{flat_table::FlatTable, indexing::{Handle, Indexing}};

// region: Position

/// position in the source text file where a \n is not considered a special character
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position1D {
    pub pos: u32,
}

/// position in the source text file
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position2D {
    pub line: u32,
    pub column: u32,
}

// endregion


// region: TokenType

pub type TokenTypeId = u16;
pub type FixedTokenTypeId = TokenTypeId;
pub type VariableTokenTypeId = TokenTypeId;

/// represents a type of token that always has the same representation in the source file
/// each keyword is a FixedTokenType
/// each operator and each delimiter is also a FixedTokenType
#[derive(PartialEq, Clone, Copy, Debug)]
pub struct FixedTokenType {
    pub id: FixedTokenTypeId,
}
impl Handle for FixedTokenType {
    type Id = FixedTokenTypeId;
    type Context<'c> = ();

    fn id(&self, _: ()) -> FixedTokenTypeId {
        self.id
    }
    fn from_id(id: FixedTokenTypeId, _: ()) -> Self {
        return FixedTokenType { id };
    }
}

/// represents a type of token that doesn't always have the same representation in the source file
/// each identifier type, string literal, number literal is a VariableTokenType
#[derive(PartialEq, Clone, Copy, Debug)]
pub struct VariableTokenType {
    pub id: VariableTokenTypeId,
}
impl Handle for VariableTokenType {
    type Id = VariableTokenTypeId;
    type Context<'c> = ();

    fn id(&self, _: ()) -> VariableTokenTypeId {
        self.id
    }
    fn from_id(id: VariableTokenTypeId, _: ()) -> Self {
        return VariableTokenType { id };
    }
}



#[derive(PartialEq, Clone, Copy, Debug)]
pub struct TokenType {
    pub id: u16,
}
impl Handle for TokenType {
    type Context<'c> = ();
    type Id = u16;

    fn id<'c>(&self, _: ()) -> Self::Id {
        self.id
    }

    fn from_id<'c>(id: Self::Id, _: ()) -> Self {
        Self { id: id }
    }
}



#[derive(PartialEq, Clone, Copy, Debug)]
pub enum TokenTypeEnum {
    Fixed(FixedTokenType),
    Variable(VariableTokenType),
}


#[derive(Debug)]
pub struct TokenTypeList {

    pub nbr_fixed_token_types: FixedTokenTypeId,
    pub nbr_variable_token_types: VariableTokenTypeId,

    // just to show meaningful info instead of ids
    // indexed by TokenType
    pub token_type_representations: Vec<String>,

    // lexemes (what we see in source file)
    // indexed by FixedTokenType
    pub fixed_token_lexemes: Vec<String>,

    // usually, the representation of fixed token tpyes should be the same as the lexeme
}

impl TokenTypeList {

    pub fn is_fixed(&self, token_type: TokenType) -> bool {
        return token_type.id < self.nbr_fixed_token_types;
    }

    pub fn is_variable(&self, token_type: TokenType) -> bool {
        return token_type.id >= self.nbr_fixed_token_types;
    }

    pub fn into_enum(&self, token_type: TokenType) -> TokenTypeEnum {
        if token_type.id < self.nbr_fixed_token_types {
            return TokenTypeEnum::Fixed(FixedTokenType { id: token_type.id });
        }
        else {
            return TokenTypeEnum::Variable(VariableTokenType{ id: token_type.id - self.nbr_fixed_token_types});
        }
    }

    // kinda useless
    pub fn into_token_type(&self, token_type_enum: TokenTypeEnum) -> TokenType {
        match token_type_enum {
            TokenTypeEnum::Fixed(fixed_token_type) => {
                TokenType { id: fixed_token_type.id(()) }
            },
            TokenTypeEnum::Variable(variable_token_type) => {
                TokenType { id: variable_token_type.id(()) + self.nbr_fixed_token_types}
            }
        }
    }

    // unchecked
    pub fn to_fixed(&self, token_type: TokenType) -> FixedTokenType {
        FixedTokenType { id: token_type.id }
    }

    // unchecked
    pub fn to_variable(&self, token_type: TokenType) -> VariableTokenType {
        VariableTokenType { id: token_type.id - self.nbr_fixed_token_types }
    }

    // ---

    pub fn fixed_token_lexeme(&self, fixed_token_type: FixedTokenType) -> &str {
        &self.fixed_token_lexemes[fixed_token_type.usize_id(())]
    }



}

// endregion

/*
option1
token_types: Vec<TokenTypeId>, index by tokens
lexemes: String // large string containing all variable lexemes one after another
lexeme_offsets: Vec<u32>, same length as token_types = the number of tokens

-> a bit of wasted memory in lexeme offsets (it can be indexed by a fixed token but it's useless)
    -> should be fine
-> no easy distinction between fixed and variable tokens: no iteration of a single token type
-> to iterate over all variable tokens
    -> we must iterate over every token
    -> or create a specific vec with copied data

option2

// id of tokens would not correspond to their place in the file
// id cannot be computed before the lexing ended (context is nbr of fixed tokens)
enum Token {
    FixedToken(FixedToken),
    VariableToken(VariableToken),
}

fixed_token_types: Vec<<FixedTokenType as Handle>::Id>, index by fixed tokens
variable_token_types: Vec<<VariableTokenType as Handle>::Id>, index by variable tokens



fn token_type(token: Token) -> TokenTypeId {
    match token {
        FixedToken(fixed_token) => fixed_token_types[token.id()],
        VariableToken(variable_token) => variable_token_types[token.id() - nbr_fixed_tokens],

    }
}

lexemes: String // large string containing all variable lexemes one after another
lexeme_offsets: Vec<u32>, same length as variable_token_types = the number of variable tokens

-> can't iterate simply over all tokens in order(actually yes )
-> tokens next to each other might not have an id next to each other
    -> we must create a vec with copied data of each token id, but with the actual order
    -> or create an array which contains the token id of the nth token (worse because one more dereference)


chosen option: a mess between the two (might change)
*/


// FixedToken, VariableToken, TokenEnum are not strictly necessary types
// , but they help to write code
// They are all just alliases of Token but with compile-time information
// FixedToken, VariableToken are not handles, because token data (token type) is not separated two separate arrays
// The only way of storing token data that makes sense is to order them by the position of the token


pub type TokenId = u32;


#[derive(PartialEq, Clone, Copy, Debug)]
pub struct FixedToken {
    token: Token,
}
impl FixedToken {
    pub fn token(&self) -> Token {
        self.token
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct VariableToken {
    token: Token,
}
impl VariableToken {
    pub fn token(&self) -> Token {
        self.token
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum TokenEnum {
    Fixed(FixedToken),
    Variable(VariableToken),
}


#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Token {
    pub id: TokenId,
}
impl Handle for Token {
    // id corresponds to the order the token comes in
    type Id = u32;
    type Context<'c> = ();

    fn id(&self, _: ()) -> u32 {
        self.id
    }

    fn from_id(id: u32, _: ()) -> Self {
        Self { id }
    }
}


pub struct TokenList {

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

}

impl TokenList {
    pub fn token_type(&self, token: Token) -> TokenType {
        self.token_types[token.usize_id(())]
    }

    pub fn is_fixed(&self, token: Token) -> bool {
        let token_type: TokenType = self.token_type(token);
        return self.token_type_list.is_fixed(token_type);
    }

    pub fn is_variable(&self, token: Token) -> bool {
        let token_type: TokenType = self.token_type(token);
        return self.token_type_list.is_variable(token_type);
    }

    // ---

    pub fn nbr_fixed_tokens(&self) -> TokenId {
        self.nbr_fixed_tokens
    }

    pub fn nbr_variable_tokens(&self) -> TokenId {
        self.nbr_variable_tokens
    }

    pub fn nbr_tokens(&self) -> TokenId {
        self.nbr_fixed_tokens() + self.nbr_variable_tokens()
    }

    pub fn nth_token(&self, n: TokenId) -> Token {
        Token { id: n }
    }

    pub fn token_enum(&self, token: Token) -> TokenEnum {
        let token_type: TokenType = self.token_type(token);
        match self.token_type_list.into_enum(token_type) {
            TokenTypeEnum::Fixed(_) => TokenEnum::Fixed(FixedToken{token}),
            TokenTypeEnum::Variable(_) => TokenEnum::Variable(VariableToken{token}),
        }
    }

    // ---

    pub fn fixed_token_size(&self, fixed_token: FixedToken) -> u8 {
        let fixed_token_type = self.token_type(fixed_token.token());
        let fixed_token_lexeme: &str = &self.token_type_list.fixed_token_lexemes[fixed_token_type.usize_id(())];
        u8::from_usize(fixed_token_lexeme.len())
    }

    pub fn variable_token_size(&self, variable_token: VariableToken) -> u8 {
        let actual_token: Token = variable_token.token();
        let start: usize = usize::try_from(self.lexeme_offsets[actual_token.usize_id(())]).unwrap();
        let end: usize = self
            .lexeme_offsets
            .get(actual_token.usize_id(()) + 1)
            .map_or(self.variable_lexemes.len(), |x| x.into_usize());
        return <u8 as Indexing>::from_usize(end - start);
    }

    pub fn token_size(&self, token: Token) -> u8 {
        match self.token_enum(token) {
            TokenEnum::Fixed(fixed_token) => self.fixed_token_size(fixed_token),
            TokenEnum::Variable(variable_token) => self.variable_token_size(variable_token),
        }
    }

    pub fn fixed_token_lexeme(&self, fixed_token: FixedToken) -> &str {
        let fixed_token_type: FixedTokenType = self.token_type_list.to_fixed(self.token_type(fixed_token.token));
        return self.token_type_list.fixed_token_lexeme(fixed_token_type);
    }

    pub fn variable_token_lexeme(&self, variable_token: VariableToken) -> &str {
        let actual_token: Token = variable_token.token;
        let start: usize = usize::try_from(self.lexeme_offsets[actual_token.usize_id(())]).unwrap();
        let end: usize = self
            .lexeme_offsets
            .get(actual_token.usize_id(()) + 1)
            .map_or(self.variable_lexemes.len(), |x| x.into_usize());

        return &self.variable_lexemes[start..end];
    }
    // ---

    pub fn to_position1d(&self, position2d: Position2D) -> Position1D {
        Position1D {
            pos: self.new_lines[position2d.line.into_usize()].pos + position2d.column + 1,
        }
    }

    pub fn to_position2d(&self, position1d: Position1D) -> Position2D {
        let new_line_index: Result<usize, usize> = self.new_lines.binary_search(&position1d);
        match new_line_index {
            Ok(valid_index) => Position2D {
                line: u32::from_usize(valid_index),
                column: position1d.pos - {
                    if valid_index == 0 {
                        0
                    } else {
                        self.new_lines[valid_index - 1].pos + 1
                    }
                },
            },
            Err(invalid_index) => Position2D {
                line: u32::from_usize(invalid_index),
                column: position1d.pos - {
                    if invalid_index == 0 {
                        0
                    } else {
                        self.new_lines[invalid_index - 1].pos + 1
                    }
                },
            },
        }
    }

    pub fn position1d(&self, token: Token) -> Position1D {
        self.token_positions[token.usize_id(())]
    }

    // usefull for real time analysis, maybe
    pub fn token_at2d(&self, position2d: Position2D) -> Option<Token> {
        self.token_at1d(self.to_position1d(position2d))
    }

    pub fn token_at1d(&self, position1d: Position1D) -> Option<Token> {
        let token_index: Result<usize, usize> = self.token_positions.binary_search(&position1d);

        match token_index {
            Ok(valid_index) => {
                // get the leftmost token that corresponds (empty tokens might be possible)
                let mut smaller_valid_index = valid_index;
                while (smaller_valid_index >= 1)
                    && (self.token_positions[smaller_valid_index - 1] == position1d)
                {
                    smaller_valid_index -= 1;
                }
                Some(self.nth_token(u32::from_usize(smaller_valid_index)))
            }
            Err(invalid_index) => {
                if invalid_index == 0 {
                    return None;
                }
                let previous_token_position: Position1D = self.token_positions[invalid_index - 1];
                let previous_token: Token =
                    self.nth_token(TokenId::from_usize(invalid_index - 1));

                if previous_token_position.pos + u32::from(self.token_size(previous_token))
                    >= position1d.pos
                {
                    Some(previous_token)
                } else {
                    None
                }
            }
        }
    }

    // ---

    pub fn iter_all_fixed_tokens(&self) -> impl Iterator<Item = (FixedToken, FixedTokenType)> {
        self.iter_all_tokens()
            .filter(|(_token, token_type)|
                self.token_type_list.is_fixed(*token_type))
            .map(|(token, token_type)| 
                (FixedToken {token}, self.token_type_list.to_fixed(token_type))   
            )
    }

    pub fn iter_all_variable_tokens(&self) -> impl Iterator<Item = (VariableToken, VariableTokenType)> {
        self.iter_all_tokens()
            .filter(|(_token, token_type)|
                self.token_type_list.is_variable(*token_type)
        )
            .map(|(token, token_type)| 
                (VariableToken {token}, self.token_type_list.to_variable(token_type))   
            )
    }

    pub fn all_token_types(&self) -> &[TokenType] {
        return &self.token_types;
    }

    pub fn iter_all_tokens(&self) -> impl Iterator<Item = (Token, TokenType)> {
        (0..self.nbr_tokens())
            .map(|token_id| Token { id: token_id })
            .map(|token| (token, self.token_type(token)))
    }

    pub fn iter_all_tokens_with_positions(
        &self,
    ) -> impl Iterator<Item = (Token, TokenType, Position1D, Position2D)> {
        (0..self.nbr_tokens())
            .map(|token_id| Token { id: token_id })
            .map(|token| (token, self.token_type(token)))
            .map(|(token, token_type)| {
                let position1d: Position1D = self.position1d(token);
                let position2d: Position2D = self.to_position2d(position1d);
                (token, token_type, position1d, position2d)
            })
    }


    /*wtf is this monstruosity
    fn all_tokens_with_lookahead<'se, const L: usize>(
        &'se self,
    ) -> impl Iterator<Item = [Option<(Token, TokenTypeEnum)>; L]> {
        struct LookaheadIterator<'se, const L: usize> {
            i: usize,
            current: [Option<(Token, TokenTypeEnum)>; L],
            token_list: &'se TokenList,
        }

        impl<'se, const L: usize> Iterator for LookaheadIterator<'se, L> {
            type Item = [Option<(Token, TokenTypeEnum)>; L];

            fn next(&mut self) -> Option<Self::Item> {
                for k in 0..(L - 1) {
                    self.current[k] = self.current[k + 1];
                }

                if self.current[0] == None {
                    return None;
                }

                self.i += 1;

                self.current[L - 1] = if TokenId::from_usize(self.i + L - 1)
                    == self.token_list.nbr_tokens()
                {
                    None
                } else {
                    let token_id: TokenId =
                        TokenId::from_usize(self.i + L - 1);
                    let token: Token = Token { id: token_id };
                    Some((token, self.token_list.token_type(token)))
                };

                return Some(self.current);
            }
        }

        let initial_current: [Option<(Token, TokenTypeEnum)>; L] = std::array::from_fn(|i: usize| {
            if TokenId::from_usize(i) == self.nbr_tokens() {
                None
            } else {
                let token_id: TokenId = TokenId::from_usize(i);
                let token: Token = Token { id: token_id };
                Some((token, self.token_type(token)))
            }
        });
        LookaheadIterator {
            i: 0,
            current: initial_current,
            token_list: &self,
        }
    }

    */
    // ---

}

// --------------------------------------------
