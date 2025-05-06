use std::cell::OnceCell;
use std::borrow::Cow;
use std::result;
use crate::datastructures::flat_table::{FlatTable, Indexing};

use num::iter::Range;
use thiserror::Error;
use derive_more::{Add, Sub, Mul, Div, AddAssign, SubAssign, MulAssign, DivAssign};
use delegate::delegate;

use crate::datastructures::bitset::BitSet;
use crate::UINT;


// --------------------------------------------

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
        let result: Result<usize, usize> = self.chars.binary_search(&c);
        match result {
            Ok(index) => Some(index),
            Err(_) => None,
        }
    }

    pub fn size(&self) -> usize {
        self.chars.len()
    }
}


// these values are totally arbitrary
// here, they are both just enough and largely enough 
// to handle the grammar of general-purpose programming language 

// to store the symbol efficiently
// meaning that there can (only) be 2^16 = 65578 symbols for one cfg
// SymbolIdx -> u16;


// to store a value that represents a CfgRule but in a contiguous way
// meaning that there can (only) be 2^16 = 65578 rules in total
// CfgRuleIdx -> u16;


const EXPECTED_RULE_SIZE: usize = 10;

#[derive(Add, Sub, Mul, Div, AddAssign, SubAssign, MulAssign, DivAssign, PartialEq, Debug, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct SymbolIdx(pub u16);

impl From<usize> for SymbolIdx {
    fn from(value: usize) -> Self {
        Self(u16::try_from(value).expect("ids of symbols must be storable in the u16 type") )
    }
}

impl From<SymbolIdx> for usize {
    fn from(value: SymbolIdx) -> Self {
        value.0 as usize
    }
}

impl Indexing for SymbolIdx {}


#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Symbol {
    pub id: SymbolIdx,
}

// impl Symbol {
//     pub fn from_local(local_symbol: LocalSymbol, offset: SymbolIdx) -> Symbol {
//         Symbol { id: local_symbol.id + offset }
//     }
// }



#[derive(PartialEq, Clone, Copy, Debug)]
pub struct OptionSymbol {
    pub id: SymbolIdx,
}

impl OptionSymbol {
    pub fn is_none(&self) -> bool {
        self.id==SymbolIdx(0)
    }

    pub fn none(&self) -> SymbolIdx {
        SymbolIdx(0)
    }
}

impl TryFrom<OptionSymbol> for Symbol {

    type Error = ();

    fn try_from(value: OptionSymbol) -> Result<Self, Self::Error> {
        if value.is_none() {
            return Err(());
        }
        else {
            return Ok(Symbol { id: value.id - SymbolIdx(1)});
        }
    }
}

impl From<Symbol> for OptionSymbol {

    fn from(value: Symbol) -> OptionSymbol {
        OptionSymbol { id: value.id + SymbolIdx(1) }
    }
}



// #[derive(PartialEq, Clone, Copy, Debug)]
// pub struct LocalSymbol {
//     pub id: SymbolIdx,
// }

// impl LocalSymbol {
//     pub fn from_non_local(non_local_symbol: Symbol, offset: SymbolIdx) -> LocalSymbol {
//         LocalSymbol { id: non_local_symbol.id - offset }
//     }
// }

// --------------------------------------------

#[derive(Debug)]
pub struct Token {
    pub token_type: Symbol,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn next_position(&self) -> (usize, usize) {
        // returns the line and column of the token that will come after
        let mut line = self.line;
        let mut column: usize = self.column;
        for c in self.lexeme.chars() {
            if c=='\n' {
                line += 1;
                column = 0;
            }
            else {
                column += 1;
            }
        }

        (line, column)
    }
}

// --------------------------------------------

pub struct SymbolSet {
    // acts like a container of the symbols from 0 to size(not included)
    representations: Vec<String>,
}

impl SymbolSet {

    pub fn new(representations: Vec<String>) -> Self {
        Self { representations }
    }

    pub fn size(&self) -> SymbolIdx {
        self.representations.len().try_into().unwrap()
    }

    pub fn get_representation(&self, local_id: SymbolIdx) -> &String {
        &self.representations[usize::from(local_id)]
    }

    /// debug only
    pub fn get_id(&self, representation: &str) -> Option<SymbolIdx> {
        self.representations
            .iter()
            .position(|s| s==representation)
            .map(|x| SymbolIdx::try_from(x).unwrap())

    }
}


// --------------------------------------------


#[derive(Error, Debug)]
pub enum CfgError {
    #[error("CfgRule  {rule:?}, has an invalid origin.")]
    InvalidRuleOrigin{rule: CfgRule},

    #[error("CfgRule  {rule:?}, has an invalid replacement symbol {symbol:?}.")]
    InvalidRuleReplacement{rule: CfgRule, symbol: Symbol},
}

// --------------------------------------------

pub struct CfgSymbolSet {
    terminals: SymbolSet,
    non_terminals: SymbolSet,
}

impl CfgSymbolSet {

    pub fn new(terminals: SymbolSet, non_terminals: SymbolSet) -> CfgSymbolSet {
        CfgSymbolSet {
            terminals,
            non_terminals,
        }
    }

    pub fn get_terminals(&self) -> &SymbolSet {
        &self.terminals
    }

    pub fn get_non_terminals(&self) -> &SymbolSet {
        &self.non_terminals
    }

    pub fn START(&self) -> Symbol {
        Symbol { id: SymbolIdx(1) }
    }

    pub fn END(&self) -> Symbol {
        Symbol { id: SymbolIdx(self.nbr_non_terminals().0 + 1) }
    }

    pub fn ERR_NON_TERM(&self) -> Symbol{
        Symbol { id: SymbolIdx(0) }
    }

    pub fn ERR_TERM(&self) -> Symbol{
        Symbol { id: self.nbr_non_terminals() }
    }


    pub fn nbr_non_terminals(&self) -> SymbolIdx {
        self.get_non_terminals().size() + SymbolIdx(2)
    }

    pub fn nbr_terminals(&self) -> SymbolIdx {
        self.get_terminals().size() + SymbolIdx(2)
    }

    pub fn nbr_symbols(&self) -> SymbolIdx {
        self.nbr_non_terminals() + self.nbr_terminals()
    }

    pub fn all_non_terminals(&self) -> impl Iterator<Item = Symbol> {
        (0..self.nbr_non_terminals().0).map(|value| Symbol {id: SymbolIdx(value)})
    }

    pub fn all_terminals(&self) -> impl Iterator<Item = Symbol> {
        (self.nbr_non_terminals().0..self.nbr_symbols().0).map(|value| Symbol {id: SymbolIdx(value)})
    }

    pub fn all_symbols(&self) -> impl Iterator<Item = Symbol> {
        (0..self.nbr_terminals().0 + self.nbr_non_terminals().0).map(|value| Symbol {id: SymbolIdx(value)})
    }

    pub fn is_terminal(&self, symbol: Symbol) -> bool {
        return symbol.id >= self.nbr_non_terminals()
    }

    pub fn is_non_terminal(&self, symbol: Symbol) -> bool {
        return symbol.id < self.nbr_non_terminals()
    }
    
    pub fn is_special(&self, symbol: Symbol) -> bool {
        return symbol == self.START() 
        || symbol == self.END()
        || symbol == self.ERR_NON_TERM()
        || symbol == self.ERR_TERM();
    }


    /// If the symbol is a non-terminal or a terminal symbol, returns the 
    /// corresponding index in the corresponding SymbolSet
    /// Returns None if the symbol is a special symbol
    pub fn to_local_non_special(&self, symbol: Symbol) -> Option<SymbolIdx> {
        if self.is_special(symbol) {
            return None;
        }

        if self.is_non_terminal(symbol) {
            return Some(symbol.id - SymbolIdx(2));
        }
        else {
            return Some(symbol.id - self.nbr_non_terminals() - SymbolIdx(2));
        }
    }

    /// If the symbol is a non-terminal or a terminal symbol, returns the 
    /// corresponding index in the corresponding set of symbols, where special symbols are included
    pub fn to_local(&self, symbol: Symbol) -> SymbolIdx {

        if self.is_non_terminal(symbol) {
            return symbol.id;
        }
        else {
            return symbol.id - self.nbr_non_terminals();
        }
    }

    /// If the symbol is a non-terminal or a terminal symbol, returns the 
    /// corresponding index in the corresponding set of symbols, where special symbols are included
    /// as well as NOSYMBOL
    pub fn to_local_nosymbol(&self, symbol: Symbol) -> SymbolIdx {

        if self.is_non_terminal(symbol) {
            return symbol.id;
        }
        else {
            return symbol.id - self.nbr_non_terminals();
        }
    }



    pub fn repr_symbol(&self, symbol: Symbol) -> &str {
        
        if symbol == self.START() {
            return "START";
        }
        if symbol == self.END() {
            return "END";
        }
        if symbol == self.ERR_NON_TERM() {
            return "ERR_NON_TERM";
        }
        if symbol == self.ERR_TERM() {
            return "ERR_TERM";
        }

        if self.is_non_terminal(symbol) {
            // unwrap is safe because we know that this isn't a special symbol
            self.get_non_terminals().get_representation(self.to_local_non_special(symbol).unwrap())
        }
        else {
            // unwrap is safe because we know that this isn't a special symbol
            self.get_terminals().get_representation(self.to_local_non_special(symbol).unwrap())
        }
        
        
    }

    // debug only
    pub fn get_symbol_by_representation(&self, representation: &str) -> Symbol {

        match representation {
            "START" => self.START(),
            "END" => self.END(),
            "ERR_TERM" => self.ERR_TERM(),
            "ERR_NON_TERM" => self.ERR_NON_TERM(),
            other => {
                Symbol {id: 
                    match self.get_non_terminals().get_id(other) {
                        Some(non_terminal_id) => non_terminal_id + SymbolIdx(2),
                        None => self.get_terminals().get_id(other).unwrap() + self.nbr_non_terminals() + SymbolIdx(2),
                    }
                }
            }
        }
    }
}

// --------------------------------------------

#[derive(Debug, Clone)]
pub struct CfgRule {
    pub origin: Symbol,
    pub replacement: Vec<Symbol>,
}

impl CfgRule {
    /// returns true iff the replacement is empty
    pub fn is_empty(&self) -> bool {
        self.replacement.len()==0
    }

    pub fn replacement_size(&self) -> usize {
        self.replacement.len()
    }
}


#[derive(Add, Sub, Mul, Div, AddAssign, SubAssign, MulAssign, DivAssign, PartialEq, Debug, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct CfgRuleIdx(pub u16);

impl From<usize> for CfgRuleIdx {
    fn from(value: usize) -> Self {
        Self(u16::try_from(value).expect("ids of rules must be storable in the u16 type") )
    }
}

impl From<CfgRuleIdx> for usize {
    fn from(value: CfgRuleIdx) -> Self {
        value.0 as usize
    }
}

impl Indexing for CfgRuleIdx {}



#[derive(Add, Sub, Mul, Div, PartialEq, Debug, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct CfgRuleBySymbolIdx(pub u16);

impl From<usize> for CfgRuleBySymbolIdx {
    fn from(value: usize) -> Self {
        Self(u16::try_from(value).expect("ids of symbols must be storable in the u16 type") )
    }
}

impl From<CfgRuleBySymbolIdx> for usize {
    fn from(value: CfgRuleBySymbolIdx) -> Self {
        value.0 as usize
    }
}

impl Indexing for CfgRuleBySymbolIdx {}


// --------------------------------------------

/// augmented context-free grammar
pub struct Cfg {
    symbol_set: CfgSymbolSet,
    rules: FlatTable<CfgRule, SymbolIdx, CfgRuleBySymbolIdx, CfgRuleIdx>,

    // -------------- cached values

    // indexed by produced symbols
    rules_producing_each_symbol: OnceCell<Vec<BitSet<UINT>>>,

    // indexed by symbols
    are_symbols_nullable: OnceCell<BitSet<UINT>>,

    // indexed by non-terminal symbols, bitset by terminal-symbols
    first_sets: OnceCell<Vec<BitSet<UINT>>>,

    /*
    get_NTsymbols_implied_by_rule
    get_NTsymbols_implied_by_symbol
    get_rules_indirectly_producing
    get_terminating_symbols
    is_word_nullable
    is_rule_nullable
    compute_
    compute_follow_sets
    compute_predict_sets
     */
}

impl  Cfg {

    // TODO: augment the grammar
    pub fn new(symbol_set: CfgSymbolSet, mut rules: Vec<CfgRule>) -> Result<Cfg, CfgError> {
        // sorts the rules to match the order of the non_terminals in the symbol_set

        let nbr_non_terminals: SymbolIdx = symbol_set.nbr_non_terminals();
        let nbr_terminals: SymbolIdx = symbol_set.nbr_terminals();

        rules.sort_by(|rule1, rule2| rule1.origin.id.cmp(&rule2.origin.id));

        // checks that each rule is valid
        for (rule_id, rule) in rules.iter().enumerate() {
            if rule.origin.id >= nbr_non_terminals {
                return Err(CfgError::InvalidRuleOrigin { rule: rule.clone() });
            }

            for replacement_symbol in &rule.replacement {
                if replacement_symbol.id >= nbr_non_terminals + nbr_terminals {
                    return Err(CfgError::InvalidRuleReplacement { rule: rule.clone(), symbol: *replacement_symbol });
                }
            }
        }

        let mut rule_origin_correspondance: Vec<CfgRuleIdx> = Vec::with_capacity(usize::from(nbr_non_terminals));
        let mut current_rule_flat_id: CfgRuleIdx = CfgRuleIdx(0);
        for non_terminal_id_value in 0..nbr_non_terminals.0 {
            rule_origin_correspondance.push(current_rule_flat_id);
            let mut i = 0;
            while ((current_rule_flat_id.0 + i) as usize != rules.len()) 
                && (rules[(current_rule_flat_id.0 + i) as usize].origin.id.0 == non_terminal_id_value) {
                i += 1;
            }
            current_rule_flat_id.0 += i;
        }

        Ok(Cfg {
            symbol_set,
            rules: FlatTable::new(rules, rule_origin_correspondance),

            rules_producing_each_symbol: OnceCell::new(),

            are_symbols_nullable: OnceCell::new(),

            first_sets: OnceCell::new(),

        })
    }

    // -------------------------- utility methods

    pub fn symbol_set(&self) -> &CfgSymbolSet {
        &self.symbol_set
    }

    delegate! {
        to self.symbol_set {
            pub fn START(&self) -> Symbol;
            pub fn END(&self) -> Symbol;
            pub fn ERR_NON_TERM(&self) -> Symbol;
            pub fn ERR_TERM(&self) -> Symbol;
            pub fn nbr_non_terminals(&self) -> SymbolIdx;
            pub fn nbr_terminals(&self) -> SymbolIdx;
            pub fn nbr_symbols(&self) -> SymbolIdx;
            pub fn all_non_terminals(&self) -> impl Iterator<Item = Symbol>;
            pub fn all_terminals(&self) -> impl Iterator<Item = Symbol>;
            pub fn all_symbols(&self) -> impl Iterator<Item = Symbol>;
            pub fn is_terminal(&self, symbol: Symbol) -> bool;
            pub fn is_non_terminal(&self, symbol: Symbol) -> bool;
            pub fn is_special(&self, symbol: Symbol) -> bool;
            pub fn to_local_non_special(&self, symbol: Symbol) -> Option<SymbolIdx>;
            pub fn to_local(&self, symbol: Symbol) -> SymbolIdx;
            pub fn repr_symbol(&self, symbol: Symbol) -> &str;
        }
    }

    /// returns the number of terminals, non_terminals, distinct non_terminals
    pub fn count_symbols_in_rule(&self, rule: &CfgRule) -> (usize, usize, SymbolIdx) {
        let (mut term, mut non_term, mut dist_non_term): (usize, usize, SymbolIdx) = (0, 0, SymbolIdx(0));

        let mut distinct_non_terminals_found: Vec<Symbol> = Vec::with_capacity(EXPECTED_RULE_SIZE);

        for &symbol in &rule.replacement {
            if self.is_terminal(symbol) {
                term += 1;
            }
            else {
                non_term += 1;
                if !distinct_non_terminals_found.contains(&symbol) {
                    dist_non_term.0 += 1;
                    distinct_non_terminals_found.push(symbol);
                }
            }
        }

        (term, non_term, dist_non_term)
    }

    // --------

    pub fn repr_rule(&self, rule_id: CfgRuleIdx) -> String {
        let mut s: String = String::new();

        let rule = self.get_rule_by_id(rule_id);
        s.push_str(self.repr_symbol(rule.origin));
        s.push_str(" --> ");

        for &replacement_symbol in &rule.replacement {
            s.push_str(self.repr_symbol(replacement_symbol));
            s.push(' ');
        }
        s.pop();
        s
    }

    pub fn nbr_rules(&self) -> CfgRuleIdx {
        return self.rules.size();
    }

    pub fn all_rules(&self) -> impl Iterator<Item = (CfgRuleIdx, &CfgRule)> {
        self.rules
            .table
            .iter()
            .enumerate()
            .map(|(id, rule)| (CfgRuleIdx::from(id), rule))
    }

    pub fn get_rule_by_id(&self, id: CfgRuleIdx) -> &CfgRule {
        // no check: we assume that every CfgRuleId constructed is valid
        self.rules.get_by_id(id)
    }

    pub fn get_rules_by_origin(&self, origin: Symbol) -> impl Iterator<Item = (CfgRuleIdx, &CfgRule)> {
        let rule_id: CfgRuleIdx = self.rules.rows[usize::from(origin.id)];
        (&self.rules[origin.id])
            .iter()
            .enumerate()
            .map(move |(id, rule)| (CfgRuleIdx::from(id) + rule_id, rule))
    }
    
    /// returns an iterator which go through each (rule_id, rule) of the grammar that can produce produced_symbol
    pub fn get_rules_producing(&self, produced_symbol: Symbol) -> impl Iterator<Item = (CfgRuleIdx, &CfgRule)> {
        
        let rules_producing_each_symbol: &Vec<BitSet<UINT>> = 
            self.rules_producing_each_symbol.get_or_init(|| self.compute_rules_producing_each_symbol());

        (&rules_producing_each_symbol[usize::from(produced_symbol.id)])
            .iter()
            .map(|rule_id| (CfgRuleIdx::from(rule_id), self.get_rule_by_id(CfgRuleIdx::from(rule_id))))
    }

    fn compute_rules_producing_each_symbol(&self) -> Vec<BitSet<UINT>> {
        // for each symbol, gives a list of the rules whose replacement contains the symbol

        // indexed by symbols produced
        let mut rules_producing_each_symbol: Vec<BitSet<UINT>> = 
        vec![BitSet::new_filled(false, usize::from(self.nbr_rules())); usize::from(self.nbr_symbols())];
        
        for (rule_id, rule) in self.all_rules() {
            for &replacement_symbol in &rule.replacement {
                rules_producing_each_symbol[usize::from(replacement_symbol.id)].insert(usize::from(rule_id));
            }
        }
        rules_producing_each_symbol
    }

    // -------------------------- nullable symbols/words/rules

    pub fn is_symbol_nullable(&self, symbol: Symbol) -> bool {
        if self.is_terminal(symbol) {
            false
        }
        else {
            self.are_symbols_nullable
                .get_or_init(|| self.compute_are_symbols_nullable())
                .contains(usize::from(symbol.id))
        }
    }

    pub fn is_word_nullable(&self, word: &[Symbol]) -> bool {
        word.iter().all(|&symbol| self.is_symbol_nullable(symbol))
    }

    pub fn is_rule_nullable(&self, rule_id: CfgRuleIdx) -> bool {
        self.get_rule_by_id(rule_id).replacement.iter().all(|&symbol| self.is_symbol_nullable(symbol))
    }

    fn compute_are_symbols_nullable(&self) -> BitSet<UINT> {
        // adapted from here:
        // https://cstheory.stackexchange.com/questions/2479/quickly-finding-empty-string-producing-nonterminals-in-a-cfg


        // initially, all symbols are considered non-nullable
        let mut are_nullable: BitSet<UINT> = BitSet::new_filled(false, usize::from(self.nbr_non_terminals()));

        // number of distinct non_terminals marked as non-nullable that the index rule can produce
        // will be initialised later
        let mut nbr_nullable: Vec<SymbolIdx> = vec![SymbolIdx(0); usize::from(self.nbr_rules())];


        // stack of non_terminals that have been marked nullable but not yet processed
        let mut unprocessed_nullable_symbols: Vec<Symbol> = Vec::new(); 

        // initialize are_nullable and nbr_nullable and unprocessed_nullable_symbols
        for (rule_id, rule) in self.all_rules() {
            if rule.is_empty(){
                if !are_nullable.contains(usize::from(rule.origin.id)) {
                    unprocessed_nullable_symbols.push(rule.origin);
                }
                are_nullable.insert(usize::from(rule.origin.id));
                nbr_nullable[usize::from(rule_id)] = SymbolIdx(0);
            }
            else {
                let (term, _, dist_non_term) = self.count_symbols_in_rule(rule);
                // if all the symbols in rule.replacement are NTsymbols, it could be nullable
                // In the opposite case, we won't even consider the rule because we know it is not nullable
                if term == 0 {
                    nbr_nullable[usize::from(rule_id)] = dist_non_term;
                }
            }
        }

        while unprocessed_nullable_symbols.len() > 0 {
            let unprocessed_nullable_symbol: Symbol = unprocessed_nullable_symbols.pop().unwrap();
            for (rule_id, rule) in self.get_rules_producing(unprocessed_nullable_symbol) {

                if self.count_symbols_in_rule(rule).0 != 0 {continue;}  // discard rules containing terminal symbols

                nbr_nullable[usize::from(rule_id)] -= SymbolIdx(1);

                if nbr_nullable[usize::from(rule_id)] == SymbolIdx(0) && !are_nullable.contains(usize::from(rule.origin.id)) {
                    are_nullable.insert(usize::from(rule.origin.id));
                    unprocessed_nullable_symbols.push(rule.origin);
                }
            }
        }

        are_nullable
    }

    // -------------------------- first sets

    fn get_first_set(&self, symbol: Symbol) -> Cow<BitSet<UINT>> {
        if self.is_terminal(symbol) {
            let mut first_set: BitSet<UINT> = BitSet::new_filled(false, usize::from(self.nbr_terminals())+1);
            // set containing only the terminal symbol itself
            first_set.insert(usize::from(OptionSymbol::from(symbol).id));
            
            Cow::Owned(first_set)
        }
        else {
            Cow::Borrowed(
            &self.first_sets
                .get_or_init(|| self.compute_first_sets())[usize::from(self.to_local(symbol))]
            )
        }
    }

    fn compute_first_sets(&self) -> Vec<BitSet<UINT>> {

        // first_sets: dict[Symbol, set[Symbol | NOSYMBOL]] = {symbol: set() for symbol in self.NTsymbols}


        // for each non-terminal symbol, contains a bitset of optional terminal symbols
        // the terminal symbol case is trivial so not computed
        let first_sets: Vec<BitSet<UINT>> = 
        vec![BitSet::new_filled(false, usize::from(self.nbr_terminals())+1); usize::from(self.nbr_non_terminals())];

        // for each non-terminal symbol, maps to the set of non-terminal symbols that rely on it (inculde it)
        let inclusions: Vec<BitSet<UINT>> = 
        vec![BitSet::new_filled(false, usize::from(self.nbr_non_terminals())); usize::from(self.nbr_non_terminals())];
        // inclusions[a] = {b, c, d} <=> first(a) included in first(b), first(c), and first(d)

        // for each terminal symbol, maps to the set of non-terminal symbols that relies on it (inculde it)
        let terminal_inclusions: Vec<BitSet<UINT>> = 
        vec![BitSet::new_filled(false, usize::from(self.nbr_non_terminals())); usize::from(self.nbr_terminals())];
        // terminal_inclusions[a] = [b, c, d] <=> a included in first(b), first(c), and first(d)

        // initialize results that include firsts
        for (rule_id, rule) in self.all_rules() {
            for symbol in rule.replacement {
                if self.is_terminal(symbol) {
                    // the Tsymbol is included in the first set of the origin of the rule
                    terminal_inclusions[usize::from(self.to_local(symbol))].insert(
                        usize::from(self.to_local(rule.origin)));
                    break;
                }

                else:
                    # the first set of the NTsymbol is included in the first set of the origin of the rule
                    inclusions[symbol].append(rule.origin)

                    if not self.is_symbol_nullable(symbol):  # we stay in the loop only if this NTsymbol is nullable
                        break
            }
        }
        # propagation of each Tsymbol
        for Tsymbol in self.Tsymbols:
            NTsymbols_to_process: list[Symbol] = terminal_inclusions[Tsymbol].copy()
            NTsymbols_processed: set[Symbol] = set()

            while len(NTsymbols_to_process)>0:
                NTsymbol_to_process = NTsymbols_to_process.pop()

                if NTsymbol_to_process in NTsymbols_processed:
                    continue

                first_sets[NTsymbol_to_process].add(Tsymbol)

                for new_NTsymbol_to_process in inclusions[NTsymbol_to_process]:
                    if not new_NTsymbol_to_process in NTsymbols_processed:
                        NTsymbols_to_process.append(new_NTsymbol_to_process)

                NTsymbols_processed.add(NTsymbol_to_process)

        for NTsymbol in self.NTsymbols:
            if self.is_symbol_nullable(NTsymbol):
                first_sets[NTsymbol].add(NoSymbol)

        self.first_sets = first_sets
    }



}



// --------------------------------------------


    // self.rules_indirectly_producing: dict[Symbol, list[CFRule]] | None = None  #
/*
    nullable_symbols: HashSet<&'grammar Symbol>,
    
    self.augmented: bool|None = None

    self.max_replacement_length: int | None = None  #

    self.NTsymbols_implied_by_rule: dict[CFRule, set[Symbol]] | None = None  #
    self.NTsymbols_implied_by_symbol: dict[Symbol, set[Symbol]] | None = None  #

    self.nullable_symbols: dict[Symbol, bool] | None = None  #

    self.rules_directly_producing: dict[Symbol, list[CFRule]] | None = None  #
    self.rules_indirectly_producing: dict[Symbol, list[CFRule]] | None = None  #

    self.terminating_symbols: dict[Symbol, bool] | None = None  #

    self.smallest_word_indirectly_produced: dict[Symbol, int] | None = None

    self.first_sets: dict[Symbol, set[Symbol]] | None = None    #
    self.follow_sets: dict[Symbol, set[Symbol]] | None = None   #
    self.predict_sets: dict[CFRule: set[Symbol]] | None = None  #

    self.first_k_sets: dict[int, dict[Symbol, set[TupleSymbolicWord]]] = {}
    self.follow_k_sets: dict[int, dict[Symbol, set[TupleSymbolicWord]]] = {}
    self.predict_k_sets: dict[int, dict[CFRule: set[TupleSymbolicWord]]] = {}
     */

