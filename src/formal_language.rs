use std::cell::OnceCell;
use std::borrow::Cow;
use std::marker::PhantomData;
use std::result;
use crate::datastructures::flat_table::{FlatTable, Indexing};

use thiserror::Error;
use derive_more::{Add, Sub, Mul, Div, AddAssign, SubAssign, MulAssign, DivAssign};
use delegate::delegate;

use crate::datastructures::bitset::BitSet;
use crate::UINT;
use crate::Idx;


// --------------------------------------------

// External represents common data for a set of values, so that it is useless to store it inside the value
pub trait Enumerable<Id, External> {
    fn id(&self, external: External) -> Id;
    fn from_id(id: Idx, external: External) -> Self;
}

impl <External, T: Enumerable<Idx, External>> Enumerable<Idx, External> for Option<T> {
    fn id(&self, external: External) -> Idx {
        match self {
            None => Idx(0),
            Some(t) => t.id(external) + Idx(1),
        }
    }
    fn from_id(id: Idx, external: External) -> Self {
        match id {
            Idx(0) => Self::None,
            Idx(_) => Self::Some(T::from_id(id - Idx(1), external)),
        }
    }
}

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
        self.chars.binary_search(&c).ok()
    }

    pub fn size(&self) -> usize {
        self.chars.len()
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct NonTerm {
    pub id: Idx,
}
impl Enumerable<Idx, ()> for NonTerm {
    fn id(&self, _: ()) -> Idx {
        self.id
    }
    fn from_id(id: Idx, _: ()) -> Self {
        return NonTerm {id};
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Term {
    pub id: Idx,
}
impl Enumerable<Idx, ()> for Term {
    fn id(&self, _: ()) -> Idx {
        self.id
    }
    fn from_id(id: Idx, _: ()) -> Self {
        return Term {id};
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Symbol {
    Term(Term),
    NonTerm(NonTerm),
}
impl Enumerable<Idx, Idx> for Symbol {
    fn id(&self, nbr_non_terms: Idx) -> Idx {
        match self {
            Symbol::NonTerm(non_term) => non_term.id(()),
            Symbol::Term( term ) => term.id(()) + nbr_non_terms,
        }
    }
    fn from_id(id: Idx, nbr_non_terms: Idx) -> Self {
        if id < nbr_non_terms {
            Symbol::NonTerm(NonTerm { id })
        }
        else {
            Symbol::Term(Term { id: id - nbr_non_terms })
        }
    }
}


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


#[derive(Error, Debug)]
pub enum CfgError {
    #[error("CfgRule  {rule:?}, has an invalid origin.")]
    InvalidRuleOrigin{rule: CfgRule},

    #[error("CfgRule  {rule:?}, has an invalid replacement symbol {symbol:?}.")]
    InvalidRuleReplacement{rule: CfgRule, symbol: Symbol},
}

// --------------------------------------------

pub struct CfgSymbolSet {
    non_terms: Vec<String>,
    special_non_terms: Vec<String>,
    terms: Vec<String>,
    special_terms: Vec<String>,
    others: Vec<String>,
}

impl CfgSymbolSet {

    const NULL_SYMBOL: &str = "None";

    // special symbols shouldn't be included 
    pub fn new(terms: Vec<String>, non_terms: Vec<String>) -> CfgSymbolSet {
        CfgSymbolSet {
            non_terms: non_terms,
            special_non_terms: vec![String::from("START"), String::from("ERR_NON_TERM")],
            terms: terms,
            special_terms: vec![String::from("END"), String::from("ERR_TERM")],
            others: vec![],
        }
    }

    // -----

    pub fn ERR_NON_TERM(&self) -> NonTerm{
        NonTerm { id: Idx(0) }
    }

    pub fn START(&self) -> NonTerm {
        NonTerm { id: Idx(1) }
    }

    pub fn ERR_TERM(&self) -> Term{
        Term { id: Idx(0) }
    }

    pub fn END(&self) -> Term {
        Term { id: Idx(1) }
    }

    // -----

    // wrappers to hide the ugly self.nbr_non_terminals()
    pub fn symbol_id(&self, symbol: Symbol) -> Idx {
        symbol.id(self.nbr_non_terminals())
    }

    pub fn symbol_from_id(&self, id: Idx) -> Symbol {
        Symbol::from_id(id, self.nbr_non_terminals())
    }

    // -----

    pub fn nbr_non_terminals(&self) -> Idx {
        Idx::from(self.non_terms.len() + self.special_non_terms.len())
    }

    pub fn nbr_terminals(&self) -> Idx {
        Idx::from(self.terms.len() + self.special_terms.len())
    }

    pub fn nbr_symbols(&self) -> Idx {
        self.nbr_non_terminals() + self.nbr_terminals() + Idx::from(self.others.len())
    }

    // -----

    pub fn all_non_terminals(&self) -> impl Iterator<Item = NonTerm> {
        (0..self.nbr_non_terminals().0).map(|value| NonTerm {id: Idx(value)})
    }

    pub fn all_terminals(&self) -> impl Iterator<Item = Term> {
        (0..self.nbr_terminals().0).map(|value| Term {id: Idx(value)})
    }

    pub fn all_symbols(&self) -> impl Iterator<Item = Symbol> {
        (self.all_non_terminals()
            .map(|non_term| Symbol::NonTerm(non_term)))
        .chain
        (self.all_terminals()
            .map(|term| Symbol::Term(term)))
    }

    // -----

    pub fn is_non_terminal(&self, symbol: Symbol) -> bool {
        match symbol {
            Symbol::NonTerm(_) => true,
            Symbol::Term(_) => false,
        }
    }

    pub fn is_terminal(&self, symbol: Symbol) -> bool {
        match symbol {
            Symbol::NonTerm(_) => false,
            Symbol::Term(_) => true,
        }
    }

    
    pub fn is_special(&self, symbol: Symbol) -> bool {
        return symbol == Symbol::NonTerm(self.START()) 
        || symbol == Symbol::NonTerm(self.ERR_NON_TERM())
        || symbol == Symbol::Term(self.END())
        || symbol == Symbol::Term(self.ERR_TERM())
    }

    // --------

    pub fn repr_non_term(&self, non_term: NonTerm) -> &str {
        if self.is_special(Symbol::NonTerm(non_term)) {
            return &self.special_non_terms[usize::from(non_term.id(()))];
        }
        else {
            return &self.non_terms[usize::from(non_term.id(())) - self.special_non_terms.len()];
        }
    }

    pub fn repr_term(&self, term: Term) -> &str {
        if self.is_special(Symbol::Term(term)) {
            return &self.special_terms[usize::from(term.id(()))];
        }
        else {
            return &self.terms[usize::from(term.id(())) - self.special_terms.len()];
        }
    }

    pub fn repr_symbol(&self, symbol: Symbol) -> &str {
        
        match symbol {
            Symbol::NonTerm(non_term) => {
                self.repr_non_term(non_term)
            }
            Symbol::Term(term) => {
                self.repr_term(term)
            }
        }
        
    }

    pub fn repr_opt_non_term(&self, opt_non_term: Option<NonTerm>) -> &str {
        match opt_non_term {
            None => CfgSymbolSet::NULL_SYMBOL,
            Some(non_term) => self.repr_non_term(non_term),
        }
    }

    pub fn repr_opt_term(&self, opt_term: Option<Term>) -> &str {
        match opt_term {
            None => CfgSymbolSet::NULL_SYMBOL,
            Some(term) => self.repr_term(term),
        }
    }

    pub fn repr_opt_symbol(&self, opt_symbol: Option<Symbol>) -> &str {
        match opt_symbol {
            None => CfgSymbolSet::NULL_SYMBOL,
            Some(symbol) => self.repr_symbol(symbol),
        }
    }
    // --------

    pub fn get_non_term_by_repr(&self, repr: &str) -> Option<NonTerm> {
        let special_non_term_index: Option<usize> = self.special_non_terms.iter().position(|element| element==repr);
        if let Some(special_non_term_index) = special_non_term_index {
            return Some(NonTerm { id: Idx::from(special_non_term_index) });
        }
        let non_term_index: Option<usize> = self.non_terms.iter().position(|element| element==repr);
        if let Some(non_term_index) = non_term_index {
            return Some(NonTerm { id: Idx::from(non_term_index + self.special_non_terms.len()) });
        }
        else {
            return None;
        }
    }

    pub fn get_term_by_repr(&self, repr: &str) -> Option<Term> {
        let special_term_index: Option<usize> = self.special_terms.iter().position(|element| element==repr);
        if let Some(special_term_index) = special_term_index {
            return Some(Term { id: Idx::from(special_term_index) });
        }
        let term_index: Option<usize> = self.terms.iter().position(|element| element==repr);
        if let Some(term_index) = term_index {
            return Some(Term { id: Idx::from(term_index + self.special_terms.len()) });
        }
        else {
            return None;
        }
    }

    pub fn get_symbol_by_representation(&self, repr: &str) -> Option<Symbol> {

        let non_term: Option<NonTerm> = self.get_non_term_by_repr(repr);
        if let Some(non_term) = non_term {
            return Some(Symbol::NonTerm(non_term));
        }
        let term: Option<Term> = self.get_term_by_repr(repr);
        if let Some(term) = term {
            return Some(Symbol::Term(term));
        }
        else {
            return None;
        }
    }

}

// --------------------------------------------

#[derive(Debug, Clone)]
pub struct CfgRule {
    pub origin: NonTerm,
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

pub struct CfgRuleId(Idx);

// --------------------------------------------

/// augmented context-free grammar
pub struct Cfg {
    symbol_set: CfgSymbolSet,
    rules: FlatTable<CfgRule, Idx>,

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

        rules.sort_by(|rule1, rule2| rule1.origin.id(()).cmp(&rule2.origin.id));

        let mut rule_origin_correspondance: Vec<Idx> = Vec::with_capacity(usize::from(symbol_set.nbr_non_terminals()));
        let mut current_rule_flat_id: Idx = Idx(0);
        for non_terminal_id_value in 0..symbol_set.nbr_non_terminals().0 {
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

    delegate! {
        to self.symbol_set {
            pub fn START(&self) -> NonTerm;
            pub fn END(&self) -> Term;
            pub fn ERR_NON_TERM(&self) -> NonTerm;
            pub fn ERR_TERM(&self) -> Term;
            pub fn symbol_id(&self, symbol: Symbol) -> Idx;
            pub fn symbol_from_id(&self, id: Idx) -> Symbol;
            pub fn nbr_non_terminals(&self) -> Idx;
            pub fn nbr_terminals(&self) -> Idx;
            pub fn nbr_symbols(&self) -> Idx;
            pub fn all_non_terminals(&self) -> impl Iterator<Item = NonTerm>;
            pub fn all_terminals(&self) -> impl Iterator<Item = Term>;
            pub fn all_symbols(&self) -> impl Iterator<Item = Symbol>;
            pub fn is_terminal(&self, symbol: Symbol) -> bool;
            pub fn is_non_terminal(&self, symbol: Symbol) -> bool;
            pub fn is_special(&self, symbol: Symbol) -> bool;
            pub fn repr_symbol(&self, symbol: Symbol) -> &str;
            pub fn repr_non_term(&self, non_term: NonTerm) -> &str;
            pub fn repr_term(&self, term: Term) -> &str;
            pub fn repr_opt_non_term(&self, opt_non_term: Option<NonTerm>) -> &str;
            pub fn repr_opt_term(&self, opt_term: Option<Term>) -> &str;
            pub fn repr_opt_symbol(&self, opt_symbol: Option<Symbol>) -> &str;
        }
    }

    /// returns the number of terminals, non_terminals, distinct non_terminals
    pub fn count_symbols_in_rule(&self, rule: &CfgRule) -> (usize, usize, Idx) {
        let (mut term, mut non_term, mut dist_non_term): (usize, usize, Idx) = (0, 0, Idx(0));

        let mut distinct_non_terminals_found: Vec<Symbol> = Vec::new();

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

    pub fn repr_rule(&self, rule_id: CfgRuleId) -> String {
        let mut s: String = String::new();

        let rule = self.get_rule_by_id(rule_id);
        s.push_str(self.repr_symbol(Symbol::NonTerm(rule.origin)));
        s.push_str(" --> ");

        for &replacement_symbol in &rule.replacement {
            s.push_str(self.repr_symbol(replacement_symbol));
            s.push(' ');
        }
        s.pop();
        s
    }

    pub fn nbr_rules(&self) -> Idx {
        return self.rules.size();
    }

    pub fn all_rules(&self) -> impl Iterator<Item = (CfgRuleId, &CfgRule)> {
        self.rules
            .table
            .iter()
            .enumerate()
            .map(|(id, rule)| (CfgRuleId(Idx::from(id)), rule))
    }

    pub fn get_rule_by_id(&self, rule_id: CfgRuleId) -> &CfgRule {
        // no check: we assume that every CfgRuleId constructed is valid
        self.rules.get_by_id(rule_id.0)
    }

    pub fn get_rules_by_origin(&self, origin: NonTerm) -> impl Iterator<Item = (CfgRuleId, &CfgRule)> {
        let rule_id: Idx = self.rules.rows[usize::from(origin.id(()))];
        (&self.rules[origin.id(())])
            .iter()
            .enumerate()
            .map(move |(id, rule)| (CfgRuleId(Idx::from(id) + rule_id), rule))
    }
    
    /// returns an iterator which go through each (rule_id, rule) of the grammar that can produce produced_symbol
    pub fn get_rules_producing(&self, produced_symbol: Symbol) -> impl Iterator<Item = (CfgRuleId, &CfgRule)> {
        
        let rules_producing_each_symbol: &Vec<BitSet<UINT>> = 
            self.rules_producing_each_symbol.get_or_init(|| self.compute_rules_producing_each_symbol());

        (&rules_producing_each_symbol[usize::from(produced_symbol.id(self.nbr_non_terminals()))])
            .iter()
            .map(|rule_id| (CfgRuleId(Idx::from(rule_id)),
                self.get_rule_by_id(CfgRuleId(Idx::from(rule_id)))))
    }

    fn compute_rules_producing_each_symbol(&self) -> Vec<BitSet<UINT>> {
        // for each symbol, gives a list of the rules whose replacement contains the symbol

        // indexed by symbols produced
        let mut rules_producing_each_symbol: Vec<BitSet<UINT>> = 
        vec![BitSet::new_filled(false, usize::from(self.nbr_rules())); usize::from(self.nbr_symbols())];
        
        for (rule_id, rule) in self.all_rules() {
            for &replacement_symbol in &rule.replacement {
                rules_producing_each_symbol[usize::from(replacement_symbol.id(self.nbr_non_terminals()))]
                .insert(usize::from(rule_id.0));
            }
        }
        rules_producing_each_symbol
    }

    // -------------------------- nullable symbols/words/rules

    pub fn is_symbol_nullable(&self, symbol: Symbol) -> bool {
        match symbol {
            Symbol::NonTerm(non_term) => {
                self.are_symbols_nullable
                    .get_or_init(|| self.compute_are_symbols_nullable())
                    .contains(usize::from(non_term.id(())))
            }
            Symbol::Term(term) => {
                false
            }
        }
    }

    pub fn is_word_nullable(&self, word: &[Symbol]) -> bool {
        word.iter().all(|&symbol| self.is_symbol_nullable(symbol))
    }

    pub fn is_rule_nullable(&self, rule_id: CfgRuleId) -> bool {
        self.get_rule_by_id(rule_id).replacement.iter().all(|&symbol| self.is_symbol_nullable(symbol))
    }

    fn compute_are_symbols_nullable(&self) -> BitSet<UINT> {
        // adapted from here:
        // https://cstheory.stackexchange.com/questions/2479/quickly-finding-empty-string-producing-nonterminals-in-a-cfg


        // initially, all symbols are considered non-nullable
        let mut are_nullable: BitSet<UINT> = BitSet::new_filled(false, usize::from(self.nbr_non_terminals()));

        // number of distinct non_terminals marked as non-nullable that the index rule can produce
        // will be initialised later
        let mut nbr_nullable: Vec<Idx> = vec![Idx(0); usize::from(self.nbr_rules())];


        // stack of non_terminals that have been marked nullable but not yet processed
        let mut unprocessed_nullable_non_terms: Vec<NonTerm> = Vec::new(); 

        // initialize are_nullable and nbr_nullable and unprocessed_nullable_symbols
        for (rule_id, rule) in self.all_rules() {
            if rule.is_empty(){
                if !are_nullable.contains(usize::from(rule.origin.id(()))) {
                    unprocessed_nullable_non_terms.push(rule.origin);
                }
                are_nullable.insert(usize::from(rule.origin.id(())));
                nbr_nullable[usize::from(rule_id.0)] = Idx(0);
            }
            else {
                let (term, _, dist_non_term) = self.count_symbols_in_rule(rule);
                // if all the symbols in rule.replacement are NTsymbols, it could be nullable
                // In the opposite case, we won't even consider the rule because we know it is not nullable
                if term == 0 {
                    nbr_nullable[usize::from(rule_id.0)] = dist_non_term;
                }
            }
        }

        while unprocessed_nullable_non_terms.len() > 0 {
            let unprocessed_nullable_non_term: NonTerm = unprocessed_nullable_non_terms.pop().unwrap();
            for (rule_id, rule) in self.get_rules_producing(Symbol::NonTerm(unprocessed_nullable_non_term)) {

                if self.count_symbols_in_rule(rule).0 != 0 {continue;}  // discard rules containing terminal symbols

                nbr_nullable[usize::from(rule_id.0)] -= Idx(1);

                if nbr_nullable[usize::from(rule_id.0)] == Idx(0) && !are_nullable.contains(usize::from(rule.origin.id)) {
                    are_nullable.insert(usize::from(rule.origin.id(())));
                    unprocessed_nullable_non_terms.push(rule.origin);
                }
            }
        }

        are_nullable
    }

    // -------------------------- first sets

    pub fn get_first_set(&self, symbol: Symbol) -> Cow<BitSet<UINT>> {
        match symbol {
            Symbol::NonTerm(non_term) => {
                Cow::Borrowed(
                    &self.first_sets
                        .get_or_init(|| self.compute_first_sets())[usize::from(non_term.id(()))]
                    )
            }
            Symbol::Term(term) => {
                let mut first_set: BitSet<UINT> = BitSet::new_filled(false, usize::from(self.nbr_terminals())+1);
                // set containing only the terminal symbol itself
                first_set.insert(usize::from(Some(term).id(())));
                
                Cow::Owned(first_set)
            }
        }
    }

    fn compute_first_sets(&self) -> Vec<BitSet<UINT>> {

        // first_sets: dict[Symbol, set[Symbol | NOSYMBOL]] = {symbol: set() for symbol in self.NTsymbols}


        // for each non-terminal symbol, contains a bitset of optional terminal symbols
        // the terminal symbol case is trivial so not computed
        let mut first_sets: Vec<BitSet<UINT>> = 
        vec![BitSet::new_filled(false, usize::from(self.nbr_terminals())+1); usize::from(self.nbr_non_terminals())];

        // for each non-terminal symbol, maps to the set of non-terminal symbols that rely on it (inculde it)
        let mut non_terminal_inclusions: Vec<BitSet<UINT>> = 
        vec![BitSet::new_filled(false, usize::from(self.nbr_non_terminals())); usize::from(self.nbr_non_terminals())];
        // inclusions[a] = {b, c, d} <=> first(a) included in first(b), first(c), and first(d)

        // for each terminal symbol, maps to the set of non-terminal symbols that relies on it (inculde it)
        let mut terminal_inclusions: Vec<BitSet<UINT>> = 
        vec![BitSet::new_filled(false, usize::from(self.nbr_non_terminals())); usize::from(self.nbr_terminals())];
        // terminal_inclusions[a] = [b, c, d] <=> a included in first(b), first(c), and first(d)

        // initialize results that include firsts
        for (rule_id, rule) in self.all_rules() {
            for &symbol in &rule.replacement {
                match symbol {
                    Symbol::NonTerm(non_term) => {
                        // the first set of the non_term is included in the first set of the origin of the rule
                        non_terminal_inclusions[usize::from(non_term.id(()))].insert(usize::from(rule.origin.id(())));

                        // we stay in the loop only if this non_term is nullable
                        if !self.is_symbol_nullable(symbol) {
                            break;
                        }
                    }
                    Symbol::Term(term) => {
                        // the term is included in the first set of the origin of the rule
                        terminal_inclusions[usize::from(term.id(()))].insert(
                            usize::from(rule.origin.id(())));
                        // break because next symbols can't be in the first set of the rule's origin
                        break;
                    }


                }
            }
        }

        // propagation of each term

        // can actually have a bigger size than self.nbr_non_terminals() because duplicates are not discarded instantly
        let mut non_terms_to_process: Vec<NonTerm> = Vec::with_capacity(usize::from(self.nbr_non_terminals()));
        // let non_terms_to_proces_set: BitSet<UINT> = BitSet::new_filled(false, usize::from(self.nbr_non_terminals()));
        let mut non_terms_processed: BitSet<UINT> = BitSet::new_filled(false, usize::from(self.nbr_non_terminals()));
        for term in self.all_terminals() {
            non_terms_processed.clear();

            // initialize non_terms_to_process with terminal_inclusions
            non_terms_to_process.clear();
            non_terms_to_process.extend(terminal_inclusions[usize::from(term.id(()))]
                .iter()
                .map(|x| NonTerm{id: Idx::from(x)}));

            while non_terms_to_process.len() > 0 {
                let non_term_to_process: NonTerm = non_terms_to_process.pop().unwrap();

                if non_terms_processed.contains(usize::from(non_term_to_process.id(()))) {
                    continue;
                }

                first_sets[usize::from(non_term_to_process.id(()))]
                    .insert(usize::from(Some(term).id(())));

                for new_non_term_index_to_process in non_terminal_inclusions[usize::from(non_term_to_process.id(()))].iter() {
                    let new_non_term_to_process: NonTerm = NonTerm { id: Idx::from(new_non_term_index_to_process) };
                    
                    if !non_terms_processed.contains(usize::from(new_non_term_to_process.id(()))) {
                        non_terms_to_process.push(new_non_term_to_process);
                    }
                }

                non_terms_processed.insert(usize::from(non_term_to_process.id(())));
            }

        }

        for non_term in self.all_non_terminals() {
            if self.is_symbol_nullable(Symbol::NonTerm(non_term)) {
                first_sets[usize::from(non_term.id(()))]
                    .insert(usize::from(Option::<Term>::None.id(())));
            }
        }

        return first_sets;
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

