// use std::cell::OnceCell;
// use std::borrow::Cow;
// use std::marker::PhantomData;
// use crate::datastructures::flat_table::FlatTable;
// // use bumpalo::{Bump, collections::Vec as BumpVec};

// use thiserror::Error;
// use delegate::delegate;


// use super::symbols::{NonTerm, NonTermId, Term, TermId, SymbolEnum, SymbolId, Alphabet};

// use crate::datastructures::indexing::{Indexing, Handle};
// use crate::BitSetUINT;

// use crate::Result;

// // avoid useless generic parameter
// type BitSet = crate::datastructures::bitset::BitSet<BitSetUINT>;
// type BumpBitSet<'c> = crate::datastructures::bump_bitset::BumpBitSet<'c, BitSetUINT>;



// #[derive(Debug)]
// pub enum CfgError {
//     InvalidRuleOrigin{rule: CfgRuleValue},

//     InvalidRuleReplacement{rule: CfgRuleValue, symbol: SymbolEnum},
// }

// // --------------------------------------------

// pub struct CfgSymbolSet {
//     non_terms: Vec<String>,
//     special_non_terms: Vec<String>,
//     terms: Vec<String>,
//     special_terms: Vec<String>,
//     others: Vec<String>,
// }

// impl CfgSymbolSet {

//     const NULL_SYMBOL: &str = "None";

//     // special symbols shouldn't be included 
//     pub fn new(terms: Vec<String>, non_terms: Vec<String>) -> CfgSymbolSet {
//         CfgSymbolSet {
//             non_terms: non_terms,
//             special_non_terms: vec![String::from("START"), String::from("ERR_NON_TERM")],
//             terms: terms,
//             special_terms: vec![String::from("END"), String::from("ERR_TERM")],
//             others: vec![],
//         }
//     }

//     // -----

//     pub fn ERR_NON_TERM(&self) -> NonTerm{
//         NonTerm { id: 0 }
//     }

//     pub fn START(&self) -> NonTerm {
//         NonTerm { id: 1 }
//     }

//     pub fn ERR_TERM(&self) -> Term{
//         Term { id: 0 }
//     }

//     pub fn END(&self) -> Term {
//         Term { id: 1 }
//     }

//     // -----

//     // wrappers to hide the ugly self.nbr_non_terminals()
//     pub fn symbol_id(&self, symbol: SymbolEnum) -> SymbolId {
//         symbol.id(self.nbr_non_terminals())
//     }

//     pub fn symbol_from_id(&self, id: SymbolId) -> SymbolEnum {
//         SymbolEnum::from_id(id, self.nbr_non_terminals())
//     }

//     // -----

//     pub fn nbr_non_terminals(&self) -> NonTermId {
//         NonTermId::from_usize(self.non_terms.len() + self.special_non_terms.len())
//     }

//     // shorthand: used often for conversions
//     pub fn e(&self) -> NonTermId {
//         self.nbr_non_terminals()
//     }

//     pub fn nbr_terminals(&self) -> TermId {
//         TermId::from_usize(self.terms.len() + self.special_terms.len())
//     }

//     pub fn nbr_symbols(&self) -> SymbolId {
//         self.nbr_non_terminals() + self.nbr_terminals() + NonTermId::from_usize(self.others.len())
//     }

//     // -----

//     pub fn all_non_terminals(&self) -> impl Iterator<Item = NonTerm> {
//         (0..self.nbr_non_terminals()).map(|value| NonTerm {id: value})
//     }

//     pub fn all_terminals(&self) -> impl Iterator<Item = Term> {
//         (0..self.nbr_terminals()).map(|value| Term {id: value})
//     }

//     pub fn all_symbols(&self) -> impl Iterator<Item = SymbolEnum> {
//         (self.all_non_terminals()
//             .map(|non_term| SymbolEnum::NonTerm(non_term)))
//         .chain
//         (self.all_terminals()
//             .map(|term| SymbolEnum::Term(term)))
//     }

//     // -----

//     pub fn is_non_terminal(&self, symbol: SymbolEnum) -> bool {
//         match symbol {
//             SymbolEnum::NonTerm(_) => true,
//             SymbolEnum::Term(_) => false,
//         }
//     }

//     pub fn is_terminal(&self, symbol: SymbolEnum) -> bool {
//         match symbol {
//             SymbolEnum::NonTerm(_) => false,
//             SymbolEnum::Term(_) => true,
//         }
//     }

    
//     pub fn is_special(&self, symbol: SymbolEnum) -> bool {
//         return symbol == SymbolEnum::NonTerm(self.START()) 
//         || symbol == SymbolEnum::NonTerm(self.ERR_NON_TERM())
//         || symbol == SymbolEnum::Term(self.END())
//         || symbol == SymbolEnum::Term(self.ERR_TERM())
//     }

//     // --------

//     pub fn repr_non_term(&self, non_term: NonTerm) -> &str {
//         if self.is_special(SymbolEnum::NonTerm(non_term)) {
//             return &self.special_non_terms[non_term.usize_id(())];
//         }
//         else {
//             return &self.non_terms[non_term.usize_id(()) - self.special_non_terms.len()];
//         }
//     }

//     pub fn repr_term(&self, term: Term) -> &str {
//         if self.is_special(SymbolEnum::Term(term)) {
//             return &self.special_terms[term.usize_id(())];
//         }
//         else {
//             return &self.terms[term.usize_id(()) - self.special_terms.len()];
//         }
//     }

//     pub fn repr_symbol(&self, symbol: SymbolEnum) -> &str {
        
//         match symbol {
//             SymbolEnum::NonTerm(non_term) => {
//                 self.repr_non_term(non_term)
//             }
//             SymbolEnum::Term(term) => {
//                 self.repr_term(term)
//             }
//         }
        
//     }

//     pub fn repr_opt_non_term(&self, opt_non_term: Option<NonTerm>) -> &str {
//         match opt_non_term {
//             None => CfgSymbolSet::NULL_SYMBOL,
//             Some(non_term) => self.repr_non_term(non_term),
//         }
//     }

//     pub fn repr_opt_term(&self, opt_term: Option<Term>) -> &str {
//         match opt_term {
//             None => CfgSymbolSet::NULL_SYMBOL,
//             Some(term) => self.repr_term(term),
//         }
//     }

//     pub fn repr_opt_symbol(&self, opt_symbol: Option<SymbolEnum>) -> &str {
//         match opt_symbol {
//             None => CfgSymbolSet::NULL_SYMBOL,
//             Some(symbol) => self.repr_symbol(symbol),
//         }
//     }
//     // --------

//     pub fn get_non_term_by_repr(&self, repr: &str) -> Option<NonTerm> {
//         let special_non_term_index: Option<usize> = self.special_non_terms.iter().position(|element| element==repr);
//         if let Some(special_non_term_index) = special_non_term_index {
//             return Some(NonTerm { id: NonTermId::from_usize(special_non_term_index) });
//         }
//         let non_term_index: Option<usize> = self.non_terms.iter().position(|element| element==repr);
//         if let Some(non_term_index) = non_term_index {
//             return Some(NonTerm { id: NonTermId::from_usize(non_term_index + self.special_non_terms.len()) });
//         }
//         else {
//             return None;
//         }
//     }

//     pub fn get_term_by_repr(&self, repr: &str) -> Option<Term> {
//         let special_term_index: Option<usize> = self.special_terms.iter().position(|element| element==repr);
//         if let Some(special_term_index) = special_term_index {
//             return Some(Term { id: TermId::from_usize(special_term_index) });
//         }
//         let term_index: Option<usize> = self.terms.iter().position(|element| element==repr);
//         if let Some(term_index) = term_index {
//             return Some(Term { id: TermId::from_usize(term_index + self.special_terms.len()) });
//         }
//         else {
//             return None;
//         }
//     }

//     pub fn get_symbol_by_representation(&self, repr: &str) -> Option<SymbolEnum> {

//         let non_term: Option<NonTerm> = self.get_non_term_by_repr(repr);
//         if let Some(non_term) = non_term {
//             return Some(SymbolEnum::NonTerm(non_term));
//         }
//         let term: Option<Term> = self.get_term_by_repr(repr);
//         if let Some(term) = term {
//             return Some(SymbolEnum::Term(term));
//         }
//         else {
//             return None;
//         }
//     }

// }

// // --------------------------------------------

// #[derive(Debug)]
// pub struct CfgRuleValue {
//     pub origin: NonTerm,
//     // not using handles for symbols is fine here, it's far from being a bottleneck
//     pub replacement: Vec<SymbolEnum>,
// }

// impl CfgRuleValue {
//     /// returns true iff the replacement is empty
//     pub fn is_empty(&self) -> bool {
//         self.replacement.len()==0
//     }

//     pub fn replacement_size(&self) -> usize {
//         self.replacement.len()
//     }
// }


// pub struct CfgRule {
//     id: u16
// }
// impl Handle for CfgRule {
//     type Context<'c> = ();
//     type Id = u16;

//     fn id<'c>(&self, _context: Self::Context<'c>) -> Self::Id {
//         self.id
//     }

//     fn from_id<'c>(id: Self::Id, _context: Self::Context<'c>) -> Self {
//         Self { id }
//     }
// }

// type CfgRuleId = <CfgRule as Handle>::Id;

// // --------------------------------------------

// /// augmented context-free grammar
// pub struct Cfg {
//     symbol_set: CfgSymbolSet,
//     rules: FlatTable<CfgRuleValue, NonTermId, usize, CfgRuleId>,

//     // -------------- cached values

//     // indexed by produced symbols
//     rules_producing_each_symbol: OnceCell<Vec<BitSet>>,

//     // indexed by symbols
//     are_symbols_nullable: OnceCell<BitSet>,

//     // indexed by non-terminal symbols, bitset by terminal symbols
//     first_sets: OnceCell<Vec<BitSet>>,

//     // indexed by symbols, bitset by terminal symbols
//     follow_sets: OnceCell<Vec<BitSet>>,

//     /*
//     get_NTsymbols_implied_by_rule
//     get_NTsymbols_implied_by_symbol
//     get_rules_indirectly_producing
//     get_terminating_symbols
//     is_word_nullable
//     is_rule_nullable
//     compute_
//     compute_follow_sets
//     compute_predict_sets
//      */
// }

// impl  Cfg {

//     // TODO: augment the grammar
//     /// also sorts the rules to match the order of the non_terminals in the symbol_set
//     pub fn new(symbol_set: CfgSymbolSet, mut rules: Vec<CfgRuleValue>) -> Result<Cfg> {

//         rules.sort_by(|rule1, rule2| rule1.origin.id(()).cmp(&rule2.origin.id));

//         let mut rule_origin_correspondance: Vec<CfgRuleId> = Vec::with_capacity(symbol_set.nbr_non_terminals().into_usize());
//         let mut current_rule_flat_id: CfgRuleId = 0;
//         for non_terminal_id_value in 0..symbol_set.nbr_non_terminals().0 {
//             rule_origin_correspondance.push(current_rule_flat_id);
//             let mut i = 0;
//             while ((current_rule_flat_id.0 + i) as usize != rules.len()) 
//                 && (rules[(current_rule_flat_id.0 + i) as usize].origin.id.0 == non_terminal_id_value) {
//                 i += 1;
//             }
//             current_rule_flat_id.0 += i;
//         }

//         Ok(Cfg {
//             symbol_set,
//             rules: FlatTable::new(rules, rule_origin_correspondance),

//             rules_producing_each_symbol: OnceCell::new(),

//             are_symbols_nullable: OnceCell::new(),

//             first_sets: OnceCell::new(),

//             follow_sets: OnceCell::new(),

//         })
//     }

//     // -------------------------- utility methods

//     delegate! {
//         to self.symbol_set {
//             pub fn START(&self) -> NonTerm;
//             pub fn END(&self) -> Term;
//             pub fn ERR_NON_TERM(&self) -> NonTerm;
//             pub fn ERR_TERM(&self) -> Term;
//             pub fn symbol_id(&self, symbol: SymbolEnum) -> Idx;
//             pub fn symbol_from_id(&self, id: Idx) -> SymbolEnum;
//             pub fn nbr_non_terminals(&self) -> Idx;
//             pub fn e(&self) -> Idx;
//             pub fn nbr_terminals(&self) -> Idx;
//             pub fn nbr_symbols(&self) -> Idx;
//             pub fn all_non_terminals(&self) -> impl Iterator<Item = NonTerm>;
//             pub fn all_terminals(&self) -> impl Iterator<Item = Term>;
//             pub fn all_symbols(&self) -> impl Iterator<Item = SymbolEnum>;
//             pub fn is_terminal(&self, symbol: SymbolEnum) -> bool;
//             pub fn is_non_terminal(&self, symbol: SymbolEnum) -> bool;
//             pub fn is_special(&self, symbol: SymbolEnum) -> bool;
//             pub fn repr_symbol(&self, symbol: SymbolEnum) -> &str;
//             pub fn repr_non_term(&self, non_term: NonTerm) -> &str;
//             pub fn repr_term(&self, term: Term) -> &str;
//             pub fn repr_opt_non_term(&self, opt_non_term: Option<NonTerm>) -> &str;
//             pub fn repr_opt_term(&self, opt_term: Option<Term>) -> &str;
//             pub fn repr_opt_symbol(&self, opt_symbol: Option<SymbolEnum>) -> &str;
//         }
//     }

//     /// returns the number of terminals, non_terminals, distinct non_terminals
//     pub fn count_symbols_in_rule(&self, rule: &CfgRuleValue) -> (usize, usize, Idx) {
//         let (mut term, mut non_term, mut dist_non_term): (usize, usize, Idx) = (0, 0, Idx(0));

//         let mut distinct_non_terminals_found: Vec<SymbolEnum> = Vec::new();

//         for &symbol in &rule.replacement {
//             if self.is_terminal(symbol) {
//                 term += 1;
//             }
//             else {
//                 non_term += 1;
//                 if !distinct_non_terminals_found.contains(&symbol) {
//                     dist_non_term.0 += 1;
//                     distinct_non_terminals_found.push(symbol);
//                 }
//             }
//         }

//         (term, non_term, dist_non_term)
//     }

//     // --------

//     pub fn repr_rule(&self, rule_id: CfgRuleId) -> String {
//         let mut s: String = String::new();

//         let rule = self.get_rule_by_id(rule_id);
//         s.push_str(self.repr_symbol(SymbolEnum::NonTerm(rule.origin)));
//         s.push_str(" --> ");

//         for &replacement_symbol in &rule.replacement {
//             s.push_str(self.repr_symbol(replacement_symbol));
//             s.push(' ');
//         }
//         s.pop();
//         s
//     }

//     pub fn nbr_rules(&self) -> Idx {
//         return self.rules.size();
//     }

//     pub fn all_rules(&self) -> impl Iterator<Item = (CfgRuleId, &CfgRuleValue)> {
//         self.rules
//             .values
//             .iter()
//             .enumerate()
//             .map(|(id, rule)| (CfgRuleId(Idx::from(id)), rule))
//     }

//     pub fn get_rule_by_id(&self, rule_id: CfgRuleId) -> &CfgRuleValue {
//         // no check: we assume that every CfgRuleId constructed is valid
//         self.rules.get_by_flat_id(rule_id.0)
//     }

//     pub fn get_rules_by_origin(&self, origin: NonTerm) -> impl Iterator<Item = (CfgRuleId, &CfgRuleValue)> {
//         let rule_id: Idx = self.rules.rows[origin.usize_id(())];
//         (&self.rules[origin.id(())])
//             .iter()
//             .enumerate()
//             .map(move |(id, rule)| (CfgRuleId(Idx::from(id) + rule_id), rule))
//     }
    
//     /// returns an iterator which go through each (rule_id, rule) of the grammar that can produce produced_symbol
//     pub fn get_rules_producing(&self, produced_symbol: SymbolEnum) -> impl Iterator<Item = (CfgRuleId, &CfgRuleValue)> {
        
//         let rules_producing_each_symbol: &Vec<BitSet<BitSetUINT>> = 
//             self.rules_producing_each_symbol.get_or_init(|| self.compute_rules_producing_each_symbol());
//         (&rules_producing_each_symbol[produced_symbol.usize_id(self.e())])
//             .iter()
//             .map(|rule_id| (CfgRuleId(Idx::from(rule_id)),
//                 self.get_rule_by_id(CfgRuleId(Idx::from(rule_id)))))
//     }

//     fn compute_rules_producing_each_symbol(&self) -> Vec<BitSet<BitSetUINT>> {
//         // for each symbol, gives a list of the rules whose replacement contains the symbol

//         // indexed by symbols produced
//         let mut rules_producing_each_symbol: Vec<BitSet<BitSetUINT>> = 
//         vec![BitSet::new_filled(false, usize::from(self.nbr_rules())); usize::from(self.nbr_symbols())];
        
//         for (rule_id, rule) in self.all_rules() {
//             for &replacement_symbol in &rule.replacement {
//                 rules_producing_each_symbol[replacement_symbol.usize_id(self.e())]
//                 .insert(usize::from(rule_id.0));
//             }
//         }
//         rules_producing_each_symbol
//     }

//     // -------------------------- nullable symbols/words/rules

//     pub fn is_symbol_nullable(&self, symbol: SymbolEnum) -> bool {
//         match symbol {
//             SymbolEnum::NonTerm(non_term) => {
//                 self.are_symbols_nullable
//                     .get_or_init(|| self.compute_are_symbols_nullable())
//                     .contains(non_term.usize_id(()))
//             }
//             SymbolEnum::Term(term) => {
//                 false
//             }
//         }
//     }

//     pub fn is_word_nullable(&self, word: &[SymbolEnum]) -> bool {
//         word.iter().all(|&symbol| self.is_symbol_nullable(symbol))
//     }

//     pub fn is_rule_nullable(&self, rule_id: CfgRuleId) -> bool {
//         self.get_rule_by_id(rule_id).replacement.iter().all(|&symbol| self.is_symbol_nullable(symbol))
//     }

//     fn compute_are_symbols_nullable(&self) -> BitSet<BitSetUINT> {
//         // adapted from here:
//         // https://cstheory.stackexchange.com/questions/2479/quickly-finding-empty-string-producing-nonterminals-in-a-cfg


//         // initially, all symbols are considered non-nullable
//         let mut are_nullable: BitSet<BitSetUINT> = BitSet::new_filled(false, usize::from(self.nbr_non_terminals()));

//         // number of distinct non_terminals marked as non-nullable that the index rule can produce
//         // will be initialised later
//         let mut nbr_nullable: Vec<Idx> = vec![Idx(0); usize::from(self.nbr_rules())];


//         // stack of non_terminals that have been marked nullable but not yet processed
//         let mut unprocessed_nullable_non_terms: Vec<NonTerm> = Vec::new(); 

//         // initialize are_nullable and nbr_nullable and unprocessed_nullable_symbols
//         for (rule_id, rule) in self.all_rules() {
//             if rule.is_empty(){
//                 if !are_nullable.contains(usize::from(rule.origin.id(()))) {
//                     unprocessed_nullable_non_terms.push(rule.origin);
//                 }
//                 are_nullable.insert(usize::from(rule.origin.id(())));
//                 nbr_nullable[usize::from(rule_id.0)] = Idx(0);
//             }
//             else {
//                 let (term, _, dist_non_term) = self.count_symbols_in_rule(rule);
//                 // if all the symbols in rule.replacement are NTsymbols, it could be nullable
//                 // In the opposite case, we won't even consider the rule because we know it is not nullable
//                 if term == 0 {
//                     nbr_nullable[usize::from(rule_id.0)] = dist_non_term;
//                 }
//             }
//         }

//         while unprocessed_nullable_non_terms.len() > 0 {
//             let unprocessed_nullable_non_term: NonTerm = unprocessed_nullable_non_terms.pop().unwrap();
//             for (rule_id, rule) in self.get_rules_producing(SymbolEnum::NonTerm(unprocessed_nullable_non_term)) {

//                 if self.count_symbols_in_rule(rule).0 != 0 {continue;}  // discard rules containing terminal symbols

//                 nbr_nullable[usize::from(rule_id.0)] -= Idx(1);

//                 if nbr_nullable[usize::from(rule_id.0)] == Idx(0) && !are_nullable.contains(usize::from(rule.origin.id)) {
//                     are_nullable.insert(usize::from(rule.origin.id(())));
//                     unprocessed_nullable_non_terms.push(rule.origin);
//                 }
//             }
//         }

//         are_nullable
//     }

//     // -------------------------- first sets

//     /// returns the first sets of all the non-terminal symbols. The first set of a non-terminal symbol is 
//     /// the set of terminal symbols that can be the first produced by the non-terminal symbol.
//     /// for example:
//     /// A -> a|aB, B -> b|C, C -> A|c,  first(A) = {a}, first(B) = {a, b, c}, first(C) = {a, c}
//     /// If the symbol loops to itself without any terminal symbol before, it doesn't affect the first set.
//     ///    for example: A -> Aa|b,  first(A) = {b}
//     /// The first set of a symbol includes None iff it is nullable """
//     pub fn get_first_set(&self, symbol: SymbolEnum) -> Cow<BitSet<BitSetUINT>> {
//         match symbol {
//             SymbolEnum::NonTerm(non_term) => {
//                 Cow::Borrowed(
//                     &self.first_sets
//                         .get_or_init(|| self.compute_first_sets())[non_term.usize_id(())]
//                     )
//             }
//             SymbolEnum::Term(term) => {
//                 let mut first_set: BitSet<BitSetUINT> = BitSet::new_filled(false, usize::from(self.nbr_terminals())+1);
//                 // set containing only the terminal symbol itself
//                 first_set.insert(usize::from(Some(term).id(())));
                
//                 Cow::Owned(first_set)
//             }
//         }
//     }

//     fn compute_first_sets(&self) -> Vec<BitSet<BitSetUINT>> {



//         // for each non-terminal symbol, contains a bitset of optional terminal symbols
//         // the terminal symbol case is trivial so not computed
//         let mut first_sets: Vec<BitSet<BitSetUINT>> = 
//         vec![BitSet::new_filled(false, usize::from(self.nbr_terminals())+1); usize::from(self.nbr_non_terminals())];

//         // for each non-terminal symbol, maps to the set of non-terminal symbols that rely on it (inculde it)
//         let mut non_terminal_inclusions: Vec<BitSet<BitSetUINT>> = 
//         vec![BitSet::new_filled(false, usize::from(self.nbr_non_terminals())); usize::from(self.nbr_non_terminals())];
//         // inclusions[a] = {b, c, d} <=> first(a) included in first(b), first(c), and first(d)

//         // for each terminal symbol, maps to the set of non-terminal symbols that relies on it (inculde it)
//         let mut terminal_inclusions: Vec<BitSet<BitSetUINT>> = 
//         vec![BitSet::new_filled(false, usize::from(self.nbr_non_terminals())); usize::from(self.nbr_terminals())];
//         // terminal_inclusions[a] = {b, c, d} <=> a included in first(b), first(c), and first(d)

//         // initialize results that include firsts
//         for (rule_id, rule) in self.all_rules() {
//             for &symbol in &rule.replacement {
//                 match symbol {
//                     SymbolEnum::NonTerm(non_term) => {
//                         // the first set of the non_term is included in the first set of the origin of the rule
//                         non_terminal_inclusions[non_term.usize_id(())].insert(rule.origin.usize_id(()));

//                         // we stay in the loop only if this non_term is nullable
//                         if !self.is_symbol_nullable(symbol) {
//                             break;
//                         }
//                     }
//                     SymbolEnum::Term(term) => {
//                         // the term is included in the first set of the origin of the rule
//                         terminal_inclusions[term.usize_id(())].insert(
//                             usize::from(rule.origin.id(())));
//                         // break because next symbols can't be in the first set of the rule's origin
//                         break;
//                     }


//                 }
//             }
//         }

//         // propagation of each term

//         // can actually have a bigger size than self.nbr_non_terminals() because duplicates are not discarded instantly
//         let mut non_terms_to_process: Vec<NonTerm> = Vec::with_capacity(usize::from(self.nbr_non_terminals()));
//         // let non_terms_to_proces_set: BitSet<UINT> = BitSet::new_filled(false, usize::from(self.nbr_non_terminals()));
//         let mut non_terms_processed: BitSet<BitSetUINT> = BitSet::new_filled(false, usize::from(self.nbr_non_terminals()));
//         for term in self.all_terminals() {
//             non_terms_processed.clear();

//             // initialize non_terms_to_process with terminal_inclusions
//             non_terms_to_process.clear();
//             non_terms_to_process.extend(terminal_inclusions[term.usize_id(())]
//                 .iter()
//                 .map(|x| NonTerm{id: Idx::from(x)}));

//             while non_terms_to_process.len() > 0 {
//                 let non_term_to_process: NonTerm = non_terms_to_process.pop().unwrap();

//                 if non_terms_processed.contains(non_term_to_process.usize_id(())) {
//                     continue;
//                 }

//                 first_sets[non_term_to_process.usize_id(())]
//                     .insert(usize::from(Some(term).id(())));

//                 for new_non_term_index_to_process in non_terminal_inclusions[non_term_to_process.usize_id(())].iter() {
//                     let new_non_term_to_process: NonTerm = NonTerm { id: Idx::from(new_non_term_index_to_process) };
                    
//                     if !non_terms_processed.contains(new_non_term_to_process.usize_id(())) {
//                         non_terms_to_process.push(new_non_term_to_process);
//                     }
//                 }

//                 non_terms_processed.insert(non_term_to_process.usize_id(()));
//             }

//         }

//         for non_term in self.all_non_terminals() {
//             if self.is_symbol_nullable(SymbolEnum::NonTerm(non_term)) {
//                 first_sets[non_term.usize_id(())]
//                     .insert(Option::<Term>::None.usize_id(()));
//             }
//         }

//         return first_sets;
//     }

//     // -------------------------- follow sets
    
//     /// returns the follow set of the the non-terminal symbol. The follow set of a non-terminal symbol is 
//     /// the set of terminal symbols that can be the first after the non-terminal symbol. for example:
//     /// A -> Aa|aB, B -> aBb|Cb, C -> Ac|c,  follow(A) = {a, c}, follow(B) = {b}, follow(C) = {b}
//     /// If nothing can go after an non-terminating symbol (loop), its follow set would be empty. example:
//     /// Start -> aA, A -> a|bA,  follow(Start) = {None}, follow(A) = {}
//     /// Start -> Aa, A -> a|bA,  follow(Start) = {None}, follow(A) = {a}
//     /// If nothing can go after a terminating symbol, its follow set would include None
//     pub fn get_follow_set(&self, symbol: SymbolEnum) -> &BitSet<BitSetUINT> {
//         &self.first_sets
//             .get_or_init(|| self.compute_first_sets())[usize::from(self.symbol_id(symbol))]                
//     }

//     fn compute_follow_sets(&self) -> Vec<BitSet<BitSetUINT>> {


//         let mut follow_sets_non_terms: Vec<BitSet<BitSetUINT>> = 
//         vec![BitSet::new_filled(false, usize::from(self.nbr_terminals())+1); usize::from(self.nbr_non_terminals())];


//         // for each non-terminal symbol, maps to the set of non-terminal symbols that rely on it (inculde it)
//         let mut inclusions_first: Vec<BitSet<BitSetUINT>> = 
//         vec![BitSet::new_filled(false, usize::from(self.nbr_non_terminals())); usize::from(self.nbr_non_terminals())];
//         // inclusions[a] = {b, c, d} <=> first(a) included in follow(b), follow(c), and follow(d)

//         // for each non-terminal symbol, maps to the set of symbols that relies on it (inculde it)
//         let mut inclusions_follow: Vec<BitSet<BitSetUINT>> = 
//         vec![BitSet::new_filled(false, usize::from(self.nbr_symbols())); usize::from(self.nbr_non_terminals())];
//         // inclusions[a] = {b, c, d} <=> follow(a) included in follow(b), follow(c), and follow(d)

//         // for each terminal symbol and None, maps to the set of symbols that relies on it (inculde it)
//         let terminal_inclusions: Vec<BitSet<BitSetUINT>> = 
//         vec![BitSet::new_filled(false, usize::from(self.nbr_symbols())); usize::from(self.nbr_terminals())+1];
//         // inclusions[a] = [b, c, d] <=> a included in follow(b), follow(c), and follow(d)

        
//         // initialize the inclusions
//         for (rule_id, rule) in self.all_rules() {
//             for (i, current_symbol) in rule.replacement.iter().enumerate() {
            
//                 match current_symbol {
//                     SymbolEnum::Term(_) => {continue;}
//                     SymbolEnum::NonTerm(current_non_term) => {

//                         if i == rule.replacement.len()-1 {
//                             // if a non-terminal symbol is at the end of the replacement of a rule, the follow set of 
//                             // the origin of the rule is included in the follow set of the end symbol
//                             inclusions_follow[usize::from(rule.origin.id(()))]
//                             .insert(current_non_term.usize_id(()));
//                             continue;
//                         }

//                         let next_symbol: SymbolEnum = rule.replacement[i+1];

//                         match next_symbol {
//                             SymbolEnum::NonTerm(next_non_term) => {
//                                 // next_symbol is included in the follow set of the current_symbol
//                                 terminal_inclusions[]
//                                 terminal_inclusions[next_symbol].append(current_symbol)
//                             }
//                             SymbolEnum::Term(next_term) => {

//                             }
//                         }
//                     }
//                 }
//             }
//         }
//         //         if next_symbol in self.Tsymbols:
//         //             # next_symbol is included in the follow set of the current_symbol
//         //             inclusions_terminal[next_symbol].append(current_symbol)

//         //         else:   # if next_symbol in self.NTsymbols:
//         //             # the first set of the next_symbol is included in the follow set of the current_symbol
//         //             inclusions_first[next_symbol].append(current_symbol)

//         // # NoSymbol is originally in the follow set of a NTsymbol if the NTsymbol is not on any rule replacement
//         // # NoSymbol will then be propagated along other Tsymbols
//         // for NTsymbol_not_produced in NTsymbols_not_produced:
//         //     inclusions_terminal[NoSymbol].append(NTsymbol_not_produced)


//         // # propagation of first sets of NTsymbols
//         // for NTsymbol_first in self.NTsymbols:
//         //     NTsymbols_to_process: list[Symbol] = inclusions_first[NTsymbol_first].copy()
//         //     NTsymbols_processed: set[Symbol] = set()

//         //     while len(NTsymbols_to_process) > 0:
//         //         NTsymbol_to_process = NTsymbols_to_process.pop()

//         //         if NTsymbol_to_process in NTsymbols_processed:
//         //             continue

//         //         first_set = self.get_first_set_of_symbol(NTsymbol_first)
//         //         first_set.discard(NoSymbol)
//         //         # NoSymbol would be in the follow set of a symbol only if the symbol is at the end of every single
//         //         # rule it appears in.
//         //         follow_sets_NTsymbols[NTsymbol_to_process].update(first_set)

//         //         for new_NTsymbol_to_process in inclusions_follow[NTsymbol_to_process]:
//         //             # follow(NTsymbol_to_process) included in follow(new_NTsymbol_to_process)
//         //             # which implies first(NTsymbol_to_process) included in follow(new_NTsymbol_to_process)
//         //             if not new_NTsymbol_to_process in NTsymbols_processed:
//         //                 NTsymbols_to_process.append(new_NTsymbol_to_process)

//         //         NTsymbols_processed.add(NTsymbol_to_process)


//         // # propagation of Tsymbols
//         // for Tsymbol in self.Tsymbols.union([NoSymbol]):
//         //     NTsymbols_to_process: list[Symbol] = inclusions_terminal[Tsymbol].copy()
//         //     NTsymbols_processed: set[Symbol] = set()

//         //     while len(NTsymbols_to_process) > 0:
//         //         NTsymbol_to_process = NTsymbols_to_process.pop()

//         //         if NTsymbol_to_process in NTsymbols_processed:
//         //             continue

//         //         follow_sets_NTsymbols[NTsymbol_to_process].add(Tsymbol)

//         //         for new_NTsymbol_to_process in inclusions_follow[NTsymbol_to_process]:
//         //             # follow(NTsymbol_to_process) included in follow(new_NTsymbol_to_process)
//         //             # which implies Tsymbol is in follow(new_NTsymbol_to_process)
//         //             if not new_NTsymbol_to_process in NTsymbols_processed:
//         //                 NTsymbols_to_process.append(new_NTsymbol_to_process)

//         //         NTsymbols_processed.add(NTsymbol_to_process)


//         // # ----------------------------------------------------------------
//         // # handles Tsymbols separately, as they are not needed for NTsymbols
//         // follow_sets_Tsymbols={Tsymbol: set() for Tsymbol in self.Tsymbols}

//         // for rule in self.rules:
//         //     for i, current_symbol in enumerate(rule.replacement):

//         //         if current_symbol in self.NTsymbols:
//         //             continue

//         //         if i==len(rule.replacement)-1:
//         //             # if Tsymbol is at the end of the replacement of a rule, the follow set of the origin of the
//         //             # rule is included in the follow set of the end symbol
//         //             follow_sets_Tsymbols[current_symbol].update(follow_sets_NTsymbols[rule.origin])
//         //             continue

//         //         next_symbol = rule.replacement[i+1]

//         //         if next_symbol in self.Tsymbols:
//         //             # next_symbol is included in the follow set of the current_symbol
//         //             follow_sets_Tsymbols[current_symbol].add(next_symbol)

//         //         else:   # if next_symbol in self.NTsymbols:
//         //             # the first set of the next_symbol is included in the follow set of the current_symbol
//         //             follow_sets_Tsymbols[current_symbol].update(self.get_first_set_of_symbol(next_symbol))


//         // follow_sets = follow_sets_NTsymbols
//         // follow_sets.update(follow_sets_Tsymbols)

//         follow_sets_non_terms
//     }
    


// }



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

