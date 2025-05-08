use std::borrow::Cow;

use crate::formal_language::*;


pub fn create_regex_grammar() -> Cfg {

    let lower_letters = String::from("azertyuiopqsdfghjklmwxcvbn");
    let upper_letters = String::from("AZERTYUIOPQSDFGHJKLMWXCVBN");
    let digits = String::from("1234567890");
    let invisible_chars = String::from(" \n\t");
    let other_chars = String::from("!\"#$%&'()*+,-./:;>=<?@[\\]^_`{|}~"); // ASCII 33-126 except

    let all_chars = [
        Vec::from_iter(lower_letters.chars()),
        Vec::from_iter(upper_letters.chars()),
        Vec::from_iter(digits.chars()),
        Vec::from_iter(invisible_chars.chars()),
        Vec::from_iter(other_chars.chars()),].concat();


    let alphabet: Alphabet = Alphabet::new(all_chars);

    let terminals: Vec<String> = vec![
        String::from("+"),
        String::from("*"),
        String::from("?"),
        String::from("+?"),
        String::from("*?"),

        String::from("{"),
        String::from("}"),
        String::from("("),
        String::from(")"),
        String::from("["),
        String::from("]"),

        String::from("|"),

        String::from("."),
        String::from("^"),
        String::from("$"),

        String::from("char"),       // abc...
        String::from("list_char"),  // abc... but in lists: [ab] <- char a or b

        String::from(","),

        String::from("int"),

        String::from("[:alnum:]"),
        String::from("[:word:]"),
        String::from("[:alpha:]"),
        String::from("[:blank:]"),
        String::from("[:cntrl:]"),
        String::from("[:digit:]"),
        String::from("[:graph:]"),
        String::from("[:lower:]"),
        String::from("[:print:]"),
        String::from("[:punct:]"),
        String::from("[:space:]"),
        String::from("[:upper:]"),
        String::from("[:xdigit:]"),
    ];

    let non_terminals: Vec<String> = vec![
        String::from("Expression"),

        String::from("Union__Extend"),

        String::from("Sequence"),
        String::from("Sequence__Extend"),

        String::from("Term"),

        String::from("Modifier"),
        String::from("Modifier__Optional"),

        String::from("Interval"),

        String::from("Integer__Optional"),

        String::from("Item"),

        String::from("Group"),

        String::from("List"),           // [abc], not empty

        String::from("ListSequence"),   // abc[:upper:]d
        String::from("ListSequence__Extend"),

        String::from("ListMember"),

        String::from("Hat__Optional"),

        String::from("CharClass"),      // [:upper:]
    ];

    let cfg_symbol_set = CfgSymbolSet::new(terminals, non_terminals);

    // NT for NonTerm
    let NT = |s: &str| match cfg_symbol_set.get_symbol_by_representation(s).unwrap() {
        Symbol::NonTerm(non_term) => non_term,
        Symbol::Term(term) => {panic!("this is not a non-terminal!");}
    };

    // S for Symbol
    let S = |s: &str| cfg_symbol_set.get_symbol_by_representation(s).unwrap();

    let rules:Vec<CfgRule> = vec![
        CfgRule { origin: NT("START"), replacement: vec![S("Expression"), S("END")]},
        CfgRule { origin: NT("START"), replacement: vec![S("END")]}, 

        CfgRule { origin: NT("Expression"), replacement: vec![S("Sequence"), S("Union__Extend")]}, 

        CfgRule { origin: NT("Union__Extend"), replacement: vec![S("|"), S("Sequence")]},
        CfgRule { origin: NT("Union__Extend"), replacement: vec![]}, 

        CfgRule { origin: NT("Sequence"), replacement: vec![S("Term"), S("Sequence__Extend")]},

        CfgRule { origin: NT("Sequence__Extend"), replacement: vec![S("Term"), S("Sequence__Extend")]},
        CfgRule { origin: NT("Sequence__Extend"), replacement: vec![]}, 

        CfgRule { origin: NT("Term"), replacement: vec![S("Item"), S("Modifier__Optional")]},
        // CfgRule { origin: S("Term"), replacement: vec![]}, 

        CfgRule { origin: NT("Modifier__Optional"), replacement: vec![S("Modifier")]},
        CfgRule { origin: NT("Modifier__Optional"), replacement: vec![]}, 

        CfgRule { origin: NT("Modifier"), replacement: vec![S("*")]},
        CfgRule { origin: NT("Modifier"), replacement: vec![S("+")]},
        CfgRule { origin: NT("Modifier"), replacement: vec![S("?")]},
        CfgRule { origin: NT("Modifier"), replacement: vec![S("+?")]},
        CfgRule { origin: NT("Modifier"), replacement: vec![S("*?")]},
        CfgRule { origin: NT("Modifier"), replacement: vec![S("Interval")]},

        CfgRule { origin: NT("Interval"), replacement: vec![S("{"), S("Integer__Optional"), S(","), S("Integer__Optional"), S("}")]},

        CfgRule { origin: NT("Integer__Optional"), replacement: vec![S("int")]},
        CfgRule { origin: NT("Integer__Optional"), replacement: vec![]}, 

        CfgRule { origin: NT("Item"), replacement: vec![S("char")]},
        CfgRule { origin: NT("Item"), replacement: vec![S(".")]},
        CfgRule { origin: NT("Item"), replacement: vec![S("^")]},
        CfgRule { origin: NT("Item"), replacement: vec![S("$")]},
        CfgRule { origin: NT("Item"), replacement: vec![S("List")]},
        CfgRule { origin: NT("Item"), replacement: vec![S("Group")]},

        CfgRule { origin: NT("Group"), replacement: vec![S("("), S("Expression"), S(")")]},

        CfgRule { origin: NT("List"), replacement: vec![S("["), S("Hat__Optional"), S("ListSequence"), S("]")]},

        CfgRule { origin: NT("Hat__Optional"), replacement: vec![S("^")]},
        CfgRule { origin: NT("Hat__Optional"), replacement: vec![]}, 

        CfgRule { origin: NT("ListSequence"), replacement: vec![S("ListMember"), S("ListSequence__Extend")]},

        CfgRule { origin: NT("ListSequence__Extend"), replacement: vec![S("ListMember"), S("ListSequence__Extend")]},
        CfgRule { origin: NT("ListSequence__Extend"), replacement: vec![]},

        CfgRule { origin: NT("ListMember"), replacement: vec![S("list_char")]},
        CfgRule { origin: NT("ListMember"), replacement: vec![S("CharClass")]},

        CfgRule { origin: NT("CharClass"), replacement: vec![S("[:alnum:]")]},
        CfgRule { origin: NT("CharClass"), replacement: vec![S("[:word:]")]},
        CfgRule { origin: NT("CharClass"), replacement: vec![S("[:alpha:]")]},
        CfgRule { origin: NT("CharClass"), replacement: vec![S("[:blank:]")]},
        CfgRule { origin: NT("CharClass"), replacement: vec![S("[:cntrl:]")]},
        CfgRule { origin: NT("CharClass"), replacement: vec![S("[:digit:]")]},
        CfgRule { origin: NT("CharClass"), replacement: vec![S("[:graph:]")]},
        CfgRule { origin: NT("CharClass"), replacement: vec![S("[:lower:]")]},
        CfgRule { origin: NT("CharClass"), replacement: vec![S("[:print:]")]},
        CfgRule { origin: NT("CharClass"), replacement: vec![S("[:punct:]")]},
        CfgRule { origin: NT("CharClass"), replacement: vec![S("[:xdigit:]")]},
        CfgRule { origin: NT("CharClass"), replacement: vec![S("[:space:]")]},
        CfgRule { origin: NT("CharClass"), replacement: vec![S("[:upper:]")]},

    ];

    let cfg = Cfg::new(cfg_symbol_set, rules);
    cfg.unwrap()
}


#[cfg(test)]
mod tests{

    use std::path::Display;

    use super::create_regex_grammar;
    use crate::{datastructures::bitset::BitSet, formal_language::*, Idx};

    #[test]
    fn test1(){
        let cfg = create_regex_grammar();

        for non_terminal in cfg.all_non_terminals() {
            println!("{:?}", non_terminal);
            println!("{:?}", cfg.repr_non_term(non_terminal));
        }

        for terminal in cfg.all_terminals() {
            println!("{:?}", terminal);
            println!("{:?}", cfg.repr_term(terminal));
        }


        for (rule_id, rule) in cfg.all_rules() {
            println!("{}", cfg.repr_rule(rule_id));
        }

        for symbol in cfg.all_symbols() {
            println!("symbol : {}", cfg.repr_symbol(symbol));
            for (rule_id, rule) in cfg.get_rules_producing(symbol) {
                println!("    {:?}", cfg.repr_rule(rule_id));
            }
        }

        for symbol in cfg.all_symbols() {
            println!("symbol : {}, {}", cfg.repr_symbol(symbol), cfg.is_symbol_nullable(symbol));
        }

        let sy = |x: usize| cfg.repr_symbol(cfg.symbol_from_id(Idx::from(x)));
        let te = |x: usize| cfg.repr_term(Term{id: Idx::from(x)});

        let opt_te = |x: usize| cfg.repr_opt_term(Option::<Term>::from_id(Idx::from(x), ()));




        for symbol in cfg.all_symbols() {
            println!("symbol: {}", cfg.repr_symbol(symbol));
            println!("first_set:");
            cfg.get_first_set(symbol).print_set(opt_te);
        }
    }
}
