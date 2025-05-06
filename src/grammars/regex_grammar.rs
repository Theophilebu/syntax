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

    let terminals: SymbolSet = SymbolSet::new(vec![
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
    ]);

    let non_terminals: SymbolSet = SymbolSet::new(vec![
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
    ]);

    let cfg_symbol_set = CfgSymbolSet::new(terminals, non_terminals);

    // S for Symbol
    let S = |s: &str| cfg_symbol_set.get_symbol_by_representation(s);

    let rules:Vec<CfgRule> = vec![
        CfgRule { origin: S("START"), replacement: vec![S("Expression"), S("END")]},
        CfgRule { origin: S("START"), replacement: vec![S("END")]}, 

        CfgRule { origin: S("Expression"), replacement: vec![S("Sequence"), S("Union__Extend")]}, 

        CfgRule { origin: S("Union__Extend"), replacement: vec![S("|"), S("Sequence")]},
        CfgRule { origin: S("Union__Extend"), replacement: vec![]}, 

        CfgRule { origin: S("Sequence"), replacement: vec![S("Term"), S("Sequence__Extend")]},

        CfgRule { origin: S("Sequence__Extend"), replacement: vec![S("Term"), S("Sequence__Extend")]},
        CfgRule { origin: S("Sequence__Extend"), replacement: vec![]}, 

        CfgRule { origin: S("Term"), replacement: vec![S("Item"), S("Modifier__Optional")]},
        // CfgRule { origin: S("Term"), replacement: vec![]}, 

        CfgRule { origin: S("Modifier__Optional"), replacement: vec![S("Modifier")]},
        CfgRule { origin: S("Modifier__Optional"), replacement: vec![]}, 

        CfgRule { origin: S("Modifier"), replacement: vec![S("*")]},
        CfgRule { origin: S("Modifier"), replacement: vec![S("+")]},
        CfgRule { origin: S("Modifier"), replacement: vec![S("?")]},
        CfgRule { origin: S("Modifier"), replacement: vec![S("+?")]},
        CfgRule { origin: S("Modifier"), replacement: vec![S("*?")]},
        CfgRule { origin: S("Modifier"), replacement: vec![S("Interval")]},

        CfgRule { origin: S("Interval"), replacement: vec![S("{"), S("Integer__Optional"), S(","), S("Integer__Optional"), S("}")]},

        CfgRule { origin: S("Integer__Optional"), replacement: vec![S("int")]},
        CfgRule { origin: S("Integer__Optional"), replacement: vec![]}, 

        CfgRule { origin: S("Item"), replacement: vec![S("char")]},
        CfgRule { origin: S("Item"), replacement: vec![S(".")]},
        CfgRule { origin: S("Item"), replacement: vec![S("^")]},
        CfgRule { origin: S("Item"), replacement: vec![S("$")]},
        CfgRule { origin: S("Item"), replacement: vec![S("List")]},
        CfgRule { origin: S("Item"), replacement: vec![S("Group")]},

        CfgRule { origin: S("Group"), replacement: vec![S("("), S("Expression"), S(")")]},

        CfgRule { origin: S("List"), replacement: vec![S("["), S("Hat__Optional"), S("ListSequence"), S("]")]},

        CfgRule { origin: S("Hat__Optional"), replacement: vec![S("^")]},
        CfgRule { origin: S("Hat__Optional"), replacement: vec![]}, 

        CfgRule { origin: S("ListSequence"), replacement: vec![S("ListMember"), S("ListSequence__Extend")]},

        CfgRule { origin: S("ListSequence__Extend"), replacement: vec![S("ListMember"), S("ListSequence__Extend")]},
        CfgRule { origin: S("ListSequence__Extend"), replacement: vec![]},

        CfgRule { origin: S("ListMember"), replacement: vec![S("list_char")]},
        CfgRule { origin: S("ListMember"), replacement: vec![S("CharClass")]},

        CfgRule { origin: S("CharClass"), replacement: vec![S("[:alnum:]")]},
        CfgRule { origin: S("CharClass"), replacement: vec![S("[:word:]")]},
        CfgRule { origin: S("CharClass"), replacement: vec![S("[:alpha:]")]},
        CfgRule { origin: S("CharClass"), replacement: vec![S("[:blank:]")]},
        CfgRule { origin: S("CharClass"), replacement: vec![S("[:cntrl:]")]},
        CfgRule { origin: S("CharClass"), replacement: vec![S("[:digit:]")]},
        CfgRule { origin: S("CharClass"), replacement: vec![S("[:graph:]")]},
        CfgRule { origin: S("CharClass"), replacement: vec![S("[:lower:]")]},
        CfgRule { origin: S("CharClass"), replacement: vec![S("[:print:]")]},
        CfgRule { origin: S("CharClass"), replacement: vec![S("[:punct:]")]},
        CfgRule { origin: S("CharClass"), replacement: vec![S("[:space:]")]},
        CfgRule { origin: S("CharClass"), replacement: vec![S("[:upper:]")]},
        CfgRule { origin: S("CharClass"), replacement: vec![S("[:xdigit:]")]},

    ];

    let cfg = Cfg::new(cfg_symbol_set, rules);
    cfg.unwrap()
}


#[cfg(test)]
mod tests{

    use super::create_regex_grammar;

    #[test]
    fn test1(){
        let cfg = create_regex_grammar();

        for non_terminal in cfg.all_non_terminals() {
            println!("{:?}", non_terminal);
            println!("{:?}", cfg.repr_symbol(non_terminal));
        }

        for terminal in cfg.all_terminals() {
            println!("{:?}", terminal);
            println!("{:?}", cfg.repr_symbol(terminal));
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
    }
}