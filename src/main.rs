mod grammars;
mod parsing;
mod lexing;
mod datastructures;
mod formal_language;

use strum::{EnumCount, IntoEnumIterator};
use strum_macros::{EnumCount as EnumCountMacro, EnumIter};

type UINT = u8; // used for Bitsets, might slightly affect performance and memory usage?

// use std::cell::OnceCell;

// struct Graph {
//     edges: Vec<(i32, i32)>,
//     span_tree_cache: OnceCell<Vec<(i32, i32)>>
// }

// impl Graph {
//     fn minimum_spanning_tree(&self) -> &Vec<(i32, i32)> {
//         self.span_tree_cache
//             .get_or_init(|| self.calc_span_tree())
//     }

//     fn calc_span_tree(&self) -> Vec<(i32, i32)> {
//         // Expensive computation goes here
//         vec![]
//     }
// }

// #[derive(EnumCountMacro, EnumIter)]
// enum ExmplNonTermSymb {
//     A,
//     B,
//     C,
// }
// let value: usize = ExmplNonTermSymb::A as usize;
// for x in ExmplNonTermSymb::iter() {
//     println!("{}", ExmplNonTermSymb::COUNT);

// }
// println!("{}", value); // prints 7

fn main() {
    println!("{}", std::mem::size_of::<Vec<u8>>());
}
