mod grammars;
mod parsing;
mod lexing;
mod datastructures;
mod formal_language;

// use strum::{EnumCount, IntoEnumIterator};
// use strum_macros::{EnumCount as EnumCountMacro, EnumIter};

type BitSetUINT = u8; // used for Bitsets, might slightly affect performance and memory usage?

pub type Result<T> = std::result::Result<T, Error>;
pub type Error = Box<dyn std::error::Error>;

fn main() {
    println!("{}", std::mem::size_of::<Vec<u8>>());
}
