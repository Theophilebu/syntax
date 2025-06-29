use crate::datastructures::indexing::Handle;

// first private use area(small): U+E000..U+F8FF
// supplementary private use area A: U+F0000..U+FFFFD
// supplementary private use area B: U+100000..U+10FFFD

const PUA_START: u32 = 0xE000;
const PUA_END: u32 = 0xF8FF;

const PUAA_START: u32 = 0xF0000;
const PUAA_END: u32 = 0xFFFFD;

const PUAB_START: u32 = 0x100000;
const PUAB_END: u32 = 0x10FFFD;


pub fn is_u32_in_first_PUA(c: u32) -> bool {
    PUA_START <= c && c <= PUA_END
}

pub fn is_char_in_first_PUA(c: char) -> bool {
    PUA_START <= u32::from(c) && u32::from(c) <= PUA_END
}

/// unpacked definition of an extended char, to be easier to use with code
/// considered valid once constructed
#[derive(Debug, Clone, Copy)]
pub enum ExtendedChar {
    None,
    Char(char), // not in private use area
    CharSet(CharSet),
}

impl From<CompactExtendedChar> for ExtendedChar {

    fn from(extended_char: CompactExtendedChar) -> Self {
        if extended_char.value == PUA_START {
            return Self::None;
        }
        else if is_u32_in_first_PUA(extended_char.value) {
            return Self::CharSet(CharSet { id: u8::try_from(extended_char.value - PUA_START - 1).unwrap() });
        }
        else {
            Self::Char(char::from_u32(extended_char.value).unwrap())
        }
    }
}

impl ExtendedChar {
    pub fn is_none(&self) -> bool {
        match self {
            ExtendedChar::None => true,
            _ => false,
        }
    }

    pub fn is_char(&self) -> bool {
        match self {
            ExtendedChar::Char(_) => true,
            _ => false,
        }
    }

    pub fn is_char_set(&self) -> bool {
        match self {
            ExtendedChar::CharSet(_) => true,
            _ => false,
        }
    }

    pub fn match_char(&self, c: char, char_set_list: &CharSetList) -> bool {
        match self {
            ExtendedChar::None => false,
            ExtendedChar::Char(c2) => (c==*c2),
            ExtendedChar::CharSet(char_set) => char_set_list.is_char_in(c, *char_set),

        }
    }
}

/// can represent a char, no char(epsilon), or a char set
#[derive(Debug, Clone, Copy)]
pub struct CompactExtendedChar {
    value: u32,
}

impl From<ExtendedChar> for CompactExtendedChar {
    fn from(extended_char: ExtendedChar) -> Self {
        match extended_char {
            ExtendedChar::None => Self {value: PUA_START},
            ExtendedChar::Char(c) => Self {value: u32::from(c)},
            ExtendedChar::CharSet(char_set) => Self { value: PUA_START + 1 + u32::from(char_set.id(())) }
        }
    }
}


impl CompactExtendedChar {
    pub fn is_none(&self) -> bool {
        self.value == PUA_END
    }

    pub fn is_char(&self) -> bool {
        !is_u32_in_first_PUA(self.value)
    }

    pub fn is_char_set(&self) -> bool {
        PUA_START < self.value && self.value <= PUA_END
    }

    pub fn match_char(&self, c: char, char_set_list: &CharSetList) -> bool {

        if self.is_none() {
            return false;
        }
        else if self.is_char() {
            return u32::from(c) == self.value;
        }
        else {
            let char_set: CharSet = CharSet { id: (self.value - PUA_START - 1).try_into().unwrap() };
            return char_set_list.is_char_in(c, char_set);
        }
    }
}


#[derive(Debug, Clone, Copy)]
pub struct CharSet {
    pub id: u8,
}
impl Handle for CharSet {

    type Context<'c> = ();
    type Id = u8;

    fn id(&self, _: ()) -> Self::Id {
        self.id
    }

    fn from_id(id: Self::Id, _: ()) -> Self {
        Self { id }
    }
}


pub struct CharSetList {
    pub membership_functions: Vec<Box<dyn Fn(char) -> bool>>
}

impl CharSetList {    
    pub fn is_char_in(&self, c: char, char_set: CharSet) -> bool{
        self.membership_functions[char_set.usize_id(())](c)
    }
}

impl std::fmt::Debug for CharSetList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CharSetList")
    }
} 