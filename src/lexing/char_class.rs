use crate::datastructures::indexing::Handle;


#[derive(Debug, Clone, Copy)]
struct CharSet {
    id: u8,
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
    membership_functions: Vec<Box<dyn Fn(char) -> bool>>
}

impl CharSetList {    
    fn is_char_in(&self, c: char, char_set: CharSet) -> bool{
        self.membership_functions[char_set.usize_id(())](c)
    }
}