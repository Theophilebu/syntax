use std::fmt::Display;

use num::{Signed, PrimInt};


#[derive(Clone, Copy)]
pub struct OptionUint<SINT: Signed + PrimInt> {
    // this represents optional unsigned integers using signed integers
    // the internal signed_value is private because everything is done with usize for the user
    // so the actual range of possible values is not the same as usize
    signed_value: SINT,
}

impl <SINT: Signed + PrimInt> OptionUint<SINT> {
    
    pub fn is_none(&self) -> bool {
        self.signed_value.is_negative()
    }

    pub fn get_value(&self) -> Option<usize> {
        if self.is_none() {
            None
        }
        else {
            Some(SINT::to_usize(&self.signed_value).unwrap())
        }
    }

    pub fn set_value(&mut self, value: Option<usize>) {
        match value {
            None => self.signed_value = -SINT::one(),
            Some(some_value) => self.signed_value = SINT::from(some_value).unwrap(),
        }
    }

    pub fn max_value() -> SINT {
        SINT::max_value()
    }
}

impl <SINT: Signed + PrimInt> From<Option<usize>> for OptionUint<SINT> {

    fn from(value: Option<usize>) -> Self {
        match value {
            None => OptionUint{signed_value: -SINT::one()},
            Some(some_value) => OptionUint{signed_value: SINT::from(some_value).unwrap()},
        } 
    }
}

impl <SINT: Signed + PrimInt> Display for OptionUint<SINT> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.get_value())
        
    }
}

#[cfg(test)]
mod tests {
    use super::OptionUint;

    #[test]
    fn test2() {
        let o: OptionUint<i16> = OptionUint::from(Some(13));
        println!("{}", o.get_value().unwrap_or(100));
    }


}