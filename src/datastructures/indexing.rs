// use derive_more::{Add, Sub, Mul, Div, AddAssign, SubAssign, MulAssign, DivAssign};


/// conversion between a handle and usize
/// panics if a conversion is not possible
pub trait Indexing: Copy {
    fn from_usize(usize_id: usize) -> Self;
    fn into_usize(self) -> usize;
}

impl Indexing for u8 {
    fn from_usize(usize_id: usize) -> Self {
        Self::try_from(usize_id).expect(
            "conversion to u8 failed. The usize_id doesn't correspond with a valid handle."
        )
    }
    fn into_usize(self) -> usize {
        usize::from(self)
    }
}

impl Indexing for u16 {
    fn from_usize(usize_id: usize) -> Self {
        Self::try_from(usize_id).expect(
            "conversion to u8 failed. The usize_id doesn't correspond with a valid handle."
        )
    }
    fn into_usize(self) -> usize {
        usize::from(self)
    }
}

impl Indexing for u32 {
    fn from_usize(usize_id: usize) -> Self {
        Self::try_from(usize_id).expect(
            "conversion to u8 failed. The usize_id doesn't correspond with a valid handle."
        )
    }
    fn into_usize(self) -> usize {
        usize::try_from(self).expect(
            "conversion from u32 to usize failed. The handle is too large to correspond with a valid usize."
        )
    }
}

impl Indexing for u64 {
    fn from_usize(usize_id: usize) -> Self {
        Self::try_from(usize_id).expect(
            "conversion to u8 failed. The usize_id doesn't correspond with a valid handle."
        )
    }
    fn into_usize(self) -> usize {
        usize::try_from(self).expect(
            "conversion from u64 to usize failed. The handle is too large to correspond with a valid usize."
        )
    }
}


// ------


// Context represents data common for a set of values, so that it is useless to store it inside the value
pub trait Handle: Sized {

    type Id: Indexing;
    type Context<'c>;

    fn id<'c>(&self, context: Self::Context<'c>) -> Self::Id;
    fn from_id<'c>(id: Self::Id, context: Self::Context<'c>) -> Self;

    // used for more concise expressions
    fn usize_id<'c>(&self, context: Self::Context<'c>) -> usize {
        (self.id(context)).into_usize()
    }

    fn from_usize<'c>(usize_id: usize, context: Self::Context<'c>) -> Self {
        Self::from_id(Self::Id::from_usize(usize_id), context)
    }

}

struct X{}

impl Handle for X {
    type Id = u32;
    type Context<'c> = &'c u32;

    fn id<'c>(&self, context: Self::Context<'c>) -> Self::Id {
        return 3;
    }

    fn from_id<'c>(id: Self::Id, context: Self::Context<'c>) -> Self {
        return X{};
    }
}

// offset of 1 for option type, None has id=0
impl <T: Handle> Handle for Option<T> {
    // very convoluted but that's ok

    type Id = <T as Handle>::Id;
    type Context<'c> = <T as Handle>::Context<'c>;

    fn id<'c>(&self, context: Self::Context<'c>) -> Self::Id {
        match self {
            None => Self::Id::from_usize(0),
            Some(t) => Self::Id::from_usize(t.usize_id(context) + 1),
        }
        // Some(t) => t.id(context)+1, impossible because we can't know that the literal 1 can be converted to an Id
    }
    fn from_id<'c>(id: Self::Id, context: Self::Context<'c>) -> Self {
        match id.into_usize() {
            0 => Self::None,
            _ => Self::Some(T::from_usize(id.into_usize() - 1, context)),
        }
        // can't match id directly because of the lack of information about its type
    }
    
}


/*
T -----> Id -----> usize
  Handle   Indexing

*/

/*
macros that would be usefull:

#[derive(Handle(TYPE))]
- on an enum:
    possible only if every variant implements Handle with the same Id
    the Id will be the same as the variants
    the context will be of type: ([<V1 as Handle>::Context, <V2 as Handle>::Context, ...],
                                  [nbr of V1 elements, ..., nbr of V(n-1) elements])
    where V(k) is the kth variant and there are n variants.

- on a struct:
    takes the first field that implements Handle and delegates everything to it
*/