// the goal of this file is to define common data structures in a more efficient way
// the target types are Vec<T> where 
//      - T is heap allocated
//      - the size of the heap allocation of T is constant (but size known at runtime)
//
// We split the Vec<T> into a Vec of all the data 