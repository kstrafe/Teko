#![feature(slice_patterns)]
extern crate num;

pub mod interpret;
pub mod parse;
pub mod data_structures;
pub mod data_structures_impls;

const VEC_CAPACITY: usize = 10;
