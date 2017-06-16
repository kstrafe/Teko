use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;
use num::bigint::BigInt;
use num::rational::BigRational;
use num::Complex;
use data_structures::{Commands, Coredata, Env, ParseState, Source, Sourcedata};
use super::VEC_CAPACITY;

impl Default for Source {
	fn default() -> Source {
		Source { line: 1, column: 1, source: "unknown".into() }
	}
}

impl Sourcedata {
	pub fn head(&self) -> Rc<Sourcedata> {
		if let &Sourcedata(_, Coredata::Pair(ref head, _)) = self {
			head.clone()
		} else {
			Rc::new(Sourcedata(Source::default(), Coredata::Null))
		}
	}
	pub fn tail(&self) -> Rc<Sourcedata> {
		if let &Sourcedata(_, Coredata::Pair(_, ref tail)) = self {
			tail.clone()
		} else {
			Rc::new(Sourcedata(Source::default(), Coredata::Null))
		}
	}
}

impl Default for ParseState {
	fn default() -> ParseState {
		ParseState {
			current_read_position:         Source::default(),
			start_of_current_lexeme:       Source::default(),
			unmatched_opening_parentheses: Vec::with_capacity(VEC_CAPACITY),
			token: String::from(""),
			stack: Vec::with_capacity(VEC_CAPACITY),
			error: None,
		}
	}
}

impl ParseState {
	pub fn from_file(filename: &str) -> ParseState {
		let mut state = ParseState::default();
		state.current_read_position = Source {
			line:   1,
			column: 1,
			source: filename.into(),
		};
		state
	}
}
