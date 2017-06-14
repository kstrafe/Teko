use std::rc::Rc;
use std::collections::HashMap;
use num::bigint::BigInt;
use num::rational::BigRational;
use num::Complex;

/*
	TODO:
	Implement simple numbers
	Implement define
	Implement set
	Implement basic calling
*/
#[derive(Clone, Debug)]
pub enum Commands {
	Define(String),              // Map String := Return
	Set(String),                 // Set value at String = Return
	If(Rc<Data>, Rc<Data>),      // If Return is non-null, run If.0, else run If.1
	Parameterize,                // Push Return into the function parameter list
	Deparameterize(Vec<String>), // Pop parameters from the environment, this happens after a function call is done
	Pushcall,                    // Push Return onto the environment's call stack
	Prepare(Rc<Data>),           // Use the top of the call stack and and prepare for a function or macro call
	Call,                        // Perform a function or macro call
	Unwind(String),              // Unwind to the given escape continuation
	Escape(String),              // Denote an escape continuation's location on the stack
	Empty,                       // Placeholder for nothing

	Plus,
}

#[derive(Clone, Debug)]
pub enum Data {
	Complex  (Source, Complex<BigRational>),
	Function (Source, Vec<String>, Rc<Data>),
	Integer  (Source, BigInt),
	Internal (Source, Commands),
	Macro    (Source, String, Rc<Data>),
	Null     (Source),
	Pair     (Source, Rc<Data>, Rc<Data>),
	Rational (Source, BigRational),
	String   (Source, String),
	Symbol   (Source, String),
}

#[derive(Clone, Debug)]
pub struct Env {
	pub content:      HashMap<String, Vec<Rc<Data>>>,
	pub call_stack:   Vec<Rc<Data>>,
	pub params:       Vec<Rc<Data>>,
	pub return_value: Rc<Data>,
}

#[derive(Clone, Debug)]
pub struct ParseState {
	pub current_read_position:         Source,
	pub start_of_current_lexeme:       Source,
	pub unmatched_opening_parentheses: Vec<Source>,
	pub token: String,
	pub stack: Vec<Rc<Data>>,
	pub error: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Source {
	pub line:   usize,
	pub column: usize,
	pub source: String,
}
