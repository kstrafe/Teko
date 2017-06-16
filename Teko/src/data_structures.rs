use std::rc::Rc;
use std::collections::HashMap;
use num::bigint::BigInt;
use num::rational::BigRational;
use num::Complex;

/// Primitive forms
#[derive(Clone)]
pub enum Commands {
	Call,
	Prepare(Statement),
	Pushcall,
	Parameterize,
	Deparameterize(Vec<String>),
	Unwind,
	Wind,
	Empty,
}

pub struct Sourcedata(pub Source, pub Coredata);
pub type Statement = Rc<Sourcedata>;
pub type Program   = Vec<Statement>;
pub type Transfer  = fn(top:     &Statement,
                        program: &mut Program,
                        env:     &mut Env);
pub enum Function { Builtin(Transfer), Library(Vec<String>, Statement) }
pub enum Macro { Builtin(Transfer), Library(String, Statement) }
pub enum Coredata {
	Complex  (Complex<BigRational>),
	Function (Function),
	Integer  (BigInt),
	Internal (Commands),
	Macro    (Macro),
	Null     ,
	Pair     (Rc<Sourcedata>, Rc<Sourcedata>),
	Rational (BigRational),
	String   (String),
	Symbol   (String),
}

#[derive(Clone)]
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

#[derive(Clone)]
pub struct Env {
	pub content:      HashMap<String, Program>,
	pub call_stack:   Vec<Rc<Sourcedata>>,
	pub params:       Vec<Vec<Rc<Sourcedata>>>,
	pub return_value: Rc<Sourcedata>,
}

#[derive(Clone)]
pub struct ParseState {
	pub current_read_position:         Source,
	pub start_of_current_lexeme:       Source,
	pub unmatched_opening_parentheses: Vec<Source>,
	pub token: String,
	pub stack: Program,
	pub error: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Source {
	pub line:   usize,
	pub column: usize,
	pub source: String,
}
