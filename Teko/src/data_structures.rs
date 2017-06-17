use std::rc::Rc;
use std::collections::HashMap;
use num::bigint::BigInt;
use num::rational::BigRational;
use num::Complex;

/// Primitive forms
#[derive(Clone)]
pub enum Commands {
	Call(Statement),
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
pub struct Env {
	pub store:  HashMap<String, Program>,
	pub calls:  Program,
	pub params: Vec<Program>,
	pub result: Statement,
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
